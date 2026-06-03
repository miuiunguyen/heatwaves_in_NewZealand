# Author: Mui Nguyen
# Date created: 
# Date modified: 

# Set environment
rm(list = ls())
# Set environment paths
data_path <- ""
save_path <- ""
mort_path <- ""
# load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)
library(writexl)
library(forcats)  # for factor reordering
library(openxlsx)
library(scales)  # for pretty_breaks()
library(ggrepel)
library(grid) # for arrow()
library(plotly)
library(cowplot)
library(readxl)

#------FIRST PART WITH TEMPERATURE DATA (Alt+L to hide) ------

#-=-----------------------------------------
# ALL TEMPERATURE BY STATIONS DATA
#-------------------------------------------
# Example: read data from data_path
files <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE)
all_temp <- lapply(files, function(f) {
  # Read the file
  df <- read_csv(f)
  # Extract file name without extension
  file_id <- tools::file_path_sans_ext(basename(f))
  # Add as new column
  df <- df %>% mutate(id = file_id)
  return(df)
}) %>%
  bind_rows()

#-------------------------------------------------------------
### Auckland AREA: TEMPERATURE DATA FROM Auckland AERO STATIONS: AGENT #1962
#-------------------------------------------------------------
all_temp_Auckland <- all_temp %>%
  mutate(station_id = sub("_.*", "", id)) %>% 
  mutate(
    `Observation time UTC` = ymd_hms(`Observation time UTC`),
    year  = year(`Observation time UTC`),
    month = month(`Observation time UTC`),
    day   = day(`Observation time UTC`)
  ) %>% 
  mutate(
    Tmax = `Maximum Temperature [Deg C]`,
    Tmin = `Minimum Temperature [Deg C]`,
    Tmean = (Tmax + Tmin)/2) %>% 
  filter(id == "1962__Temperature__daily_updated")

# Filter data summer season 2000-2021
# Filter for dates between 1 Nov 2000 and 31 Mar 2021
summer_data <- all_temp_Auckland %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  filter(date >= as.Date("2000-11-01") & date <= as.Date("2021-03-31")) %>% 
  filter(month %in% c(11, 12, 1, 2, 3)) %>%
  mutate(
    # Create 'season_year_start' which is the year when the season started
    season_year_start = if_else(month %in% c(11, 12), year, year - 1),
    # Create season label like "2000/01"
    season = paste0(season_year_start, "/", substr(season_year_start + 1, 3, 4))
  ) %>%
  select(station_id, season, date, year, month, day, Tmax, Tmin, Tmean)

# Check
head(summer_data)

#------------------------------------------------------------
### FIGURE 5: PLOT THE AVERAGE OF DAILY MEAN TEMPERATURE DMT, DAILY MAX TEMP, HIGHEST RECORD DAILY TEMP, PER SUMMER SEASON 2000/01-2019/20
#------------------------------------------------------------

summary_table_1 <- summer_data %>%
  group_by(season) %>%
  summarise(
    Tmax_avg = mean(Tmax, na.rm = TRUE),
    Tmax_max = max(Tmax, na.rm = TRUE),
    DMT_avg = mean(Tmean, na.rm = TRUE)
  ) %>%
  arrange(season)  # optional, to sort by season

print(summary_table_1)

# Convert data to long format for ggplot
summary_table_long_1 <- summary_table_1 %>%
  pivot_longer(cols = c(Tmax_avg, Tmax_max, DMT_avg),
               names_to = "variable", values_to = "value")
# Rename variables for legend
summary_table_long_1$variable <- recode(summary_table_long_1$variable,
                           Tmax_avg = "Tmax (average)",
                           Tmax_max = "Tmax (max)",
                           DMT_avg = "DMT (average)")

# Plot
aver_by_season <- ggplot(summary_table_long_1, aes(x = season, y = value, color = variable, group = variable)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_text(data = summary_table_long_1 %>% group_by(variable),
            aes(label = sprintf("%.1f", value)),
            hjust = 0.5, vjust = -1.2, size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = c("Tmax (average)" = "#ffc20e",   # gold
                                "Tmax (max)" = "#DC7C28",       # orange
                                "DMT (average)" = "#4F81BD")) + # blue
  labs(x = "season", y = "temperature (°C)", color = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        legend.position = "top",
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 1.08,
                                    margin = margin(r = -80), #shift right
                                    face = "bold",size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "grey80")) + # keep only major
  coord_cartesian(clip = "off") +
  scale_y_continuous(limits = c(10, 35),breaks = seq(10, 35, 5),
                     expand = c(0, 0))

aver_by_season

# save the plot
# Create a filename
file_name <- "Fig5_average_by_season.png"
# Full path
full_path <- file.path(save_path, file_name)

ggsave(full_path, plot = aver_by_season, width = 10, height = 6, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA)
)

#-------------------------------------------------------------------
## FIGURE 6: AVERAGE DAILY MAX TEMPERATURE, BY SUMMER MONTH, 2000/01 -2019/20
#-------------------------------------------------------------------
summer_data <- summer_data %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE))  # Dec, Jan, Feb

# Summarise by month
summary_table_2 <- summer_data %>%
  group_by(month) %>%
  summarise(
    count_Tmax = sum(!is.na(Tmax)),
    Tmax_avg = mean(Tmax, na.rm = TRUE),
    Tmax_max = max(Tmax, na.rm = TRUE),
    Tmax_sd = sd(Tmax, na.rm = TRUE),
    DMT_avg = mean(Tmean, na.rm = TRUE),
    error = 1.96 * Tmax_sd / sqrt(count_Tmax),
    lower_CI = Tmax_avg - error,
    upper_CI = Tmax_avg + error
  ) %>%
  arrange(month)

# Convert to long format for ggplot
summary_table_long_2 <- summary_table_2 %>%
  pivot_longer(cols = c(Tmax_avg, Tmax_max, DMT_avg),
               names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable,
                           Tmax_avg = "Tmax (average)",
                           Tmax_max = "Tmax (max)",
                           DMT_avg = "DMT (average)")) %>% 
  filter(variable == "Tmax (average)",
         month %in% c("Nov", "Dec", "Jan", "Feb", "Mar")) %>%
  mutate(
    month_full = recode(month,
                        "Nov" = "November",
                        "Dec" = "December",
                        "Jan" = "January",
                        "Feb" = "February",
                        "Mar" = "March"),
    month_full = factor(month_full,
                        levels = c("November", "December", "January", "February", "March")))

# plot 
aver_by_month <- summary_table_long_2 %>% 
  ggplot(aes(x = month_full, y = value)) +
  geom_col(width = 0.7, fill = "#85b640") +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), 
                width = 0.1, 
                color = "black", 
                size = 0.3) +
  labs(x = "month", y = "Tmax (average)\n(°C)") +
  geom_text(aes(label =  sprintf("%.1f", value)), 
            position = position_stack(vjust = 0.04), 
            color = "black", size = 4) +   # label inside the bar
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 6, size = 12),
    legend.position = "none",
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 4),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.1, margin = margin(r = -85)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 30, r = 10, b = 10, l = 10)
  ) +
  coord_cartesian(ylim = c(0, 25), clip = "off")

# save the plot
# Create a filename
file_name <- "Fig6_average_by_month.png"
# Full path
full_path <- file.path(save_path, file_name)
ggsave(full_path, plot = aver_by_month, width = 10, height = 4, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA)
)

# ----------------------------------------------
### CALCULATE HEATWAVES DAYS -------------------
#-----------------------------------------------
#use: temp_adf above

head(all_temp_Auckland)

#-----------------------------------------------
#### EHF APPROACH ------------------------------
#-----------------------------------------------

# Filter for dates between 1 Nov 2000 and 30 Apr 2021
ext_summer_data <- all_temp_Auckland %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  filter(date >= as.Date("2000-10-01") & date <= as.Date("2021-04-30")) %>% 
  filter(month %in% c(10, 11, 12, 1, 2, 3, 4)) %>%
  mutate(
    # Create 'season_year_start' which is the year when the season started
    season_year_start = if_else(month %in% c(10,11, 12), year, year - 1),
    # Create season label like "2000/01"
    season = paste0(season_year_start, "/", substr(season_year_start + 1, 3, 4))
  ) %>%
  select(station_id, season, date, year, month, day, Tmax, Tmin, Tmean)

# Check
head(ext_summer_data)

#------ CALCULATE ALL THRESHOLD OF EHF: PERIOD 1981-2010 -----------
# -----------------------------------------------

# Calculate 95% percentile of Tmean from 1981 to 2010
percentile_95_1981_2010 <- all_temp_Auckland %>%
  filter(year >= 1981, year <= 2010) %>%
  summarise(percentile_95 = quantile(Tmean, probs = 0.95, na.rm = TRUE)) %>%
  pull(percentile_95)

print(percentile_95_1981_2010)

percentile_95_1971_2010 <- all_temp_Auckland %>%
  filter(year >= 1971, year <= 2010) %>%
  summarise(percentile_95 = quantile(Tmean, probs = 0.95, na.rm = TRUE)) %>%
  pull(percentile_95)

per_Tmax_99_1981_2010 <- all_temp_Auckland %>%
  filter(year >= 1981, year <= 2010) %>%
  summarise(percentile_99 = quantile(Tmax, probs = 0.99, na.rm = TRUE)) %>%
  pull(percentile_99)

per_Tmax_99_1971_2010 <- all_temp_Auckland %>%
  filter(year >= 1971, year <= 2010) %>%
  summarise(percentile_99 = quantile(Tmax, probs = 0.99, na.rm = TRUE)) %>%
  pull(percentile_99)

# calculate 85th threshold of EHF
ehf_threshold <- all_temp_Auckland %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  arrange(station_id, date) %>%  # Make sure data is sorted by Station and Date
  group_by(station_id) %>%
  mutate(
    # Calculate rolling mean of Tmean over current and next 2 days
    Tmean_rollmean3 = (Tmean + lead(Tmean, 1) + lead(Tmean, 2)) / 3,
    # Calculate EHI_sig
    EHI_sig = Tmean_rollmean3 - percentile_95_1981_2010,
    # 30-day rolling mean of past DMT (excluding current day)
    DMT_past30 = zoo::rollapplyr(Tmean, width = 30, FUN = function(x) mean(x, na.rm = TRUE), fill = NA, partial = FALSE, align = "right"),
    # Shift by one day to exclude current day
    DMT_past30_lag = lag(DMT_past30, 1),
    #EHI_accl
    EHI_accl = Tmean_rollmean3 - DMT_past30_lag,
    EHI = abs(EHI_accl) * EHI_sig,
    EHI_new = pmax(0,EHI_sig) * pmax(1,EHI_accl),
    #heat day if EHI>0
    ehf_heat_day = if_else(EHI > 0.0000000001, 1, 0)
  ) %>%
  ungroup()

per_ehf_85th_alldata <- ehf_threshold %>%
  filter (EHI_new > 0) %>% 
  #filter(year >= 1971, year <= 2010) %>%
  summarise(percentile_85 = quantile(EHI_new, probs = 0.85, na.rm = TRUE)) %>%
  pull(percentile_85)

per_ehf_85th_1981_2010 <- ehf_threshold %>%
  filter (EHI_new > 0) %>% 
  filter(year >= 1981, year <= 2010) %>%
  summarise(percentile_85 = quantile(EHI_new, probs = 0.85, na.rm = TRUE)) %>%
  pull(percentile_85)

per_ehf_85th_1966_2021 <- ehf_threshold %>%
  filter (EHI_new > 0) %>% 
  filter(year >= 1966, year <= 2021) %>%
  summarise(percentile_85 = quantile(EHI_new, probs = 0.85, na.rm = TRUE)) %>%
  pull(percentile_85)

# Calculate heatwaves with summer season (Nov - Mar)
ehf_summer <- ehf_threshold %>%
  filter(month %in% c(11, 12, 1, 2, 3)) %>%
  filter(date >= as.Date("2000-10-01") & date <= as.Date("2021-04-30")) %>% 
  mutate(
    # Create 'season_year_start' which is the year when the season started
    season_year_start = if_else(month %in% c(11, 12), year, year - 1),
    # Create season label like "2000/01"
    season = paste0(season_year_start, "/", substr(season_year_start + 1, 3, 4))
  ) %>%
  select(station_id, season, date, year, month, day, Tmax, Tmin, Tmean, EHI_sig, EHI_accl, EHI, EHI_new, ehf_heat_day) %>% 
  mutate(ehf_excess_heat = ifelse(EHI > 0.0000000001, EHI, 0),
         # create severe and extreme heatwaves
         ehf_severe_heat_day = ifelse(EHI_new >= per_ehf_85th_1966_2021, 1, 0),
         ehf_extreme_heat_day = ifelse(EHI_new >= 3 * per_ehf_85th_1966_2021, 1, 0),
         #try with another period: same as T95 1981-2010
         ehf_severe_heat_day_2 = ifelse(EHI_new >= per_ehf_85th_1981_2010, 1, 0),
         ehf_extreme_heat_day_2 = ifelse(EHI_new >= 3 * per_ehf_85th_1981_2010, 1, 0),
        # create heatwaves days by heat_day plus next 2days from heat day
        ehf_heatwave = if_else(ehf_heat_day == 1 | lag(ehf_heat_day, 1) == 1 | lag(ehf_heat_day, 2) == 1,1, 0),
        ehf_severe_heatwave = if_else(ehf_severe_heat_day == 1 | lag(ehf_severe_heat_day, 1) == 1 | lag(ehf_severe_heat_day, 2) == 1, 1, 0),
        ehf_extreme_heatwave = if_else(ehf_extreme_heat_day == 1 | lag(ehf_extreme_heat_day, 1) == 1 | lag(ehf_extreme_heat_day, 2) == 1, 1, 0),
        #excess heat for severe and extreme hw
        ehf_severe_excess_heat = ifelse(ehf_severe_heat_day == 1, EHI, 0),
        ehf_extreme_excess_heat = ifelse(ehf_extreme_heat_day == 1, EHI, 0),
        # mutually exclusive severity level
        severity_level = case_when(
          ehf_extreme_heatwave == 1 ~ "Extreme",
          ehf_severe_heatwave == 1  ~ "Severe",
          ehf_heatwave == 1         ~ "Low",
          TRUE ~ "No heatwave"
        ),
        
        # optional: control ordering in plots
        severity_level = factor(severity_level, 
                                levels = c("Low", "Severe", "Extreme", "No heatwave"))
  ) %>% 
  ungroup()

get_episodes <- function(df, flag_col, prefix) {
  df %>%
    arrange(station_id, date) %>%
    group_by(station_id) %>%
    mutate(run_id = data.table::rleid(.data[[flag_col]])) %>%
    group_by(station_id, run_id) %>%
    mutate(run_length = n(),
           episode_flag = .data[[flag_col]] == 1 & run_length >= 3) %>%
    ungroup() %>%
    group_by(station_id) %>%
    mutate("{prefix}_episode_id" := ifelse(episode_flag, cumsum(c(0, diff(episode_flag)) == 1), NA_integer_)) %>%
    ungroup() %>%
    group_by(station_id, .data[[paste0(prefix, "_episode_id")]]) %>%
    mutate("{prefix}_duration_days" := ifelse(!is.na(.data[[paste0(prefix, "_episode_id")]]), n(), NA_integer_)) %>%
    ungroup() %>%
    select(-run_id, -run_length, -episode_flag)
}

ehf_summer <- ehf_summer %>%
  get_episodes("ehf_heatwave", "ehf") %>%
  get_episodes("ehf_severe_heatwave", "ehf_severe") %>%
  get_episodes("ehf_extreme_heatwave", "ehf_extreme")

# Table: Number of heatwave episodes and heatwaves day
ehf_summary_season <- ehf_summer %>%
  group_by(station_id, season) %>%
  summarise(
    num_episodes = n_distinct(ehf_episode_id[!is.na(ehf_episode_id)]), # count episodes
    num_hw_days = sum(ehf_heatwave, na.rm = TRUE),              # total heatwave days
    total_excess_temp = sprintf("%.1f", sum(ehf_excess_heat, na.rm = TRUE)),
    # severe heatwave
    num_severe_episodes = n_distinct(ehf_severe_episode_id[!is.na(ehf_severe_episode_id)]), # count episodes
    num_severe_hw_days = sum(ehf_severe_heatwave, na.rm = TRUE),              # total heatwave days
    total_severe_excess_temp = sprintf("%.1f", sum(ehf_severe_excess_heat, na.rm = TRUE)),
    # extreme heatwave
    num_extreme_episodes = n_distinct(ehf_extreme_episode_id[!is.na(ehf_extreme_episode_id)]), # count episodes
    num_extreme_hw_days = sum(ehf_extreme_heatwave, na.rm = TRUE),              # total heatwave days
    total_extreme_excess_temp = sprintf("%.1f", sum(ehf_extreme_excess_heat, na.rm = TRUE))) %>% 
  ungroup()

# Export data to excel, both table in the different sheet
write_xlsx(
  list(
    "ehf_summer" = ehf_summer,
    "ehf_summary_season" = ehf_summary_season
  ),
  path = file.path(save_path, "heatwaves_summer.xlsx")
)

#------------------------------------------------
## Figure 7: Number of heatwave episodes and heatwaves day, EHF approach
#-------------------------------------------------

ehf_summary_long <- ehf_summary_season %>%
  mutate(across(c(num_episodes, num_hw_days, total_excess_temp,
                  num_severe_episodes, num_severe_hw_days, total_severe_excess_temp,
                  num_extreme_episodes, num_extreme_hw_days, total_extreme_excess_temp), 
                as.numeric)) %>%
  pivot_longer(
    cols = c(num_episodes, num_hw_days, total_excess_temp,
             num_severe_episodes, num_severe_hw_days, total_severe_excess_temp,
             num_extreme_episodes, num_extreme_hw_days, total_extreme_excess_temp),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    # keep season order (latest at top)
    season = factor(season, levels = rev(sort(unique(season)))),
    
    # recode metrics into readable labels
    metric = recode(metric,
                    num_episodes = "Number of Heatwave Episodes",
                    num_hw_days = "Number of Heatwave Days",
                    total_excess_temp = "Heatwave Excess Temperature (°C)",
                    num_severe_episodes = "Number of Severe Heatwave Episodes",
                    num_severe_hw_days = "Number of Severe Heatwave Days",
                    total_severe_excess_temp = "Severe Heatwave Excess Temperature (°C)",
                    num_extreme_episodes = "Number of Extreme Heatwave Episodes",
                    num_extreme_hw_days = "Number of Extreme Heatwave Days",
                    total_extreme_excess_temp = "Extreme Heatwave Excess Temperature (°C)"
    ),
    
    # order facets in logical sequence
    metric = factor(metric, levels = c(
      "Number of Heatwave Episodes", "Number of Heatwave Days", "Heatwave Excess Temperature (°C)",
      "Number of Severe Heatwave Episodes", "Number of Severe Heatwave Days", "Severe Heatwave Excess Temperature (°C)",
      "Number of Extreme Heatwave Episodes", "Number of Extreme Heatwave Days", "Extreme Heatwave Excess Temperature (°C)"
    ))
  )

# set facet-specific x-axis max values
ehf_summary_long <- ehf_summary_long %>%
  mutate(
    x_max = case_when(
      str_detect(metric, "Episodes") ~ 10,
      str_detect(metric, "Days") ~ 70,
      str_detect(metric, "Excess Temperature") ~ 150
    )
  )

ehf_heatwaves <- ehf_summary_long %>%
  filter(metric %in% c("Number of Heatwave Episodes",
                       "Number of Heatwave Days",
                       "Heatwave Excess Temperature (°C)")) %>%
  ggplot(aes(x = value, y = season)) + 
  geom_col(fill = "#85b640") +
  geom_text(
    aes(label = ifelse(
      value == 0, 
      "", 
      ifelse(metric == "Heatwave Excess Temperature (°C)",
             sprintf("%.1f", value),      # one decimal
             as.character(value))         # otherwise keep as integer
    )),
    hjust = -0.2,   # pushes labels slightly outside bar
    size = 3.5
  ) +
  geom_blank(aes(x = x_max)) +  # ensures x-axis extends to x_max
  facet_wrap(~ metric, scales = "free_x", strip.position = "top") +
  labs(
    x = "",
    y = "EHF \nseason",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12, angle = 0, 
                                    hjust = 0, vjust = 1.1, 
                                    margin = margin(r = -40)),
        strip.text = element_text(face = "bold", hjust = 0, size = 10)
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    breaks = function(x) pretty(x, n = 4),
    expand = expansion(mult = c(0, 0.05))
  )

ehf_severe_heatwaves <- ehf_summary_long %>%
  filter(metric %in% c("Number of Severe Heatwave Episodes",
                       "Number of Severe Heatwave Days",
                       "Severe Heatwave Excess Temperature (°C)")) %>%
  ggplot(aes(x = value, y = season)) + 
  geom_col(fill = "#85b640") +
  geom_text(
    aes(label = ifelse(
      value == 0, 
      "", 
      ifelse(metric == "Severe Heatwave Excess Temperature (°C)",
             sprintf("%.1f", value),      # one decimal
             as.character(value))         # otherwise keep as integer
    )),
    hjust = -0.2,   # pushes labels slightly outside bar
    size = 3.5
  ) +
  geom_blank(aes(x = x_max)) +  # ensures x-axis extends to x_max
  facet_wrap(~ metric, scales = "free_x", strip.position = "top") +
  labs(
    x = "",
    y = "EHF \nseason",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12, angle = 0, 
                                    hjust = 0, vjust = 1.1, 
                                    margin = margin(r = -40)),
        strip.text = element_text(face = "bold", hjust = 0, size = 10)
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    breaks = function(x) pretty(x, n = 4),
    expand = expansion(mult = c(0, 0.05))
  )

ehf_extreme_heatwaves <- ehf_summary_long %>%
  filter(metric %in% c("Number of Extreme Heatwave Episodes",
                       "Number of Extreme Heatwave Days",
                       "Extreme Heatwave Excess Temperature (°C)")) %>%
  ggplot(aes(x = value, y = season)) + 
  geom_col(fill = "#85b640") +
  geom_text(
    aes(label = ifelse(
      value == 0, 
      "", 
      ifelse(metric == "Extreme Heatwave Excess Temperature (°C)",
             sprintf("%.1f", value),      # one decimal
             as.character(value))         # otherwise keep as integer
    )),
    hjust = -0.2,   # pushes labels slightly outside bar
    size = 3.5
  ) +
  geom_blank(aes(x = x_max)) +  # ensures x-axis extends to x_max
  facet_wrap(~ metric, scales = "free_x", strip.position = "top") +
  labs(
    x = "",
    y = "EHF \nseason",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12, angle = 0, 
                                    hjust = 0, vjust = 1.1, 
                                    margin = margin(r = -40)),
        strip.text = element_text(face = "bold", hjust = 0, size = 10)
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    breaks = function(x) pretty(x, n = 4),
    expand = expansion(mult = c(0, 0.05))
  )

# save the plot
# Full path
full_path_1 <- file.path(save_path, "fig7a_ehf_heatwaves.png")
full_path_2 <- file.path(save_path, "fig7b_ehf_severe_heatwaves.png")
full_path_3 <- file.path(save_path, "fig7c_ehf_extreme_heatwaves.png")

ggsave(full_path_1, plot = ehf_heatwaves, width = 10, height = 6, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA))

ggsave(full_path_2, plot = ehf_severe_heatwaves, width = 10, height = 6, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA))

ggsave(full_path_3, plot = ehf_extreme_heatwaves, width = 10, height = 6, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA))

#------------------------------------------------
## Figure 8: TEN WORST HEATWAVES EPISODES BASED ON EXCESS TEMPERATURE
#-------------------------------------------------
# Create the table of episode, duration, excess temp. Tmax, season
ehf_hw_episode <- ehf_summer %>%
  filter(!is.na(ehf_episode_id)) %>%                # Remove NA episodes
  group_by(station_id, ehf_episode_id, season) %>%
  summarise(
    Duration = mean(ehf_duration_days, na.rm = TRUE),
    `Heatwave Excess temperature (°C)` = sum(ehf_excess_heat, na.rm = TRUE),
    `Tmax (average)` = mean(Tmax, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    station_id = station_id,
    Episode = ehf_episode_id,
    Season = season
  ) %>%
  arrange(desc(`Heatwave Excess temperature (°C)`))

ehf_severe_hw_episode <- ehf_summer %>%
  filter(!is.na(ehf_severe_episode_id)) %>%                # Remove NA episodes
  group_by(station_id, ehf_severe_episode_id, season) %>%
  summarise(
    Duration = mean(ehf_severe_duration_days, na.rm = TRUE),
    `Severe Heatwave Excess temperature (°C)` = sum(ehf_severe_excess_heat, na.rm = TRUE),
    `Tmax (average)` = mean(Tmax, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    station_id = station_id,
    Episode = ehf_severe_episode_id,
    Season = season
  ) %>%
  arrange(desc(`Severe Heatwave Excess temperature (°C)`))

ehf_extreme_hw_episode <- ehf_summer %>%
  filter(!is.na(ehf_extreme_episode_id)) %>%                # Remove NA episodes
  group_by(station_id, ehf_extreme_episode_id, season) %>%
  summarise(
    Duration = mean(ehf_extreme_duration_days, na.rm = TRUE),
    `Extreme Heatwave Excess temperature (°C)` = sum(ehf_extreme_excess_heat, na.rm = TRUE),
    `Tmax (average)` = mean(Tmax, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    station_id = station_id,
    Episode = ehf_extreme_episode_id,
    Season = season
  ) %>%
  arrange(desc(`Extreme Heatwave Excess temperature (°C)`))

# Export to Excel
# Load the existing file
wb <- loadWorkbook(file.path(save_path, "heatwaves_summer.xlsx"))

# Add as a new sheet
if ("ehf_hw_episode" %in% names(wb)) removeWorksheet(wb, "ehf_hw_episode")
addWorksheet(wb, "ehf_hw_episode")
writeData(wb, "ehf_hw_episode", ehf_hw_episode)
# Save without removing existing sheets
saveWorkbook(wb, file.path(save_path, "heatwaves_summer.xlsx"), overwrite = TRUE)

# Add as a new sheet
if ("ehf_severe_hw_episode" %in% names(wb)) removeWorksheet(wb, "ehf_severe_hw_episode")
addWorksheet(wb, "ehf_severe_hw_episode")
writeData(wb, "ehf_severe_hw_episode", ehf_severe_hw_episode)
# Save without removing existing sheets
saveWorkbook(wb, file.path(save_path, "heatwaves_summer.xlsx"), overwrite = TRUE)

# Add as a new sheet
if ("ehf_extreme_hw_episode" %in% names(wb)) removeWorksheet(wb, "ehf_extreme_hw_episode")
addWorksheet(wb, "ehf_extreme_hw_episode")
writeData(wb, "ehf_extreme_hw_episode", ehf_extreme_hw_episode)
# Save without removing existing sheets
saveWorkbook(wb, file.path(save_path, "heatwaves_summer.xlsx"), overwrite = TRUE)

# plot ten worst heatwaves episodes
# Select top 10 by excess temperature
ehf_top10 <- ehf_hw_episode %>%
  arrange(desc(`Heatwave Excess temperature (°C)`)) %>%
  slice_head(n = 10)

# Scatter plot
ehf_worst_hw <- ggplot(ehf_top10, aes(x = Duration, y = `Tmax (average)`)) +
  geom_point(aes(size = `Heatwave Excess temperature (°C)`), color = "#85b640", alpha = 0.8) +
  geom_label_repel(
    aes(label = paste0("Ep",Episode, " - S", Season )),
    fill = "#f9f4e7",                 # Soft warm beige
    color = "#2b2b2b",                 # Dark gray text for contrast
    size = 3,                        # Font size
    label.size = 0.2,                  # Border thickness
    label.colour = "#85b640",          # Muted gold border
    label.r = unit(0.15, "lines"),     # Slightly more rounded corners
    box.padding = 0.5,                 
    label.padding = unit(0.2, "lines"),
    segment.color = "#85b640",         # Arrow color matches border
    max.overlaps = Inf
  ) +
  scale_size_continuous(range = c(2, 8), breaks = c(10, 40, 70), limits = c(0, 80)) +
  scale_x_continuous(
    limits = c(0, 30),
    breaks = pretty_breaks(n = 7)  # auto-rounded breaks for x-axis
  ) +
  scale_y_continuous(
    limits = c(25, 30),
    breaks = pretty_breaks(n = 5)  # auto-rounded breaks for y-axis
  ) +
  labs(
    x = "duration",
    y = bquote(bold("average") ~ bold(T[max])),
    size = "Heatwave \nExcess temperature (°C)"
  ) +
  theme_light()+
  theme(
    legend.position = c(0.98, 0.98),     # X, Y coordinates (near top-right)
    legend.justification = c("right", "top"),  # Align legend corner to position
    legend.background = element_rect(fill = "white", color = "grey90"),  
    legend.key.size = unit(0.3, "lines"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.box.margin = margin(1, 1, 1, 1),
    legend.spacing = unit(0.1, "lines"),
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 0),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.06, margin = margin(r = -70)),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 30, r = 5, b = 5, l = 5)  # top margin opened
  )

# Severe HEATWAVE
ehf_severe_top10 <- ehf_severe_hw_episode %>%
  arrange(desc(`Severe Heatwave Excess temperature (°C)`)) %>%
  slice_head(n = 10)

# Scatter plot
ehf_worst_severe_hw <- ggplot(ehf_severe_top10, aes(x = Duration, y = `Tmax (average)`)) +
  geom_point(aes(size = `Severe Heatwave Excess temperature (°C)`), color = "#85b640", alpha = 0.8) +
  geom_label_repel(
    aes(label = paste0("Ep",Episode, " - S", Season )),
    fill = "#f9f4e7",                 # Soft warm beige
    color = "#2b2b2b",                 # Dark gray text for contrast
    size = 3,                        # Font size
    label.size = 0.2,                  # Border thickness
    label.colour = "#85b640",          # Muted gold border
    label.r = unit(0.15, "lines"),     # Slightly more rounded corners
    box.padding = 0.5,                 
    label.padding = unit(0.2, "lines"),
    segment.color = "#85b640",         # Arrow color matches border
    max.overlaps = Inf
  ) +
  scale_size_continuous(range = c(2, 8), breaks = c(10, 40, 70), limits = c(0, 80)) +
  scale_x_continuous(
    limits = c(0, 30),
    breaks = pretty_breaks(n = 7)  # auto-rounded breaks for x-axis
  ) +
  scale_y_continuous(
    limits = c(25, 30),
    breaks = pretty_breaks(n = 5)  # auto-rounded breaks for y-axis
  ) +
  labs(
    x = "duration",
    y = bquote(bold("average") ~ bold(T[max])),
    size = "Severe Heatwave \nExcess temperature (°C)"
  ) +
  theme_light()+
  theme(
    legend.position = c(0.98, 0.98),     # X, Y coordinates (near top-right)
    legend.justification = c("right", "top"),  # Align legend corner to position
    legend.background = element_rect(fill = "white", color = "grey90"),
    legend.key.size = unit(0.3, "lines"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.box.margin = margin(1, 1, 1, 1),
    legend.spacing = unit(0.1, "lines"),
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 0),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.06, margin = margin(r = -70)),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 30, r = 5, b = 5, l = 5)  # top margin opened
  )

# Extreme HEATWAVE
ehf_extreme_top10 <- ehf_extreme_hw_episode %>%
  arrange(desc(`Extreme Heatwave Excess temperature (°C)`)) %>%
  slice_head(n = 10)

# Scatter plot
ehf_worst_extreme_hw <- ggplot(ehf_extreme_top10, aes(x = Duration, y = `Tmax (average)`)) +
  geom_point(aes(size = `Extreme Heatwave Excess temperature (°C)`), color = "#85b640", alpha = 0.8) +
  geom_label_repel(
    aes(label = paste0("Ep",Episode, " - S", Season )),
    fill = "#f9f4e7",                 # Soft warm beige
    color = "#2b2b2b",                 # Dark gray text for contrast
    size = 3,                        # Font size
    label.size = 0.2,                  # Border thickness
    label.colour = "#85b640",          # Muted gold border
    label.r = unit(0.15, "lines"),     # Slightly more rounded corners
    box.padding = 0.5,                 
    label.padding = unit(0.2, "lines"),
    segment.color = "#85b640",         # Arrow color matches border
    max.overlaps = Inf
  ) +
  scale_size_continuous(range = c(2, 8), breaks = c(10, 40, 70), limits = c(0, 80)) +
  scale_x_continuous(
    limits = c(0, 30),
    breaks = pretty_breaks(n = 7)  # auto-rounded breaks for x-axis
  ) +
  scale_y_continuous(
    limits = c(25, 30),
    breaks = pretty_breaks(n = 5)  # auto-rounded breaks for y-axis
  ) +
  labs(
    x = "duration",
    y = bquote(bold("average") ~ bold(T[max])),
    size = "Extreme Heatwave \nExcess temperature (°C)"
  ) +
  theme_light()+
  theme(
    legend.position = c(0.98, 0.98),     # X, Y coordinates (near top-right)
    legend.justification = c("right", "top"), 
    legend.background = element_rect(fill = "white", color = "grey90"),
    legend.key.size = unit(0.3, "lines"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.box.margin = margin(1, 1, 1, 1),
    legend.spacing = unit(0.1, "lines"),
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 0),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.06, margin = margin(r = -70)),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 30, r = 5, b = 5, l = 5)  # top margin opened
  )


# save the plot

full_path_1 <- file.path(save_path, "fig8a_ehf_worst_heatwaves.png")
full_path_2 <- file.path(save_path, "fig8b_ehf_worst_severe_heatwaves.png")
full_path_3 <- file.path(save_path, "fig8c_ehf_worst_extreme_heatwaves.png")

ggsave(full_path_1, plot = ehf_worst_hw, width = 5, height = 5, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA))

ggsave(full_path_2, plot = ehf_worst_severe_hw, width = 5, height = 5, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA))

ggsave(full_path_3, plot = ehf_worst_extreme_hw, width = 5, height = 5, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA))

#----------------------------------------------
#-----------------------------------------------
#### MetService APPROACH ------------------------------
#-----------------------------------------------
#-------------------------------------------------

## Setup threshold for MetService here
met_thres_Tmax_27 <- 27
met_thres_Tmax_28 <- 28
met_thres_Tmax_29 <- 29
met_thres_Tmax_30 <- 30
met_thres_Tmean_23 <- 23
met_thres_Tmean_24 <- 24

# Threshold Tmax = 27/Tmean = 23
met_summer_2723 <- summer_data %>%
  mutate(met_one_day_alert = case_when(
      Tmax >= met_thres_Tmax_27 + 4 ~ "Extremely hot",
      Tmax >= met_thres_Tmax_27 + 1 ~ "Very hot",
      Tmax >= met_thres_Tmax_27 ~ "Hot",
      TRUE ~ "")) %>% 
  # --- two-day alert condition ---
  mutate(
    cond_two_day = if_else(Tmean >= pmax(21, met_thres_Tmean_23) & Tmax >= 27, 1, 0),
    met_two_days_alert = if_else(
      (cond_two_day == 1 & (lag(cond_two_day) == 1 | lead(cond_two_day) == 1)),
      1, 0)
    ) %>% ungroup() %>% 
  mutate(met_alert = if_else(
    (!is.na(met_one_day_alert) & met_one_day_alert != "") | met_two_days_alert == 1,
    1, 0)) %>%
  mutate(met_heat_day = if_else(met_alert > 0, 1, 0)) %>% 
  mutate(met_excess_heat = pmax(0, 
                                pmax(ifelse(!is.na(met_one_day_alert), Tmax - met_thres_Tmax_27, NA),
                                     Tmean - pmax(21, met_thres_Tmean_23),
                                     na.rm = TRUE))) %>% 
  arrange(station_id, year, month, day) %>%  # make sure data is ordered by date
  group_by(station_id) %>%                  # process each station_id separately
  mutate(
    # get run-length encoding of met_heat_day
    rle_id = rep(seq_along(rle(met_heat_day)$lengths), times = rle(met_heat_day)$lengths),
    rle_value = rep(rle(met_heat_day)$values, times = rle(met_heat_day)$lengths),
    met_heatwave_1days = ifelse(rle_value == 1 & ave(rle_value, rle_id, FUN = length) >= 1, 1, 0)
  ) %>% ungroup() %>%
  select(-rle_id, -rle_value) %>% 
  # create episode id for consecutive heatwave days
  mutate(met_episode_id_1days = ifelse(met_heatwave_1days == 1,
                                       cumsum(c(0, diff(met_heatwave_1days)) == 1),
                                       NA_integer_)) %>%
  ungroup() %>%
  # calculate duration of each heatwave episode: 1days
  group_by(station_id, met_episode_id_1days) %>%
  mutate(
    met_duration_days_1days = ifelse(!is.na(met_episode_id_1days), n(), NA_integer_)) %>%
  ungroup() 

# Threshold Tmax = 28/Tmean = 23
met_summer_2823 <- summer_data %>%
  mutate(met_one_day_alert = case_when(
    Tmax >= met_thres_Tmax_28 + 4 ~ "Extremely hot",
    Tmax >= met_thres_Tmax_28 + 1 ~ "Very hot",
    Tmax >= met_thres_Tmax_28 ~ "Hot",
    TRUE ~ "")) %>% 
  # --- two-day alert condition ---
  mutate(
    cond_two_day = if_else(Tmean >= pmax(21, met_thres_Tmean_23) & Tmax >= 27, 1, 0),
    met_two_days_alert = if_else(
      (cond_two_day == 1 & (lag(cond_two_day) == 1 | lead(cond_two_day) == 1)),
      1, 0)
  ) %>% ungroup() %>% 
  mutate(met_alert = if_else(
    (!is.na(met_one_day_alert) & met_one_day_alert != "") | met_two_days_alert == 1,
    1, 0)) %>%
  mutate(met_heat_day = if_else(met_alert > 0, 1, 0)) %>% 
  mutate(met_excess_heat = pmax(0, 
                                pmax(ifelse(!is.na(met_one_day_alert), Tmax - met_thres_Tmax_28, NA),
                                     Tmean - pmax(21, met_thres_Tmean_23),
                                     na.rm = TRUE))) %>% 
  arrange(station_id, year, month, day) %>%  # make sure data is ordered by date
  group_by(station_id) %>%                  # process each station_id separately
  mutate(
    # get run-length encoding of met_heat_day
    rle_id = rep(seq_along(rle(met_heat_day)$lengths), times = rle(met_heat_day)$lengths),
    rle_value = rep(rle(met_heat_day)$values, times = rle(met_heat_day)$lengths),
    met_heatwave_1days = ifelse(rle_value == 1 & ave(rle_value, rle_id, FUN = length) >= 1, 1, 0)
  ) %>% ungroup() %>%
  select(-rle_id, -rle_value) %>% 
  # create episode id for consecutive heatwave days
  mutate(met_episode_id_1days = ifelse(met_heatwave_1days == 1,
                                       cumsum(c(0, diff(met_heatwave_1days)) == 1),
                                       NA_integer_)) %>%
  ungroup() %>%
  # calculate duration of each heatwave episode: 1days
  group_by(station_id, met_episode_id_1days) %>%
  mutate(
    met_duration_days_1days = ifelse(!is.na(met_episode_id_1days), n(), NA_integer_)) %>%
  ungroup()

# Threshold Tmax = 28/Tmean = 23
met_summer_2923 <- summer_data %>%
  mutate(met_one_day_alert = case_when(
    Tmax >= met_thres_Tmax_29 + 4 ~ "Extremely hot",
    Tmax >= met_thres_Tmax_29 + 1 ~ "Very hot",
    Tmax >= met_thres_Tmax_29 ~ "Hot",
    TRUE ~ "")) %>% 
  # --- two-day alert condition ---
  mutate(
    cond_two_day = if_else(Tmean >= pmax(21, met_thres_Tmean_23) & Tmax >= 27, 1, 0),
    met_two_days_alert = if_else(
      (cond_two_day == 1 & (lag(cond_two_day) == 1 | lead(cond_two_day) == 1)),
      1, 0)
  ) %>% ungroup() %>% 
  mutate(met_alert = if_else(
    (!is.na(met_one_day_alert) & met_one_day_alert != "") | met_two_days_alert == 1,
    1, 0)) %>%
  mutate(met_heat_day = if_else(met_alert > 0, 1, 0)) %>% 
  mutate(met_excess_heat = pmax(0, 
                                pmax(ifelse(!is.na(met_one_day_alert), Tmax - met_thres_Tmax_29, NA),
                                     Tmean - pmax(21, met_thres_Tmean_23),
                                     na.rm = TRUE))) %>% 
  arrange(station_id, year, month, day) %>%  # make sure data is ordered by date
  group_by(station_id) %>%                  # process each station_id separately
  mutate(
    # get run-length encoding of met_heat_day
    rle_id = rep(seq_along(rle(met_heat_day)$lengths), times = rle(met_heat_day)$lengths),
    rle_value = rep(rle(met_heat_day)$values, times = rle(met_heat_day)$lengths),
    met_heatwave_1days = ifelse(rle_value == 1 & ave(rle_value, rle_id, FUN = length) >= 1, 1, 0)
  ) %>% ungroup() %>%
  select(-rle_id, -rle_value) %>% 
  # create episode id for consecutive heatwave days
  mutate(met_episode_id_1days = ifelse(met_heatwave_1days == 1,
                                       cumsum(c(0, diff(met_heatwave_1days)) == 1),
                                       NA_integer_)) %>%
  ungroup() %>%
  # calculate duration of each heatwave episode: 1days
  group_by(station_id, met_episode_id_1days) %>%
  mutate(
    met_duration_days_1days = ifelse(!is.na(met_episode_id_1days), n(), NA_integer_)) %>%
  ungroup()

# Threshold Tmax = 30/Tmean = 24
met_summer_3024 <- summer_data %>%
  mutate(met_one_day_alert = case_when(
    Tmax >= met_thres_Tmax_30 + 4 ~ "Extremely hot",
    Tmax >= met_thres_Tmax_30 + 1 ~ "Very hot",
    Tmax >= met_thres_Tmax_30 ~ "Hot",
    TRUE ~ "")) %>% 
  # --- two-day alert condition ---
  mutate(
    cond_two_day = if_else(Tmean >= pmax(21, met_thres_Tmean_24) & Tmax >= 27, 1, 0),
    met_two_days_alert = if_else(
      (cond_two_day == 1 & (lag(cond_two_day) == 1 | lead(cond_two_day) == 1)),
      1, 0)
  ) %>% ungroup() %>% 
  mutate(met_alert = if_else(
    (!is.na(met_one_day_alert) & met_one_day_alert != "") | met_two_days_alert == 1,
    1, 0)) %>%
  mutate(met_heat_day = if_else(met_alert > 0, 1, 0)) %>% 
  mutate(met_excess_heat = pmax(0, 
                                pmax(ifelse(!is.na(met_one_day_alert), Tmax - met_thres_Tmax_30, NA),
                                     Tmean - pmax(21, met_thres_Tmean_24),
                                     na.rm = TRUE))) %>% 
  arrange(station_id, year, month, day) %>%  # make sure data is ordered by date
  group_by(station_id) %>%                  # process each station_id separately
  mutate(
    # get run-length encoding of met_heat_day
    rle_id = rep(seq_along(rle(met_heat_day)$lengths), times = rle(met_heat_day)$lengths),
    rle_value = rep(rle(met_heat_day)$values, times = rle(met_heat_day)$lengths),
    met_heatwave_1days = ifelse(rle_value == 1 & ave(rle_value, rle_id, FUN = length) >= 1, 1, 0)
  ) %>% ungroup() %>%
  select(-rle_id, -rle_value) %>% 
  # create episode id for consecutive heatwave days
  mutate(met_episode_id_1days = ifelse(met_heatwave_1days == 1,
                                       cumsum(c(0, diff(met_heatwave_1days)) == 1),
                                       NA_integer_)) %>%
  ungroup() %>%
  # calculate duration of each heatwave episode: 1days
  group_by(station_id, met_episode_id_1days) %>%
  mutate(
    met_duration_days_1days = ifelse(!is.na(met_episode_id_1days), n(), NA_integer_)) %>%
  ungroup()

# Export to Excel
# Load the existing file
wb <- loadWorkbook(file.path(save_path, "heatwaves_summer.xlsx"))

# Add met_summer_2723 sheet
if ("met_summer_2723" %in% names(wb)) removeWorksheet(wb, "met_summer_2723")
addWorksheet(wb, "met_summer_2723")
writeData(wb, "met_summer_2723", met_summer_2723)

# Add met_summer_2823 sheet
if ("met_summer_2823" %in% names(wb)) removeWorksheet(wb, "met_summer_2823")
addWorksheet(wb, "met_summer_2823")
writeData(wb, "met_summer_2823", met_summer_2823)

# Add met_summer_2923 sheet
if ("met_summer_2923" %in% names(wb)) removeWorksheet(wb, "met_summer_2923")
addWorksheet(wb, "met_summer_2923")
writeData(wb, "met_summer_2923", met_summer_2923)

# Add met_summer_3024 sheet
if ("met_summer_3024" %in% names(wb)) removeWorksheet(wb, "met_summer_3024")
addWorksheet(wb, "met_summer_3024")
writeData(wb, "met_summer_3024", met_summer_3024)

# Save without removing existing sheets
saveWorkbook(wb, file.path(save_path, "heatwaves_summer.xlsx"), overwrite = TRUE)

#----------------------------
# Table: Number of heatwave episodes and heatwaves day
#----------------------------
met_summer_all <- bind_rows(
  met_summer_2723 %>% mutate(threshold = "Tmax = 27C/Tmean = 23C"),
  met_summer_2823 %>% mutate(threshold = "Tmax = 28C/Tmean = 23C"),
  met_summer_2923 %>% mutate(threshold = "Tmax = 29C/Tmean = 23C"),
  met_summer_3024 %>% mutate(threshold = "Tmax = 30C/Tmean = 24C")
)

met_summary_season <- met_summer_all %>%
  group_by(station_id, threshold, season) %>%
  summarise(
    num_norm_episodes = n_distinct(met_episode_id_1days, na.rm = TRUE),  # count episodes ignoring NA
    num_norm_hw_days = sum(met_heatwave_1days, na.rm = TRUE),             # total heatwave days
    total_norm_excess_temp = sprintf("%.1f",sum(ifelse(!is.na(met_episode_id_1days), met_excess_heat, 0), na.rm = TRUE),1),
    .groups = "drop"
  )

# Export to Excel
# Load the existing file
wb <- loadWorkbook(file.path(save_path, "heatwaves_summer.xlsx"))
# Add as a new sheet
if ("met_summary_season" %in% names(wb)) removeWorksheet(wb, "met_summary_season")
addWorksheet(wb, "met_summary_season")
writeData(wb, "met_summary_season", met_summary_season)
# Save without removing existing sheets
saveWorkbook(wb, file.path(save_path, "heatwaves_summer.xlsx"), overwrite = TRUE)

#------------------------------------------------
## Figure 13: Number of heatwave episodes and heatwaves day, MET approach
#-------------------------------------------------
# threshold = "Tmax = 27C/Tmean = 23C"
met_heatwaves_2723 <- met_summary_season %>%
  filter(threshold == "Tmax = 27C/Tmean = 23C") %>% 
  mutate(across(c(num_norm_episodes, num_norm_hw_days, total_norm_excess_temp), as.numeric)) %>%
  pivot_longer(
    cols = c(num_norm_episodes, num_norm_hw_days, total_norm_excess_temp),
    names_to = "metric",
    values_to = "value") %>%
  mutate(
    # Make season an ordered factor with desired order reversed (top to bottom)
    season = factor(season, levels = rev(sort(unique(season)))),
    # Recode metric for nicer labels and factor for order of facets
    metric = recode(metric,
                    num_norm_episodes = "Number of Heatwave Episodes",
                    num_norm_hw_days = "Number of Heatwave Days",
                    total_norm_excess_temp = "Heatwave Excess Temperature (°C)"),
    metric = factor(metric, levels = c("Number of Heatwave Episodes", 
                                       "Number of Heatwave Days", 
                                       "Heatwave Excess Temperature (°C)")),
    x_max = case_when(str_detect(metric, "Episodes") ~ 10,
                             str_detect(metric, "Days") ~ 70,
                             str_detect(metric, "Excess Temperature") ~ 150)) %>% 
  filter(metric %in% c("Number of Heatwave Episodes",
                       "Number of Heatwave Days",
                       "Heatwave Excess Temperature (°C)")) %>%
  ggplot(aes(x = value, y = season)) +
  geom_col(fill = "#85b640") +
  geom_text(aes(label = ifelse(value == 0, "", 
                               ifelse(metric == "Heatwave Excess Temperature (°C)",
                                      sprintf("%.1f", value),  # always 1 decimal place
                                      as.character(value)))) ,    # keep integers as they are
            hjust = -0.2,   # pushes labels slightly outside bar
            size = 3.5) +
  # geom_blank ensures x-axis extends to x_max for each facet
  geom_blank(aes(x = x_max)) +
  facet_wrap(~ metric, scales = "free_x", strip.position = "top") +
  labs(
    x = "",
    y = "MetService \nseason",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12, angle = 0, 
                                    hjust = 0, vjust = 1.1, 
                                    margin = margin(r = -72)),
        strip.text = element_text(face = "bold", hjust = 0, size = 10) # Bold + left align
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    breaks = function(x) pretty(x, n = 4),  # automatic nice breaks
    expand = expansion(mult = c(0, 0.05))  # small extra space on the right
  )

# threshold = "Tmax = 28C/Tmean = 23C"
met_heatwaves_2823 <- met_summary_season %>%
  filter(threshold == "Tmax = 28C/Tmean = 23C") %>% 
  mutate(across(c(num_norm_episodes, num_norm_hw_days, total_norm_excess_temp), as.numeric)) %>%
  pivot_longer(
    cols = c(num_norm_episodes, num_norm_hw_days, total_norm_excess_temp),
    names_to = "metric",
    values_to = "value") %>%
  mutate(
    # Make season an ordered factor with desired order reversed (top to bottom)
    season = factor(season, levels = rev(sort(unique(season)))),
    # Recode metric for nicer labels and factor for order of facets
    metric = recode(metric,
                    num_norm_episodes = "Number of Heatwave Episodes",
                    num_norm_hw_days = "Number of Heatwave Days",
                    total_norm_excess_temp = "Heatwave Excess Temperature (°C)"),
    metric = factor(metric, levels = c("Number of Heatwave Episodes", 
                                       "Number of Heatwave Days", 
                                       "Heatwave Excess Temperature (°C)")),
    x_max = case_when(str_detect(metric, "Episodes") ~ 10,
                      str_detect(metric, "Days") ~ 70,
                      str_detect(metric, "Excess Temperature") ~ 150)) %>% 
  filter(metric %in% c("Number of Heatwave Episodes",
                       "Number of Heatwave Days",
                       "Heatwave Excess Temperature (°C)")) %>%
  ggplot(aes(x = value, y = season)) +
  geom_col(fill = "#85b640") +
  geom_text(aes(label = ifelse(value == 0, "", 
                               ifelse(metric == "Heatwave Excess Temperature (°C)",
                                      sprintf("%.1f", value),  # always 1 decimal place
                                      as.character(value)))) ,    # keep integers as they are
            hjust = -0.2,   # pushes labels slightly outside bar
            size = 3.5) +
  # geom_blank ensures x-axis extends to x_max for each facet
  geom_blank(aes(x = x_max)) +
  facet_wrap(~ metric, scales = "free_x", strip.position = "top") +
  labs(
    x = "",
    y = "MetService \nseason",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12, angle = 0, 
                                    hjust = 0, vjust = 1.1, 
                                    margin = margin(r = -72)),
        strip.text = element_text(face = "bold", hjust = 0, size = 10) # Bold + left align
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    breaks = function(x) pretty(x, n = 4),  # automatic nice breaks
    expand = expansion(mult = c(0, 0.05))  # small extra space on the right
  )

# threshold = "Tmax = 29C/Tmean = 23C"
met_heatwaves_2923 <- met_summary_season %>%
  filter(threshold == "Tmax = 29C/Tmean = 23C") %>% 
  mutate(across(c(num_norm_episodes, num_norm_hw_days, total_norm_excess_temp), as.numeric)) %>%
  pivot_longer(
    cols = c(num_norm_episodes, num_norm_hw_days, total_norm_excess_temp),
    names_to = "metric",
    values_to = "value") %>%
  mutate(
    # Make season an ordered factor with desired order reversed (top to bottom)
    season = factor(season, levels = rev(sort(unique(season)))),
    # Recode metric for nicer labels and factor for order of facets
    metric = recode(metric,
                    num_norm_episodes = "Number of Heatwave Episodes",
                    num_norm_hw_days = "Number of Heatwave Days",
                    total_norm_excess_temp = "Heatwave Excess Temperature (°C)"),
    metric = factor(metric, levels = c("Number of Heatwave Episodes", 
                                       "Number of Heatwave Days", 
                                       "Heatwave Excess Temperature (°C)")),
    x_max = case_when(str_detect(metric, "Episodes") ~ 10,
                      str_detect(metric, "Days") ~ 70,
                      str_detect(metric, "Excess Temperature") ~ 150)) %>% 
  filter(metric %in% c("Number of Heatwave Episodes",
                       "Number of Heatwave Days",
                       "Heatwave Excess Temperature (°C)")) %>%
  ggplot(aes(x = value, y = season)) +
  geom_col(fill = "#85b640") +
  geom_text(aes(label = ifelse(value == 0, "", 
                               ifelse(metric == "Heatwave Excess Temperature (°C)",
                                      sprintf("%.1f", value),  # always 1 decimal place
                                      as.character(value)))) ,    # keep integers as they are
            hjust = -0.2,   # pushes labels slightly outside bar
            size = 3.5) +
  # geom_blank ensures x-axis extends to x_max for each facet
  geom_blank(aes(x = x_max)) +
  facet_wrap(~ metric, scales = "free_x", strip.position = "top") +
  labs(
    x = "",
    y = "MetService \nseason",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12, angle = 0, 
                                    hjust = 0, vjust = 1.1, 
                                    margin = margin(r = -72)),
        strip.text = element_text(face = "bold", hjust = 0, size = 10) # Bold + left align
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    breaks = function(x) pretty(x, n = 4),  # automatic nice breaks
    expand = expansion(mult = c(0, 0.05))  # small extra space on the right
  )

# threshold = "Tmax = 30C/Tmean = 24C"
met_heatwaves_3024 <- met_summary_season %>%
  filter(threshold == "Tmax = 30C/Tmean = 24C") %>% 
  mutate(across(c(num_norm_episodes, num_norm_hw_days, total_norm_excess_temp), as.numeric)) %>%
  pivot_longer(
    cols = c(num_norm_episodes, num_norm_hw_days, total_norm_excess_temp),
    names_to = "metric",
    values_to = "value") %>%
  mutate(
    # Make season an ordered factor with desired order reversed (top to bottom)
    season = factor(season, levels = rev(sort(unique(season)))),
    # Recode metric for nicer labels and factor for order of facets
    metric = recode(metric,
                    num_norm_episodes = "Number of Heatwave Episodes",
                    num_norm_hw_days = "Number of Heatwave Days",
                    total_norm_excess_temp = "Heatwave Excess Temperature (°C)"),
    metric = factor(metric, levels = c("Number of Heatwave Episodes", 
                                       "Number of Heatwave Days", 
                                       "Heatwave Excess Temperature (°C)")),
    x_max = case_when(str_detect(metric, "Episodes") ~ 10,
                      str_detect(metric, "Days") ~ 70,
                      str_detect(metric, "Excess Temperature") ~ 150)) %>% 
  filter(metric %in% c("Number of Heatwave Episodes",
                       "Number of Heatwave Days",
                       "Heatwave Excess Temperature (°C)")) %>%
  ggplot(aes(x = value, y = season)) +
  geom_col(fill = "#85b640") +
  geom_text(aes(label = ifelse(value == 0, "", 
                               ifelse(metric == "Heatwave Excess Temperature (°C)",
                                      sprintf("%.1f", value),  # always 1 decimal place
                                      as.character(value)))) ,    # keep integers as they are
            hjust = -0.2,   # pushes labels slightly outside bar
            size = 3.5) +
  # geom_blank ensures x-axis extends to x_max for each facet
  geom_blank(aes(x = x_max)) +
  facet_wrap(~ metric, scales = "free_x", strip.position = "top") +
  labs(
    x = "",
    y = "MetService \nseason",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12, angle = 0, 
                                    hjust = 0, vjust = 1.1, 
                                    margin = margin(r = -72)),
        strip.text = element_text(face = "bold", hjust = 0, size = 10) # Bold + left align
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    breaks = function(x) pretty(x, n = 4),  # automatic nice breaks
    expand = expansion(mult = c(0, 0.05))  # small extra space on the right
  )

# save the plot
full_path_1 <- file.path(save_path, "fig13a_met_heatwaves_2723.png")
full_path_2 <- file.path(save_path, "fig13b_met_heatwaves_2823.png")
full_path_3 <- file.path(save_path, "fig13a_met_heatwaves_2923.png")
full_path_4 <- file.path(save_path, "fig13b_met_heatwaves_3024.png")

ggsave(full_path_1, plot = met_heatwaves_2723, width = 10, height = 6, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA))

ggsave(full_path_2, plot = met_heatwaves_2823, width = 10, height = 6, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA))

ggsave(full_path_3, plot = met_heatwaves_2923, width = 10, height = 6, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA))

ggsave(full_path_4, plot = met_heatwaves_3024, width = 10, height = 6, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA))

#------------------------------------------------
## Figure 14: TEN WORST HEATWAVES EPISODES BASED ON EXCESS TEMPERATURE
#-------------------------------------------------

met_hw_episode <- met_summer_all %>%
  filter(!is.na(met_episode_id_1days)) %>%                # Remove NA episodes
  group_by(station_id, threshold, met_episode_id_1days, season) %>%
  summarise(
    Duration = mean(met_duration_days_1days, na.rm = TRUE),
    `Heatwave Excess temperature (°C)` = sum(met_excess_heat, na.rm = TRUE),
    `Tmax (average)` = mean(Tmax, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    station_id = station_id,
    Episode = met_episode_id_1days,
    Season = season
  ) %>%
  group_by(threshold) %>%   # <-- key step
  arrange(desc(`Heatwave Excess temperature (°C)`), .by_group = TRUE) %>%
  ungroup()

# Export to Excel
# Load the existing file
wb <- loadWorkbook(file.path(save_path, "heatwaves_summer.xlsx"))

# Add as a new sheet
if ("met_hw_episode" %in% names(wb)) removeWorksheet(wb, "met_hw_episode")
addWorksheet(wb, "met_hw_episode")
writeData(wb, "met_hw_episode", met_hw_episode)

# Save without removing existing sheets
saveWorkbook(wb, file.path(save_path, "heatwaves_summer.xlsx"), overwrite = TRUE)


met_top10 <- met_hw_episode %>%
  group_by(threshold) %>% 
  arrange(desc(`Heatwave Excess temperature (°C)`)) %>%
  slice_head(n = 10) %>%  ungroup()

# Scatter plot
met_worst_hw <- ggplot(met_top10, aes(x = Duration, y = `Tmax (average)`)) +
  geom_point(aes(size = `Heatwave Excess temperature (°C)`), color = "#85b640", alpha = 0.8) +
  geom_label_repel(
    aes(label = paste0("Ep",Episode, " - S", Season )),
    fill = "#f9f4e7",                 # Soft warm beige
    color = "#2b2b2b",                 # Dark gray text for contrast
    size = 3,                        # Font size
    label.size = 0.2,                  # Border thickness
    label.colour = "#85b640",          # Muted gold border
    label.r = unit(0.15, "lines"),     # Slightly more rounded corners
    box.padding = 0.5,                 
    label.padding = unit(0.2, "lines"),
    segment.color = "#85b640",         # Arrow color matches border
    max.overlaps = Inf
  ) +
  scale_size_continuous(range = c(2, 8), breaks = c(1,5,10), limits = c(0, 10)) +
  scale_x_continuous(
    limits = c(0, 6),
    breaks = pretty_breaks(n = 7)  # auto-rounded breaks for x-axis
  ) +
  scale_y_continuous(
    limits = c(25, 30),
    breaks = pretty_breaks(n = 5)  # auto-rounded breaks for y-axis
  ) +
  labs(
    x = "duration",
    y = bquote(bold("average") ~ bold(T[max])),
    size = "Heatwave \nExcess temperature (°C)"
  ) +
  theme_light()+
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "white", color = "grey90"), 
    legend.key.size = unit(0.3, "lines"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.box.margin = margin(1, 1, 1, 1),
    legend.spacing = unit(0.1, "lines"),
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 0),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.08, margin = margin(r = -70)),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 30, r = 5, b = 5, l = 5)) +  
  facet_wrap(~threshold) +
  theme(strip.text = element_text(color = "black",        # text color
                                  face = "bold"))
        # strip.background = element_rect(
        #   fill = "#F7F7F7",       # background color of facet label
        #   color = "black"))         # border of strip
met_worst_hw

full_path_1 <- file.path(save_path, "fig14a_met_worst_heatwaves.png")

ggsave(full_path_1, plot = met_worst_hw, width = 8, height = 7, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA))

#-----------------------------------------------------
#-------------------------------------------------------
#---- COMPARATION TWO APPROACHES: EHF and MetService -----------------------
#------------------------------------------------------
#-----------------------------------------------------
# use ehf_summer,  met_summer_all
# join data
#ehf_summer$month <- month.abb[as.numeric(ehf_summer$month)]
ehf_summer <- ehf_summer %>% mutate(month = factor(month, levels = 1:12, labels = month.abb))
met_summer_all <- met_summer_all %>% mutate(month = as.character(month))

#merged_summer <- ehf_summer %>%
#  full_join(met_summer, by = c("station_id", "season", "date", "year", "month", "day", "Tmax", "Tmin", "Tmean")) %>% 
#  mutate(all_heatwave = if_else((ehf_heatwave == 1 |  met_heatwave_1days == 1), 1, 0),
#         all_severe_heatwave = if_else((ehf_severe_heatwave == 1 |  met_heatwave_1days == 1), 1, 0),) 

#Combine ehf & Metservice
# total_days_all <- sum(merged_summer$all_heatwave == 1, na.rm = TRUE)
# total_days_severe <- sum(merged_summer$all_severe_heatwave == 1, na.rm = TRUE)

#ehf
ehf_days_all <- sum(ehf_summer$ehf_heatwave == 1, na.rm = TRUE)
ehf_days_severe_all <- sum(ehf_summer$ehf_severe_heatwave == 1, na.rm = TRUE)
#MetService
# met_days_all_2723 <- sum(met_summer_2723$met_heatwave_1days == 1, na.rm = TRUE)
# met_days_all_2823 <- sum(met_summer_2823$met_heatwave_1days == 1, na.rm = TRUE)
# met_days_all_2923 <- sum(met_summer_2923$met_heatwave_1days == 1, na.rm = TRUE)
# met_days_all_3024 <- sum(met_summer_3024$met_heatwave_1days == 1, na.rm = TRUE)

ehf_all_heatwave <- ehf_summer %>% 
  mutate( month = case_when(
      month %in% c("1", "Jan")   ~ "January",
      month %in% c("2", "Feb")   ~ "February",
      month %in% c("3", "Mar")   ~ "March",
      month %in% c("11", "Nov")  ~ "November",
      month %in% c("12", "Dec")  ~ "December",
      TRUE ~ month)) %>% 
  group_by(station_id, month) %>%
  summarise(
    ehf_days = sum(ehf_heatwave == 1, na.rm = TRUE),  # ehf heatwave days
    ehf_perc_days = ehf_days / ehf_days_all * 100,
    ehf_days_severe = sum(ehf_severe_heatwave == 1, na.rm = TRUE),  # ehf severe heatwave days
    ehf_perc_days_severe = ehf_days_severe / ehf_days_severe_all * 100
  )

met_all_heatwave <- met_summer_all %>% 
  mutate(
    month = case_when(
      month %in% c("1", "Jan")  ~ "January",
      month %in% c("2", "Feb")  ~ "February",
      month %in% c("3", "Mar")  ~ "March",
      month %in% c("11", "Nov") ~ "November",
      month %in% c("12", "Dec") ~ "December",
      TRUE ~ month
    )
  ) %>% 
  group_by(station_id, threshold, month) %>%
  summarise(
    met_days = sum(met_heatwave_1days == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  group_by(station_id, threshold) %>% 
  mutate(
    met_days_all = sum(met_days, na.rm = TRUE),
    met_perc_days = met_days / met_days_all * 100
  ) %>% ungroup()

#-------------------------------------------------------------------
## FIGURE 15: TOTAL HEATWAVES DAYS , BY SUMMER MONTH, 2000/01 -2019/20
#-------------------------------------------------------------------

# plot 
# make month a factor with custom order
# summary_all_heatwave <- summary_all_heatwave %>%
#   mutate(month = factor(month, levels = c("November", "December", "January", "February", "March")))

# EHF only
ehf_hw_by_month <- ehf_all_heatwave %>% 
  mutate(month = factor(month,
                   levels = c("November", "December", "January", "February", "March"))) %>% 
  ggplot(aes(x = month, y = ehf_perc_days)) +
  geom_col(width = 0.7, fill = "#85b640") +
  labs(x = "Month", y = "heatwave days (%)") +
  geom_text(aes(label = sprintf("%.1f%%", ehf_perc_days), y = 0),  # start from bottom
            vjust = -0.8,                                       # adjust vertical position inside bar
            color = "black", size = 4) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(limits = c(0, 60)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 7, size = 12),
    legend.position = "none",
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 4),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.07, margin = margin(r = -100)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 30, r = 10, b = 10, l = 10)
  )

ehf_severe_hw_by_month <- ehf_all_heatwave %>% 
  mutate(month = factor(month,
                        levels = c("November", "December", "January", "February", "March"))) %>% 
  ggplot(aes(x = month, y = ehf_perc_days_severe)) +
  geom_col(width = 0.7, fill = "#85b640") +
  labs(x = "Month", y = "heatwave days (%)") +
  geom_text(aes(label = sprintf("%.1f%%", ehf_perc_days_severe), y = 0),  # start from bottom
            vjust = -0.8,                                       # adjust vertical position inside bar
            color = "black", size = 4) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(limits = c(0, 60)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 7, size = 12),
    legend.position = "none",
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 4),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.07, margin = margin(r = -100)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 30, r = 10, b = 10, l = 10)
  )
# MetService
met_hw_by_month_2723 <- met_all_heatwave %>% 
  filter(threshold == "Tmax = 27C/Tmean = 23C") %>% 
  mutate(month = factor(month,
                        levels = c("November", "December", "January", "February", "March"))) %>% 
  ggplot(aes(x = month, y = met_perc_days)) +
  geom_col(width = 0.7, fill = "#85b640") +
  labs(x = "Month", y = "heatwave days (%)") +
  geom_text(aes(label = sprintf("%.1f%%", met_perc_days), y = 0),  # start from bottom
            vjust = -0.8,                                       # adjust vertical position inside bar
            color = "black", size = 4) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(limits = c(0, 60)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 7, size = 12),
    legend.position = "none",
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 4),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.07, margin = margin(r = -100)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 30, r = 10, b = 10, l = 10))

met_hw_by_month_2823 <- met_all_heatwave %>% 
  filter(threshold == "Tmax = 28C/Tmean = 23C") %>% 
  mutate(month = factor(month,
                        levels = c("November", "December", "January", "February", "March"))) %>% 
  ggplot(aes(x = month, y = met_perc_days)) +
  geom_col(width = 0.7, fill = "#85b640") +
  labs(x = "Month", y = "heatwave days (%)") +
  geom_text(aes(label = sprintf("%.1f%%", met_perc_days), y = 0),  # start from bottom
            vjust = -0.8,                                       # adjust vertical position inside bar
            color = "black", size = 4) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(limits = c(0, 60)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 7, size = 12),
    legend.position = "none",
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 4),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.07, margin = margin(r = -100)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 30, r = 10, b = 10, l = 10))

met_hw_by_month_2923 <- met_all_heatwave %>% 
  filter(threshold == "Tmax = 29C/Tmean = 23C") %>% 
  mutate(month = factor(month,
                        levels = c("November", "December", "January", "February", "March"))) %>% 
  ggplot(aes(x = month, y = met_perc_days)) +
  geom_col(width = 0.7, fill = "#85b640") +
  labs(x = "Month", y = "heatwave days (%)") +
  geom_text(aes(label = sprintf("%.1f%%", met_perc_days), y = 0),  # start from bottom
            vjust = -0.8,                                       # adjust vertical position inside bar
            color = "black", size = 4) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(limits = c(0, 60)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 7, size = 12),
    legend.position = "none",
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 4),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.07, margin = margin(r = -100)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 30, r = 10, b = 10, l = 10))

met_hw_by_month_3024 <- met_all_heatwave %>% 
  filter(threshold == "Tmax = 30C/Tmean = 24C") %>% 
  mutate(month = factor(month,
                        levels = c("November", "December", "January", "February", "March"))) %>% 
  ggplot(aes(x = month, y = met_perc_days)) +
  geom_col(width = 0.7, fill = "#85b640") +
  labs(x = "Month", y = "heatwave days (%)") +
  geom_text(aes(label = sprintf("%.1f%%", met_perc_days), y = 0),  # start from bottom
            vjust = -0.8,                                       # adjust vertical position inside bar
            color = "black", size = 4) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(limits = c(0, 100)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 7, size = 12),
    legend.position = "none",
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 4),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.07, margin = margin(r = -100)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 30, r = 10, b = 10, l = 10))

# save the plot
# Create a filename

full_path_1 <- file.path(save_path, "Fig15a_ehf_hw_days_by_month.png")
full_path_2 <- file.path(save_path, "Fig15b_ehf_severe_hw_days_by_month.png")
full_path_3 <- file.path(save_path, "Fig15c_met_2723_hw_days_by_month.png")
full_path_4 <- file.path(save_path, "Fig15c_met_2823_hw_days_by_month.png")
full_path_5 <- file.path(save_path, "Fig15c_met_2923_hw_days_by_month.png")
full_path_6 <- file.path(save_path, "Fig15c_met_3024_hw_days_by_month.png")


ggsave(full_path_1, plot = ehf_hw_by_month, width = 6, height = 4, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA))

ggsave(full_path_2, plot = ehf_severe_hw_by_month, width = 6, height = 4, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA))

ggsave(full_path_3, plot = met_hw_by_month_2723, width = 6, height = 4, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA))

ggsave(full_path_4, plot = met_hw_by_month_2823, width = 6, height = 4, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA))

ggsave(full_path_5, plot = met_hw_by_month_2923, width = 6, height = 4, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA))

ggsave(full_path_6, plot = met_hw_by_month_3024, width = 6, height = 4, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA))


#----------------------------------------------------------------------
# FIGURE 16: COMPARATION OF HEATWAVE DURATION VS TMAX
#---------------------------------------------------------------------

# Combine plot
# Add a column for approach in each dataset
ehf_hw_episode <- ehf_hw_episode %>% mutate(Approach = "EHF - all heatwaves")
ehf_severe_hw_episode <- ehf_severe_hw_episode %>% mutate(Approach = "EHF - severe heatwaves")
met_hw_episode <- met_hw_episode %>% mutate(Approach = "MetService")

# Combine all datasets
combined_hw <- bind_rows(ehf_hw_episode %>% mutate(threshold = "Tmax = 27C/Tmean = 23C"),
                         ehf_hw_episode %>% mutate(threshold = "Tmax = 28C/Tmean = 23C"),
                         ehf_hw_episode %>% mutate(threshold = "Tmax = 29C/Tmean = 23C"),
                         ehf_hw_episode %>% mutate(threshold = "Tmax = 30C/Tmean = 24C"),
                         
                         ehf_severe_hw_episode %>% mutate(threshold = "Tmax = 27C/Tmean = 23C"),
                         ehf_severe_hw_episode %>% mutate(threshold = "Tmax = 28C/Tmean = 23C"),
                         ehf_severe_hw_episode %>% mutate(threshold = "Tmax = 29C/Tmean = 23C"),
                         ehf_severe_hw_episode %>% mutate(threshold = "Tmax = 30C/Tmean = 24C"),
                         
                         met_hw_episode) %>% 
  mutate(`Heatwave Excess temperature (°C)` = if_else(
    Approach == "EHF - severe heatwaves",
    `Severe Heatwave Excess temperature (°C)`,
    `Heatwave Excess temperature (°C)`
  ))

# Plot
hw_plot <- combined_hw %>% 
  ggplot(aes(x = Duration, y = `Tmax (average)`,
                      size = `Heatwave Excess temperature (°C)`,
                      color = Approach)) +
  geom_point(alpha = 0.8) +
  scale_size_continuous(range = c(2, 12), breaks = c(10, 30, 60, 90)) +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  scale_color_manual(
    values = c(
      "MetService" = "red",
      "EHF - all heatwaves" = "#85b640",
      "EHF - severe heatwaves" = "#285a34")) +
  labs(
    x = "duration",
    y = bquote(bold("average") ~ bold(T[max])),
    size = "Heatwave Excess Temperature (°C)",
    color = "Approaches"
  ) +
  theme_light() +
  facet_wrap(~threshold)+
  theme(strip.text = element_text(color = "black",        # text color
                                  face = "bold"),
        # strip.background = element_rect(
        #   fill = "#F7F7F7",       # background color of facet label
        #   color = "white"), #border
    legend.position = "top",
    legend.box = "vertical",
    legend.background = element_rect(fill = "white", color = "grey90"),
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 0),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.09, margin = margin(r = -70)),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 30, r = 5, b = 5, l = 5)) +
  guides(
    size = guide_legend(order = 2, nrow = 1, byrow = TRUE, override.aes = list(fill = "white", color = "black", shape = 21)),
    color = guide_legend(order = 1, nrow = 1, byrow = TRUE)
  )
hw_plot

# save the plot
file_name <- "fig16_hw_duration_comparation.png"
# Full path
full_path <- file.path(save_path, file_name)

ggsave(full_path, plot = hw_plot, width = 9, height = 7, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA)
)
#----------------------------------------------------------------------
# FIGURE 17: COMPARATION OF HEATWAVE DURATION VS TMAX: TOP 10 HW
#----------------------------------------------------------------------

# Combine plot
# Add a column for approach in each dataset
ehf_top10 <- ehf_top10 %>% mutate(Approach = "EHF - all heatwaves")
ehf_severe_top10 <- ehf_severe_top10 %>% mutate(Approach = "EHF - severe heatwaves")
met_top10 <- met_top10 %>% mutate(Approach = "MetService")

# Combine all datasets
top10_combined_hw <- bind_rows(ehf_top10 %>% mutate(threshold = "Tmax = 27C/Tmean = 23C"),
                               ehf_top10 %>% mutate(threshold = "Tmax = 28C/Tmean = 23C"),
                               ehf_top10 %>% mutate(threshold = "Tmax = 29C/Tmean = 23C"),
                               ehf_top10 %>% mutate(threshold = "Tmax = 30C/Tmean = 24C"),
                               
                               ehf_severe_top10 %>% mutate(threshold = "Tmax = 27C/Tmean = 23C"),
                               ehf_severe_top10 %>% mutate(threshold = "Tmax = 28C/Tmean = 23C"),
                               ehf_severe_top10 %>% mutate(threshold = "Tmax = 29C/Tmean = 23C"),
                               ehf_severe_top10 %>% mutate(threshold = "Tmax = 30C/Tmean = 24C"),
                               
                               met_top10) %>% 
  mutate(`Heatwave Excess temperature (°C)` = if_else(
    Approach == "EHF - severe heatwaves",
    `Severe Heatwave Excess temperature (°C)`,
    `Heatwave Excess temperature (°C)`
  ))


# Plot
top10_hw_plot <- ggplot(top10_combined_hw,
                  aes(x = Duration, y = `Tmax (average)`,
                      size = `Heatwave Excess temperature (°C)`,
                      color = Approach)) +
  # geom_label_repel(
  #   aes(label = paste0("S", Season )),
  #   fill = "#f9f4e7",                 # Soft warm beige
  #   color = "#2b2b2b",                 # Dark gray text for contrast
  #   size = 3,                        # Font size
  #   label.size = 0.2,                  # Border thickness
  #   label.colour = "#85b640",          # Muted gold border
  #   label.r = unit(0.15, "lines"),     # Slightly more rounded corners
  #   box.padding = 0.5,                 
  #   label.padding = unit(0.2, "lines"),
  #   segment.color = "#85b640",         # Arrow color matches border
  #   max.overlaps = Inf
  # ) +
  geom_point(alpha = 0.8) +
  scale_size_continuous(range = c(2, 12), breaks = c(10, 30, 60, 90)) +
  scale_x_continuous(breaks = seq(0, 28, by = 2)) +
  scale_color_manual(
    values = c(
      "MetService" = "red",
      "EHF - all heatwaves" = "#85b640",
      "EHF - severe heatwaves" = "#285a34")) +
  labs(
    x = "duration",
    y = bquote(bold("average") ~ bold(T[max])),
    size = "Heatwave Excess Temperature (°C)",
    color = "Approaches"
  ) +
  theme_light() +
  facet_wrap(~threshold) +
  theme(strip.text = element_text(color = "black",        # text color
                                  face = "bold"),
        # strip.background = element_rect(
        #   fill = "#F7F7F7",       # background color of facet label
        #   color = "white"), #border
        legend.position = "top",
        legend.box = "vertical",
        legend.background = element_rect(fill = "white", color = "grey90"),
        axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 0),
        axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.09, margin = margin(r = -70)),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 30, r = 5, b = 5, l = 5)) +
  guides(
    size = guide_legend(order = 2, nrow = 1, byrow = TRUE, override.aes = list(fill = "white", color = "black", shape = 21)),
    color = guide_legend(order = 1, nrow = 1, byrow = TRUE)
  )
top10_hw_plot
# save the plot
file_name <- "fig17_top10_hw_duration_comparation.png"
# Full path
full_path <- file.path(save_path, file_name)

ggsave(full_path, plot = top10_hw_plot, width = 9, height = 7, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA)
)

# Calculate the number of worst episodes by season
top10_hw_summary <- top10_combined_hw %>%
  group_by(Season, threshold) %>%
  summarise(
    n_episodes = n_distinct(Episode),  # count unique episodes
    .groups = "drop")  %>%
  pivot_wider(
    names_from = threshold,
    values_from = n_episodes,
    values_fill = 0
  )
top10_hw_summary

# Export to Excel
# Export data to excel, both table in the different sheet
write_xlsx(
  list(
    "top10_hw_summary" = top10_hw_summary
  ),
  path = file.path(save_path, "comparation_all.xlsx")
)

# # Load the existing file
# comparation <- loadWorkbook(file.path(save_path, "comparation_all.xlsx"))
# 
# # Add as a new sheet
# if ("top10_hw_summary" %in% names(comparation)) removeWorksheet(comparation, "top10_hw_summary")
# addWorksheet(comparation, "top10_hw_summary")
# writeData(comparation, "top10_hw_summary", top10_hw_summary)
# 
# # Save without removing existing sheets
# saveWorkbook(comparation, file.path(save_path, "comparation_all.xlsx"), overwrite = TRUE)

##-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# MORTALITY DATA --------------------------------------------------------------
# -----------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Read first sheet
mort <- read_excel(file.path(mort_path, "excess_mortality.xlsx"), 
                   sheet = 1) 
#-------------------------------------------------------------------
## FIGURE 3/4: AVERAGE DAILY MAX TEMPERATURE, BY SUMMER MONTH/season, 2000/01 -2019/20
#-------------------------------------------------------------------
mort_summer <- mort %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  filter(date >= as.Date("2000-11-01") & date <= as.Date("2021-03-31")) %>% 
  filter(month %in% c(11, 12, 1, 2, 3)) %>%
  mutate(
    # Create 'season_year_start' which is the year when the season started
    season_year_start = if_else(month %in% c(11, 12), year, year - 1),
    # Create season label like "2000/01"
    season = paste0(season_year_start, "/", substr(season_year_start + 1, 3, 4))
  ) %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE))  # Dec, Jan, Feb

# Summarise by month
mort_summary <- mort_summer %>%
  group_by(month) %>%
  summarise(
    count_obs_mort = sum(!is.na(`observed mortality`)),
    obs_mort_avg = mean(`observed mortality`, na.rm = TRUE),
    obs_mort_sd = sd(`observed mortality`, na.rm = TRUE),
    error = 1.96 * obs_mort_sd / sqrt(count_obs_mort),
    lower_CI = obs_mort_avg - error,
    upper_CI = obs_mort_avg + error
  ) %>%
  arrange(month)

mort_total <- mort_summer %>%
  summarise(
    total_obs_count = sum(!is.na(`observed mortality`)),
    overall_min     = min(`observed mortality`, na.rm = TRUE),
    overall_max     = max(`observed mortality`, na.rm = TRUE)
  )

# Convert to long format for ggplot
mort_summary_long <- mort_summary %>%
  pivot_longer(cols = c(count_obs_mort, obs_mort_avg, obs_mort_sd),
               names_to = "variable", values_to = "value") %>%
  filter(variable == "obs_mort_avg",
         month %in% c("Nov", "Dec", "Jan", "Feb", "Mar")) %>%
  mutate(
    month_full = recode(month,
                        "Nov" = "November",
                        "Dec" = "December",
                        "Jan" = "January",
                        "Feb" = "February",
                        "Mar" = "March"),
    month_full = factor(month_full,
                        levels = c("November", "December", "January", "February", "March")))

# plot 
mort_aver_by_month <- mort_summary_long %>% 
  ggplot(aes(x = month_full, y = value)) +
  geom_col(width = 0.7, fill = "#85b640") +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), 
                width = 0.1, 
                color = "black", 
                size = 0.3) +
  labs(x = "month", y = "observed mortality (average)") +
  geom_text(aes(label =  sprintf("%.1f", value)), 
            position = position_stack(vjust = 0.04), 
            color = "black", size = 4) +   # label inside the bar
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 6, size = 12),
    legend.position = "none",
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 4),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.04, margin = margin(r = -150)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 30, r = 10, b = 10, l = 10)
  ) +
  scale_y_continuous(breaks = seq(0, 20, 2), limits = c(0, 18))  # set grid at 0,2,4,6,8,10

# save the plot
# Create a filename
file_name <- "Fig3_mort_average_by_month.png"
# Full path
full_path <- file.path(save_path, file_name)
ggsave(full_path, plot = mort_aver_by_month, width = 10, height = 4, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA)
)


# Summarise by season
mort_summary_ss <- mort_summer %>%
  group_by(season) %>%
  summarise(
    count_obs_mort = sum(!is.na(`observed mortality`)),
    obs_mort_avg = mean(`observed mortality`, na.rm = TRUE),
    obs_mort_sd = sd(`observed mortality`, na.rm = TRUE),
    error = 1.96 * obs_mort_sd / sqrt(count_obs_mort),
    lower_CI = obs_mort_avg - error,
    upper_CI = obs_mort_avg + error
  ) %>%
  arrange(season)

# plot 
mort_aver_by_ss <- mort_summary_ss %>% 
  ggplot(aes(x = season, y = obs_mort_avg)) +
  geom_col(width = 0.7, fill = "#85b640") +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), 
                width = 0.1, 
                color = "black", 
                size = 0.3) +
  labs(x = "season", y = "observed mortality (average)") +
  geom_text(aes(label =  sprintf("%.1f", obs_mort_avg)), 
            position = position_stack(vjust = 0.04), 
            color = "black", size = 3) +   # label inside the bar
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.5, size = 10),
    legend.position = "none",
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 4),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.07, margin = margin(r = -150)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 30, r = 10, b = 10, l = 10)
  ) +
  scale_y_continuous(breaks = seq(0, 20, 2), limits = c(0, 20))  # set grid at 0,2,4,6,8,10

# save the plot
# Create a filename
file_name <- "Fig4_mort_average_by_ss.png"
# Full path
full_path <- file.path(save_path, file_name)
ggsave(full_path, plot = mort_aver_by_ss, width = 10, height = 4, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA)
)

#--------------------------------------------------
#-- JOIN HEATWAVES DATA AND MORTALITY -------------
#--------------------------------------------------
# use merged_summer and mort_summer
# Full outer join (all rows from both, keep everything)
ehf_summer$month   <- as.character(ehf_summer$month)
met_summer_all$month   <- as.character(met_summer_all$month)
mort_summer$month   <- as.character(mort_summer$month)

final_hw_mort <- ehf_summer %>% 
  mutate(threshold = "EHF") %>% 
  full_join( met_summer_all, by = c("station_id", "season", "date", "year", "month", "day", "Tmax", "Tmin", "Tmean", "threshold")) %>%
  full_join(mort_summer, by = c("date", "season", "year", "day", "month"))

write.csv(final_hw_mort,
          file = file.path(save_path, "final_hw_mort.csv"),
          row.names = FALSE)

#----------------------------------------------------------------
# Note: incase just running heatwave and mortality part, run from here since it will take time to run all above
#----------------------------------------------------------------

# Load csv file (from 'data_path')
final_hw_mort <- read.csv(file.path(save_path, "final_hw_mort.csv"))

#-----SECOND PART: CREATE SUMMARY COMBINATION OF HEATWAVE AND MORTALITY PART
# -------------------------------------
# FIGURE 18: HEATWAVES EP DURING 2019/20 SEASON
#--------------------------------------------
hw_mort_2019_20 <- final_hw_mort %>% filter(season == "2019/20")
min_date <- min(hw_mort_2019_20$date, na.rm = TRUE)
max_date <- max(hw_mort_2019_20$date, na.rm = TRUE)
hw_mort_2019_20$date <- as.Date(hw_mort_2019_20$date)
hw_mort_2019_20 <- hw_mort_2019_20 %>%
  mutate(heatwave_flag = ifelse(ehf_heatwave == 1, "Heatwave", NA)) %>%
  mutate(heatwave_flag = forcats::fct_drop(factor(heatwave_flag))) %>% 
  mutate(heatwave_severe_flag = ifelse(ehf_severe_heatwave == 1, "Severe heatwave", NA)) %>%
  mutate(heatwave_severe_flag = forcats::fct_drop(factor(heatwave_severe_flag))) %>% 
  mutate(heatwave_flag_met = ifelse(met_heatwave_1days == 1, "Heatwave", NA)) %>%
  mutate(heatwave_flag_met = forcats::fct_drop(factor(heatwave_flag_met)))

# calculate max_y separately
max_y <- max(hw_mort_2019_20$Tmax, na.rm = TRUE)
max_y_rounded <- ceiling(max_y / 5) * 5

# Plot
hw_mort_ehf <- hw_mort_2019_20 %>% filter (threshold == "EHF") %>% 
  ggplot(aes(x = date)) +
  # --- severe heatwave shading (draw first so it appears under light grey if overlap) ---
  geom_tile(
    data = hw_mort_2019_20 %>% filter(!is.na(heatwave_severe_flag)),
    aes(x = date, y = (max_y_rounded / 2), height = max_y_rounded,
        fill = heatwave_severe_flag),
    width = 1,
    alpha = 0.7,
    inherit.aes = FALSE
  ) +
  # Heatwave shading
  geom_tile(
    data = hw_mort_2019_20 %>% filter(!is.na(heatwave_flag)),
    aes(x = date, y=(max_y_rounded / 2), height = max_y_rounded, 
        fill = heatwave_flag),
    width = 1, 
    alpha = 0.6,
    inherit.aes = FALSE
  ) +
  # Observed mortality
  geom_line(aes(y = observed.mortality, color = "Observed mortality")) +
  # Baseline mortality
  geom_line(aes(y = baseline2y.mortality, color = "Baseline mortality")) +
  # Tmax (secondary y-axis)
  geom_line(aes(y = Tmax, color = "Tmax")) +
  # Colors for lines
  scale_color_manual(values = c(
    "Observed mortality" = "darkgreen",
    "Baseline mortality" = "olivedrab3",
    "Tmax" = "red")) +
  # Fill for heatwave shading
  scale_fill_manual(
    values = c(
      "Heatwave" = "grey75",
      "Severe heatwave" = "black"),
    na.translate = FALSE,   # drops NA from the legend
    guide = guide_legend(title = NULL) # optional: no title
  )+
  # Y-axis and secondary axis
  scale_y_continuous(name = "Number of deaths\n \nEHF",
                     sec.axis = sec_axis(~ ., name = "Temperature (°C)")) +
  # X-axis
  scale_x_date(date_labels = "%d-%m-%Y",
               breaks = seq(as.Date("2019-11-01"), as.Date("2020-03-31"), by = "7 days"),
               limits = as.Date(c("2019-11-01", "2020-03-31")),
               expand = c(0,0)) +
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.1, margin = margin(r = -100), lineheight = 0.6),
    axis.title.y.right = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.01, margin = margin(l = -90)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 0, l = 10)
  ) +
  # Labels
  labs(x = "", color = "", fill = "")


hw_mort_met_2823 <- hw_mort_2019_20 %>% filter (threshold == "Tmax = 28C/Tmean = 23C") %>% 
  ggplot(aes(x = date)) +
  # Grey rectangles for heatwave periods
  # Heatwave shading
  geom_tile(
    data = hw_mort_2019_20 %>% filter (threshold == "Tmax = 28C/Tmean = 23C") %>% filter(!is.na(heatwave_flag_met)),
    aes(x = date, y=(max_y_rounded / 2), height = max_y_rounded, 
        fill = heatwave_flag_met),
    width = 1, 
    alpha = 0.6,
    inherit.aes = FALSE) +
  # Observed mortality
  geom_line(aes(y = observed.mortality, color = "Observed mortality")) +
  # Baseline mortality
  geom_line(aes(y = baseline2y.mortality, color = "Baseline mortality")) +
  # Tmax (secondary y-axis)
  geom_line(aes(y = Tmax, color = "Tmax")) +
  scale_y_continuous(name = "MetService (Threshold: Tmax = 28C/Tmean = 23C)",
                     sec.axis = sec_axis(~ ., name = ""))+  # secondary axis
  scale_color_manual(values = c(
    "Observed mortality" = "darkgreen",
    "Baseline mortality" = "olivedrab3",
    "Tmax" = "red")) +
  # Fill for heatwave shading
  scale_fill_manual(
    values = c("Heatwave" = "grey75"),
    na.translate = FALSE,   # drops NA from the legend
    guide = guide_legend(title = NULL) # optional: no title
  ) +
  scale_x_date(date_labels = "%d-%m-%Y",
               breaks = seq(as.Date("2019-11-01"), as.Date("2020-03-31"), by = "7 days"),
               limits = as.Date(c("2019-11-01", "2020-03-31")),
               expand = c(0,0)) +     # about 22 breaks across the range
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 1),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.01, margin = margin(r = -280)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 0, l = 10)) +
  labs(x = "", color = "")

hw_mort_met_2923 <- hw_mort_2019_20 %>% filter (threshold == "Tmax = 29C/Tmean = 23C") %>% 
  ggplot(aes(x = date)) +
  # Grey rectangles for heatwave periods
  # Heatwave shading
  geom_tile(
    data = hw_mort_2019_20 %>% filter (threshold == "Tmax = 29C/Tmean = 23C") %>% filter(!is.na(heatwave_flag_met)),
    aes(x = date, y=(max_y_rounded / 2), height = max_y_rounded, 
        fill = heatwave_flag_met),
    width = 1, 
    alpha = 0.6,
    inherit.aes = FALSE) +
  # Observed mortality
  geom_line(aes(y = observed.mortality, color = "Observed mortality")) +
  # Baseline mortality
  geom_line(aes(y = baseline2y.mortality, color = "Baseline mortality")) +
  # Tmax (secondary y-axis)
  geom_line(aes(y = Tmax, color = "Tmax")) +
  scale_y_continuous(name = "MetService (Threshold: Tmax = 29C/Tmean = 23C)",
                     sec.axis = sec_axis(~ ., name = ""))+  # secondary axis
  scale_color_manual(values = c(
    "Observed mortality" = "darkgreen",
    "Baseline mortality" = "olivedrab3",
    "Tmax" = "red")) +
  # Fill for heatwave shading
  scale_fill_manual(
    values = c("Heatwave" = "grey75"),
    na.translate = FALSE,   # drops NA from the legend
    guide = guide_legend(title = NULL) # optional: no title
  ) +
  scale_x_date(date_labels = "%d-%m-%Y",
               breaks = seq(as.Date("2019-11-01"), as.Date("2020-03-31"), by = "7 days"),
               limits = as.Date(c("2019-11-01", "2020-03-31")),
               expand = c(0,0)) +     # about 22 breaks across the range
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    #axis.line.x = element_line(),
    axis.text.x = element_text(angle = 89.9, size = 10, hjust = 0, vjust = 0.2, margin = margin(t = 100)),
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 2.5),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.01, margin = margin(r = -280)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 0, r = 10, b = 0, l = 10)) + 
    labs(x = "Date (2019/20)", color = "")

# combine plots: use hw_mort_ehf and met
hw_mort_all <- plot_grid(
  hw_mort_ehf, hw_mort_met_2823, hw_mort_met_2923,
  ncol = 1,                  # vertical stack
  align = "v",               # align vertically
  rel_heights = c(1.2,1,1.2), # adjust relative heights if needed
  labels = NULL
)
# save the plot
# Create a filename
file_name <- "Fig18_hw_mort_2019_20.png"
# Full path
full_path <- file.path(save_path, file_name)
ggsave(full_path, plot = hw_mort_all , width = 8, height = 10, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA)
)

# -------------------------------------
# FIGURE 19: HEATWAVES EP DURING 2017/18 SEASON
#--------------------------------------------
hw_mort_2017_18 <- final_hw_mort %>% filter(season == "2017/18")
min_date <- min(hw_mort_2017_18$date, na.rm = TRUE)
max_date <- max(hw_mort_2017_18$date, na.rm = TRUE)
hw_mort_2017_18$date <- as.Date(hw_mort_2017_18$date)
hw_mort_2017_18 <- hw_mort_2017_18 %>%
  mutate(heatwave_flag = ifelse(ehf_heatwave == 1, "Heatwave", NA)) %>%
  mutate(heatwave_flag = forcats::fct_drop(factor(heatwave_flag))) %>% 
  mutate(heatwave_severe_flag = ifelse(ehf_severe_heatwave == 1, "Severe heatwave", NA)) %>%
  mutate(heatwave_severe_flag = forcats::fct_drop(factor(heatwave_severe_flag))) %>% 
  mutate(heatwave_flag_met = ifelse(met_heatwave_1days == 1, "Heatwave", NA)) %>%
  mutate(heatwave_flag_met = forcats::fct_drop(factor(heatwave_flag_met)))

# calculate max_y separately
max_y <- max(hw_mort_2017_18$Tmax, na.rm = TRUE)
max_y_rounded <- ceiling(max_y / 5) * 5

# Plot
hw_mort_ehf_17 <- hw_mort_2017_18 %>% filter (threshold == "EHF") %>% 
  ggplot(aes(x = date)) +
  # --- severe heatwave shading (draw first so it appears under light grey if overlap) ---
  geom_tile(
    data = hw_mort_2017_18 %>% filter(!is.na(heatwave_severe_flag)),
    aes(x = date, y = (max_y_rounded / 2), height = max_y_rounded,
        fill = heatwave_severe_flag),
    width = 1,
    alpha = 0.7,
    inherit.aes = FALSE
  ) +
  # Heatwave shading
  geom_tile(
    data = hw_mort_2017_18 %>% filter(!is.na(heatwave_flag)),
    aes(x = date, y=(max_y_rounded / 2), height = max_y_rounded, 
        fill = heatwave_flag),
    width = 1, 
    alpha = 0.6,
    inherit.aes = FALSE
  ) +
  # Observed mortality
  geom_line(aes(y = observed.mortality, color = "Observed mortality")) +
  # Baseline mortality
  geom_line(aes(y = baseline2y.mortality, color = "Baseline mortality")) +
  # Tmax (secondary y-axis)
  geom_line(aes(y = Tmax, color = "Tmax")) +
  # Colors for lines
  scale_color_manual(values = c(
    "Observed mortality" = "darkgreen",
    "Baseline mortality" = "olivedrab3",
    "Tmax" = "red")) +
  # Fill for heatwave shading
  scale_fill_manual(
    values = c(
      "Heatwave" = "grey75",
      "Severe heatwave" = "black"),
    na.translate = FALSE,   # drops NA from the legend
    guide = guide_legend(title = NULL) # optional: no title
  )+
  # Y-axis and secondary axis
  scale_y_continuous(name = "Number of deaths\n \nEHF",
                     sec.axis = sec_axis(~ ., name = "Temperature (°C)")) +
  # X-axis
  scale_x_date(date_labels = "%d-%m-%Y",
               breaks = seq(as.Date("2017-11-01"), as.Date("2018-03-31"), by = "7 days"),
               limits = as.Date(c("2017-11-01", "2018-03-31")),
               expand = c(0,0)) +
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.11, margin = margin(r = -100), lineheight = 0.6),
    axis.title.y.right = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.01, margin = margin(l = -90)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 0, l = 10)
  ) +
  # Labels
  labs(x = "", color = "", fill = "")


hw_mort_met_2823_17 <- hw_mort_2017_18 %>% filter (threshold == "Tmax = 28C/Tmean = 23C") %>% 
  ggplot(aes(x = date)) +
  # Grey rectangles for heatwave periods
  # Heatwave shading
  geom_tile(
    data = hw_mort_2017_18 %>% filter (threshold == "Tmax = 28C/Tmean = 23C") %>% filter(!is.na(heatwave_flag_met)),
    aes(x = date, y=(max_y_rounded / 2), height = max_y_rounded, 
        fill = heatwave_flag_met),
    width = 1, 
    alpha = 0.6,
    inherit.aes = FALSE) +
  # Observed mortality
  geom_line(aes(y = observed.mortality, color = "Observed mortality")) +
  # Baseline mortality
  geom_line(aes(y = baseline2y.mortality, color = "Baseline mortality")) +
  # Tmax (secondary y-axis)
  geom_line(aes(y = Tmax, color = "Tmax")) +
  scale_y_continuous(name = "MetService (Threshold: Tmax = 28C/Tmean = 23C)",
                     sec.axis = sec_axis(~ ., name = ""))+  # secondary axis
  scale_color_manual(values = c(
    "Observed mortality" = "darkgreen",
    "Baseline mortality" = "olivedrab3",
    "Tmax" = "red")) +
  # Fill for heatwave shading
  scale_fill_manual(
    values = c("Heatwave" = "grey75"),
    na.translate = FALSE,   # drops NA from the legend
    guide = guide_legend(title = NULL) # optional: no title
  ) +
  scale_x_date(date_labels = "%d-%m-%Y",
               breaks = seq(as.Date("2017-11-01"), as.Date("2018-03-31"), by = "7 days"),
               limits = as.Date(c("2017-11-01", "2018-03-31")),
               expand = c(0,0)) +     # about 22 breaks across the range
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 1),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.02, margin = margin(r = -280)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 0, l = 10)) +
  labs(x = "", color = "")

hw_mort_met_2923_17 <- hw_mort_2017_18 %>% filter (threshold == "Tmax = 29C/Tmean = 23C") %>% 
  ggplot(aes(x = date)) +
  # Grey rectangles for heatwave periods
  # Heatwave shading
  geom_tile(
    data = hw_mort_2017_18 %>% filter (threshold == "Tmax = 29C/Tmean = 23C") %>% filter(!is.na(heatwave_flag_met)),
    aes(x = date, y=(max_y_rounded / 2), height = max_y_rounded, 
        fill = heatwave_flag_met),
    width = 1, 
    alpha = 0.6,
    inherit.aes = FALSE) +
  # Observed mortality
  geom_line(aes(y = observed.mortality, color = "Observed mortality")) +
  # Baseline mortality
  geom_line(aes(y = baseline2y.mortality, color = "Baseline mortality")) +
  # Tmax (secondary y-axis)
  geom_line(aes(y = Tmax, color = "Tmax")) +
  scale_y_continuous(name = "MetService (Threshold: Tmax = 29C/Tmean = 23C)",
                     sec.axis = sec_axis(~ ., name = ""))+  # secondary axis
  scale_color_manual(values = c(
    "Observed mortality" = "darkgreen",
    "Baseline mortality" = "olivedrab3",
    "Tmax" = "red")) +
  # Fill for heatwave shading
  scale_fill_manual(
    values = c("Heatwave" = "grey75"),
    na.translate = FALSE,   # drops NA from the legend
    guide = guide_legend(title = NULL) # optional: no title
  ) +
  scale_x_date(date_labels = "%d-%m-%Y",
               breaks = seq(as.Date("2017-11-01"), as.Date("2018-03-31"), by = "7 days"),
               limits = as.Date(c("2017-11-01", "2018-03-31")),
               expand = c(0,0)) +     # about 22 breaks across the range
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    #axis.line.x = element_line(),
    axis.text.x = element_text(angle = 89.9, size = 10, hjust = 0, vjust = 0.2, margin = margin(t = 100)),
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 2.5),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.02, margin = margin(r = -280)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 0, r = 10, b = 0, l = 10)) + 
  labs(x = "Date (2017/18)", color = "")

# combine plots: use hw_mort_ehf and met
hw_mort_all_17 <- plot_grid(
  hw_mort_ehf_17, hw_mort_met_2823_17, hw_mort_met_2923_17,
  ncol = 1,                  # vertical stack
  align = "v",               # align vertically
  rel_heights = c(1.2,1,1.2), # adjust relative heights if needed
  labels = NULL
)
# save the plot
# Create a filename
file_name <- "Fig19_hw_mort_2017_18.png"
# Full path
full_path <- file.path(save_path, file_name)
ggsave(full_path, plot = hw_mort_all_17 , width = 8, height = 11, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA)
)


# -------------------------------------
# FIGURE 19 EXTRA: HEATWAVES EP DURING 2015/16 SEASON
#--------------------------------------------
hw_mort_2015_16 <- final_hw_mort %>% filter(season == "2015/16")
min_date <- min(hw_mort_2015_16$date, na.rm = TRUE)
max_date <- max(hw_mort_2015_16$date, na.rm = TRUE)
hw_mort_2015_16$date <- as.Date(hw_mort_2015_16$date)
hw_mort_2015_16 <- hw_mort_2015_16 %>%
  mutate(heatwave_flag = ifelse(ehf_heatwave == 1, "Heatwave", NA)) %>%
  mutate(heatwave_flag = forcats::fct_drop(factor(heatwave_flag))) %>% 
  mutate(heatwave_severe_flag = ifelse(ehf_severe_heatwave == 1, "Severe heatwave", NA)) %>%
  mutate(heatwave_severe_flag = forcats::fct_drop(factor(heatwave_severe_flag))) %>% 
  mutate(heatwave_flag_met = ifelse(met_heatwave_1days == 1, "Heatwave", NA)) %>%
  mutate(heatwave_flag_met = forcats::fct_drop(factor(heatwave_flag_met)))

# calculate max_y separately
max_y <- max(hw_mort_2015_16$Tmax, na.rm = TRUE)
max_y_rounded <- ceiling(max_y / 5) * 5

# Plot
hw_mort_ehf_15 <- hw_mort_2015_16 %>% filter (threshold == "EHF") %>% 
  ggplot(aes(x = date)) +
  # --- severe heatwave shading (draw first so it appears under light grey if overlap) ---
  geom_tile(
    data = hw_mort_2015_16 %>% filter(!is.na(heatwave_severe_flag)),
    aes(x = date, y = (max_y_rounded / 2), height = max_y_rounded,
        fill = heatwave_severe_flag),
    width = 1,
    alpha = 0.7,
    inherit.aes = FALSE
  ) +
  # Heatwave shading
  geom_tile(
    data = hw_mort_2015_16 %>% filter(!is.na(heatwave_flag)),
    aes(x = date, y=(max_y_rounded / 2), height = max_y_rounded, 
        fill = heatwave_flag),
    width = 1, 
    alpha = 0.6,
    inherit.aes = FALSE
  ) +
  # Observed mortality
  geom_line(aes(y = observed.mortality, color = "Observed mortality")) +
  # Baseline mortality
  geom_line(aes(y = baseline2y.mortality, color = "Baseline mortality")) +
  # Tmax (secondary y-axis)
  geom_line(aes(y = Tmax, color = "Tmax")) +
  # Colors for lines
  scale_color_manual(values = c(
    "Observed mortality" = "darkgreen",
    "Baseline mortality" = "olivedrab3",
    "Tmax" = "red")) +
  # Fill for heatwave shading
  scale_fill_manual(
    values = c(
      "Heatwave" = "grey75",
      "Severe heatwave" = "black"),
    na.translate = FALSE,   # drops NA from the legend
    guide = guide_legend(title = NULL) # optional: no title
  )+
  # Y-axis and secondary axis
  scale_y_continuous(name = "Number of deaths\n \nEHF",
                     sec.axis = sec_axis(~ ., name = "Temperature (°C)")) +
  # X-axis
  scale_x_date(date_labels = "%d-%m-%Y",
               breaks = seq(as.Date("2015-11-01"), as.Date("2016-03-31"), by = "7 days"),
               limits = as.Date(c("2015-11-01", "2016-03-31")),
               expand = c(0,0)) +
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.11, margin = margin(r = -100), lineheight = 0.6),
    axis.title.y.right = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.01, margin = margin(l = -90)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 0, l = 10)
  ) +
  # Labels
  labs(x = "", color = "", fill = "")


hw_mort_met_2823_15 <- hw_mort_2015_16 %>% filter (threshold == "Tmax = 28C/Tmean = 23C") %>% 
  ggplot(aes(x = date)) +
  # Grey rectangles for heatwave periods
  # Heatwave shading
  geom_tile(
    data = hw_mort_2015_16 %>% filter (threshold == "Tmax = 29C/Tmean = 23C") %>% filter(!is.na(heatwave_flag_met)),
    aes(x = date, y=(max_y_rounded / 2), height = max_y_rounded, 
        fill = heatwave_flag_met),
    width = 1, 
    alpha = 0.6,
    inherit.aes = FALSE) +
  # Observed mortality
  geom_line(aes(y = observed.mortality, color = "Observed mortality")) +
  # Baseline mortality
  geom_line(aes(y = baseline2y.mortality, color = "Baseline mortality")) +
  # Tmax (secondary y-axis)
  geom_line(aes(y = Tmax, color = "Tmax")) +
  scale_y_continuous(name = "MetService (Threshold: Tmax = 28C/Tmean = 23C)",
                     sec.axis = sec_axis(~ ., name = ""))+  # secondary axis
  scale_color_manual(values = c(
    "Observed mortality" = "darkgreen",
    "Baseline mortality" = "olivedrab3",
    "Tmax" = "red")) +
  # Fill for heatwave shading
  scale_fill_manual(
    values = c("Heatwave" = "grey75"),
    na.translate = FALSE,   # drops NA from the legend
    guide = guide_legend(title = NULL) # optional: no title
  ) +
  scale_x_date(date_labels = "%d-%m-%Y",
               breaks = seq(as.Date("2015-11-01"), as.Date("2016-03-31"), by = "7 days"),
               limits = as.Date(c("2015-11-01", "2016-03-31")),
               expand = c(0,0)) +     # about 22 breaks across the range
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 1),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.02, margin = margin(r = -280)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 0, l = 10)) +
  labs(x = "", color = "")

hw_mort_met_2923_15 <- hw_mort_2015_16 %>% filter (threshold == "Tmax = 29C/Tmean = 23C") %>% 
  ggplot(aes(x = date)) +
  # Grey rectangles for heatwave periods
  # Heatwave shading
  geom_tile(
    data = hw_mort_2015_16 %>% filter (threshold == "Tmax = 29C/Tmean = 23C") %>% filter(!is.na(heatwave_flag_met)),
    aes(x = date, y=(max_y_rounded / 2), height = max_y_rounded, 
        fill = heatwave_flag_met),
    width = 1, 
    alpha = 0.6,
    inherit.aes = FALSE) +
  # Observed mortality
  geom_line(aes(y = observed.mortality, color = "Observed mortality")) +
  # Baseline mortality
  geom_line(aes(y = baseline2y.mortality, color = "Baseline mortality")) +
  # Tmax (secondary y-axis)
  geom_line(aes(y = Tmax, color = "Tmax")) +
  scale_y_continuous(name = "MetService (Threshold: Tmax = 29C/Tmean = 23C)",
                     sec.axis = sec_axis(~ ., name = ""))+  # secondary axis
  scale_color_manual(values = c(
    "Observed mortality" = "darkgreen",
    "Baseline mortality" = "olivedrab3",
    "Tmax" = "red")) +
  # Fill for heatwave shading
  scale_fill_manual(
    values = c("Heatwave" = "grey75"),
    na.translate = FALSE,   # drops NA from the legend
    guide = guide_legend(title = NULL) # optional: no title
  ) +
  scale_x_date(date_labels = "%d-%m-%Y",
               breaks = seq(as.Date("2015-11-01"), as.Date("2016-03-31"), by = "7 days"),
               limits = as.Date(c("2015-11-01", "2016-03-31")),
               expand = c(0,0)) +     # about 22 breaks across the range
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    #axis.line.x = element_line(),
    axis.text.x = element_text(angle = 89.9, size = 10, hjust = 0, vjust = 0.2, margin = margin(t = 100)),
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 2.5),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.02, margin = margin(r = -280)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 0, r = 10, b = 0, l = 10)) + 
  labs(x = "Date (2015/16)", color = "")

# combine plots: use hw_mort_ehf and met
hw_mort_all_15 <- plot_grid(
  hw_mort_ehf_15, hw_mort_met_2823_15, hw_mort_met_2923_15,
  ncol = 1,                  # vertical stack
  align = "v",               # align vertically
  rel_heights = c(1.2,1,1.2), # adjust relative heights if needed
  labels = NULL
)
# save the plot
# Create a filename
file_name <- "Fig19_extra_hw_mort_2015_16.png"
# Full path
full_path <- file.path(save_path, file_name)
ggsave(full_path, plot = hw_mort_all_15 , width = 8, height = 11, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA)
)


# -------------------------------------
# Table 5: Summary of key heatwave and mortality variables
#--------------------------------------------

# since the name of MetService approach is now met_episode_id_1days and met_duration_days_1days, we will rename to similar other approach
final_hw_mort <- final_hw_mort %>% 
  mutate(met_episode_id   = met_episode_id_1days,
         met_duration_days = met_duration_days_1days)

# Function to summarise by chosen approach
make_summary <- function(data, approach) {
  episode_id_col <- paste0(approach, "_episode_id")
  duration_col   <- paste0(approach, "_duration_days")
  
  data %>%
    # drop empty/NA episode ids up front
    dplyr::filter(!is.na(.data[[episode_id_col]])) %>%
    dplyr::group_by(threshold, .data[[episode_id_col]]) %>%
    dplyr::summarise(
      episode      = dplyr::first(.data[[episode_id_col]]),
      duration     = max(.data[[duration_col]], na.rm = TRUE),
      excess_sum   = sum(excess2y.mortality,    na.rm = TRUE),
      baseline_sum = sum(baseline2y.mortality,  na.rm = TRUE),
      observed_sum = sum(observed.mortality,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # convert -Inf (empty/all-NA groups) to NA so they don't poison sums
    dplyr::mutate(duration = dplyr::if_else(is.finite(duration), duration, NA_real_)) %>%
    dplyr::mutate(
      lower_CI_sum = qchisq(0.025, 2 * observed_sum) / 2,
      upper_CI_sum = qchisq(0.975, 2 * (observed_sum + 1)) / 2,
      lower_CI     = lower_CI_sum - baseline_sum,
      upper_CI     = upper_CI_sum - baseline_sum
    ) %>%
    dplyr::select(
      threshold, episode, duration, excess_sum, baseline_sum, observed_sum,
      lower_CI_sum, upper_CI_sum, lower_CI, upper_CI
    )
}

# run for EHF,  MET
ehf_summary <- make_summary(final_hw_mort, "ehf") %>% mutate(approach = "EHF - All heatwaves")
met_summary <- make_summary(final_hw_mort, "met") %>% mutate(approach = "MetService")

#-----------------
# severe heatwave for EHF
#----------------

# Function to summarise by chosen approach
severe_make_summary <- function(data, approach) {
  episode_id_col <- paste0(approach, "_severe_episode_id")
  duration_col   <- paste0(approach, "_severe_duration_days")
  
  data %>%
    # drop empty/NA episode ids up front
    dplyr::filter(!is.na(.data[[episode_id_col]])) %>%
    dplyr::group_by(threshold, .data[[episode_id_col]]) %>%
    dplyr::summarise(
      episode      = dplyr::first(.data[[episode_id_col]]),
      duration     = max(.data[[duration_col]], na.rm = TRUE),
      excess_sum   = sum(excess2y.mortality,    na.rm = TRUE),
      baseline_sum = sum(baseline2y.mortality,  na.rm = TRUE),
      observed_sum = sum(observed.mortality,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # convert -Inf (empty/all-NA groups) to NA so they don't poison sums
    dplyr::mutate(duration = dplyr::if_else(is.finite(duration), duration, NA_real_)) %>%
    dplyr::mutate(
      lower_CI_sum = qchisq(0.025, 2 * observed_sum) / 2,
      upper_CI_sum = qchisq(0.975, 2 * (observed_sum + 1)) / 2,
      lower_CI     = lower_CI_sum - baseline_sum,
      upper_CI     = upper_CI_sum - baseline_sum
    ) %>%
    dplyr::select(
      threshold, episode, duration, excess_sum, baseline_sum, observed_sum,
      lower_CI_sum, upper_CI_sum, lower_CI, upper_CI
    )
}

# run for EHF, MET
ehf_severe_summary <- severe_make_summary(final_hw_mort, "ehf") %>% mutate(approach = "EHF - Severe heatwaves")

# Combine all into one table
all_hw_mort_summary <- bind_rows(ehf_summary, ehf_severe_summary, met_summary) %>%
  select(approach, everything())  # put approach first

# rename variable in column approach and threshold
all_hw_mort_summary <- all_hw_mort_summary %>%
  dplyr::mutate(
    threshold = dplyr::case_when(
      approach == "EHF - All heatwaves"    ~ "All heatwaves",
      approach == "EHF - Severe heatwaves" ~ "Severe heatwaves",
      TRUE ~ threshold
    ),
    
    # optional: combine both EHF groups into one approach name
    approach = dplyr::case_when(
      grepl("^EHF", approach) ~ "EHF",
      TRUE ~ approach
    )
  )

# export to excel 
# Load the existing file
#export merged_summer to excel file
write_xlsx(
  list(
    "summary_hw_mort" = all_hw_mort_summary
  ),
  path = file.path(save_path, "summary_hw_mort.xlsx")
)

# Load file (from 'data_path')
all_hw_mort_summary <- read_excel(file.path(save_path, "summary_hw_mort.xlsx"), sheet = "summary_hw_mort")

#--------------------------------
# Total each approach
#--------------------------------
all_hw_mort_total <- all_hw_mort_summary %>%
  dplyr::filter(is.finite(duration) | is.na(duration)) %>%  # drop +/-Inf only
  dplyr::group_by(approach, threshold) %>%
  dplyr::summarise(
    heatwave_episodes = dplyr::n(),  # number of episodes
    duration_total = sum(duration, na.rm = TRUE),
    excess         = round(sum(excess_sum,   na.rm = TRUE),1),
    baseline       = round(sum(baseline_sum, na.rm = TRUE),1),
    observed       = sum(observed_sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    lower_CI_sum = qchisq(0.025, 2 * observed) / 2,
    upper_CI_sum = qchisq(0.975, 2 * (observed + 1)) / 2,
    lower_CI     = round(lower_CI_sum - baseline, 1),
    upper_CI     = round(upper_CI_sum - baseline, 1),
    excess_mortality = paste0(excess, " (", lower_CI," to ",upper_CI,")")
  )

# export to excel summary_hw_mort
# Load the existing file
summary <- loadWorkbook(file.path(save_path, "summary_hw_mort.xlsx"))

# Add as a new sheet
if ("all_hw_mort_total" %in% names(summary)) removeWorksheet(summary, "all_hw_mort_total")
addWorksheet(summary, "all_hw_mort_total")
writeData(summary, "all_hw_mort_total", all_hw_mort_total)

# Save without removing existing sheets
saveWorkbook(summary, file.path(save_path, "summary_hw_mort.xlsx"), overwrite = TRUE)


#-----------------------------------------------------------------------------
# Figure 20: Number of cumulative excess deaths during heatwave episodes, total (2000/01 to 2020/21)
#-----------------------------------------------------------------------------

all_hw_mort_total$approach <- factor(all_hw_mort_total$approach, levels = c("EHF", "MetService"))
all_hw_mort_total <- all_hw_mort_total %>%
  mutate(x_axis = interaction(approach, threshold, sep = " / "))

custom_colors <- c(
  "All heatwaves"         = "#85b640",
  "Severe heatwaves"      = "#6eb7e0",
  "Tmax = 27C/Tmean = 23C"= "#ffc20c",
  "Tmax = 28C/Tmean = 23C"= "#f7914d",
  "Tmax = 29C/Tmean = 23C"= "#f9b8bc",
  "Tmax = 30C/Tmean = 24C"= "#ffeabd"
)

excess_during_hw <- ggplot(all_hw_mort_total, aes(x = x_axis, y = excess, fill = threshold)) +
  geom_col(width = 0.7, color = NA) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.1, size = 0.5, alpha = 0.8,
                color = "grey50") +
  geom_label(aes(label = sprintf("%.1f", excess)),
             color = "black",
             fill = "white", alpha = 0.5,
             label.size = 0,
             position = position_stack(vjust = 0.05),
             label.r = unit(0.1, "cm"),
             vjust = 0) +
  facet_grid(. ~ approach, scales = "free", space='free') +
  xlab("Total (2000/01 to 2020/21)") +
  ylab("Number of deaths\n(excess mortality)") +
  labs(fill = NULL) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(
    limits = \(x) range(pretty(x, n = 10)),
    breaks = scales::pretty_breaks(n = 10)
  ) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 10), 
    axis.text.x = element_blank(),
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 4),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.13, margin = margin(r = -95)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 0, l = 10),
    strip.text = element_text(face = "bold", size = 12))


# save the plot
# Create a filename
file_name <- "Fig20_excess_during_hw.png"
# Full path
full_path <- file.path(save_path, file_name)
ggsave(full_path, plot = excess_during_hw , width = 10, height = 6, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA)
)

#------------------
# ALL SEASON TOTAL
#------------------

#-------------------------------------------------------------
# Excess mortality for seasons
#-------------------------------------------------------------
# Function to summarise by chosen approach
season_ep_summary <- function(data, approach) {
  episode_id_col <- paste0(approach, "_episode_id")
  duration_col   <- paste0(approach, "_duration_days")
  temp_excess_col <- paste0(approach, "_excess_heat")
  data %>%
    # drop empty/NA episode ids up front
    dplyr::filter(!is.na(.data[[episode_id_col]])) %>%
    dplyr::group_by(threshold, season, .data[[episode_id_col]]) %>%
    dplyr::summarise(
      episode      = dplyr::first(.data[[episode_id_col]]),
      start_date     = min(as.Date(date), na.rm = TRUE),  # starting date
      end_date       = max(as.Date(date), na.rm = TRUE),  # ending date
      duration     = max(.data[[duration_col]], na.rm = TRUE),
      excess_temp = sum(.data[[temp_excess_col]], na.rm = TRUE),
      excess_sum   = round(sum(excess2y.mortality, na.rm = TRUE), 1),
      baseline_sum = round(sum(baseline2y.mortality,  na.rm = TRUE),1),
      observed_sum = round(sum(observed.mortality,  na.rm = TRUE),1),
      .groups = "drop"
    ) %>%
    # convert -Inf (empty/all-NA groups) to NA so they don't poison sums
    dplyr::mutate(duration = dplyr::if_else(is.finite(duration), duration, NA_real_)) %>%
    dplyr::mutate(
      lower_CI_sum = qchisq(0.025, 2 * observed_sum) / 2,
      upper_CI_sum = qchisq(0.975, 2 * (observed_sum + 1)) / 2,
      lower_CI     = round(lower_CI_sum - baseline_sum, 1),
      upper_CI     = round(upper_CI_sum - baseline_sum, 1),
      date = paste0(format(start_date, "%d/%m/%Y"), " – ", format(end_date, "%d/%m/%Y")),
      excess_mortality = paste0(excess_sum, " (", lower_CI," to ",upper_CI,")")
    ) %>%
    dplyr::select(
      threshold, season, episode, date, duration, excess_temp, excess_sum, observed_sum, baseline_sum, 
      lower_CI_sum, upper_CI_sum, lower_CI, upper_CI, excess_mortality
    )
}


# Function to summarise by chosen approach
severe_season_ep_summary <- function(data, approach) {
  episode_id_col <- paste0(approach, "_severe_episode_id")
  duration_col   <- paste0(approach, "_severe_duration_days")
  temp_excess_col <- paste0(approach, "_severe_excess_heat")
  data %>%
    # drop empty/NA episode ids up front
    dplyr::filter(!is.na(.data[[episode_id_col]])) %>%
    dplyr::group_by(threshold, season, .data[[episode_id_col]]) %>%
    dplyr::summarise(
      episode      = dplyr::first(.data[[episode_id_col]]),
      start_date     = min(as.Date(date), na.rm = TRUE),  # starting date
      end_date       = max(as.Date(date), na.rm = TRUE),  # ending date
      duration     = max(.data[[duration_col]], na.rm = TRUE),
      excess_temp = sum(.data[[temp_excess_col]], na.rm = TRUE),
      excess_sum   = round(sum(excess2y.mortality, na.rm = TRUE), 1),
      baseline_sum = round(sum(baseline2y.mortality,  na.rm = TRUE),1),
      observed_sum = round(sum(observed.mortality,  na.rm = TRUE),1),
      .groups = "drop"
    ) %>%
    # convert -Inf (empty/all-NA groups) to NA so they don't poison sums
    dplyr::mutate(duration = dplyr::if_else(is.finite(duration), duration, NA_real_)) %>%
    dplyr::mutate(
      lower_CI_sum = qchisq(0.025, 2 * observed_sum) / 2,
      upper_CI_sum = qchisq(0.975, 2 * (observed_sum + 1)) / 2,
      lower_CI     = round(lower_CI_sum - baseline_sum, 1),
      upper_CI     = round(upper_CI_sum - baseline_sum, 1),
      date = paste0(format(start_date, "%d/%m/%Y"), " – ", format(end_date, "%d/%m/%Y")),
      excess_mortality = paste0(excess_sum, " (", lower_CI," to ",upper_CI,")")
    ) %>%
    dplyr::select(
      threshold, season, episode, date, duration, excess_temp, excess_sum, observed_sum, baseline_sum, 
      lower_CI_sum, upper_CI_sum, lower_CI, upper_CI, excess_mortality
    )
}

# run for EHF, MEt
ehf_season_ep_summary <- season_ep_summary(final_hw_mort, "ehf") %>% mutate(approach = "EHF - All heatwaves")
met_season_ep_summary <- season_ep_summary(final_hw_mort, "met") %>% mutate(approach = "MetService")
ehf_severe_season_ep_summary <- severe_season_ep_summary(final_hw_mort, "ehf") %>% mutate(approach = "EHF - Severe heatwaves")

# Combine all into one table
all_season_ep_summary <- bind_rows(ehf_season_ep_summary, ehf_severe_season_ep_summary, met_season_ep_summary) %>%
  select(approach, everything())  # put approach first

# rename variable in column approach and threshold
all_season_ep_summary <- all_season_ep_summary %>%
  dplyr::mutate(
    threshold = dplyr::case_when(
      approach == "EHF - All heatwaves"    ~ "All heatwaves",
      approach == "EHF - Severe heatwaves" ~ "Severe heatwaves",
      TRUE ~ threshold
    ),
    
    # optional: combine both EHF groups into one approach name
    approach = dplyr::case_when(
      grepl("^EHF", approach) ~ "EHF",
      TRUE ~ approach
    )
  )


# export to excel summary_hw_mort
# Load the existing file
summary <- loadWorkbook(file.path(save_path, "summary_hw_mort.xlsx"))

# Add as a new sheet
if ("all_season_ep_summary" %in% names(summary)) removeWorksheet(summary, "all_season_ep_summary")
addWorksheet(summary, "all_season_ep_summary")
writeData(summary, "all_season_ep_summary", all_season_ep_summary)

# Save without removing existing sheets
saveWorkbook(summary, file.path(save_path, "summary_hw_mort.xlsx"), overwrite = TRUE)

#--------------------------------
# Total each approach by season
#--------------------------------
all_season_total <- all_season_ep_summary %>%
  dplyr::filter(is.finite(duration) | is.na(duration)) %>%  # drop +/-Inf only
  dplyr::group_by(approach, threshold, season) %>%
  dplyr::summarise(
    duration_total = sum(duration, na.rm = TRUE),
    excess         = round(sum(excess_sum,   na.rm = TRUE),1),
    baseline       = round(sum(baseline_sum, na.rm = TRUE),1),
    observed       = sum(observed_sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    lower_CI_sum = qchisq(0.025, 2 * observed) / 2,
    upper_CI_sum = qchisq(0.975, 2 * (observed + 1)) / 2,
    lower_CI     = round(lower_CI_sum - baseline, 1),
    upper_CI     = round(upper_CI_sum - baseline, 1),
    excess_mortality = paste0(excess, " (", lower_CI," to ",upper_CI,")")
  )

# export to excel summary_hw_mort
# Load the existing file
summary <- loadWorkbook(file.path(save_path, "summary_hw_mort.xlsx"))
# Add as a new sheet
if ("all_season_total" %in% names(summary)) removeWorksheet(summary, "all_season_total")
addWorksheet(summary, "all_season_total")
writeData(summary, "all_season_total", all_season_total)

# Save without removing existing sheets
saveWorkbook(summary, file.path(save_path, "summary_hw_mort.xlsx"), overwrite = TRUE)

#-----------------------------------------------------------------------------
# Figure 21: Number of excess deaths, 2015/16, 2017/18 and 2020/21
#-----------------------------------------------------------------------------

all_season_total$approach <- factor(all_season_total$approach, levels = c("EHF", "MetService"))
all_season_total <- all_season_total %>%
  mutate(x_axis = interaction(approach, threshold, sep = " / "))

fig21_filtered <- all_season_total %>% filter(season %in% c("2015/16", "2017/18","2019/20"))
fig21_201516 <- all_season_total %>% filter(season %in% c("2015/16"))
fig21_201718 <- all_season_total %>% filter(season %in% c("2017/18"))
fig21_201920 <- all_season_total %>% filter(season %in% c("2019/20"))

excess_by_season <- ggplot(fig21_filtered, aes(x = x_axis, y = excess, fill = threshold)) +
  geom_col(width = 0.7, color = NA) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.1, size = 0.5, alpha = 0.8,
                color = "grey50") +
  geom_label(aes(label = sprintf("%.1f", excess)),
             size = 2.5, 
             color = "black",
             fill = "white", alpha = 0.5,
             label.size = 0,
             position = position_stack(vjust = 0.05),
             label.r = unit(0.1, "cm"),
             vjust = 0) +
  facet_grid(season ~ approach, scales = "free", space='free') +
  xlab("") +
  ylab("Number of deaths (excess mortality)") +
  labs(fill = NULL) +
  scale_fill_manual(values = custom_colors) +
  theme_light(base_size = 14) +
   scale_y_continuous(
     breaks = function(x) {
       # Include 0 in breaks
       range_min <- min(0, floor(min(x) / 20) * 20)
       range_max <- ceiling(max(x) / 20) * 20
       seq(range_min, range_max, by = 20)
     }, expand = expansion(mult = c(0.01, 0.1))) +
  # scale_y_continuous(
  #   limits = \(x) range(pretty(x, n = 10)),
  #   breaks = scales::pretty_breaks(n = 10)
  # ) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 8), 
    axis.text.x = element_blank(),
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 4),
    axis.title.y = element_text(face = "bold", size = 12, angle = 90, hjust = 0.5, vjust = 2, margin = margin(r = 0)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10),
    strip.text = element_text(face = "bold", color = "black"),
    panel.spacing = unit(1.5, "lines"))

excess_by_season


# save the plot
# Create a filename
file_name <- "Fig21_excess_by_season.png"
# Full path
full_path <- file.path(save_path, file_name)
ggsave(full_path, plot = excess_by_season , width = 6, height = 8, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA)
)


#-------------------------------------------------------------------------
#-- Excess mortality for the ten worst heatwave episodes during 2001–2021 
#--------------------------------------------------------------------------

top10_season_ep <- all_season_ep_summary %>%
  group_by(approach, threshold) %>%
  slice_max(order_by = excess_temp, n = 10, with_ties = FALSE) %>%
  arrange(approach, threshold, episode) %>%   # sort within each approach
  ungroup()

# export to excel summary_hw_mort
# Load the existing file
summary <- loadWorkbook(file.path(save_path, "summary_hw_mort.xlsx"))
# Add as a new sheet
if ("top10_season_ep" %in% names(summary)) removeWorksheet(summary, "top10_season_ep")
addWorksheet(summary, "top10_season_ep")
writeData(summary, "top10_season_ep", top10_season_ep)

# Save without removing existing sheets
saveWorkbook(summary, file.path(save_path, "summary_hw_mort.xlsx"), overwrite = TRUE)

#--------------------------------
# Total each approach by top 10 season
#--------------------------------
top10_season_ep_total <- top10_season_ep %>%
  dplyr::filter(is.finite(duration) | is.na(duration)) %>%  # drop +/-Inf only
  dplyr::group_by(approach, threshold) %>%
  dplyr::summarise(
    duration_total = sum(duration, na.rm = TRUE),
    excess         = round(sum(excess_sum,   na.rm = TRUE),1),
    baseline       = round(sum(baseline_sum, na.rm = TRUE),1),
    observed       = sum(observed_sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    lower_CI_sum = qchisq(0.025, 2 * observed) / 2,
    upper_CI_sum = qchisq(0.975, 2 * (observed + 1)) / 2,
    lower_CI     = round(lower_CI_sum - baseline, 1),
    upper_CI     = round(upper_CI_sum - baseline, 1),
    excess_mortality = paste0(excess, " (", lower_CI," to ",upper_CI,")")
  )

# export to excel summary_hw_mort
# Load the existing file
summary <- loadWorkbook(file.path(save_path, "summary_hw_mort.xlsx"))
# Add as a new sheet
if ("top10_season_ep_total" %in% names(summary)) removeWorksheet(summary, "top10_season_ep_total")
addWorksheet(summary, "top10_season_ep_total")
writeData(summary, "top10_season_ep_total", top10_season_ep_total)

# Save without removing existing sheets
saveWorkbook(summary, file.path(save_path, "summary_hw_mort.xlsx"), overwrite = TRUE)

#-----------------------------------------------------------------------------
# Figure 22: Number of cumulative excess deaths during heatwave episodes, total (2000/01 to 2020/21)
#-----------------------------------------------------------------------------

top10_season_ep_total$approach <- factor(top10_season_ep_total$approach, levels = c("EHF", "MetService"))
top10_season_ep_total <- top10_season_ep_total %>%
  mutate(x_axis = interaction(approach, threshold, sep = " / "))

excess_top10 <- ggplot(top10_season_ep_total, aes(x = x_axis, y = excess, fill = threshold)) +
  geom_col(width = 0.7, color = NA) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.1, size = 0.5, alpha = 0.8,
                color = "grey50") +
  geom_label(aes(label = sprintf("%.1f", excess)),
             color = "black",
             fill = "white", alpha = 0.5,
             label.size = 0,
             position = position_stack(vjust = 0.05),
             label.r = unit(0.1, "cm"),
             vjust = 0) +
  facet_grid(. ~ approach, scales = "free", space='free') +
  xlab("Ten worst heatwaves identified by each approach (2000/01 to 2020/21)") +
  ylab("Number of deaths\n(excess mortality)") +
  labs(fill = NULL) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(
    breaks = function(x) {
      seq(from = floor(min(x) / 50) * 50,
          to   = ceiling(max(x) / 50) * 50,
          by   = 50)
    }) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 10), 
    axis.text.x = element_blank(),
    axis.title.x = element_text(face = "bold", size = 12, angle = 0, hjust = 0.5, vjust = 4),
    axis.title.y = element_text(face = "bold", size = 12, angle = 0, hjust = 0, vjust = 1.13, margin = margin(r = -95)),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 0, l = 10),
    strip.text = element_text(face = "bold", size = 12))

excess_top10

# save the plot
# Create a filename
file_name <- "Fig22_excess_during_hw_top10.png"
# Full path
full_path <- file.path(save_path, file_name)
ggsave(full_path, plot = excess_top10 , width = 8, height = 5, dpi = 300) + theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA)
)



