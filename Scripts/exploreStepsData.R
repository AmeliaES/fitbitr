# Analyse steps data from my FitBit
# Ideas:
# Find out how many steps I do a day on average
# Find out if there's a relationship with time of year, eg. less active in winter more active in summer
# Find out if there's a relationship with the day, eg. weekday or weekend
# Find out when during the day am I usually most active

# Explore missing data, could my results be bias because I don't wear my fitbit at certain times in the day?
# Also need to find out what sets the intervals in data collection when I am wearing my fitbit, as i dont think it's every minute...

# ------------------------------
# Load libraries
library(rjson) # for reading in JSON files - steps data from fitbit is json
library(stringr)
library(dplyr)
library(lubridate) # for manipulating dates and times
library(tibble)
library(ggplot2)
library(scales) # allows labeling of axes in ggplot to change scientific notation to using commas for big numebrs
library(tidyr)
# ------------------------------
# Set ggplot theme
# Define season colours
seasonColours <- c("Spring" = "#a0c73e",
                   "Summer" = "#e35bd2",
                   "Autumn" = "#f5ae2c",
                   "Winter" = "#afd3e3")

# Define colours for the night
nightColours <- c("night" = "#6f6278",
                  "dawnDusk" = "#d9b6b8",
                  "day" = "#f9fade")

# Custom theme with defined colors
custom_theme <- function() {
  theme_minimal() +
    theme(text = element_text(size = 15))
}

# Set the custom theme globally
theme_set(custom_theme())


# ------------------------------
# Load data
# Find all files that contain raw steps data (lots of the files are in JSON format)
pathSteps <- "/Users/aes/GitRepos/fitbitr/Takeout/Fitbit/Global Export Data/"
dates <- list.files(pathSteps) %>%
  str_subset("steps") %>% # only files containing steps
  str_extract("\\d{4}-\\d{2}-\\d{2}") %>% # extract just the date part
  str_subset("^2017", negate = T) # remove dates in 2017, as this was data from a different fitbit

# Loop over each date to extract the steps data from each file
data <- lapply(dates, function(date){
steps <- fromJSON(file= paste0(pathSteps, "steps-",date,".json"))

data <- data.frame( dateTime = unname(unlist(sapply(steps,function(x) x[1]))) , # extracts the date and time stamp
                    steps = as.numeric(unname(unlist(sapply(steps,function(x) x[2]))))) # extracts the number of steps

return(data)
}) %>% do.call(rbind,.)


# ------------------------------
# Extract date from the dateTime stamp. Date is in the format of MM/DD/YY, time is HH:MM:SS
data <- data %>%
  mutate(dateTime = as_datetime(dateTime, format = "%m/%d/%y %H:%M:%S")) %>%
  mutate(date = date(dateTime)) %>%
  mutate(time = as_hms(dateTime)) %>%
  as.tibble()

# ------------------------------
# Whats the interval between times?
data <- data %>%
  group_by(date) %>% # this ensures intervals are only done per day
  arrange(date, time) %>%
  mutate(interval = as.numeric(difftime(lead(dateTime), dateTime, units = "mins"))) %>%
  ungroup()

# check
data %>%
  filter(date == "2022-07-06" | date == "2022-07-07") %>%
  print(n = nrow(.))
# at the end of 6th the entry for interval is NA, which is what we expect.

# Plot the interval per steps
png("Plots/intervals.png", res = 300, height = 1500, width = 3000)
print(
data %>%
  drop_na() %>%
ggplot(data = .) +
  geom_point(aes(x = interval/60, y = steps), alpha = 0.5) +
  labs(x = "Time between data collection (hours)",
       y = "Number of steps")
)
dev.off()
# Hard to see a relationship here, also interval may be high when i take the fitbit off for times during the day
# but even when I am wearing it it looks like it's not collecting data every minute
# why is there a minus point?


# ------------------------------
# Whats the max number of steps I ever did in a minute?!
data %>%
  filter(interval == 1) %>%
  filter(steps == max(steps)) # 177

png("Plots/stepsPerMinute.png", res = 300, height = 1600, width = 3200)
print(
data %>%
  filter(interval == 1) %>%
  filter(steps != 0) %>%
ggplot(data = .) +
  geom_bar(aes(x = steps)) +
  labs(x = "Number of steps per minute")
)
# Interesting! I hope this doesn't suggest I spend most my time shuffling around the house!
# Could incorporate heart rate data here to see if the second peak is walking and the third much smaller peak is running/dancing.
dev.off()

# ------------------------------
# Add up the amount of steps per hour per date
dataByHour <- data %>%
  group_by(date, hour = hour(dateTime)) %>%
  summarise(total_steps = sum(steps)) %>%
  ungroup()

head(dataByHour)

min(dataByHour$hour)
# this is 0, change to 1, as it's more intuitive for hour to be 1 to 24 not 0 to 23
dataByHour <- dataByHour %>%
  mutate(hour = hour + 1)

# Check that is correct
data %>%
  filter(date == "2022-07-06") %>%
  filter(hour(time) == 16) %>%
  pull(steps) %>% sum() # looks good, same as in dataByHour

# ------------------------------
# Data of steps per day
dataByDay <- dataByHour %>%
  group_by(date) %>%
  summarise(stepsPerDay = sum(total_steps))

# ------------------------------
# Split into weekday/weekend
dataByHour <- dataByHour %>%
  mutate(day_of_week = factor(weekdays(date), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  mutate(weekend = ifelse(day_of_week %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

dataByDay <- dataByDay %>%
  mutate(day_of_week = factor(weekdays(date), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  mutate(weekend = ifelse(day_of_week %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

# Function to produce summary statistics (mean and +/- sd)
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

# Plot the number of steps per day type
png("Plots/stepsPerDay.png", res = 300, height = 1500, width = 2000)
print(
dataByDay %>%
ggplot(data = ., aes( x = day_of_week, y = stepsPerDay)) +
  geom_violin(fill = "deepskyblue4", alpha = 0.5)+
  stat_summary(fun.data=data_summary) +
  labs(x = "",
       y = "Steps per day") +
  scale_y_continuous(labels = label_number(big.mark = ",", decimal.mark = ".", suffix = ""))
)
dev.off()

# ------------------------------
# let's check our data range, if we're going to look at seasons and months we should look at the same amount of data for each month
min(data$date)
max(data$date)
# Let's subset to begin from Spring 2023
dataByDay <- dataByDay %>%
  filter(date >= "2023-03-01" & date < "2024-03-01")

dataByHour <- dataByHour %>%
  filter(date >= "2023-03-01" & date < "2024-03-01")

# ------------------------------
# Add a column for season
# Define months for each season, spring is 3:5, ie. March to May
spring = 3:5
summer = 6:8
autumn = 9:11
winter = c(12, 1,2)

dataByDay <- dataByDay %>%
  mutate(season = factor(case_when(month(date) %in% spring ~ "Spring",
                                   month(date) %in% summer ~ "Summer",
                                   month(date) %in% autumn ~ "Autumn",
                                   month(date) %in% winter ~ "Winter"),
                         levels = c("Spring", "Summer", "Winter", "Autumn")))

dataByHour <- dataByHour %>%
  mutate(season = factor(case_when(month(date) %in% spring ~ "Spring",
                                   month(date) %in% summer ~ "Summer",
                                   month(date) %in% autumn ~ "Autumn",
                                   month(date) %in% winter ~ "Winter"),
                         levels = c("Spring", "Summer", "Winter", "Autumn")))



png("Plots/stepsPerDaySeason.png", res = 300, height = 2000, width = 2500)
print(
dataByDay %>%
  ggplot(data = ., aes( x = day_of_week, y = stepsPerDay)) +
  geom_violin(aes(fill = season), alpha = 0.5)+
  stat_summary(fun.data=data_summary) +
  labs(x = "",
       y = "Steps per day") +
  scale_fill_manual(values = seasonColours, guide = "none") +
  facet_wrap(~ season) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(labels = label_number(big.mark = ",", decimal.mark = ".", suffix = ""))
)
dev.off()


# -------------------------------------
# Plot a bar plot per season for steps per hour
# For all days vs only weekend data

png("Plots/StepsPerHour.png", res = 300, height = 1500, width = 2000)
dataByHour %>%
  group_by(season, hour) %>%
  summarise(mean = mean(total_steps)) %>%
  ggplot(data = ., aes(x = hour, y = mean)) +
  geom_col(aes(fill = season),  position = "dodge")+
  scale_fill_manual(values = seasonColours)+
  scale_x_continuous(breaks = 1:24) +
  scale_y_continuous(labels = label_number(big.mark = ",", decimal.mark = ".", suffix = "")) +
  labs(fill = "",
       y = "Mean number of steps",
       x = "Time of day (Hour)")
dev.off()


# See if we can make this plot nicer

circularSeasonsPlot <- function(data, ylim, title, ylimMinus){
  data %>%
    group_by(season,hour) %>%
    summarise(mean = mean(total_steps, na.rm = T)) %>%
    mutate(daylight = factor(case_when(season == "Spring" & hour %in% c(20:24, 1:5) ~ "night",
                                       season == "Spring" & hour %in% c(6,19) ~ "dawnDusk",
                                       season == "Spring" & hour %in% c(7:18) ~ "day",
                                       season == "Summer" & hour %in% c(21:24,1:4) ~ "night",
                                       season == "Summer" & hour %in% c(5,20) ~ "dawnDusk",
                                       season == "Summer" & hour %in% c(6:19) ~ "day",
                                       season == "Autumn" & hour %in% c(19:24,1:6) ~ "night",
                                       season == "Autumn" & hour %in% c(7,18) ~ "dawnDusk",
                                       season == "Autumn" & hour %in% c(8:17) ~ "day",
                                       season == "Winter" & hour %in% c(18:24,1:7) ~ "night",
                                       season == "Winter" & hour %in% c(8,17) ~ "dawnDusk",
                                       season == "Winter" & hour %in% c(9:16) ~ "day",) )) %>%
    mutate(innerColourValue = ylimMinus+70) %>%
    ggplot(data = . ) +
    geom_col(aes(x = hour, y = mean, fill = season),  position = "dodge")+
    geom_col(aes(x = hour, y = innerColourValue, fill = daylight, color = daylight) , position = "dodge")+
    scale_fill_manual(values = c(seasonColours, nightColours) , guide = "none") +
    scale_color_manual(values = c(seasonColours, nightColours) , guide = "none") +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      text = element_text(size = 15),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.caption = element_text( face = "italic", color = "#aba5ad", lineheight = 0.4),
      plot.background = element_rect(fill = "#f5f5f2", color = "white"),
      plot.title = element_text(hjust = 0.5, size = 12, color = "#49374f"),
      strip.text = element_text(size = 10, color = "#49374f")
    ) +
    labs(fill = "") +
    coord_polar(start = 6.15) +
    facet_wrap(~season) +
    scale_x_continuous(breaks = 1:24, labels = 1:24)+
    scale_y_continuous(breaks = seq(0,2000, 500), limits = c(ylimMinus,ylim)) +
    geom_text(aes(x = hour, y = -300, label = hour ), size = 2.5, color = "#aba5ad")+
    labs(caption = "Each circular grid line represents 500 steps.\n
       Inner circles represent light levels of day and night.",
         title = title)
}


png("Plots/StepsPerHourCircular.png", res = 500, height = 3000, width = 3000)
print(
  circularSeasonsPlot(data = dataByHour,
                      ylim = 1200,
                      ylimMinus = -2000,
                      title = "Average number of steps taken per hour\nacross multiple days in each season")
)
dev.off()


# Restrict to weekend data, as we know this is higher
png("Plots/StepsPerHourCircularWeekend.png", res = 500, height = 3000, width = 3000)
 print(
   circularSeasonsPlot(data = filter(dataByHour, weekend == "Weekend"),
                       ylim = 1900,
                       ylimMinus = -3000,
                       title = "Average number of steps taken per hour\nacross weekends in each season")
 )
 dev.off()







