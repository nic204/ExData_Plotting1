library(tidyverse)
library(dplyr)
library(lubridate)


consumption <- read.table("household_power_consumption.txt", 
                          sep = ";", header = TRUE, 
                          stringsAsFactors = FALSE, na.strings = "?") %>%
  mutate(
    Date = dmy(Date),
    Global_active_power = as.numeric(Global_active_power),
    DateTime = as.POSIXct(
      paste(Date, Time),         
      format = "%Y-%m-%d %H:%M:%S")
  ) %>%
  filter(Date %in% c(as.Date("2007-02-01"), as.Date("2007-02-02")))


ggplot(consumption, aes(x = Global_active_power)) + # Specify data and map 'value' to x-axis
  geom_histogram(bins = 12, fill= "red", color = "black") +         # Add the histogram layer
  labs(title = "Global Active Power", # Add labels
       x = "Value",
       y = "Global Active Power (Kilowatts")+
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)  # center the title
  )

ggplot(consumption, aes(x = DateTime, y = Global_active_power)) +
  geom_line() +
  labs(
    x = "",
    y = "Global Active Power (kilowatts)"
  ) +
  scale_x_datetime(
    date_breaks = "1 day",
    date_labels = "%a"           # Thu, Fri, Sat
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  )


# Sub-metering plot -----------------------------------------------
ggplot(consumption, aes(x = DateTime)) +
  geom_line(aes(y = Sub_metering_1, color = "Sub_metering_1")) +
  geom_line(aes(y = Sub_metering_2, color = "Sub_metering_2")) +
  geom_line(aes(y = Sub_metering_3, color = "Sub_metering_3")) +
  scale_color_manual(
    values = c(
      "Sub_metering_1" = "black",
      "Sub_metering_2" = "red",
      "Sub_metering_3" = "blue"
    ),
    breaks = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
    guide = guide_legend(title = "")
  ) +
  scale_x_datetime(
    date_breaks = "1 day",
    date_labels = "%a"   # Thu, Fri, Sat
  ) +
  labs(
    x = "",
    y = "Energy sub metering"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.85, 0.85)  # similar location to base R
  )


