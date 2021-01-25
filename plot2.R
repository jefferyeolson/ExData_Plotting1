library(tidyverse)
library(lubridate)
power <- read.table("household_power_consumption.txt", sep = ";", quote = "")
names(power) <- power[1,]
power <- power[-1,]
power <- power %>%
        mutate(date_time = as.POSIXlt(paste(power$Date, power$Time, sep = " "), format = "%d/%m/%Y %H:%M:%S"), 
               global_active_power = as.numeric(Global_active_power, length = 4), 
               global_reactive_power = as.numeric(Global_reactive_power, length = 4),
               voltage = as.numeric(Voltage, length = 6),
               global_intensity = as.numeric(Global_intensity, length = 5),
               sub_metering_1 = as.numeric(Sub_metering_1, length = 4),
               sub_metering_2 = as.numeric(Sub_metering_2, length = 4),
               sub_metering_3 = as.numeric(Sub_metering_3, length = 4)
        ) %>%
        select(date_time:sub_metering_3) %>%
        filter(date_time >= "2007-02-01" & date_time < "2007-02-03")
dev.print(png, file = "plot2.png",
          width = 480, 
          height = 480, 
          units = "px")
png(file = "plot2.png", bg = "transparent")
plot(power$date_time, 
     power$global_active_power, 
     type = "l", 
     xlab = "", 
     ylab = "Global Active Power (kilowatts")
dev.off()
