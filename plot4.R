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
dev.print(png, file = "plot4.png",
          width = 480, 
          height = 480, 
          units = "px")
png(file = "plot4.png", bg = "transparent")
par(mfrow = c(2,2))
plot(x = power$date_time, 
     y =power$global_active_power,
     type = "l",
     xlab = "",
     ylab = "Global Active Power")
plot(x = power$date_time, 
     y =power$voltage,
     type = "l",
     xlab = "datetime",
     ylab = "Voltage")
x = c(power$date_time,
      power$date_time, 
      power$date_time)
y = c(power$sub_metering_1, 
      power$sub_metering_2, 
      power$sub_metering_3) 
plot(x, y, 
     type = "n",
     xlab = "",
     ylab = "Energy sub meeting")
points(x = x[1:2880], 
       y = y[1:2880], 
       type = "l", 
       col = "black",
       xlab = NULL, 
       ylab = "Energy sub meeting")
points(x = x[2881:5760], 
       y = y[2881:5760], 
       type = "l", 
       col = "red", 
       xlab = NULL, 
       ylab = "Energy sub meeting")
points(x = x[5761:8640], 
       y = y[5761:8640], 
       type = "l", 
       col = "blue",
       xlab = NULL, 
       ylab = "Energy sub meeting")
legend("topright",
       legend = c("sub_metering_1", "sub_metering_2", "sub_metering_3"), 
       fill = c("black", "red", "blue"),
       bty = "n",
       cex = 0.6)
plot(x = power$date_time, 
     y =power$global_reactive_power,
     type = "l",
     xlab = "datetime",
     ylab = "Global_reactive_power")
dev.off()
