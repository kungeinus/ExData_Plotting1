library(data.table)
library(lubridate)
housecoms_all<-read.table("household_power_consumption.txt",header = TRUE, sep = ";")
housecoms_all$Date_Time<-as.POSIXct(dmy_hms(paste(housecoms_all$Date,housecoms_all$Time)))
housecoms_sub<-housecoms_all[year(housecoms_all$Date_Time)==2007 & month(housecoms_all$Date_Time)==2 & mday(housecoms_all$Date_Time) %in% c(1,2),c(10,3,4,5,7,8,9)]
rm(list="housecoms_all")
for (i in 2:7){
        housecoms_sub[,i]<-as.numeric(as.character(housecoms_sub[,i]))
}
png(filename = "plot4.png", height = 480, width = 480)
par(mfrow = c(2, 2))
with(housecoms_sub,{
        plot(Global_active_power~Date_Time, xlab="", ylab = "Global Active Power (killowatts)", type = "l")
        plot(Voltage~Date_Time, xlab="datetime", ylab = "Voltage", type = "l")
        plot(Sub_metering_1~Date_Time, xlab = "", ylab = "Energy sub metering", type = "l", col = "black")
        lines(Sub_metering_2~Date_Time, xlab = "", ylab = "Energy sub metering", type = "l", col = "red")
        lines(Sub_metering_3~Date_Time, xlab = "", ylab = "Energy sub metering", type = "l", col = "blue")
        legend("topright", lty = 1, col = c("black", "red","blue"), legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"))
        plot(Global_reactive_power~Date_Time, xlab="datetime", ylab = "Global_reactive_power", type = "l")
}
)
dev.off()