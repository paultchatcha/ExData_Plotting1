plot3 <- function(){
  
  ### Reading data
  elect <- read.table(file = "household_power_consumption.txt", sep = ";", header = TRUE)
  
  ### Cleaning it
  datetime <- paste(elect$Date, elect$Time)
  DateTime <- strptime(datetime, "%d/%m/%Y %H:%M:%S")
  elect2 <- cbind(DateTime, elect)
  elect3 <- subset(elect2, 
                   DateTime >= as.POSIXct("2007-02-01") & DateTime < as.POSIXct("2007-02-03"), 
                   select = -c(Date, Time))
  clean_data <- transform(elect3, 
                          Global_active_power = as.numeric(as.character(Global_active_power)), 
                          Global_reactive_power = as.numeric(as.character(Global_reactive_power)), 
                          Voltage = as.numeric(as.character(Voltage)), 
                          Global_intensity = as.numeric(as.character(Global_intensity)), 
                          Sub_metering_1 = as.numeric(as.character(Sub_metering_1)), 
                          Sub_metering_2 = as.numeric(as.character(Sub_metering_2)), 
                          Sub_metering_3 = as.numeric(as.character(Sub_metering_3)))
  
  ### Drawing the graph
  png(file = "plot3.png", bg = "transparent")
  plot(x = clean_data$DateTime, y = clean_data$Sub_metering_1, ylab = "Energy sub metering", xlab = "", type = "n")
  lines(clean_data$DateTime, clean_data$Sub_metering_1, lty=1)
  lines(clean_data$DateTime, clean_data$Sub_metering_2, lty=1, col = "red")
  lines(clean_data$DateTime, clean_data$Sub_metering_3, lty=1, col = "blue")
  legend("topright", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = 1, lwd = 1, cex = 0.75)
  dev.off()
}