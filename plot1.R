library(dplyr)

#reading the data into R and selecting given dates
energy_data <- read.table("household_power_consumption.txt", sep = ";",na.strings = "?")
colnames(energy_data) <- energy_data[1,]
energy_data <- energy_data[-1, ] 
energy_tib <- as_tibble(energy_data)
energy_data_sel <- subset(energy_data, energy_data$Date == "1/2/2007"| energy_tib$Date == "2/2/2007")
energy_data_sel <- energy_data_sel %>% mutate_at(c('Global_active_power', 'Global_reactive_power',
        'Voltage', 'Global_intensity', 'Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'), as.numeric)

# Function to convert single-digit day and month to two-digit format and then convert from character
# to date format
convert_to_two_digits <- function(dates) {
        parts <- strsplit(dates, "/")
        dates <- sapply(parts, function(x) {
                x[1] <- sprintf("%02d", as.numeric(x[1])) # Convert day to two digits
                x[2] <- sprintf("%02d", as.numeric(x[2])) # Convert month to two digits
                paste(x, collapse = "/")
        })
        return(dates)
}
energy_data_sel<- energy_data_sel %>% mutate(Date, date = convert_to_two_digits(Date))
energy_data_sel <- energy_data_sel %>% mutate(date, date2 = as.Date(date, format="%d/%m/%Y"))


#creating and saving graph
png('plot1.png', width = 480, height = 480)
hist(energy_data_sel$Global_active_power, main="Global active power", xlab= "Global active power (kilowatts)", col="red")
dev.off()