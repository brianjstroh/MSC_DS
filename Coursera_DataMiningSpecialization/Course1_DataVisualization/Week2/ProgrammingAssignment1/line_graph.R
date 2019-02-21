library(ggplot2)
library(plotly)


unzip("rawdata.zip")
GISTEMP.data <-read.csv(file = unzip("rawdata.zip",list = TRUE)$Name[2])
flat_line_data <-select(GISTEMP.data, Year, Global = Glob, names(GISTEMP.data)[8:15])
flat_line_data <-melt (flat_line_data, id.vars = names(line_data)[1], variable.name = "Region", value.name = "Temp.Diffs")

#ggplot(data = filter(flat_line_data, Region != "Global"), aes(x= Year, y = Temp.Diffs, color = Region)) + geom_line()
#ggplot(data = flat_line_data, aes(x= Year, y = Temp.Diffs, color = Region)) + geom_line()

flat_line_data$decadegroups <- paste0(trunc(flat_line_data$Year/10),flat_line_data$Region)
decade_line_data <- aggregate(flat_line_data[,3], flat_line_data[4], mean)
decade_line_data$decade <- as.integer(substr(decade_line_data$decadegroups,1,3))*10
decade_line_data$Region <- as.character(substr(decade_line_data$decadegroups,4,20))

colnames(decade_line_data)[colnames(decade_line_data)=="x"] <- "Avg.Temp"

my_plot <- ggplot(data = decade_line_data, aes(x= decade, y = Avg.Temp, color = Region)) + 
                  geom_line()+
                  geom_point()+
                  xlab("Decade")+
                  ylab("Average Temperature Anomaly")+
                  ggtitle("Average Temperature Anomalies by Decade and Latitude Region")
my_plot <- ggplotly(my_plot)
