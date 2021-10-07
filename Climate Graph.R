# Analysis - P. neochilus paper

## ---------------- Climate Graph ---------------- ##

library(ggplot2)
library(readr)

#data
climate <- read.csv(file.choose())

# if you need a specific order on the x axis create an object.
order <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")

#creates a climate graph with Temperature and Rainfall over the months (2 axis)

tiff('climate.tiff', units="in", width= 4.9, height= 3, res=300, compression = 'lzw')

ggplot(climate)  +
  geom_bar(aes(x= factor(Month, level = order), y=Rainfall),stat="identity", fill="black", colour="black")+
  geom_line(aes(x= factor(Month, level = order), y=Average.Temperature),size = 1, color="#252525", group = 1)+
  geom_point(aes(x= factor(Month, level = order), y=Average.Temperature),size = 2, color="#252525", group = 1)+
  geom_line(aes(x= factor(Month, level = order), y=Max.Temperature),size = 1, color="red", group = 1) +
  geom_point(aes(x= factor(Month, level = order), y=Max.Temperature),size = 2, color="red", group = 1)+
  geom_line(aes(x= factor(Month, level = order), y=Minimum.Temperature),size = 1, color="blue", group = 1)+
  geom_point(aes(x= factor(Month, level = order), y=Minimum.Temperature),size = 2, color="blue", group = 1)+
  scale_y_continuous(sec.axis = sec_axis(~., name = "Temperature (ºC)"))+
  labs( x = "Months")

#better resolution to save the plots
ggsave("climate.png", type = "cairo", dpi = 600 )

dev.off()
