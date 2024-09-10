# Libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(viridis)
library(maps)

#Participant map 
# Get the world polygon and extract USA

USA <- map_data("world") %>% filter(region=="USA")

#Data 
setwd("path")
data=read_excel("nameofdemographicsdatafile.xlsx", sheet="location", col_names = TRUE)

map<- ggplot() +
  geom_polygon(data = USA, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(data=data, aes(x=lon, y=lat, color=count), size=data$count)+ coord_map()+xlim(-150,-50)+ylim(20,50)
#adding color scale
map+scale_size_continuous(range=c(1,12)) +
  scale_color_viridis()+
  theme_void() 

#Education level
data2=read_excel("nameofdemographicsdatafile.xlsx", sheet="education", col_names = TRUE)
p<-ggplot(data=data2, aes(x=level, y=count, fill=level)) +
  geom_bar(stat="identity")
p+ theme_classic()+ scale_fill_brewer(palette="Blues")

#Race 
data3=read_excel("nameofdemographicsdatafile.xlsx", sheet="race", col_names = TRUE)

ggplot(data3, aes(x="", y=count, fill=race)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + theme_void()+ scale_fill_brewer(palette="Set2") 

#Age
data4=read_excel("nameofdemographicsdatafile.xlsx", sheet="age", col_names = TRUE)

p<-ggplot(data=data4, aes(x=age_range, y=count, fill=age_range)) +
  geom_bar(stat="identity")
p+ theme_classic()+ scale_fill_brewer(palette="Blues")

#science curiousity 
data5=read_excel("nameofdemographicsdatafile.xlsx", sheet="science", col_names = TRUE)
p<-ggplot(data=data5, aes(x=percent, y=activity)) +
  geom_bar(stat="identity")
p+ theme_classic()
