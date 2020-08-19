corona <- read.csv("covid_19_data.csv")
library(tidyverse)
library(ggplot2)
library(sf)
library(mapdata)
library(maps)
library(maptools)
install.packages('raster')
library(raster)
library(dplyr)

local <- getwd()

chinaMap<-getData('GADM',country="CHN",level=1) #province level; getData is part of package raster
ChinaRD0<-readRDS("/Users/johnzhang/Documents/novel-corona-virus-2019-dataset/CHN_adm0.rds") #Country outline
ChinaRD1<-readRDS("/Users/johnzhang/Documents/novel-corona-virus-2019-dataset/CHN_adm1.rds") #provinces
ChinaRD2<-readRDS("/Users/johnzhang/Documents/novel-corona-virus-2019-dataset/CHN_adm2.rds") #prefectures
ChinaRD3<-readRDS("/Users/johnzhang/Documents/novel-corona-virus-2019-dataset/CHN_adm3.rds") #counties
names(ChinaRD1) #variables in ChinaRD1 provincial dataset

local_dir <- "/Users/johnzhang/Documents/novel-corona-virus-2019-dataset/China.ship/"





china_map1 <- readShapePoly("bou2_4p.shp")
length(china_map1)
china_map2 <- china_map1@data
head(china_map2)
china_map3 <- fortify(china_map1)
china_map2$NAME <- iconv(china_map2$NAME, from = 'GBK')
plot(china_map1)
ggplot(china_map1, aes(x = long, y = lat, group = group)) +
  geom_path(color = "grey40") +
  geom_polygon(fill = 'beige')
china_map2$NAME <- iconv(china_map2$NAME, from = "GBK") ## transform to UTF-8 coding format
china_map2x <- data.frame(china_map2,id=seq(0:924)-1) ## prepare to join by id
library(plyr)
china_map4 <- join(china_map3, china_map2x, type = "full")
ggplot(china_map4, aes(x = long, y = lat, group = group, fill = NAME)) +
  geom_path(color = 'grey40') +
  geom_polygon() +
  scale_fill_manual(values = rainbow(33), guide = F) +
  coord_map()
NAME <- c("北京市", "天津市", "河北省", "山西省", "内蒙古自治区", "辽宁省", "吉林省", "黑龙江省", "上海市", "江苏省", "浙江省", "安徽省", "福建省", "江西省", "山东省", "河南省", "湖北省",  "湖南省", "广东省", "广西壮族自治区", "海南省", "重庆市", "四川省", "贵州省", "云南省", "西藏自治区", "陕西省", "甘肃省", "青海省", "宁夏回族自治区", "新疆维吾尔自治区", "台湾省", "香港特别行政区")
pop <- c(7355291, 3963604, 20813492, 10654162, 8470472, 15334912, 9162183,  13192935, 8893483, 25635291, 20060115, 19322432, 11971873, 11847841, 30794664, 26404973, 17253385, 19029894, 32222752, 13467663, 2451819, 10272559, 26383458, 10745630, 12695396, 689521, 11084516, 7113833, 1586635, 1945064, 6902850, 23193638, 7026400)
pop <- data.frame(NAME, pop)
china_map_pop <- join(china_map4, pop, type = "full")
ggplot(china_map_pop, aes(x = long, y = lat, group = group, fill = pop)) +
  geom_polygon() +
  geom_path(color = "grey40") +
  coord_map()


WuhanCities<-ChinaRD3[ChinaRD3$NAME_2=="Wuhan",]
plot(WuhanCities)
plot(WuhanCities[WuhanCities$NAME_3=="Wuhan",],border="blue",add=TRUE) #Notice add=T; this draws on top of current plot
text(coordinates(WuhanCities),
     labels=WuhanCities$NAME_3,col="red",cex=0.7)




wuhan_map = fortify(WuhanCities) # ggplot2::fortify converts "sp" object to data.frame

ggplot(wuhan_map, aes(long, lat, group=group, fill=id)) +
  geom_polygon(show.legend = F) +
  ggtitle("Map of Hubei and Wuhan")

view(hubei)
hubei <- corona %>% filter(Country.Region == 'Mainland China')%>% filter(Province.State == 'Hubei')

scatter.smooth(x  = hubei$ObservationDate , hubei$ Confirmed) #Shows Confirmed Cases Over Time

Chinaprovinces_day1_hubei <- corona %>% filter(Province.State!= "") %>% group_by(Province.State) %>%filter(ObservationDate == '01/22/2020') 
ggplot(Chinaprovinces_day1_hubei,aes(x = Province.State,y = Confirmed)) + geom_point()

ggplot(Chinaprovinces_day1_hubei, aes(Confirmed,fill = Province.State)) +
  geom_histogram(bins = 20)

Chinaprovinces_daylast_hubei <- corona %>% filter(Province.State!= "") %>% group_by(Province.State) %>%filter(ObservationDate == '03/10/20') %>%filter(Country.Region == 'Mainland China')
ggplot(Chinaprovinces_daylast_hubei, aes(Confirmed,fill=Province.State))+geom_histogram(bins=20)

Chinaprovinces_day1_no_hubeicorona<- corona %>% filter(Province.State != "") %>% group_by(Province.State) %>%filter(ObservationDate == '01/22/2020') %>%filter(Country.Region == 'Mainland China') %>% filter(Province.State != "Hubei") 
ggplot(Chinaprovinces_day1_no_hubeicorona,aes(Confirmed,fill=Province.State))+geom_histogram(bins=20)

Chinaprovinces_day_last_nohubei <- corona %>% filter(Province.State != "") %>% group_by(Province.State) %>%filter(ObservationDate == '03/10/20') %>%filter(Country.Region == 'Mainland China') %>% filter(Province.State != "Hubei") 
ggplot(Chinaprovinces_day_last_nohubei,aes(Confirmed,fill=Province.State))+geom_histogram(bins=20)

install.packages(("usmap"))
library(usmap)

plot_usmap(data = statepop, values = "pop_2015", color = "red") + 
  scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
  theme(legend.position = "left")


US <- corona %>% filter(Country.Region == "US") 


Washington <- US %>% filter(Province.State == "Washington") 
ggplot(Washington,aes(Confirmed, fill = Province.State)) + geom_histogram(bins =20 ) +labs(x = "Time Elasped through Two Months", y = "Washington's Confirmed Cases")


NewYork <- US %>% filter(Province.State == ("New York"))  
ggplot(NewYork, aes(ObservationDate,Confirmed,fill=Province.State))+geom_bar(stat='identity')

view(US)

Day1US <- US %>% filter(ObservationDate=='01/22/2020' )
ggplot(Day1US,aes(ObservationDate,Confirmed, fill = Province.State))+geom_point()

Day_last <- US %>% filter(ObservationDate == '03/11/20')
ggplot(Day_last,aes(Confirmed,fill =Province.State))+geom_histogram()

view(corona)

corona_age <- read.csv("Corona_age_set.csv")
corona_age$age <- as.integer(corona_age$age)



ggplot(corona_age, aes(gender, fill = gender) ) +geom_bar()
break_down <- ggplot(corona_age,aes(x = "", y = gender, fill = gender)) + geom_bar(stat = 'identity',width = 1)
pie_chart <- break_down + coord_polar("y", start=0)

par(mfrow = c(3, 2)) 
age_10 <- corona_age %>% filter(age> 0 & age < 10 )
age_25 <- corona_age %>% filter(age > 10 & age < 30)
age_35 <- corona_age %>% filter(age > 30 & age < 60)
age_60_death <- corona_age %>% filter(age > 60 & age < 100)
a <- ggplot(age_10, aes(x= '',age, fill = gender) ) +geom_bar(stat = 'identity') + labs(x = "count of gender", y = "proportion of gender") + coord_polar("y",start=0) 
b <- ggplot(age_25, aes(x= '',age, fill = gender) ) +geom_bar(stat = 'identity') + labs(x = "count of gender", y = "proportion of gender") + coord_polar("y",start=0) 
c <- ggplot(age_35, aes(x= '',age, fill = gender) ) +geom_bar(stat = 'identity') + labs(x = "count of gender", y = "proportion of gender") + coord_polar("y",start=0) 
d <- ggplot(age_60_death, aes(x= '',age, fill = gender) ) +geom_bar(stat = 'identity') + labs(x = "count of gender", y = "proportion of gender") + coord_polar("y",start=0) 

library("cowplot")
plot_grid(a, b, c,d,
          labels = c("proportion 0 - 10 ", " 10- 30", "30- 60" , "60 -100"),
          ncol = 2, nrow = 2)

