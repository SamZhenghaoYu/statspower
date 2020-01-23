
library(readxl)
dat1 <- read_excel("C:/Users/yzhyu/Downloads/Accidental_Drug_Related_Deaths_2012-2018 (1).xlsx")
#relationship between ages and number of accidental drug related deaths
summary(dat1$Age)
dat1$Age<-as.numeric(dat1$Age)
table(dat1$Age)
data_age<-data.frame(table(dat1$Age))


colnames(data_age)=c("Age","Number of people")
data_age$Age<-as.numeric(data_age$Age)
plot(data_age$Age,data_age$`Number of people`,main = "age of accidental drug related deaths 2012-2018",
     ylab="Number of death",xlab="Age",type='l')
lines(lowess(data_age$Age,data_age$`Number of people`), col="blue")
#the number of accidental drug related deaths converge to people with ages of about 40 roughly, 
#but we can also see that the highest number of death converge to ages of about 28 and 50.
#we can see that maybe the reasons why death converge to middle ages
#are stress levels and economical abiltiy.

barplot(data_age$`Number of people`,xlab = as.character(data_age$Age))

#male&accidental drug related death concentration on map
library(readxl)
dat_project <- read_excel("C:/Users/yzhyu/Downloads/Accidental_Drug_Related_Deaths_2012-2018.xlsx")
library(ggmap)#Load libraries
library(ggplot2)

dat_project<-dat_project[dat_project$Sex=="Female",]
latitude<-dat_project$Latitude
city<-dat_project$`death city`
longitude<-dat_project$Longitude
positions <- data.frame(lat=longitude,
                        lon=latitude)#Simulate some geographical coordinates #Switch out for your data that has real GPS coords


register_google(key = "AIzaSyAYlWqF-yuYPVX_geEi4A_mujjFwkg2nTM")

dat_project$Longitude[dat_project$`death city`=="Berlin"]
#use them to find the coordinates of middle point of connecticut
dat_project$Latitude[dat_project$`death city`=="Berlin"]
map <- get_map(location=c(lon=-72.7493,
                          lat=41.62605), zoom=8, maptype='roadmap', color='bw')#Get the map from Google Maps
ggmap(map, extent = "device") +
  geom_density2d(data = positions, aes(x = lon, y = lat), size = 0.3) + 
  stat_density2d(data = positions, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)


#female&accidental drug related death concentration on map
library(readxl)
dat_project <- read_excel("C:/Users/yzhyu/Downloads/Accidental_Drug_Related_Deaths_2012-2018.xlsx")
library(ggmap)#Load libraries
library(ggplot2)
dat_project<-dat_project[dat_project$Sex=="Female",]
length(dat_project$ID)

latitude<-dat_project$Latitude
city<-dat_project$`death city`
longitude<-dat_project$Longitude
positions <- data.frame(lat=longitude,
                        lon=latitude)#Simulate some geographical coordinates #Switch out for your data that has real GPS coords


register_google(key = "AIzaSyAYlWqF-yuYPVX_geEi4A_mujjFwkg2nTM")

dat_project$Longitude[dat_project$`death city`=="Berlin"]
#use them to find the coordinates of middle point of connecticut
dat_project$Latitude[dat_project$`death city`=="Berlin"]
map <- get_map(location=c(lon=-72.7493,
                          lat=41.62605), zoom=8, maptype='roadmap', color='bw')#Get the map from Google Maps
ggmap(map, extent = "device") +
  geom_density2d(data = positions, aes(x = lon, y = lat), size = 0.3) + 
  stat_density2d(data = positions, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)
#contingency table
library(readxl)
dat_project <- read_excel("C:/Users/yzhyu/Downloads/Accidental_Drug_Related_Deaths_2012-2018.xlsx")
#Stacked Barplot of race&sex of Accidental Drug-related deaths in 2012-2018
table(dat_project$Sex,dat_project$Race)
dat_sexrace<-table(dat_project$Sex,dat_project$Race)
barplot(dat_sexrace,col=c("red","yellow","green"),legend= rownames(dat_sexrace),
        main="Stacked Barplot of race&sex of Accidental Drug-related deaths in 2012-2018")



#pie_chart for race in accidental drug_related deaths
library(readxl)
dat_project <- read_excel("C:/Users/yzhyu/Downloads/Accidental_Drug_Related_Deaths_2012-2018.xlsx")
dat_project
pie(table(dat_project$Race),col = rainbow(6),main="Pie chart for races")
mydata<-data.frame(table(dat_project$Race))
mydata
pct<-round(mydata$Freq/(length(dat_project$Race))*100,digits = 1)
lbls <- paste(mydata$Var1, pct)
lbls <- paste(lbls,"%",sep="")
pie(table(dat_project$Race),labels = lbls, col = rainbow(6),main="Pie chart for races")

#piechart: comparison of accidental drug-related death percentages in race population
race<-c("Hispanic&White"," White","Black")
percentage<-c(0.133,0.67,0.110)
data_new<-data.frame(race,percentage)
#black 3, HW 7, white 11
data_black<-mydata$Freq[3]/(data_new$percentage[3]*3572665)*100
data_HW<-mydata$Freq[7]/(data_new$percentage[1]*3572665)*100
data_white<-mydata$Freq[11]/(data_new$percentage[2]*3572665)*100
kk<-c(data_HW,data_white,data_black)
#comparison of ratios
aaa<-data_white/(data_white+data_HW+data_black)
bbb<-data_HW/(data_white+data_HW+data_black)
ccc<-data_black/(data_white+data_HW+data_black)
pct1<-round(c(bbb,aaa,ccc)*100,digits=1)
lbls1 <- paste(race, pct1)
lbls1 <- paste(lbls1,"%",sep="")
race
bb<-data.frame(race, kk)
pie(bb$kk,labels = lbls1,col =c("blue","green","yellow"),main = "piechart: comparison of accidental drug-related death percentages in race population")

#density plot for age&Black
library(readxl)
dat_project <- read_excel("C:/Users/yzhyu/Downloads/Accidental_Drug_Related_Deaths_2012-2018.xlsx")
dat_new<-dat_project[(dat_project$Race=="Black"),]
dat_new
d<-density(dat_new$Age,na.rm=T)
#ignore NA REFERENCE:https://stackoverflow.com/questions/22491210/proper-density-plotting
plot(d,main="Kernal density of accidental drug_related deaths for Black people")
polygon(d,col="blue")
#the number of accidental drug related deaths converge to black people with ages of about 50.
#we can see that maybe the reasons why death converge to middle ages is health condition.

#scatterplot colored by smooth density
library(readxl)
mydata<-read_excel("C:/Users/yzhyu/Downloads/Accidental_Drug_Related_Deaths_2012-2018.xlsx")
mydata$Lon<-mydata$Latitude
mydata$Lat<-mydata$Longitude

with(mydata,smoothScatter(Lon, Lat),
                   main="Scatter Plot colored by Smoothed Densities")


table(mydata$Location,mydata$Heroin)
mydata$Heroin[is.na(mydata$Heroin)]<-"other"

#

library(readxl)
mydata<-read_excel("C:/Users/yzhyu/Downloads/Accidental_Drug_Related_Deaths_2012-2018.xlsx")
mydata$COD<-tolower(mydata$COD)
mydata$cod_heroin<-rep(0,5105)
 for (i in 1:5105){
  if (!is.na(mydata$Heroin[i])){
    mydata$cod_heroin[i] <-1
  }
 }
mydata<-mydata[mydata$cod_heroin==1,]

dat1<-mydata
data_age<-data.frame(table(dat1$Age))

colnames(data_age)=c("Age","Number of people")
data_age$Age<-as.numeric(data_age$Age)

plot(data_age$Age,data_age$`Number of people`,main = "deaths vs age for accidental Heroin_related deaths 2012-2018",
     ylab="Number of death",xlab="Age",type="b")
lines(lowess(data_age$Age,data_age$`Number of people`), col="blue")


library(readxl)
mydata<-read_excel("C:/Users/yzhyu/Downloads/Accidental_Drug_Related_Deaths_2012-2018.xlsx")
mydata$COD<-tolower(mydata$COD)
mydata$cod_cocaine<-rep(0,5105)
for (i in 1:5105){
  if (!is.na(mydata$Cocaine[i])){
    mydata$cod_cocaine[i] <-1
  }
}
mydata$cod_cocaine

colnames(mydata)
dat2<-mydata
colnames(dat2)
data_age2<-data.frame(table(dat2$Age))

colnames(data_age2)=c("Age","Number of people")
data_age2$Age<-as.numeric(data_age2$Age)

plot(data_age2$Age,data_age2$`Number of people`,main = "deaths vs age for accidental Cocaine_related deaths 2012-2018",
     ylab="Number of death",xlab="Age",type="b")
lines(lowess(data_age$Age,data_age$`Number of people`), col="blue")
str(data_age$Age)
#cause of death: heroin vs cocaine
opar <- par(no.readonly=TRUE)
par(lwd=2, cex=1.5, font.lab=2, pin =c(4,4))
plot(data_age$Age, data_age$`Number of people`, type="b",
     pch=15, lty=1, col="red", ylim=c(0, 200),
     main="Cause of Deaths:Heroin_related vs. Cocaine_related",
     xlab="Age", ylab="Number of Deaths")
lines(data_age2$Age, data_age2$`Number of people`, type="b",
      pch=17, lty=2, col="blue")


legend("topright", inset=.05, title="Drug Type", c("Heroin","Cocaine"),
       lty=c(1, 2), pch=c(15, 17), col=c("red", "blue"), bty="n")





#density plot for age& three races
library(readxl)
dat_project <- read_excel("C:/Users/yzhyu/Downloads/Accidental_Drug_Related_Deaths_2012-2018.xlsx")
dat_new<-dat_project[(dat_project$Race=="Black"),]
dat_new
d<-density(dat_new$Age,na.rm=T)
dat_new2<-dat_project[(dat_project$Race=="White"),]
dat_new2
d2<-density(dat_new2$Age,na.rm = T)
plot(d,col="green",main="Densities of accidental deaths(Black, White& Hispanic, White)")
lines(d2,col="red")
dat_new3<-dat_project[(dat_project$Race=="Hispanic, White"),]
dat_new3
d3<-density(dat_new3$Age,na.rm=T)
lines(d3,col="blue")


#residence& deathcities
library(readxl)
dat_project <- read_excel("C:/Users/yzhyu/Downloads/Accidental_Drug_Related_Deaths_2012-2018.xlsx")
dat_new<-dat_project
table(dat_new$ResidenceCity,dat_new$DeathCity)
table(dat_new$ResidenceCity)
str(mydata$Date)
mydata$Date<-as.Date(mydata$Date)
death_date<-table(mydata$Date)
barplot(death_date,col="blue")

library(readxl)
dat_project <- read_excel("C:/Users/yzhyu/Downloads/Accidental_Drug_Related_Deaths_2012-2018.xlsx")
dat_new<-dat_project
length(dat_new[dat_new$DeathCity== "WATERBURY",]$ID)
length(dat_new[dat_new$DeathCity== "STAMFORD",]$ID)

#location 
table(dat_new$Location)
library(readxl)
dat_project <- read_excel("C:/Users/yzhyu/Downloads/Accidental_Drug_Related_Deaths_2012-2018.xlsx")
dat_project
library(plotrix)
pie3D(table(dat_project$Location),labels = (data.frame(table(dat_project$Location)))$Var1,col = rainbow(6),main="Pie chart for Location")
mydata<-data.frame(table(dat_project$Location))
mydata
pct<-round(mydata$Freq/(length(dat_project$Location))*100,digits = 1)
lbls <- paste(mydata$Var1, pct)
lbls <- paste(lbls,"%",sep="")
pie3D(table(dat_project$Location),labels = lbls, col = rainbow(6),main="Pie chart for Location")
#density plot for age&Black