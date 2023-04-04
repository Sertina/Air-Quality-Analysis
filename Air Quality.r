---
#title: "Air Quality Analysis"
#author: "Sertina"
#date: "8/4/2021"
#output: pdf_document
---

## Load libraries & import the data
library(tidyverse)
Air_Quality<-read.csv(file="airquality.csv")

str(Air_Quality) #Print the structure
names(Air_Quality) #List the variables
head(Air_Quality, 15) #Print the top 15 rows

## User-Defined Function
square_of_solar<-function(){
  Air_Quality2<-Air_Quality
  (Air_Quality2$Solar.R)^2
}

square_of_solar()

## Filter rows
Air_Quality<-filter(Air_Quality, Air_Quality$Wind<10) 

## Independent Variables: Ozone, Solar.R, Wind, Temp 
## Dependent Variable: Day
## Unused Variable: X
Air_Quality <- cbind(Air_Quality$Ozone,
                     Air_Quality$Solar.R, Air_Quality$Wind,
                     Air_Quality$Temp)
Air_Quality = as.data.frame(Air_Quality)

## Remove missing values & duplicate rows
Air_Quality<-na.omit(Air_Quality)
Air_Quality %>% distinct()

head(Air_Quality, 15)

## Rename columns
Air_Quality<-rename(Air_Quality, Ozone=V1, Solar_Rad=V2, Wind=V3, Temperature=V4)

## Reorder rows in descending order
Air_Quality %>% arrange(desc(Air_Quality$Ozone)) 

## Add new variables
Air_Quality$Double_Wind = (Air_Quality$Wind)*2
Air_Quality$Half_Ozone = (Air_Quality$Ozone)/2
head(Air_Quality, 8)

## Create a training set using random number generator engine
set.seed(1234)
Air_Quality %>% sample_frac(0.80, replace = FALSE) 

## Calculate descriptive statistics
summary(Air_Quality)
mean(Air_Quality$Ozone)
median(Air_Quality$Ozone)
range(Air_Quality$Ozone)

## User-defined mode function
user_mode<-function(x){
  modeVal<-unique(x)
  
  #Match returns a vector of the positions of the first 
  #matches of its arguments
  modeVal[which.max(tabulate(match(x, modeVal)))]
}

user_mode(Air_Quality$Ozone)

## Bar Plot 

#Tell geom_bar that y-values will be provided
ggplot(data=Air_Quality, aes(x=Ozone, y=Temperature)) +
  geom_bar(stat="identity", fill="red") +
  labs(y="Temperature")

## Scatter Plot 

#Turn off confidence intervals
ggplot(data=Air_Quality, aes(x=Ozone, y=Temperature)) +
  geom_point(color="red") + labs(y="Temperature") +
  geom_smooth(method='lm', se=FALSE)

## Calculate Pearson correlation
cor(Air_Quality$Ozone, Air_Quality$Temperature, method="pearson")

## Conclusion
#Based on our analysis, there is a correlation between Ozone & Temperature. 
#From the bar plot it can be seen that 
#the temperature reaches its maximum around 25 for ozone. 
#The scatter plot shows an exponential relationship between temperature & ozone.