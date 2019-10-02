#charging librarys
library(quantmod)
library(devtools)
library(AnomalyDetection)
library(hrbrthemes)
library(tidyverse)
library(ggplot2)
library(data.table)

#change here the directory to import dataset and travel.R function
setwd("D:/CÓDIGOS PRÓPRIOS EM R/FUNÇÕES")

#importing dataset
eurusd <- fread("EURUSDH1.csv",header=T,sep = ";")
eurusd <- as.data.frame(eurusd)

#lagging close prices to calculate returns
eurusd$close_lag <- Lag(eurusd$close,k=1)
eurusd$close_lag2 <- Lag(eurusd$close,k=2)
eurusd <- eurusd[3:length(eurusd$close),]
eurusd$close_lag <- as.integer(eurusd$close_lag)
eurusd$close_lag2 <- as.integer(eurusd$close_lag2)

#calculating returns
eurusd$returns <- eurusd$close_lag-eurusd$close_lag2

#surcing functions
source("travel.R")

#replacing NA's by 0 using travel.R function
eurusd <- travel(eurusd,element = 0)

#separating returns and Date to anamoly analisys
sub_eur <- eurusd[,c("Date","returns")]
sub_eur <- as.data.frame(sub_eur)

#converting Date into POSIXct date type
sub_eur$Date <- as.POSIXct(sub_eur$Date,tz="GMT",
                           tryFormats = c("%Y.%m.%d %H:%M"))


#analysing anamolys with AnomalyDetection
ad_eur <- ad_ts(sub_eur, max_anoms=0.05, direction='both',alpha = 0.05)

# modifying Dates to POSIXct format to be compatible with ggplot2
sub_eur$Date <- as.POSIXct(sub_eur$Date)
ad_eur$timestamp <- as.POSIXct(ad_eur$timestamp)

#ploting anamolys in returns
library(ggplot2)
ggplot() +
  geom_line(
    data=sub_eur, aes(Date, returns), 
    size=0.125, color="blue"
  )  +
  geom_point(
    data=ad_eur, aes(timestamp, anoms), color="red", alpha=1/3
  ) +
  scale_x_datetime(date_labels="%b") +
  scale_y_comma()


#storaging in "anom" variable the returns that are conteined into ad_eur$anoms
anom <- ifelse(sub_eur$returns %in% ad_eur$anoms,sub_eur$returns,0)

#calling close prices back to sub_eur dataset to craete trade system
sub_eur <- cbind(sub_eur,eurusd$close,anom)
names(sub_eur)[3] <- "close"

#lagging close prices to calculate target
sub_eur$close_lag <- Lag(sub_eur$close,k=1)
sub_eur <- sub_eur[2:length(sub_eur$close),]
sub_eur$close_lag <- as.integer(sub_eur$close_lag)

#calculating target
sub_eur$target <- sub_eur$close-sub_eur$close_lag

#calculating results
sub_eur$results <- ifelse(sub_eur$anom>0 & sub_eur$target>=0,sub_eur$target,NA)
sub_eur$results <- ifelse(sub_eur$anom>0 & sub_eur$target<=0,sub_eur$target,sub_eur$results)
sub_eur$results <- ifelse(sub_eur$anom<0 & sub_eur$target<0,sub_eur$target*(-1),sub_eur$results)
sub_eur$results <- ifelse(sub_eur$anom<0 & sub_eur$target>0,sub_eur$target*(-1),sub_eur$results)
sub_eur$results <- ifelse(sub_eur$anom<0 & sub_eur$target==0,0,sub_eur$results)
sub_eur$results <- ifelse(sub_eur$anom==0,0,sub_eur$results)

#creating a acumulated curve of results
sub_eur$acumu <- cumsum(sub_eur$results)

#ploting results
plot(sub_eur$acumu,type="l",
     col = "blue",
     ylab = "Acumulated points",
     xlab = "Time",
     panel.first = grid(9, lty = 1, lwd = 2),
     main="EURUSD H1 2003 - 2019")
