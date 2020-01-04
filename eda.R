library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)

data_mobil123<-as.data.frame(read_excel("mobil123.xlsx"))
data_mobil123<-data_mobil123[-1]

data_mobil123<-na.omit(data_mobil123)
data_mobil123$harga<-data_mobil123$harga/1000000
data_mobil123$masa_pakai<-2020-data_mobil123$tahun

xyz<-data_mobil123$mileage[data_mobil123$mileage<1000000]

summary(data_mobil123)

a<-ggplot(data=data_mobil123,aes(x=harga))+
  geom_histogram(bins=20)
plot(a)

a<-ggplot(data=data_mobil123,aes(x=mileage))+
  geom_histogram(bins=20)
plot(a)
