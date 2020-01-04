library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(fastDummies)
library(regclass)

options(max.print=5000)

data_mobil123<-as.data.frame(read_excel("mobil123.xlsx"))
data_mobil123<-data_mobil123[-1]

data_mobil123<-na.omit(data_mobil123)
data_mobil123$harga<-data_mobil123$harga
data_mobil123$masa_pakai<-2020-data_mobil123$tahun
data_mobil123<-filter(data_mobil123, mileage < 690000)

summary(data_mobil123)

a<-ggplot(data=data_mobil123,aes(x=harga))+
  geom_histogram(bins=20)
plot(a)

a<-ggplot(data=data_mobil123,aes(x=mileage))+
  geom_histogram(bins=20)
plot(a)

#data_mobil123<-dummy_columns(data_mobil123, select_columns=c("transmisi"))

lm_datamobil123<-lm(harga~mileage+mesin+masa_pakai+model-1,data=data_mobil123)
summary(lm_datamobil123)
VIF(lm_datamobil123)

test_mileage<-lm(mileage~masa_pakai-1,data=data_mobil123)
summary(test_mileage)
