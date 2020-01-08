library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(fastDummies)
library(regclass)

library(mlbench)
library(caret)
library(leaps)
library(MASS)
library(doParallel)

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

lm_datamobil123<-lm(harga~mileage+mesin+masa_pakai+model+merk+warna,data=data_mobil123)
summary(lm_datamobil123)
VIF(lm_datamobil123)

#pake caret
set.seed(3456)
trainIndex <- createDataPartition(data_mobil123$harga, p = .8, 
                                  list = FALSE, 
                                  times = 1)
control <- trainControl(method="cv", number=10)
data_mobil123_train<-data_mobil123[trainIndex,]
data_mobil123_test<-data_mobil123[-trainIndex,]
model <- train(harga~mileage+mesin+masa_pakai+model, 
               data=data_mobil123_train, method="lm", trControl=control,preProc="range")
model

varImp(model)

#Feature Selection with MASS
step.model<-stepAIC(lm_datamobil123,direction="both",trace=FALSE)
summary(step.model)

#print(results)
# list the chosen features
#predictors(results)

test_mileage<-lm(mileage~masa_pakai-1,data=data_mobil123)
summary(test_mileage)
