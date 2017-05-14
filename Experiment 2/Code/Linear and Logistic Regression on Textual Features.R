## Read training data
training_data<-read.csv("blogData_train.csv",header=T, na.strings=c(""))
## Scaling training data to reduce error
scaled_data<-data.frame(scale(training_data))	
## Creating subset of data with only the reqired columns
lm_trainData <- subset(scaled_data, select = c(63:262,281))

## Linear Regression Model
lmfitscaled<-lm(formula = X281~., data=lm_trainData)
mean(lmfitscaled$residuals^2)
#0.9734328
summary(lmfit)

## logistic Regression model
##normalizing the data so that it is in [0,1] range
lm_normalized = (lm_trainData-min(lm_trainData))/(max(lm_trainData)-min(lm_trainData))
lr <- glm(X281 ~.,family=binomial(link='logit'),data = lm_normalized )
mean(lr$residuals^2)
## 0.4132554
-------------------------------------------------------------------------------------------------------------------------------------------
## Read test data for February month and merge all the files
install.packages('data.table')
library('data.table')
files <- list.files(path = "C:\\Users\\Nikita\\Documents\\February",pattern = ".csv")
data_dir <- paste(getwd(),'February',sep = "/")
temp <- lapply(paste(data_dir,files,sep = "/"), read.csv,header=T)
data <- rbindlist( temp )
febdata<-data

## Creating subset of data with only the reqired columns
lm_febTestData <- subset(febdata, select = c(63:262,281))
names(lm_febTestData)
## Linear Regression Model
feb_lmfitscaled<-lm(formula = X281~., data=lm_febTestData)
mean(feb_lmfitscaled$residuals^2)
# 838.5721
## logistic Regression model
##normalizing the data so that it is in [0,1] range
feb_normalized = (lm_febTestData-min(lm_febTestData))/(max(lm_febTestData)-min(lm_febTestData))
feb_lr <- glm(X281 ~.,family=binomial(link='logit'),data = feb_normalized )
mean(feb_lr$residuals^2)
## 2490.748

## Read test data for February month and merge all the files
files_mar <- list.files(path = "C:\\Users\\Nikita\\Documents\\March",pattern = ".csv")
data_dir_mar <- paste(getwd(),'March',sep = "/")
temp_mar <- lapply(paste(data_dir_mar,files_mar,sep = "/"), read.csv)
data_mar<- rbindlist( temp_mar )
mardata<-data_mar
names(mardata)

## Creating subset of data with only the reqired columns
mar_subset <- subset(mardata, select = c(63:262,281))
## Linear Regression Model
mar_lmfitscaled<-lm(formula = X281~., data=mar_subset)
mean(mar_lmfitscaled$residuals^2)
#  710.5483

## logistic Regression model
##normalizing the data so that it is in [0,1] range
mar_normalized = (mar_subset-min(mar_subset))/(max(mar_subset)-min(mar_subset))
mar_lr <- glm(X281 ~.,family=binomial(link='logit'),data = mar_normalized)
mean(mar_lr$residuals^2)
##  2705663
