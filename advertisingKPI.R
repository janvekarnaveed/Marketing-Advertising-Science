
#Reading the given CSV file in RStudio
dataProf <- read.csv("data.csv", header=T, na.strings=c("","NA",".","Not Available","n/a","N/A")) ##Changed blank cells to NA

#Summary of the Dataset
summary(dataProf)

#Sructure of dataProf dataset to find out the datatype of variables
str(dataProf)

#Extracting date to convert date to date format
data1 <- dataProf["date"]
data2 <- dataProf[,2:22]

#Using as.Date to convert date which is factor to date format
data1$date<-as.Date(data1$date, format="%d-%b-%y") 

#Cleaning columns with thousand separators
cleandata2 <- function(c){as.numeric( gsub('[^a-zA-Z0-9.]', '', c))}

#Apply cleandata2 function on the data2 dataframe
data2[] <- sapply(data2, cleandata2) 

#column bind date and rest of the columns to get formatted dataset
dataProfBind <- cbind(data1,data2) # Combining date and other variables to make a formatted data frame

#Checking if the correct data types have been assigned to the columns
str(dataProfBind)

#Imputation of Missing Data
#Calculating how much data is missing from the dataset
permiss <- function(x){sum(is.na(x))/length(x)*100} 
missingper<-as.data.frame(apply(dataProfBind,2,permiss))

#Cutoff 60% for missing values, Google_nonbrand_e_Spend,Google_nonbrand_e_IMP have 86.82% of missing values


#Dropping Google_nonbrand_e_Spend,Google_nonbrand_e_IMP since it has very high missing value
dataProfBind_imp <- subset(dataProfBind, select = -c(Google_nonbrand_e_Spend, Google_nonbrand_e_IMP ) )

#A few KPI values are missing which can be used as a test dataset later
testProf <- subset(dataProfBind_imp,is.na(dataProfBind_imp$KPI)==TRUE)

#dataProfBind_imp2 has values after the elimination of KPI column missing values
dataProfBind_imp2 <- subset(dataProfBind_imp,is.na(dataProfBind_imp$KPI)==FALSE)

summary(dataProfBind_imp2)

install.packages("mice")
library(mice)


#Imputation can be done using MICE library in R, imputing leaving date column
miceImpute <- mice(dataProfBind_imp2[,-c(1)],m=1,maxit=50,meth='rf',seed=500)# imputation done using RF yields better F Statistics in the statistical model over the ppm method 
miceimp_model1 <- complete(miceImpute,1)
miceimp_model1<- as.data.frame(cbind(dataProfBind_imp2$date,miceimp_model1))
colnames(miceimp_model1)[1]<-"date"

#Summary to check if all the missing values have been imputed
summary(miceimp_model1)


#For regression modeling date variable is excluded
imp_model2 <- miceimp_model1[,-1]


library(usdm)
vif(imp_model2) 
vif_var<-vifstep(imp_model2,th=10) # identify collinear variables that should be excluded
imp_model3 <- exclude(imp_model2, vif_var) ##Excluding the multi collinearity problem variables 
View(imp_model3)


#After removing multicollinearity selecting the best variables using Stepwise Regression


fullmod = lm(KPI ~ ., data= imp_model3)
summary(fullmod)


nothing = lm(KPI ~ 1, data= imp_model3)
summary(nothing)


backwards = step(fullmod) # Backwards selection is the default

summary(backwards)

formula(backwards)

#Forward Selection

forwards = step(nothing, scope=list(lower=formula(nothing),upper=formula(fullmod)), data=imp_model3, direction="forward")


bothways = step(nothing, list(lower=formula(nothing),upper=formula(fullmod)), data=dataSample1, direction="both",trace=0)

formula(forwards)
formula(bothways)

#Not considering the insignificant variables from the result of summary(backwards)
back2 = lm(KPI ~ GDN_IMP+FB_Spend+FB_IMP+Google_nonbrand_IMP+Google_brand_spend+Google_brand_imp+YT_IMP+radio_spend+radio_imp+tv60_imp+tv30_imp+tv15_imp , data=imp_model3)
summary(back2)

back3 = lm(KPI ~ GDN_IMP+FB_Spend+FB_IMP+Google_nonbrand_IMP+Google_brand_spend+Google_brand_imp+YT_IMP+radio_spend+radio_imp+tv60_imp+tv30_imp,data=imp_model3)
summary(back3)

library(car)



#FInding the outliers

plot(back3, which = 1:4)


# Auto correlation

acf(back3$residuals)
durbinWatsonTest(back3)

library(DataCombine)
econ_data <- data.frame(imp_model3, resid_mod1=back3$residuals)
econ_data_1 <- slide(econ_data, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
econ_data_2 <- na.omit(econ_data_1)
lmMod2 <- lm(KPI ~ GDN_IMP+FB_Spend+FB_IMP+Google_nonbrand_IMP+Google_brand_spend+Google_brand_imp+YT_IMP+radio_spend+radio_imp+tv60_imp+tv30_imp+ lag1, data=econ_data_2)

summary(lmMod2)

acf(lmMod2$residuals)
durbinWatsonTest(lmMod2)


#Checking for Homoskedasticity
install.packages("sandwich")
library(sandwich)
library(lmtest)

#Now to check whether our data has heteroskedasticity or not, we will construct a variance covariance matrix
vcovHC(lmMod2, omega = NULL, type = "HC4")

mean(lmMod2$residuals)
plot(lmMod2)

#Finally to remove this heteroskedasticity, we use the coeftest() function in R
coeftest(lmMod2, df = Inf, vcovHC(lmMod2, omega = NULL, type = "HC4"))
waldtest(lmMod2, vcov = vcovHC)
mean(lmMod2$residuals)

#Cost efficiency of each media channek
library(sqldf)

#Cost_Efficiency=[SUM(Media channel Impressions)/SUM(Media channel Spend)] aggregated over the given 3 years in the dataset
ce1<-sqldf("SELECT SUM(GDN_Spend),SUM(GDN_IMP),(SUM(GDN_IMP)/SUM(GDN_Spend)) as GDN_CE,
             SUM(FB_Spend),SUM(FB_IMP),(SUM(FB_IMP)/SUM(FB_Spend)) as FB_CE,
             SUM(Google_nonbrand_Spend),SUM(Google_nonbrand_IMP),(SUM(Google_nonbrand_IMP)/SUM(Google_nonbrand_Spend)) as GNB_CE,
             SUM(Google_brand_spend),SUM(Google_brand_imp),(SUM(Google_brand_imp)/SUM(Google_brand_spend)) as GB_CE,
             SUM(YT_Spend),SUM(YT_IMP),(SUM(YT_IMP)/SUM(YT_Spend)) as YT_CE, 
             SUM(radio_spend),SUM(radio_imp),(SUM(radio_imp)/SUM(radio_spend)) as radio_CE,
             SUM(tv60_spend),SUM(tv60_imp),(SUM(tv60_imp)/SUM(tv60_spend)) as tv60_CE,
             SUM(tv30_spend),SUM(tv30_imp),(SUM(tv30_imp)/SUM(tv30_spend)) as tv30_CE,
             SUM(tv15_spend),SUM(tv15_imp),(SUM(tv15_imp)/SUM(tv15_spend)) as tv15_CE 
             FROM laglinear_model1")


ce1_new <- ce1[,c("GDN_CE","FB_CE","GNB_CE","GB_CE","YT_CE","radio_CE","tv60_CE","tv30_CE","tv15_CE")]

library(reshape2)
library(ggplot2)
CETransp <- melt(ce1_new)

##Order in descending order of cost efficiency 
CETransp<- CETransp[order(-CETransp$value),]

#Plotting cost efficiency for each  media channel

ggplot(data=CETransp, aes(x=variable, y=value)) +
  geom_bar(stat="identity") +ggtitle("Cost Effiency of Media Channels")  +labs(x="Media Channel",y="Cost Efficiency" )
