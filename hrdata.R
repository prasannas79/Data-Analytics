#HR case study
#Install packages
#install.packages("e0711")
install.packages("randomForest")
installed.packages("party")
install.packages("caret")
install.packages("e1071")
install.packages("dummies")
install.packages("heuristica")
install.packages("SDMTools")
library(dummies)
library(heuristica)
library(SDMTools)
library(caret)

#Get Working directory
getwd()

#Set Working Directory
setwd("C:/Users/Prasanna S/Documents/R")
#Load dataset
df<-read.csv("C:/Users/Prasanna S/Documents/R/hr.csv")
#To identify number of variables & observations
dim(df)
#Allows to identify no. of variables & observations & variable types
str(df)
#visualize top 6 values

head(df,6)

#visualize bottom 6 values

tail(hr,6)

#Get the names of the variables
names(df)
#descriptive stats of variables
summary(df)

#Variables in the dataframe
#1 Age
#2 Attrition
#3 BusinessTravel
#4 DailyRate
#5 Department
#6 DistanceFromHome
#7 Education
#8 EducationField
#9 EmployeeCount
#10 EmployeeNumber
#11 EnvironmentSatisfaction
#12 Gender
#13 HourlyRate
#14 JobInvolvement
#15 JobLevel
#16 JobRole
#17 JobSatisfaction
#18 MaritalStatus
#19 MonthlyIncome
#20 MonthlyRate
#21 NumCompaniesWorked
#22 Over18
#23 OverTime
#24 PercentSalaryHike
#25 PerformanceRating
#26 RelationshipSatisfaction
#27 StandardHours
#28 StockOptionLevel
#29 TotalWorkingYears
#30 TrainingTimesLastYear
#31 WorkLifeBalance
#32 YearsAtCompany
#33 YearsInCurrentRole
#34 YearsSinceLastPromotion
#35 YearsWithCurrManager

#To remove the variables that are not required for analysis
HR<-df [,-c(3,5,7,8,9,11,12,28,31)]

str(HR)
dim(HR)

#convert character attribute to numeric values
df$Gender <- as.numeric(df$Gender)
table(df$Gender)

#To convert below fields as numeric
#Department, education field, business travel, job role, marital sstatus, over18, overtime

df$Department <- as.numeric(df$Department)
table(df$Department)
df$EducationField <- as.numeric(df$EducationField)
table(df$EducationField)
df$EducationField<-as.numeric(df$EducationField)
table(df$EducationField)
df$BusinessTravel<-as.numeric(df$BusinessTravel)
table(df$BusinessTravel)
df$JobRole<-as.numeric(df$JobRole)
table(df$JobRole)
df$MaritalStatus<-as.numeric(df$MaritalStatus)
table(df$MaritalStatus)
df$Over18<-as.numeric(df$Over18)
table(df$Over18)
df$OverTime<-as.numeric(df$OverTime)
table(df$OverTime)

#structure of dataset after transformation
str(df)

#Creating dummy variables
#job satisfaction,#overtime,#marital status,#job role,#business travel
#education field, #department, #gender, #worklife balance, #training time, #stock options
#relationship satisfaction, #environment satisfaction, #job level
#job environment, #education
jsat<-dummy(df$JobSatisfaction)
ot<-dummy(df$OverTime)
ms<-dummy(df$MaritalStatus)
jrole<-dummy(df$JobRole)
bt<-dummy(df$BusinessTravel)
eduf<-dummy(df$EducationField)
dept<-dummy(df$Department)
gdr<-dummy(df$Gender)
wlb<-dummy(df$WorkLifeBalance)
tt<-dummy(df$TrainingTimesLastYear)
sop<-dummy(df$StockOptionLevel)
relsat<-dummy(df$RelationshipSatisfaction)
envsat<-dummy(df$EnvironmentSatisfaction)
jobl<-dummy(df$JobLevel)
jinv<-dummy(df$JobInvolvement)
edu<-dummy(df$Education)

#Create a new data frame with dummy variables included
HR <- data.frame(jsat,ot,ms,jrole,bt,eduf,dept,gdr,wlb,tt,sop,relsat,envsat,jobl,jinv,edu)
str(HR)


#Predictive analysis component: split data into 70 : 30 ratio Training and Testing respectively

train_test <- sample(2,nrow(HR),prob=c(0.80,0.20),replace=TRUE)
train_set <- HR[train_test==1,]
test <- HR[train_test==2,]

#Applying classifier Logistic regression

#Train Model
log_reg = glm(Attrition~.,data = train_set,family = 'binomial'("logit"))
summary(log_reg)
#Prediction - train
log_prediction <- predict(log_reg,test)
#score Model
log_prediction <- ifelse(log_prediction>0.5,1,0)
#Evaluate Model
confusionMatrix(test$Attrition, log_prediction)


