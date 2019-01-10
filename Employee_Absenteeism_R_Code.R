#setting path
setwd("E:/Absenteeism Project For Edwisor/Absenteism_R_Project")
#Reading the file as xls
library(readxl)
df <- read_excel("Absenteeism_at_work_Project.xls")
#Performing Recoding and analysis
library(tidyverse)
glimpse(df)
df = sapply(df,as.numeric)
df_data = as.data.frame(df)
str(df_data)
#Missing Value Analysis
#For Reason of Absence
ggplot(df_data,aes(x = factor(df_data[,2]))) + 
  geom_histogram(stat = "count",fill = "red") + 
  xlab("Reason For Absence")
#For Month Of Absence
ggplot(df_data,aes(x = factor(df_data[,3]))) + 
  geom_histogram(stat = "count",fill = "blue") + 
  xlab("Month of absence")
#For Day Of Week
ggplot(df_data,aes(x = factor(df_data[,4]))) + 
  geom_histogram(stat = "count",fill = "black") + 
  xlab("Day Of Week")
# For Seasons
ggplot(df_data,aes(x = factor(df_data[,5]))) + 
       geom_histogram(stat = "count",fill = "brown") + 
       xlab("Seasons")
#For Education
ggplot(df_data,aes(x = factor(Education))) + 
       geom_histogram(stat = "count",fill = "brown") + 
       xlab("Education")
#Replacing 0 with NA for Reason For Absence
index = which(df_data[,2] == 0)
df_data[,2] = replace(df_data[,2],index,NA)
#Replacing 0 with NA for Month Of Absence
index = which(df_data[,3] == 0)
df_data[,3] = replace(df_data[,3],index,NA)
#Counting Number of NA values
summary(df_data)
#Total Number Of NA values
sum(is.na(df_data))
#Missing Value Imputation
df_data_mock = data.frame(df_data)
df_data[10,2] = NA
#Mean Method
df_data$`Reason for absence`[is.na(df_data$`Reason for absence`)] = mean(df_data$`Reason for absence`,na.rm = TRUE)
df_data[10,2]
#Mean Value = 20.38
df_data = data.frame(df_data_mock)
df_data[10,2] = NA
#Median Method
df_data$`Reason.for.absence`[is.na(df_data$`Reason.for.absence`)] = median(df_data$`Reason.for.absence`,na.rm = TRUE)
df_data[10,2] 
#Median Value = 23
#Knn Imputation
df_data = data.frame(df_data_mock)

for (i in 1:21)
{
  
  index = which(is.na(df_data[,i]))
  df_data[index,i] = median(df[,i],na.rm = TRUE)
}
#Rounding the values
df_data = data.frame(round(df_data,0))
#Performning EDA and recommendations
#Making a table to find all reasons for absence
df_data_top_reasons = df_data %>% group_by(Reason.for.absence) %>% 
summarize(Count_of_Absentees = n()) %>% arrange(desc(Count_of_Absentees)) 
#Turning into data frame
df_data_top_reasons = data.frame(df_data_top_reasons)
#Plotting the three most frequent reasons for absence
df_data_top_reasons %>% top_n(3,wt = Count_of_Absentees) %>% 
ggplot(aes(x = factor(Reason.for.absence),y = Count_of_Absentees)) + 
geom_bar(stat = "identity") + xlab("Reason For Absence")
#Parameters of people who have taken the longest hours off from work
top_absentees = df_data %>% 
arrange(desc(Absenteeism.time.in.hours)) %>% 
top_n(10,wt = Absenteeism.time.in.hours)
#Comparing summary Statistics of top_absentees and df_data
summary(df_data)
summary(top_absentees)
#Monthly Losses
monthly_losses = df_data %>% group_by(Month.of.absence) %>% 
  summarize(Monthly_Loss = sum(Absenteeism.time.in.hours))
#Bar Plot
monthly_losses %>% ggplot(aes(x = factor(Month.of.absence),y = Monthly_Loss)) + 
geom_bar(stat = "identity",fill = "red") + 
xlab("Month Of Absence") + ylab("Monthly Loss")
#Model Development
#Outlier Analysis
#Recoding Variables
categorical_vars = c("Pet","Social.smoker","Social.drinker","Son",
                     "Education","Seasons","Day.of.the.week","Month.of.absence","Reason.for.absence","Disciplinary.failure")


#Boxplot of Absenteeism in Hours
df_data %>% ggplot(aes(x = "", y = Absenteeism.time.in.hours)) + geom_boxplot() +ggtitle("Absenteeism Time In Hours")
#Boxplot Of Age
ggplot(data = df_data,aes(x = "", y = Age)) + geom_boxplot() + ggtitle("Age")
#Boxplot of Height
ggplot(data = df_data,aes(x = "", y = Height)) + geom_boxplot() + ggtitle("Height")
#Boxplot Of Transportation Expense
ggplot(data = df_data,aes(x = "", y = Transportation.expense)) + geom_boxplot() + ggtitle("Transportation Expenses")
#Boxplot Of Workload Average
ggplot(data = df_data,aes(x = "", y = Work.load.Average.day)) + geom_boxplot() + ggtitle("Daily Workload Average")
#Boxplot of Distance From Residence to work
ggplot(data = numeric_data,aes(x = "", y = Distance.from.Residence.to.Work)) + geom_boxplot() + ggtitle("Distance From Residence To work")
# Boxplot of Service Time
ggplot(data = df_data,aes(x = "", y = Service.time)) + geom_boxplot() + ggtitle("Service.time")
#Boxplot Of Hit Target
ggplot(data = df_data,aes(x = "", y = Hit.target)) + geom_boxplot() + ggtitle("Hit.target")
#Feature Selection using corrgram
library(corrgram)
#Correlation Plot
numeric_vars = c("ID","Transportation.expense","Distance.from.Residence.to.Work",
                 "Service.time","Age","Work.load.Average.day","Hit.target",
                 "Weight","Height","Body.mass.index","Absenteeism.time.in.hours")
numeric_data = df_data[,numeric_vars]
corrgram(numeric_data,order = F,
         upper.panel = panel.pie,text.panel = panel.txt,main = "Correlation Plot")
#Variable Weight Has To Be Removed
#Performing Chi Square Tests
categorical_data = df_data[,categorical_vars]
for (i in 1:10)
{
  print(names(categorical_data[i]))
  print(chisq.test(df_data$Absenteeism.time.in.hours,categorical_data[,i]))
}
#Removing variables Weight,Social.smoker,Pet,ID and Education
df_data = subset(df_data,select = -c(Weight,Social.smoker,Pet,Education,ID))
#Removing Ouliers From The Data
#outlier_vars = c("Absenteeism.time.in.hours","Transportation.expense","Service.time","Age","Work.load.Average.day","Distance.from.Residence.to.Work","Service.time","Height","Hit.target","Reason.for.absence")
cnames = colnames(df_data)
#Removing Outliers
for (i in cnames)
{
    val = df_data[,i][df_data[,i] %in% boxplot.stats(df_data[,i])$out]
    df_data = df_data[which(!df_data[,i] %in% val),]
}
#Feature Importance using Caret package
library(randomForest)
library(caret)
#Using Random Forest Model To Implement Faeture Importance
fit_rf = randomForest(Absenteeism.time.in.hours~., data=df_data,importance = TRUE)
varImp(fit_rf)
varImpPlot(fit_rf)
#Feature Scaling
#numeric_index = sapply(df_data,is.numeric)
#numeric_data = df_data[,numeric_index]
cnames = colnames(numeric_data)
#Predictor Variable should not be normalised and "Weight" to be removed
cnames = cnames[-11]
cnames = cnames[-8]
cnames = cnames[-1]
for (i in cnames)
{
  print(i)
  df_data[,i] = (df_data[,i] - min(df_data[,i]))/(max(df_data[,i] - min(df_data[,i])))
}
#Developing a decision tree model. 
library(rpart)
set.seed(1234)
index = sample(1:nrow(df_data),0.8*nrow(df_data))
train_data = df_data[index,]
test_data = df_data[-index,]
decision_tree_fit = rpart(Absenteeism.time.in.hours ~.,data = train_data,method = "anova")
summary(decision_tree_fit)

#Studying the rules of the decision tree
library(rpart.plot)
rpart.plot(decision_tree_fit, box.palette="RdBu", shadow.col="gray")
#Predicting Value
predictions_decision_tree = predict(decision_tree_fit,newdata = test_data[,-16])
#Error Calculation of RMSE using custom function
rmse = function(Ypred,Y){
  return(sqrt(mean((Y - Ypred)^2)))
}
#Error Calculation of MAE using custom function
mae <- function(Ypred,Y)
{
     mean(abs(Y-Ypred))
}
#Function to calculate RSquared
rsq <- function (Ypred, Y) 
{
  cor(Ypred, Y) ^ 2
}
RMSE_decision_tree = rmse(test_data[,16],predictions_decision_tree)
MAE_decision_tree = mae(test_data[,16],predictions_decision_tree)
#Plotting the predicted values vs actual values on scatterplot
decision_tree_plot = data.frame(cbind(test_data[,16],predictions_decision_tree))
decision_tree_plot %>% ggplot(aes(x = decision_tree_plot[,2],y = decision_tree_plot[,1])) + geom_point() + xlab("Predictions") + ylab("Actual Values")
#Calculating R Squared of decision tree
Rsq_decision_tree = rsq(predictions_decision_tree,test_data[,16])
#Developing a Linear Regression Model
library(usdm)
vifcor(numeric_data[,-16],th = 0.9)

linear_regression_fit = lm(Absenteeism.time.in.hours ~.,data = train_data)
#applying model to the test data
linear_predictions = predict(linear_regression_fit,newdata = test_data[,-16])
#Calculating RMSE,R^2 and MAE
RMSE_linear_regression = rmse(linear_predictions,test_data[,16])
MAE_linear_regression = mae(test_data[,16],linear_predictions)
Rsq_linear_regression = rsq(linear_predictions,test_data[,16])
summary(linear_regression_fit)
#Plot Actual vs Predicted Values
linear_regression_plot = data.frame(cbind(test_data[,16],linear_predictions))
linear_regression_plot %>% ggplot(aes(x = linear_regression_plot[,2],y = linear_regression_plot[,1])) + geom_point()  + xlab("Predictions") + ylab("Actual Values")
#Preparing Random Forest Model
Random_Forest_fit = randomForest(Absenteeism.time.in.hours ~.,data = train_data,importance = TRUE,ntree = 500)
#Predicting Values For Random Forest Model
random_forest_predictions = predict(Random_Forest_fit,newdata = test_data[,-16])

#Calculating RMSE,MAE and R^2
RMSE_Random_forest = rmse(random_forest_predictions,test_data[,16])
MAE_random_forest = mae(random_forest_predictions,test_data[,16])
Rsq_random_forest = rsq(random_forest_predictions,test_data[,16])
random_forest_plot = data.frame(cbind(test_data[,16],random_forest_predictions))
random_forest_plot %>% ggplot(aes(x = random_forest_plot[,2],y = random_forest_plot[,1])) + geom_point()  + xlab("Predictions") + ylab("Actual Values")