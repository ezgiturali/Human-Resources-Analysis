library(dplyr)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(GGally)
library(ROCR)
library(tidyr)
#importing data
employee_survey_data<-read.csv("...\\employee_survey.csv",stringsAsFactors = F)
manager_survey_data<-read.csv("...\\manager_survey.csv",stringsAsFactors = F)
general_data<-read.csv("...\\general.csv",stringsAsFactors = F)
in_time <- read.csv("...\\login_time.csv",stringsAsFactors = F)
out_time <- read.csv("...\\logout_time.csv",stringsAsFactors = F)

str(employee_survey_data)    
str(manager_survey_data)    
str(general_data)    
str(in_time)    
str(out_time)    
#all have 4410 variables

#checking for unique Ids 
length(unique(tolower(employee_survey_data$EmployeeID)))  
length(unique(tolower(manager_survey_data$EmployeeID))) 
length(unique(tolower(general_data$EmployeeID)))
length(unique(tolower(in_time$X))) 
length(unique(tolower(out_time$X))) 


 
#converting accurate time format
in_time <- sapply(in_time, function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S"))
in_time<-as.data.frame(in_time)

out_time <- sapply(out_time, function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S"))
out_time<-as.data.frame(out_time)

#removing first column for both data frames as they are not useful
in_time$X<-NULL
out_time$X<-NULL

#calculating the difference
diff<-out_time-in_time

#removing columns with all NA values
diff <- diff[,colSums(is.na(diff))<nrow(diff)]

#converting all values to numeric
diff<-sapply(diff,function(x) as.numeric(x))
diff<-as.data.frame(diff)
#checking diff data frame
class(diff)

#new vector for storing employee id to join with average work hours
EmployeeID<-seq(from = 1, to = 4410, by = 1)

#calculating the average working hours
diff$AvgWorkHrs<-apply(diff,1,mean,na.rm=TRUE)

#creating vverage work hours per employeee data frame
AvgWorkHrs<-cbind(EmployeeID,diff$AvgWorkHrs)
AvgWorkHrs<-as.data.frame(AvgWorkHrs)

#using setdiff to check wether all data is for same employees only
setdiff(employee_survey_data$EmployeeID,manager_survey_data$EmployeeID) 
setdiff(manager_survey_data$EmployeeID,general_data$EmployeeID) 
setdiff(general_data$EmployeeID,AvgWorkHrs$EmployeeID) 
#all data is for same employees only since it is 0

#merging all the data frames 
attrition_data<-merge(employee_survey_data,manager_survey_data,by="EmployeeID",all=F)
attrition_data<-merge(attrition_data,general_data,by="EmployeeID",all=F)
attrition_data<-merge(attrition_data,AvgWorkHrs,by="EmployeeID",all = F)
colnames(attrition_data)[30] <- "AvgWorkhrs"
View(attrition_data)

#############data cleaning
str(attrition_data)

#removing 3 variables from data frame which have the same value for all rows
attrition_data$EmployeeCount<-NULL
attrition_data$Over18<-NULL
attrition_data$StandardHours<-NULL

#correcting format as given in data dictionary  
attrition_data$Education[which(attrition_data$Education==1)]<-'Below College'
attrition_data$Education[which(attrition_data$Education==2)]<-'College'
attrition_data$Education[which(attrition_data$Education==3)]<-'Bachelor'
attrition_data$Education[which(attrition_data$Education==4)]<-'Master'
attrition_data$Education[which(attrition_data$Education==5)]<-'Doctor'
attrition_data$EnvironmentSatisfaction[which(attrition_data$EnvironmentSatisfaction==1)]<-'Low'
attrition_data$EnvironmentSatisfaction[which(attrition_data$EnvironmentSatisfaction==2)]<-'Medium'
attrition_data$EnvironmentSatisfaction[which(attrition_data$EnvironmentSatisfaction==3)]<-'High'
attrition_data$EnvironmentSatisfaction[which(attrition_data$EnvironmentSatisfaction==4)]<-'Very High'
attrition_data$JobInvolvement[which(attrition_data$JobInvolvement==1)]<-'Low'
attrition_data$JobInvolvement[which(attrition_data$JobInvolvement==2)]<-'Medium'
attrition_data$JobInvolvement[which(attrition_data$JobInvolvement==3)]<-'High'
attrition_data$JobInvolvement[which(attrition_data$JobInvolvement==4)]<-'Very High'
attrition_data$JobSatisfaction[which(attrition_data$JobSatisfaction==1)]<-'Low'
attrition_data$JobSatisfaction[which(attrition_data$JobSatisfaction==2)]<-'Medium'
attrition_data$JobSatisfaction[which(attrition_data$JobSatisfaction==3)]<-'High'
attrition_data$JobSatisfaction[which(attrition_data$JobSatisfaction==4)]<-'Very High'
attrition_data$WorkLifeBalance[which(attrition_data$WorkLifeBalance==1)]<-'Bad'
attrition_data$WorkLifeBalance[which(attrition_data$WorkLifeBalance==2)]<-'Good'
attrition_data$WorkLifeBalance[which(attrition_data$WorkLifeBalance==3)]<-'Better'
attrition_data$WorkLifeBalance[which(attrition_data$WorkLifeBalance==4)]<-'Best'
attrition_data$PerformanceRating[which(attrition_data$PerformanceRating==1)]<-'Low'
attrition_data$PerformanceRating[which(attrition_data$PerformanceRating==2)]<-'Good'
attrition_data$PerformanceRating[which(attrition_data$PerformanceRating==3)]<-'Excellent'
attrition_data$PerformanceRating[which(attrition_data$PerformanceRating==4)]<-'Outstanding'

attrition <- attrition_data

#Missing value
sapply(attrition, function(x) sum(is.na(x)))
#there are 63 NAs

#removing NAs as they dont have a significant presence in the data set, 63/2484=2% NAs 
attrition <- attrition[!is.na(attrition$EnvironmentSatisfaction),]
attrition <- attrition[!is.na(attrition$JobSatisfaction),]
attrition <- attrition[!is.na(attrition$WorkLifeBalance),]
attrition <- attrition[!is.na(attrition$NumCompaniesWorked),]
attrition <- attrition[!is.na(attrition$TotalWorkingYears),]
#check for NAs again
sapply(attrition, function(x) sum(is.na(x))) 
#removed NA from the data set, now we have no NAs 

##############################################
#checking categorical variables using bar charts

plot_grid(ggplot(attrition, aes(x=factor(Attrition) ,  group=EnvironmentSatisfaction)) + 
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", position = "stack") +
            geom_text(aes( label = scales::percent(..prop..),
                           y= ..prop.. ), stat= "count", vjust = -.5) +
            labs(y = "Percent", x="EnvironmentSatisfaction" , fill="Attrition") +
            facet_grid(~EnvironmentSatisfaction) +
            scale_y_continuous(labels = scales::percent) ,
          ggplot(attrition, aes(x=factor(Attrition) ,  group=JobSatisfaction)) + 
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", position = "stack") +
            geom_text(aes( label = scales::percent(..prop..),
                           y= ..prop.. ), stat= "count", vjust = -.5) +
            labs(y = "Percent", x= "JobSatisfaction" ,fill="Attrition") +
            facet_grid(~JobSatisfaction) +
            scale_y_continuous(labels = scales::percent),
          ggplot(attrition, aes(x=factor(Attrition) ,  group=WorkLifeBalance)) + 
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", position = "stack") +
            geom_text(aes( label = scales::percent(..prop..),
                           y= ..prop.. ), stat= "count", vjust = -.5) +
            labs(y = "Percent", x= "WorkLifeBalance" ,fill="Attrition") +
            facet_grid(~WorkLifeBalance) +
            scale_y_continuous(labels = scales::percent),
          ggplot(attrition, aes(x=factor(Attrition) ,  group=JobInvolvement)) + 
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", position = "stack") +
            geom_text(aes( label = scales::percent(..prop..),
                           y= ..prop.. ), stat= "count", vjust = -.5) +
            labs(y = "Percent", x= "JobInvolvement" ,fill="Attrition") +
            facet_grid(~JobInvolvement) +
            scale_y_continuous(labels = scales::percent)
)

#if job involvement is low there is a attrition.
#if workbalancelife is bad there is a attrition.
#low env. satisfaction causes attrition
#also low job satisfaction causes attrition according to the graphs.

plot_grid(ggplot(attrition, aes(x=factor(Attrition) ,  group=PerformanceRating)) + 
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", position = "stack") +
            geom_text(aes( label = scales::percent(..prop..),
                           y= ..prop.. ), stat= "count", vjust = -.5) +
            labs(y = "Percent", x="PerformanceRating" , fill="Attrition") +
            facet_grid(~PerformanceRating) +
            scale_y_continuous(labels = scales::percent) ,
          ggplot(attrition, aes(x=factor(Attrition) ,  group=BusinessTravel)) + 
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", position = "stack") +
            geom_text(aes( label = scales::percent(..prop..),
                           y= ..prop.. ), stat= "count", vjust = -.5) +
            labs(y = "Percent", x= "BusinessTravel" ,fill="Attrition") +
            facet_grid(~BusinessTravel) +
            scale_y_continuous(labels = scales::percent),
          ggplot(attrition, aes(x=factor(Attrition) ,  group=Department)) + 
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", position = "stack") +
            geom_text(aes( label = scales::percent(..prop..),
                           y= ..prop.. ), stat= "count", vjust = -.5) +
            labs(y = "Percent", x= "Department" ,fill="Attrition") +
            facet_grid(~Department) +
            scale_y_continuous(labels = scales::percent),
          ggplot(attrition, aes(x=factor(Attrition) ,  group=Education)) + 
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", position = "stack") +
            geom_text(aes( label = scales::percent(..prop..),
                           y= ..prop.. ), stat= "count", vjust = -.5) +
            labs(y = "Percent", x= "Education" ,fill="Attrition") +
            facet_grid(~Education) +
            scale_y_continuous(labels = scales::percent)
)
#in performance rating, attrition is high on outstanding column.
#in business travel, attrition is high on travel frequently column.
#in department, there is a problem in hr.
#in education, college column has the higher attrition.

plot_grid(ggplot(attrition, aes(x=factor(Attrition) ,  group=EducationField)) + 
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", position = "stack") +
            geom_text(aes( label = scales::percent(..prop..),
                           y= ..prop.. ), stat= "count", vjust = -.5) +
            labs(y = "Percent", x="EducationField" , fill="Attrition") +
            facet_grid(~EducationField) +
            scale_y_continuous(labels = scales::percent) ,
          ggplot(attrition, aes(x=factor(Attrition) ,  group=Gender)) + 
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", position = "stack") +
            geom_text(aes( label = scales::percent(..prop..),
                           y= ..prop.. ), stat= "count", vjust = -.5) +
            labs(y = "Percent", x= "Gender" ,fill="Attrition") +
            facet_grid(~Gender) +
            scale_y_continuous(labels = scales::percent),
          ggplot(attrition, aes(x=factor(Attrition) ,  group=JobRole)) + 
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", position = "stack") +
            geom_text(aes( label = scales::percent(..prop..),
                           y= ..prop.. ), stat= "count", vjust = -.5) +
            labs(y = "Percent", x= "JobRole" ,fill="Attrition") +
            facet_grid(~JobRole) +
            scale_y_continuous(labels = scales::percent),
          ggplot(attrition, aes(x=factor(Attrition) ,  group=MaritalStatus)) + 
            geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count", position = "stack") +
            geom_text(aes( label = scales::percent(..prop..),
                           y= ..prop.. ), stat= "count", vjust = -.5) +
            labs(y = "Percent", x= "MaritalStatus" ,fill="Attrition") +
            facet_grid(~MaritalStatus) +
            scale_y_continuous(labels = scales::percent)
)

#in education field, attrition is high on human resources column.
#in gender, male has the highest attirtion.
#in job role, research direct. has the higest attrition.
#in marital, single column has the highest attrition. 

##for age
ggplot(attrition,aes(Age,fill=Attrition))+geom_density()+facet_grid(~Attrition)
#we can see that less age leave compared to old people
#high age people are less likely to leave

#years with current manager
ggplot(attrition,aes(YearsWithCurrManager,fill=Attrition))+geom_density()+facet_grid(~Attrition)
#people are tend to leave

#since last promotion
ggplot(attrition,aes(YearsSinceLastPromotion,fill=Attrition))+geom_density()+facet_grid(~Attrition)
#who have recently been promoted are most likely to leave 

#average working hours 
ggplot(attrition,aes(AvgWorkhrs,fill=Attrition))+geom_density()+facet_grid(~Attrition)
#Employees which working less than 8 hours are more likely to stay in the company



#Histogram and Boxplots for numeric variables 
tema2<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

tema3<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(attrition, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(attrition, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+tema2, 
          align = "v",ncol = 1)
#age has no outliers

plot_grid(ggplot(attrition, aes(AvgWorkhrs))+ geom_histogram(binwidth = 10),
          ggplot(attrition, aes(x="",y=AvgWorkhrs))+ geom_boxplot(width=0.1)+coord_flip()+tema2, 
          align = "v",ncol = 1)
#avg. work hours has outliers.

plot_grid(ggplot(attrition, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(attrition, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+tema2, 
          align = "v",ncol = 1)
#distance from home has no outliers.

plot_grid(ggplot(attrition, aes(MonthlyIncome))+ geom_histogram(binwidth = 10),
          ggplot(attrition, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+tema2, 
          align = "v",ncol = 1)
#monthly income has too many outliers.

plot_grid(ggplot(attrition, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),
          ggplot(attrition, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+tema2, 
          align = "v",ncol = 1)
#percent salary hike has no outliers

plot_grid(ggplot(attrition, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
          ggplot(attrition, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+tema2, 
          align = "v",ncol = 1)
#years at company has outliers.

plot_grid(ggplot(attrition, aes(JobLevel))+ geom_histogram(binwidth = 10),
          ggplot(attrition, aes(x="",y=JobLevel))+ geom_boxplot(width=0.1)+coord_flip()+tema2, 
          align = "v",ncol = 1)
#job level has no outliers

plot_grid(ggplot(attrition, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 10),
          ggplot(attrition, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+tema2, 
          align = "v",ncol = 1)
#number of companies has one outlier

plot_grid(ggplot(attrition, aes(StockOptionLevel))+ geom_histogram(binwidth = 10),
          ggplot(attrition, aes(x="",y=StockOptionLevel))+ geom_boxplot(width=0.1)+coord_flip()+tema2, 
          align = "v",ncol = 1)
#stock option level has one outlier

plot_grid(ggplot(attrition, aes(TotalWorkingYears))+ geom_histogram(binwidth = 10),
          ggplot(attrition, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+tema2, 
          align = "v",ncol = 1)
#total working years have outliers

plot_grid(ggplot(attrition, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 10),
          ggplot(attrition, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+tema2, 
          align = "v",ncol = 1)
#trainingitmeslastyear has three outliers

plot_grid(ggplot(attrition, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 10),
          ggplot(attrition, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+tema2, 
          align = "v",ncol = 1)
#years since last promotion has outliers

plot_grid(ggplot(attrition, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 10),
          ggplot(attrition, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+tema2, 
          align = "v",ncol = 1)
#years with current manager has outliers.


#removing outliers for few variables from the data set as seen from the boxplots
box <- boxplot.stats(attrition_data$YearsAtCompany)
out <- box$out
ad1 <- attrition_data[ !attrition_data$YearsAtCompany %in% out, ]
attrition <- ad1
box <- boxplot.stats(attrition$AvgWorkhrs)
out <- box$out
ad1 <- attrition[ !attrition$AvgWorkhrs %in% out, ]
attrition <- ad1
box <- boxplot.stats(attrition$NumCompaniesWorked)
out <- box$out
ad1 <- attrition[ !attrition$NumCompaniesWorked %in% out, ]
attrition <- ad1
box <- boxplot.stats(attrition$StockOptionLevel)
out <- box$out
ad1 <- attrition[ !attrition$StockOptionLevel %in% out, ]
attrition <- ad1
box <- boxplot.stats(attrition$TotalWorkingYears)
out <- box$out
ad1 <- attrition[ !attrition$TotalWorkingYears %in% out, ]
attrition <- ad1
box <- boxplot.stats(attrition$TrainingTimesLastYear)
out <- box$out
ad1 <- attrition[ !attrition$TrainingTimesLastYear %in% out, ]
attrition <- ad1
box <- boxplot.stats(attrition$YearsSinceLastPromotion)
out <- box$out
ad1 <- attrition[ !attrition$YearsSinceLastPromotion %in% out, ]
attrition <- ad1
box <- boxplot.stats(attrition$YearsWithCurrManager)
out <- box$out
ad1 <- attrition[ !attrition$YearsWithCurrManager %in% out, ]
attrition <- ad1

####numerical
str(attrition)
shapiro.test(attrition$MonthlyIncome)
wilcox.test(MonthlyIncome ~ Attrition, attrition)#statistically insignificant

shapiro.test(attrition$Age)#the age distribution is not normal. Therefore we conduct the wilcox test
wilcox.test(Age ~ Attrition, attrition) #As p value is lower than 0,05 than the difference of ages between attritions is statistically significant

shapiro.test(attrition$DistanceFromHome)
wilcox.test(DistanceFromHome ~ Attrition, attrition) #we can conclude that there is not significant difference between attritions in terms of DistanceFromHome

shapiro.test(attrition$NumCompaniesWorked)
wilcox.test(NumCompaniesWorked ~ Attrition, attrition)#statistically insignificant

shapiro.test(attrition$PercentSalaryHike)
wilcox.test(PercentSalaryHike ~ Attrition, attrition)#statistically insignificant

shapiro.test(attrition$TotalWorkingYears)
wilcox.test(TotalWorkingYears ~ Attrition, attrition)#significant

shapiro.test(attrition$TrainingTimesLastYear)
wilcox.test(TrainingTimesLastYear ~ Attrition, attrition)#statistically insignificant

shapiro.test(attrition$YearsAtCompany)
wilcox.test(YearsAtCompany ~ Attrition, attrition)#significant

shapiro.test(attrition$YearsSinceLastPromotion)
wilcox.test(YearsSinceLastPromotion ~ Attrition, attrition)#significant

shapiro.test(attrition$YearsWithCurrManager)
wilcox.test(YearsWithCurrManager ~ Attrition, attrition)#significant

shapiro.test(attrition$AvgWorkhrs)
wilcox.test(AvgWorkhrs ~ Attrition, attrition)#significant
#####categorical
attrition$EnvironmentSatisfaction <- as.factor(attrition$EnvironmentSatisfaction)
chisq.test(attrition$EnvironmentSatisfaction, attrition$Attrition)#significant

attrition$JobSatisfaction <- as.factor(attrition$JobSatisfaction)
chisq.test(attrition$JobSatisfaction, attrition$Attrition)#significant

attrition$WorkLifeBalance <- as.factor(attrition$WorkLifeBalance)
chisq.test(attrition$WorkLifeBalance, attrition$Attrition)#significant

attrition$JobInvolvement <- as.factor(attrition$JobInvolvement)
chisq.test(attrition$JobInvolvement, attrition$Attrition)#insignificant

attrition$PerformanceRating <-as.factor(attrition$PerformanceRating)
chisq.test(attrition$PerformanceRating, attrition$Attrition)#insignificant

attrition$BusinessTravel <-as.factor(attrition$BusinessTravel)
chisq.test(attrition$BusinessTravel, attrition$Attrition) #there is a significant relationship between two variables

attrition$Department <-as.factor(attrition$Department)
chisq.test(attrition$Department, attrition$Attrition) #there is a significant relationship between department and Attrition

attrition$Education <-as.factor(attrition$Education)
chisq.test(attrition$Education, attrition$Attrition) #Since p<0.05 there is a a significant relationshio between Attrition and education

attrition$EducationField <-as.factor(attrition$EducationField)
chisq.test(attrition$EducationField, attrition$Attrition) #significant


attrition$Gender <-as.factor(attrition$Gender)
chisq.test(attrition$Gender, attrition$Attrition) #Insignifcant

attrition$JobLevel <-as.factor(attrition$JobLevel)
chisq.test(attrition$JobLevel, attrition$Attrition) #Insignificant

attrition$JobRole <-as.factor(attrition$JobRole)
chisq.test(attrition$JobRole, attrition$Attrition) #significant

attrition$MaritalStatus <-as.factor(attrition$MaritalStatus)
chisq.test(attrition$MaritalStatus, attrition$Attrition) #significant




attrition_final <- attrition[ , -1]



#Logistic Regression: 
#Lower indicates a more parsimonious model, relative to a model fit
#with a higher AIC


#Initial model
attrition$Attrition <-as.factor(attrition$Attrition)
model1 = glm(Attrition ~ MaritalStatus+JobRole+EducationField+Education+
               Department+BusinessTravel+BusinessTravel+WorkLifeBalance+
               JobSatisfaction+EnvironmentSatisfaction+AvgWorkhrs+
               YearsWithCurrManager+YearsAtCompany+TotalWorkingYears+
               Age, attrition_final, family = "binomial")


summary(model1)


#stepAIC chooses the best model from the class according to the Akaike information criterion (AIC)
model2<- stepAIC(model1, direction="both")

summary(model2)
sort(vif(model2), decreasing  = T)

# Removing multicollinearity through VIF check
library(car)
vif(model2)
#all have low vif and model 2 has the smallest AIC, so that is our model.

