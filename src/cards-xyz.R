#---------------------------------------------------------
#|               BFS Capstone Project                    |
#| Group number : 21                                     |
#| Batch        : PGDDA - March 2017                     |
#---------------------------------------------------------

#Set the working directory
setwd("D:/Abhineet/Study/IIIT-B/7. Capstone Project/1. Capstone-BFS")

#install.packages("knitr")

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(GGally)
library(Information)
library(outliers)
library(Hmisc)
library(dplyr)
library(magrittr)
library(knitr)
library(data.table)
library(ROCR)
library(pROC) 
library(caret)

#-------------------------------------------------------
#Synopsis:
#-------------------------------------------------------
#  Credit card companies (CredX in this case) primarily earn their money in three ways. 
#  They charge merchants around 2% to 3% of every transaction made using their credit card. 
#  They charge customers interest on unpaid balance carried from month to month. 
#  And they charge a variety of fees, including annual and late fees. For these reasons, credit
#  card companies earn more money the more customers they have, and are always looking for more
#  people to use their services.

#------------------------------------------------------------------------
#Business Objective: Delinquent vs default customers  [problem statement]
#------------------------------------------------------------------------
#  As We are that credit X it has experienced an increase in credit loss. The CEO believes that
#  the best strategy to mitigate credit risk is to 'acquire the right customers'.
#  Help CredX identify right applicants to provide credit cards to by using predictive models i.e. 
#  by determining the factors affecting credit risk, create strategies to mitigate the acquisition 
#  risk and assess the financial benefit increasing the profitability of credit cards.

#------------------------------------------------------
#Solution
#------------------------------------------------------
# TO solve this business problem statement of acquisitionanalytics we are applying CRISP-DM framework  which 
# includes the following steps:
# 1. Business Understanding
# 2. Data Understanding
     #2.1 check for null values, sanity check, duplicate records
     #2.2 Univariate analysis for categorical variables 
          #2.2.1 Histogram/Bar chart to understand the distribution
          #2.2.2 Box plot to identify the outliers
     #2.3 univarivariate and bivariate analysis for continous variables
          #2.3.1 Histogram to understand the distribution
          #2.3.2 Box plot to identify the outliers
# 3. Data Preparation
     #3.1 Remove or mutate missing values based on the business justification
     #3.2 outlier treatment
     #3.3 data imputation for missing values with WOE
# 4. Feature selection
     #4.1 Chi-square test for feature selection for categorical variables
     #4.2 IV test for feature selection for continuous  variables
# 4. Data Modeling - Prepare the below  different models
     #4.1 logistic regression - starts with it
     #4.2 Decision tree/random forest
     #4.3 SVM if possible 
# 5. Model Evaluation - select the best model based on the below criteria
      #Accuracy,Sensitivity, Specificity of the model
      #KS statistics
      #Application score card based on the probability
      #vintage curve
# 6. Model Deployment

#----------------------------------------------------
# 1. Business understanding
#----------------------------------------------------
#  With the large potential profit in credit card banking also comes the risk of customers not paying
#  off their credit card balance. As CredX seeks to expand, it is important that they exercise good risk
#  control- because they will try to expand their customer base and in the run acquire certain risky customers .
#  a) There may be certain customers who are habitual defaulters from DPD - but eventually pay - 
#     These are 'medium risk- high revenue' customers
#  b) There may be certain customers who are regular DPD payers  - they are' low risk - low revenue' 
#  c) There may be certain customers who are total defaulters and do not pay - These are 'high risk- no revenue' and high credit loss customers.
#  Customers of category medium risk need are the right of customers to be acquired , Low risk are good to have
#  to increase the customer base but they are not high revenue customers ( as revenue stream from them is 
#  limited to only transactional costs) ,Medium risk customers are generally higher revenue customers - revenue
#  from whom is transactional cost+ late fee/interest earned  - while  completely high risk customers need to
#  be avoided as they major contributors to credit loss.

#----------------------------------------------------
#  2. Data Understanding - EDA
#----------------------------------------------------
# Data Sourcing - Merge the data set
#----------------------------------------------------
demographic_data <- read.csv("Demographic data.csv", stringsAsFactors = FALSE)
credit_bureau_data <- read.csv("Credit Bureau data.csv", stringsAsFactors = FALSE)


# Check duplicate records with respect to entire row data
sum(duplicated(demographic_data)) 
sum(duplicated(credit_bureau_data)) 
# No duplicate Records.

# Check duplicate records with respect to application id
# Check duplicate records with respect to application id in demographic_data
demographic_data[duplicated(demographic_data$Application.ID),]$Application.ID 
# Application Ids viz.  765011468, 653287861, 671989187 are duplicate
demographic_data <- demographic_data[which(!duplicated(demographic_data$Application.ID)), ]

# Check duplicate records with respect to application id in credit_bureau_data
credit_bureau_data[duplicated(credit_bureau_data$Application.ID),]$Application.ID 
# Application Ids viz.  765011468, 653287861, 671989187 are duplicate
credit_bureau_data <- credit_bureau_data[which(!duplicated(credit_bureau_data$Application.ID)), ]

#Check for missing Values
colSums(is.na(demographic_data))
colSums(is.na(credit_bureau_data))
# There are null records in two columns Performance.Tag - 1425 records and No.of.dependents - 3
# As we have to look the data where we have Performance.Tag means for customers have approval result
# we can ignore records having NAs for Performance.Tag.
demographic_data <- demographic_data[!is.na(demographic_data$Performance.Tag),]
unique(demographic_data$Performance.Tag)
credit_bureau_data <- credit_bureau_data[!is.na(credit_bureau_data$Performance.Tag),]
unique(credit_bureau_data$Performance.Tag)

# Lets see values in No.of.dependents
#unique(demographic_data$No.of.dependents)
# As we see values from 1-5 are available and NA(s) are there so here we are taking assumption 
# that NA means no dependents available and updating the dataset with 0 instead of NA.
#demographic_data$No.of.dependents[is.na(demographic_data$No.of.dependents)] <- 0

# Now lets merge the credit burew data with demographic data for all the applications
credit_card_applications <- merge(x = demographic_data, y = credit_bureau_data, by = "Application.ID", all = TRUE)
credit_card_applications$Performance.Tag <- credit_card_applications$Performance.Tag.x
credit_card_applications <- credit_card_applications[ , !(names(credit_card_applications) %in% c("Performance.Tag.x","Performance.Tag.y"))]
write.csv(credit_card_applications, "credit_card_applications.csv")
summary(credit_card_applications)

#compute correlation matrix of entire data set
#corr_res <- cor(credit_card_applications)
#round(corr_res, 2)

str(credit_card_applications)
#----------------------------------------------------
# Data Cleaning - Feature understanding and Univeriate analysis
#----------------------------------------------------
credit_card_eda <- credit_card_applications
#factor(credit_card_eda$Performance.Tag)
credit_card_eda$Performance.Tag <- as.factor(credit_card_eda$Performance.Tag)

#1. Application.ID
# As we know the application id is unique identifier for application of credit card so we can very well remove it.
credit_card_eda <- credit_card_eda[ , !(names(credit_card_eda) %in% c("Application.ID"))]

#2. Age
unique(credit_card_eda$Age)
# as age 0 and -3 are invalid age we can remove these inavid records
credit_card_eda <- credit_card_eda[!credit_card_eda$Age %in% c(0,-3),]
summary(credit_card_eda$Age)
# Age range is 15 to 65 let's categorize the age groups Youth (15-24 years), Adults (25-64 years), Seniors (65 years and over)
# create same category in dataset
credit_card_eda$AgeCategory <- cut(credit_card_eda$Age, 
                                   breaks = c(-Inf, 25, 61, Inf), 
                                   labels = c("Youth", "Adults", "Seniors"), 
                                   right = FALSE)
# We can remove old column
credit_card_eda <- credit_card_eda[ , !(names(credit_card_eda) %in% c("Age"))]
# check the distribution
ggplot(data = credit_card_eda, aes(x=AgeCategory)) + 
  geom_histogram(stat = "count")
# looking at the distribution it looks like more CC is distributed to adults


#3. Gender
unique(credit_card_eda$Gender)
# in dataset some of the genders are empty means it is invalid we can remove invalid data
credit_card_eda <- credit_card_eda[!credit_card_eda$Gender %in% c(""),]
ggplot(data = credit_card_eda, aes(x=Gender)) + 
  geom_histogram(stat = "count")
# looking at the distribution it looks like more CC is distributed more for Male

#4. Marital.Status..at.the.time.of.application.
names(credit_card_eda)[2]<-paste("Marital.Status")
unique(credit_card_eda$Marital.Status)
# in dataset some of the Marital Status are empty means it is invalid we can remove invalid data
credit_card_eda <- credit_card_eda[!credit_card_eda$Marital.Status %in% c(""),]
ggplot(data = credit_card_eda, aes(x=Marital.Status)) + 
  geom_histogram(stat = "count")
# looking at the distribution it looks like more CC is distributed more for Married

#5. No.of.dependents
unique(credit_card_eda$No.of.dependents)
# in dataset some of the No of dependents are empty means it is invalid we can remove invalid data
credit_card_eda <- credit_card_eda[!is.na(credit_card_eda$No.of.dependents),]
ggplot(data = credit_card_eda, aes(x=No.of.dependents)) + 
  geom_histogram(stat = "count")
# looking at the distribution it looks like more CC is distributed for 1-3 dependents

#6. Income
summary(credit_card_eda$Income)
# in dataset some of the Income are -0.5 and 0 which is invalid we can remove invalid data
credit_card_eda <- credit_card_eda[!credit_card_eda$Income %in% c(-0.5,0),]
ggplot(data = credit_card_eda, aes(y=Income, x= Performance.Tag)) + 
  geom_boxplot()
credit_card_eda$IncomeRange <- cut(credit_card_eda$Income, 
                                   breaks = c(-Inf, 16, 31, 45, Inf), 
                                   labels = c("Low", "Middle", "UpperMiddle", "High"), 
                                   right = FALSE)
ggplot(data = credit_card_eda, aes(x=IncomeRange)) + 
  geom_histogram(stat = "count")
# looking at the distribution one important point captured is that CC distribution 
#   is more for Low income range comparision to UpperMiddle

#7. Education
unique(credit_card_eda$Education)
# in dataset some of the Education are empty means it is invalid we can remove invalid data
credit_card_eda <- credit_card_eda[!credit_card_eda$Education %in% c(""),]
ggplot(data = credit_card_eda, aes(x=Education)) + 
  geom_histogram(stat = "count")
# looking at the distribution it looks like more CC is distributed less for Phd holders compare to batchlor and Master
# also cc is dirtibuted more toward professionals.

#8. Profession
unique(credit_card_eda$Profession)
# in dataset some of the Proffession are empty means it is invalid we can remove invalid data
credit_card_eda <- credit_card_eda[!credit_card_eda$Profession %in% c(""),]
ggplot(data = credit_card_eda, aes(x=Profession)) + 
  geom_histogram(stat = "count")
# looking at the distribution it looks like more CC is distributed more to salaried people.

#9. Type.of.residence
unique(credit_card_eda$Type.of.residence)
# in dataset some of the Type.of.residence are empty means it is invalid we can remove invalid data
credit_card_eda <- credit_card_eda[!credit_card_eda$Type.of.residence %in% c(""),]
ggplot(data = credit_card_eda, aes(x=Type.of.residence)) + 
  geom_histogram(stat = "count")
# looking at the distribution it looks like more CC is distributed for people having rented accomodation.

#10. No.of.months.in.current.residence
unique(credit_card_eda$No.of.months.in.current.residence)
summary(credit_card_eda$No.of.months.in.current.residence)
credit_card_eda$Residence.Years <- cut(credit_card_eda$No.of.months.in.current.residence, 
                                   breaks = c(-Inf, 25, 61, Inf), 
                                   labels = c("< 2 Years", "2-5 Years", "> 5 Years"), 
                                   right = FALSE)
ggplot(data = credit_card_eda, aes(x=Residence.Years)) + 
  geom_histogram(stat = "count")
# looking at the distribution it looks like more CC is distributed for 
#   people staying at current residents less than 2 years

#11. No.of.months.in.current.company
unique(credit_card_eda$No.of.months.in.current.company)
summary(credit_card_eda$No.of.months.in.current.company)
credit_card_eda$Company.Years <- cut(credit_card_eda$No.of.months.in.current.company, 
                                       breaks = c(-Inf, 25, 61, Inf), 
                                       labels = c("< 2 Years", "2-5 Years", "> 5 Years"), 
                                       right = FALSE)
ggplot(data = credit_card_eda, aes(x=Company.Years)) + 
  geom_histogram(stat = "count")
# looking at the distribution it looks like more CC is distributed for 
#   ploples working at same company between 2-5 years

#12. No.of.times.90.DPD.or.worse.in.last.6.months
unique(credit_card_eda$No.of.times.90.DPD.or.worse.in.last.6.months)
ggplot(data = credit_card_eda, aes(x=No.of.times.90.DPD.or.worse.in.last.6.months)) + 
  geom_histogram(stat = "count")

#13. No.of.times.60.DPD.or.worse.in.last.6.months
unique(credit_card_eda$No.of.times.60.DPD.or.worse.in.last.6.months)
ggplot(data = credit_card_eda, aes(x=No.of.times.60.DPD.or.worse.in.last.6.months)) + 
  geom_histogram(stat = "count")

#14. No.of.times.30.DPD.or.worse.in.last.6.months
unique(credit_card_eda$No.of.times.30.DPD.or.worse.in.last.6.months)
ggplot(data = credit_card_eda, aes(x=No.of.times.30.DPD.or.worse.in.last.6.months)) + 
  geom_histogram(stat = "count")

#15. No.of.times.90.DPD.or.worse.in.last.12.months
unique(credit_card_eda$No.of.times.90.DPD.or.worse.in.last.12.months)
ggplot(data = credit_card_eda, aes(x=No.of.times.90.DPD.or.worse.in.last.12.months)) + 
  geom_histogram(stat = "count")

#16. No.of.times.60.DPD.or.worse.in.last.12.months
unique(credit_card_eda$No.of.times.60.DPD.or.worse.in.last.12.months)
ggplot(data = credit_card_eda, aes(x=No.of.times.60.DPD.or.worse.in.last.12.months)) + 
  geom_histogram(stat = "count")

#17. No.of.times.30.DPD.or.worse.in.last.12.months
unique(credit_card_eda$No.of.times.30.DPD.or.worse.in.last.12.months)
ggplot(data = credit_card_eda, aes(x=No.of.times.30.DPD.or.worse.in.last.12.months)) + 
  geom_histogram(stat = "count")

#18. Avgas.CC.Utilization.in.last.12.months
summary(credit_card_eda$Avgas.CC.Utilization.in.last.12.months)

# 1020 records having NA's it means it requires treatment
# for these cases replacing the same with median value or WOE Value 
credit_card_eda[is.na(credit_card_eda$Avgas.CC.Utilization.in.last.12.months),]$Avgas.CC.Utilization.in.last.12.months <- 30
x_Avgas.CC.Utilization.in.last.12.months <- credit_card_eda$Avgas.CC.Utilization.in.last.12.months
qnt <- quantile(x_Avgas.CC.Utilization.in.last.12.months, probs=c(.25, .75), na.rm = T)
caps <- quantile(x_Avgas.CC.Utilization.in.last.12.months, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x_Avgas.CC.Utilization.in.last.12.months, na.rm = T)
x_Avgas.CC.Utilization.in.last.12.months[x_Avgas.CC.Utilization.in.last.12.months < (qnt[1] - H)] <- caps[1]
x_Avgas.CC.Utilization.in.last.12.months[x_Avgas.CC.Utilization.in.last.12.months > (qnt[2] + H)] <- caps[2]
credit_card_eda$x_Avgas.CC.Utilization.in.last.12.months <- x_Avgas.CC.Utilization.in.last.12.months
summary(credit_card_eda$Avgas.CC.Utilization.in.last.12.months)
ggplot(data = credit_card_eda, aes(y=Avgas.CC.Utilization.in.last.12.months, x= Performance.Tag)) + 
  geom_boxplot()
# Avgas.CC.Utilization.in.last.12.months looks strong predictor for the performance tag 

#19. No.of.trades.opened.in.last.6.months
summary(credit_card_eda$No.of.trades.opened.in.last.6.months)
# 1 Na can be removed
credit_card_eda <- credit_card_eda[!is.na(credit_card_eda$No.of.trades.opened.in.last.6.months),]
ggplot(data = credit_card_eda, aes(x=No.of.trades.opened.in.last.6.months)) + 
  geom_histogram(stat = "count")

#20. No.of.trades.opened.in.last.12.months
summary(credit_card_eda$No.of.trades.opened.in.last.12.months)
credit_card_eda <- credit_card_eda[!is.na(credit_card_eda$No.of.trades.opened.in.last.6.months),]
ggplot(data = credit_card_eda, aes(x=No.of.trades.opened.in.last.12.months)) + 
  geom_histogram(stat = "count")

#21. No.of.PL.trades.opened.in.last.6.months
summary(credit_card_eda$No.of.PL.trades.opened.in.last.6.months)
ggplot(data = credit_card_eda, aes(x=No.of.PL.trades.opened.in.last.6.months)) + 
  geom_histogram(stat = "count")

#22. No.of.PL.trades.opened.in.last.12.months
unique(credit_card_eda$No.of.PL.trades.opened.in.last.12.months)
ggplot(data = credit_card_eda, aes(x=No.of.PL.trades.opened.in.last.12.months)) + 
  geom_histogram(stat = "count")

#23. No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
unique(credit_card_eda$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
ggplot(data = credit_card_eda, aes(x=No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)) + 
  geom_histogram(stat = "count")


#24. No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
unique(credit_card_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
ggplot(data = credit_card_eda, aes(x=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)) + 
  geom_histogram(stat = "count")

#25. Presence.of.open.home.loan
unique(credit_card_eda$Presence.of.open.home.loan)
credit_card_eda <- credit_card_eda[!is.na(credit_card_eda$Presence.of.open.home.loan),]
ggplot(data = credit_card_eda, aes(x=Presence.of.open.home.loan)) + 
  geom_histogram(stat = "count")

#26. Outstanding.Balance
summary(credit_card_eda$Outstanding.Balance)
ggplot(data = credit_card_eda, aes(y=Outstanding.Balance, x= Performance.Tag)) + 
  geom_boxplot()
## Looks  like people with higher outstanding balance are more of defaulters.

#27. Total.No.of.Trades
summary(credit_card_eda$Total.No.of.Trades)
ggplot(data = credit_card_eda, aes(x=Total.No.of.Trades)) + 
  geom_histogram(stat = "count")

#28. Presence.of.open.auto.loan
unique(credit_card_eda$Presence.of.open.auto.loan)
ggplot(data = credit_card_eda, aes(x=Presence.of.open.auto.loan)) + 
  geom_histogram(stat = "count")

#--------------------------------------------------
# Bivariate Analysis
#--------------------------------------------------

plot_grid(ggplot(data = credit_card_eda, aes(x=Education, fill = Performance.Tag)) + 
  geom_bar(stat = "count", position = "dodge"),
ggplot(credit_card_eda, aes(Education, fill = Performance.Tag)) + 
  geom_bar(position = "fill", stat = "count") )
# By looking at plot no pattern is found so, Education may not be an important predictor.


plot_grid(ggplot(data = credit_card_eda, aes(x=Gender, fill = Performance.Tag)) + 
  geom_bar(stat = "count"),
ggplot(credit_card_eda, aes(Gender, fill = Performance.Tag)) + 
  geom_bar(position = "fill", stat = "count") )
# By looking at plot no pattern is found so, Gender may not be an important predictor.

plot_grid(ggplot(data = credit_card_eda, aes(x=Marital.Status, fill = Performance.Tag)) + 
  geom_bar(stat = "count"),
ggplot(credit_card_eda, aes(Marital.Status, fill = Performance.Tag)) + 
  geom_bar(position = "fill", stat = "count"))
# By looking at plot no pattern is found so, Marital status may not be an important predictor.

plot_grid(ggplot(data = credit_card_eda, aes(x=Profession, fill = Performance.Tag)) + 
  geom_bar(stat = "count"),
ggplot(credit_card_eda, aes(Profession, fill = Performance.Tag)) + 
    geom_bar(position = "fill", stat = "count")
  )
# By looking at plot no pattern is found so, Profession may not be an important predictor.

plot_grid(ggplot(data = credit_card_eda, aes(x=Type.of.residence, fill = Performance.Tag)) + 
  geom_bar(stat = "count"),
ggplot(credit_card_eda, aes(Type.of.residence, fill = Performance.Tag)) + 
  geom_bar(position = "fill", stat = "count"))
# By looking at plot no pattern is found so, ResidenceType may not be an important predictor.

plot_grid(ggplot(data = credit_card_eda, aes(x=No.of.dependents, fill = Performance.Tag)) + 
  geom_bar(stat = "count"),
ggplot(credit_card_eda, aes(No.of.dependents, fill = Performance.Tag)) + 
  geom_bar(position = "fill", stat = "count"))
# By looking at plot no pattern is found so, No.of.dependents may not be an important predictor.

plot_grid(ggplot(data = credit_card_eda, aes(x=Residence.Years, fill = Performance.Tag)) + 
  geom_bar(stat = "count"),
  ggplot(credit_card_eda, aes(Residence.Years, fill = Performance.Tag)) + 
    geom_bar(position = "fill", stat = "count"))
# By looking at plot No.of.Years.in.current.residence may be an important predictor.


plot_grid(ggplot(data = credit_card_eda, aes(x=Company.Years, fill = Performance.Tag)) + 
  geom_bar(stat = "count"),
ggplot(credit_card_eda, aes(Company.Years, fill = Performance.Tag)) + 
    geom_bar(position = "fill", stat = "count"))
# By looking at plot No.of.Years.in.current.company may be an important predictor.

plot_grid(ggplot(data = credit_card_eda, aes(x=IncomeRange, fill = Performance.Tag)) + 
  geom_bar(stat = "count"),
ggplot(credit_card_eda, aes(IncomeRange, fill = Performance.Tag)) + 
    geom_bar(position = "fill", stat = "count"))
# By looking at plot Income may be an important predictor.

plot_grid(ggplot(data = credit_card_eda, aes(x=No.of.trades.opened.in.last.6.months, fill = Performance.Tag)) + 
            geom_bar(stat = "count"),
          ggplot(credit_card_eda, aes(No.of.trades.opened.in.last.6.months, fill = Performance.Tag)) + 
            geom_bar(position = "fill", stat = "count"))
# By looking at plot No.of.trades.opened.in.last.6.months may be an important predictor.

plot_grid(ggplot(data = credit_card_eda, aes(x=No.of.trades.opened.in.last.12.months, fill = Performance.Tag)) + 
            geom_bar(stat = "count"),
          ggplot(credit_card_eda, aes(No.of.trades.opened.in.last.12.months, fill = Performance.Tag)) + 
            geom_bar(position = "fill", stat = "count"))
# By looking at plot No.of.trades.opened.in.last.12.months may be an important predictor.

plot_grid(ggplot(data = credit_card_eda, aes(x=No.of.PL.trades.opened.in.last.6.months, fill = Performance.Tag)) + 
            geom_bar(stat = "count"),
          ggplot(credit_card_eda, aes(No.of.PL.trades.opened.in.last.6.months, fill = Performance.Tag)) + 
            geom_bar(position = "fill", stat = "count"))
# By looking at plot No.of.PL.trades.opened.in.last.6.months may be an important predictor.

plot_grid(ggplot(data = credit_card_eda, aes(x=No.of.PL.trades.opened.in.last.12.months, fill = Performance.Tag)) + 
            geom_bar(stat = "count"),
          ggplot(credit_card_eda, aes(No.of.PL.trades.opened.in.last.12.months, fill = Performance.Tag)) + 
            geom_bar(position = "fill", stat = "count"))
# By looking at plot No.of.PL.trades.opened.in.last.12.months may be an important predictor.

plot_grid(ggplot(data = credit_card_eda, aes(x=No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., fill = Performance.Tag)) + 
            geom_bar(stat = "count"),
          ggplot(credit_card_eda, aes(No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., fill = Performance.Tag)) + 
            geom_bar(position = "fill", stat = "count"))
# By looking at plot No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. may be an important predictor.

plot_grid(ggplot(data = credit_card_eda, aes(x=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., fill = Performance.Tag)) + 
            geom_bar(stat = "count"),
          ggplot(credit_card_eda, aes(No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., fill = Performance.Tag)) + 
            geom_bar(position = "fill", stat = "count"))
# By looking at plot No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. may be an important predictor.

plot_grid(ggplot(data = credit_card_eda, aes(x=Presence.of.open.home.loan, fill = Performance.Tag)) + 
            geom_bar(stat = "count"),
          ggplot(credit_card_eda, aes(Presence.of.open.home.loan, fill = Performance.Tag)) + 
            geom_bar(position = "fill", stat = "count"))
# By looking at plot Presence.of.open.home.loan may be an important predictor.

plot_grid(ggplot(data = credit_card_eda, aes(x=Total.No.of.Trades, fill = Performance.Tag)) + 
            geom_bar(stat = "count"),
          ggplot(credit_card_eda, aes(Total.No.of.Trades, fill = Performance.Tag)) + 
            geom_bar(position = "fill", stat = "count"))
# By looking at plot Total.No.of.Trades may be an important predictor.

plot_grid(ggplot(data = credit_card_eda, aes(x=Presence.of.open.auto.loan, fill = Performance.Tag)) + 
            geom_bar(stat = "count"),
          ggplot(credit_card_eda, aes(Presence.of.open.auto.loan, fill = Performance.Tag)) + 
            geom_bar(position = "fill", stat = "count"))
# By looking at plot Presence.of.open.auto.loan may be an important predictor.

credit_card_eda_30DPD_grp_6 <- group_by(credit_card_eda, No.of.times.30.DPD.or.worse.in.last.6.months,Performance.Tag)
credit_card_eda_30DPD_6 <-  summarise(credit_card_eda_30DPD_grp_6,AvgOutstanding = mean(Outstanding.Balance))
credit_card_eda_60DPD_grp_6 <- group_by(credit_card_eda, No.of.times.60.DPD.or.worse.in.last.6.months,Performance.Tag)
credit_card_eda_60DPD_6 <-  summarise(credit_card_eda_60DPD_grp_6,AvgOutstanding = mean(Outstanding.Balance))
credit_card_eda_90DPD_grp_6 <- group_by(credit_card_eda, No.of.times.90.DPD.or.worse.in.last.6.months,Performance.Tag)
credit_card_eda_90DPD_6 <-  summarise(credit_card_eda_90DPD_grp_6,AvgOutstanding = mean(Outstanding.Balance))
credit_card_eda_30DPD_grp_12 <- group_by(credit_card_eda, No.of.times.30.DPD.or.worse.in.last.12.months,Performance.Tag)
credit_card_eda_30DPD_12 <-  summarise(credit_card_eda_30DPD_grp_12,AvgOutstanding = mean(Outstanding.Balance))
credit_card_eda_60DPD_grp_12 <- group_by(credit_card_eda, No.of.times.60.DPD.or.worse.in.last.12.months,Performance.Tag)
credit_card_eda_60DPD_12 <-  summarise(credit_card_eda_60DPD_grp_12,AvgOutstanding = mean(Outstanding.Balance))
credit_card_eda_90DPD_grp_12 <- group_by(credit_card_eda, No.of.times.90.DPD.or.worse.in.last.12.months,Performance.Tag)
credit_card_eda_90DPD_12 <-  summarise(credit_card_eda_90DPD_grp_12,AvgOutstanding = mean(Outstanding.Balance))

plot_grid(
  ggplot(data = credit_card_eda_30DPD_6, aes(x=No.of.times.30.DPD.or.worse.in.last.6.months,
                                           y = AvgOutstanding, fill = Performance.Tag,
                                           col = Performance.Tag)) + geom_line(size = 1.5),
  
  ggplot(data = credit_card_eda_60DPD_6, aes(x=No.of.times.60.DPD.or.worse.in.last.6.months,
                                           y = AvgOutstanding, fill = Performance.Tag,
                                           col = Performance.Tag)) + geom_line(size = 1.5),
  
  ggplot(data = credit_card_eda_90DPD_6, aes(x=No.of.times.90.DPD.or.worse.in.last.6.months,
                                           y = AvgOutstanding, fill = Performance.Tag,
                                           col = Performance.Tag)) + geom_line(size = 1.5),
  
  ggplot(data = credit_card_eda_30DPD_12, aes(x=No.of.times.30.DPD.or.worse.in.last.12.months,
                                           y = AvgOutstanding, fill = Performance.Tag,
                                           col = Performance.Tag)) + geom_line(size = 1.5),
  
  ggplot(data = credit_card_eda_60DPD_12, aes(x=No.of.times.60.DPD.or.worse.in.last.12.months,
                                           y = AvgOutstanding, fill = Performance.Tag,
                                           col = Performance.Tag)) + geom_line(size = 1.5),
  
  ggplot(data = credit_card_eda_90DPD_12, aes(x=No.of.times.90.DPD.or.worse.in.last.12.months,
                                           y = AvgOutstanding, fill = Performance.Tag,
                                           col = Performance.Tag)) + geom_line(size = 1.5)
)


credit_card_eda_30DPD_6 <-  summarise(credit_card_eda_30DPD_grp_6,AvgIncome = mean(Income))
credit_card_eda_60DPD_6 <-  summarise(credit_card_eda_60DPD_grp_6,AvgIncome = mean(Income))
credit_card_eda_90DPD_6 <-  summarise(credit_card_eda_90DPD_grp_6,AvgIncome = mean(Income))
credit_card_eda_30DPD_12 <-  summarise(credit_card_eda_30DPD_grp_12,AvgIncome = mean(Income))
credit_card_eda_60DPD_12 <-  summarise(credit_card_eda_60DPD_grp_12,AvgIncome = mean(Income))
credit_card_eda_90DPD_12 <-  summarise(credit_card_eda_90DPD_grp_12,AvgIncome = mean(Income))

plot_grid(
  ggplot(data = credit_card_eda_30DPD_6, aes(x=No.of.times.30.DPD.or.worse.in.last.6.months,
                                             y = AvgIncome, fill = Performance.Tag,
                                             col = Performance.Tag)) + geom_line(size = 1.5),
  
  ggplot(data = credit_card_eda_60DPD_6, aes(x=No.of.times.60.DPD.or.worse.in.last.6.months,
                                             y = AvgIncome, fill = Performance.Tag,
                                             col = Performance.Tag)) + geom_line(size = 1.5),
  
  ggplot(data = credit_card_eda_90DPD_6, aes(x=No.of.times.90.DPD.or.worse.in.last.6.months,
                                             y = AvgIncome, fill = Performance.Tag,
                                             col = Performance.Tag)) + geom_line(size = 1.5),
  
  ggplot(data = credit_card_eda_30DPD_12, aes(x=No.of.times.30.DPD.or.worse.in.last.12.months,
                                              y = AvgIncome, fill = Performance.Tag,
                                              col = Performance.Tag)) + geom_line(size = 1.5),
  
  ggplot(data = credit_card_eda_60DPD_12, aes(x=No.of.times.60.DPD.or.worse.in.last.12.months,
                                              y = AvgIncome, fill = Performance.Tag,
                                              col = Performance.Tag)) + geom_line(size = 1.5),
  
  ggplot(data = credit_card_eda_90DPD_12, aes(x=No.of.times.90.DPD.or.worse.in.last.12.months,
                                              y = AvgIncome, fill = Performance.Tag,
                                              col = Performance.Tag)) + geom_line(size = 1.5)
)

#----------------------------------------------------------------------------------
#IMP - Yet to merge
#----------------------------------------------------------------------------------

box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")


#Box plot for DPD values 
plot_grid(ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.times.90.DPD.or.worse.in.last.6.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.times.90.DPD.or.worse.in.last.12.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.times.60.DPD.or.worse.in.last.6.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.times.60.DPD.or.worse.in.last.12.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.times.30.DPD.or.worse.in.last.6.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.times.30.DPD.or.worse.in.last.12.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 4)



#Box plot for numerical categorial variables
#--------------------------------------------
credit_card_applications$Age <- as.numeric(credit_card_applications$Age)
plot_grid(ggplot(credit_card_applications, aes(x=Performance.Tag,y=Income, fill=Performance.Tag))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.dependents, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=Age, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=Presence.of.open.home.loan, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=Presence.of.open.auto.loan, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 4)
#variable Presence.of.open.auto.loan and Presence.of.open.home.loan are insigficant as they do not have any impact on the default
#variable Age and 'No.of.dependents' are both insignificants as they default and non-default have the same data range
#variable seems to be significant as maximum default occurs around the range 20 to 25 and it has no outlier

# Histogram and Boxplots for numeric variables
# --------------------------------------------
str(credit_card_applications)


#No.of.months.in.current.residence
plot_grid(ggplot(credit_card_applications, aes(No.of.months.in.current.residence,fill = Performance.Tag))+ geom_histogram(binwidth = 10),
          ggplot(credit_card_applications, aes(x="",y=No.of.months.in.current.residence))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)



#No.of.months.in.current.company
plot_grid(ggplot(credit_card_applications, aes(No.of.months.in.current.company,fill = Performance.Tag))+ geom_histogram(binwidth = 12, colour='blue'),
          ggplot(credit_card_applications, aes(x="",y=No.of.months.in.current.company))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


#Avgas.CC.Utilization.in.last.12.months
plot_grid(ggplot(credit_card_applications, aes(Avgas.CC.Utilization.in.last.12.months,fill = Performance.Tag))+ geom_histogram(binwidth = 10, colour='blue'),
          ggplot(credit_card_applications, aes(x="",y=Avgas.CC.Utilization.in.last.12.months))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
#No.of.trades.opened.in.last.6.months
plot_grid(ggplot(credit_card_applications, aes(No.of.trades.opened.in.last.6.months,fill = Performance.Tag))+ geom_histogram(binwidth = 1, colour='blue'),
          ggplot(credit_card_applications, aes(x="",y=No.of.trades.opened.in.last.6.months))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#No.of.trades.opened.in.last.12.months 
plot_grid(ggplot(credit_card_applications, aes(No.of.trades.opened.in.last.12.months ,fill = Performance.Tag))+ geom_histogram(binwidth = 1, colour='blue'),
          ggplot(credit_card_applications, aes(x="",y=No.of.trades.opened.in.last.12.months))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#No.of.PL.trades.opened.in.last.6.months 
plot_grid(ggplot(credit_card_applications, aes(No.of.PL.trades.opened.in.last.6.months ,fill = Performance.Tag))+ geom_histogram(binwidth = 1, colour='blue'),
          ggplot(credit_card_applications, aes(x="",y=No.of.PL.trades.opened.in.last.6.months))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#No.of.PL.trades.opened.in.last.12.months 
plot_grid(ggplot(credit_card_applications, aes(No.of.PL.trades.opened.in.last.12.months ,fill = Performance.Tag))+ geom_histogram(binwidth = 1, colour='blue'),
          ggplot(credit_card_applications, aes(x="",y=No.of.PL.trades.opened.in.last.12.months))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 
#Since the name is long it is renamed as No.of.Inquiries.in.last.6.months
names(credit_card_applications)[names(credit_card_applications) == 'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.'] <- 'No.of.Inquiries.in.last.6.months'
plot_grid(ggplot(credit_card_applications, aes(No.of.Inquiries.in.last.6.months ,fill = Performance.Tag))+ geom_histogram(binwidth = 1, colour='blue'),
          ggplot(credit_card_applications, aes(x="",y=No.of.Inquiries.in.last.6.months))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 
#Since the name is long it is renamed as No.of.Inquiries.in.last.12.months
names(credit_card_applications)[names(credit_card_applications) == 'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.'] <- 'No.of.Inquiries.in.last.12.months'
plot_grid(ggplot(credit_card_applications, aes(No.of.Inquiries.in.last.12.months ,fill = Performance.Tag))+ geom_histogram(binwidth = 1, colour='blue'),
          ggplot(credit_card_applications, aes(x="",y=No.of.Inquiries.in.last.12.months))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#Outstanding.Balance 
credit_card_applications$Outstanding.Balance.in.lakh <- credit_card_applications$Outstanding.Balance/100000
plot_grid(ggplot(credit_card_applications, aes(Outstanding.Balance.in.lakh ,fill = Performance.Tag))+ geom_histogram(binwidth = 5, colour='blue'),
          ggplot(credit_card_applications, aes(x="",y=Outstanding.Balance.in.lakh))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


#Total.No.of.Trades
plot_grid(ggplot(credit_card_applications, aes(Total.No.of.Trades ,fill = Performance.Tag))+ geom_histogram(binwidth = 5, colour='blue'),
          ggplot(credit_card_applications, aes(x="",y=Total.No.of.Trades))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


# Boxplots of numeric variables relative to performance tag
plot_grid(ggplot(credit_card_applications, aes(x=Performance.Tag,y=Outstanding.Balance.in.lakh, fill=Performance.Tag))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=Total.No.of.Trades, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.Inquiries.in.last.12.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.Inquiries.in.last.6.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.PL.trades.opened.in.last.12.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.PL.trades.opened.in.last.6.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.trades.opened.in.last.12.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.trades.opened.in.last.6.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=Avgas.CC.Utilization.in.last.12.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 4)

plot_grid(ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.months.in.current.residence, fill=Performance.Tag))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.months.in.current.company , fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 4)

# Variales 'No.of.months.in.current.residence'  seems to be significant and sees large number of default in the range of 20 to 25, also it has no outliers
# Variales 'No.of.months.in.current.company'  seems to be significant and sees large number of default in the range of 30 to 40, also it has no outliers  


#by looking into the box plot and histogram
# Variable 'Outstanding.Balance.in.lakh'  sees large number of default in the range of 5 to 10 lakh and it has large number of outliers as well

# Variable 'Avgas.CC.Utilization.in.last.12.month'  sees large number of default in the range of 30 to 40

# Variale 'No.of.PL.trades.opened.in.last.12.month's  sees large number of default in the range of 2 to 5 and  it has no outliers

# Variable 'No.of.Inquiries.in.last.12.months' sees large number of default in the range of 2 to 5 and  it has very less outliers

# Variable 'No.of.Inquiries.in.last.6.months'  sees large number of default in the range of 0 to 2.5 and  it has very less outliers

# Variable 'Total.No.of.Trades'  has large number of outliers

#the above box plot shows that the below variables have significant outliers
# Outstanding.Balance.in.lakh, Total.No.of.Trades

#less outliers are observed for below variables
# No.of.Inquiries.in.last.12.months, No.of.trades.opened.in.last.12.months, No.of.trades.opened.in.last.6.months

#outlier treatment for Outstanding.Balance.in.lakh using capping

#For  values that lie outside the 1.5 * IQR limits, we could cap it by replacing those observations outside the lower limit with the value of 5th %ile 
#and those that lie above the upper limit, with the value of 95th %ile. Below is a sample code that achieves this.

x_Outstanding.Balance.in.lakh <- credit_card_applications$Outstanding.Balance.in.lakh
qnt <- quantile(x_Outstanding.Balance.in.lakh, probs=c(.25, .75), na.rm = T)
caps <- quantile(x_Outstanding.Balance.in.lakh, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x_Outstanding.Balance.in.lakh, na.rm = T)
x_Outstanding.Balance.in.lakh[x_Outstanding.Balance.in.lakh < (qnt[1] - H)] <- caps[1]
x_Outstanding.Balance.in.lakh[x_Outstanding.Balance.in.lakh > (qnt[2] + H)] <- caps[2]
credit_card_applications$Outstanding.Balance.in.lakh <- x_Outstanding.Balance.in.lakh

#outlier treatment for Total.No.of.Trades using capping

x_Total.No.of.Trades <- credit_card_applications$Total.No.of.Trades
qnt <- quantile(x_Total.No.of.Trades, probs=c(.25, .75), na.rm = T)
caps <- quantile(x_Total.No.of.Trades, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x_Total.No.of.Trades, na.rm = T)
x_Total.No.of.Trades[x_Total.No.of.Trades < (qnt[1] - H)] <- caps[1]
x_Total.No.of.Trades[x_Total.No.of.Trades > (qnt[2] + H)] <- caps[2]
credit_card_applications$Total.No.of.Trades <- x_Total.No.of.Trades


plot_grid(ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.months.in.current.residence, fill=Performance.Tag))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.months.in.current.company , fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.times.90.DPD.or.worse.in.last.6.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.times.90.DPD.or.worse.in.last.12.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.times.60.DPD.or.worse.in.last.6.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.times.60.DPD.or.worse.in.last.12.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.times.30.DPD.or.worse.in.last.6.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(credit_card_applications, aes(x=Performance.Tag,y=No.of.times.30.DPD.or.worse.in.last.12.months, fill=Performance.Tag))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 4)

# Variales 'No.of.months.in.current.residence'  seems to be significant and sees large number of default in the range of 20 to 25, also it has no outliers
# Variales 'No.of.months.in.current.company'  seems to be significant and sees large number of default in the range of 30 to 40, also it has no outliers  

#categorical variables 

str(credit_card_applications)
categorical_variables <- c("Gender","Marital.Status..at.the.time.of.application.","No.of.dependents",
                           "Education","Profession","Type.of.residence","Presence.of.open.home.loan","Presence.of.open.auto.loan","Performance.Tag",
                           "No.of.times.90.DPD.or.worse.in.last.6.months","No.of.times.90.DPD.or.worse.in.last.12.months",
                           "No.of.times.60.DPD.or.worse.in.last.6.months","No.of.times.60.DPD.or.worse.in.last.12.months",
                           "No.of.times.30.DPD.or.worse.in.last.6.months","No.of.times.30.DPD.or.worse.in.last.12.months")

#continuous variables
continuous_variables <- c("Income","No.of.months.in.current.residence","No.of.months.in.current.company","Performance.Tag",
                          "Avgas.CC.Utilization.in.last.12.months","No.of.trades.opened.in.last.6.months","No.of.trades.opened.in.last.12.months","No.of.PL.trades.opened.in.last.6.months",     
                          "No.of.PL.trades.opened.in.last.12.months","No.of.Inquiries.in.last.6.months","No.of.Inquiries.in.last.12.months","Outstanding.Balance.in.lakh", 
                          "Total.No.of.Trades") 

credit_card_applications_categorical_var <- credit_card_applications[,categorical_variables]
credit_card_applications_numerical_var <- credit_card_applications[,continuous_variables]




#role rate matrix
#----------------
columes_for_role_rate_matrix_dpd_6_months <- c("No.of.times.30.DPD.or.worse.in.last.6.months",
                           "No.of.times.60.DPD.or.worse.in.last.6.months",
                           "No.of.times.90.DPD.or.worse.in.last.6.months")

columes_for_role_rate_matrix_dpd_12_months <- c("No.of.times.30.DPD.or.worse.in.last.12.months",
                                  "No.of.times.60.DPD.or.worse.in.last.12.months",
                                  "No.of.times.90.DPD.or.worse.in.last.12.months")

df_role_rate_matrix_dpd_6_months <- credit_card_applications[,columes_for_role_rate_matrix_dpd_6_months]

df_role_rate_matrix_dpd_12_months <- credit_card_applications[,columes_for_role_rate_matrix_dpd_12_months]

View(df_role_rate_matrix_dpd_12_months)

trans.matrix <- function(X, prob=T)
{
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
}

mat_role_rate_dpd_dpd_6_months <- trans.matrix(as.matrix(df_role_rate_matrix_dpd_6_months))

mat_role_rate_dpd_dpd_12_months <- trans.matrix(as.matrix(df_role_rate_matrix_dpd_12_months))

old_cols_dpd_6_months <- colnames(mat_role_rate_dpd_dpd_6_months)

new_cols_dpd_6_months <- c("30.DPD.last.6.mon","60.DPD.last.6.mon","90.DPD.last.6.mon")

old_cols_dpd_12_months <- colnames(mat_role_rate_dpd_dpd_12_months)

new_cols_dpd_12_months <- c("30.DPD.last.12.mon","60.DPD.last.12.mon","90.DPD.last.12.mon")

mat_role_rate_dpd_dpd_6_months
colnames(mat_role_rate_dpd_dpd_6_months) <- c("current","0-29","30-59","60-89","90-119","120+")
rownames(mat_role_rate_dpd_dpd_6_months) <- c("current","0-29","30-59","60-89","90-119","120+","Charged Off", "Paid")

write.csv(mat_role_rate_dpd_dpd_6_months, "mat_role_rate_dpd_dpd_6_months.csv")

colnames(mat_role_rate_dpd_dpd_12_months) <- c("current","0-29","30-59","60-89","90-119","120-149","150-179","180+")
rownames(mat_role_rate_dpd_dpd_12_months) <- c("current","0-29","30-59","60-89","90-119","120-149","150-179","180+","Charged Off","Paid")
write.csv(mat_role_rate_dpd_dpd_12_months, "mat_role_rate_dpd_dpd_12_months.csv")

# Correlation between categorical variables
#-----------------------------------------
#rename the long column names to short column names to better visualize the corelation matrixs

new_column_names_categorical <- c("perf.tag","Gender","Marital.Status","No.of.dependents",
                                  "Education","Profession","Type.of.residence","home.loan","auto.loan",
                                  "90.DPD.6.mon","90.DPD.12.mon",
                                  "60.DPD.6.mon","60.DPD.12.mon",
                                  "30.DPD.6.mon","30.DPD.12.mon"     
                      ) 

#TODO - to find the corelation between the DPD significant variables

# Correlation between numeric variables
#-----------------------------------------
#rename the long column names to short column names to better visualize the corelation matrixs



new_column_names_continuous <- c("Income","mon.curr.res","mon.curr.com","perf.tag",
                                 "CC.Util.12.mon","trades.6.mon","trades.12.mon","PL.trades.6.mon",     
                      "PL.trades.12.mon","Inq.6.mon","Inq.12.mons","Out.Bal", 
                      "Trades") 
x <- credit_card_applications_numerical_var
for(i in 1:length(continuous_variables)) names(x)[names(x) == continuous_variables[i]] = new_column_names[i]

#ggpairs(x[, 1:5])

cor(x[, c("Out.Bal","Trades","Inq.6.mon","Inq.12.mons")], use = "complete.obs")
#var No.of.Inquiries.in.last.12.months and No.of.Inquiries.in.last.6.months seems to be highly corelated with other independent variables, we can ignore them 

cor(x[, c("Out.Bal","Trades","trades.6.mon","trades.12.mon","PL.trades.6.mon",     
          "PL.trades.12.mon")], use = "complete.obs")
#var No.of.trades.opened.in.last.12.months,No.of.PL.trades.opened.in.last.12.months,
# No.of.trades.opened.in.last.6.months,No.of.PL.trades.opened.in.last.6.months
# seems to be highly corelated with other independent variables, we can ignore them 

cor(x[, c("Income","Outstanding.Balance.in.lakh","Inq.6.mon","Inq.12.mons","mon.curr.res", "mon.curr.com", "PL.trades.12.mon",
          "PL.trades.6.mon")], use = "complete.obs")


#From the EDA, following seems to be the significant variables
# Income, No.of.months.in.current.residence, No.of.months.in.current.company, No.of.Inquiries.in.last.6.months, No.of.Inquiries.in.last.2.months
# No.of.PL.trades.opened.in.last.6.months, No.of.PL.trades.opened.in.last.12.months
# But remember, among these significant variables, one or two variables are highly correlated to each other which can be further
# anyalysed and removed as part of stepAIC


#Using WOE to the continuous variables
colnames(credit_card_applications_numerical_var)
str(credit_card_applications)
IV <- create_infotables(data=credit_card_applications_numerical_var, y="Performance.Tag.x", bins=10, parallel=TRUE)
IV$Summary

#------------------------------------------------------------------------------
# 3. Data Prepration
#------------------------------------------------------------------------------
# Feature selection
# Performing Hypothysis testing to find the significant variables:
# 1. Education - Null hypothesis is Education is insignificant in deciding customer will default
cc_applications_Edu <- table(credit_card_eda$Performance.Tag, credit_card_eda$Education)
chisq.test(cc_applications_Edu)
#X-squared = 2.5828, df = 4, p-value = 0.6299
#since p value >.05 we accept that education is insignificant variable.

# 2. Gender - Null hypothesis is Gender is insignificant in deciding customer will default
cc_applications_Gender <- table(credit_card_eda$Performance.Tag, credit_card_eda$Gender)
chisq.test(cc_applications_Gender)
#X-squared = 0.96875, df = 1, p-value = 0.325

##since p value >.05 we accept that Gender  is insignificant variable.

# 3. Marital status - Null hypothesis: Marital status is insignificant in deciding customer will default
cc_applications_Married <- table(credit_card_eda$Performance.Tag, credit_card_eda$Marital.Status)
chisq.test(cc_applications_Married)
#X-squared = 0.2546, df = 1, p-value = 0.6139

##since p value >.05 we accept that Marital status   is insignificant variable.


# 4. Profession - Null hypothesis: Profession is insignificant in deciding customer will default
cc_applications_Profession <- table(credit_card_eda$Performance.Tag, credit_card_eda$Profession)
chisq.test(cc_applications_Profession)
# X-squared = 5.8465, df = 2, p-value = 0.05376

##since p value >.05 we accept that profession   is insignificant variable.

# 5. ResidenceType - Null hypothesis: ResidenceType is insignificant in deciding customer will default
cc_applications_Residence <- table(credit_card_eda$Performance.Tag, credit_card_eda$Type.of.residence)
chisq.test(cc_applications_Residence)
# X-squared = 2.2175, df = 4, p-value = 0.6958

##since p value >.05 we accept that ResidenceType is insignificant variable.


# 6. Number of dependents - Null hypothesis: Number of dependents is insignificant in deciding customer will default
cc_applications_Dependents <- table(credit_card_eda$Performance.Tag, credit_card_eda$No.of.dependents)
chisq.test(cc_applications_Dependents)
#X-squared = 7.6824, df = 4, p-value = 0.1039

##since p value >.05 we accept that ResidenceType is insignificant variable.


# 7. Number of years in current residence - Null hypothesis: Number of month in current residence is insignificant in deciding customer will default
cc_applications_residenceYrs <- table(credit_card_eda$Performance.Tag, credit_card_eda$Residence.Years)
chisq.test(cc_applications_residenceYrs)
# X-squared = 87.972, df = 2, p-value = 2.2e-16

##since p value <.05 we accept reject the null hypothesis - ie Number of years in current residence is a significant variable


# 8. Number of years  in current company - Null hypothesis: Number of month in current company is insignificant in deciding customer will default
cc_applications_companyYrs <- table(credit_card_eda$Performance.Tag, credit_card_eda$Company.Years)
chisq.test(cc_applications_companyYrs)
#X-squared = 42.432, df = 2, p-value = 6.108e-10
##since p value <.05 we accept reject the null hypothesis - ie Number of years  in current company is a significant variable


# 9. Income - Null hypothesis: Income is insignificant in deciding customer will default
cc_applications_income <- table(credit_card_eda$Performance.Tag, credit_card_eda$IncomeRange)
chisq.test(cc_applications_income)
# X-squared = 99.127, df = 3, p-value = 2.2e-16

##since p value <.05 we accept reject the null hypothesis - ie Income range  is a significant variable


# 10. Age  - Null hypothesis is Education is insignificant in deciding customer will default
summary(credit_card_eda$Age)
##credit_card_eda$AgeRange <- cut(credit_card_eda$Age, 
                                                    #    breaks = c(-Inf, 11, 21, 31, 41, 51, 61, Inf), 
                                                       # labels = c("0-10 Years", "11-20 Years", "21-30 Years", "31-40 Years", "41-50 Years", "51-60 Years", ">60 Years"), 
                                                       # right = FALSE)
cc_applications_age <- table(credit_card_eda$Performance.Tag, credit_card_eda$AgeCategory)
chisq.test(cc_applications_age)
#X-squared = 2.6088, df = 2, p-value = 0.2713

##since p value >.05 we accept reject the null hypothesis - ie Age category  is a insignificant variable

# 11. No.of.times.90.DPD.or.worse.in.last.6.months- Null hypothesis is that No.of.times.90.DPD.or.worse.in.last.6.months is an insignificant vaariable
cc_application_No.of.times.90.DPD.or.worse.in.last.6.months <- table(credit_card_eda$Performance.Tag,credit_card_eda$No.of.times.90.DPD.or.worse.in.last.6.months)
chisq.test(cc_application_No.of.times.90.DPD.or.worse.in.last.6.months)

# X-squared = 557.67, df = 3, p-value < 2.2e-16
#since p value <.05 we  reject the null hypothesis - ie No.of.times.90.DPD.or.worse.in.last.6.months  is a significant variable

# 12.No.of.times.60.DPD.or.worse.in.last.6.months - Null hypotheis is that No.of.times.60.DPD.or.worse.in.last.6.months is insignificant variable 
cc_application_No.of.times.60.DPD.or.worse.in.last.6.months <- table(credit_card_eda$Performance.Tag,credit_card_eda$No.of.times.60.DPD.or.worse.in.last.6.months)
chisq.test(cc_application_No.of.times.60.DPD.or.worse.in.last.6.months)
# X-squared = 712.14, df = 5, p-value < 2.2e-16
#since p value <.05 we  reject the null hypothesis - ie No.of.times.60.DPD.or.worse.in.last.6.months  is a significant variable

#13. No.of.times.30.DPD.or.worse.in.last.6.months - Null hypotheis is that No.of.times.30.DPD.or.worse.in.last.6.months is insignificant variable 
cc_application_No.of.times.30.DPD.or.worse.in.last.6.months  <- table(credit_card_eda$Performance.Tag,credit_card_eda$No.of.times.30.DPD.or.worse.in.last.6.months)
chisq.test(cc_application_No.of.times.30.DPD.or.worse.in.last.6.months )

# X-squared = 818.74, df = 7, p-value < 2.2e-16
# since p value <.05 we  reject the null hypothesis - ie No.of.times.30.DPD.or.worse.in.last.6.months  is a significant variable

# 14. No.of.times.90.DPD.or.worse.in.last.12.months - Null hypotheis is that No.of.times.90.DPD.or.worse.in.last.12.months is insignificant variable 

cc_application_No.of.times.90.DPD.or.worse.in.last.12.months <- table(credit_card_eda$Performance.Tag,credit_card_eda$No.of.times.90.DPD.or.worse.in.last.12.months)
chisq.test(cc_application_No.of.times.90.DPD.or.worse.in.last.12.months)

# X-squared = 713.49, df = 5, p-value < 2.2e-16
# since p value <.05 we  reject the null hypothesis - ie No.of.times.90.DPD.or.worse.in.last.12.months  is a significant variable

#15. No.of.times.60.DPD.or.worse.in.last.12.months- Null hypotheis is that No.of.times.60.DPD.or.worse.in.last.12.months is insignificant variable  

cc_application_No.of.times.60.DPD.or.worse.in.last.12.months <- table(credit_card_eda$Performance.Tag,credit_card_eda$No.of.times.60.DPD.or.worse.in.last.12.months)
chisq.test(cc_application_No.of.times.60.DPD.or.worse.in.last.12.months)

# X-squared = 627.45, df = 7, p-value < 2.2e-16
# since p value <.05 we  reject the null hypothesis - ie No.of.times.60.DPD.or.worse.in.last.12.months  is a significant variable


#16. No.of.times.30.DPD.or.worse.in.last.12.months- Null hypotheis is that No.of.times.30.DPD.or.worse.in.last.12.months is insignificant variable  

cc_application_No.of.times.30.DPD.or.worse.in.last.12.months <- table(credit_card_eda$Performance.Tag,credit_card_eda$No.of.times.30.DPD.or.worse.in.last.12.months)
chisq.test(cc_application_No.of.times.30.DPD.or.worse.in.last.12.months)

#X-squared = 746.64, df = 9, p-value < 2.2e-16

# since p value <.05 we  reject the null hypothesis - ie No.of.times.30.DPD.or.worse.in.last.12.months  is a significant variable


#17.Avgas.CC.Utilization.in.last.12.months - Null hypotheis is that Avgas.CC.Utilization.in.last.12.months is insignificant variable 
# Please ensure NA are properly removed since we are getting a warning in chi sqaure test 

cc_application_Avgas.CC.Utilization.in.last.12.months <- table(credit_card_eda$Performance.Tag,credit_card_eda$Avgas.CC.Utilization.in.last.12.months)
chisq.test(cc_application_Avgas.CC.Utilization.in.last.12.months)
#X-squared = 1013.4, df = 113, p-value < 2.2e-16

# since p value <.05 we  reject the null hypothesis - ie vgas.CC.Utilization.in.last.12.months is a significant variable

# 18 No.of.trades.opened.in.last.6.months - Null hypothesis is that No.of.trades.opened.in.last.6.months is an insignificant variable

cc_application_No.of.trades.opened.in.last.6.months <- table(credit_card_eda$Performance.Tag,credit_card_eda$No.of.trades.opened.in.last.6.months)
chisq.test(cc_application_No.of.trades.opened.in.last.6.months)


#X-squared = 516.94, df = 12, p-value < 2.2e-16
# since p value <.05 we  reject the null hypothesis - ie No.of.trades.opened.in.last.6.month is a significant variable

#19. No.of.trades.opened.in.last.12.months- Null hypothesis is that No.of.trades.opened.in.last.12.months is an insignificant variable
cc_application_No.of.trades.opened.in.last.12.months <- table(credit_card_eda$Performance.Tag,credit_card_eda$No.of.trades.opened.in.last.12.months)
chisq.test(cc_application_No.of.trades.opened.in.last.12.months)

#X-squared = 771.09, df = 28, p-value < 2.2e-16
# since p value <.05 we  reject the null hypothesis - ie No.of.trades.opened.in.last.12.month is a significant variable

# 20. No.of.PL.trades.opened.in.last.6.months- null hypothesis is that No.of.PL.trades.opened.in.last.6.months is an insignificant variable

cc_application_No.of.PL.trades.opened.in.last.6.months <- table(credit_card_eda$Performance.Tag,credit_card_eda$No.of.PL.trades.opened.in.last.6.months)
chisq.test(cc_application_No.of.PL.trades.opened.in.last.6.months)

#X-squared = 576.47, df = 6, p-value < 2.2e-16

# since p value <.05 we  reject the null hypothesis - ie No.of.PL.trades.opened.in.last.6.months is a significant variable

#21.No.of.PL.trades.opened.in.last.12.months- null hypothesis is that No.of.PL.trades.opened.in.last.12.months is an insignificant variable

cc_application_No.of.PL.trades.opened.in.last.12.months <- table(credit_card_eda$Performance.Tag,credit_card_eda$No.of.PL.trades.opened.in.last.12.months)
chisq.test(cc_application_No.of.PL.trades.opened.in.last.12.months)

# X-squared = 717.11, df = 12, p-value < 2.2e-16
# since p value <.05 we  reject the null hypothesis - ie No.of.PL.trades.opened.in.last.12.months is a significant variable

#22. No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.- null hypothesis is that No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. is an insignificant variable

cc_application_No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- table(credit_card_eda$Performance.Tag,credit_card_eda$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
chisq.test(cc_application_No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

# X-squared = 528.65, df = 10, p-value < 2.2e-16
# since p value <.05 we  reject the null hypothesis - ie No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.is a signficant variable.

#23. No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.- null hypothesis is that No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. is an insignificant variable

cc_application_No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- table(credit_card_eda$Performance.Tag,credit_card_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
chisq.test(cc_application_No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

# X-squared = 684.01, df = 20, p-value < 2.2e-16

#since p value <.05 we  reject the null hypothesis - ie No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.is a signficant variable.

#24. Presence.of.open.home.loan - null hypothesis is that Presence.of.open.home.loan is an insignificant variable

cc_application_Presence.of.open.home.loan <- table(credit_card_eda$Performance.Tag,credit_card_eda$Presence.of.open.home.loan)
chisq.test(cc_application_Presence.of.open.home.loan)

#X-squared = 43.353, df = 1, p-value = 4.571e-11
#since p value <.05 we  reject the null hypothesis - ie Presence.of.open.home.loan is a signficant variable.

#25. Presence.of.open.auto.loan

cc_Presence.of.open.auto.loan <- table(credit_card_eda$Performance.Tag,credit_card_eda$Presence.of.open.auto.loan)
chisq.test(cc_Presence.of.open.auto.loan)

# X-squared = 4.1051, df = 1, p-value = 0.04276
#since p value <.05 we  reject the null hypothesis - ie Presence.of.open.auto.loan is a signficant variable.

#26. Outstanding.Balance- Null hypoteisis is that Outstanding.Balance is an insignificant variable

cc_Outstanding.Balance <- table(credit_card_eda$Performance.Tag,credit_card_eda$Outstanding.Balance)
chisq.test(cc_Outstanding.Balance )

# X-squared = 65693, df = 63716, p-value = 2.086e-08
#since p value <.05 we  reject the null hypothesis - ie Outstanding.Balance is a signficant variable.

#27. Total.No.of.Trades - Null hypothesis is that Total.No.of.Trades is an insignificant variable

cc_Total.No.of.Trades <- table(credit_card_eda$Performance.Tag,credit_card_eda$Total.No.of.Trades)
chisq.test(cc_Total.No.of.Trades )

# X-squared = 697.03, df = 44, p-value < 2.2e-16
# #since p value <.05 we  reject the null hypothesis - ie Total.No.of.Trades is a signficant variable.

#-------------Summary of feature selection-chqi square test ------------------------------------------------------------
#######################################################################################################

# Sno     Variable name                                               pvalue                   Significant 
# 1.      Education                                                   0.6299                            No
# 2.      Gender                                                      0.325                             No
# 3.      Marital status                                              0.6139                            No
# 4.      Profession                                                  0.05376                           No
# 5.      Residence Type                                              0.6958                            No
# 6.      No of dependents                                            0.1039                            No
# 7.      Number of years in current residence                        2.2e-16                           Yes
# 8.      Number of years in current company                          6.108e-10                         Yes
# 9.      Income                                                      2.2e-16                           Yes 
# 10.     Age                                                         0.2713                            No
# 11.     No.of.times.90.DPD.or.worse.in.last.6.months                2.2e-16                           Yes
# 12.     No.of.times.60.DPD.or.worse.in.last.6.months                2.2e-16                           Yes
# 13.     No.of.times.30.DPD.or.worse.in.last.6.months                2.2e-16                           Yes
# 14.     No.of.times.90.DPD.or.worse.in.last.12.months               2.2e-16                           Yes
# 15.     No.of.times.60.DPD.or.worse.in.last.12.months               2.2e-16                           Yes
# 16.     No.of.times.30.DPD.or.worse.in.last.12.months               2.2e-16                           Yes
# 17.     Avgas.CC.Utilization.in.last.12.months                      2.2e-16                           Yes
# 18.     No.of.trades.opened.in.last.6.months                        2.2e-16                           Yes
# 19.     No.of.trades.opened.in.last.12.months                       2.2e-16                           Yes
# 20.     No.of.PL.trades.opened.in.last.6.months                     2.2e-16                           Yes
# 21.     No.of.PL.trades.opened.in.last.12.months                    2.2e-16                           Yes
# 22.     No.of.Inquiries.in.last.6.months..excluding.home.auto.loans 2.2e-16                           Yes
# 23      No.of.Inquiries.in.last.12.months..excluding.home.auto.loans2.2e-16                           No
# 24.     Presence.of.open.home.loan                                  4.571e-11                         Yes
# 25.     Presence.of.open.auto.loan                                  0.04276                           Yes
# 26.     Outstanding.Balance                                         2.086e-08                         Yes
# 27.     Total.No.of.Trades                                          2.2e-16                           Yes 
 

#Feature selection
#------------------
#Chi square test to do the feature selection for categorical variables
str(credit_card_applications_categorical_var)
credit_card_applications_categorical_var
mutate_each_(funs(factor(.)),categorical_variables)
str(credit_card_applications_categorical_var)
#Gender - check if it is significant based on p value from chi square test
table(credit_card_applications_categorical_var$Gender, credit_card_applications_categorical_var$Performance.Tag.x)
chisq.test(credit_card_applications_categorical_var$Gender, credit_card_applications_categorical_var$Performance.Tag.x, correct=FALSE)
#since p-value = 0.6113 > 0.05, we accept the null hypothesis that the variable Gender is insignificant 

#Marital.Status..at.the.time.of.application. - check if it is significant based on p value from chi square test
table(credit_card_applications_categorical_var$Marital.Status..at.the.time.of.application., credit_card_applications_categorical_var$Performance.Tag.x)
chisq.test(credit_card_applications_categorical_var$Marital.Status..at.the.time.of.application., credit_card_applications_categorical_var$Performance.Tag.x, correct=FALSE)
#since p-value = 0.7715 > 0.05, we accept the null hypothesis that the variable Marital.Status is insignificant

#No.of.dependents - check if it is significant based on p value from chi square test
table(credit_card_applications_categorical_var$No.of.dependents, credit_card_applications_categorical_var$Performance.Tag.x)
chisq.test(credit_card_applications_categorical_var$No.of.dependents, credit_card_applications_categorical_var$Performance.Tag.x, correct=FALSE)
#since p-value = 0.1826 > 0.05, we accept the null hypothesis that the variable No.of.dependents is insignificant 

#Education - check if it is significant based on p value from chi square test
table(credit_card_applications_categorical_var$Education, credit_card_applications_categorical_var$Performance.Tag.x)
chisq.test(credit_card_applications_categorical_var$Education, credit_card_applications_categorical_var$Performance.Tag.x, correct=FALSE)
#since p-value = 0.8104 > 0.05, we accept the null hypothesis that the variable Education is insignificant 

#Profession - check if it is significant based on p value from chi square test
table(credit_card_applications_categorical_var$Profession, credit_card_applications_categorical_var$Performance.Tag.x)
chisq.test(credit_card_applications_categorical_var$Profession, credit_card_applications_categorical_var$Performance.Tag.x, correct=FALSE)
#since p-value = 0.06038 > 0.05, we accept the null hypothesis that the variable Profession is insignificant 

#Type.of.residence - check if it is significant based on p value from chi square test
table(credit_card_applications_categorical_var$Type.of.residence, credit_card_applications_categorical_var$Performance.Tag.x)
chisq.test(credit_card_applications_categorical_var$Type.of.residence, credit_card_applications_categorical_var$Performance.Tag.x, correct=FALSE)
#since p-value = p-value = 0.7655 > 0.05 , we accept the null hypothesis that the variable Type.of.residence is insignificant 

#Presence.of.open.home.loan - check if it is significant based on p value from chi square test
table(credit_card_applications_categorical_var$Presence.of.open.home.loan, credit_card_applications_categorical_var$Performance.Tag.x)
chisq.test(credit_card_applications_categorical_var$Presence.of.open.home.loan, credit_card_applications_categorical_var$Performance.Tag.x, correct=FALSE)
#since p-value = 1.716e-11 < 0.05, we reject the null hypothesis and conclude that variable  'Presence.of.open.home.loan' is significant 

#Presence.of.open.auto.loan - check if it is significant based on p value from chi square test
table(credit_card_applications_categorical_var$Presence.of.open.auto.loan, credit_card_applications_categorical_var$Performance.Tag.x)
chisq.test(credit_card_applications_categorical_var$Presence.of.open.auto.loan, credit_card_applications_categorical_var$Performance.Tag.x, correct=FALSE)
#since p-value = 0.03454 < 0.05, we reject the null hypothesis and conclude that variable  'Presence.of.open.auto.loan' is significant 

#No.of.times.90.DPD.or.worse.in.last.6.months - check if it is significant based on p value from chi square test
table(credit_card_applications_categorical_var$No.of.times.90.DPD.or.worse.in.last.6.months, credit_card_applications_categorical_var$Performance.Tag.x)
chisq.test(credit_card_applications_categorical_var$No.of.times.90.DPD.or.worse.in.last.6.months, credit_card_applications_categorical_var$Performance.Tag.x, correct=FALSE)
#since p-value = p-value < 2.2e-16, we reject the null hypothesis and conclude that variable  'No.of.times.90.DPD.or.worse.in.last.6.months' is significant 


#No.of.times.90.DPD.or.worse.in.last.12.months - check if it is significant based on p value from chi square test
table(credit_card_applications_categorical_var$No.of.times.90.DPD.or.worse.in.last.12.months, credit_card_applications_categorical_var$Performance.Tag.x)
chisq.test(credit_card_applications_categorical_var$No.of.times.90.DPD.or.worse.in.last.12.months, credit_card_applications_categorical_var$Performance.Tag.x, correct=FALSE)
#since p-value = p-value < 2.2e-16, we reject the null hypothesis and conclude that variable  'No.of.times.90.DPD.or.worse.in.last.12.months' is significant 

#No.of.times.60.DPD.or.worse.in.last.6.months - check if it is significant based on p value from chi square test
table(credit_card_applications_categorical_var$No.of.times.60.DPD.or.worse.in.last.6.months, credit_card_applications_categorical_var$Performance.Tag.x)
chisq.test(credit_card_applications_categorical_var$No.of.times.60.DPD.or.worse.in.last.6.months, credit_card_applications_categorical_var$Performance.Tag.x, correct=FALSE)
#since p-value = p-value < 2.2e-16, we reject the null hypothesis and conclude that variable  'No.of.times.60.DPD.or.worse.in.last.6.months' is significant 


#No.of.times.60.DPD.or.worse.in.last.12.months - check if it is significant based on p value from chi square test
table(credit_card_applications_categorical_var$No.of.times.60.DPD.or.worse.in.last.12.months, credit_card_applications_categorical_var$Performance.Tag.x)
chisq.test(credit_card_applications_categorical_var$No.of.times.60.DPD.or.worse.in.last.12.months, credit_card_applications_categorical_var$Performance.Tag.x, correct=FALSE)
#since p-value = p-value < 2.2e-16, we reject the null hypothesis and conclude that variable  'No.of.times.60.DPD.or.worse.in.last.12.months' is significant 


#No.of.times.30.DPD.or.worse.in.last.6.months - check if it is significant based on p value from chi square test
table(credit_card_applications_categorical_var$No.of.times.30.DPD.or.worse.in.last.6.months, credit_card_applications_categorical_var$Performance.Tag.x)
chisq.test(credit_card_applications_categorical_var$No.of.times.30.DPD.or.worse.in.last.6.months, credit_card_applications_categorical_var$Performance.Tag.x, correct=FALSE)
#since p-value = p-value < 2.2e-16, we reject the null hypothesis and conclude that variable  'No.of.times.30.DPD.or.worse.in.last.6.months' is significant 


#No.of.times.30.DPD.or.worse.in.last.12.months - check if it is significant based on p value from chi square test
table(credit_card_applications_categorical_var$No.of.times.30.DPD.or.worse.in.last.12.months, credit_card_applications_categorical_var$Performance.Tag.x)
chisq.test(credit_card_applications_categorical_var$No.of.times.30.DPD.or.worse.in.last.12.months, credit_card_applications_categorical_var$Performance.Tag.x, correct=FALSE)
#since p-value = p-value < 2.2e-16, we reject the null hypothesis and conclude that variable  'No.of.times.30.DPD.or.worse.in.last.12.months' is significant 

#ALL the DPD  variables are significant for chi square test, now we have to multicollinaearity among them so that we can reduce the variable 
# which is function of others

#WOE to do the feature selection for continuous variables
#--------------------------------------------------------
colnames(credit_card_applications_numerical_var)
str(credit_card_applications_numerical_var)
credit_card_applications_numerical_var$Performance.Tag <- as.integer(credit_card_applications_numerical_var$Performance.Tag)
class(credit_card_eda$Performance.Tag)
IV <- create_infotables(data=credit_card_applications_numerical_var, y="Performance.Tag", bins=10, parallel=TRUE)
IV$Summary

knitr::kable(head(IV$Summary))

knitr::kable(IV$Tables$Avgas.CC.Utilization.in.last.12.months)
knitr::kable(IV$Tables$No.of.trades.opened.in.last.12.months)
knitr::kable(IV$Tables$No.of.PL.trades.opened.in.last.12.months)
knitr::kable(IV$Tables$No.of.Inquiries.in.last.12.months)

#--------------------------------------------------------------------
# Prepare data for modeling
#--------------------------------------------------------------------

# As we have many numeric variables and ranges varies for each so for batter impact 
#  identification scaling all numerical variables.
No.of.trades.opened.in.last.6.months <- scale(credit_card_eda$No.of.trades.opened.in.last.6.months)
No.of.PL.trades.opened.in.last.6.months <- scale(credit_card_eda$No.of.PL.trades.opened.in.last.6.months)
No.of.Inquiries.in.last.6.months <- scale(credit_card_eda$No.of.Inquiries.in.last.6.months)
Total.No.of.Trades <- scale(credit_card_eda$Total.No.of.Trades)
Income <- scale(credit_card_eda$Income)
No.of.months.in.current.residence <- scale(credit_card_eda$No.of.months.in.current.residence)
No.of.months.in.current.company <- scale(credit_card_eda$No.of.months.in.current.company)
Avgas.CC.Utilization.in.last.12.months <- scale(credit_card_eda$Avgas.CC.Utilization.in.last.12.months)       
No.of.trades.opened.in.last.12.months <- scale(credit_card_eda$No.of.trades.opened.in.last.12.months)
No.of.PL.trades.opened.in.last.12.months <- scale(credit_card_eda$No.of.PL.trades.opened.in.last.12.months)
No.of.Inquiries.in.last.12.months <- scale(credit_card_eda$No.of.Inquiries.in.last.12.months)
Outstanding.Balance <- scale(credit_card_eda$Outstanding.Balance)
No.of.times.30.DPD.or.worse.in.last.6.months <- scale(credit_card_eda$No.of.times.30.DPD.or.worse.in.last.6.months)
No.of.times.90.DPD.or.worse.in.last.6.months <- scale(credit_card_eda$No.of.times.90.DPD.or.worse.in.last.6.months)
No.of.times.60.DPD.or.worse.in.last.6.months <- scale(credit_card_eda$No.of.times.60.DPD.or.worse.in.last.6.months) 
No.of.times.30.DPD.or.worse.in.last.12.months <- scale(credit_card_eda$No.of.times.30.DPD.or.worse.in.last.12.months)
No.of.times.90.DPD.or.worse.in.last.12.months <- scale(credit_card_eda$No.of.times.90.DPD.or.worse.in.last.12.months)
No.of.times.60.DPD.or.worse.in.last.12.months <- scale(credit_card_eda$No.of.times.60.DPD.or.worse.in.last.12.months)

credit_card_num_vars <- data.frame(No.of.trades.opened.in.last.6.months, No.of.PL.trades.opened.in.last.6.months,
                                  No.of.Inquiries.in.last.6.months,No.of.Inquiries.in.last.6.months,
                                  Total.No.of.Trades, Income, No.of.months.in.current.residence, No.of.months.in.current.company,
                                  Avgas.CC.Utilization.in.last.12.months,
                                  No.of.trades.opened.in.last.12.months, Avgas.CC.Utilization.in.last.12.months,
                                  No.of.trades.opened.in.last.12.months, No.of.PL.trades.opened.in.last.12.months,
                                  No.of.Inquiries.in.last.12.months, Outstanding.Balance,
                                  No.of.times.30.DPD.or.worse.in.last.6.months, No.of.times.90.DPD.or.worse.in.last.6.months,
                                  No.of.times.60.DPD.or.worse.in.last.6.months, No.of.times.30.DPD.or.worse.in.last.12.months,
                                  No.of.times.90.DPD.or.worse.in.last.12.months, No.of.times.60.DPD.or.worse.in.last.12.months)

# We have following categorical variables
Gender <- as.factor(credit_card_eda$Gender)                         
No.of.dependents <- as.factor(credit_card_eda$No.of.dependents)
Education  <- as.factor(credit_card_eda$Education)
Type.of.residence <- as.factor(credit_card_eda$Type.of.residence)
Presence.of.open.home.loan <- as.factor(credit_card_eda$Presence.of.open.home.loan)
AgeCategory <- as.factor(credit_card_eda$AgeCategory)
Marital.Status <- as.factor(credit_card_eda$Marital.Status)
Profession <- as.factor(credit_card_eda$Profession)
Presence.of.open.auto.loan <- as.factor(credit_card_eda$Presence.of.open.auto.loan)

credit_card_fact <- data.frame(Gender, No.of.dependents, Education, Type.of.residence,
                               Presence.of.open.home.loan, AgeCategory,
                               Marital.Status, Profession, Presence.of.open.auto.loan)
credit_card_final<- cbind(credit_card_num_vars,credit_card_fact)
credit_card_final$Performance.Tag <- credit_card_eda$Performance.Tag

#IV calculation, WOE analysis and imputation if required
colnames(credit_card_eda)

data1_credit_card_eda <- credit_card_eda[, -which(names(credit_card_eda) %in% c("IncomeRange", "Residence.Years", "Company.Years", "x_Avgas.CC.Utilization.in.last.12.months" ))]

data1_credit_card_eda$Performance.Tag <- as.numeric(levels(data1_credit_card_eda$Performance.Tag))[data1_credit_card_eda$Performance.Tag]
colnames(data1_credit_card_eda)

IV <- create_infotables(data=data1_credit_card_eda, y="Performance.Tag", bins=10, parallel=TRUE)

IV$Summary

plot_infotables(IV, IV$Summary$Variable[1:4], same_scales=TRUE)

#1. Avgas.CC.Utilization.in.last.12.months variable WOE analysis and mutation if required
print(IV$Tables$Avgas.CC.Utilization.in.last.12.months, row.names=FALSE)

# Avgas.CC.Utilization.in.last.12.months    N    Percent         WOE         IV
# [0,4]                                     5493 0.07923777 -0.80842811 0.03636934
# [5,6]                                     5435 0.07840111 -0.79760307 0.07155488
# [7,8]                                     6828 0.09849545 -0.79889041 0.11587769
# [9,11]                                    9560 0.13790517 -0.67128981 0.16209784
# [12,14]                                   6575 0.09484587 -0.46771427 0.17893561
# [15,22]                                   7416 0.10697748 -0.06395137 0.17936054
# [23,36]                                   6837 0.09862528  0.46472310 0.20581255
# [37,51]                                   7159 0.10327020  0.58331176 0.25197321
# [52,71]                                   7000 0.10097659  0.56351111 0.29370218
# [72,113]                                  7020 0.10126509  0.38040064 0.31119019


data1_credit_card_eda[(data1_credit_card_eda$Avgas.CC.Utilization.in.last.12.months <= 8),]$Avgas.CC.Utilization.in.last.12.months <- -0.79760307  
data1_credit_card_eda[(data1_credit_card_eda$Avgas.CC.Utilization.in.last.12.months >= 9)
                      & (data1_credit_card_eda$Avgas.CC.Utilization.in.last.12.months <= 11),]$Avgas.CC.Utilization.in.last.12.months <- -0.67128981                    
data1_credit_card_eda[(data1_credit_card_eda$Avgas.CC.Utilization.in.last.12.months >= 12)
                      & (data1_credit_card_eda$Avgas.CC.Utilization.in.last.12.months <= 14),]$Avgas.CC.Utilization.in.last.12.months <- -0.46771427                     
data1_credit_card_eda[(data1_credit_card_eda$Avgas.CC.Utilization.in.last.12.months >= 15)
                      & (data1_credit_card_eda$Avgas.CC.Utilization.in.last.12.months <= 22),]$Avgas.CC.Utilization.in.last.12.months <- -0.06395137                     
data1_credit_card_eda[(data1_credit_card_eda$Avgas.CC.Utilization.in.last.12.months >= 23)
                      & (data1_credit_card_eda$Avgas.CC.Utilization.in.last.12.months <= 36),]$Avgas.CC.Utilization.in.last.12.months <- 0.46472310                     
data1_credit_card_eda[(data1_credit_card_eda$Avgas.CC.Utilization.in.last.12.months >= 23)
                      & (data1_credit_card_eda$Avgas.CC.Utilization.in.last.12.months <= 36),]$Avgas.CC.Utilization.in.last.12.months <- 0.46472310                     
data1_credit_card_eda[(data1_credit_card_eda$Avgas.CC.Utilization.in.last.12.months >= 37)
                      & (data1_credit_card_eda$Avgas.CC.Utilization.in.last.12.months <= 71),]$Avgas.CC.Utilization.in.last.12.months <- 0.58331176                     
data1_credit_card_eda[(data1_credit_card_eda$Avgas.CC.Utilization.in.last.12.months >= 72)
                      & (data1_credit_card_eda$Avgas.CC.Utilization.in.last.12.months <= 113),]$Avgas.CC.Utilization.in.last.12.months <- 0.38040064                     

#2. No.of.trades.opened.in.last.12.months variable WOE analysis and mutation if required
print(IV$Tables$No.of.trades.opened.in.last.12.months, row.names=FALSE)
# No.of.trades.opened.in.last.12.months     N    Percent          WOE         IV
# [0,0]                                   4659  0.06720713 -0.669594615 0.02242751
# [1,1]                                   11311 0.16316374 -1.027487864 0.13307894
# [2,2]                                   9262  0.13360645 -0.812609632 0.19493183
# [3,3]                                   4660  0.06722156  0.004791377 0.19493337
# [4,5]                                   9372  0.13519323  0.109255835 0.19663032
# [6,7]                                   8268  0.11926778  0.444887382 0.22567276
# [8,9]                                   7162  0.10331347  0.570461657 0.26957180
# [10,12]                                 6677  0.09631724  0.487698868 0.29833324
# [13,28]                                 7952  0.11470940  0.002084386 0.29833374

data1_credit_card_eda[(data1_credit_card_eda$No.of.trades.opened.in.last.12.months == 0),]$No.of.trades.opened.in.last.12.months <- -0.669594615                                             
data1_credit_card_eda[(data1_credit_card_eda$No.of.trades.opened.in.last.12.months == 1),]$No.of.trades.opened.in.last.12.months <- -1.027487864
data1_credit_card_eda[(data1_credit_card_eda$No.of.trades.opened.in.last.12.months == 2),]$No.of.trades.opened.in.last.12.months <- -0.812609632
data1_credit_card_eda[(data1_credit_card_eda$No.of.trades.opened.in.last.12.months == 3),]$No.of.trades.opened.in.last.12.months <- 0.004791377                                             
data1_credit_card_eda[(data1_credit_card_eda$No.of.trades.opened.in.last.12.months >= 4)
                      & (data1_credit_card_eda$No.of.trades.opened.in.last.12.months <= 5),]$No.of.trades.opened.in.last.12.months <- 0.109255835                    
data1_credit_card_eda[(data1_credit_card_eda$No.of.trades.opened.in.last.12.months >= 6)
                      & (data1_credit_card_eda$No.of.trades.opened.in.last.12.months <= 7),]$No.of.trades.opened.in.last.12.months <- 0.487698868                    
data1_credit_card_eda[(data1_credit_card_eda$No.of.trades.opened.in.last.12.months >= 8)
                      & (data1_credit_card_eda$No.of.trades.opened.in.last.12.months <= 9),]$No.of.trades.opened.in.last.12.months <- 0.570461657                    
data1_credit_card_eda[(data1_credit_card_eda$No.of.trades.opened.in.last.12.months >= 10)
                      & (data1_credit_card_eda$No.of.trades.opened.in.last.12.months <= 12),]$No.of.trades.opened.in.last.12.months <- 0.487698868                    
data1_credit_card_eda[(data1_credit_card_eda$No.of.trades.opened.in.last.12.months >= 13)
                      & (data1_credit_card_eda$No.of.trades.opened.in.last.12.months <= 28),]$No.of.trades.opened.in.last.12.months <- 0.002084386                    

#3. No.of.PL.trades.opened.in.last.12.months variable WOE analysis and mutation if required
print(IV$Tables$No.of.PL.trades.opened.in.last.12.months, row.names=FALSE)
# No.of.PL.trades.opened.in.last.12.months     N   Percent        WOE        IV
# [0,0]                                       25397 0.3663575 -0.9021938 0.2015034
# [1,1]                                       6624  0.0955527 -0.1311880 0.2030527
# [2,2]                                       6814  0.0982935  0.2509848 0.2100069
# [3,3]                                       8106  0.1169309  0.4105099 0.2338596
# [4,4]                                       7877  0.1136275  0.4987698 0.2695346
# [5,5]                                       6176  0.0890902  0.4200868 0.2886521
# [6,12]                                      8329  0.1201477  0.2406022 0.2964260
data1_credit_card_eda[(data1_credit_card_eda$No.of.PL.trades.opened.in.last.12.months == 0),]$No.of.PL.trades.opened.in.last.12.months <- -0.9021938                                             
data1_credit_card_eda[(data1_credit_card_eda$No.of.PL.trades.opened.in.last.12.months == 1),]$No.of.PL.trades.opened.in.last.12.months <- -0.1311880                                             
data1_credit_card_eda[(data1_credit_card_eda$No.of.PL.trades.opened.in.last.12.months == 2),]$No.of.PL.trades.opened.in.last.12.months <- 0.2509848                                             
data1_credit_card_eda[(data1_credit_card_eda$No.of.PL.trades.opened.in.last.12.months == 3),]$No.of.PL.trades.opened.in.last.12.months <- 0.4200868                                             
data1_credit_card_eda[(data1_credit_card_eda$No.of.PL.trades.opened.in.last.12.months == 4),]$No.of.PL.trades.opened.in.last.12.months <- 0.4987698                                             
data1_credit_card_eda[(data1_credit_card_eda$No.of.PL.trades.opened.in.last.12.months == 5),]$No.of.PL.trades.opened.in.last.12.months <- 0.4200868                                             
data1_credit_card_eda[(data1_credit_card_eda$No.of.PL.trades.opened.in.last.12.months >= 6)
                      & (data1_credit_card_eda$No.of.PL.trades.opened.in.last.12.months <= 12),]$No.of.PL.trades.opened.in.last.12.month <- 0.2509848                   

#4. No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. variable WOE analysis and mutation if required
print(IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., row.names=FALSE)

# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.     N    Percent          WOE        IV
# [0,0]                                                             20442 0.29488049 -1.070125700 0.2132596
# [1,1]                                                             3650 0.05265208  -0.036980894 0.2133304
# [2,2]                                                             7869 0.11351211  0.138857418 0.2156636
# [3,3]                                                             8932 0.12884613  0.164542324 0.2194270
# [4,4]                                                             7095 0.10234698  0.247948865 0.2264839
# [5,5]                                                             4910 0.07082786  0.583098241 0.2581168
# [6,8]                                                             8925 0.12874515  0.482618708 0.2956744
# [9,20]                                                            7500 0.10818920  0.009143055 0.2956835


data1_credit_card_eda[(data1_credit_card_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. == 0),]$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- -1.070125700                                            
data1_credit_card_eda[(data1_credit_card_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. == 1),]$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- -0.036980894                                           
data1_credit_card_eda[(data1_credit_card_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. == 2),]$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- 0.138857418                                           
data1_credit_card_eda[(data1_credit_card_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. == 3),]$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- 0.164542324                                          
data1_credit_card_eda[(data1_credit_card_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. == 4),]$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- 0.247948865                                         
data1_credit_card_eda[(data1_credit_card_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. == 5),]$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- 0.583098241                                         
data1_credit_card_eda[(data1_credit_card_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. >= 6)
                      & (data1_credit_card_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <= 8),]$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- 0.482618708                   
data1_credit_card_eda[(data1_credit_card_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. >= 9)
                      & (data1_credit_card_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <= 20),]$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- 0.009143055                   

#5. Outstanding.Balance variable WOE analysis and mutation if required
print(IV$Tables$Outstanding.Balance, row.names=FALSE)

# Outstanding.Balance                                 N    Percent        WOE             IV
# [0,6841]                                           6931 0.09998125      -0.7765175     0.04290355
# [6843,25636]                                       6933 0.10001010      -0.9107738    0.09876633
# [25637,387052]                                     6932 0.09999567      -0.1412201    0.10063656
# [387063,585483]                                    6932 0.09999567       0.2528617    0.10782375
# [585496,774228]                                    6933 0.10001010       0.4521857    0.13306931
# [774241,972229]                                    6932 0.09999567       0.4331688    0.15602558
# [972243,1356902]                                   6932 0.09999567       0.3988273    0.17517342
# [1356915,2960834]                                  6932 0.09999567      -0.3810991    0.18741359
# [2960840,3283579]                                  6933 0.10001010      -0.8221463    0.23461969
# [3283758,5218801]                                  6933 0.10001010       0.2971522    0.24475447
data1_credit_card_eda[(data1_credit_card_eda$Outstanding.Balance >= 0)
                      & (data1_credit_card_eda$Outstanding.Balance <= 6841),]$Outstanding.Balance <- -0.7765175                  
data1_credit_card_eda[(data1_credit_card_eda$Outstanding.Balance >= 6843)
                      & (data1_credit_card_eda$Outstanding.Balance <= 25636),]$Outstanding.Balance <- -0.9107738                 
data1_credit_card_eda[(data1_credit_card_eda$Outstanding.Balance >= 25637)
                      & (data1_credit_card_eda$Outstanding.Balance <= 387052),]$Outstanding.Balance <- -0.1412201                 
data1_credit_card_eda[(data1_credit_card_eda$Outstanding.Balance >= 387063)
                      & (data1_credit_card_eda$Outstanding.Balance <= 585483),]$Outstanding.Balance <- 0.2528617              
data1_credit_card_eda[(data1_credit_card_eda$Outstanding.Balance >= 585496)
                      & (data1_credit_card_eda$Outstanding.Balance <= 774228),]$Outstanding.Balance <- .4521857                
data1_credit_card_eda[(data1_credit_card_eda$Outstanding.Balance >= 774241)
                      & (data1_credit_card_eda$Outstanding.Balance <= 972229),]$Outstanding.Balance <- .4521857                
data1_credit_card_eda[(data1_credit_card_eda$Outstanding.Balance >= 972243)
                      & (data1_credit_card_eda$Outstanding.Balance <= 1356902),]$Outstanding.Balance <- 0.3988273               
data1_credit_card_eda[(data1_credit_card_eda$Outstanding.Balance >= 1356915)
                      & (data1_credit_card_eda$Outstanding.Balance <= 2960834),]$Outstanding.Balance <- -0.3810991              
data1_credit_card_eda[(data1_credit_card_eda$Outstanding.Balance >= 2960840)
                      & (data1_credit_card_eda$Outstanding.Balance <= 3283579),]$Outstanding.Balance <- -0.8221463             
data1_credit_card_eda[(data1_credit_card_eda$Outstanding.Balance >= 3283758)
                      & (data1_credit_card_eda$Outstanding.Balance <= 5218801),]$Outstanding.Balance <- 0.2971522             

#6. No.of.times.30.DPD.or.worse.in.last.6.months variable WOE analysis and mutation if required
print(IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months , row.names=FALSE)
# 
# No.of.times.30.DPD.or.worse.in.last.6.months     N          Percent        WOE              IV
# [0,0]                                          49611        0.7156499      -0.3888079     0.09087243
# [1,1]                                          9470         0.1366069       0.4648701     0.12753711
# [2,7]                                          10242        0.1477432       0.7403733     0.24218377

data1_credit_card_eda[(data1_credit_card_eda$No.of.times.30.DPD.or.worse.in.last.6.months == 0),]$No.of.times.30.DPD.or.worse.in.last.6.months <- -0.3888079                                           
data1_credit_card_eda[(data1_credit_card_eda$No.of.times.30.DPD.or.worse.in.last.6.months == 1),]$No.of.times.30.DPD.or.worse.in.last.6.months <- 0.4648701                                           
data1_credit_card_eda[(data1_credit_card_eda$No.of.times.30.DPD.or.worse.in.last.6.months>= 2)
                      & (data1_credit_card_eda$No.of.times.30.DPD.or.worse.in.last.6.months <= 7),]$No.of.times.30.DPD.or.worse.in.last.6.months <- 0.7403733            


#7. No.of.Trades variable WOE analysis and mutation if required
print(IV$Tables$Total.No.of.Trades , row.names=FALSE)
# 
# Total.No.of.Trades                                  N        Percent         WOE              IV
# [0,1]                                               3618    0.05219047     -0.69529646      0.01857657
# [2,2]                                               6724    0.09699523     -1.02386251      0.08398655
# [3,3]                                               8563    0.12352322     -0.70424032      0.12892215
# [4,4]                                               7452    0.10749679     -0.44545617      0.14640106
# [5,5]                                               5692    0.08210839     -0.04761427      0.14658320
# [6,6]                                               4953    0.07144815      0.12922060      0.14784937
# [7,8]                                               9332    0.13461622      0.37606696      0.17052397
# [9,10]                                              7117    0.10266434      0.54138176      0.20927456
# [11,19]                                             8449    0.12187874      0.42575485      0.23621054
# [20,44]                                             7423    0.10707846     -0.07201718      0.23674795

data1_credit_card_eda[(data1_credit_card_eda$Total.No.of.Trades>= 0)
                      & (data1_credit_card_eda$Total.No.of.Trades <= 1),]$Total.No.of.Trades <- -0.70424032            

data1_credit_card_eda[(data1_credit_card_eda$Total.No.of.Trades ==2),]$Total.No.of.Trades <- -1.02386251             
data1_credit_card_eda[(data1_credit_card_eda$Total.No.of.Trades == 3),]$Total.No.of.Trades <- -0.70424032            
data1_credit_card_eda[(data1_credit_card_eda$Total.No.of.Trades == 4),]$Total.No.of.Trades <- -0.44545617           
data1_credit_card_eda[(data1_credit_card_eda$Total.No.of.Trades == 5),]$Total.No.of.Trades <- -0.04761427
data1_credit_card_eda[(data1_credit_card_eda$Total.No.of.Trades == 6),]$Total.No.of.Trades <- 0.12922060
data1_credit_card_eda[(data1_credit_card_eda$Total.No.of.Trades>= 7)
                      & (data1_credit_card_eda$Total.No.of.Trades <= 8),]$Total.No.of.Trades <-  0.37606696            
data1_credit_card_eda[(data1_credit_card_eda$Total.No.of.Trades>= 9)
                      & (data1_credit_card_eda$Total.No.of.Trades <= 10),]$Total.No.of.Trades <-  0.54138176            
data1_credit_card_eda[(data1_credit_card_eda$Total.No.of.Trades>= 11)
                      & (data1_credit_card_eda$Total.No.of.Trades <= 19),]$Total.No.of.Trades <-  0.42575485            
data1_credit_card_eda[(data1_credit_card_eda$Total.No.of.Trades>= 20)
                      & (data1_credit_card_eda$Total.No.of.Trades <= 44),]$Total.No.of.Trades <- -0.07201718          

#  WE can aslo try replaing [0,1],[3,3],[20,44] with -0.07201718

#8. No.of.PL.trades.opened.in.last.6.months variable WOE analysis and mutation if required
print(IV$Tables$No.of.PL.trades.opened.in.last.6.months , row.names=FALSE)

# No.of.PL.trades.opened.in.last.6.months     N              Percent        WOE                 IV
# [0,0]                                      30639           0.4419745      -0.6521666       0.1409477
# [1,1]                                      13509           0.1948704       0.1963563       0.1491740
# [2,2]                                      12529           0.1807337       0.4386676       0.1918362
# [3,6]                                      12646           0.1824214       0.3574090       0.2193472

data1_credit_card_eda[(data1_credit_card_eda$No.of.PL.trades.opened.in.last.6.months == 0),]$No.of.PL.trades.opened.in.last.6.months <- -0.6521666             
data1_credit_card_eda[(data1_credit_card_eda$No.of.PL.trades.opened.in.last.6.months == 1),]$No.of.PL.trades.opened.in.last.6.months <- 0.1963563              
data1_credit_card_eda[(data1_credit_card_eda$No.of.PL.trades.opened.in.last.6.months == 2),]$No.of.PL.trades.opened.in.last.6.months <- 0.4386676              
data1_credit_card_eda[(data1_credit_card_eda$No.of.PL.trades.opened.in.last.6.months>= 3)
                      & (data1_credit_card_eda$No.of.PL.trades.opened.in.last.6.months <= 6),]$No.of.PL.trades.opened.in.last.6.months <- 0.3574090           

#9.No.of.times.90.DPD.or.worse.in.last.12.months  variable WOE analysis and mutation if required
print(IV$Tables$ No.of.times.90.DPD.or.worse.in.last.12.months,row.names=FALSE)

# No.of.times.90.DPD.or.worse.in.last.12.months     N          Percent        WOE           IV
# [0,0]                                           50001       0.7212758     -0.3583064    0.07883027
# [1,1]                                           11634       0.1678231      0.5086622    0.13388889
# [2,5]                                           7688        0.1109011      0.7191401    0.21426427

data1_credit_card_eda[(data1_credit_card_eda$No.of.times.90.DPD.or.worse.in.last.12.months == 0),]$No.of.times.90.DPD.or.worse.in.last.12.months <- -0.3583064            
data1_credit_card_eda[(data1_credit_card_eda$No.of.times.90.DPD.or.worse.in.last.12.months == 1),]$No.of.times.90.DPD.or.worse.in.last.12.months <- 0.5086622            
data1_credit_card_eda[(data1_credit_card_eda$No.of.times.90.DPD.or.worse.in.last.12.months>= 2)
                      & (data1_credit_card_eda$No.of.times.90.DPD.or.worse.in.last.12.months <= 5),]$No.of.times.90.DPD.or.worse.in.last.12.months <- 0.7191401           


#10.No.of.times.60.DPD.or.worse.in.last.6.months   variable WOE analysis and mutation if required
print(IV$Tables$ No.of.times.60.DPD.or.worse.in.last.6.months ,row.names=FALSE)

# No.of.times.60.DPD.or.worse.in.last.6.months     N     Percent              WOE         IV
# [0,0]                                           51379  0.7411537       -0.3379006   0.07269008
# [1,5]                                           17944 0.2588463         0.6213074   0.20634740

data1_credit_card_eda[(data1_credit_card_eda$No.of.times.60.DPD.or.worse.in.last.6.months == 0),]$No.of.times.60.DPD.or.worse.in.last.6.months <- -0.3379006           
data1_credit_card_eda[(data1_credit_card_eda$No.of.times.60.DPD.or.worse.in.last.6.months>= 1)
                      & (data1_credit_card_eda$No.of.times.60.DPD.or.worse.in.last.6.months <= 5),]$No.of.times.60.DPD.or.worse.in.last.6.months <- 0.6213074           

#11.No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.variable WOE analysis and mutation if required
print(IV$Tables$ No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,row.names=FALSE)

#11. No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.     N   Percent         WOE       IV
# [0,0]                                                            24682 0.3560434   -0.72079721   0.1347441
# [1,1]                                                            13110 0.1891147    0.17784390   0.1412370
# [2,2]                                                            12788 0.1844698    0.21203509   0.1503840
# [3,4]                                                            11471 0.1654718    0.50750241   0.2043942
# [5,10]                                                           7272  0.1049002    0.00822695   0.2044014

data1_credit_card_eda[(data1_credit_card_eda$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. == 0),]$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- -0.72079721          
data1_credit_card_eda[(data1_credit_card_eda$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. == 1),]$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- 0.17784390           
data1_credit_card_eda[(data1_credit_card_eda$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. == 2),]$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- 0.21203509          
data1_credit_card_eda[(data1_credit_card_eda$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.>= 3)
                      & (data1_credit_card_eda$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <= 4),]$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- 0.50750241          
data1_credit_card_eda[(data1_credit_card_eda$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.>= 5)
                      & (data1_credit_card_eda$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <= 10),]$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- 0.00822695          
#12. No.of.times.30.DPD.or.worse.in.last.12.monthsvariable WOE analysis and mutation if required
print(IV$Tables$ No.of.times.30.DPD.or.worse.in.last.12.months,row.names=FALSE)
# No.of.times.30.DPD.or.worse.in.last.12.months     N         Percent        WOE              IV
# [0,0]                                           44382       0.6402204      -0.3776992       0.07709045
# [1,2]                                           17541       0.2530329       0.2795787       0.09960293
# [3,9]                                           7400        0.1067467       0.7963014       0.19801187

data1_credit_card_eda[(data1_credit_card_eda$No.of.times.30.DPD.or.worse.in.last.12.months == 0),]$No.of.times.30.DPD.or.worse.in.last.12.months <- -0.3776992          
data1_credit_card_eda[(data1_credit_card_eda$No.of.times.30.DPD.or.worse.in.last.12.months>= 1)
                      & (data1_credit_card_eda$No.of.times.30.DPD.or.worse.in.last.12.months <= 2),]$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- 0.2795787          
data1_credit_card_eda[(data1_credit_card_eda$No.of.times.30.DPD.or.worse.in.last.12.months>= 3)
                      & (data1_credit_card_eda$No.of.times.30.DPD.or.worse.in.last.12.months<= 9),]$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- 0.7963014         

#13.  No.of.trades.opened.in.last.6.months variable WOE analysis and mutation if required
print(IV$Tables$No.of.trades.opened.in.last.6.months,row.names=FALSE)
# No.of.trades.opened.in.last.6.months     N        Percent            WOE             IV
# [0,0]                                  11859      0.1710688          -0.6702805     0.05718732
# [1,1]                                  20008      0.2886199          -0.4765630     0.11017857
# [2,2]                                  12079      0.1742423           0.2316003     0.12058095
# [3,3]                                  9382       0.1353375          0.4309928      0.15130778
# [4,4]                                  6283       0.0906337          0.5213678      0.18273513
# [5,12]                                 9712       0.1400978          0.1343678      0.18542600

colnames(data1_credit_card_eda)
summary(data1_credit_card_eda$No.of.trades.opened.in.last.6.months)
data1_credit_card_eda[(data1_credit_card_eda$No.of.trades.opened.in.last.6.months == 0),]$No.of.trades.opened.in.last.6.months <- -0.6702805        
data1_credit_card_eda[(data1_credit_card_eda$No.of.trades.opened.in.last.6.months == 1),]$No.of.trades.opened.in.last.6.months <- -0.4765630          
data1_credit_card_eda[(data1_credit_card_eda$No.of.trades.opened.in.last.6.months == 2),]$No.of.trades.opened.in.last.6.months <- 0.2316003          
data1_credit_card_eda[(data1_credit_card_eda$No.of.trades.opened.in.last.6.months == 3),]$No.of.trades.opened.in.last.6.months <- 0.4309928         
data1_credit_card_eda[(data1_credit_card_eda$No.of.trades.opened.in.last.6.months == 4),]$No.of.trades.opened.in.last.6.months <-  0.5213678        
data1_credit_card_eda[(data1_credit_card_eda$No.of.trades.opened.in.last.6.months>= 5)
                      & (data1_credit_card_eda$No.of.trades.opened.in.last.6.months<= 12),]$No.of.trades.opened.in.last.6.months <- 0.1343678          

# There is some issue here in this variable as this feild is showing only values =12 though i see number of values here 
# i am getting the below error 
#Error in `$<-.data.frame`(`*tmp*`, No.of.trades.opened.in.last.6.months,  : 
#                           replacement has 1 row, data has 0

#14.No.of.times.60.DPD.or.worse.in.last.12.months variable WOE analysis and mutation if required
print(IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months,row.names=FALSE)
# No.of.times.60.DPD.or.worse.in.last.12.months        N          Percent        WOE             IV
# [0,0]                                               45391       0.6547755      -0.3529942     0.06961871
# [1,1]                                               12778       0.1843255       0.2128601     0.07883339
# [2,7]                                               11154       0.1608990       0.6916731     0.18530510
data1_credit_card_eda[(data1_credit_card_eda$No.of.times.60.DPD.or.worse.in.last.12.months == 0),]$No.of.times.60.DPD.or.worse.in.last.12.months <- -0.3529942       
data1_credit_card_eda[(data1_credit_card_eda$No.of.times.60.DPD.or.worse.in.last.12.months == 1),]$No.of.times.60.DPD.or.worse.in.last.12.months <- -0.2128601      
data1_credit_card_eda[(data1_credit_card_eda$No.of.times.60.DPD.or.worse.in.last.12.months>= 2)
                      & (data1_credit_card_eda$No.of.times.60.DPD.or.worse.in.last.12.months<= 7),]$No.of.trades.opened.in.last.6.months <- 0.6916731          

# i am getting the below error here 
# Error in data1_credit_card_eda[(data1_credit_card_eda$No.of.times.60.DPD.or.worse.in.last.12.months >=  : 
#                                   cannot change value of locked binding for '*tmp*'
#15.No.of.times.90.DPD.or.worse.in.last.6.months variable WOE analysis and mutation if required
print(IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months,row.names=FALSE)
# No.of.times.90.DPD.or.worse.in.last.6.months     N      Percent          WOE            IV
# [0,0]                                          54164    0.781328       -0.2615304      0.04748669
# [1,3]                                          15159    0.218672        0.6208793      0.16022119

data1_credit_card_eda[(data1_credit_card_eda$No.of.times.90.DPD.or.worse.in.last.6.months == 0),]$No.of.times.90.DPD.or.worse.in.last.6.months <- -0.2615304      
data1_credit_card_eda[(data1_credit_card_eda$No.of.times.90.DPD.or.worse.in.last.6.months>= 1)
                      & (data1_credit_card_eda$No.of.times.90.DPD.or.worse.in.last.6.months<= 3),]$No.of.times.90.DPD.or.worse.in.last.6.months <- 0.6208793          

# i am getting the below error here 
# Error in data1_credit_card_eda[(data1_credit_card_eda$No.of.times.60.DPD.or.worse.in.last.12.months >=  : 
#                                   cannot change value of locked binding for '*tmp*'

#16.No.of.months.in.current.residence variable WOE analysis and mutation if required
print(IV$Tables$No.of.months.in.current.residence,row.names=FALSE)
# No.of.months.in.current.residence                 N            Percent         WOE           IV
# [6,9]                                          34373          0.49583832     -0.27278704    0.03262165
# [10,28]                                        6897           0.09949079      0.49517703    0.06335746
# [29,49]                                        7188           0.10368853      0.29634371    0.07380395
# [50,72]                                        6951           0.10026975      0.13676102    0.07580126
# [73,97]                                        6870           0.09910131      0.13962510    0.07786158
# [98,126]                                       7044           0.10161130      -0.07216751   0.07837365
data1_credit_card_eda[(data1_credit_card_eda$No.of.months.in.current.residence>= 6)
                      & (data1_credit_card_eda$No.of.months.in.current.residence<= 9),]$No.of.months.in.current.residence <- -0.27278704          
data1_credit_card_eda[(data1_credit_card_eda$No.of.months.in.current.residence>= 10)
                      & (data1_credit_card_eda$No.of.months.in.current.residence<= 28),]$No.of.months.in.current.residence <- 0.49517703         
data1_credit_card_eda[(data1_credit_card_eda$No.of.months.in.current.residence>= 29)
                      & (data1_credit_card_eda$No.of.months.in.current.residence<= 49),]$No.of.months.in.current.residence <- 0.29634371        
data1_credit_card_eda[(data1_credit_card_eda$No.of.months.in.current.residence>= 50)
                      & (data1_credit_card_eda$No.of.months.in.current.residence<= 72),]$No.of.months.in.current.residence <- 0.13962510       
data1_credit_card_eda[(data1_credit_card_eda$No.of.months.in.current.residence>= 73)
                      & (data1_credit_card_eda$No.of.months.in.current.residence<= 97),]$No.of.months.in.current.residence <- 0.13962510       
data1_credit_card_eda[(data1_credit_card_eda$No.of.months.in.current.residence>= 98)
                      & (data1_credit_card_eda$No.of.months.in.current.residence<= 126),]$No.of.months.in.current.residence <- -0.07216751       

##17. Income variable WOE analysis and mutation if required
print(IV$Tables$ Income,row.names=FALSE)
# 
# Income                         N    Percent          WOE            IV
# [1,5]                         6183 0.08919118     0.312564105      0.01007282
# [6,10]                        6480 0.09347547     0.277805051      0.01827738
# [11,16]                       7871 0.11354096     0.067205150      0.01880626
# [17,21]                       6765 0.09758666     0.083838715      0.01951913
# [22,26]                       6776 0.09774534     0.008747956      0.01952664
# [27,31]                       6779 0.09778861     0.081672203      0.02020386
# [32,36]                       6786 0.09788959     -0.160484187     0.02254782
# [37,41]                       6686 0.09644707     -0.260808691     0.02837914
# [42,48]                       7738 0.11162241     -0.177301167     0.03161685
# [49,60]                       7259 0.10471272     -0.359873425     0.04315356
data1_credit_card_eda[(data1_credit_card_eda$Income >= 1)
                      &(data1_credit_card_eda$Income <= 5),]$Income <- 0.312564105                     
data1_credit_card_eda[(data1_credit_card_eda$Income >= 6)
                      &(data1_credit_card_eda$Income <= 10),]$Income <- 0.277805051
data1_credit_card_eda[(data1_credit_card_eda$Income >= 11)
                      &(data1_credit_card_eda$Income <= 16),]$Income <- 0.067205150
data1_credit_card_eda[(data1_credit_card_eda$Income >= 17)
                      &(data1_credit_card_eda$Income <= 21),]$Income <- 0.083838715
data1_credit_card_eda[(data1_credit_card_eda$Income >= 22)
                      &(data1_credit_card_eda$Income <= 26),]$Income <- 0.008747956
data1_credit_card_eda[(data1_credit_card_eda$Income >= 27)
                      &(data1_credit_card_eda$Income <= 31),]$Income <- 0.083838715
data1_credit_card_eda[(data1_credit_card_eda$Income >= 32)
                      &(data1_credit_card_eda$Income <= 36),]$Income <- -0.160484187 
data1_credit_card_eda[(data1_credit_card_eda$Income >= 37)
                      &(data1_credit_card_eda$Income <= 41),]$Income <- 0.260808691
data1_credit_card_eda[(data1_credit_card_eda$Income >= 42)
                      &(data1_credit_card_eda$Income <= 48),]$Income <- -0.177301167
data1_credit_card_eda[(data1_credit_card_eda$Income >= 49)
                      &(data1_credit_card_eda$Income <= 60),]$Income <- -0.359873425

# we can see if we can merge [32,36] and [42,48]
# i am getting the below error in this code above 
#Error: unexpected '&' in "&"

##18. No.of.months.in.current.company variable WOE analysis and mutation if required
print(IV$Tables$ No.of.months.in.current.company,row.names=FALSE)

# No.of.months.in.current.company                         N    Percent         WOE          IV
# [3,5]                                                   6630 0.09563925  0.10158530 0.001034151
# [6,12]                                                  6756 0.09745683  0.17606079 0.004310632
# [13,19]                                                 6880 0.09924556  0.20859828 0.009065916
# [20,26]                                                 6875 0.09917343  0.03957811 0.009224109
# [27,33]                                                 7056 0.10178440 -0.08523204 0.009935345
# [34,40]                                                 7127 0.10280859  0.02250839 0.009987971
# [41,47]                                                 7168 0.10340003 -0.17191682 0.012814619
# [48,53]                                                 6086 0.08779193 -0.22155705 0.016712862
# [54,61]                                                 7742 0.11168011 -0.21998037 0.021604939
# [62,133]                                                 7003 0.10101986  0.06437921 0.022036193

data1_credit_card_eda[(data1_credit_card_eda$No.of.months.in.current.company  >= 3)
                      &(data1_credit_card_eda$No.of.months.in.current.company  <= 5),]$No.of.months.in.current.company  <- 0.10158530
data1_credit_card_eda[(data1_credit_card_eda$No.of.months.in.current.company  >= 6)
                      &(data1_credit_card_eda$No.of.months.in.current.company  <= 12),]$No.of.months.in.current.company  <- 0.17606079
data1_credit_card_eda[(data1_credit_card_eda$No.of.months.in.current.company  >= 13)
                      &(data1_credit_card_eda$No.of.months.in.current.company  <= 19),]$No.of.months.in.current.company  <- 0.20859828
data1_credit_card_eda[(data1_credit_card_eda$No.of.months.in.current.company  >= 20)
                      &(data1_credit_card_eda$No.of.months.in.current.company  <= 26),]$No.of.months.in.current.company  <- 0.03957811
data1_credit_card_eda[(data1_credit_card_eda$No.of.months.in.current.company  >= 27)
                      &(data1_credit_card_eda$No.of.months.in.current.company  <= 33),]$No.of.months.in.current.company  <- -0.08523204
data1_credit_card_eda[(data1_credit_card_eda$No.of.months.in.current.company  >= 34)
                      &(data1_credit_card_eda$No.of.months.in.current.company  <= 40),]$No.of.months.in.current.company  <- 0.02250839
data1_credit_card_eda[(data1_credit_card_eda$No.of.months.in.current.company  >= 41)
                      &(data1_credit_card_eda$No.of.months.in.current.company  <= 47),]$No.of.months.in.current.company  <- -0.17191682
data1_credit_card_eda[(data1_credit_card_eda$No.of.months.in.current.company  >= 48)
                      &(data1_credit_card_eda$No.of.months.in.current.company  <= 53),]$No.of.months.in.current.company  <- -0.22155705
data1_credit_card_eda[(data1_credit_card_eda$No.of.months.in.current.company  >= 54)
                      &(data1_credit_card_eda$No.of.months.in.current.company  <= 61),]$No.of.months.in.current.company  <- -0.22155705
data1_credit_card_eda[(data1_credit_card_eda$No.of.months.in.current.company  >= 62)
                      &(data1_credit_card_eda$No.of.months.in.current.company  <= 133),]$No.of.months.in.current.company  <- 0.06437921
##19. Presence.of.open.home.loan WOE analysis and mutation if required
print(IV$Tables$ Presence.of.open.home.loan,row.names=FALSE)

# Presence.of.open.home.loan                        N   Percent         WOE          IV
# [0,0]                                            51344 0.7406488  0.07138878 0.003900401
# [1,1]                                            17979 0.2593512 -0.23420948 0.016696682

data1_credit_card_eda[(data1_credit_card_eda$Presence.of.open.home.loan == 0),]$Presence.of.open.home.loan <- 0.07138878      
data1_credit_card_eda[(data1_credit_card_eda$Presence.of.open.home.loan == 1),]$Presence.of.open.home.loan <- -0.23420948     

##20. No.of.dependents WOE analysis and mutation if required
print(IV$Tables$ No.of.dependents,row.names=FALSE)

# No.of.dependents                        N            Percent          WOE           IV
# [1,1]                                  15101        0.2178354  0.043755300 0.0004255052
# [2,2]                                  15012        0.2165515 -0.083590902 0.0018820695
# [3,3]                                  15519        0.2238651  0.056769045 0.0026225668
# [4,4]                                  11909        0.1717900 -0.028751801 0.0027627257
# [5,5]                                  11782        0.1699580 -0.002762942 0.0027640215


data1_credit_card_eda[(data1_credit_card_eda$No.of.dependents == 1),]$No.of.dependents <- 0.043755300     
data1_credit_card_eda[(data1_credit_card_eda$No.of.dependents == 2),]$No.of.dependents <- -0.083590902     
data1_credit_card_eda[(data1_credit_card_eda$No.of.dependents == 3),]$No.of.dependents <- 0.056769045     
data1_credit_card_eda[(data1_credit_card_eda$No.of.dependents == 4),]$No.of.dependents <- -0.028751801     
data1_credit_card_eda[(data1_credit_card_eda$No.of.dependents == 5),]$No.of.dependents <- -0.002762942     

##21.   Profession WOE analysis and mutation if required
print(IV$Tables$  Profession,row.names=FALSE)

# Profession                  N             Percent         WOE           IV
# SAL                       39372           0.567950      -0.02680598    0.0004031361
# SE                        13835           0.199573       0.08717894    0.0019819216
# SE_PROF                   16116           0.232477       -0.01328905 0.0020227280

data1_credit_card_eda[(data1_credit_card_eda$Profession == "SAL"),]$Profession <- -0.02680598 
data1_credit_card_eda[(data1_credit_card_eda$Profession == "SE" ),]$Profession <- 0.08717894    
data1_credit_card_eda[(data1_credit_card_eda$Profession == "SE_PROF" ),]$Profession <- 0.232477    
data1_credit_card_eda$Profession <- as.numeric(data1_credit_card_eda$Profession)
# getting this error for all the below variables 
#Error in `[.data.frame`(`*tmp*`, (data1_credit_card_eda$Profession ==  : 
#object 'SE_PROF' not found#

##22.Presence.of.open.auto.loan  WOE analysis and mutation if required
print(IV$Tables$Presence.of.open.auto.loan ,row.names=FALSE)
# 
# Presence.of.open.auto.loan                   N    Percent         WOE           IV
# [0,0]                                     63425 0.91492001  0.01179038     0.0001278745
# [1,1]                                     5898  0.08507999 -0.13558199      0.0015983507

data1_credit_card_eda[(data1_credit_card_eda$Presence.of.open.auto.loan == 0),]$Presence.of.open.auto.loan <- 0.01179038  
data1_credit_card_eda[(data1_credit_card_eda$Presence.of.open.auto.loan == 1 ),]$Presence.of.open.auto.loan <- -0.13558199 


##23.AgeCategory  WOE analysis and mutation if required
print(IV$Tables$AgeCategory ,row.names=FALSE)

# AgeCategory                       N     Percent          WOE           IV
# Adults                           64235 0.926604446     0.003911577     1.420286e-05
# Seniors                          4795  0.069168963    -0.025015444      5.699471e-05
# Youth                            293  0.004226591     -0.590204941      1.190403e-03

data1_credit_card_eda[(data1_credit_card_eda$AgeCategory  ==Adults),]$AgeCategory <- 0.003911577 
data1_credit_card_eda[(data1_credit_card_eda$AgeCategory == Seniors),]$AgeCategory <- -0.025015444 
data1_credit_card_eda[(data1_credit_card_eda$AgeCategory == Youth ),]$AgeCategory <- -0.590204941
data1_credit_card_eda$AgeCategory <- as.numeric(data1_credit_card_eda$AgeCategory)

##24.Type.of.residence WOE analysis and mutation if required
print(IV$Tables$Type.of.residence ,row.names=FALSE)
# 
# Type.of.residence                          N       Percent           WOE               IV
# Company provided                          1589     0.02292169       0.0720528608      0.0001230039
# Living with Parents                       1758     0.02535955       0.0765454687      0.0002769077
# Others                                    197      0.00284177      -0.5281807385      0.0009036669
# Owned                                     13889    0.20035198      -0.0007681379      0.0009037850
# Rented                                    51890    0.74852502      -0.0031805681      0.0009113461

#  Looks like we can combine company provided and Living with parents on the basis of  WOE values
data1_credit_card_eda[(data1_credit_card_eda$Type.of.residence == "Company provided" ),]$Type.of.residence <-0.0765454687
data1_credit_card_eda[(data1_credit_card_eda$Type.of.residence == "Living with Parents"  ),]$Type.of.residence <-0.0765454687
data1_credit_card_eda[(data1_credit_card_eda$Type.of.residence == "Others"   ),]$Type.of.residence <- -0.5281807385
data1_credit_card_eda[(data1_credit_card_eda$Type.of.residence == "Owned"   ),]$Type.of.residence <- -0.0007681379
data1_credit_card_eda[(data1_credit_card_eda$Type.of.residence == "Rented"    ),]$Type.of.residence <- -0.0031805681
data1_credit_card_eda$Type.of.residence <- as.numeric(data1_credit_card_eda$Type.of.residence)

##25.Education WOE analysis and mutation if required
print(IV$Tables$Education ,row.names=FALSE)

# Education                     N     Percent          WOE            IV
# Bachelor                    17216  0.248344705       0.015453894    5.973170e-05
# Masters                     23333  0.336583818       0.007524619    7.885481e-05
# Others                      117    0.001687752       0.507970381    6.308785e-04
# Phd                         4438   0.064019157       -0.026487167   6.752519e-04
# Professional                24219  0.349364569       -0.016781413    7.728863e-04


data1_credit_card_eda[(data1_credit_card_eda$Education == "Bachelor"),]$Education <- 0.015453894
data1_credit_card_eda[(data1_credit_card_eda$Education == "Masters"),]$Education <- 0.007524619
data1_credit_card_eda[(data1_credit_card_eda$Education == "Others"),]$Education <- 0.507970381
data1_credit_card_eda[(data1_credit_card_eda$Education == "Phd" ),]$Education <- -0.026487167
data1_credit_card_eda[(data1_credit_card_eda$Education == "Professional"),]$Education <- -0.016781413
data1_credit_card_eda$Education <- as.numeric(data1_credit_card_eda$Education)
# GETTING the below error for all categories 

##26.Gender WOE analysis and mutation if required
print(IV$Tables$Gender ,row.names=FALSE)
# Gender                          N   Percent         WOE          IV
# F                             16393 0.2364727  0.03360694 0.000271225
# M                             52930 0.7635273 -0.01062152 0.000356946

data1_credit_card_eda[(data1_credit_card_eda$Gender == "F"),]$Gender <- 0.03360694
data1_credit_card_eda[(data1_credit_card_eda$Gender == "M"),]$Gender <- -0.01062152
data1_credit_card_eda$Gender <- as.numeric(data1_credit_card_eda$Gender)

##27.Gender Marital.Status analysis and mutation if required
print(IV$Tables$Marital.Status ,row.names=FALSE)
# 
# Marital.Status                 N   Percent          WOE           IV
# Married                     59085 0.8523145 -0.004180567       1.486755e-05
# Single                      10238 0.1476855  0.023819161       9.957677e-05
data1_credit_card_eda[(data1_credit_card_eda$Marital.Status   == "Married" ),]$Marital.Status   <- -0.004180567 
data1_credit_card_eda[(data1_credit_card_eda$Marital.Status   == "Single" ),]$Marital.Status   <- 0.023819161 
data1_credit_card_eda$Marital.Status <- as.numeric(data1_credit_card_eda$Marital.Status)
colnames(data1_credit_card_eda)

#--------------------------------------------------------------------
# 4. Data Modeling - Logistic regression
#--------------------------------------------------------------------

# splitting the data between train and test
#dummies<- data.frame(sapply(credit_card_fact, 
#                            function(x) data.frame(model.matrix(~x-1,data =credit_card_fact))[,-1]))
colnames(data1_credit_card_eda)
#credit_card_regression <- cbind(credit_card_num_vars,dummies)
credit_card_regression <- data1_credit_card_eda
credit_card_regression$Performance.Tag <- as.factor(data1_credit_card_eda$Performance.Tag)

set.seed(100)

indices = sample.split(credit_card_regression$Performance.Tag, SplitRatio = 0.7)

train = credit_card_regression[indices,]

test = credit_card_regression[!(indices),]

model_1 = glm(Performance.Tag ~ ., data = train, family = "binomial")
summary(model_1)
# Stepwise selection
#model_2<- stepAIC(model_1, direction="both")
#summary(model_2)
# Removing multicollinearity through VIF check
model_3 <- glm(Performance.Tag ~ Gender + No.of.dependents + No.of.months.in.current.company + 
                 No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                 No.of.trades.opened.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance, family = "binomial", data = train)
summary(model_3)
vif(model_3)

# Remove No.of.depenNo.of.Inquiries.in.last.6.months..excluding.home...auto.loans.dents.x2
model_4 <- glm(Performance.Tag ~ Gender + No.of.dependents + No.of.months.in.current.company + 
                 No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                 No.of.trades.opened.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance, family = "binomial", data = train)
summary(model_4)
vif(model_4)

#Remove Outstanding.Balance
model_5 <- glm(Performance.Tag ~ Gender + No.of.dependents + No.of.months.in.current.company + 
                 No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                 No.of.trades.opened.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 
                 , family = "binomial", data = train)
summary(model_5)
vif(model_5)

# Remove Gender
model_6 <- glm(Performance.Tag ~ No.of.dependents + No.of.months.in.current.company + 
                 No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                 No.of.trades.opened.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 
               , family = "binomial", data = train)
summary(model_6)
vif(model_6)

# Remove No.of.months.in.current.company 
model_7 <- glm(Performance.Tag ~ No.of.dependents + 
                 No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                 No.of.trades.opened.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 
               , family = "binomial", data = train)
summary(model_7)
vif(model_7)

#Remove No.of.dependents
model_8 <- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.12.months + 
                 Avgas.CC.Utilization.in.last.12.months + 
                 No.of.trades.opened.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 
               , family = "binomial", data = train)
summary(model_8)
vif(model_8)

# Remove No.of.PL.trades.opened.in.last.12.months
model_9 <- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.12.months + 
                 Avgas.CC.Utilization.in.last.12.months + 
                 No.of.trades.opened.in.last.12.months +  
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 
               , family = "binomial", data = train)
summary(model_9)
vif(model_9)

#Remove No.of.trades.opened.in.last.12.months
model_10 <- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.12.months + 
                  Avgas.CC.Utilization.in.last.12.months + 
                  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 
                , family = "binomial", data = train)
summary(model_10)
vif(model_10)


final_model<- model_10
summary(final_model)

### Model Evaluation
### Test Data ####

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-42])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
#View(test)

# Let's use the probability cutoff of 50%.

test_pred_default <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_default <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))
table(test_actual_default,test_pred_default)

test_pred_default <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_default, test_actual_default, positive = "Yes")
test_conf

# Let's Choose the cutoff value. 
# 
# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_default, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.001428351 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(0.001,.80,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

min(abs(OUT[,1]-OUT[,2]))
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<=0.03)]

print(cutoff)

#get the optimal cut from the test_pred data

# Let's choose a cutoff value of 0.05 for final model

test_cutoff_default <- factor(ifelse(test_pred >=0.049, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_default, test_actual_default, positive = "Yes")

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc
sens
spec

test_cutoff_default <- ifelse(test_cutoff_default=="Yes",1,0)
test_actual_default <- ifelse(test_actual_default=="Yes",1,0)
summary(test_actual_default)

pred_object_test<- prediction(test_cutoff_default, test_actual_default)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

#plot ROC 
# ROC curves
plot(performance_measures_test, main = "ROC curve for Employee Attrition",  colorize=T, lwd = 3)

#plot ROC chart using pROC package as it gives the better visualization
glm_link_scores <- predict(final_model,  test[,-1], type="link")
glm_response_scores <- predict(final_model,  test[,-1], type="response")

plot(roc(test$Performance.Tag, glm_response_scores, direction="<"),
     col="green", lwd=3, main="ROC curve of Employee Attrition")


# Lift & Gain Chart 
# plotting the lift chart

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_default, test_pred, groups = 10)

Attrition_decile

#application score card
data_score_card <- credit_card_regression
colnames(data_score_card)
data_score_card$prob_bad<- predict(final_model, type = "response", 
                                   newdata = data_score_card[,-27])

data_score_card$prob_good <- (1 - data_score_card$prob_bad)
data_score_card$odds_good <- data_score_card$prob_good/data_score_card$prob_bad
data_score_card$log_odds_good <- log(data_score_card$odds_good)

factorOdds <- 20/log(2) # Odds to double 20 points
factorOdds
offset_factorOdds <- 400 - (factorOdds*log(10)) # 10:1 at a score of 400
data_score_card$score <- offset_factorOdds + factorOdds * data_score_card$log_odds_good

cut_off_score <- offset_factorOdds + factorOdds * log((1-0.049)/0.049)
cut_off_score

#total number of good customers
nrow(data_score_card[(data_score_card$score >= cut_off_score),]) 

#total customers
nrow(data_score_card)


#-------------------------------
#install.packages("RGT")
#install.packages("rpart.plot")
#install.packages("rpart")
#install.packages("rattle")

library(rpart)
library(rattle)
library(rpart.plot)

Credit_card_DT <- credit_card_eda[, -which(names(credit_card_eda) %in% c("IncomeRange", "Residence.Years", "Company.Years", "x_Avgas.CC.Utilization.in.last.12.months" ))]
colnames(Credit_card_DT)
numericcols <- c( "No.of.dependents" ,"Income" ,"No.of.months.in.current.residence" ,"No.of.months.in.current.company",                                
                  "No.of.times.90.DPD.or.worse.in.last.6.months" ,"No.of.times.60.DPD.or.worse.in.last.6.months"  ,                 
                  "No.of.times.30.DPD.or.worse.in.last.6.months", "No.of.times.90.DPD.or.worse.in.last.12.months"  ,                
                  "No.of.times.60.DPD.or.worse.in.last.12.months" ,"No.of.times.30.DPD.or.worse.in.last.12.months"  ,                
                  "Avgas.CC.Utilization.in.last.12.months", "No.of.trades.opened.in.last.6.months"         ,                  
                  "No.of.trades.opened.in.last.12.months" , "No.of.PL.trades.opened.in.last.6.months"        ,                
                  "No.of.PL.trades.opened.in.last.12.months" , "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans." ,
                  "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.", "Outstanding.Balance"     ,                                       
                  "Total.No.of.Trades" )



factorcols <- c("Gender" ,"Marital.Status", "Education", "Profession", "Type.of.residence", 
                "Presence.of.open.auto.loan" ,"Presence.of.open.home.loan" , "AgeCategory" )

Credit_card_DT[, numericcols] <- lapply(numericcols, function(x) as.numeric(as.character(Credit_card_DT[, x])))
Credit_card_DT[, factorcols] <- lapply(factorcols, function(x) as.factor(as.character(Credit_card_DT[, x])))

write.csv(Credit_card_DT, "Credit_card_DT.csv")
# Let's split the data in training and test datasets.

str(Credit_card_DT)
split_indices <- sample.split(Credit_card_DT$Performance.Tag, SplitRatio = 0.70)
train_dt <- Credit_card_DT[split_indices, ]
test_dt <- Credit_card_DT[!split_indices, ]

set.seed(3033)
split_indices <- createDataPartition(y = Credit_card_DT$Performance.Tag, p= 0.7, list = FALSE)
train_dt <- Credit_card_DT[split_indices,]
test_dt <- Credit_card_DT[-split_indices,]
anyNA(Credit_card_DT)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
dtree_fit <- train(Performance.Tag ~ ., data = train_dt, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)
dtree_fit

#Plot Decision Tree
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)
dtree_fit$finalModel$variable.importance

test_pred <- predict(dtree_fit, newdata = test_dt)
confusionMatrix(test_pred, test_dt$Performance.Tag )  #check accuracy

#Prune the tree using the best cp.
tree.pruned <- prune(dtree_fit$finalModel, cp = 0.000324728)
prp(tree.pruned, box.palette = "Reds", tweak = 1.2)
conf.matrix <- table(train_dt$Performance.Tag, predict(tree.pruned,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)


# Package required for randomForest algorithm is:
# install randomForest
library(randomForest)
#---------------------------------------------------------    

# Spliting the bank data in 70:30 ratio

set.seed(101)
Credit_card_RM <- Credit_card_DT
Credit_card_RM$Performance.Tag <- as.factor(ifelse(Credit_card_RM$Performance.Tag==1,"yes","no"))
split_indices <- sample.split(Credit_card_RM$Performance.Tag, SplitRatio = 0.70)
train_rf <- Credit_card_RM[split_indices, ]
test_rf <- Credit_card_RM[!split_indices, ]

#---------------------------------------------------------    

# Building the model 

creditcard_rf <- randomForest(Performance.Tag ~., data = train_rf, proximity = F, do.trace = T, mtry = 5)

# Predict response for test data

rf_pred <- predict(creditcard_rf, test_rf[, -27], type = "prob")

#---------------------------------------------------------    

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
min(abs(OUT_rf[,1]-OUT_rf[,2]))
cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<=0.038)]

# The plot shows that cutoff value of around 05% optimises sensitivity and accuracy

predicted_response <- factor(ifelse(rf_pred[, 2] >= 0.052, "yes", "no"))

conf_forest <- confusionMatrix(predicted_response, test_rf[, 27], positive = "yes")

conf_forest

# Sensitivity
conf_forest$byClass[1]

# Specificity 
conf_forest$byClass[2]

# Accuracy 
conf_forest$overall[1]


# Final RF important variables
importance <- creditcard_rf$importance 

importance <- data.frame(importance)
