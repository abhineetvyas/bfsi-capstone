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

#-------------------------------------------------------
#Synopsis:
#-------------------------------------------------------
#  Credit card companies (CredX in this case) primarily earn their money in three ways. 
#  They charge merchants around 2% to 3% of every transaction made using their credit card. 
#  They charge customers interest on unpaid balance carried from month to month. 
#  And they charge a variety of fees, including annual and late fees. For these reasons, credit
#  card companies earn more money the more customers they have, and are always looking for more
#  people to use their services.

#-------------------------------------------------------
#Business Objective: Delinquent vs default customers  [problem statement]
#-------------------------------------------------------
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
No.of.trades.opened.in.last.6.months <- scale(credit_card_eda$No.of.PL.trades.opened.in.last.6.months)
No.of.PL.trades.opened.in.last.6.months <- scale(credit_card_eda$No.of.PL.trades.opened.in.last.6.months)
No.of.Inquiries.in.last.6.months <- scale(credit_card_eda$No.of.Inquiries.in.last.6.months)
Total.No.of.Trades <- scale(credit_card_eda$Total.No.of.Trades)
Income <- scale(credit_card_eda$Income)
No.of.months.in.current.residence <- scale(credit_card_eda$No.of.months.in.current.residence)
Avgas.CC.Utilization.in.last.12.months <- scale(credit_card_eda$Avgas.CC.Utilization.in.last.12.months)       
No.of.trades.opened.in.last.12.months <- scale(credit_card_eda$No.of.trades.opened.in.last.12.months)
No.of.PL.trades.opened.in.last.12.months <- scale(credit_card_eda$No.of.PL.trades.opened.in.last.12.months)
No.of.Inquiries.in.last.12.months <- scale(credit_card_eda$No.of.Inquiries.in.last.12.months)
Outstanding.Balance <- scale(credit_card_eda$Outstanding.Balance)

credit_card_num_vars <- data.frame(No.of.trades.opened.in.last.6.months, No.of.PL.trades.opened.in.last.6.months,
                                  No.of.Inquiries.in.last.6.months,No.of.Inquiries.in.last.6.months,
                                  Total.No.of.Trades, Income, No.of.months.in.current.residence, Avgas.CC.Utilization.in.last.12.months,
                                  No.of.trades.opened.in.last.12.months, Avgas.CC.Utilization.in.last.12.months,
                                  No.of.trades.opened.in.last.12.months, No.of.PL.trades.opened.in.last.12.months,
                                  No.of.Inquiries.in.last.12.months, Outstanding.Balance)

# We have following categorical variables
Gender <- as.factor(credit_card_eda$Gender)                         
No.of.dependents <- as.factor(credit_card_eda$No.of.dependents)
Education  <- as.factor(credit_card_eda$Education)
Type.of.residence <- as.factor(credit_card_eda$Type.of.residence)
Company.Years <- as.factor(credit_card_eda$Company.Years)
No.of.times.60.DPD.or.worse.in.last.6.months <- as.factor(credit_card_eda$No.of.times.60.DPD.or.worse.in.last.6.months) 
No.of.times.90.DPD.or.worse.in.last.12.months <- as.factor(credit_card_eda$No.of.times.90.DPD.or.worse.in.last.12.months)
No.of.times.30.DPD.or.worse.in.last.12.months <- as.factor(credit_card_eda$No.of.times.30.DPD.or.worse.in.last.12.months)
Presence.of.open.home.loan <- as.factor(credit_card_eda$Presence.of.open.home.loan)
AgeCategory <- as.factor(credit_card_eda$AgeCategory)
Marital.Status <- as.factor(credit_card_eda$Marital.Status)
Profession <- as.factor(credit_card_eda$Profession)
No.of.times.90.DPD.or.worse.in.last.6.months <- as.factor(credit_card_eda$No.of.times.90.DPD.or.worse.in.last.6.months)
No.of.times.30.DPD.or.worse.in.last.6.months <- as.factor(credit_card_eda$No.of.times.30.DPD.or.worse.in.last.6.months)
No.of.times.60.DPD.or.worse.in.last.12.months <- as.factor(credit_card_eda$No.of.times.60.DPD.or.worse.in.last.12.months)
Presence.of.open.auto.loan <- as.factor(credit_card_eda$Presence.of.open.auto.loan)

credit_card_fact <- data.frame(Gender, No.of.dependents, Education, Type.of.residence, Company.Years, 
                                       No.of.times.60.DPD.or.worse.in.last.6.months, No.of.times.90.DPD.or.worse.in.last.12.months,
                                       No.of.times.30.DPD.or.worse.in.last.12.months, Presence.of.open.home.loan,AgeCategory,
                                       Marital.Status, Profession, No.of.times.90.DPD.or.worse.in.last.6.months,
                                       No.of.times.30.DPD.or.worse.in.last.6.months, Presence.of.open.auto.loan)



credit_card_final<- cbind(credit_card_num_vars,credit_card_fact)

credit_card_final$Performance.Tag <- credit_card_eda$Performance.Tag

#--------------------------------------------------------------------
# 4. Data Modeling - Logistic regression
#--------------------------------------------------------------------

# splitting the data between train and test
dummies<- data.frame(sapply(credit_card_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =credit_card_fact))[,-1]))

credit_card_regression <- cbind(credit_card_num_vars,dummies)
set.seed(100)

indices = sample.split(credit_card_final$Performance.Tag, SplitRatio = 0.7)

train = credit_card_final[indices,]

test = credit_card_final[!(indices),]

model_1 = glm(Performance.Tag ~ ., data = train, family = "binomial")
summary(model_1)# AIC: 16413
# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2)
# Removing multicollinearity through VIF check
#model_3