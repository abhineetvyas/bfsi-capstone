#---------------------------------------------------------
#|               BFS Capstone Project                    |
#| Group number : 21                                     |
#| Batch        : PGDDA - March 2017                     |
#---------------------------------------------------------

#Set the working directory
setwd("D:/Abhineet/Study/IIIT-B/7. Capstone Project/1. Capstone-BFS")

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
#Business Objective: 
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
# 1. Business Understanding - Delinquent vs default customers  [problem statement]
# 2. Data Understanding
     #2.1 check for null values, sanity check, duplicate records
     #2.2 Univariate and bivariate analysis for categorical variables 
          #2.2.1 Histogram/Bar chart to understand the distribution and try to get the business insight
          #2.2.2 Box plot  to identify the outliers  (to be discussed)
     #2.3 univarivariate and bivariate analysis for continous variables
          #2.3.1 Histogram to understand the distribution and get the business insight
          #2.3.2 Box plot to identify the outliers
# 3. Data Preparation
     #3.1 Remove or mutate missing values based on the business justification
     #3.2 duplicate records with respect to application id need to be removed
     #3.3 outlier treatment also based on the business justification
     #3.4 Feature selection
          #3.3.1 Chi-square test for feature selection for categorical variables
          #3.3.2 IV test for feature selection for continuous  variables
          #3.3.3 Missing values treatement for contunuous variables using WOE
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
#  2. Data Understanding
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
demographic_data <- demographic_data[!duplicated(demographic_data), ]

# Check duplicate records with respect to application id in credit_bureau_data
credit_bureau_data[duplicated(credit_bureau_data$Application.ID),]$Application.ID 
# Application Ids viz.  765011468, 653287861, 671989187 are duplicate
credit_bureau_data <- credit_bureau_data[!duplicated(credit_bureau_data), ]

unique(demographic_data$Performance.Tag)
unique(credit_bureau_data$Performance.Tag)


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
unique(demographic_data$No.of.dependents)
# As we see values from 1-5 are available and NA(s) are there so here we are taking assumption 
# that NA means no dependents available and updating the dataset with 0 instead of NA.
demographic_data$No.of.dependents[is.na(demographic_data$No.of.dependents)] <- 0

# Now lets merge the credit burew data with demographic data for all the applications
credit_card_applications <- merge(x = demographic_data, y = credit_bureau_data, by = "Application.ID", all = TRUE)
write.csv(credit_card_applications, "credit_card_applications.csv")
credit_card_applications$Performance.Tag <- as.factor(credit_card_applications$Performance.Tag.x)
credit_card_applications <- credit_card_applications[ , !(names(credit_card_applications) %in% c("Performance.Tag.x","Performance.Tag.y"))]

summary(credit_card_applications)

#compute correlation matrix of entire data set
#corr_res <- cor(credit_card_applications)
#round(corr_res, 2)

str(credit_card_applications)

# Perform Exploratory Data Analysis
credit_card_eda <- credit_card_applications
# 1. Education
unique(credit_card_eda$Education)
credit_card_eda$Education[credit_card_eda$Education==""]<-"Others"
ggplot(data = credit_card_eda, aes(x=Education, fill = Performance.Tag)) + 
  geom_bar(stat = "count")

# 2. Gender
unique(credit_card_eda$Gender)
credit_card_eda$Gender[credit_card_eda$Gender==""]<-"O"
ggplot(data = credit_card_eda, aes(x=Gender, fill = Performance.Tag)) + 
  geom_bar(stat = "count")

# 3. Marital status
unique(credit_card_eda$Marital.Status..at.the.time.of.application.)
credit_card_eda$Marital.Status..at.the.time.of.application.[credit_card_eda$Marital.Status..at.the.time.of.application.==""]<-"O"
ggplot(data = credit_card_eda, aes(x=Marital.Status..at.the.time.of.application., fill = Performance.Tag)) + 
  geom_bar(stat = "count")

# 4. Profession
unique(credit_card_eda$Profession)
credit_card_eda$Profession[credit_card_eda$Profession==""]<-"Other"
ggplot(data = credit_card_eda, aes(x=Profession, fill = Performance.Tag)) + 
  geom_bar(stat = "count")

# 5. ResidenceType
unique(credit_card_eda$Type.of.residence)
credit_card_eda$Type.of.residence[credit_card_eda$Type.of.residence==""]<-"Other"
ggplot(data = credit_card_eda, aes(x=Type.of.residence, fill = Performance.Tag)) + 
  geom_bar(stat = "count")


# 6. Number of dependents
unique(credit_card_eda$No.of.dependents)
ggplot(data = credit_card_eda, aes(x=No.of.dependents, fill = Performance.Tag)) + 
  geom_bar(stat = "count")

# 7. Number of month in current residence
credit_card_eda$No.of.Years.in.current.residence <- cut(credit_card_eda$No.of.months.in.current.residence, 
                       breaks = c(-Inf, 37, 73, 109, Inf), 
                       labels = c("0-3 Years", "4-6 Years", "7-9 Years", ">10 Years"), 
                       right = FALSE)
ggplot(data = credit_card_eda, aes(x=No.of.Years.in.current.residence, fill = Performance.Tag)) + 
  geom_bar(stat = "count")

# 8. Number of month in current company
credit_card_eda$No.of.Years.in.current.company <- cut(credit_card_eda$No.of.months.in.current.company, 
                                                                 breaks = c(-Inf, 13, 25, 37, 49, 61, 73, Inf), 
                                                                 labels = c("0-1 Years", "1-2 Years", "2-3 Years", "3-4 Years", "4-5 Years"
                                                                            , "5-6 Years", ">6 Years"), 
                                                                 right = FALSE)
ggplot(data = credit_card_eda, aes(x=No.of.Years.in.current.company, fill = Performance.Tag)) + 
  geom_bar(stat = "count")

# 9. Income
credit_card_eda$IncomeRange <- cut(credit_card_eda$Income, 
                                                               breaks = c(-Inf, 16, 31, 46, Inf), 
                                                               labels = c("0-15", "16-30","31-45", "45-60"), 
                                                               right = FALSE)
ggplot(data = credit_card_eda, aes(x=IncomeRange, fill = Performance.Tag)) + 
  geom_bar(stat = "count")


box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

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
                           "Education","Profession","Type.of.residence","Presence.of.open.home.loan","Presence.of.open.auto.loan","Performance.Tag.x")


continuous_variables <- c("Income","No.of.months.in.current.residence","No.of.months.in.current.company","Performance.Tag.x","No.of.times.90.DPD.or.worse.in.last.6.months",
                           "No.of.times.60.DPD.or.worse.in.last.6.months","No.of.times.30.DPD.or.worse.in.last.6.months","No.of.times.90.DPD.or.worse.in.last.12.months",
                           "No.of.times.60.DPD.or.worse.in.last.12.months","No.of.times.30.DPD.or.worse.in.last.12.months","Avgas.CC.Utilization.in.last.12.months",      
                           "No.of.trades.opened.in.last.6.months","No.of.trades.opened.in.last.12.months","No.of.PL.trades.opened.in.last.6.months",     
                           "No.of.PL.trades.opened.in.last.12.months","No.of.Inquiries.in.last.6.months","No.of.Inquiries.in.last.12.months","Outstanding.Balance.in.lakh", 
                           "Total.No.of.Trades") 

credit_card_applications_numerical_var <- credit_card_applications[,continuous_variables]
credit_card_applications_categorical_var <- credit_card_applications[,categorical_variables]


#Feature selection
#------------------
#Chi square test to do the feature selection for categorical variables
str(credit_card_applications_categorical_var)
credit_card_applications_categorical_var %<>%
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


#WOE to do the feature selection for continuous variables
#--------------------------------------------------------
colnames(credit_card_applications_numerical_var)
str(credit_card_applications)
IV <- create_infotables(data=credit_card_applications_numerical_var, y="Performance.Tag.x", bins=10, parallel=TRUE)
IV$Summary


# Correlation between numeric variables
#-----------------------------------------
#rename the long column names to short column names to better visulize the corelation matrixs
new_column_names <- c("Income","mon.curr.res","mon.curr.com","perf.tag","90.DPD.6.mon",
                      "60.DPD.6.mon","30.DPD.6.mon","90.DPD.12.mon",
                      "60.DPD.12.mon","30.DPD.12.mon","CC.Util.12.mon",      
                      "trades.6.mon","trades.12.mon","PL.trades.6.mon",     
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
#X-squared = 1.5913, df = 4, p-value = 0.8104

# 2. Gender - Null hypothesis is Gender is insignificant in deciding customer will default
cc_applications_Gender <- table(credit_card_eda$Performance.Tag, credit_card_eda$Gender)
chisq.test(cc_applications_Gender)
#X-squared = 0.98444, df = 2, p-value = 0.6113

# 3. Marital status - Null hypothesis: Marital status is insignificant in deciding customer will default
cc_applications_Married <- table(credit_card_eda$Performance.Tag, credit_card_eda$Marital.Status..at.the.time.of.application.)
chisq.test(cc_applications_Married)
#X-squared = 0.51874, df = 2, p-value = 0.7715

# 4. Profession - Null hypothesis: Profession is insignificant in deciding customer will default
cc_applications_Profession <- table(credit_card_eda$Performance.Tag, credit_card_eda$Profession)
chisq.test(cc_applications_Profession)
# X-squared = 7.3928, df = 3, p-value = 0.06038

# 5. ResidenceType - Null hypothesis: ResidenceType is insignificant in deciding customer will default
cc_applications_Residence <- table(credit_card_eda$Performance.Tag, credit_card_eda$Type.of.residence)
chisq.test(cc_applications_Residence)
# X-squared = 2.5727, df = 5, p-value = 0.7655


# 6. Number of dependents - Null hypothesis: Number of dependents is insignificant in deciding customer will default
cc_applications_Dependents <- table(credit_card_eda$Performance.Tag, credit_card_eda$No.of.dependents)
chisq.test(cc_applications_Dependents)
#X-squared = 7.5544, df = 5, p-value = 0.1826

# 7. Number of month in current residence - Null hypothesis: Number of month in current residence is insignificant in deciding customer will default
cc_applications_residenceYrs <- table(credit_card_eda$Performance.Tag, credit_card_eda$No.of.Years.in.current.residence)
chisq.test(cc_applications_residenceYrs)
# X-squared = 27.46, df = 3, p-value = 0.000004715

# 8. Number of month in current company - Null hypothesis: Number of month in current company is insignificant in deciding customer will default
cc_applications_companyYrs <- table(credit_card_eda$Performance.Tag, credit_card_eda$No.of.Years.in.current.company)
chisq.test(cc_applications_companyYrs)
#X-squared = 148.74, df = 82, p-value = 0.000000002207

# 9. Income - Null hypothesis: Income is insignificant in deciding customer will default
cc_applications_income <- table(credit_card_eda$Performance.Tag, credit_card_eda$IncomeRange)
chisq.test(cc_applications_income)
# X-squared = 182.91, df = 62, p-value = 0.00000000000000022
