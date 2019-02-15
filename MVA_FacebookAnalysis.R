#Soukhyada Vaidya
#Assignment 1: Facebook Ad Analysis
#Loading the data
dat <- read.csv("C:/Users/Soukhyada/Desktop/KAG_conversion_data.csv")
#View the data
View(dat)
#Displays the first few rows of the dataset
head(dat)
#Display the structure of the attributes
str(dat)
#Loading required packages
library(tidyverse)
#Gives an overview of the data
glimpse(dat)
#Checking variable: age
unique(dat$age)

#Adding New Columns for Analysis : 
#Click Rate(click_rate)
#clicks per 10000 impressions
dat<- dat %>% mutate(click_rate = as.factor(ifelse(dat$Impressions != 0 , round(dat$Clicks/dat$Impressions*10000) , 0)))

#Conversion Rate(conv_rate)
#Approved conversions as a percentage of total conversions
dat<- dat %>% mutate(conv_rate = ifelse(dat$Total_Conversion != 0 , round(dat$Approved_Conversion/dat$Total_Conversion*100), 0))
     
#Conversion rate as factor
dat$conv_rate <-cut(dat$conv_rate, seq(0,100,10), right=TRUE, labels=c("0-10%","10-20%","20-30%","30-40%","40-50%","50-60%","60-70%","70-80%","80-90%","90-100%"))
dat$conv_rate[is.na(dat$conv_rate)] <- "0-10%"

#Loss amount
dat<- dat %>%  mutate(loss_amount = ifelse(dat$Clicks !=0, dat$Spent*(1 - dat$Approved_Conversion / dat$Clicks),0))

#Loss rate 
dat<- dat %>% mutate(loss_rate =  ifelse(dat$Spent>0, round(dat$loss_amount/dat$Spent)*100,0)) 
dat$loss_rate <-cut(dat$loss_rate, seq(0,100,10), right=TRUE, labels=c("0-10%","10-20%","20-30%","30-40%","40-50%","50-60%","60-70%","70-80%","80-90%","90-100%"))
dat$loss_rate[is.na(dat$loss_rate)] <- "0-10%"

##Data Analysis
#Gives the details of xyz_campaign_id in tabular format
table(dat$xyz_campaign_id)

#There are 3 companies and the analysis will be done seperately
dat_916<-dat %>% filter(xyz_campaign_id %in% c("916"))

#Loading package for plots.
library(ggplot2)

##Analysis done on ad campaign 916
ggplot(data = dat_916) +aes(x = age, y = click_rate, fill = age) +geom_tile() +theme_minimal() +facet_wrap(vars(gender))
#Analysis:For both gender groups Male and Female the age group 30-34 seems to respond with higher click rate ranging from 2 to 5
#But we cannot say anything conclusive about other groups because we don't have sufficient data in these groups 


# Analysis  using using "loss rate" column. It shows how many approved conversion a company gets as a return to spent money. If it's 0 , the return equals to spend. If it's 100%, all spend is a loss.
ggplot(data = dat_916) +aes(x = age, y = loss_rate, fill = loss_rate) +geom_tile() +theme_minimal() +facet_wrap(vars(gender))
#Analysis:We cannot conclude anything from these plots because in all the age groups of both genders there is a 50% loss of 0-10


##Analysis on comparison of ad campaigns w.r.t Advertising Spend and Conversions
ggplot(dat, aes(as.factor(xyz_campaign_id), Spent)) + geom_boxplot() + labs(x = "Campaign", y = "Advertising Spend")
ggplot(dat, aes(as.factor(xyz_campaign_id), Total_Conversion)) + geom_boxplot() + labs(x = "Campaign", y = "Conversions")
#From the above first box-plot we can see that company 1178 spends more on advertisements comparatively
#And the second box plot shows there is a more total conversion rate for company 1178

#Hence, from the above plots, let's do further analysis on ad campaign 1178

##Analysis done on ad campaign 1178
#Creating a new dataframe that just includes the data from that campaign.
dat1178 <- dat %>%filter(xyz_campaign_id == 1178)

#Loading package for plotting barplots and histograms
library(DataExplorer)

#Checking the data variable by variable
#Plotting the frequencies w.r.t each variable
options(repr.plot.width=4, repr.plot.height=4)
plot_bar(dat1178)
options(repr.plot.width=8, repr.plot.height=4)
plot_histogram(dat1178[-2])

#Preliminary analysis of campaign 1178
#Let's look at what happens to the number of conversions and the value of our conversions when we spend more money on our campaign.
options(repr.plot.width=6, repr.plot.height=3)
ggplot(dat1178, aes(Spent, Total_Conversion)) + geom_point() + geom_smooth(method = "lm") +
  labs(x = "Amount spent on campaign", y = "Total number of conersions")
ggplot(dat1178, aes(Spent, Total_Conversion)) + geom_point() + geom_smooth(method = "lm") +
  labs(x = "Amount spent on campaign", y = "Total value of conversions")
#Analysis:Here, it looks like the more we spend, the more we get back.
#But the amount of data is quite sparse at the right-hand side so we cannot say that the statement is accurate without considering more analysis.

#Now, lets find correlation for 1178 Campaign
#Loading packages for correlation
library(caret)
library(corrplot)
ds_1178_predict<-dat1178 %>% select( age, gender,interest, Impressions,Clicks,Spent)
#Setting random number as seed
set.seed(1234)
ds_1178_predict_Data <- dummyVars("~.",data=ds_1178_predict, fullRank=T)
ds_1178_predict_final <- as.data.frame(predict(ds_1178_predict_Data,ds_1178_predict))
print(names(ds_1178_predict_final))

corMatMy <- cor(ds_1178_predict_final)
corrplot(corMatMy, order = "hclust")
#Analysis: From the correlation matrix we can see that it's pretty obvious that the clicks are strongly correlated to impressions and spent.

#Conclusion: Data Analysis and Visualization done on different campaigns to find out the relation between multiple factors such as Click rate, conversion rate and loss rate. 