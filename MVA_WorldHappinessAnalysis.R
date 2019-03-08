#Soukhyada Vaidya
#Assignment: World Happiness Analysis
#Loading the data
worldh <- read.csv("C:/Users/Soukhyada/Desktop/MVA/WH_2017.csv")

#Loading packages required for the analysis
library(plyr)
library(plotly)
library(dplyr)
library(tidyverse)
library(lubridate)
library(caTools)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(data.table)
library(tidyr)
library(corrgram)       
library(corrplot)
library(formattable)
library(cowplot)
library(ggpubr)
library(plot3D)
library(latexpdf)
library(car)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(mice)

#View the data
View(worldh)
#Displays the first few rows of the dataset
head(worldh)
#Display the structure of the attributes
str(worldh)

# Adding another column name "Continent"
worldh$Continent <- NA

# Deleting unnecessary columns (Whisker.high and Whisker.low)
worldh <- worldh[, -c(4,5)]

# Changing the name of columns
colnames (worldh) <- c("Country", "Happiness.Rank", "Happiness.Score",
                       "Economy", "Family", "Life.Expectancy", "Freedom", "Generosity",
                       "Trust", "Dystopia.Residual", "Continent")

# Adding the values for Continent name in the data.

worldh$Continent[which(worldh$Country %in% c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
                                             "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
                                             "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong S.A.R., China", "Philippines",
                                             "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
                                             "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
                                             "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
                                             "Cambodia", "Afghanistan", "Yemen", "Syria"))] <- "Asia"
worldh$Continent[which(worldh$Country %in% c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
                                             "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
                                             "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
                                             "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
                                             "Russia", "Lithuania", "Latvia", "Moldova", "Romania",
                                             "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus",
                                             "Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
                                             "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
                                             "Bulgaria", "Albania", "Ukraine"))] <- "Europe"
worldh$Continent[which(worldh$Country %in% c("Canada", "Costa Rica", "United States", "Mexico",  
                                             "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                                             "Jamaica", "Nicaragua", "Dominican Republic", "Honduras",
                                             "Haiti"))] <- "North America"
worldh$Continent[which(worldh$Country %in% c("Chile", "Brazil", "Argentina", "Uruguay",
                                             "Colombia", "Ecuador", "Bolivia", "Peru",
                                             "Paraguay", "Venezuela"))] <- "South America"
worldh$Continent[which(worldh$Country %in% c("New Zealand", "Australia"))] <- "Australia"
worldh$Continent[which(is.na(worldh$Continent))] <- "Africa"

# Moving the Continent column at the second position.

worldh <- worldh %>% select(Country,Continent, everything())

str(worldh)

#Converting the Continent values into factorial.
worldh$Continent <- as.factor(worldh$Continent)

# Finding the correlation between numerical columns
Num.cols <- sapply(worldh, is.numeric)
Cor.data <- cor(worldh[, Num.cols])
corrplot(Cor.data, method = 'color')

#Analysis: We can see there is an inverse correlation between "Happiness Rank" and all the other numerical variables. In other words, the lower the happiness rank, the higher the happiness score, and the higher the other seven factors that contribute to happiness. So let's remove the happiness rank, and see the correlation again.

# Create a correlation plot
newdatacor = cor(worldh[c(3:10)])
corrplot(newdatacor, method = "number")
#Analysis: In the above cor plot, Economy, life expectancy, and family play the most significant role in contributing to happiness. 
#Trust and generosity have the lowest impact on the happiness score.

#Plotting ScatterPLot
plot_ly(data = worldh, 
        x=~Economy, y=~Happiness.Score, type = "scatter",
        text = ~paste("Country:", Country)) %>% 
  layout(title = "Happiness and GDP", 
         xaxis = list(title = "GDP per Capita"),
         yaxis = list(title = "Happiness Score"))
#Analysis: This interactive scatterplot shows that there is a strong positive correlation between GDP and Happiness.

#Let's do multiple Regression
dat <- worldh[c("Happiness.Score","Economy","Generosity")]
head(dat)
plot(dat)
#It seems like there is a positive correlation between economy and happiness score but this is not true between happiness score
#and generosity.

#3D plot of same
scatter3D(dat$Generosity, dat$Economy, dat$Happiness.Score, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed",
          main = "Happiness data", xlab = "Generosity",
          ylab ="Economy", zlab = "Happiness.Score")
#From the scatter plot we cannot determine that combination of high economy and generosity leads to greater happiness score. 
#This is something we have to conclude after analyzing the effect of these 2 taken together.


# Checking the outliers in the dataset using the boxplot.
names(worldh)[4] <- "Happiness_Score"

ggplot(worldh, aes(x=Continent, y= Happiness_Score, colour = Continent)) + 
  
  geom_boxplot() + 
  
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  
  labs(title = "Happiness Score Boxplot",
       
       x = "Continent",
       
       y = "Happiness Score")


 

##Checking for normality using shaprio test

qqPlot(worldh$Economy)
shapiro.test(worldh$Economy)

#p-value is greater than 0.05 implying that the data is not significantly different from normal distribution 
qqPlot(worldh$Family)
shapiro.test(worldh$Family)

qqPlot(worldh$Life.Expectancy)
shapiro.test(worldh$Life.Expectancy)

qqPlot(worldh$Freedom)
shapiro.test(worldh$Freedom)

qqPlot(worldh$Generosity)
shapiro.test(worldh$Generosity)

qqPlot(worldh$Trust)
shapiro.test(worldh$Trust)

#Family,Life expectancy and trust variables are not normally distributed

####PCA################
act_col <- c(3, 5:10)

act_col

happiness_new <- worldh[, act_col]

cor(happiness_new)

happiness_pca <- prcomp(happiness_new,scale=TRUE)

summary(happiness_pca)

(eigen_happiness <- happiness_pca$sdev^2)

eigen_happiness

names(eigen_happiness) <- paste("PC",1:7,sep="")

sumlambdas <- sum(eigen_happiness)
sumlambdas
propvar <- eigen_happiness/sumlambdas
propvar
cumvar_happiness <- cumsum(propvar)
cumvar_happiness
matlambdas <- rbind(eigen_happiness,propvar,cumvar_happiness)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")

round(matlambdas,4)
summary(happiness_pca)
happiness_pca$rotation
print(happiness_pca)


happiness_pca$x

happiness_new
md.pattern(happiness_new)
happy.pca <- PCA(happiness_new, graph = F)
eig.val <- get_eigenvalue(happy.pca)
eig.val
fviz_eig(happy.pca, addlabels = TRUE, ylim = c(0, 60), linecolor = "purple", barfill = "orange", barcolor = "orange")

#Showing the variables
var <- get_pca_var(happy.pca)
fviz_pca_var(happy.pca, col.var = "darkblue")
#Analysis:We see that for instance family, life expectancy and economy are highly correlated. Trust in the government and freedom are also correlated.
#We also see that life expectancy, etc are more correlated with the first dimension whereas freedom, generousity are more correlated with the second dimension.

#Here ,Cos2 shows the quality of representation
fviz_cos2(happy.pca, choice ="var", axes = 1:2, top = 10, color = "dark blue" )

#Contribution of the variables
var$contrib

#Contribution of the top 5 variables
fviz_contrib(happy.pca, choice = "var", axes = 1, top = 5)

#PCA plot with "fviz_pca_ind"
ind <- get_pca_ind(happy.pca)
ind

happy.pca$ind

#Plotting the graph
fviz_pca_ind (happy.pca, pointsize = "cos2", pointshape = 22, fill = "blue", repel = TRUE)

#Method to show only the 50 countries best represented.
plot(happy.pca,  select = "cos2 50", cex=1,  col.ind = "darkblue", title = "50 countries with highest cos2", cex.main=2, col.main= "darkblue")


