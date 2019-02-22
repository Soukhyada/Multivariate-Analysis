#Soukhyada Vaidya
#Assignment: World Happiness Analysis
#Loading the data
worldh <- read.csv("C:/Users/Soukhyada/Desktop/WH_2017.csv")

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

#View the data
View(worldh)
#Displays the first few rows of the dataset
head(worldh)
#Display the structure of the attributes
str(worldh)

# Changing the name of columns
colnames (worldh) <- c("Country", "Happiness.Rank", "Happiness.Score",
                          "Whisker.High", "Whisker.Low", "Economy", "Family",
                          "Life.Expectancy", "Freedom", "Generosity",
                          "Trust", "Dystopia.Residual")

# Deleting unnecessary columns (Whisker.high and Whisker.low)
worldh <- worldh[, -c(4,5)]

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
