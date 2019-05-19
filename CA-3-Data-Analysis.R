
setwd("C:/Users/akki2/Documents")
Unemployment_data <- read.csv('data.csv', skip = 4)
head(Unemployment_data, 36)

# Remove months using sub() in unemploument data
Unemployment_data$Period.Unit. <- sub("^(\\d{4}).*$", "\\1", Unemployment_data$Period.Unit.)
install.packages("dplyr")
library(dplyr)

# Modified columnnames
colnames(Unemployment_data)[1] <- "Period"
colnames(Unemployment_data)[2] <- "Unemployment_Rate"

# Grouping unemployment_data by year and summarizing  Percent column by taking mean of unemployment rate.
Unemployment_data <- Unemployment_data %>% 
  group_by(Period) %>%
  summarize(Unemployment_Rate = mean(Unemployment_Rate))



# Changing type of period from charcter to numeric
Unemployment_data$Period <- as.numeric(as.character(Unemployment_data$Period))

# Extracting unemployment_data from 2003 to 2016
Unemployment_data <- Unemployment_data[(21:34), ]
Unemployment_data


str(Unemployment_data)



Crime_data <- read.csv('garda_stations.csv')
head(Crime_data)

# Extracting Burglary and related offences from 2003 to 2016.
Crime_data <- Crime_data %>% select(61:74) 



head(Crime_data,3)

# Taking mean of every column and summarise all columns using summarise_all().
Crime_data <- Crime_data %>% summarise_all("mean")
head(Crime_data, 2)

install.packages("tidyr")
library(tidyr)

# Converting two rows into two columns.
Crime_data <- Crime_data %>% gather(Period, Crime_rate, 1:14)
head(Crime_data)
install.packages("magrittr")
library(magrittr)

# removing charcters from Period column in Crime_data dataset.
Crime_data %<>% mutate(Period = substr(Period, nchar(Period)-34+30, nchar(Period)))
Crime_data

# Removing special charcters "." from Period column in Crime_data dataset 
Crime_data$Period <- gsub("[[:punct:]]", "", Crime_data$Period)

# Converting Period column from char to numeric type.
Crime_data$Period <- as.numeric(as.character(Crime_data$Period))
str(Crime_data)



# Merging two dtasets into one dataset.
Crime_Unemployment_rate <- merge(Crime_data, Unemployment_data, by=c("Period"), all=TRUE)
# Sorting data by year
Crime_Unemployment_rate <- Crime_Unemployment_rate[order(Crime_Unemployment_rate$Period),] 

str(Crime_Unemployment_rate)

install.packages("ggpubr")
install.packages("dplyr")
install.packages("MASS")
install.packages("plyr")
install.packages("ggplot2")
library(MASS)
library(plyr)
library(ggplot2)
library(dplyr)
library(ggpubr)

head(Crime_Unemployment_rate)

# Adding a Categorical column where Unemployment rate greater than 12 will be treated as High 
# and less than will be treated as Low

Crime_Unemployment_rate$Unemployment_rate_greater_than_12 <- ifelse(Crime_Unemployment_rate$Unemployment_Rate > 12,
                                                                    "High", "Low")

# Compute summary statistics by groups:



# Visualize data using box plots
ggboxplot(Crime_Unemployment_rate, x = "Unemployment_rate_greater_than_12",
          y="Crime_rate",palette=c("#00AFBB","#E7B800"),ylab = "Crime_rate", xlab = "Unemployment rate greater than 12",
          color = "red", fill = "green")


# Shapiro-Wilk normality test for Rate of Crime
shapiro.test(Crime_Unemployment_rate$Crime_rate)# p = 7.266e-06
# Shapiro-Wilk normality test for Rate of unemployment
shapiro.test(Crime_Unemployment_rate$Unemployment_Rate) # p = 0.02611


# As the one P value is less then 0.05, normality is not assummed.

# Wilcoxon test
wilcox.test(Crime_Unemployment_rate$Crime_rate)
wilcox.test(Crime_Unemployment_rate$Unemployment_Rate)


# FInding standard error
ddply(Crime_Unemployment_rate, ~ Unemployment_rate_greater_than_12, summarize,
      group.size = length(Crime_rate),
      mean.Crime_rate = mean(Crime_rate),
      sd.Crime_rate = sd(Crime_rate),
      se.mean.Crime_rate = sd.Crime_rate / sqrt(group.size)
)

# Apply indipendent two sample t-test using t.test()
Crime_Unemployment_rate.t.test <- t.test(Crime_rate ~ Unemployment_rate_greater_than_12, 
                                         data = Crime_Unemployment_rate)
Crime_Unemployment_rate.t.test


names(Crime_Unemployment_rate.t.test)


Crime_Unemployment_rate.t.test$p.value


Crime_Unemployment_rate.t.test$estimate  # group means


Crime_Unemployment_rate.t.test$conf.int  # confidence interval for difference


attr(Crime_Unemployment_rate.t.test$conf.int, "conf.level")  # confidence level


# Calculate difference in means between crime rate and unemployment rate
Crime_Unemployment_rate.t.test$estimate
Crime_rate.unemployment.diff <- round(Crime_Unemployment_rate.t.test$estimate[1] - Crime_Unemployment_rate.t.test$estimate[2], 1)
Crime_rate.unemployment.diff

# Confidence level as a %
conf.level <- attr(Crime_Unemployment_rate.t.test$conf.int, "conf.level") * 100
conf.level

# Our study finds that crime rate is increased when Unemployment is greater than 12 

getwd()

