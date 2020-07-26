# Project Checkpoint #4
# R in Action
# Anjana Chamarthi achamarthi6@gatech.edu

# Name of Data Set: Loan Data for Credit Risk Models

# Description of Data Set: Data set consists of loan data which is used to 
# build a credit risk model for consumer lending

# Research Question: Hope to learn if there are any correlations between any 
# of the variables given & if possible, get a list of covariates which 
# matter for default prediction

# This reads from the CSV file and stores data in 'loanData'.
loanData <- read.csv('lc_loans_2007_2014.csv', header = TRUE)

# This helps getsnworking directories.
getwd()

# Data Transformations:
# This is known as a response transformation
summary(loanData)

plot(loan_amnt ~ annual_inc, data = loanData, col = "blue",
     pch = 17, cex = 1, main = "Annual Income vs. Loan Amount")

fit <- lm(loanData$annual_inc ~ loanData$loan_amnt)
summary(fit)

# Exploratory Data Analysis
install.packages("tidyverse")
install.packages("Hmisc")
install.packages("ggplot2")
install.packages("funModeling")
library(tidyverse) 
library(Hmisc)
library(ggplot2)
library(funModeling)

data = loanData %>% select(loan_amnt, title, home_ownership, annual_inc)

new_data <- function(data)
{
  glimpse(data)
  freq(data) 
  print(profiling_num(data))
  plot_num(data)
  describe(data)
}

new_data(data)

# Data Models & Visualizations
sts <- subset(loanData, select = c("mort_acc", "acc_now_delinq"))
summary(sts)
cor(sts)
plot(sts)

hist(loanData$loan_amnt)

ggplot(loanData, aes(x = avg_cur_bal, y = delinq_amnt)) +
  geom_point() + geom_point(aes(color = Default)) +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")