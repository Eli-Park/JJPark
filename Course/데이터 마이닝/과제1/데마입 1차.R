####################################
## Data Mining team assignment #1 ##
####################################

## Install and call packages
install.packages("arules")
install.packages("arulesViz")

library(arules)
library(arulesViz)


## Set directory and read data file
setwd("C:/Users/pkmon/Desktop/Data1.BankService")
dir()

Bank <- read.transactions("BNKSERV.csv", format = "single", cols = c(1, 2), sep = ",", skip = 1, rm.duplicates = TRUE)
inspect(Bank)
as(Bank, "data.frame")
Bank.2 <- read.csv("BNKSERV.csv")

nrow(Bank.1)
SN <- table(Bank.2[,2])
## Generate rules
rule1 <- apriori(Bank, parameter = list(support = 0.1, confidence = 0.2, minlen = 2), control = list(verbose = FALSE))
inspect(rule1)
rule1.sorted = sort(rule1, by=c("lift","support"))
inspect(rule1.sorted)

library(dplyr)
rules.sub = subset(rule1.sorted, lift > 1)

R <- inspect(rules.sub)
R$rhs <- as.character(R$rhs)
R <- subset(R, rhs != "{CKING}" & rhs != "{SVG}")
R$rhs <- as.factor(R$rhs)
R

## Plot
plot(rules.sub, method="graph", engine = 'interactive')
plot(rules.sub, method="grouped", measure=c("lift","support"))



nrow(Bank.1)
