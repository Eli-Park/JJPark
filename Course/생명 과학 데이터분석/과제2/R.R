#DATA read
setwd("C:/Users/pkmon/Desktop/생과데 과제2")
guilt <- read.csv("Guilt.csv")
psycho <- read.csv("psycho.csv")
safety <- read.csv("safety.csv")
reading <- read.csv("reading.csv")
reading2 <- read.csv("reading2.csv")

#4-1
library("ISLR")
#1)
guilt <- guilt[order(guilt$imprison),]
guilt.freq <- cbind(guilt[1:4,4], guilt[5:8,4])
guilt.reside <- guilt[1:4, 1]
guilt.arrest <- guilt[1:4, 2]
str(guilt.reside)
str(guilt.arrest)

guilt.fit <- glm(guilt.freq ~ guilt.reside + guilt.arrest, data = guilt, family = binomial(link = "logit"))
summary(guilt.fit)

exp(guilt.fit$coefficients)
guilt.fit$xlevels

#4-5
library("VGAM")
head(psycho)
psy.fit <- vglm(ordered(degree)~factor(class)+incident, data = psycho, family = cumulative(parallel=TRUE))
summary(psy.fit)

psy.fit1 <- vglm(ordered(degree)~factor(class)+incident, data = psycho, family = cumulative)
summary(psy.fit1)


#4-7
str(safety)
fit.safety = glm(obs ~ seatbelt + bounce + death + seatbelt:bounce + seatbelt:death +
               bounce:death + seatbelt:bounce:death, family=poisson(link=log), data=safety)
summary(fit.safety)

fit.safety1 = glm(obs ~ seatbelt + bounce + death + seatbelt:bounce + seatbelt:death +
                   bounce:death, family=poisson(link=log), data=safety)
summary(fit.safety1)

fit.safety1 = glm(obs ~ seatbelt + bounce + death + seatbelt:bounce + seatbelt:death , family=poisson(link=log), data=safety)
summary(fit.safety1)

fit.safety1 = glm(obs ~ seatbelt + bounce + death + seatbelt:bounce +
                    bounce:death, family=poisson(link=log), data=safety)
summary(fit.safety1)

fit.safety1 = glm(obs ~ seatbelt + bounce + death + seatbelt:death +
                    bounce:death, family=poisson(link=log), data=safety)
summary(fit.safety1)

fit.safety1 = glm(obs ~ seatbelt + bounce + death + seatbelt:death
                    , family=poisson(link=log), data=safety)
summary(fit.safety1)

fit.safety1 = glm(obs ~ seatbelt + bounce + death  +
                    bounce:death, family=poisson(link=log), data=safety)
summary(fit.safety1)

fit.safety1 = glm(obs ~ seatbelt + bounce + death + seatbelt:bounce
                    , family=poisson(link=log), data=safety)
summary(fit.safety1)

fit.safety1 = glm(obs ~ seatbelt + bounce + death, family=poisson(link=log), data=safety)
summary(fit.safety1)






safety1 <- safety[order(safety$death),]
death.freq <- cbind(safety1[1:4,4], safety1[5:8,4])
seatf <- safety1[1:4,1]
bouncef <- safety1[1:4,2]

safe.logit <- glm(death.freq ~ factor(seatf) + factor(bouncef), data = safety1, family = binomial(link = "logit"))
summary(safe.logit)

#4-9
fit.reading = glm(obs ~ factor(time) + factor(gender) + factor(type) + time:gender + time:type +
                    gender:type + time:gender:type, family=poisson(link=log), data=reading)
summary(fit.reading)

fit.reading1 = glm(obs ~ factor(time) + factor(gender) + factor(type) + time:gender + time:type
                    , family=poisson(link=log), data=reading)
summary(fit.reading1)

fit.reading1 = glm(obs ~ factor(time) + factor(gender) + factor(type) + time:gender +
                     gender:type, family=poisson(link=log), data=reading)
summary(fit.reading1)

fit.reading1 = glm(obs ~ factor(time) + factor(gender) + factor(type) + time:type +
                     gender:type, family=poisson(link=log), data=reading)
summary(fit.reading1)

fit.reading1 = glm(obs ~ factor(time) + factor(gender) + factor(type) + time:gender, family=poisson(link=log), data=reading)
summary(fit.reading1)

fit.reading1 = glm(obs ~ factor(time) + factor(gender) + factor(type) +gender:type, family=poisson(link=log), data=reading)
summary(fit.reading1)

fit.reading1 = glm(obs ~ factor(time) + factor(gender) + factor(type), family=poisson(link=log), data=reading)
summary(fit.reading1)


#-2

fit.reading = glm(obs ~ factor(time) + factor(gender) + factor(type) + time:gender + time:type +
                    gender:type, family=poisson(link=log), data=reading2)
summary(fit.reading)

fit.reading1 = glm(obs ~ factor(time) + factor(gender) + factor(type) + time:gender + time:type
                   , family=poisson(link=log), data=reading2)
summary(fit.reading1)

fit.reading1 = glm(obs ~ factor(time) + factor(gender) + factor(type) + time:gender +
                     gender:type, family=poisson(link=log), data=reading2)
summary(fit.reading1)

fit.reading1 = glm(obs ~ factor(time) + factor(gender) + factor(type) + time:type +
                     gender:type, family=poisson(link=log), data=reading2)
summary(fit.reading1)

fit.reading1 = glm(obs ~ factor(time) + factor(gender) + factor(type) + time:gender, family=poisson(link=log), data=reading2)
summary(fit.reading1)

fit.reading1 = glm(obs ~ factor(time) + factor(gender) + factor(type) +gender:type, family=poisson(link=log), data=reading2)
summary(fit.reading1)

fit.reading1 = glm(obs ~ factor(time) + factor(gender) + factor(type), family=poisson(link=log), data=reading2)
summary(fit.reading1)
