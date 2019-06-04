##SAS에서 입력한 데이터를 csv로 내보내어 사용하였습니다.
b <- read.csv("C:/Users/pkmon/Desktop/생과데1/B.csv")
diet <- read.csv("C:/Users/pkmon/Desktop/생과데1/diet.csv")
diet$meal <- as.factor(diet$meal)
#2.1
var(b1)
var(b2)
var.test(x ~ group, data=b, alternative = "two.sided")

#2.7
res <- aov(kg ~ exercise + meal, data=diet)
res
summary(res)
library(agricolae)
SCHEFFE1 <- scheffe.test(res, "exercise", group=TRUE, console=FALSE)
SCHEFFE2 <- scheffe.test(res, "meal", group=TRUE, console=FALSE)
SCHEFFE1
SCHEFFE2
