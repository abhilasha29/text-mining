ICU=read.csv("icu2.csv", header = TRUE)
ICUsub=ICU[, 2:21]

#ICUsub$STA=factor(ICUsub$STA)
#ICUsub$GENDER=factor(ICUsub$GENDER)
#ICUsub$RACE=factor(ICUsub$RACE)
#ICUsub[, c(3,4,5,6,7,8,9,12,13,14,15,16,17,18,19,20)]=as.factor(ICUsub[, c(3,4,5,6,7,8,9,12,13,14,15,16,17,18,19,20)])

for (i in c(1,3,4,5,6,7,8,9,12,13,14,15,16,17,18,19,20)){
ICUsub[, i]=as.factor(ICUsub[, i])
}


#Model0
output=glm(ICUsub$STA~., family = binomial, data = ICUsub)
summary(output)


attach(output)
pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)
detach(output)
prob <- predict(output, type = 'response')

threshold <- 0.5

predict <- ifelse(prob>threshold, 1, 0)

table(ICUsub$STA, predict)


#Model1
output1=glm(ICUsub$STA~ICUsub$LOC, family = binomial, data = ICUsub)
summary(output1)


prob1 = predict(output1, type = 'response')
predict1 <- ifelse(prob1>threshold, 1, 0)
table(ICUsub$STA, predict1)


#Model2
output2=glm(ICUsub$STA~ICUsub$LOC+ICUsub$TYP+ICUsub$PH+ICUsub$PCO+ICUsub$CAN+ICUsub$AGE, family = binomial, data = ICUsub)
summary(output2)

prob2 = predict(output2, type = 'response')
predict2 <- ifelse(prob2>threshold, 1, 0)

table(ICUsub$STA, predict2)


#Split Data
library(caTools)

# Generate a random number sequence that can be reproduced to check results thru the seed number.
set.seed(22)

# Randomly split data from Y into two sets in predefined ratio while preserving relative ratios of different values in Y.
split <- sample.split(ICUsub$STA, SplitRatio = 0.7)

# Get training and test data
trainset <- subset(ICUsub, split == TRUE)
testset <- subset(ICUsub, split == FALSE)
