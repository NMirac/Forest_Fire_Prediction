library(randomForest)
library(caret)
str(fdata)
fdata$type <- as.factor(fdata$type)
table(fdata$type)


set.seed(111)
ind <- sample(2, nrow(fdata), replace = TRUE, prob = c(0.7,0.3))
train <- fdata[ind==1,]
test <- fdata[ind==2,]

set.seed(112)
rf <- randomForest(type~., data=train)
print(rf)
p1 <- predict(rf, train)

p2 <- predict(rf, test)
confusionMatrix(p2, test$type)




