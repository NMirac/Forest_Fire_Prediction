library(randomForest)
library(caret)
str(gdata)
gdata$type <- as.factor(gdata$type)
table(gdata$type)


set.seed(128)
ind <- sample(2, nrow(gdata), replace = TRUE, prob = c(0.7,0.3))
train <- gdata[ind==1,]
test <- gdata[ind==2,]

set.seed(225)
rf <- randomForest(type~., data=train, ntree = 500, mtry = 8)
print(rf)
p1 <- predict(rf, train)

p2 <- predict(rf, test)
confusionMatrix(p2, test$type)

ct <- ctree(type ~ ., data = train, ntree = 100, mtry = 8)
plot(ct)

