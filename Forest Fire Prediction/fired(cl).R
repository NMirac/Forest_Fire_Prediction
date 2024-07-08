library(party)
library(partykit)
library(caret)
str(gdata)
gdata$type <- as.factor(gdata$type)
table(gdata$type)


set.seed(250)
ind <- sample(2, nrow(gdata), replace = TRUE, prob = c(0.7,0.3))
train <- gdata[ind==1,]
test <- gdata[ind==2,]


myf <- type ~ DMC+DC+ISI+Temperature+RH+Wind+Rain+FFMC

gdata_ctree <- ctree(myf, data=train)

table(predict(gdata_ctree), train$type)

p4 <- predict(gdata_ctree, test)
confusionMatrix(p4, test$type)

ct <- ctree(type ~ ., data = gdata)
plot(ct)
