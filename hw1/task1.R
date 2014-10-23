# libraries
library(lattice)


# prepade data
adv <- read.csv("~/GitRepos/MachineLearning2014/hw1/Advertising.csv")
adv$X <- NULL
smpsNum <- dim(adv)[1]
trainSize <- smpsNum * 2 / 3
testSize <- smpsNum - trainSize
trainInds <- sample(1:smpsNum, size = trainSize)
adv.train <- adv[trainInds, ]
adv.test <- adv[-trainInds, ]

#funcs
rss <- function(reals, preds) {
    return(sum((reals - preds)^2))
}

predWithRss <-function(mod) {
    predTrain <- predict(mod, adv.train)
    predTest <- predict(mod, adv.test)
    rssTrain <- rss(adv.train$Sales, predTrain) / trainSize
    rssTest <- rss(adv.test$Sales, predTest) / testSize
    return (list(xyplot(predTrain ~ adv.train$Sales), rssTrain, xyplot(predTest ~ adv.test$Sales), rssTest))
}

# 1. create scatterplots
l <- lm(Sales ~ ., data = adv.train)
predTest <- predict(l, adv.test)
pltTest <- xyplot(predTest ~ adv.test$Sales)
predTrain <- predict(l, adv.train)
pltTrain <- xyplot(predTrain ~ adv.train$Sales)

# 2. rss
print(summary(l))
rssTrain <- rss(adv.train$Sales, predTrain) / trainSize
rssTest <- rss(adv.test$Sales, predTest) / testSize
print("Mean RSS for train set: ")
print(rssTrain)
print("Mean RSS for test set: ")
print(rssTest)

# 3. rss for modified models
lNoNews <- update(l, . ~ . - Newspaper)
print(summary(lNoNews))
resNoNews <- predWithRss(lNoNews)
print("Mean RSS for train set without Newspaper: ")
print(resNoNews[[2]])
print("Mean RSS for test set without Newspaper: ")
print(resNoNews[[4]])

lNoTv <- update(l, . ~ . - TV)
print(summary(lNoTv))
resNoTv <- predWithRss(lNoTv)
print("Mean RSS for train set without TV: ")
print(resNoTv[[2]])
print("Mean RSS for test set without TV: ")
print(resNoTv[[4]])

lInt <- update(l, . ~ . - TV - Radio - Newspaper)
print(summary(lInt))
resInt <- predWithRss(lInt)
print("Mean RSS for train set with intercept only: ")
print(resInt[[2]])
print("Mean RSS for test set with intercept only: ")
print(resInt[[4]])