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
meanRss <- function(reals, preds) {
    return(mean((reals - preds)^2))
}

predWithRss <-function(mod) {
    predTrain <- predict(mod, adv.train)
    predTest <- predict(mod, adv.test)
    rssTrain <- meanRss(adv.train$Sales, predTrain)
    rssTest <- meanRss(adv.test$Sales, predTest)
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
rssTrain <- meanRss(adv.train$Sales, predTrain)
rssTest <- meanRss(adv.test$Sales, predTest)
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

#4. Additional task: polinomial regression
p1 <- lm(Sales ~ poly(TV, 1, raw=TRUE), data=adv.train)
print(summary(p1))
print("AIC for polynomial regression, deg = 1: ")
print(AIC(p1))
print("BIC for polynomial regression, deg = 1: ")
print(BIC(p1))
resP1 <- predWithRss(p1)
print("Mean RSS for train set (polynomial regression, deg = 1): ")
print(resP1[[2]])
print("Mean RSS for test set (polynomial regression, deg = 1): ")
print(resP1[[4]])

p2 <- lm(Sales ~ poly(TV, 2, raw=TRUE), data=adv.train)
print(summary(p2))
print("AIC for polynomial regression, deg = 2: ")
print(AIC(p2))
print("BIC for polynomial regression, deg = 2: ")
print(BIC(p2))
resP2 <- predWithRss(p2)
print("Mean RSS for train set (polynomial regression, deg = 2): ")
print(resP2[[2]])
print("Mean RSS for test set (polynomial regression, deg = 2): ")
print(resP2[[4]])

p3 <- lm(Sales ~ poly(TV, 3, raw=TRUE), data=adv.train)
print(summary(p3))
print("AIC for polynomial regression, deg = 3: ")
print(AIC(p3))
print("BIC for polynomial regression, deg = 3: ")
print(BIC(p3))
resP3 <- predWithRss(p3)
print("Mean RSS for train set (polynomial regression, deg = 3): ")
print(resP3[[2]])
print("Mean RSS for test set (polynomial regression, deg = 3): ")
print(resP3[[4]])

p4 <- lm(Sales ~ poly(TV, 4, raw=TRUE), data=adv.train)
print(summary(p4))
print("AIC for polynomial regression, deg = 4: ")
print(AIC(p4))
print("BIC for polynomial regression, deg = 4: ")
print(BIC(p4))
resP4 <- predWithRss(p4)
print("Mean RSS for train set (polynomial regression, deg = 4): ")
print(resP4[[2]])
print("Mean RSS for test set (polynomial regression, deg = 4): ")
print(resP4[[4]])