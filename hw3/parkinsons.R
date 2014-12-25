library(MASS) # AIC(), BIC(), lda(), qda()
library(lattice) # xyplot(), densityplot()
library(latticeExtra) # layer()
library(ROCR) # performance(), prediction()
library(caret) # specificity(),
# sensitivity()
library(nnet) # multinom()
library(e1071) # naiveBayes(), tune()
specificity <- caret:::specificity
sensitivity <- caret:::sensitivity
ROC <- function(predicted, actual, ...) {
  pred <- prediction(predicted, as.numeric(actual))
  roc <- performance(pred, measure = "tpr",
                     x.measure = "fpr", ...)
  roc
}
xyplot.performance <- function(x, ...) {
  xyplot(x@y.values[[1]] ~ x@x.values[[1]],
         xlab = x@x.name, ylab = x@y.name,
         type = "l", ...) + layer_(abline(a = 0,
                                          b = 1, col = "red"))
  55}
AUC <- function(predicted, actual, ...) {
  pred <- prediction(predicted, as.numeric(actual))
  perf <- performance(pred, measure = "auc",
                      ...)
  perf@y.values[[1]]
}
roc.opt <- function(predicted, actual, cutoff = NULL,
                    measure = c("mean", "max", "err")) {
  pred <- prediction(predicted, as.numeric(actual))
  perf <- performance(pred, measure = "fpr",
                      x.measure = "fnr")
  measure <- match.arg(measure)
  fpr <- perf@y.values[[1]]
  fnr <- perf@x.values[[1]]
  npos <- pred@n.pos[[1]]
  nneg <- pred@n.neg[[1]]
  err <- (fpr * nneg + fnr * npos)/(npos +
                                      nneg)
  error.rate <- switch(measure, mean = (fpr +
                                          fnr)/2, max = pmax(fpr, fnr), err = err)
  if (is.null(cutoff)) {
    i <- which.min(error.rate)
  } else {
    i <- which.min(abs(perf@alpha.values[[1]] -
                         cutoff))
  }
  list(cutoff = perf@alpha.values[[1]][i],
       fpr = fpr[i], fnr = fnr[i], err = err[i],
       error.rate = error.rate[i])
}
simple.predict.glm <- function(x, newdata,
                               ...) {
  response <- predict(x, newdata, type = "response",
                      ...)
  factor(levels(x$model[, 1])[1 + as.integer(response >
                                               0.5)])
}
my.predict.glm <- function(x, newdata = x$data,
                           ..., measure = "max") {
  opt <- roc.opt(fitted(x), as.numeric(x$model[,
                                               1]), measure = measure)
  cutoff <- opt$cutoff
  factor(as.integer(predict(x, newdata = newdata,
                            type = "response") > cutoff), labels = levels(x$model[,
                                                                                  1]))
}
error.fun.max <- function(true, predicted) {
  561 - min(sensitivity(predicted, true),
            specificity(predicted, true))
}
error.fun.mean <- function(true, predicted) {
  1 - mean(sensitivity(predicted, true),
           specificity(predicted, true))
}
my.lda <- function(x, data, ...) {
  out <- lda(x, data, ...)
  out$data <- data
  out
}
my.qda <- function(x, data, ...) {
  out <- qda(x, data, ...)
  out$data <- data
  out
}
simple.predict.da <- function(...) predict(...)$class
my.predict.da <- function(x, newdata, cutoff.data = x$data,
                          ..., measure = "max") {
  response <- model.frame(x$terms, cutoff.data)[,
                                                1]
  opt <- roc.opt(predict(x, cutoff.data)$posterior[,
                                                   2], as.numeric(response), measure = measure)
  cutoff <- opt$cutoff
  factor(as.integer(predict(x, newdata = newdata)$posterior[,
                                                            2] > cutoff), labels = levels(response))
}

# main
# part 1

pdata <- read.csv(file = "data/parkinsons.csv", comment.char="#")
pdata$status <- factor(pdata$status, labels = c("Healthy", "Sick"))
contrasts(pdata$status)
# pdata.noname <- subset(pdata, select = -c(name))
# pdata.noname.corrected = pdata.noname
# pdata.noname.corrected$MDVP.Jitter.Abs. <- pdata.noname.corrected$MDVP.Jitter.Abs. * 1000
# #lda.model <- lda(status ~ ., data = pdata.noname.corrected)
# #lda.model
# 
# mod.glm <- glm(status ~ ., 
#                data = pdata.noname.corrected, 
#                family = binomial(link = "logit"))
# summary(mod.glm)
# 
# tn.logit <- tune(glm,
#                  status ~ ., 
#                  data = pdata.noname.corrected, 
#                  family = binomial(link = "logit"),
#                  predict.func = simple.predict.glm,
#                  tunecontrol = tune.control(sampling = "cross", cross = 10))
# tn.logit$performances
# 
# mod.mln <- multinom(status ~ ., 
#                 data = pdata.noname.corrected,
#                 trace = FALSE)
# summary(mod.mln)
# 
# tn.mln <- tune(multinom, 
#                status ~ ., 
#                data = pdata.noname.corrected,
#                trace = FALSE)
# tn.mln$performances
# 
# tn.nb <- tune(naiveBayes, status ~ ., data = pdata.noname.corrected)
# tn.nb$performances
# 
# tn.lda <- tune(lda, 
#                status ~ ., 
#                data = pdata.noname.corrected, 
#                predict.func = simple.predict.da,
#                tunecontrol = tune.control(sampling = "cross", cross = 10))
# tn.lda$performances
# 
# tn.qda <- tune(qda, 
#                status ~ ., 
#                data = pdata.noname.corrected, 
#                predict.func = simple.predict.da,
#                tunecontrol = tune.control(sampling = "cross", cross = 10))
# tn.qda$performances
# 
# mod.glm.aic <- stepAIC(mod.glm)
# mod.mln.aic <- stepAIC(mod.mln)
# 
# tn.lda <- tune(lda, 
#                status ~ MDVP.Jitter... + MDVP.PPQ + Jitter.DDP + MDVP.APQ + 
#                  Shimmer.DDA + RPDE + spread2 + PPE, 
#                data = pdata.noname.corrected, 
#                predict.func = simple.predict.da,
#                tunecontrol = tune.control(sampling = "cross", cross = 10))
# tn.lda$performances
# 
# splom(subset(pdata.noname.corrected, 
#              select = c(MDVP.Jitter..., MDVP.PPQ, Jitter.DDP, MDVP.APQ, 
#                           Shimmer.DDA, RPDE, spread2, PPE)))
# 
# tn.lda <- tune(lda, 
#                status ~ MDVP.Jitter... + MDVP.PPQ + Jitter.DDP + MDVP.APQ + 
#                  Shimmer.DDA + RPDE + spread2 + PPE, 
#                data = pdata.noname.corrected, 
#                predict.func = simple.predict.da,
#                tunecontrol = tune.control(sampling = "cross", cross = 10))
# tn.lda$performances
# 
# tn.lda <- tune(lda, 
#                status ~ MDVP.Jitter... + MDVP.PPQ + Jitter.DDP + 
#                                RPDE + spread2 + PPE, 
#                data = pdata.noname.corrected, 
#                predict.func = simple.predict.da,
#                tunecontrol = tune.control(sampling = "cross", cross = 10))
# tn.lda$performances
# 
# tn.logit <- tune(glm,
#                  status ~ MDVP.Jitter... + MDVP.PPQ + Jitter.DDP + MDVP.APQ + 
#                    Shimmer.DDA + RPDE + spread2 + PPE, 
#                  data = pdata.noname.corrected, 
#                  family = binomial(link = "logit"),
#                  predict.func = simple.predict.glm,
#                  tunecontrol = tune.control(sampling = "cross", cross = 10))
# tn.logit$performances
# 
# tn.logit <- tune(glm,
#                  status ~ MDVP.Jitter... + MDVP.PPQ + Jitter.DDP + 
#                    RPDE + spread2 + PPE, 
#                  data = pdata.noname.corrected, 
#                  family = binomial(link = "logit"),
#                  predict.func = simple.predict.glm,
#                  tunecontrol = tune.control(sampling = "cross", cross = 10))
# tn.logit$performances

# part 2

pdata.grouped = pdata
pdata.grouped$name = sapply(pdata.grouped$name, 
                            function(x) {x = as.character(x); substr(x, 1, nchar(x) - 2)})
pdata.grouped = aggregate(subset(pdata.grouped, select = c(-name, -status)), 
                          list(pdata.grouped$name, pdata.grouped$status), mean)
names(pdata.grouped)[2] = "status"
pdata.grouped <- subset(pdata.grouped, select = -c(Group.1))
pdata.grouped$MDVP.Jitter.Abs. <- pdata.grouped$MDVP.Jitter.Abs. * 1000

mod.glm <- glm(status ~ ., 
               data = pdata.grouped, 
               family = binomial(link = "logit"))
summary(mod.glm)

tn.logit <- tune(glm,
                 status ~ ., 
                 data = pdata.grouped, 
                 family = binomial(link = "logit"),
                 predict.func = simple.predict.glm,
                 tunecontrol = tune.control(sampling = "cross", cross = 10))
tn.logit$performances

mod.mln <- multinom(status ~ ., 
                    data = pdata.grouped,
                    trace = FALSE)
summary(mod.mln)

tn.mln <- tune(multinom, 
               status ~ ., 
               data = pdata.grouped,
               trace = FALSE)
tn.mln$performances

tn.nb <- tune(naiveBayes, status ~ ., data = pdata.grouped)
tn.nb$performances

tn.lda <- tune(lda, 
               status ~ ., 
               data = pdata.grouped, 
               predict.func = simple.predict.da,
               tunecontrol = tune.control(sampling = "cross", cross = 10))
tn.lda$performances

# tn.qda <- tune(qda, 
#                status ~ ., 
#                data = pdata.grouped, 
#                predict.func = simple.predict.da,
#                tunecontrol = tune.control(sampling = "cross", cross = 10))
# tn.qda$performances

mod.mln.aic <- stepAIC(mod.mln)

tn.lda <- tune(glm, 
               status ~ MDVP.Fhi.Hz. + MDVP.Jitter... + MDVP.RAP + MDVP.PPQ + 
                 MDVP.Shimmer.dB. + Shimmer.APQ5 + NHR + HNR + DFA + spread1 + D2, 
               data = pdata.grouped, 
               family = binomial(link = "logit"),
               predict.func = simple.predict.glm,
               tunecontrol = tune.control(sampling = "cross", cross = 10))
tn.lda$performances

tn.lda <- tune(lda, 
               status ~ MDVP.Fhi.Hz. + MDVP.Jitter... + MDVP.RAP + MDVP.PPQ + 
                 MDVP.Shimmer.dB. + Shimmer.APQ5 + NHR + HNR + DFA + spread1 + D2, 
               data = pdata.grouped, 
               predict.func = simple.predict.da,
               tunecontrol = tune.control(sampling = "cross", cross = 10))
tn.lda$performances

tn.mln <- tune(multinom, 
               status ~ MDVP.Fhi.Hz. + MDVP.Jitter... + MDVP.RAP + MDVP.PPQ + 
                 MDVP.Shimmer.dB. + Shimmer.APQ5 + NHR + HNR + DFA + spread1 + D2, 
               data = pdata.grouped,
               trace = FALSE)
tn.mln$performances

tn.nb <- tune(naiveBayes, 
              status ~ MDVP.Fhi.Hz. + MDVP.Jitter... + MDVP.RAP + MDVP.PPQ + 
                MDVP.Shimmer.dB. + Shimmer.APQ5 + NHR + HNR + DFA + spread1 + D2, 
              data = pdata.grouped)
tn.nb$performances

splom(subset(pdata.grouped, 
             select = c(MDVP.Fhi.Hz., MDVP.Jitter..., MDVP.RAP, MDVP.PPQ,  
                          MDVP.Shimmer.dB., Shimmer.APQ5, NHR, HNR, DFA, spread1, D2)))

tn.lda <- tune(lda, 
               status ~ MDVP.Fhi.Hz. + MDVP.Jitter... + MDVP.RAP + MDVP.PPQ + 
                 MDVP.Shimmer.dB. + Shimmer.APQ5 + NHR + HNR + DFA + spread1 + D2, 
               data = pdata.grouped, 
               predict.func = simple.predict.da,
               tunecontrol = tune.control(sampling = "cross", cross = 10))
tn.lda$performances

tn.mln <- tune(multinom, 
               status ~ MDVP.Fhi.Hz. + MDVP.Jitter... + MDVP.RAP + MDVP.PPQ + 
                 MDVP.Shimmer.dB. + Shimmer.APQ5 + NHR + HNR + DFA + spread1 + D2, 
               data = pdata.grouped,
               trace = FALSE)
tn.mln$performances