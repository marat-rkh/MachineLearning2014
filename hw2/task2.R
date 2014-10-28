library(MASS)
library(lattice)
library(latticeExtra)
library(e1071)

df <- read.csv2(file = "input-data/I.csv")
df <- subset(df, select = c(PPIND, NEW10, FULLTIME, IN_STATE, ROOM, ADD_FEE,
                            PH_D, GRADUAT, SAL_FULL, NUM_FULL))
df$PPIND <- factor(df$PPIND, labels = c("Public", "Private"))
df <- na.exclude(df)
df.priv <- subset(df, PPIND == "Private")

# all predictors with log
fit2 <- lm(NEW10 ~ FULLTIME + log(IN_STATE) + log(ROOM) + log(ADD_FEE) + 
           log(SAL_FULL) + PH_D + GRADUAT + NUM_FULL, data = df.priv)
summary(fit2)

# manual removing
fit2.manual <- lm(NEW10 ~ log(IN_STATE) + log(ROOM) + log(ADD_FEE) + 
                    log(SAL_FULL) + GRADUAT + NUM_FULL, data = df.priv)
summary(fit2.manual)
AIC(fit2.manual)
tune(lm, fit2.manual$call$formula, data = df.priv,
     tunecontrol = tune.control(sampling = "cross", cross = 36))
levelplot(cor(fit2.manual$model)^2, par.settings = list(regions = list(col = colorRampPalette(grey(1:0)))),
          scales = list(x = list(rot = 90)), xlab = "", ylab = "")

fit2.manual <- lm(NEW10 ~ log(IN_STATE) + log(ROOM) + log(ADD_FEE) + 
                                    GRADUAT + NUM_FULL, data = df.priv)
summary(fit2.manual)
AIC(fit2.manual)
tune(lm, fit2.manual$call$formula, data = df.priv,
     tunecontrol = tune.control(sampling = "cross", cross = 36))
levelplot(cor(fit2.manual$model)^2, par.settings = list(regions = list(col = colorRampPalette(grey(1:0)))),
          scales = list(x = list(rot = 90)), xlab = "", ylab = "")

fit2.manual <- lm(NEW10 ~ log(IN_STATE) + log(ROOM) + 
                    GRADUAT + NUM_FULL, data = df.priv)
summary(fit2.manual)
AIC(fit2.manual)
tune(lm, fit2.manual$call$formula, data = df.priv,
     tunecontrol = tune.control(sampling = "cross", cross = 36))
levelplot(cor(fit2.manual$model)^2, par.settings = list(regions = list(col = colorRampPalette(grey(1:0)))),
          scales = list(x = list(rot = 90)), xlab = "", ylab = "")

fit2.manual <- lm(NEW10 ~
                    GRADUAT + NUM_FULL, data = df.priv)
summary(fit2.manual)
AIC(fit2.manual)
tune(lm, fit2.manual$call$formula, data = df.priv,
     tunecontrol = tune.control(sampling = "cross", cross = 36))
levelplot(cor(fit2.manual$model)^2, par.settings = list(regions = list(col = colorRampPalette(grey(1:0)))),
          scales = list(x = list(rot = 90)), xlab = "", ylab = "")

# aic removing
fit2.aic <- stepAIC(fit2)
summary(fit2.aic)
AIC(fit2.aic)
tune(lm, fit2.aic$call$formula, data = df.priv,
     tunecontrol = tune.control(sampling = "cross", cross = 36))
levelplot(cor(fit2.aic$model)^2, par.settings = list(regions = list(col = colorRampPalette(grey(1:0)))),
          scales = list(x = list(rot = 90)), xlab = "", ylab = "")

fit2.aic <- update(fit2.aic, . ~ .  - log(IN_STATE))
summary(fit2.aic)
AIC(fit2.aic)
tune(lm, fit2.aic$call$formula, data = df.priv,
     tunecontrol = tune.control(sampling = "cross", cross = 36))
levelplot(cor(fit2.aic$model)^2, par.settings = list(regions = list(col = colorRampPalette(grey(1:0)))),
          scales = list(x = list(rot = 90)), xlab = "", ylab = "")


# general model
contrasts(df$PPIND) <- contr.treatment
contrasts(df$PPIND)
df$PPIND <- as.factor(df$PPIND)

gm <- lm(formula = NEW10 ~ (log(IN_STATE) + log(ADD_FEE) + GRADUAT + NUM_FULL) * PPIND, data = df)
summary(gm)
AIC(gm)
tune(lm, gm$call$formula, data = df,
     tunecontrol = tune.control(sampling = "cross", cross = 95))

gm.aic <- stepAIC(gm)
summary(gm.aic)
AIC(gm.aic)
tune(lm, gm.aic$call$formula, data = df,
     tunecontrol = tune.control(sampling = "cross", cross = 95))




