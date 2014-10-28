library(lattice)
library(MASS)

# creating dataframe
df <- read.table("input-data/teengambling.txt")
df$sex <- as.factor(df$sex)
contrasts(df$sex) <- contr.treatment
contrasts(df$sex)

# creating model
l <- lm(gamble ~ . ^2, data = df)
laic <- stepAIC(l)
summary(laic)
