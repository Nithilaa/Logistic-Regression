library(MASS)
library(plyr)

college <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
college <- transform(college, rank = as.factor(rank))

admit.logit <- glm(admit ~ ., data = college, family = "binomial")
summary(admit.logit)

coef(admit.logit)["gpa"]

or.gpa <- round(exp(coef(admit.logit)["gpa"]), 3)
or.gpa

anova(update(admit.logit, . ~ . - rank), admit.logit, test = "Chisq")

coef(admit.logit)["rank3"]

# Exponentiate to get odds ratio
or.rank3 <- round(exp(coef(admit.logit)["rank3"]), 3)
or.rank3

predict(admit.logit, newdata = data.frame(gre = 670, gpa = 3.2, rank = as.factor(1)))

predict(admit.logit, newdata = data.frame(gre = 750, gpa = 3.7, rank = as.factor(2)))

predict(admit.logit, newdata = data.frame(gre = 750, gpa = 3.7, rank = as.factor(2)),
        type = "response")


plot(college$admit ~ college$gpa + college$gre, col="red4")
lines(college$admit ~ college$gpa + college$gre, col="green4", lwd=2)