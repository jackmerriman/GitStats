library(tidyverse)
library(survival)
library(stargazer)
#load data
library(eha)
data <- child
str(child)
head(child)

#create a survival object
survival <- Surv(data$enter, data$exit, data$event)
#specify the Cox Proportional Hazard model
mod1 <- coxph(survival ~ m.age + sex, data = data)
stargazer(mod1)

#chisq test
modnull <- coxph(survival ~ 1, data = data)
stargazer(anova(modnull, mod1))

#interpret coefficients through exponentiation
stargazer(exp(mod1$coefficients))

#specify interactive model
mod2 <- coxph(survival ~ m.age * sex, data = data)
stargazer(mod2)
#chisq test
stargazer(anova(mod1, mod2))

#create a fitted values object for plotting
modfit <- survfit(mod1)
plot(modfit, ylim = c(0.7,1), xlab = 'Time', ylab = 'Survival Proportion',
     main = 'Plot of Additive Model')
legend('topright',legend = c('predicted values',  'confidence intervals'),
       lty = c(1,2), cex = 0.8)

#Find the modal age to set a base value for the predicted sex data
table(round(data$m.age))
#modal age is 30 with 1490 observations
predat1 <- with(child, data.frame(sex = c('male', 'female'), m.age = 30.0))
plot(survfit(mod1, newdata = predat1), conf.int = TRUE, ylim = c(0.7,1),
     col = c('blue', 'red'), xlab = 'Time', ylab = 'Survival Proportion',
     main = 'Predicted Values for Sex')
legend('topright',legend = c('boy', 'girl', 'confidence intervals'),
       col = c('blue', 'red', 'black'), lty = c(1,1,2), cex = 0.8)

#Fit predicted age data to ten either side of the modal age
predat2 <- with(child, data.frame(sex = 'male', m.age = c(20.0, 30.0, 40.0)))
plot(survfit(mod1, newdata = predat2), conf.int = TRUE, ylim = c(0.7,1),
     col = c('darkgreen', 'orange', 'purple'), xlab = 'Time', ylab = 'Survival Proportion',
     main = 'Predicted Values for Age')
legend('topright',legend = c('20 year-old mother', '30 year-old mother',
                              '40 year-old mother', 'confidence intervals'),
       col = c('darkgreen', 'orange', 'purple', 'black'),
       lty = c(1,1,1,2), cex = 0.8)

