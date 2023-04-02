##################################
# Tutorial 7: Survival Analysis #
##################################

data(child)

#### Completed
# a)
child_surv <- with(child, Surv(enter, exit, event))

km <- survfit(child_surv ~ 1, data = child)
summary(km, times = seq(0, 15, 1))
plot(km, main = "Kaplan-Meier Plot", xlab = "Years", ylim = c(0.7, 1))
autoplot(km)

km_socBranch <- survfit(child_surv ~ socBranch, data = child)
autoplot(km_socBranch)

# b)
cox <- coxph(child_surv ~ sex + socBranch, data = child)
summary(cox)
drop1(cox, test = "Chisq")
stargazer(cox, type = "text")

# There is a 0.08 decrease in the expected log of the hazard for female babies compared to 
# male, holding socBranch constant. There is a 0.34 increase in the expected log of the hazard
# for babies of businessmen compared to officials, holding sex constant.

# exponentiate parameter estimates to obtain hazard ratios
exp(-0.083546)
# The hazard ratio of female babies is 0.92 that of male babies, i.e. female babies are less
# likely to die (92 female babies die for every 100 male babies; female deaths are 8% lower, etc.)

cox_fit <- survfit(cox)
autoplot(cox_fit)

newdat <- with(child, 
               data.frame(
                 sex = c("male", "female"), socBranch="official"
                 )
               )

plot(survfit(cox, newdata = newdat), xscale = 12,
     conf.int = T,
     ylim = c(0.6, 1),
     col = c("red", "blue"),
     xlab = "Time",
     ylab = "Survival proportion",
     main = "")
legend("bottomleft",
       legend=c("Male", "Female"),
       lty = 1, 
       col = c("red", "blue"),
       text.col = c("red", "blue"))
# Note: the confidence intervals on this plot are for the prediction, not
# the standard error of the terms in the model (the effect of sex in the 
# cox ph model was significant, here the CIs overlap. Always check your
# results and interpretation!)


# Adding an interaction
cox.int <- coxph(child_surv ~ sex * socBranch, data = child)
summary(cox.int)
drop1(cox.int, test = "Chisq")
stargazer(cox.int, type = "text")
