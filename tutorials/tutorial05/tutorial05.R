#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c('tidyverse', 'MASS', 'nnet', 'reshape'),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Ordered multinomial logits:
  
  # This data set is analyzed by Long (1997).  The response variable has four ordered categories:
  # Strongly Disagree, Disagree, Agree, and Strongly Agree in relation to the statement
  # “A working mother can establishjust as warm and secure a relationship with her children as a mother who does not work."
  
  # The explanatory variables are:
  # the year of the survey (1977 or 1989),
  # the gender of the respondent, 
  # the race of the respondent (white or non-white), 
  # the respondent’s age, and 
  # the prestige of the respondent’s occupation (a quantitative variable)

workingMoms <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/WorkingMoms.txt", header=T)

# (a) Perform an ordered (proportional odds) logistic regression of attitude toward working mothers on the other variables.
# What conclusions do you draw?

workingMoms$attitude <- factor(workingMoms$attitude, ordered=TRUE,
                               levels = c('SD', 'D', 'A', 'SA'))
str(workingMoms)
?polr

model <- polr(attitude ~ age + prestige + gender + year + race,
              data=workingMoms, Hess=TRUE)

summary(model)
summary_table <- coef(summary(model))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

ci <- confint(model)
ci

# (b) Assess whether the proportional-odds assumption appears to hold for this regression. 
# Fit a multinomial logit model to the data, and compare and contrast the results with those from the proportional odds model.

for(i in 1:length(unique(workingMoms$attitude))) {
  assign(paste("model", i, sep = ""),
         glm(ifelse(attitude==unique(workingMoms$attitude)[i], 1, 0)
             ~ age + prestige + gender + year + race, data = workingMoms,),
         envir=globalenv())
}

model1
model2
model3
model4

workingMoms %>%
  ggplot(aes(x=prestige))+
  geom_histogram(binwidth = 10)

predict_data <- data.frame(
  age = rep(c(20,40,60,80), each = 24), prestige = rep(c(25,50,75), 4, each = 8), 
  gender = rep(c('Female', 'Male'), 12, each = 4), year = rep(c('Year1977', 'Year1989'), 24, each = 2),
  race = rep(c(0, 1), 48)
  )

predict_data
predict_data1 <- data.frame(gender = rep(c("Female", "Male"), each = 2),
                            year = rep(c("Year1977", "Year1989"), 2))
predict_data1

plot_data <- melt(cbind(predict_data, predict(model, predict_data,
                        type = "probs")), id.vars = colnames(predict_data),
                  variable.name = "Level", value.name = 'Probability')

plot_data %>%
  filter(year == 'Year1977')%>%
  ggplot(aes(x = as.factor(age), y = value))+
  geom_boxplot()

# (c) Consider that possibility that gender interacts with the other explanatory variables in influencing the response variable. 
# What do you find?