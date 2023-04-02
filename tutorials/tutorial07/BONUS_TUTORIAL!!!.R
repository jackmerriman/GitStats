######################################
# BONUS TUTORIAL!!! Tobit Regression #
######################################

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

lapply(c("tidyverse", "VGAM"),  pkgTest)

#### Tobit Regression: regression models for censored and truncated data

# Long (1997) provides a dataset of factors affecting a scientist's first academic job. The 
# outcome variable, `job`, is the prestige of the first job, rated 1 to 5 (higher is more 
# distinguished). Other variables include:
# - `fem`: 1 if female, 0 if male
# - `phd`: prestige of PhD department
# - `ment`: citations received by mentor
# - `fel`: 1 if fellowship held, otherwise 0
# - `art`: number of articles published
# - `cit`: number of citations received

# a) Explore the distribution of `job`. What conclusions do you draw regarding the original 
# coding of the `job` variable? what effects could this have on the possible use of this 
# variable as an outcome in a regression model? Run an OLS model with `jobs` as the dependent 
# variable. Comment on the results.

# b) The `jobcen` variable is a recoding of `job` such that values equal to or less than 1
# are scored 1. Run an OLS model using this *censored* variable as the outcome. How do the 
# results differ from the previous model? 

# c) Drop all observations from the dataset where `jobcen` is less than or equal to 1. Run an
# OLS model on this *truncated* dataset. How do the results differ from the previous models?

# d) Finally, using `job` as the dependent variable, run a tobit regression using the vglm()
# function from the VGAM package. You will need to set the arguments correctly, so read the
# help file for this function. Comment on the results.
