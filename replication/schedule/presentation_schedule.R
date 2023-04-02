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

lapply(c(""),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##############
# import data
##############

# student roster
student_roster <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2023/main/replication/schedule/student_roster.csv", na.strings = c("NA", "<NA>", "-"))

# randomly select order based on seeds from student IDs
set.seed(12345)
schedule_order <- sample(student_roster$First_name, dim(student_roster)[1], replace = F)

# resulting schedule posted here:
# https://docs.google.com/spreadsheets/d/183ejb-5JF-L3LJTydrQSbnQnyujg-4ep6ZQhKSRV1zw/edit#gid=0
