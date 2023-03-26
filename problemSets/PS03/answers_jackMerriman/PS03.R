pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c('tidyverse', 'MASS', 'nnet', 'reshape', 'stargazer'),  pkgTest)

dat <- read.csv("gdpChange.csv")
df <- dat[,c(10,6,7)]

#replace numerical values with categories
for (i in 1:length(df$GDPWdiff)) {
  if (df$GDPWdiff[i] > 0) {
    df$GDPWdiff[i] <- "positive"
  }
  if (df$GDPWdiff[i] < 0) {
    df$GDPWdiff[i] <- "negative"
  }
  if (df$GDPWdiff[i] == 0) {
    df$GDPWdiff[i] <- "no change"
  }
}
#turn the column into an unordered factor
df$GDPWdiff <- factor(df$GDPWdiff, c("no change", "negative", "positive"),
                      ordered = FALSE)
#create an unordered regression model
model1 <- multinom(GDPWdiff ~ OIL + REG, data = df)
summary(model1)
stargazer(model1)
#create predicted values for estimating cutoffs
unorderedPredict <- data.frame('OIL' = c(0,1,0,1),'REG' = c(0,0,1,1))
predictValues1 <- predict(model1, unorderedPredict, type = 'probs')
rownames(predictValues1) <- c('<50%, non-democracy', '>50%, non-democracy',
                              '<50%, democracy', '>50%, democracy')

predictValues1

cdfNC <- ecdf(predictValues1[,'no change'])
cdfNeg <- ecdf(predictValues1[,'negative'])
cdfPos <- ecdf(predictValues1[,'positive'])

cutoff1 <- uniroot(function(x) cdfNC(x) - cdfNeg(x), c(0, 1))$root
cutoff1
predictValues1
plot(cdfNC, xlim = c(-1, 1), main = "ECDF for 'no change' category")
lines(cdfNeg, col='red')
lines(cdfPos, col='green')

exp(coef(model1)[,c(1:3)])

set.seed(2023)
#Make the factor ordered
df$oGDPWdiff <- factor(factor(df$GDPWdiff, c("negative", "no change", "positive"),
                              ordered = TRUE))
#Run ordered logit regression
model2 <- polr(oGDPWdiff ~ OIL + REG, data = df, Hess = TRUE)
summary(model2)
stargazer(model2)
#Predict values
predictValues2 <- predict(model2, unorderedPredict, type = 'probs')
rownames(predictValues2) <- c('<50%, non-democracy', '>50%, non-democracy',
                              '<50%, democracy', '>50%, democracy')
predictValues2

## Question 2

mexico <- read.csv('MexicoMuniData.csv')
#create poisson regression
mexmodel <- glm(
  PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
  data = mexico,
  family = poisson
  )
summary(mexmodel)
stargazer(mexmodel)
exp(coef(mexmodel))

hypDistrict <- data.frame('competitive.district' = 1,
                          'marginality.06' = 0,
                          'PAN.governor.06' = 1)

#Set all coefficients to 1 as they are intercept and dummy variables. Marginality to 0
hypLambda <- exp(coefs[1] + coefs[2]*0 + coefs[3] + coefs[4])
hypLambda

exp(predict(mexmodel, hypDistrict))
