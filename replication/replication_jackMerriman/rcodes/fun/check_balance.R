
check.balance.rdd <- function(treat, covs, fom.rhs, data, placebo.coef = NULL, weights = NULL, digit = 2){
  
  if(is.null(placebo.coef)){
    placebo.coef <- treat
  }
  
  # subset
  df.trt <- data[data[,treat] == 1,]
  df.ctl <- data[data[,treat] == 0,]
  
  # n obs
  n.trt <- apply(df.trt[,covs], 2, function(x){sum(!is.na(x))})
  n.ctl <- apply(df.ctl[,covs], 2, function(x){sum(!is.na(x))})
  
  # means
  mean.trt <- apply(df.trt[,covs], 2, mean, na.rm = T)
  mean.ctl <- apply(df.ctl[,covs], 2, mean, na.rm = T)
  
  # variances
  var.trt <- apply(df.trt[,covs], 2, var, na.rm = T)
  var.ctl <- apply(df.ctl[,covs], 2, var, na.rm = T)
  
  # standardized difference
  std.dif <- (mean.trt - mean.ctl) / sqrt(var.trt + var.ctl)
  
  # variance ratio
  var.r <- var.trt / var.ctl
  
  # placebo tests
  betas <- p.vals <- n <- c()
  for(v in covs){
    idx.tmp <- !is.na(data[,v])
    est     <- felm(formula(paste0(v, fom.rhs)), data = data[idx.tmp,], weights = weights[idx.tmp])
    b       <- est$coefficients[placebo.coef,1]
    p       <- est$cpval[placebo.coef]
    betas   <- c(betas, b)
    p.vals  <- c(p.vals, p)
    n       <- c(n, est$N)
  }
  
  # return as df
  out.df <- data.frame(covariates     = covs,
                       n.treat        = n.trt,
                       n.control      = n.ctl,
                       mean.treat     = round(mean.trt, digit),
                       mean.control   = round(mean.ctl, digit),
                       std.difference = round(std.dif, digit),
                       var.ratio      = round(var.r, digit),
                       placebo.beta   = round(betas, digit),
                       placebo.p      = round(p.vals, digit),
                       placebo.n      = n, 
                       star           = ifelse(p.vals < 0.05, '*', ifelse(p.vals < 0.1, '.', '')))
  return(out.df)
}

