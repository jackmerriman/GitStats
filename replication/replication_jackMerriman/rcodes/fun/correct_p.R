
# function for correcting multiple hypothesis testing
correct.p <- function(x, group.col = NULL, method = 'BH', digits = 2){
  
  # p-values
  if(is.null(group.col)){
    p <- p.adjust(x$p, method = method)
  }
  else{
    p      <- rep(NA, length(x$p))
    groups <- x[,group.col]
    for(i in unique(groups)){
      p[groups == i] <- p.adjust(x$p[groups == i], method = method)
    }
  }
  star <- ifelse(p < 0.05, '*', ifelse(p < 0.1, '.', ''))
  
  # CIs
  q   <- 0.05 * x$p/p
  lwr <- x$coef - qt(1-(q/2), x$df)*x$se
  upr <- x$coef + qt(1-(q/2), x$df)*x$se
  
  # insert and return
  x$p    <- p
  x$star <- star
  x$lwr  <- lwr
  x$upr  <- upr
  
  # rounding
  if(!is.null(digits)){
    n     <- unlist(lapply(x, is.numeric))  
    x[,n] <- round(x[,n], digits)
  }
  
  return(x)
}
