
# function for extracting results
extract.felm <- function(est, vnames, labels = NULL, digits = 2){
  
  # if no nan
  if(!any(is.nan(est$pval))){
    
    # summary
    suppressWarnings(
      sum.tmp <- summary(est)
    )
    
    # loop for each bw
    sum.df <- data.frame(x     = vnames,
                         coef  = sum.tmp$coefficients[vnames,1],
                         se    = sum.tmp$coefficients[vnames,2], 
                         lwr   = sum.tmp$coefficients[vnames,1] - qt(0.975, est$df)*sum.tmp$coefficients[vnames,2],
                         upr   = sum.tmp$coefficients[vnames,1] + qt(0.975, est$df)*sum.tmp$coefficients[vnames,2],
                         n     = sum.tmp$N,
                         df    = est$df,
                         p     = sum.tmp$coefficients[vnames,4],
                         star  = ifelse(sum.tmp$coefficients[vnames,4] < 0.05, '*', 
                                        ifelse(sum.tmp$coefficients[vnames,4] < 0.1, '.', '')))
  }
  
  # if nan
  else{
    sum.df <- data.frame(coef  = NA,
                         se    = NA,
                         lwr   = NA,
                         upr   = NA,
                         n     = NA, 
                         df    = NA,
                         p     = NA,
                         star  = '')
  }
  
  # add labels
  if(!is.null(labels)){
    for(i in length(labels):1){
      sum.df <- cbind(rep(labels[i], nrow(sum.df)), sum.df)
      if(is.null(names(labels))){
        colnames(sum.df)[1] <- paste0('label', i)
      }
      else{
        colnames(sum.df)[1] <- names(labels)[i]
      }
    }
  }
  
  # round
  if(!is.null(digits)){
    sum.df <- round.felm(sum.df, digits)
  }
  
  # rownames
  row.names(sum.df) <- 1:nrow(sum.df)
  
  # return
  return(sum.df)
}

# function for rounding
round.felm <- function(x, digits = 4){
  x[,c('coef', 'se', 'lwr', 'upr', 'n', 'p')] <- round(x[,c('coef', 'se', 'lwr', 'upr', 'n', 'p')], digits = digits)
  return(x)
}



