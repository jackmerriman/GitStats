
batch.did <- function(fom.rhs, data, ovs, coefs = NULL, subset.list = NULL, labels = NULL, weights = NULL, digit = 2){
  
  require(dplyr)
  
  source('rcodes/fun/extract_felm.R')
  
  no.subset <- F
  if(is.null(subset.list)){
    subset.list <- list('No subset' = rep(T, nrow(data)))
    no.subset <- T
  }
  
  out.df <- data.frame()
  
  for(i in 1:length(ovs)){
      
    for(j in 1:length(subset.list)){
        
      # subset 
      df.tmp <- data[subset.list[[j]],]
      w      <- weights[subset.list[[j]]]
        
      # estimation
      fom.tmp  <- paste0(ovs[i], fom.rhs)
      est.tmp  <- felm(formula(fom.tmp), data = df.tmp, weights = w, psdef = F)
              
      # if coefs null
      if(is.null(coefs)){
        coefs <- rownames(est.tmp$coefficients)
      }  
      
      # loop for coefs
      for(k in 1:length(coefs)){

        # extract quantity
        ext.tmp <- extract.felm(est.tmp, coefs[k], digit = digit,
                                labels = c(ov     = if(is.null(names(ovs)[i])){ovs[i]}else{names(ovs)[i]}, 
                                           var    = if(is.null(names(coefs)[k])){coefs[k]}else{names(coefs)[k]},
                                           subset = if(is.null(names(subset.list)[j])){subset.list[j]}else{names(subset.list)[j]}))
        out.df <- bind_rows(out.df, ext.tmp)
      }
    }
  }
  
  if(no.subset){
    out.df <- out.df[,colnames(out.df) != 'subset']
  } 
  
  # add labels
  if(!is.null(labels)){
    for(i in length(labels):1){
      out.df <- cbind(rep(labels[i], nrow(out.df)), out.df)
      if(is.null(names(labels))){
        colnames(out.df)[1] <- paste0('label', i)
      }
      else{
        colnames(out.df)[1] <- names(labels)[i]
      }
    }
  }
  
  rownames(out.df) <- 1:nrow(out.df)
  
  return(out.df)
}


