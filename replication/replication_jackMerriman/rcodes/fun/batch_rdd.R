
batch.rdd <- function(fom.rhs, data, ovs, bws, coefs = NULL, subset.list = NULL, labels = NULL, triangular.weights = T, digit = 2){
  
  require(dplyr)
  
  source('rcodes/fun/extract_felm.R')
  source('rcodes/fun/make_w.R')
  
  no.subset <- F
  if(is.null(subset.list)){
    subset.list <- list('No subset' = rep(T, nrow(data)))
    no.subset <- T
  }
  
  out.df <- data.frame()
  
  for(i in 1:length(ovs)){
    
    for(j in 1:length(bws)){
      
      for(k in 1:length(subset.list)){
          
        # subset 
        df.tmp <- data[subset.list[[k]],] %>% filter(abs(run_var) <= bws[j])
  
        # estimation
        fom.tmp  <- paste0(ovs[i], fom.rhs)
        if(triangular.weights){
          est.tmp  <- felm(formula(fom.tmp), data = df.tmp, weights = make.w(df.tmp$run_var, bws[j], drop.cut = F), psdef = F)
        }
        else{
          est.tmp  <- felm(formula(fom.tmp), data = df.tmp, weights = NULL, psdef = F)
        }
        
        # if coefs null
        if(is.null(coefs)){
          coefs <- rownames(est.tmp$coefficients)
        }  
        
        # loop for coefs
        for(l in 1:length(coefs)){
          
          # extract quantity
          ext.tmp <- extract.felm(est.tmp, coefs[l], digit = digit,
                                  labels = c(ov     = if(is.null(names(ovs)[i])){ovs[i]}else{names(ovs)[i]}, 
                                             var    = if(is.null(names(coefs)[l])){coefs[l]}else{names(coefs)[l]}, 
                                             bw     = if(is.null(names(bws)[j])){bws[j]}else{names(bws)[j]},
                                             subset = if(is.null(names(subset.list)[k])){subset.list[k]}else{names(subset.list)[k]}))
          out.df <- bind_rows(out.df, ext.tmp)
        }
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


