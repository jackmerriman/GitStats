
summary2<- function(data, digits = 2){
  
  data.frame(var  = colnames(data),
             mean = round(apply(data, 2, mean, na.rm = T), digits),
             sd   = round(apply(data, 2, sd, na.rm = T), digits),
             min  = round(apply(data, 2, min, na.rm = T), digits),
             max  = round(apply(data, 2, max, na.rm = T), digits),
             n    = apply(data, 2, function(x){sum(!is.na(x))}))
  
}