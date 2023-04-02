make.w <- function(x, bw, cut = 0, drop.cut = T){
  w        <- (bw+1) - abs(x-cut)
  w[w < 0] <- 0
  if(drop.cut){
    w           <- w/bw
    w[x == cut] <- 0
  }
  else{
    w <- w/(bw+1)
  }
  return(w)
}