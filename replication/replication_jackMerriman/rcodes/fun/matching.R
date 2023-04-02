
matching <- function(group.id, treat, p.score, caliper = 0.01){
  
  require(MatchIt)
  
  # make dataset
  df.tmp <- data.frame(group.id, treat, p.score, n = 1)
  df.tmp <- df.tmp[complete.cases(df.tmp),]
  df.agg <- aggregate(cbind(treat, p.score) ~ group.id, df.tmp, first)
  df.agg <- merge(df.agg, aggregate(n ~ group.id, df.tmp, sum), by = 'group.id')
  
  # matching
  set.seed(123)
  m <- matchit(treat ~ p.score, data = df.agg, method = 'nearest', estimand = 'ATT', distance = df.agg$p.score, replace = F, caliper = caliper, std.caliper = F, m.order = 'random', discard = 'both')
  
  # get ids
  mm   <- na.omit(m$match.matrix)
  ids1 <- df.agg[rownames(mm),'group.id']
  ids2 <- df.agg[mm[,1], 'group.id']
  ids  <- c(ids1, ids2)

  # merge the number of observations
  df.w <- merge(data.frame(ids = 1:length(group.id), group.id, treat, p.score), df.agg[,c('group.id', 'n')], by = 'group.id', all.x = T, all.y = F)
  
  # get output
  m <- (df.w$group.id %in% ids)
  
  # reorder
  m <- m[order(df.w$ids)]
  
  # return
  return(m)
}

matching.cem <- function(group.id, treat, covs, cut.p){
  
  require(MatchIt)
  
  # make dataset
  df.tmp <- cbind(data.frame(group.id = group.id, treat = treat, n = 1), covs)
  df.tmp <- df.tmp[complete.cases(df.tmp),]
  df.agg <- aggregate(x = df.tmp[,c('treat', colnames(covs))], by = list(group.id = df.tmp$group.id), first)
  df.agg <- merge(df.agg, aggregate(n ~ group.id, df.tmp, sum), by = 'group.id')
  
  # matching
  set.seed(123)
  m <- matchit(formula(paste0('treat ~', paste(colnames(covs), collapse = '+'))), 
               data = df.agg, method = 'cem', cutpoints = cut.p, k2k=T, estimand='ATT')
  
  # get ids
  mm   <- na.omit(m$match.matrix)
  ids1 <- df.agg[rownames(mm),'group.id']
  ids2 <- df.agg[mm[,1], 'group.id']
  ids  <- c(ids1, ids2)
  
  # merge the number of observations
  df.w <- merge(data.frame(ids = 1:length(group.id), group.id), df.agg[,c('group.id', 'n')], by = 'group.id', all.x = T, all.y = F)
  
  # get output
  m <- (df.w$group.id %in% ids)
 
  # reorder
  m <- m[order(df.w$ids)]
  
  # return
  return(m)
}