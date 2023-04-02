
rm(list = ls()); invisible(gc())

library(tidyverse)
library(lfe)

source('rcodes/fun/extract_felm.R')

# load data
df.sc <- readRDS('data/soccer.rds') %>% filter(league == 'Premier League') %>% select(team, match_id, match_date, birth_country, player_id, absent)
df.tw <- readRDS('data/twitter_cleaned.rds')

# keep the last season up to 2015-04-21
tw.date <- as.Date(unique(df.tw$date))
df.sc   <- df.sc %>% filter(between(match_date, as.Date('2014-08-01'), tw.date))

# aggregate the data to player level
df.plyr <- df.sc %>% group_by(player_id, birth_country, team) %>% summarise(n.belong = n(), n.present = sum(1-absent), .groups = 'keep')
m       <- median(df.plyr$n.present)
df.plyr <- df.plyr %>% mutate(dum.belong = as.numeric(n.belong > 0), dum.present = as.numeric(n.present >= m), dum.absent = as.numeric((n.present < m)))

# aggregate the data to team levels
df.team  <- df.plyr %>% group_by(birth_country, team) %>% summarize(dum.belong  = as.numeric(sum(dum.belong)  > 0), 
                                                                    dum.present = as.numeric(sum(dum.present) > 0), 
                                                                    dum.absent  = as.numeric(sum(dum.absent)  > 0 & sum(dum.present) == 0), .groups = 'keep')

# merge it to twitter data
df <- left_join(df.tw, df.team, c('team' = 'team', 'country' = 'birth_country'))

# fill na
df[is.na(df)] <- 0

# drop countries that are not included in the event data analysis
df <- df %>% filter(!country %in% c("Cape Verde", "Comoros", "Botswana", "Djibouti", "Eritrea", "Ethiopia", "Lesotho", "Mauritius", "Rwanda", "Sudan", "Tanzania"))

# analysis
est1 <- felm(follower ~ dum.belong | team + country | 0 | country, data = df)
est2 <- felm(follower ~ dum.present + dum.absent | team + country | 0 | country, data = df)

# extract the main results
out.df <- rbind(extract.felm(est1, 'dum.belong'), 
                extract.felm(est2, c('dum.present', 'dum.absent')))

# save it
write.csv(out.df, 'results/tab_a2_1_a.csv', row.names = F)


