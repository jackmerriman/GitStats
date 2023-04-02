
rm(list = ls()); invisible(gc())

library(tidyverse)
library(ggthemes)

########################################################################################################################################
### Preparation

# load soccer data
df    <- readRDS('data/soccer.rds')
df.fd <- readRDS('data/fd_cleaned_all.rds')

# subset to 2005-2018 seasons
df1    <- df    %>% filter(between(season, 2005, 2018))
df.fd1 <- df.fd %>% filter(between(match_date, min(as.Date("2005-07-12")), as.Date("2019-06-01")))

########################################################################################################################################
### Figure 1

df.gg <- aggregate(df$player_id, list(year=df$season), function(x){length(unique(x))})
for(y in 1948:2019){
  if(all(df$season != y)){df.gg <- bind_rows(df.gg, c(year=y, x=0))}
}
gg    <- ggplot(df.gg, aes(x = year, y = x)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = seq(1945, 2020, 5)) +
  xlab(NULL) + ylab('Number of Players Born in Africa') + 
  theme_tufte()
gg

ggsave('results/fig_1.png', gg, height = 6, width = 6)
ggsave('results/eps/fig_1.eps', gg, height = 6, width = 6, device = cairo_ps)

########################################################################################################################################
### p.5

# Number of African players
n.players <- unique(length(unique(df1$player_id)))
print('Number of African players')
print(n.players)

# % of games with African players
n.games        <- nrow(df.fd1)/2                                    # total N of games
n.games.appear <- length(unique(df1$match_id[df1$pitch == 1]))      # N of games of African players
print('% of games with African players')
print(100 * n.games.appear / n.games)

# Number of African players per game
n.players.per.game <- sum(df1$pitch)/n.games
print('Number of African players per game')
print(n.players.per.game)

# Origins of African players
l.regions <- sort(table(df1$birth_subregion[!duplicated(df1$player_id)]))
print('% of African players by region')
print(100 * l.regions / sum(l.regions))

# Leagues of African players
l.leagues <- sort(table(df1$league[!duplicated(df1$player_id)]))
print('% of African players by leagues')
print(100 * l.leagues / sum(l.leagues))

########################################################################################################################################
### Figure A1-1

df.gg <- df1 %>% distinct(player_id, .keep_all = T) %>% group_by(birth_country) %>% tally() %>% arrange(n) %>% 
  mutate(birth_country = factor(birth_country, levels = unique(birth_country)))
gg    <- ggplot(df.gg, aes(x = n, y = birth_country)) +
  geom_bar(stat="identity") +
  ylab(NULL) + xlab('Number of Players') + 
  theme_tufte()
gg

ggsave('results/fig_a1_1.png', gg, height = 6, width = 6)
ggsave('results/eps/fig_a1_1.eps', gg, height = 6, width = 6, device = cairo_ps)
