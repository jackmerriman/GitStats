library(tidyverse)
library(alpaca)
#to be run in conjunction with describe_soccer.R
showdata.players <- df[c(1,20000,50000,100000,150000,175000),]
showdata.matches <- df.fd[c(1, 10000,20010,60000,70000,75000),]

##TWIST, MEASURING THE EFFECT OF A DRAW
#to be run in conjunction with analyze_panel.R

source('rcodes/fun/extract_felm.R')
source('rcodes/fun/batch_did.R')
source('rcodes/fun/matching.R')
source('rcodes/fun/summary2.R')

# load data
df <- readRDS('data/panel_final_birth.rds')
colnames(df)

# drop if there are players from the same country in opposing teams
df <- df %>% filter(both_teams_birth == 0)
jack.df <- df
# subset to win-even and lose-even data
df1 <- df %>% filter(result != 'win')
df2 <- df %>% filter(result != 'lose')

# calculate propensity score (%)
df1 <- df1 %>% mutate(p_score = 100 * lose_odds / (lose_odds + even_odds))
df2 <- df2 %>% mutate(p_score = 100 * win_odds  / (win_odds + even_odds))
jack.df <- jack.df %>% mutate(p_score = 100 * even_odds)
# subset to close games
df.c1 <- df1 %>% filter(abs(score_dif) < 2)
df.c2 <- df2 %>% filter(abs(score_dif) < 2)
df.c  <- df  %>% filter(abs(score_dif) < 2)

########################################################################################################################################
### Matching

# close games
df.c1$m <- matching(group.id  = df.c1$match_team_id, 
                    treat     = df.c1$lose, 
                    p.score   = df.c1$p_score, caliper = 5)
df.c2$m <- matching(group.id  = df.c2$match_team_id, 
                    treat     = df.c2$win, 
                    p.score   = df.c2$p_score, caliper = 5)
jack.df$m <- matching(group.id  = jack.df$match_team_id, 
                      treat     = jack.df$even, 
                      p.score   = jack.df$p_score, caliper = 5)

df.m1 <- df.c1 %>% filter(m == 1)
df.m2 <- df.c2 %>% filter(m == 1)
jack.m <- jack.df %>% filter(m == 1)
# all games
df1$m <- matching(group.id  = df1$match_team_id, 
                  treat     = df1$lose, 
                  p.score   = df1$p_score, caliper = 5)
df2$m <- matching(group.id  = df2$match_team_id, 
                  treat     = df2$win, 
                  p.score   = df2$p_score, caliper = 5)

########################################################################################################################################
### Table 2

# outcome variables
c.types         <- c(1, 2, 3)
out.vars        <- paste0('acled_event_type', c.types, '_dif3')
names(out.vars) <- c('Demonstration', 'Riot', 'Battle')

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.m1, out.vars, 'lose')
ext.tmp2 <- batch.did(' ~ win  | 0 | 0 | birth_country + match_id', df.m2, out.vars, 'win')
jack.tmp <- batch.did(' ~ even  | 0 | 0 | birth_country + match_id', jack.m, out.vars, 'even')

out.df   <- bind_rows(ext.tmp1, ext.tmp2)
jack.df <- bind_rows(out.df, jack.tmp)
out.df
jack.df


# save
write.csv(jack.df, 'twist/tab_1.csv', row.names = F)

#####FIGURE 3

# outcome variables
d.seq           <- c(paste('l', 3:1, sep = ''), 0:3)
out.vars        <- paste0('acled_event_type1_', d.seq)
names(out.vars) <- d.seq

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.m1, out.vars, 'lose', labels = c(model = 1))
ext.tmp2 <- batch.did(' ~ win  | 0 | 0 | birth_country + match_id', df.m2, out.vars, 'win', labels = c(model = 2))
jack.tmp <- batch.did(' ~ even  | 0 | 0 | birth_country + match_id', jack.m, out.vars, 'even', labels = c(model = 3))
out.df   <- bind_rows(ext.tmp1, ext.tmp2)
jack.df2 <- bind_rows(out.df, jack.tmp)

# saving it
write.csv(jack.df2, 'twist/fig_3.csv', row.names = F)

# graph
gg.df <- out.df %>%  mutate(time = gsub('l', '-', ov) %>% as.numeric(.)) %>% mutate(sig = (star == '*'))
jack.gg <- jack.df2 %>%  mutate(time = gsub('l', '-', ov) %>% as.numeric(.)) %>% mutate(sig = (star == '*'))
gg <- ggplot(gg.df, aes(x = time, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_line(size = 0.1, col = 'gray40') + geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) + 
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.1, size = 2.5) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  scale_x_continuous(breaks = -3:3) +
  
  ylab('Change in Demonstration Probability (%)') + xlab('Days from a Football Game') +
  theme_tufte() + theme(legend.position = 'none', strip.placement = 'outside') +
  
  facet_grid(rows = vars(x), switch = 'y', labeller =  as_labeller(c(lose = 'Loss', win = 'Win')), scales = 'free_y')

jack.plot <- ggplot(jack.gg, aes(x = time, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_line(size = 0.1, col = 'gray40') + geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) + 
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.1, size = 2.5) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  scale_x_continuous(breaks = -3:3) +
  
  ylab('Change in Demonstration Probability (%)') + xlab('Days from a Football Game') +
  theme_tufte() + theme(legend.position = 'none', strip.placement = 'outside') +
  
  facet_grid(rows = vars(x), switch = 'y', labeller =  as_labeller(c(lose = 'Loss', win = 'Win', even = 'Draw')), scales = 'free_y')

jack.plot

gg

# save
ggsave('results/fig_3.png', gg, height = 5, width = 6)
ggsave('results/eps/fig_3.eps', gg, height = 5, width = 6, device = cairo_ps)

