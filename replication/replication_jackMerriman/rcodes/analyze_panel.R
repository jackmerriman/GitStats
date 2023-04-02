
rm(list = ls()); invisible(gc())

library(tidyverse)
library(ggthemes)
library(MatchIt)
library(lfe)

source('rcodes/fun/extract_felm.R')
source('rcodes/fun/batch_did.R')
source('rcodes/fun/matching.R')
source('rcodes/fun/summary2.R')
432-69
# load data
df <- readRDS('data/panel_final_birth.rds')
colnames(df)

# drop if there are players from the same country in opposing teams
df <- df %>% filter(both_teams_birth == 0)

# subset to win-even and lose-even data
df1 <- df %>% filter(result != 'win')
df2 <- df %>% filter(result != 'lose')

# calculate propensity score (%)
df1 <- df1 %>% mutate(p_score = 100 * lose_odds / (lose_odds + even_odds))
df2 <- df2 %>% mutate(p_score = 100 * win_odds  / (win_odds + even_odds))

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

df.m1 <- df.c1 %>% filter(m == 1)
df.m2 <- df.c2 %>% filter(m == 1)

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
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# save
write.csv(out.df, 'results/tab_2.csv', row.names = F)

########################################################################################################################################
### Figure 3

# outcome variables
d.seq           <- c(paste('l', 3:1, sep = ''), 0:3)
out.vars        <- paste0('acled_event_type1_', d.seq)
names(out.vars) <- d.seq

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.m1, out.vars, 'lose', labels = c(model = 1))
ext.tmp2 <- batch.did(' ~ win  | 0 | 0 | birth_country + match_id', df.m2, out.vars, 'win', labels = c(model = 2))
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# saving it
write.csv(out.df, 'results/fig_3.csv', row.names = F)

# graph
gg.df <- out.df %>%  mutate(time = gsub('l', '-', ov) %>% as.numeric(.)) %>% mutate(sig = (star == '*'))
gg <- ggplot(gg.df, aes(x = time, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_line(size = 0.1, col = 'gray40') + geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) + 
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.1, size = 2.5) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  scale_x_continuous(breaks = -3:3) +
  
  ylab('Change in Demonstration Probability (%)') + xlab('Days from a Football Game') +
  theme_tufte() + theme(legend.position = 'none', strip.placement = 'outside') +
  
  facet_grid(rows = vars(x), switch = 'y', labeller =  as_labeller(c(lose = 'Loss', win = 'Win')), scales = 'free_y')
gg

# save
ggsave('results/fig_3.png', gg, height = 5, width = 6)
ggsave('results/eps/fig_3.eps', gg, height = 5, width = 6, device = cairo_ps)

########################################################################################################################################
### Figure 4

# outcome variables
out.vars        <- paste0('scad_protest', c('', '_cgov', '_rgov', '_ngov', '_nonethnic', '_ethnic', '_small', '_large'), '_dif3')
names(out.vars) <- c('All\nevents', 'Central\ngov.', 'Local\ngov.', 'Other', 'Non-ethnic', 'Ethnic', 'Small', 'Large')

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.m1, out.vars, 'lose')
ext.tmp2 <- batch.did(' ~ win  | 0 | 0 | birth_country + match_id', df.m2, out.vars, 'win')
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# saving it
write.csv(out.df, 'results/fig_4.csv', row.names = F)

# graph
gg.df <- out.df %>%
  mutate(sig  = (star == '*'), 
         ov   = factor(ov, levels = unique(names(out.vars))),
         x    = if_else(x == 'lose', 'Loss', 'Win'),
         type = factor(rep(c(' ', rep('Target',3), rep('Issue',2), rep('Size',2)),2), levels = c(' ', 'Target', 'Issue', 'Size')))
gg <- ggplot(gg.df, aes(x = ov, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.1, size = 2) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab('Change in Demonstration Probability (%)') + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  
  facet_grid(cols = vars(type), rows = vars(x), switch = 'both', scales = 'free_x', space = 'free')

# save
ggsave('results/fig_4.png', gg, height = 4, width = 6)
ggsave('results/eps/fig_4.eps', gg, height = 4, width = 6, device = cairo_ps)

########################################################################################################################################
### Table 3

# analysis
ext.tmp1 <- extract.felm(felm(I(100*lose) ~ p_score | 0 | 0 | birth_country + match_id, df1), 'p_score')
ext.tmp2 <- extract.felm(felm(I(100*lose) ~ p_score | 0 | 0 | birth_country + match_id, df.c1), 'p_score')
ext.tmp3 <- extract.felm(felm(I(100*win)  ~ p_score | 0 | 0 | birth_country + match_id, df2), 'p_score')
ext.tmp4 <- extract.felm(felm(I(100*win)  ~ p_score | 0 | 0 | birth_country + match_id, df.c2), 'p_score')
out.df   <- bind_rows(ext.tmp1, ext.tmp2, ext.tmp3, ext.tmp4)

# save
write.csv(out.df, 'results/tab_3.csv', row.names = F)

########################################################################################################################################
### Figure 5

# Samples
game.types <- c('All games', 'Close games', 'All games\n(matched)', 'Close games\n(matched)')

# loop
out.df <- data.frame()
for(i in game.types){
  for(j in c('lose', 'win')){
    
    # select the data
    if(i == 'All games' & j == 'lose'){df.tmp <- df1}
    if(i == 'All games' & j == 'win') {df.tmp <- df2}
    if(i == 'Close games' & j == 'lose'){df.tmp <- df.c1}
    if(i == 'Close games' & j == 'win') {df.tmp <- df.c2}
    if(i == 'All games\n(matched)' & j == 'lose'){df.tmp <- df1 %>% filter(m == 1)}
    if(i == 'All games\n(matched)' & j == 'win') {df.tmp <- df2 %>% filter(m == 1)}
    if(i == 'Close games\n(matched)' & j == 'lose'){df.tmp <- df.m1}
    if(i == 'Close games\n(matched)' & j == 'win') {df.tmp <- df.m2}
    
    # placebo tests
    out.vars <- c('p_score', 'rank_dif_p')
    ext.tmp  <- batch.did(paste0('~', j, '| 0 | 0 | birth_country + match_id'), df.tmp, out.vars, j, labels = c(data = i))
    out.df   <- bind_rows(out.df, ext.tmp)
  }
}

# saving it
write.csv(out.df, 'results/fig_5.csv', row.names = F)

# graph
gg.df <- out.df %>% filter(ov == 'rank_dif_p') %>% mutate(data = factor(data, levels = game.types), sig = (star == '*'), coef = coef, lwr = lwr, upr = upr) 
gg <- ggplot(gg.df, aes(x = data, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.05, size = 3) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab('Ranking Difference (percentile)') + xlab('') +
  theme_tufte() + theme(legend.position = 'none', strip.placement = 'outside') +
  
  facet_grid(cols = vars(x), switch = 'y', 
             labeller =  as_labeller(c(lose = 'Loss\n(Loss v.s. Draw)', win = 'Win\n(Win v.s. Draw)')))
gg
# save
ggsave('results/fig_5.png', gg, height = 4, width = 6)
ggsave('results/eps/fig_5.eps', gg, height = 4, width = 6, device = cairo_ps)

########################################################################################################################################
### Figure 6 (left)

# analysis
ext.tmp1 <- batch.did(' ~ lose*factor(ntile(season_appearance, 3)) - 1 - lose | 0 | 0 | birth_country + match_id', df.m1, 'acled_event_type1_dif3')
ext.tmp2 <- batch.did(' ~ win*factor(ntile(season_appearance, 3))  - 1 - win  | 0 | 0 | birth_country + match_id', df.m2, 'acled_event_type1_dif3')

# append
out.df <- bind_rows(ext.tmp1 %>% filter(startsWith(x, 'lose:')), 
                    ext.tmp2 %>% filter(startsWith(x, 'win:')))

# saving it
write.csv(out.df, 'results/fig_6_a.csv', row.names = F)

# dataset of ggplot
gg.df <- out.df %>% mutate(sig  = (star == '*'), 
                           var  = if_else(startsWith(var, 'lose'), 'Loss', 'Win'),
                           x    = case_when(endsWith(x, '1') ~ '1st tertile',
                                            endsWith(x, '2') ~ '2nd tertile',
                                            endsWith(x, '3') ~ '3rd tertile'))

# graph
gg <- ggplot(gg.df, aes(x = x, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.05, size = 3) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab("Change in Demonstration Probability (%)") + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  
  facet_grid(rows = vars(var), switch = 'both', scales = 'free_x')

# save
ggsave('results/fig_6_a.png', gg, height = 4, width = 3)
ggsave('results/eps/fig_6_a.eps', gg, height = 4, width = 3, device = cairo_ps)

########################################################################################################################################
### Figure 6 (right)

# lists of subset
s.list1 <- list('Regular\nseason'   = (df.m1$league != 'Champions League'),
                'Champions\nLeague' = (df.m1$league == 'Champions League'))
s.list2 <- list('Regular\nseason'   = (df.m2$league != 'Champions League'),
                'Champions\nLeague' = (df.m2$league == 'Champions League'))

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.m1, 'acled_event_type1_dif3', 'lose', s.list1)
ext.tmp2 <- batch.did(' ~ win | 0 | 0 | birth_country + match_id', df.m2, 'acled_event_type1_dif3', 'win', s.list2)
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# saving it
write.csv(out.df, 'results/fig_6_b.csv', row.names = F)

# dataset of ggplot
gg.df <- out.df %>% mutate(sig    = (star == '*'), 
                           var    = if_else(startsWith(var, 'lose'), 'Loss', 'Win'),
                           subset = factor(subset, levels = c('Regular\nseason', 'Champions\nLeague')))

# graph (all CL)
gg <- ggplot(gg.df, aes(x = subset, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.05, size = 3) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab("Change in Demonstration Probability (%)") + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  
  facet_grid(rows = vars(var), switch = 'both', scales = 'free_x')

# save
ggsave('results/fig_6_b.png', gg, height = 4, width = 3)
ggsave('results/eps/fig_6_b.eps', gg, height = 4, width = 3, device = cairo_ps)

########################################################################################################################################
### Table A4-1

vars    <- c('lose', 'even', 'win', 'p_score', 'acled_event_type1_dif3', 'acled_event_type2_dif3', 'acled_event_type3_dif3', 
             paste0('scad_protest', c('', '_cgov', '_rgov', '_ngov', '_nonethnic', '_ethnic', '_small', '_large'), '_dif3'), 
             'rank_dif_p', 'season_appearance')
sum.df1 <- summary2(df.c1[,vars])
sum.df2 <- summary2(df.c2[,vars])

# save them
write.csv(sum.df1, 'results/tab_a4_1_a.csv', row.names = F)
write.csv(sum.df2, 'results/tab_a4_1_b.csv', row.names = F)

########################################################################################################################################
### Figure A5-1

# outcome variables
d.seq           <- c('dif3', 'placebo1', 'placebo2', 'placebo3')
out.vars        <- paste0('acled_event_type1_', d.seq)
names(out.vars) <- d.seq

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.m1, out.vars, 'lose')
ext.tmp2 <- batch.did(' ~ win  | 0 | 0 | birth_country + match_id', df.m2, out.vars, 'win')
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# saving it
write.csv(out.df, 'results/fig_a5_1.csv', row.names = F)

# graph
gg.df <- out.df %>% mutate(sig = (star == '*'))
gg <- ggplot(gg.df, aes(x = ov, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = coef), hjust = 0, vjust = 0, nudge_x = 0.05, size = 3) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  scale_x_discrete(labels = c(dif3 = 'Treatment', placebo1 = 'Placebo A', placebo2 = 'Placebo B', placebo3 = 'Placebo C')) + 
  
  ylab('Change in Demonstration Probability (%)') + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = 'outside') +
  
  facet_grid(cols = vars(x), switch = 'y', labeller =  as_labeller(c(lose = 'Loss', win = 'Win')))

# save
ggsave('results/fig_a5_1.png', gg, height = 3, width = 6)
ggsave('results/eps/fig_a5_1.eps', gg, height = 3, width = 6, device = cairo_ps)

########################################################################################################################################
### Figure A6-1

# outcome variables
out.vars <- c('acled_event_type1_africa_dif3', 'scad_protest_africa_dif3', 'scad_protest_america_dif3')

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.m1, out.vars, 'lose')
ext.tmp2 <- batch.did(' ~ win  | 0 | 0 | birth_country + match_id', df.m2, out.vars, 'win')
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# saving it
write.csv(out.df, 'results/fig_a6_1.csv', row.names = F)

# graph
gg.df <- out.df %>% mutate(sig = (star == '*'), ov = factor(ov, levels = out.vars))
gg <- ggplot(gg.df, aes(x = ov, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = coef), hjust = 0, vjust = 0, nudge_x = 0.05, size = 3) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  scale_x_discrete(labels = c(acled_event_type1_africa_dif3 = 'Non-conational\nAfrica (ACLED)', 
                              scad_protest_africa_dif3      = 'Non-conational\nAfrica (SCAD)', 
                              scad_protest_america_dif3     = 'Latin America\n(SCAD)')) + 
  
  ylab('Change in Protest Probability (%)') + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = 'outside') +
  
  facet_grid(cols = vars(x), switch = 'y', labeller =  as_labeller(c(lose = 'Loss', win = 'Win')))

# save
ggsave('results/fig_a6_1.png', gg, height = 3, width = 6)
ggsave('results/eps/fig_a6_1.eps', gg, height = 3, width = 6, device = cairo_ps)

########################################################################################################################################
### Table A7-1

# outcome variables
out.vars <- c('Non-ethnic' = 'acled_nonethnic_type1_dif3', 'Ethnic' = 'acled_ethnic_type1_dif3')

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.m1, out.vars, 'lose')
ext.tmp2 <- batch.did(' ~ win  | 0 | 0 | birth_country + match_id', df.m2, out.vars, 'win')
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# save
write.csv(out.df, 'results/tab_a7_1.csv', row.names = F)

########################################################################################################################################
### Figure A8-1

# lists of subset
climax.games <- c('last 16 1st leg', 'last 16 2nd leg', 'Quarter-Finals 1st leg', 'Quarter-Finals 2nd leg', 
                  'Semi-Finals 1st Leg', 'Semi-Finals 2nd Leg', 'Final')
s.list1 <- list('Regular\nseason'   = (df.m1$league != 'Champions League'),
                'CL\n(Group stage)' = startsWith(df.m1$match_day, 'Group'),
                'CL\n(Knockout)'    = (df.m1$match_day %in% climax.games))
s.list2 <- list('Regular\nseason'   = (df.m2$league != 'Champions League'),
                'CL\n(Group stage)' = startsWith(df.m2$match_day, 'Group'),
                'CL\n(Knockout)'    = (df.m2$match_day %in% climax.games))

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.m1, 'acled_event_type1_dif3', 'lose', s.list1)
ext.tmp2 <- batch.did(' ~ win | 0 | 0 | birth_country + match_id', df.m2, 'acled_event_type1_dif3', 'win', s.list2)
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# saving it
write.csv(out.df, 'results/fig_a8_1.csv', row.names = F)

# dataset of ggplot
gg.df <- out.df %>% mutate(sig    = (star == '*'), 
                           var    = if_else(startsWith(var, 'lose'), 'Loss', 'Win'),
                           subset = factor(subset, levels = c('Regular\nseason', 'CL\n(Group stage)', 'CL\n(Knockout)')))

# graph
gg <- ggplot(gg.df, aes(x = subset, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.05, size = 3) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab("Change in Demonstration Probability (%)") + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  
  facet_grid(rows = vars(var), switch = 'both', scales = 'free_x')

# save
ggsave('results/fig_a8_1.png', gg, height = 4, width = 3)
ggsave('results/eps/fig_a8_1.eps', gg, height = 4, width = 3, device = cairo_ps)

########################################################################################################################################
### Figure A8-2

# analysis
ext.tmp1 <- batch.did(' ~ lose*factor(team_CL == 1) - 1 - lose | 0 | 0 | birth_country + match_id', df.m1, 'acled_event_type1_dif3')
ext.tmp2 <- batch.did(' ~ win*factor(team_CL == 1)  - 1 - win  | 0 | 0 | birth_country + match_id', df.m2, 'acled_event_type1_dif3')

# append
out.df <- bind_rows(ext.tmp1 %>% filter(startsWith(x, 'lose:')), 
                    ext.tmp2 %>% filter(startsWith(x, 'win:')))

# saving it
write.csv(out.df, 'results/fig_a8_2.csv', row.names = F)

# dataset of ggplot
gg.df <- out.df %>% mutate(sig  = (star == '*'), 
                           var  = if_else(startsWith(var, 'lose'), 'Loss', 'Win'),
                           x    = case_when(endsWith(x, 'TRUE')  ~ 'Yes',
                                            endsWith(x, 'FALSE') ~ 'No'))

# graph
gg <- ggplot(gg.df, aes(x = x, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.05, size = 3) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab("Change in Demonstration Probability (%)") + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  
  facet_grid(rows = vars(var), switch = 'both', scales = 'free_x')

# save
ggsave('results/fig_a8_2.png', gg, height = 4, width = 3)
ggsave('results/eps/fig_a8_2.eps', gg, height = 4, width = 3, device = cairo_ps)

########################################################################################################################################
### Figure A8-3

# making a factor
# Because team_popularity has many zeros and negligible values, ntile() should not be used.
q1    <- quantile(df.m1 %>% filter(team_popularity > 0) %>% pull(team_popularity), (1:2)/3)
q2    <- quantile(df.m2 %>% filter(team_popularity > 0) %>% pull(team_popularity), (1:2)/3)
df.s1 <- df.m1 %>% mutate(team_popularity_f = case_when(team_popularity < q1[1] ~ 1,
                                                        q1[1] <= team_popularity & team_popularity < q1[2] ~ 2,
                                                        q1[2] <= team_popularity ~ 3))
df.s2 <- df.m2 %>% mutate(team_popularity_f = case_when(team_popularity < q2[1] ~ 1,
                                                        q2[1] <= team_popularity & team_popularity < q2[2] ~ 2,
                                                        q2[2] <= team_popularity ~ 3))

# analysis
ext.tmp1 <- batch.did(' ~ lose*factor(team_popularity_f) - 1 - lose | 0 | 0 | birth_country + match_id', df.s1, 'acled_event_type1_dif3')
ext.tmp2 <- batch.did(' ~ win* factor(team_popularity_f) - 1 - win  | 0 | 0 | birth_country + match_id', df.s2, 'acled_event_type1_dif3')

# append
out.df <- bind_rows(ext.tmp1 %>% filter(startsWith(x, 'lose:')), 
                    ext.tmp2 %>% filter(startsWith(x, 'win:')))

# saving it
write.csv(out.df, 'results/fig_a8_3.csv', row.names = F)

# dataset of ggplot
gg.df <- out.df %>% mutate(sig  = (star == '*'), 
                           var  = if_else(startsWith(var, 'lose'), 'Loss', 'Win'),
                           x    = case_when(endsWith(x, '1') ~ '1st tertile',
                                            endsWith(x, '2') ~ '2nd tertile',
                                            endsWith(x, '3') ~ '3rd tertile'))

# graph
gg <- ggplot(gg.df, aes(x = x, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.05, size = 3) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab("Change in Demonstration Probability (%)") + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  
  facet_grid(rows = vars(var), switch = 'both', scales = 'free_x')

# save
ggsave('results/fig_a8_3.png', gg, height = 4, width = 3)
ggsave('results/eps/fig_a8_3.eps', gg, height = 4, width = 3, device = cairo_ps)

########################################################################################################################################
### Figure A9-1

# analysis
ext.tmp1 <- batch.did(' ~ lose*factor(ntile(p_score,3)) - 1 - lose | 0 | 0 | birth_country + match_id', df.m1, 'acled_event_type1_dif3')
ext.tmp2 <- batch.did(' ~ win*factor(ntile(p_score,3))  - 1 - win  | 0 | 0 | birth_country + match_id', df.m2, 'acled_event_type1_dif3')

# append
out.df <- bind_rows(ext.tmp1 %>% filter(startsWith(x, 'lose:')), 
                    ext.tmp2 %>% filter(startsWith(x, 'win:')))

# saving it
write.csv(out.df, 'results/fig_a9_1.csv', row.names = F)

# dataset of ggplot
gg.df <- out.df %>% mutate(sig  = (star == '*'), 
                           var  = if_else(startsWith(var, 'lose'), 'Loss', 'Win'),
                           x    = case_when(endsWith(x, '1') ~ '1st tertile',
                                            endsWith(x, '2') ~ '2nd tertile',
                                            endsWith(x, '3') ~ '3rd tertile'))

# graph
gg <- ggplot(gg.df, aes(x = x, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.05, size = 3) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab("Change in Demonstration Probability (%)") + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  
  facet_grid(rows = vars(var), switch = 'both', scales = 'free_x')

# save
ggsave('results/fig_a9_1.png', gg, height = 4, width = 3)
ggsave('results/eps/fig_a9_1.eps', gg, height = 4, width = 3, device = cairo_ps)

########################################################################################################################################
### Figure A9-2

# coarsening the variable
df1 <- df1 %>% mutate(score_dif2 = relevel(factor(replace(score_dif, score_dif < -3, -3)), ref = '0'))
df2 <- df2 %>% mutate(score_dif2 = relevel(factor(replace(score_dif, score_dif >  3,  3)), ref = '0'))

# analysis
ext.tmp1 <- batch.did(' ~ score_dif2  | 0 | 0 | birth_country + match_id', df1 %>% filter(m == 1), 'acled_event_type1_dif3')
ext.tmp2 <- batch.did(' ~ score_dif2  | 0 | 0 | birth_country + match_id', df2 %>% filter(m == 1), 'acled_event_type1_dif3')
out.df   <- bind_rows(ext.tmp1, ext.tmp2) %>% filter(var != '(Intercept)') %>% mutate(score_dif = as.numeric(gsub('score_dif2', '', var)))

# saving it
write.csv(out.df, 'results/fig_a9_2.csv', row.names = F)

# graph
gg.df <- out.df %>% rbind(., setNames(data.frame('', '', '', 0, 0, 0, 0, 0, 0, 1, '', 0), colnames(.))) %>% mutate(sig  = (star == '*'))
gg    <- ggplot(gg.df, aes(x = score_dif, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.05, size = 2.5) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_x_continuous(breaks = -3:3, labels = c('< -3', -2:2, '> 3')) +
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab("Change in Demonstration Probability (%)") + xlab('Score Difference') +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside")

# save
ggsave('results/fig_a9_2.png', gg, height = 3, width = 5)
ggsave('results/eps/fig_a9_2.eps', gg, height = 3, width = 5, device = cairo_ps)

########################################################################################################################################
### Figure A10-1 (top right)

# lists of subset
s.list1 <- list("2005-09" = (df.m1$season %in% 2005:2009), 
                "2010-14" = (df.m1$season %in% 2009:2014),
                "2015-18" = (df.m1$season %in% 2015:2018))
s.list2 <- list("2005-09" = (df.m2$season %in% 2005:2009), 
                "2010-14" = (df.m2$season %in% 2009:2014),
                "2015-18" = (df.m2$season %in% 2015:2018))

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.m1, 'acled_event_type1_dif3', 'lose', s.list1)
ext.tmp2 <- batch.did(' ~ win  | 0 | 0 | birth_country + match_id', df.m2, 'acled_event_type1_dif3', 'win', s.list2)
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# saving it
write.csv(out.df, 'results/fig_a10_1_a.csv', row.names = F)

# graph
gg.df  <- out.df %>% mutate(sig    = (star == '*'), 
                            x      = if_else(x == 'lose', 'Loss', 'Win'),
                            subset = factor(subset, levels = c("2005-09", "2010-14", "2015-18")))
gg    <- ggplot(gg.df, aes(x = subset, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.1, size = 3) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab(NULL) + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  
  facet_grid(rows = vars(x), switch = 'both', scales = 'free_x', space='free')

# save
ggsave('results/fig_a10_1_a.png', gg, height = 4, width = 3)
ggsave('results/eps/fig_a10_1_a.eps', gg, height = 4, width = 3, device = cairo_ps)

########################################################################################################################################
### Figure A10-1 (top left)

# lists of subset
s.list1 <- list("West"   = (df.m1$birth_subregion == "Western Africa"), 
                "South"  = (df.m1$birth_subregion == "Southern Africa"),
                "North"  = (df.m1$birth_subregion == "Northern Africa"),
                "East"   = (df.m1$birth_subregion == "Eastern Africa"))
s.list2 <- list("West"   = (df.m2$birth_subregion == "Western Africa"), 
                "South"  = (df.m2$birth_subregion == "Southern Africa"),
                "North"  = (df.m2$birth_subregion == "Northern Africa"),
                "East"   = (df.m2$birth_subregion == "Eastern Africa"))

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.m1, 'acled_event_type1_dif3', 'lose', s.list1)
ext.tmp2 <- batch.did(' ~ win  | 0 | 0 | birth_country + match_id', df.m2, 'acled_event_type1_dif3', 'win', s.list2)
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# saving it
write.csv(out.df, 'results/fig_a10_1_b.csv', row.names = F)

# graph
gg.df  <- out.df %>% mutate(sig    = (star == '*'), 
                            x      = if_else(x == 'lose', 'Loss', 'Win'),
                            subset = factor(subset, levels = c('West', 'South', 'North', 'East')))
gg    <- ggplot(gg.df, aes(x = subset, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.1, size = 3) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab(NULL) + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  
  facet_grid(rows = vars(x), switch = 'both', scales = 'free_x', space='free')

# save
ggsave('results/fig_a10_1_b.png', gg, height = 4, width = 3)
ggsave('results/eps/fig_a10_1_b.eps', gg, height = 4, width = 3, device = cairo_ps)

########################################################################################################################################
### Figure A10-1 (bottom)

# lists of subset
s.list1 <- list("Champions\nLeague" = (df.m1$league == "Champions League"),
                "Premier\nLeague"   = (df.m1$league == "Premier League"), 
                "La Liga"           = (df.m1$league == "La Liga"),
                "Serie A"           = (df.m1$league == "Serie A"),
                "Bundesliga"        = (df.m1$league == "Bundesliga"),
                "Ligue 1"           = (df.m1$league == "Ligue 1"))
s.list2 <- list("Champions\nLeague" = (df.m2$league == "Champions League"),
                "Premier\nLeague"   = (df.m2$league == "Premier League"), 
                "La Liga"           = (df.m2$league == "La Liga"),
                "Serie A"           = (df.m2$league == "Serie A"),
                "Bundesliga"        = (df.m2$league == "Bundesliga"),
                "Ligue 1"           = (df.m2$league == "Ligue 1"))

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.m1, 'acled_event_type1_dif3', 'lose', s.list1)
ext.tmp2 <- batch.did(' ~ win  | 0 | 0 | birth_country + match_id', df.m2, 'acled_event_type1_dif3', 'win', s.list2)
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# saving it
write.csv(out.df, 'results/fig_a10_1_c.csv', row.names = F)

# graph
gg.df <- out.df %>%
  mutate(sig    = (star == '*'), 
         x      = if_else(x == 'lose', 'Loss', 'Win'),
         subset = factor(subset, levels = c('Champions\nLeague', 'Premier\nLeague', 'La Liga', 'Serie A', 'Bundesliga', 'Ligue 1')))
gg <- ggplot(gg.df, aes(x = subset, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.1, size = 3) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab(NULL) + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  
  facet_grid(rows = vars(x), switch = 'both', scales = 'free_x', space='free')

# save
ggsave('results/fig_a10_1_c.png', gg, height = 4, width = 4)
ggsave('results/eps/fig_a10_1_c.eps', gg, height = 4, width = 4, device = cairo_ps)

########################################################################################################################################
### Table A11-1

# load data
df.ct  <- readRDS('data/panel_final_citizenship.rds') %>% filter(both_teams_citizenship == 0)
df.ct1 <- df.ct %>% filter(result != 'win') %>% mutate(p_score = 100 * lose_odds / (lose_odds + even_odds)) %>% filter(abs(score_dif) < 2)
df.ct2 <- df.ct %>% filter(result != 'lose') %>% mutate(p_score = 100 * win_odds  / (win_odds + even_odds)) %>% filter(abs(score_dif) < 2)

# matching
df.ct1$m <- matching(group.id  = df.ct1$match_team_id, 
                     treat     = df.ct1$lose, 
                     p.score   = df.ct1$p_score, caliper = 5)
df.ct2$m <- matching(group.id  = df.ct2$match_team_id, 
                     treat     = df.ct2$win, 
                     p.score   = df.ct2$p_score, caliper = 5)

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | country + match_id', df.ct1 %>% filter(m == 1), 'acled_event_type1_dif3', 'lose')
ext.tmp2 <- batch.did(' ~ win  | 0 | 0 | country + match_id', df.ct2 %>% filter(m == 1), 'acled_event_type1_dif3', 'win')
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# save
write.csv(out.df, 'results/tab_a11_1.csv', row.names = F)

########################################################################################################################################
### Table A11-2

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.m1 %>% filter(absent == 0), 
                      'acled_event_type1_dif3', 'lose', labels = c('player' = 'present'))
ext.tmp2 <- batch.did(' ~ win  | 0 | 0 | birth_country + match_id', df.m2 %>% filter(absent == 0), 
                      'acled_event_type1_dif3', 'win',  labels = c('player' = 'present'))
ext.tmp3 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.m1 %>% filter(absent == 1), 
                      'acled_event_type1_dif3', 'lose', labels = c('player' = 'absent'))
ext.tmp4 <- batch.did(' ~ win  | 0 | 0 | birth_country + match_id', df.m2 %>% filter(absent == 1), 
                      'acled_event_type1_dif3', 'win',  labels = c('player' = 'absent'))
out.df   <- bind_rows(ext.tmp1, ext.tmp2, ext.tmp3, ext.tmp4)

# save
write.csv(out.df, 'results/tab_a11_2.csv', row.names = F)

########################################################################################################################################
### Table A11-3

# dropping obs
df.s1 <- df.c %>% 
  group_by(birth_country, match_date) %>% 
  summarise(idx_lose = mean(lose), idx_win = mean(win)) %>% 
  left_join(df.m1, ., by = c('birth_country', 'match_date')) %>%
  filter(!(idx_lose > 0 & idx_win > 0))
df.s2 <- df.c %>% 
  group_by(birth_country, match_date) %>% 
  summarise(idx_lose = mean(lose), idx_win = mean(win)) %>% 
  left_join(df.m2, ., by = c('birth_country', 'match_date')) %>%
  filter(!(idx_lose > 0 & idx_win > 0))

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.s1, 'acled_event_type1_dif3', 'lose')
ext.tmp2 <- batch.did(' ~ win  | 0 | 0 | birth_country + match_id', df.s2, 'acled_event_type1_dif3', 'win')
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# save
write.csv(out.df, 'results/tab_a11_3.csv', row.names = F)

########################################################################################################################################
### Table A11-4

# aggregation
df.agg1 <- df.m1 %>% 
  group_by(birth_country, match_team_id) %>%
  summarise_at(vars(lose, acled_event_type1_dif3, match_id), first)
df.agg2 <- df.m2 %>% 
  group_by(birth_country, match_team_id) %>%
  summarise_at(vars(win, acled_event_type1_dif3, match_id), first)

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.agg1, 'acled_event_type1_dif3', 'lose')
ext.tmp2 <- batch.did(' ~ win  | 0 | 0 | birth_country + match_id', df.agg2, 'acled_event_type1_dif3', 'win')
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# save
write.csv(out.df, 'results/tab_a11_4.csv', row.names = F)

########################################################################################################################################
### Table A11-5

# outcome variables
d.seq.tmp           <- c('incidence3', 'count3', 'countdif3', 'countlog3', 'countlogdif3')
out.vars.tmp        <- paste0('acled_event_type1_', d.seq.tmp)
names(out.vars.tmp) <- d.seq.tmp

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.m1, out.vars.tmp, 'lose')
ext.tmp2 <- batch.did(' ~ win  | 0 | 0 | birth_country + match_id', df.m2, out.vars.tmp, 'win')
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# save
write.csv(out.df, 'results/tab_a11_5.csv', row.names = F)

########################################################################################################################################
### Table A11-6

# formula for lags
lag.fom <- paste(paste0('acled_event_type1_l', 1:3), collapse = ' + ')

# analysis
ext.tmp1 <- batch.did(paste0(' ~ lose +', lag.fom, ' | 0 | 0 | birth_country + match_id'), df.m1, 'acled_event_type1_mean3')
ext.tmp2 <- batch.did(paste0(' ~ win +',  lag.fom, ' | 0 | 0 | birth_country + match_id'), df.m2, 'acled_event_type1_mean3')
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# save
write.csv(out.df, 'results/tab_a11_6.csv', row.names = F)

########################################################################################################################################
### Table A11-7

# make target shoot differences
df.s1 <- df.c1 %>% mutate(shot_target_dif = team_shot_target - enemy_shot_target) %>% drop_na(shot_target_dif)
df.s2 <- df.c2 %>% mutate(shot_target_dif = team_shot_target - enemy_shot_target) %>% drop_na(shot_target_dif)

# matching on p.score and target shoot differences
cut.p <- list(p_score = seq(0, 100, 5), shot_target_dif = seq(-30, 30, 1))
cem1  <- matching.cem(group.id = df.s1$match_team_id, treat = df.s1$lose, covs = df.s1[,c('p_score', 'shot_target_dif')], cut.p = cut.p)
cem2  <- matching.cem(group.id = df.s2$match_team_id, treat = df.s2$win,  covs = df.s2[,c('p_score', 'shot_target_dif')], cut.p = cut.p)

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.s1[cem1,], 'acled_event_type1_dif3', 'lose')
ext.tmp2 <- batch.did(' ~ win  | 0 | 0 | birth_country + match_id', df.s2[cem2,], 'acled_event_type1_dif3', 'win')
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# save
write.csv(out.df, 'results/tab_a11_7.csv', row.names = F)

########################################################################################################################################
### Table A11-8

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df1 %>% filter(m == 1), 'acled_event_type1_dif3', 'lose')
ext.tmp2 <- batch.did(' ~ win  | 0 | 0 | birth_country + match_id', df2 %>% filter(m == 1), 'acled_event_type1_dif3', 'win')
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# save
write.csv(out.df, 'results/tab_a11_8.csv', row.names = F)

########################################################################################################################################
### Table A11-9

# analysis
ext.tmp1 <- batch.did(' ~ lose | 0 | 0 | birth_country + match_id', df.c1, 'acled_event_type1_dif3', 'lose')
ext.tmp2 <- batch.did(' ~ win  | 0 | 0 | birth_country + match_id', df.c2, 'acled_event_type1_dif3', 'win')
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# save
write.csv(out.df, 'results/tab_a11_9.csv', row.names = F)

########################################################################################################################################
### Table A11-10

# analysis
ext.tmp1 <- batch.did(' ~ lose + p_score | 0 | 0 | birth_country + match_id', df.c1, 'acled_event_type1_dif3', 'lose')
ext.tmp2 <- batch.did(' ~ win  + p_score | 0 | 0 | birth_country + match_id', df.c2, 'acled_event_type1_dif3', 'win')
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# save
write.csv(out.df, 'results/tab_a11_10.csv', row.names = F)

########################################################################################################################################
### Figure A11-1

# List of FEs
FEs <- c('Player\nFE'          = 'player_id',
         'Year\nFE'            = 'year',
         'Month\nFE'           = 'month',
         'Week\nFE'            = 'week',
         'Day of the Week\nFE' = 'weekday',
         'All time\nFEs'       = 'year + month + week + weekday',
         'Year\nFE'            = 'player_year',
         'Month\nFE'           = 'player_month',
         'Week\nFE'            = 'player_week',
         'Day of the Week\nFE' = 'player_weekday',
         'All time\nFEs'       = 'player_year + player_month + player_week + player_weekday')
PSs = c('Player-specific FEs', rep('Time FEs', 5), rep('Player-specific FEs', 5))

# loop for FEs
out.df <- data.frame()
for(i in 1:length(FEs)){
  
  # analysis
  ext.tmp1 <- batch.did(paste0(' ~ lose | ', FEs[i], ' | 0 | birth_country + match_id'), df.m1, 
                        'acled_event_type1_dif3', 'lose', labels = c(fe = names(FEs)[i], ps = PSs[i]))
  ext.tmp2 <- batch.did(paste0(' ~ win  | ', FEs[i], ' | 0 | birth_country + match_id'), df.m2, 
                        'acled_event_type1_dif3', 'win', labels = c(fe = names(FEs)[i], ps = PSs[i]))
  out.df   <- bind_rows(out.df, ext.tmp1, ext.tmp2)
}

# saving it
write.csv(out.df, 'results/fig_a11_1.csv', row.names = F)

# graph
gg.df <- out.df %>% filter(x == 'lose') %>% mutate(sig = (star == '*'), 
                                                   fe  = factor(fe, levels = unique(names(FEs))), 
                                                   ps  = factor(ps, levels = rev(unique(PSs))))
gg    <- ggplot(gg.df, aes(x = fe, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) + 
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.1, size = 2) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab('Change in Demonstration Probability (%)') + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  facet_grid( rows = vars(ps), switch = 'y', scales = 'free_x')

# save
ggsave('results/fig_a11_1_a.png', gg, height = 3, width = 5)
ggsave('results/eps/fig_a11_1_a.eps', gg, height = 3, width = 5, device = cairo_ps)

# graph
gg.df <- out.df %>% filter(x == 'win') %>% mutate(sig = (star == '*'), 
                                                  fe  = factor(fe, levels = unique(names(FEs))), 
                                                  ps  = factor(ps, levels = rev(unique(PSs))))
gg <- ggplot(gg.df, aes(x = fe, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) + 
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.1, size = 2) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab('Change in Demonstration Probability (%)') + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  facet_grid( rows = vars(ps), switch = 'y', scales = 'free_x')

# save
ggsave('results/fig_a11_1_b.png', gg, height = 3, width = 5)
ggsave('results/eps/fig_a11_1_b.eps', gg, height = 3, width = 5, device = cairo_ps)

########################################################################################################################################
### Figure A11-2

# calipers
c.vals <- c(1:10)

# loop
out.df <- data.frame()
for(i in c.vals){
  
  # matching
  mtch1 <- matching(group.id = df.c1$match_team_id, treat = df.c1$lose, p.score = df.c1$p_score, caliper = i)
  mtch2 <- matching(group.id = df.c2$match_team_id, treat = df.c2$win,  p.score = df.c2$p_score, caliper = i)
  
  # subset
  df.tmp1 <- df.c1[mtch1,]
  df.tmp2 <- df.c2[mtch2,]
  
  # analysis
  ext.tmp1 <- batch.did(' ~ lose  | 0 | 0 | birth_country + match_id', df.tmp1, 'acled_event_type1_dif3', 
                        'lose', labels = c(caliper = i))
  ext.tmp2 <- batch.did(' ~ win   | 0 | 0 | birth_country + match_id', df.tmp2, 'acled_event_type1_dif3', 
                        'win',  labels = c(caliper = i))
  out.df   <- bind_rows(out.df, ext.tmp1, ext.tmp2)
}

# saving it
write.csv(out.df, 'results/fig_a11_2.csv', row.names = F)

# plot it
gg.df <- out.df %>% mutate(sig = (star == '*'), 
                           x   = if_else(x == 'lose', 'Loss', 'Win'),
                           lab = factor(paste0(caliper, ' (N = ', n, ')'), levels = paste0(caliper, ' (N = ', n, ')')[order(caliper)]))
gg <- ggplot(gg.df, aes(x = caliper, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.1, size = 1.5) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  scale_x_continuous(breaks = c.vals) +
  
  ylab('Change in Demonstration Probability (%)') + xlab('Caliper (Assignment probability; %)') +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  
  facet_grid(cols = vars(x), switch = 'y')

# save
ggsave('results/fig_a11_2.png', gg, height = 3, width = 6.25)
ggsave('results/eps/fig_a11_2.eps', gg, height = 3, width = 6.25, device = cairo_ps)

########################################################################################################################################
### Figure A11-3

# loop
out.df <- data.frame()
for(i in 1:3){
  
  # analysis
  ext.tmp1 <- batch.did(' ~ lose  | 0 | 0 | birth_country + match_id', df.m1, paste0('acled_event_type1_dif', i), 
                        'lose', labels = c(window.size = i))
  ext.tmp2 <- batch.did(' ~ win | 0 | 0 | birth_country + match_id', df.m2, paste0('acled_event_type1_dif', i), 
                        'win', labels = c(window.size = i))
  out.df   <- bind_rows(out.df, ext.tmp1, ext.tmp2)
}

# saving it
write.csv(out.df, 'results/fig_a11_3.csv', row.names = F)

# graph
gg.df <- out.df %>% mutate(sig = (star == '*'), x = if_else(x == 'lose', 'Loss', 'Win'), window.size = factor(window.size))
gg <- ggplot(gg.df, aes(x = window.size, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.1, size = 3) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab('Change in Demonstration Probability (%)') + xlab('Window Size (days)') +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  
  facet_grid(cols = vars(x), switch = 'y')

# save
ggsave('results/fig_a11_3.png', gg, height = 3, width = 6)
ggsave('results/eps/fig_a11_3.eps', gg, height = 3, width = 6, device = cairo_ps)

########################################################################################################################################
### Figure A11-4

# lists for loop
countries <- sort(unique(df.m1$birth_country))

# loop
out.df <- data.frame()
for(cntry in countries){
  
  # analysis
  ext.tmp1 <- batch.did(' ~ lose  | 0 | 0 | birth_country + match_id', df.m1[df.m1$birth_country != cntry,], 
                        'acled_event_type1_dif3', 'lose', labels = c(country = cntry))
  ext.tmp2 <- batch.did(' ~ win   | 0 | 0 | birth_country + match_id', df.m2[df.m2$birth_country != cntry,], 
                        'acled_event_type1_dif3', 'win',  labels = c(country = cntry))
  out.df   <- bind_rows(out.df, ext.tmp1, ext.tmp2)
}

# saving it
write.csv(out.df, 'results/fig_a11_4.csv', row.names = F)

# plot it
gg.df <- out.df %>% filter(x == 'lose') %>% 
  mutate(sig = (star == '*'), 
         n   = nrow(df.m1) - n,
         lab = factor(paste0(country, ' (N = ', n, ')'), levels = paste0(country, ' (N = ', n, ')')[order(coef)]))
gg <- ggplot(gg.df, aes(x = coef, y = lab, xmin = lwr, xmax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_y = 0.1, nudge_x = 0.005, size = 3) +
  geom_vline(xintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab('Dropped Country') + xlab('Change in Demonstration Probability (%)') +
  theme_tufte() + theme(legend.position = 'none')

# save
ggsave('results/fig_a11_4_a.png', gg, height = 8, width = 4)
ggsave('results/eps/fig_a11_4_a.eps', gg, height = 8, width = 4, device = cairo_ps)

# plot it
gg.df <- out.df %>% filter(x == 'win') %>% 
  mutate(sig = (star == '*'), 
         n   = nrow(df.m2) - n,
         lab = factor(paste0(country, ' (N = ', n, ')'), levels = paste0(country, ' (N = ', n, ')')[order(coef)]))
gg <- ggplot(gg.df, aes(x = coef, y = lab, xmin = lwr, xmax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_y = 0.1, nudge_x = 0.005, size = 3) +
  geom_vline(xintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab('Dropped Country') + xlab('Change in Demonstration Probability (%)') +
  theme_tufte() + theme(legend.position = 'none')

# save
ggsave('results/fig_a11_4_b.png', gg, height = 8, width = 4)
ggsave('results/eps/fig_a11_4_b.eps', gg, height = 8, width = 4, device = cairo_ps)
