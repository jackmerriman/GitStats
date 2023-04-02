
rm(list = ls()); invisible(gc())

library(tidyverse)
library(ggthemes)
library(cowplot)
library(haven)
library(lfe)
library(rdd)

source('rcodes/fun/check_balance.R')
source('rcodes/fun/batch_rdd.R')
source('rcodes/fun/correct_p.R')
source('rcodes/fun/summary2.R')
source('rcodes/fun/matching.R')
source('rcodes/fun/make_w.R')

# load data
df       <- readRDS('data/survey_final_birth.rds')
out.vars <- readRDS('data/out_vars.rds')
cov.vars <- readRDS('data/cov_vars.rds')
w.vars   <- readRDS('data/w_vars.rds')
s.vars   <- readRDS('data/s_vars.rds')

# bandwidths
bws <- 1:3

# main bandwidth
bw.main <- 3

# drop if there are players from the same country in opposing teams (actually, no such case in RD, but to be sure)
df <- df %>% filter(both_teams_birth == 0)

# subset to win-even and lose-even data
df1 <- df %>% filter(result != 'win')
df2 <- df %>% filter(result != 'lose')

# calculate propensity score
df1 <- df1 %>% mutate(p_score = 100 * lose_odds / (lose_odds + even_odds))
df2 <- df2 %>% mutate(p_score = 100 * win_odds  / (win_odds + even_odds))

# subset to close games
df.c1 <- df1 %>% filter(abs(score_dif) < 2 & run_var != 0)
df.c2 <- df2 %>% filter(abs(score_dif) < 2 & run_var != 0)
df.c  <- df  %>% filter(abs(score_dif) < 2 & run_var != 0)

# include propensity score as a covariate
cov.vars <- c(cov.vars, 'Propensity score' = 'p_score')

# theme of ggplot
grid.theme  <- theme_few() + theme(legend.position = 'none', strip.placement = "outside", 
                                   axis.title = element_text(size=10),
                                   panel.border = element_rect(colour = "gray80", fill = NA),
                                   plot.background = element_rect(colour = "gray60", fill = NA))

########################################################################################################################################
### Figure 7

# analysis
ext.tmp1 <- batch.rdd(' ~ treat*lose | 0 | 0 | country + match_id', df.c1, out.vars, bw.main, c(lose = 'treat:lose'), digit = NULL)
ext.tmp2 <- batch.rdd(' ~ treat*win  | 0 | 0 | country + match_id', df.c2, out.vars, bw.main, c(win  = 'treat:win'),  digit = NULL)
ext.tmp1 <- correct.p(ext.tmp1)
ext.tmp2 <- correct.p(ext.tmp2)
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# saving it
write.csv(out.df, 'results/fig_7.csv', row.names = F)

x.min <- min(out.df$lwr)
x.max <- max(out.df$upr)*1.5

# graph (attribution)
gg.df <- out.df %>% 
  filter(bw == bw.main & (startsWith(ov, 'Trust') | startsWith(ov, 'Performance'))) %>%
  mutate(sig = (star == '*'), ov = factor(ov, levels = rev(names(out.vars))))
gg1 <- ggplot(gg.df, aes(y = ov, x = coef, xmin = lwr, xmax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_y = 0.1, nudge_x = -0.2, size = 3) +
  geom_vline(xintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab(NULL) + xlab('Change in Survey Score') +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  facet_grid(cols = vars(var), labeller =  as_labeller(c(lose = 'Loss', win = 'Win')))

# graph (mood)
gg.df <- out.df %>% filter(bw == bw.main & !startsWith(ov, 'Trust') & !startsWith(ov, 'Performance') & !startsWith(ov, 'National')) %>%
  mutate(sig = (star == '*'), ov = factor(ov, levels = rev(names(out.vars))))
gg2 <- ggplot(gg.df, aes(y = ov, x = coef, xmin = lwr, xmax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_y = 0.1, nudge_x = -0.2, size = 3) +
  geom_vline(xintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab(NULL) + xlab('Change in Survey Score') +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  facet_grid(cols = vars(var), labeller =  as_labeller(c(lose = 'Loss', win = 'Win')))

# graph (nationalism)
gg.df <- out.df %>% filter(bw == bw.main & startsWith(ov, 'National')) %>%
  mutate(sig = (star == '*'), ov = factor(ov, levels = rev(names(out.vars))))
gg3 <- ggplot(gg.df, aes(y = ov, x = coef, xmin = lwr, xmax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_y = 0.1, nudge_x = -0.2, size = 3) +
  geom_vline(xintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_x_continuous(limits = c(x.min, x.max)) +
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab(NULL) + xlab('Change in Survey Score') +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  facet_grid(cols = vars(var), labeller =  as_labeller(c(lose = '', win = '')))

# combine the graphs
gg.grid.tmp <- plot_grid(gg2 + grid.theme + xlab(NULL), gg3 + grid.theme,
                         ncol = 1, nrow = 2, labels = c('   Mood', '   Rally'), align = 'v', rel_heights = c(2.8,1), label_size = 12, hjust = 0)
gg.grid     <- plot_grid(gg1 + grid.theme, gg.grid.tmp,
                         ncol = 2, labels = '   Attribution', rel_widths = c(1.1, 1), label_size = 12, hjust = 0) + 
  theme(panel.border = element_rect(colour = "white", fill = NA, size = 1))

# save
ggsave('results/fig_7.png', gg.grid, height = 6, width = 6)
ggsave('results/eps/fig_7.eps', gg.grid, height = 6, width = 6, device = cairo_ps)

########################################################################################################################################
### Figure 8

# estimation
ext.tmp1 <- batch.rdd(' ~ lose*factor(run_var)-1-lose | 0 | 0 | country + match_id', df1 %>% filter(abs(score_dif) < 2), out.vars[1], bw.main, triangular.weights = F)
ext.tmp2 <- batch.rdd(' ~ win*factor(run_var)-1-win   | 0 | 0 | country + match_id', df2 %>% filter(abs(score_dif) < 2), out.vars[1], bw.main, triangular.weights = F)
ext.tmp1 <- ext.tmp1 %>% filter(startsWith(var, 'lose:'))
ext.tmp2 <- ext.tmp2 %>% filter(startsWith(var, 'win:'))

# clean the labels
ext.tmp1 <- ext.tmp1 %>% mutate(var = 'lose', x = as.numeric(gsub('lose:factor\\(run_var\\)', '', x)))
ext.tmp2 <- ext.tmp2 %>% mutate(var = 'win',  x = as.numeric(gsub('win:factor\\(run_var\\)',  '', x)))

# the close-game RDD yields biased estimates without survey-date RDD.
# To avoid misinterpretation, subtract the estimates by the pre-game average effects.
out.df <- data.frame()
for(v in names(out.vars[1])){
  ext.sub1 <- ext.tmp1[ext.tmp1$ov == v,]
  ext.sub2 <- ext.tmp2[ext.tmp2$ov == v,]
  ref.val1 <- mean(ext.sub1$coef[ext.sub1$x %in% -3:-1])
  ref.val2 <- mean(ext.sub2$coef[ext.sub2$x %in% -3:-1])
  ext.sub1 <- ext.sub1 %>% mutate(coef.adj = coef - ref.val1, lwr.adj = lwr - ref.val1, upr.adj = upr - ref.val1)
  ext.sub2 <- ext.sub2 %>% mutate(coef.adj = coef - ref.val2, lwr.adj = lwr - ref.val2, upr.adj = upr - ref.val2)
  out.df   <- bind_rows(out.df, ext.sub1, ext.sub2)
}

# saving it
write.csv(out.df, 'results/fig_8.csv', row.names = F)

# graph
gg.df <- out.df %>% mutate(sig = (sign(lwr.adj) == sign(upr.adj)), 
                           var = if_else(startsWith(var, 'lose'), 'Loss', 'Win'), 
                           x   = factor(x))
gg    <- ggplot(gg.df, aes(y = coef.adj, x = x, ymin = lwr.adj, ymax = upr.adj, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) + geom_line(size = 0.1, col = 'gray40') +
  geom_text(aes(label = round(coef.adj, 2)), hjust = 0, vjust = 0, nudge_x = 0.1, size = 2.5) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab('Change in Trust in a Leader') + xlab('Days from a Football Game') +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  facet_grid(rows = vars(var), switch = 'y', scales = 'free_y')

# save
ggsave('results/fig_8.png', gg, height = 5, width = 6)
ggsave('results/eps/fig_8.eps', gg, height = 5, width = 6, device = cairo_ps)

########################################################################################################################################
### Figure 9

# estimation
ext.tmp1 <- batch.rdd(' ~ treat*lose | 0 | 0 | country + match_id', df.c1, w.vars, bw.main, c(lose = 'treat:lose'), digit = NULL)
ext.tmp2 <- batch.rdd(' ~ treat*win  | 0 | 0 | country + match_id', df.c2, w.vars, bw.main, c(win  = 'treat:win'),  digit = NULL)
ext.tmp1 <- correct.p(ext.tmp1)
ext.tmp2 <- correct.p(ext.tmp2)
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# saving it
write.csv(out.df, 'results/fig_9.csv', row.names = F)

# graph
gg.df <- out.df %>% 
  filter(bw == bw.main) %>%
  mutate(sig   = (star == '*'), 
         ov    = factor(ov, levels = rev(names(w.vars))),
         x_min = if_else(var == 'lose', -1.7, -0.55),
         x_max = if_else(var == 'lose',  0.55,  1.7))
gg <- ggplot(gg.df, aes(y = ov, x = coef, xmin = lwr, xmax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_y = 0.1, nudge_x = -0.2, size = 3) +
  geom_vline(xintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  scale_x_continuous(breaks = seq(-1.5, 1.5, 0.5)) +
  
  ylab(NULL) + xlab('Change in Survey Score') +
  grid.theme + theme(plot.background = NULL) + 
  facet_grid(cols = vars(var), labeller =  as_labeller(c(lose = 'Loss', win = 'Win')), scales = 'free_x') +
  geom_blank(aes(x = x_min)) + geom_blank(aes(x = x_max))

# save
ggsave('results/fig_9.png', gg, height = 5, width = 5)
ggsave('results/eps/fig_9.eps', gg, height = 5, width = 5, device = cairo_ps)

########################################################################################################################################
### Figure 10

# estimation
ext.tmp1 <- batch.rdd(' ~ treat*lose | 0 | 0 | country + match_id', df.c1, s.vars, bw.main, c(lose = 'treat:lose'), digit = NULL)
ext.tmp2 <- batch.rdd(' ~ treat*win  | 0 | 0 | country + match_id', df.c2, s.vars, bw.main, c(win  = 'treat:win'),  digit = NULL)
ext.tmp1 <- correct.p(ext.tmp1)
ext.tmp2 <- correct.p(ext.tmp2)
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# saving it
write.csv(out.df, 'results/fig_10.csv', row.names = F)

# graph
gg.df <- out.df %>% 
  filter(bw == bw.main) %>%
  mutate(sig   = (star == '*'), 
         ov    = factor(ov, levels = rev(names(s.vars))),
         x_min = -1,
         x_max = 0.5)
gg <- ggplot(gg.df, aes(y = ov, x = coef, xmin = lwr, xmax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_y = 0.1, nudge_x = -0.2, size = 3) +
  geom_vline(xintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  scale_x_continuous(breaks = seq(-1.5, 1.5, 0.5)) +
  
  ylab(NULL) + xlab('Change in Survey Score') +
  grid.theme + theme(plot.background = NULL) + 
  facet_grid(cols = vars(var), labeller =  as_labeller(c(lose = 'Loss', win = 'Win')), scales = 'free_x') +
  geom_blank(aes(x = x_min)) + geom_blank(aes(x = x_max))

# save
ggsave('results/fig_10.png', gg, height = 6, width = 5)
ggsave('results/eps/fig_10.eps', gg, height = 6, width = 5, device = cairo_ps)

########################################################################################################################################
### Table 5

# balance check (loss)
ext.tmp1 <- check.balance.rdd('treat_lose', cov.vars, ' ~ treat*lose | 0 | 0 | country + match_id', placebo.coef = 'treat:lose', df.c1, weights = make.w(df.c1$run_var, 3))
ext.tmp1 <- ext.tmp1 %>% mutate(covariates = rownames(ext.tmp1), match = 'lose')

# balance check (win)
ext.tmp2 <- check.balance.rdd('treat_win', cov.vars, ' ~ treat*win | 0 | 0 | country + match_id', placebo.coef = 'treat:win', df.c2, weights = make.w(df.c2$run_var, 3))
ext.tmp2 <- ext.tmp2 %>% mutate(covariates = rownames(ext.tmp2), match = 'win')

# save it
out.df <- bind_rows(ext.tmp1, ext.tmp2)
write.csv(out.df, 'results/tab_5.csv', row.names = F)

########################################################################################################################################
### Table 5 (density tests)

# make data frame for density test (including run_var = 0, excluding duplicated respondents)
df.d1 <- df1 %>% filter(abs(score_dif) < 2 & !duplicated(resp_id))
df.d2 <- df2 %>% filter(abs(score_dif) < 2 & !duplicated(resp_id))

# density test
print('Density test p-values (loss and win)')
print(round(DCdensity(df.d1$run_var, 0, 1, 3, plot = F), 2))
print(round(DCdensity(df.d2$run_var, 0, 1, 3, plot = F), 2))

########################################################################################################################################
### Figure 11

# analysis
ext.tmp1 <- batch.rdd(' ~ treat*lose*factor(ntile(season_appearance, 3))-1-treat-lose-treat:lose | 0 | 0 | country + match_id', df.c1, out.vars[1], bw.main)
ext.tmp2 <- batch.rdd(' ~ treat*win*factor(ntile(season_appearance, 3))-1-treat-win-treat:win    | 0 | 0 | country + match_id', df.c2, out.vars[1], bw.main)

# append
out.df <- bind_rows(ext.tmp1 %>% filter(startsWith(var, 'treat:lose:')), 
                    ext.tmp2 %>% filter(startsWith(var, 'treat:win:')))

# saving it
write.csv(out.df, 'results/fig_11.csv', row.names = F)

# dataset for ggplot
gg.df <- out.df %>% mutate(sig  = (star == '*'), 
                           var  = if_else(startsWith(var, 'treat:lose:'), 'Loss', 'Win'),
                           x    = case_when(endsWith(x, '1') ~ '1st tertile',
                                            endsWith(x, '2') ~ '2nd tertile',
                                            endsWith(x, '3') ~ '3rd tertile'))

# graph
gg <- ggplot(gg.df, aes(x = x, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.05, size = 3) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab("Change in Trust in a Leader") + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  
  facet_grid(rows = vars(var), switch = 'both', scale = 'free_x')

# save
ggsave('results/fig_11.png', gg, height = 4, width = 3)
ggsave('results/eps/fig_11.eps', gg, height = 4, width = 3, device = cairo_ps)

########################################################################################################################################
### Figure 12

# conditioning variables
cond.vars <- c('Education' = 'educ_group', 'Gender' = 'female', 'Muslim' = 'muslim', 
               'Television\noften' = 'television_dum', 'Internet\noften' = 'internet_dum')

# loop for the variables
out.df <- data.frame()
for(i in 1:length(cond.vars)){
  
  # estimation
  ext.tmp1 <- batch.rdd(paste0(' ~ treat*lose*factor(', cond.vars[i], ')-1-treat-lose-treat:lose | 0 | 0 | country + match_id'), df.c1, out.vars[1], bw.main, labels = c(model = 1, cond = names(cond.vars)[i]))
  ext.tmp2 <- batch.rdd(paste0(' ~ treat*win*factor(',  cond.vars[i], ')-1-treat-win-treat:win   | 0 | 0 | country + match_id'), df.c2, out.vars[1], bw.main, labels = c(model = 2, cond = names(cond.vars)[i]))
  ext.tmp1 <- ext.tmp1 %>% filter(startsWith(x, 'treat:lose:'))
  ext.tmp2 <- ext.tmp2 %>% filter(startsWith(x, 'treat:win:'))
  out.df   <- bind_rows(out.df, ext.tmp1, ext.tmp2)
}

# saving it
write.csv(out.df, 'results/fig_12.csv', row.names = F)

# make data
gg.df <- out.df %>% mutate(sig  = (star == '*'), 
                           x    = if_else(startsWith(var, 'treat:lose'), 'Loss', 'Win'),
                           val  = case_when(endsWith(var, 'factor(educ_group)0') ~ 'No',
                                            endsWith(var, 'factor(educ_group)1') ~ 'First',
                                            endsWith(var, 'factor(educ_group)2') ~ 'Second',
                                            endsWith(var, 'factor(educ_group)3') ~ 'Higher',
                                            endsWith(var, 'factor(female)0') ~ 'Male',
                                            endsWith(var, 'factor(female)1') ~ 'Female',
                                            endsWith(var, 'factor(muslim)0') ~ 'No',
                                            endsWith(var, 'factor(muslim)1') ~ 'Yes',
                                            endsWith(var, 'factor(television_dum)1') ~ 'Yes',
                                            endsWith(var, 'factor(television_dum)0') ~ 'No',
                                            endsWith(var, 'factor(internet_dum)1') ~ 'Yes',
                                            endsWith(var, 'factor(internet_dum)0') ~ 'No'),
                           val  = factor(val, levels = c('No', 'First', 'Second', 'Higher', 
                                                         'Male', 'Female', 'Non-Muslim', 'Muslim', 'Yes')),
                           cond = factor(cond, levels = names(cond.vars)))

# graph
gg <- ggplot(gg.df, aes(y = coef, x = val, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.1, size = 2) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  xlab(NULL) + ylab('Change in Trust in a Leader') +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  
  facet_grid(rows = vars(x), cols = vars(cond), switch = 'both', scales = 'free_x', space='free')

# save
ggsave('results/fig_12.png', gg, height = 3, width = 6)
ggsave('results/eps/fig_12.eps', gg, height = 3, width = 6, device = cairo_ps)

########################################################################################################################################
### Table A12-1

# variables
vars <- c('treat', 'run_var', 'lose', 'win', 'even', out.vars, cov.vars, w.vars, s.vars)

# summarize them
sum.df1 <- summary2(df.c1[,vars] %>% filter(abs(run_var) <= bw.main))
sum.df2 <- summary2(df.c2[,vars] %>% filter(abs(run_var) <= bw.main))

# save them
write.csv(sum.df1, 'results/tab_a12_1_a.csv', row.names = F)
write.csv(sum.df2, 'results/tab_a12_1_b.csv', row.names = F)

########################################################################################################################################
### Table A14-1

# analysis
ext.tmp1 <- batch.rdd(' ~ treat*lose | 0 | 0 | country + match_id', df.c1, out.vars, bw.main, c(lose = 'treat:lose'), digit = NULL)
ext.tmp2 <- batch.rdd(' ~ treat*win  | 0 | 0 | country + match_id', df.c2, out.vars, bw.main, c(win  = 'treat:win'),  digit = NULL)
ext.tmp1 <- correct.p(ext.tmp1)
ext.tmp2 <- correct.p(ext.tmp2)
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# save
write.csv(out.df, 'results/tab_a14_1.csv', row.names = F)

########################################################################################################################################
### Figure A15-1

# analysis
ext.tmp1 <- batch.rdd(' ~ treat*lose*factor(ntile(p_score, 3))-1-treat-lose-treat:lose | 0 | 0 | country + match_id', df.c1, out.vars[1], bw.main)
ext.tmp2 <- batch.rdd(' ~ treat*win*factor(ntile(p_score, 3))-1-treat-win-treat:win    | 0 | 0 | country + match_id', df.c2, out.vars[1], bw.main)

# append
out.df <- bind_rows(ext.tmp1 %>% filter(startsWith(var, 'treat:lose:')), 
                    ext.tmp2 %>% filter(startsWith(var, 'treat:win:')))

# saving it
write.csv(out.df, 'results/fig_a15_1.csv', row.names = F)

# dataset for ggplot
gg.df <- out.df %>% mutate(sig  = (star == '*'), 
                           var  = if_else(startsWith(var, 'treat:lose:'), 'Loss', 'Win'),
                           x    = case_when(endsWith(x, '1') ~ '1st tertile',
                                            endsWith(x, '2') ~ '2nd tertile',
                                            endsWith(x, '3') ~ '3rd tertile'))

# graph
gg <- ggplot(gg.df, aes(x = x, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.05, size = 3) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab("Change in Trust in a Leader") + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  
  facet_grid(rows = vars(var), switch = 'both', scale = 'free_x')

# save
ggsave('results/fig_a15_1.png', gg, height = 4, width = 3)
ggsave('results/eps/fig_a15_1.eps', gg, height = 4, width = 3, device = cairo_ps)

########################################################################################################################################
### Figure A15-2

# coarsening the variable
df1 <- df1 %>% mutate(score_dif2 = relevel(factor(replace(score_dif, score_dif < -3, -3)), ref = '0'))
df2 <- df2 %>% mutate(score_dif2 = relevel(factor(replace(score_dif, score_dif >  3,  3)), ref = '0'))

# estimation
ext.tmp1 <- batch.rdd('~ treat*score_dif2 | 0 | 0 | country + match_id', df1 %>% filter(run_var != 0), out.vars[1], bw.main)
ext.tmp2 <- batch.rdd('~ treat*score_dif2  | 0 | 0 | country + match_id', df2 %>% filter(run_var != 0), out.vars[1], bw.main)
out.df   <- bind_rows(ext.tmp1, ext.tmp2) %>% filter(grepl('treat:score_dif2', var)) %>% mutate(score_dif = as.numeric(gsub('treat:score_dif2', '', var)))

# saving it
write.csv(out.df, 'results/fig_a15_2.csv', row.names = F)

# graph
gg.df <- out.df %>% rbind(., setNames(data.frame('', '', 3, '', 0, 0, 0, 0, 0, 0, 1, '', 0), colnames(.))) %>% mutate(sig  = (star == '*'))
gg    <- ggplot(gg.df, aes(x = score_dif, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.05, size = 2.5) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_x_continuous(breaks = -3:3, labels = c('< -3', -2:2, '> 3')) +
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab("Change in Trust in a Leader") + xlab('Score Difference') +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside")

# save
ggsave('results/fig_a15_2.png', gg, height = 3, width = 5)
ggsave('results/eps/fig_a15_2.eps', gg, height = 3, width = 5, device = cairo_ps)

########################################################################################################################################
### Figure A16-1

# lists of subset
s.list1 <- list("2005-09" = (df.c1$year %in% 2005:2009), 
                "2010-14" = (df.c1$year %in% 2009:2014),
                "2015-18" = (df.c1$year %in% 2015:2018))
s.list2 <- list("2005-09" = (df.c2$year %in% 2005:2009), 
                "2010-14" = (df.c2$year %in% 2009:2014),
                "2015-18" = (df.c2$year %in% 2015:2018))

# estimation
ext.tmp1 <- batch.rdd('~ treat*lose | 0 | 0 | country + match_id', df.c1, out.vars[1], bw.main, 'treat:lose', s.list1)
ext.tmp2 <- batch.rdd('~ treat*win  | 0 | 0 | country + match_id', df.c2, out.vars[1], bw.main, 'treat:win',  s.list2)
out.df1  <- bind_rows(ext.tmp1, ext.tmp2)

# lists of subset
s.list1 <- list("West"   = (df.c1$birth_subregion == "Western Africa"), 
                "South"  = (df.c1$birth_subregion == "Southern Africa"),
                "North"  = (df.c1$birth_subregion == "Northern Africa"),
                "East"   = (df.c1$birth_subregion == "Eastern Africa"))
s.list2 <- list("West"   = (df.c2$birth_subregion == "Western Africa"), 
                "South"  = (df.c2$birth_subregion == "Southern Africa"),
                "North"  = (df.c2$birth_subregion == "Northern Africa"),
                "East"   = (df.c2$birth_subregion == "Eastern Africa"))

# estimation
ext.tmp1 <- batch.rdd('~ treat*lose | 0 | 0 | country + match_id', df.c1, out.vars[1], bw.main, 'treat:lose', s.list1)
ext.tmp2 <- batch.rdd('~ treat*win  | 0 | 0 | country + match_id', df.c2, out.vars[1], bw.main, 'treat:win',  s.list2)
out.df2  <- bind_rows(ext.tmp1, ext.tmp2)

# graph
out.df <- bind_rows(out.df1 %>% mutate(type = 'Time'), out.df2 %>% mutate(type = 'Regions'))
gg.df  <- out.df %>% mutate(sig    = (star == '*'), 
                            x      = if_else(x == 'treat:lose', 'Loss', 'Win'),
                            type   = factor(type, levels = c('Regions', 'Time', 'League')),
                            subset = factor(subset, levels = c("2005-09", "2010-14", "2015-18", 
                                                               'West', 'South', 'North', 'East')))
gg    <- ggplot(gg.df, aes(x = subset, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.1, size = 3) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab("Change in Trust in a Leader") + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  
  facet_grid(cols = vars(type), rows = vars(x), switch = 'both', scales = 'free_x', space = 'free')

# save
write.csv(out.df, 'results/fig_a16_1.csv', row.names = F)
ggsave('results/fig_a16_1.png', gg, height = 4, width = 6)
ggsave('results/eps/fig_a16_1.eps', gg, height = 4, width = 6, device = cairo_ps)

########################################################################################################################################
### Figure A17-1

# conditioning variables
cond.vars <- c('Age' = 'age_group', 'Christian' = 'christ', 'Employed' = 'employed')

# loop for the variables
out.df <- data.frame()
for(i in 1:length(cond.vars)){
  
  # estimation
  ext.tmp1 <- batch.rdd(paste0(' ~ treat*lose*factor(', cond.vars[i], ')-1-treat-lose-treat:lose | 0 | 0 | country + match_id'), df.c1, out.vars[1], bw.main, labels = c(model = 1, cond = names(cond.vars)[i]))
  ext.tmp2 <- batch.rdd(paste0(' ~ treat*win*factor(',  cond.vars[i], ')-1-treat-win-treat:win   | 0 | 0 | country + match_id'), df.c2, out.vars[1], bw.main, labels = c(model = 2, cond = names(cond.vars)[i]))
  ext.tmp1 <- ext.tmp1 %>% filter(startsWith(x, 'treat:lose:'))
  ext.tmp2 <- ext.tmp2 %>% filter(startsWith(x, 'treat:win:'))
  out.df   <- bind_rows(out.df, ext.tmp1, ext.tmp2)
}

# saving it
write.csv(out.df, 'results/fig_a17_1.csv', row.names = F)

# make data
gg.df <- out.df %>% mutate(sig  = (star == '*'), 
                           x    = if_else(startsWith(var, 'treat:lose'), 'Loss', 'Win'),
                           val  = case_when(endsWith(var, 'factor(age_group)20') ~ '<30',
                                            endsWith(var, 'factor(age_group)30') ~ '30s',
                                            endsWith(var, 'factor(age_group)40') ~ '40s',
                                            endsWith(var, 'factor(age_group)50') ~ '50s',
                                            endsWith(var, 'factor(age_group)60') ~ '>60',
                                            endsWith(var, 'factor(christ)0') ~ 'No',
                                            endsWith(var, 'factor(christ)1') ~ 'Yes',
                                            endsWith(var, 'factor(employed)0') ~ 'No',
                                            endsWith(var, 'factor(employed)1') ~ 'Yes'),
                           val  = factor(val, levels = c('<30', '30s', '40s', '50s', '>60', 'No', 'Yes', 
                                                         'Non-Muslim', 'Muslim')),
                           cond = factor(cond, levels = names(cond.vars)))

# plot it
gg    <- ggplot(gg.df, aes(y = coef, x = val, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.1, size = 2) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  xlab(NULL) + ylab('Change in Trust in a Leader') +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  
  facet_grid(rows = vars(x), cols = vars(cond), switch = 'both', scales = 'free_x', space = 'free')

# save
ggsave('results/fig_a17_1.png', gg, height = 3, width = 6)
ggsave('results/eps/fig_a17_1.eps', gg, height = 3, width = 6, device = cairo_ps)

########################################################################################################################################
### Table A18-1

# load the data
df.ct  <- readRDS('data/survey_final_citizenship.rds') %>% filter(both_teams_citizenship == 0)
df.ct1 <- df.ct %>% filter(result != 'win')  %>% mutate(p_score = 100 * lose_odds / (lose_odds + even_odds)) %>% filter(abs(score_dif) < 2 & run_var != 0)
df.ct2 <- df.ct %>% filter(result != 'lose') %>% mutate(p_score = 100 * win_odds  / (win_odds + even_odds))  %>% filter(abs(score_dif) < 2 & run_var != 0)

# estimation
ext.tmp1 <- batch.rdd(' ~ treat*lose | 0 | 0 | country + match_id', df.ct1, out.vars[1], bw.main, c(lose = 'treat:lose'))
ext.tmp2 <- batch.rdd(' ~ treat*win  | 0 | 0 | country + match_id', df.ct2, out.vars[1], bw.main, c(win  = 'treat:win'))
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

write.csv(out.df, 'results/tab_a18_1.csv', row.names = F)

########################################################################################################################################
### Table A18-2

# estimation
ext.tmp1 <- batch.rdd(' ~ treat*lose | 0 | 0 | country + match_id', df.c1[df.c1$absent == 0,], out.vars[1], bw.main, c(lose = 'treat:lose'), labels = c('player' = 'present'))
ext.tmp2 <- batch.rdd(' ~ treat*win  | 0 | 0 | country + match_id', df.c2[df.c2$absent == 0,], out.vars[1], bw.main, c(win  = 'treat:win'),  labels = c('player' = 'present'))
ext.tmp3 <- batch.rdd(' ~ treat*lose | 0 | 0 | country + match_id', df.c1[df.c1$absent == 1,], out.vars[1], bw.main, c(lose = 'treat:lose'), labels = c('player' = 'absent'))
ext.tmp4 <- batch.rdd(' ~ treat*win  | 0 | 0 | country + match_id', df.c2[df.c2$absent == 1,], out.vars[1], bw.main, c(win  = 'treat:win'),  labels = c('player' = 'absent'))
out.df   <- bind_rows(ext.tmp1, ext.tmp2, ext.tmp3, ext.tmp4)

# save it
write.csv(out.df, 'results/tab_a18_2.csv', row.names = F)

########################################################################################################################################
### Table A18-3

# dropping obs
df.s1 <- df.c %>% 
  group_by(country, match_date) %>% 
  summarise(idx_lose = mean(lose), idx_win = mean(win)) %>% 
  left_join(df.c1, ., by = c('country', 'match_date')) %>%
  filter(!(idx_lose > 0 & idx_win > 0))
df.s2 <- df.c %>% 
  group_by(country, match_date) %>% 
  summarise(idx_lose = mean(lose), idx_win = mean(win)) %>% 
  left_join(df.c2, ., by = c('country', 'match_date')) %>%
  filter(!(idx_lose > 0 & idx_win > 0))

# estimation
ext.tmp1 <- batch.rdd(' ~ treat*lose | 0 | 0 | country + match_id', df.s1, out.vars[1], bw.main, c(lose = 'treat:lose'))
ext.tmp2 <- batch.rdd(' ~ treat*win  | 0 | 0 | country + match_id', df.s2, out.vars[1], bw.main, c(win  = 'treat:win'))
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

write.csv(out.df, 'results/tab_a18_3.csv', row.names = F)

########################################################################################################################################
### Table A18-4

# aggregation
df.agg1 <- df.c1 %>% 
  group_by(resp_id, country, match_team_id) %>%
  summarize_at(vars(out.vars, 'treat', 'lose', 'run_var', 'match_id'), first)
df.agg2 <- df.c2 %>% 
  group_by(resp_id, country, match_team_id) %>%
  summarize_at(vars(out.vars, 'treat', 'win', 'run_var', 'match_id'), first)

# estimation
ext.tmp1 <- batch.rdd(' ~ treat*lose | 0 | 0 | country + match_id', df.agg1, out.vars[1], bw.main, c(lose = 'treat:lose'))
ext.tmp2 <- batch.rdd(' ~ treat*win  | 0 | 0 | country + match_id', df.agg2, out.vars[1], bw.main, c(win  = 'treat:win'))
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# save
write.csv(out.df, 'results/tab_a18_4.csv', row.names = F)

########################################################################################################################################
### Table A18-5

# estimation
ext.tmp1 <- batch.rdd(' ~ treat*lose | 0 | 0 | country + match_id', df1 %>% filter(run_var != 0), out.vars[1], bw.main, c(lose = 'treat:lose'))
ext.tmp2 <- batch.rdd(' ~ treat*win  | 0 | 0 | country + match_id', df2 %>% filter(run_var != 0), out.vars[1], bw.main, c(win  = 'treat:win'))
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# save
write.csv(out.df, 'results/tab_a18_5.csv', row.names = F)

########################################################################################################################################
### Table A18-6

# matching
mtch1 <- matching(group.id  = df.c1$match_team_id, treat = df.c1$lose, p.score = df.c1$p_score, caliper = 5)
mtch2 <- matching(group.id  = df.c2$match_team_id, treat = df.c2$win,  p.score = df.c2$p_score, caliper = 5)

# estimation
ext.tmp1 <- batch.rdd(' ~ treat*lose | 0 | 0 | country + match_id', df.c1[mtch1,], out.vars[1], bw.main, c(lose = 'treat:lose'))
ext.tmp2 <- batch.rdd(' ~ treat*win  | 0 | 0 | country + match_id', df.c2[mtch2,], out.vars[1], bw.main, c(win  = 'treat:win'))
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# save it
write.csv(out.df, 'results/tab_a18_6.csv', row.names = F)

########################################################################################################################################
### Table A18-7

# estimation
### no variation in red_card in df.c1
### no variation in goal, assist, yellow card, or red card in df.c2
ext.tmp1 <- batch.rdd(paste0(' ~ treat*lose +', paste(cov.vars, collapse = '+'), ' + goal + assist + yellow_card | 0 | 0 | country + match_id'), df.c1, out.vars[1], bw.main, c(lose = 'treat:lose'))
ext.tmp2 <- batch.rdd(paste0(' ~ treat*win  +', paste(cov.vars, collapse = '+'), ' | 0 | 0 | country + match_id'), df.c2, out.vars[1], bw.main, c(win  = 'treat:win'))
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# save it
write.csv(out.df, 'results/tab_a18_7.csv', row.names = F)

########################################################################################################################################
### Table A18-8

# loop for polynomial order
out.df <- data.frame()
for(i in 1:2){
  
  # estimation
  ext.tmp1 <- batch.rdd(paste0(' ~ poly(run_var,', i, ')*treat + lose + treat:lose | 0 | 0 | country + match_id'), df.c1, 
                        out.vars[1], bw.main, c(lose = 'treat:lose'), labels = c(order = i))
  ext.tmp2 <- batch.rdd(paste0(' ~ poly(run_var,', i, ')*treat + win  + treat:win  | 0 | 0 | country + match_id'), df.c2, 
                        out.vars[1], bw.main, c(win  = 'treat:win'),  labels = c(order = i))
  out.df   <- bind_rows(out.df, ext.tmp1, ext.tmp2)
}

# save the results
write.csv(out.df, 'results/tab_a18_8.csv', row.names = F)

########################################################################################################################################
### Figure A18-1

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
  
  ext.tmp1 <- tryCatch({
    batch.rdd(paste0(' ~ treat*lose |', FEs[i], '| 0 | player_id + match_id'), df.c1, out.vars[1], bw.main, c(lose = 'treat:lose'), labels = c(fe = names(FEs)[i], ps = PSs[i]))
  }, warning = function(w){})
  
  ext.tmp2 <- tryCatch({
    batch.rdd(paste0(' ~ treat*win |', FEs[i], '| 0 | player_id + match_id'), df.c2, out.vars[1], bw.main, c(win = 'treat:win'), labels = c(fe = names(FEs)[i], ps = PSs[i]))
  }, warning = function(w){})
  
  out.df <- rbind(out.df, ext.tmp1, ext.tmp2)
}

# saving it
write.csv(out.df, 'results/fig_a18_1.csv', row.names = F)

# graph
gg.df <- out.df %>% filter(x == 'treat:lose') %>% mutate(sig = (star == '*'), fe = factor(fe, levels = unique(names(FEs))), ps = factor(ps, levels = rev(unique(PSs))))
gg <- ggplot(gg.df, aes(x = fe, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) + 
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.1, size = 2) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab('Change in Trust in a Leader') + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  facet_grid( rows = vars(ps), switch = 'y', scales = 'free_x')

# save
ggsave('results/fig_a18_1_a.png', gg, height = 3, width = 5)
ggsave('results/eps/fig_a18_1_a.eps', gg, height = 3, width = 5, device = cairo_ps)

# graph
gg.df <- out.df %>% filter(x == 'treat:win') %>% mutate(sig = (star == '*'), fe = factor(fe, levels = unique(names(FEs))), ps = factor(ps, levels = rev(unique(PSs))))
gg <- ggplot(gg.df, aes(x = fe, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) + 
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.1, size = 2) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab('Change in Trust in a Leader') + xlab(NULL) +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  facet_grid( rows = vars(ps), switch = 'y', scales = 'free_x')

# save
ggsave('results/fig_a18_1_b.png', gg, height = 3, width = 5)
ggsave('results/eps/fig_a18_1_b.eps', gg, height = 3, width = 5, device = cairo_ps)

########################################################################################################################################
### Figure A18-2

# estimation
ext.tmp1 <- batch.rdd(' ~ treat*lose | 0 | 0 | country + match_id', df.c1, out.vars[1], bws, c(lose = 'treat:lose'))
ext.tmp2 <- batch.rdd(' ~ treat*win  | 0 | 0 | country + match_id', df.c2, out.vars[1], bws, c(win  = 'treat:win'))
out.df   <- bind_rows(ext.tmp1, ext.tmp2)

# saving it
write.csv(out.df, 'results/fig_a18_2.csv', row.names = F)

# graph
gg.df <- out.df %>% mutate(x   = if_else(x == 'treat:lose', 'Loss', 'Win'), 
                           sig = (star == '*'), 
                           lab = factor(paste0(bw, '\n(N = ', n, ')'), levels = paste0(bw, '\n(N = ', n, ')')))
gg    <- ggplot(gg.df, aes(x = lab, y = coef, ymin = lwr, ymax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_x = 0.05, size = 3) +
  geom_hline(yintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab("Change in Trust in a Leader") + xlab('Time Window') +
  theme_tufte() + theme(legend.position = 'none', strip.placement = "outside") +
  
  facet_grid(cols = vars(x), switch = 'both', scales = 'free_x')

# save
ggsave('results/fig_a18_2.png', gg, height = 3, width = 6)
ggsave('results/eps/fig_a18_2.eps', gg, height = 3, width = 6, device = cairo_ps)

########################################################################################################################################
### Figure A18-3

# lists for loop
countries <- sort(unique(c(df.c1$country, df.c2$country)))

# loop
out.df <- data.frame()
for(cntry in countries){
  
  # analysis
  ext.tmp1 <- batch.rdd(' ~ treat*lose | 0 | 0 | country + match_id', df.c1[df.c1$country != cntry,], out.vars[1], bw.main, c(lose = 'treat:lose'), labels = c(country = cntry))
  ext.tmp2 <- batch.rdd(' ~ treat*win  | 0 | 0 | country + match_id', df.c2[df.c2$country != cntry,], out.vars[1], bw.main, c(win  = 'treat:win'),  labels = c(country = cntry))
  out.df   <- bind_rows(out.df, ext.tmp1, ext.tmp2)
}

# saving it
write.csv(out.df, 'results/fig_a18_3.csv', row.names = F)

# plot it
gg.df <- out.df %>% filter(var == 'lose') %>% 
  mutate(sig = (star == '*'), 
         n   = nrow(df.c1) - n,
         lab = factor(paste0(country, ' (N = ', n, ')'), levels = paste0(country, ' (N = ', n, ')')[order(coef, decreasing = T)]))
gg <- ggplot(gg.df, aes(x = coef, y = lab, xmin = lwr, xmax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_y = 0.1, nudge_x = 0.005, size = 3) +
  geom_vline(xintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab('Dropped Country') + xlab("Change in Trust in a Leader") +
  theme_tufte() + theme(legend.position = 'none')

# save
ggsave('results/fig_a18_3_a.png', gg, height = 6, width = 4)
ggsave('results/eps/fig_a18_3_a.eps', gg, height = 6, width = 4, device = cairo_ps)

# plot it
gg.df <- out.df %>% filter(var == 'win') %>% 
  mutate(sig = (star == '*'), 
         n   = nrow(df.c2) - n,
         lab = factor(paste0(country, ' (N = ', n, ')'), levels = paste0(country, ' (N = ', n, ')')[order(coef, decreasing = T)]))
gg <- ggplot(gg.df, aes(x = coef, y = lab, xmin = lwr, xmax = upr, color = sig, group = 1)) +
  
  geom_errorbar(width = 0, size = 0.1) + geom_point(size = 1.5) +
  geom_text(aes(label = round(coef,2)), hjust = 0, vjust = 0, nudge_y = 0.1, nudge_x = 0.005, size = 3) +
  geom_vline(xintercept = 0, linetype = 'dashed', col = 'black', size = 0.5, alpha = 0.2) +
  
  scale_color_manual(values = c('FALSE' = 'black', 'TRUE' = 'indianred')) +
  
  ylab('Dropped Country') + xlab("Change in Trust in a Leader") +
  theme_tufte() + theme(legend.position = 'none')

# save
ggsave('results/fig_a18_3_b.png', gg, height = 6, width = 4)
ggsave('results/eps/fig_a18_3_b.eps', gg, height = 6, width = 4, device = cairo_ps)
