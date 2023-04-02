
# log file
sink('results/log.txt')

# Clear the environment
remove(list = ls())

# Checkpoint
checkpoint::checkpoint('2020-07-30', checkpoint_location = 'rpkgs', r_version = '4.0.2', scan_now = F)

# description of african soccer players
source('replication data/rcodes/describe_soccer.R')

# analyzing the panel dataset
source('replication data/rcodes/analyze_panel.R')

# analyzing the survey data
source('replication data/rcodes/analyze_survey.R')

# correlation between player presence and twitter followers
source('replication data/rcodes/analyze_twitter.R')

# correlation between player presence and Google search trends
source('replication data/rcodes/analyze_gtrend.R')

# end the log file
sink()
