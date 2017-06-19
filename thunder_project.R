library(readr)
library(ggplot2)
library(ggthemes)
library(data.table)
library(gdata)

# read in data ------------------------------------------------------------

nba <- read_csv("pbp.csv")
ft15 <- read_csv("ftPct15-16.csv")
ft16 <- read_csv("ftPct16-17.csv")
ft <- merge(ft15,ft16, by="Team") # merge the free throw data
standings <- read_csv('standings.csv')





# creating a year variable ------------------------------------------------

nba$year <- substr(nba$game_id, start=4, stop=5)
nba$year[nba$year=='15'] <- '15-16'
nba$year[nba$year=='16'] <- '16-17'
nba$year <- as.factor(nba$year)





# creating score variables --------------------------------------------------

# first fix the nas in score, fill the NA with the previous row
na.lomf <- function(x) {
  if (length(x) > 0L) {
    non.na.idx <- which(!is.na(x))
    if (is.na(x[1L])) {
      non.na.idx <- c(1L, non.na.idx)
    }
    rep.int(x[non.na.idx], diff(c(non.na.idx, length(x) + 1L)))
  }
}
nba$score <- na.lomf(nba$score)

# extract the scores, split them, and unlist them
character <- as.character(nba$score)
character1 <- strsplit(character, split = " - ")
character2 <- unlist(character1)

# add the scores into the df as numeric values
nba$home_score <- as.numeric(character2[seq(2,588732,2)])
nba$away_score <- as.numeric(character2[seq(1,588731,2)])
nba$score_difference <- nba$home_score - nba$away_score






# create a winning variable -----------------------------------------------

# new df with unique games reversed (last occurence)
nba$sequence_id <- nba$sequence_id + 1 #add 1 because the first entry starts at 0
resultdf <- nba[sort(nba$sequence_id, decreasing = T),c(2,22)]
resultdf <- resultdf[!duplicated(resultdf$game_id),]

# add result into new df
resultdf$result <- ifelse(resultdf$score_difference > 0, '1',
                     ifelse(resultdf$score_difference < 0, '0', NA))

# merge df to have a result variable for game
nba <- merge(nba,resultdf, by='game_id')

# fix variables
nba$score_difference.y <- NULL
colnames(nba)[colnames(nba) == 'score_difference.x'] <- 'score_difference'
nba$result <- as.factor(nba$result)

# add a 1 for when the game is over, helps force predictions toward 0 or 1
nba$final <- ifelse(nba$event_type == 'End Period' & nba$score_difference !=0,
                    1, 0)
nba$final <- as.factor(nba$final)





# convert time to variable to a numeric value of time played --------------

# in order to make time easier to make calculations with, convert to how much
# time has been played in seconds instead of time remaing
# so if there is 3:55 remaining, we have 720-(3*60+55) = 485
# then 485 + 2160 = 2645 (add the time from previous 3 quarters)
minutes <- as.numeric(substr(nba$play_clock, start=1,stop=2))
seconds <- as.numeric(substr(nba$play_clock, start=4,stop=5))
nba$time_played <- 720 - (minutes*60 + seconds)
nba$time_played <- nba$time_played + 2160 #add time for previous 3 quarters





# less than two minutes ---------------------------------------------------

# creating a variable for if there is less than 2 minutes
# this is to account for the importance of late game situations

nba$last2 <- ifelse(nba$time_played >= 2760, 1, 0)
nba$last2 <- as.factor(nba$last2)





# getting rid of overtime games -------------------------------------------
# since overtime periods (and 4th quarters of those games) should be treated 
# differently than regualr games, they have been excluded from the data since
# they take up aboout only 6% of the games

ot <- nba$game_id[nba$period>4]
ot1 <- unique(ot)

nba <- nba[!(nba$game_id %in% ot1),]

# if overtime is wanted in the dataset, the following code can be used to fix it
#nba$time_played <- ifelse(nba$period == 4, nba$time_played , ifelse(
#  nba$period == 5, nba$time_played + 2460, ifelse(
#    nba$period == 6, nba$time_played + 2760, ifelse(
#      nba$period == 7, nba$time_played + 3060, test$time_played + 1200
#    )
#  )
#))

#would need to adjust 'last 2' variable if overtime was used





# creating team variables -------------------------------------------------

# getting the id for each team in original df
nba$home_team <- ifelse(is.na(nba$away_description), nba$player1_team, NA)
nba$away_team <- ifelse(is.na(nba$home_description), nba$player1_team, NA)

# making a new df with unique game_id and home team
hteamdf <- nba[,c(1,27)]
hteamdf <- unique(hteamdf[c('game_id','home_team')])
hteamdf <- na.omit(hteamdf)
hteamdf <- hteamdf[!duplicated(hteamdf$game_id),]
nba <- merge(nba, hteamdf, by='game_id') #merge dfs to get home teams

# making a new df with unique game_id and away team
ateamdf <- nba[,c(1,28)]
ateamdf <- unique(ateamdf[c('game_id','away_team')])
ateamdf <- na.omit(ateamdf)
ateamdf <- ateamdf[!duplicated(ateamdf$game_id),]
nba <- merge(nba, ateamdf, by='game_id') #merge dfs to get away teams

# fixing variables
nba$home_team.x <- NULL
nba$away_team.x <- NULL
colnames(nba)[colnames(nba) == 'home_team.y'] <- 'home_team'
colnames(nba)[colnames(nba) == 'away_team.y'] <- 'away_team'






# adding extra variables to each year -------------------------------------

# creating df by year
nba15 <- nba[nba$year=='15-16',]
nba16 <- nba[nba$year=='16-17',]

# add final rankings for the 15-16 season and fix variable names
r15 <- standings[,c(1,2)]
nba15 <- merge(nba15,r15,by.x='home_team',by.y='Team')
nba15 <- merge(nba15,r15,by.x='away_team',by.y='Team')
colnames(nba15)[colnames(nba15) == 'rank15.x'] <- 'home_rank'
colnames(nba15)[colnames(nba15) == 'rank15.y'] <- 'away_rank'

# add final rankings for the 16-17 season and fix variable names
r16 <- standings[,c(1,3)]
nba16 <- merge(nba16,r16,by.x='home_team',by.y='Team')
nba16 <- merge(nba16,r16,by.x='away_team',by.y='Team')
colnames(nba16)[colnames(nba16) == 'rank16.x'] <- 'home_rank'
colnames(nba16)[colnames(nba16) == 'rank16.y'] <- 'away_rank'

# converting home ft from 15 to decimal
fthome15 <- ft$fthome15
fthome15 <- gsub("%", "", fthome15)
fthome15 <- as.numeric(fthome15)
fthome15 <- fthome15/100
ft$fthome15 <- fthome15

# converting away ft from 15 to decimal
ftaway15 <- ft$ftaway15
ftaway15 <- gsub("%", "", ftaway15)
ftaway15 <- as.numeric(ftaway15)
ftaway15 <- ftaway15/100
ft$ftaway15 <- ftaway15

# converting home ft from 16 to decimal
fthome16 <- ft$fthome16
fthome16 <- gsub("%", "", fthome16)
fthome16 <- as.numeric(fthome16)
fthome16 <- fthome16/100
ft$fthome16 <- fthome16

# converting away ft from 16 to decimal
ftaway16 <- ft$ftaway16
ftaway16 <- gsub("%", "", ftaway16)
ftaway16 <- as.numeric(ftaway16)
ftaway16 <- ftaway16/100
ft$ftaway16 <- ftaway16

# merging the dataframes to get free throw percentage for home and away
dfh15 <- ft[,c(1,3)]
nba15 <- merge(nba15,dfh15,by.x='home_team',by.y='Team')
dfa15 <- ft[,c(1,4)]
nba15 <- merge(nba15,dfa15,by.x='away_team',by.y='Team')
dfh16 <- ft[,c(1,6)]
nba16 <- merge(nba16,dfh16,by.x='home_team',by.y='Team')
dfa16 <- ft[,c(1,7)]
nba16 <- merge(nba16,dfa16,by.x='away_team',by.y='Team')

# fix the variable names
colnames(nba15)[colnames(nba15) == 'fthome15'] <- 'ft_home'
colnames(nba15)[colnames(nba15) == 'ftaway15'] <- 'ft_away'
colnames(nba16)[colnames(nba16) == 'fthome16'] <- 'ft_home'
colnames(nba16)[colnames(nba16) == 'ftaway16'] <- 'ft_away'

# rbind the dataframes
nba <- rbind(nba15,nba16)
nba <- nba[order(nba$sequence_id),]

# create a difference in rankings for the teams with home team as the reference
# we want positive if the team is ranked higher, so we take away-home
nba$rank_difference <- nba$away_rank - nba$home_rank





# creating intentional fouls in last two minutes --------------------------
# intentional fouls here are defined as 'Personal Take Foul' in the last two minutes

nba$intent_foul_home <- as.factor(ifelse(nba$home_description %like% 'Personal Take Foul'
                                          & nba$time_played > 600, 1, 0))
nba$intent_foul_away <- as.factor(ifelse(nba$away_description %like% 'Personal Take Foul'
                                         & nba$time_played > 600, 1, 0))






# creating possession variable --------------------------------------------
# for the possession variable, we are defining it as the if the team is gaining the
# possession in the next play

# for example, if the away team makes a shot, the home team gains the next possession

# this should give us more predictive power especially in late game situations

# if the away team makes a shot with 10 seconds remaining and that same row of data
# has the away team with possession we are not gaining any information, they just 
# made a shot so of course they had the ball

# giving the home team a 1 for the previous example shows they are gaining possession
# of the ball, which should show an increase in probability of winning in late
# game situations when compared to not having a possession variable at all

# we look at every situation that can change possession or keep possession of the ball

# note that we are not counting shooting free throws as possession, so we first
# assign all fouls in the penalty a value of 0 and also don't count shooting fouls

# also in cases with 'Rebound', we see these only occur if a player is shooting 
# multiple fts and misses any one but the last one, we assign these the previous value

# for timeoutsand subs, we will also assign the previous values
# note that it is 'Substitution ' and 'Timeout ', there is a space at the end

nba$home_next_pos <- ifelse(nba$away_description %like% '.PN', 0, 
                      ifelse(nba$home_description %like% 'REBOUND' |
                        nba$away_description %like% 'Turnover' |
                        (nba$event_type == 'Made Shot' & is.na(nba$home_description)) |
                        nba$away_description %like% 'Free Throw 1 of 1 ' |
                        nba$away_description %like% 'Free Throw 2 of 2 ' |
                        nba$away_description %like% 'Free Throw 3 of 3 ' |
                        (nba$event_type == 'Jump Ball' & nba$player3_team == nba$home_team) |
                        (nba$event_description == 'Kicked Ball' & is.na(nba$home_description)) |
                        (nba$event_description == 'Defensive Goaltending' & is.na(nba$away_description)) |
                        (nba$event_type == 'Foul' & nba$event_description %like% 'Offensive' & is.na(nba$home_description)) |
                        (nba$event_description == 'Loose Ball' & is.na(nba$home_description)) |
                        (nba$event_description == 'Clear Path' & is.na(nba$home_description)) |
                        (nba$event_description == 'Defense 3 Second' & is.na(nba$home_description)) |
                        nba$away_description %like% 'FLAGRANT' |
                        nba$away_description %like% 'IN.FOUL' |
                        (nba$event_description == 'Personal' & is.na(nba$home_description)) |
                        (nba$event_description == 'Personal Block' & is.na(nba$home_description))
                      , 1, ifelse(
                        (nba$away_description %like% 'Timeout' | nba$home_description %like% 'Timeout') |
                        nba$event_type == 'Substitution ', NA, 0
                      )))

nba$away_next_pos <- ifelse(nba$home_description %like% '.PN', 0, 
                      ifelse(nba$away_description %like% 'REBOUND' |
                        nba$home_description %like% 'Turnover' |
                        (nba$event_type == 'Made Shot' & is.na(nba$away_description)) |
                        nba$home_description %like% 'Free Throw 1 of 1 ' |
                        nba$home_description %like% 'Free Throw 2 of 2 ' |
                        nba$home_description %like% 'Free Throw 3 of 3 ' |
                        (nba$event_type == 'Jump Ball' & nba$player3_team == nba$away_team) |
                        (nba$event_type == 'Kicked Ball' & is.na(nba$away_description)) |
                        (nba$event_description == 'Defensive Goaltending' & is.na(nba$home_description)) |
                        (nba$event_type == 'Foul' & nba$event_description %like% 'Offensive' & is.na(nba$away_description)) |
                        (nba$event_description == 'Loose Ball' & is.na(nba$away_description)) |
                        (nba$event_description == 'Clear Path' & is.na(nba$away_description)) |
                        (nba$event_description == 'Defense 3 Second' & is.na(nba$away_description)) |
                        nba$home_description %like% 'FLAGRANT' |
                        nba$home_description %like% 'IN.FOUL' |
                        (nba$event_description == 'Personal' & is.na(nba$away_description)) |
                        (nba$event_description == 'Personal Block' & is.na(nba$away_description))
                      , 1, ifelse(
                        (nba$away_description %like% 'Timeout' | nba$home_description %like% 'Timeout') |
                        nba$event_type == 'Substitution ', NA, 0
                      )))

# now we assign timeouts and subs the previous possession row

nba$home_next_pos <- ifelse((nba$away_description %like% 'Timeout' | nba$home_description %like% 'Timeout') |
                              (nba$away_description %like% 'SUB' | nba$home_description %like% 'SUB'),
                            na.lomf(nba$home_next_pos), nba$home_next_pos)

nba$away_next_pos <- ifelse((nba$away_description %like% 'Timeout' | nba$home_description %like% 'Timeout') |
                              (nba$away_description %like% 'SUB' | nba$home_description %like% 'SUB'),
                            na.lomf(nba$away_next_pos), nba$away_next_pos)

# if there are still other NAs, we assign them 0
nba$home_next_pos[is.na(nba$home_next_pos)] <- 0
nba$away_next_pos[is.na(nba$away_next_pos)] <- 0

# assign the possession values to be a factor
nba$home_next_pos <- as.factor(nba$home_next_pos)
nba$away_next_pos <- as.factor(nba$away_next_pos)





# reording the columns of the df ------------------------------------------

nba <- nba[,c('game_id', 'sequence_id', 'period', 'home_team', 'away_team',
            'play_clock', 'score', 'home_score', 'away_score',
            'score_difference', 'result','final', 'time_played', 'last2',
            'home_rank', 'away_rank','rank_difference', 'ft_home', 'ft_away',
            'home_next_pos', 'away_next_pos',
            'intent_foul_home', 'intent_foul_away', 'home_description',
            'away_description','player1_id', 'player1_name',
            'player1_team', 'player2_id','player2_name', 'player2_team',
            'player3_id', 'player3_name','player3_team', 'event_type',
            'event_description', 'year')]





# saving csv of the updated df for future work ----------------------------

#write.csv(nba,'pbp_updated.csv',row.names = F)
  #only run above line if rest of code has been run and is desired to be saved
#nba <- read_csv('pbp_updated.csv')
  #if already saved the new csv, run the above line to read directly in as df





# removing objects to clear our enviornment -------------------------------

keep(nba, sure = T)






# thunder vs kings 15-16 december --------------------------------------------

# this is the game we will be using to look at the prediction power of our models
okc.sac <- nba[nba$game_id=='0021500303',]





# making models of varying complexity -------------------------------------

# score difference only
m1 <- glm(result ~ score_difference, family='binomial', data=nba)
summary(m1)
# null deviance 363117
# residual deviance 131577

# predictions for the first model
p1 <- predict(m1, okc.sac, type='response')
tail(p1) # look at last 6 predictions
okc.sac$p1 <- p1

rm(m1,p1) #clear our environment

# score difference and time
m2 <- glm(result ~ score_difference * time_played, family='binomial', data=nba)
summary(m2)
# null deviance 363117
# residual deviance 123551, we see a significant improvement

# predictions for the second model
p2 <- predict(m2, okc.sac, type='response')
tail(p2)
okc.sac$p2 <- p2

rm(m2,p2)

# score difference, time, last2, and final
m3 <- glm(result ~ score_difference * time_played * last2 * final,
          family='binomial', data=nba)
summary(m3)
# null deviance 363117
# residual deviance 120227, see another improvement

# predictions for the third model
p3 <- predict(m3, okc.sac, type='response')
tail(p3)
okc.sac$p3 <- p3

rm(m3,p3)

# score difference, time, last2, final, intentional fouls, and fts

# we now only consider interaction terms between 3 variables and not higher
# higher order interactions are nonsignificant and take much longer to run
m4 <- glm(result ~ (score_difference + time_played + last2 + final +
                      intent_foul_home + intent_foul_away + ft_home + ft_away)^3,
          family='binomial', data=nba)
summary(m4)
# null deviance 363117
# residual deviance 119730, see a slight improvement

# predictions for the fourth model
p4 <- predict(m4, okc.sac, type='response')
tail(p4)
okc.sac$p4 <- p4

rm(m4,p4)

# score difference, time, last2, final, intentional fouls, fts, and possession
m5 <- glm(result ~ (score_difference + time_played + last2 + final +
                      intent_foul_home + intent_foul_away + ft_home + ft_away +
                      home_next_pos + away_next_pos)^3, family='binomial', data=nba)
summary(m5)
# null deviance 363117
# residual deviance 119216, another slight improvement

#predictions for the fifth model
p5 <- predict(m5, okc.sac, type='response')
tail(p5)
okc.sac$p5 <- p5

rm(m5,p5)

# score difference, time, last2, final, intentional fouls, fts, possession
# and rank difference
m6 <- glm(result ~ (score_difference + time_played + last2 + final +
                      intent_foul_home + intent_foul_away + ft_home + ft_away +
                      home_next_pos + away_next_pos + rank_difference)^3,
          family='binomial', data=nba)
summary(m6)
# null deviance 363117
# residual deviance 112622, see a significant improvement


# predictions for the sixth model
p6 <- predict(m6, okc.sac, type='response')
tail(p6)
okc.sac$p6 <- p6

# we will also get predictions for every game
predictions <- predict(m6, nba, type='response')
nba$prediction <- predictions

rm(m6,p6,predictions)

# save both as csv for future refernce

#write.csv(nba, 'pbp_predictions.csv', row.names=F)
#write.csv(okc.sac, 'okc_sac.csv', row.names=F)
#nba <- read_csv('pbp_predictions.csv')
#okc.sac <-read_csv('okc_sac.csv')

# visualization of each model ---------------------------------------------


# let us create vectors we will use as our labels for time remaining
time <- c('12:00', '9:00', '6:00', '3:00', '0:00')
numeric_time <- c(2160, 2340, 2520, 2700, 2880)

# model 1
g1 <- ggplot(okc.sac, aes(time_played, p1)) +
  geom_line(colour='white') +
  theme_hc(bgcolor='darkunica') +
  ylim(0,1) +
  scale_x_continuous(breaks=numeric_time, labels=time) +
  xlab('Time Remaining') +
  ylab('Probability of Winning') +
  theme(legend.position = 'none',
        axis.text = element_text(size=8),
        axis.title = element_text(size=10))

ggsave('graphs//model1.png', plot=g1, width=5, height=3.5)

# model 2
g2 <- ggplot(okc.sac, aes(time_played, p2)) +
  geom_line(colour='white') +
  theme_hc(bgcolor='darkunica') +
  ylim(0,1) +
  scale_x_continuous(breaks=numeric_time, labels=time) +
  xlab('Time Remaining') +
  ylab('Probability of Winning') +
  theme(legend.position = 'none',
        axis.text = element_text(size=8),
        axis.title = element_text(size=10))

ggsave('graphs//model2.png', plot=g2, width=5, height=3.5)

# model 3
g3 <- ggplot(okc.sac, aes(time_played, p3)) +
  geom_line(colour='white') +
  theme_hc(bgcolor='darkunica') +
  ylim(0,1) +
  scale_x_continuous(breaks=numeric_time, labels=time) +
  xlab('Time Remaining') +
  ylab('Probability of Winning') +
  theme(legend.position = 'none',
        axis.text = element_text(size=8),
        axis.title = element_text(size=10))

ggsave('graphs//model3.png', plot=g3, width=5, height=3.5)

# model 4
g4 <- ggplot(okc.sac, aes(time_played, p4)) +
  geom_line(colour='white') +
  theme_hc(bgcolor='darkunica') +
  ylim(0,1) +
  scale_x_continuous(breaks=numeric_time, labels=time) +
  xlab('Time Remaining') +
  ylab('Probability of Winning') +
  theme(legend.position = 'none',
        axis.text = element_text(size=8),
        axis.title = element_text(size=10))

ggsave('graphs//model4.png', plot=g4, width=5, height=3.5)

# model 5
g5 <- ggplot(okc.sac, aes(time_played, p5)) +
  geom_line(colour='white') +
  theme_hc(bgcolor='darkunica') +
  ylim(0,1) +
  scale_x_continuous(breaks=numeric_time, labels=time) +
  xlab('Time Remaining') +
  ylab('Probability of Winning') +
  theme(legend.position = 'none',
        axis.text = element_text(size=8),
        axis.title = element_text(size=10)) +
  geom_point(x=2820,y=okc.sac$p5[okc.sac$time_played==2820],
             color='red') +
  geom_text(x=2775,y=okc.sac$p5[okc.sac$time_played==2820]+.07,
            color='white',size=1.9, label='1:00 left
probability=.23')

ggsave('graphs//model5.png', plot=g5, width=5, height=3.5)

# model5 against sequence id instead of time
g5_seq <- ggplot(okc.sac, aes(sequence_id, p5)) +
  geom_line(colour='white') +
  theme_hc(bgcolor='darkunica') +
  ylim(0,1) +
  ylab('Probability of Winning') +
  theme(legend.position = 'none',
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) 

ggsave('graphs//model5_sequence.png', plot=g5_seq, width=5, height=3.5)

# model 6
g6 <- ggplot(okc.sac, aes(time_played, p6)) +
  geom_line(colour='white') +
  theme_hc(bgcolor='darkunica') +
  ylim(0,1) +
  scale_x_continuous(breaks=numeric_time, labels=time) +
  xlab('Time Remaining') +
  ylab('Probability of Winning') +
  theme(legend.position = 'none',
        axis.text = element_text(size=8),
        axis.title = element_text(size=10)) +
  geom_point(x=2820,y=okc.sac$p6[okc.sac$time_played==2820],
             color='red') +
  geom_text(x=2775,y=okc.sac$p6[okc.sac$time_played==2820]+.05,
            color='white',size=1.9, label='1:00 left
probability=.54')

ggsave('graphs//model6.png', plot=g6, width=5, height=3.5)

# model 6 with sequence id instead of time
g6_seq <- ggplot(okc.sac, aes(sequence_id, p6)) +
  geom_line(colour='white') +
  theme_hc(bgcolor='darkunica') +
  ylim(0,1) +
  ylab('Probability of Winning') +
  theme(legend.position = 'none',
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_point(x=okc.sac$sequence_id[127],y=okc.sac$p6[okc.sac$sequence_id=='38337'],
             color='red') +
  geom_text(x=okc.sac$sequence_id[118],y=okc.sac$p6[okc.sac$sequence_id=='38337']+.05,
            color='white', size=1.9,label='1:00 left
probability=.54')

ggsave('graphs//model6_sequence.png', plot=g6_seq, width=5, height=3.5)

rm(g1,g2,g3,g4,g5,g5_seq,g6,g6_seq,numeric_time,time)





# should we foul or not? --------------------------------------------------

home_behind <- nba[nba$score_difference == -1 &
                     (nba$time_played==2850 | nba$time_played==2851 | nba$time_played==2852) &
                     nba$away_next_pos==1,]

# we see games 0021600548 is nearly identical but there is 28 seconds and a steal by the leading team
# we see game 0021600849 is nearly identical but there was a steal with 30 seconds and timeout called by the leading team
# we see games 0021500420 and 0021601048 is nearly identical but there is 30 seconds left
# we see game 0021501190 matches the situation perfectly
 
away_behind <- nba[nba$score_difference == 1 &
                     (nba$time_played==2850 | nba$time_played==2851 | nba$time_played==2852 ) &
                     nba$home_next_pos==1,]

# we see game 0021500105 is nearly identical but there is 30 seconds left

# we see game 0021500681 is nearly identical but has 30 seconds and the trailing team
# just made a basket

rm(away_behind,home_behind)

# we need to slightly alter our model since some variables cannot be determined
# it will not offer the exact same probability but will be as close as we can get
# however not having rank difference will heavily sway the probability

m_foul <- glm(result ~ (score_difference + time_played + last2 + final +
                                intent_foul_home + intent_foul_away +
                                home_next_pos + away_next_pos)^3,
                    family='binomial', data=nba)

score_difference <- c(-1, -1, -1,-1,-2,-2,-3)
time_played <- c(2851,rep(2852,6))
last2 <- as.factor(rep(1,7))
final <- as.factor(rep(0,7))
intent_foul_home <- as.factor(c(0,1,rep(0,5)))
intent_foul_away <- as.factor(rep(0,7))
home_next_pos <- as.factor(c(0,0,0,1,0,1,1))
away_next_pos <- as.factor(c(1,0,1,0,1,0,0))

foul <- data.frame(score_difference,time_played,last2,final,intent_foul_home,
                        intent_foul_away,home_next_pos,away_next_pos)

p <- predict(m_foul, foul, type='response')
foul$p <- p

score_difference <- c(-1,-1, -1, -3,-4)
time_played <- c(2851, 2875, 2875, 2875, 2875)
last2 <- as.factor(rep(1,5))
final <- as.factor(rep(0,5))
intent_foul_home <- as.factor(rep(0,5))
intent_foul_away <- as.factor(rep(0,5))
home_next_pos <- as.factor(c(0,1,0,1,1))
away_next_pos <- as.factor(c(1,0,1,0,0))

no_foul <- data.frame(score_difference,time_played,last2,final,intent_foul_home,
                      intent_foul_away,home_next_pos,away_next_pos)

p <- predict(m_foul, no_foul, type= 'response')
no_foul$p <- p

rm(score_difference,time_played,last2,final,intent_foul_home,intent_foul_away,
   home_next_pos,away_next_pos,foul,no_foul,p,m_foul)






# resting players ---------------------------------------------------------

# we first look at the beginning of the fourth quarter

# teams with a high probability of winning
rest1 <- nba[nba$prediction >.85 & nba$event_type=='Start Period',]
table(rest1$result)
791/(40+791)
summary(rest1$score_difference)
rest1 <- rest1[order(rest1$score_difference),]

# teams with a low probability of winning
rest2 <- nba[nba$prediction <.15 & nba$event_type=='Start Period',]
table(rest2$result)
16/(447+16)
summary(rest2$score_difference)
rest2 <- rest2[order(rest2$score_difference),]

# now we look at in between 4 minutes and 5 minutes

# teams with a high probability of winning
rest3 <- nba[nba$prediction > .8 & nba$time_played > 2580 & nba$time_played < 2640,]
table(rest3$result)
9654/(296+9654)
summary(rest3$score_difference)
rest3 <- rest3[order(rest3$score_difference),]

#teams with a low probability of winning
rest4 <- nba[nba$prediction < .2 & nba$time_played > 2580 & nba$time_played <2640,]
table(rest4$result)
221/(6269+221)
summary(rest4$score_difference)
