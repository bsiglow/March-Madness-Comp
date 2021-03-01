######################################################
########## March Madness - Data Preparation ##########
######################################################
#
#
#
#
#

# Load tidyverse
library(tidyverse)

# Load in data
OG_RegSeasonStat <- read.csv("https://www.dropbox.com/s/di27y44wsl92dqs/MRegularSeasonDetailedResults.csv?dl=1")
# View data
head(OG_RegSeasonStat)

# Create season/team identifiers
OG_RegSeasonStat$WTIDSeason <- paste(OG_RegSeasonStat$Season, OG_RegSeasonStat$WTeamID)
sum(is.na(OG_RegSeasonStat$WTeamID))
OG_RegSeasonStat$LTIDSeason <-paste(OG_RegSeasonStat$Season, OG_RegSeasonStat$LTeamID)
sum(is.na(OG_RegSeasonStat$LTeamID))

# Team names vector
teams <- unique(c(OG_RegSeasonStat$LTIDSeason, OG_RegSeasonStat$WTIDSeason))

# Load Ranks:
rank_table <-  read.csv("https://www.dropbox.com/s/fi3m97x6424qjrb/MMasseyOrdinals.csv?dl=1")
rank_table$TeamID_Year <-  paste(rank_table$Season, rank_table$TeamID)
rank_db <- spread(rank_table, key = c("SystemName"),
                  value = c("OrdinalRank"), fill = NA)

# Calculate previous game stats:
res_w <- res_l <-  as.data.frame(matrix(NA, nrow = nrow(OG_RegSeasonStat), ncol = 25))
rank_w <- rank_l <- as.data.frame(matrix(NA, nrow = nrow(OG_RegSeasonStat), ncol = ncol(rank_db)))


for (i in 1:nrow(OG_RegSeasonStat)){
  # Extract winning team winning games
  temp_w1 <- OG_RegSeasonStat[OG_RegSeasonStat$WTIDSeason == OG_RegSeasonStat$WTIDSeason[i] &
                               OG_RegSeasonStat$DayNum < OG_RegSeasonStat$DayNum[i], c(4,6, 8:21) ]
  # Extract winning team losing games
  temp_w2 <- OG_RegSeasonStat[OG_RegSeasonStat$LTIDSeason == OG_RegSeasonStat$WTIDSeason[i] &
                                OG_RegSeasonStat$DayNum < OG_RegSeasonStat$DayNum[i], c(6, 4,8,22:34)]
  # Name winning team game columns
  names(temp_w1) <- names(temp_w2) <- c("Score", "Opp_score", "NumOT",     
                                        "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA",
                                        "OR", "DR", "Ast", "TO", "Stl", "Blk", "PF")
  # Join winning team games
  w_db <- rbind.data.frame(temp_w1, temp_w2)
  # Calculate winning team score differential
  w_db$score_diff <- w_db$Score - w_db$Opp_score
  
  # Extract losing team winning games
  temp_l1 <- OG_RegSeasonStat[OG_RegSeasonStat$WTIDSeason == OG_RegSeasonStat$LTIDSeason[i] &
                                OG_RegSeasonStat$DayNum < OG_RegSeasonStat$DayNum[i], c(4,6, 8:21) ]
  # Extract losing team losing games
  temp_l2 <- OG_RegSeasonStat[OG_RegSeasonStat$LTIDSeason == OG_RegSeasonStat$LTIDSeason[i] &
                                OG_RegSeasonStat$DayNum < OG_RegSeasonStat$DayNum[i], c(6,4,8,22:34)]
  # Name losing team game columns
  names(temp_l1) <- names(temp_l2) <- c("Score", "Opp_score",  "NumOT",     
                                        "FGM", "FGA", "FGM3", "FGA3", "FTM", "FTA",
                                        "OR", "DR", "Ast", "TO", "Stl", "Blk", "PF")
  # Join losing team games
  l_db <- rbind.data.frame(temp_l1, temp_l2)
  # Calculate losing team score differential
  l_db$score_diff <- l_db$Score - l_db$Opp_score
  
  # Extract winning team previous ranks
  w_ranks <- rank_db[rank_db$TeamID_Year == OG_RegSeasonStat$WTIDSeason[i] &
                          rank_db$RankingDayNum < OG_RegSeasonStat$DayNum[i],]
  # If number of ranks given is greater than 0
  if(nrow(w_ranks) > 0){
    # Fill in ranks till last day
    w_ranks <- fill(w_ranks, .direction = c("down"))
    # Extract ranks
    rank_w[i,] <- w_ranks[which.min(abs(w_ranks$RankingDayNum - OG_RegSeasonStat$DayNum[i])),]
  }
  # Extract losing team ranks
  l_ranks <- rank_db[rank_db$TeamID_Year == OG_RegSeasonStat$LTIDSeason[i] &
                          rank_db$RankingDayNum < OG_RegSeasonStat$DayNum[i],]
  # If number of previous losing ranks is greater than 0
  if(nrow(l_ranks) > 0){
    # Fill in ranks till last day
    l_ranks <- fill(l_ranks, .direction = c("down")) 
    # Extract losing ranks
    rank_l[i,] <- l_ranks[which.min(abs(l_ranks$RankingDayNum - OG_RegSeasonStat$DayNum[i])),]
  }
  
  # Assign teamID
  res_w[i,1] <- OG_RegSeasonStat$WTIDSeason[i] # To add in individual team id I believe this works
  res_w[i, 2] <-  sum(w_db$FGM)/sum(w_db$FGA) # Calculate Field goal percentage
  res_w[i,3] <- mean(w_db$Score) # Calculate average score
  res_w[i,4] <- mean(w_db$NumOT) # Calculate average overtime
  res_w[i, 5] <- mean(w_db$FGA) # Calculate average field goals attempts
  res_w[i, 6] <- sum(w_db$FGM3)/sum(w_db$FGA3) # Calculate 3 point percentage
  res_w[i,7] <- mean(w_db$FGA3) # Calculate average 3 point attempts
  res_w[i, 8] <- sum(w_db$FTM)/sum(w_db$FTA) # Calculate free throw percentage
  res_w[i, 9] <-  mean(w_db$FTA) # Calculate average free throws attempted
  res_w[i, 10] <- mean(w_db$OR) # Calculate average offensive rebounds
  res_w[i, 11] <- mean(w_db$DR) # Calculate average defensive rebounds
  res_w[i, 12] <- mean(w_db$Ast) # Calculate average assists
  res_w[i, 13] <- mean(w_db$TO) # Calcualte average turnovers
  res_w[i, 14] <- mean(w_db$Stl) # Calculate average steals
  res_w[i, 15] <- mean(w_db$Blk) # Calcualte  average blocks
  res_w[i, 16] <- mean(w_db$PF) # Calculate average? 
  res_w[i, 17] <- mean(w_db$score_diff) # Calcualte average score differential
  res_w[i, 18] <- sum(w_db$Score > w_db$Opp_score) # Calculate number of wins
  res_w[i, 19] <- sum(w_db$Score < w_db$Opp_score) # Calcualte number of wins
  res_w[i, 20] <- sum(w_db$FGA - w_db$FGA3)/sum(w_db$FGA3) # Calculate 2/3 point ratio
  
  # Assign teamID
  res_l[i,1] <- OG_RegSeasonStat$LTIDSeason[i] # To add in individual team id I believe this works
  res_l[i, 2] <-  sum(l_db$FGM)/sum(l_db$FGA) # Calculate field goal percentage
  res_l[i,3] <- mean(l_db$Score) # Calculate average score
  res_l[i,4] <- mean(l_db$NumOT) # Calcualte number of overtime periods
  res_l[i, 5] <- mean(l_db$FGA) # Calculate average field goal attempts
  res_l[i, 6] <- sum(l_db$FGM3)/sum(l_db$FGA3) # Calculate three point percentage
  res_l[i,7] <- mean(l_db$FGA3) # Calculate average three point attempts
  res_l[i, 8] <- sum(l_db$FTM)/sum(l_db$FTA) # Calculate free throw percentage
  res_l[i, 9] <-  mean(l_db$FTA) # Calculate average free throw attempts
  res_l[i, 10] <- mean(l_db$OR) # Calculate average offensive rebounds
  res_l[i, 11] <- mean(l_db$DR) # Calculate average defensive rebounds
  res_l[i, 12] <- mean(l_db$Ast) # Calculate average assists
  res_l[i, 13] <- mean(l_db$TO) # Calculate average turnovers
  res_l[i, 14] <- mean(l_db$Stl) # Calculate average steals
  res_l[i, 15] <- mean(l_db$Blk) # Calculate average blocks
  res_l[i, 16] <- mean(l_db$PF) # Calculate average ?
  res_l[i, 17] <- mean(l_db$score_diff) # Calculate average score differential
  res_l[i, 18] <- sum(l_db$Score > l_db$Opp_score) # Calculate number of wins
  res_l[i, 19] <- sum(l_db$Score < l_db$Opp_score) # Calculate number of losses
  res_l[i, 20] <- sum(l_db$FGA - l_db$FGA3)/sum(l_db$FGA3) # Calculate 2/3 point ratio
}


# Save collated data frame
save(res_w, res_l, rank_l, rank_w, OG_RegSeasonStat, file="collated_march_madness.rda")

# Fix names for calculated summary statistics
names(res_w)[1:20] <- names(res_l)[1:20] <- c("TeamID_Year", "FG_Prec", "Avg_Score","AVG_OT", "Avg_FGA", "FG3_Prec",
                  "Avg_FGA3", "FT_Prec", "AVG_FTA", "AVG_OR", "AVG_DR", "AVG_AST",
                  "ACG_TO", "AvG_Stl", "AVG_Blk", "AVG_PF", "AVG_Scor_Diff", "Num_Wins",
                  "Num_Loss","2pt_3pt_ratio") # Rename

# Add names to rank datasets
names(rank_w) <- names(rank_l) <- names(rank_db)

# Duplicate data frames
res_wa <- res_w
res_lb <- res_l
rank_wa <- rank_w
rank_lb <- rank_l

# Assign winning team as a and losing team as b
names(res_wa) <- paste(names(res_w), "_a", sep = "")
names(res_lb) <- paste(names(res_l), "_b", sep = "")
names(rank_wa) <- paste(names(rank_w), "_a", sep = "")
names(rank_lb) <- paste(names(rank_l), "_b", sep = "")


# Duplicate data frames
res_wb <- res_w
res_la <- res_l
rank_wb <- rank_w
rank_la <- rank_l

# Asisgn winning team as b and losing team as a
names(res_wb) <- paste(names(res_w), "_b", sep = "")
names(res_la) <- paste(names(res_l), "_a", sep = "")
names(rank_wb) <- paste(names(rank_w), "_b", sep = "")
names(rank_la) <- paste(names(rank_l), "_a", sep = "")

# Join datasets together
game_db <- rbind.data.frame(cbind.data.frame(res_wa[,1:20], res_lb[,1:20], 
                                             rank_wa[,5:ncol(rank_wa)], rank_lb[,5:ncol(rank_lb)]),
                            cbind.data.frame(res_la[,1:20], res_wb[,1:20], 
                                             rank_la[,5:ncol(rank_la)], rank_wb[,5:ncol(rank_wb)]))




## Fix ranks
# If missing then give max + 1
for(i in 41:(ncol(game_db) - 1)){
  game_db[is.na(game_db[,i]),i] <- max(game_db[,i], na.rm = T) + 1
}

## Add rank differentials
# Create data frame to store rank differentials
rank_diff <- as.data.frame(matrix(NA, nrow(game_db), ncol = ncol(rank_w) - 4))

# Loop through rank columns
for(i in 1:ncol(rank_diff)){
  # Calculate rank differential
  rank_diff[,i] <- game_db[,i+40] - game_db[,i + 214]
}
# Assign names for rank differential
names(rank_diff) <- paste(names(rank_db)[5:ncol(rank_db)], "_diff", sep = "")
# Add rank differential to game db
game_db_comp <- cbind.data.frame(game_db, rank_diff)

# Create response variable
game_db_comp$resp_var <- c(rep(1, nrow(res_w)), rep(0, nrow(res_l)))


save(game_db_comp, file = "combined_mm_data.rda")

# Drop rows and columns with missing values 

game_db_use <- game_db_comp[!is.na(game_db_comp$FG_Prec_a) &
                            !is.na(game_db_comp$FG_Prec_b) &
                            !is.na(game_db_comp$FT_Prec_a) &
                            !is.na(game_db_comp$FT_Prec_b),  
                            !names(game_db_comp) %in% c("CRW_a", "HRN_a", "MvG_a", "PH_a", "PMC_a", "ZAM_a",
                                                        "CRW_b", "HRN_b", "MvG_b", "PH_b", "PMC_b","ZAM_b",
                                                        "CRW_diff", "HRN_diff", "MvG_diff", "PH_diff",
                                                        "PMC_diff", "ZAM_diff")]
# Drop remaining missing values


summary(game_db_use[,1:100])
summary(game_db_use[,101:200])
summary(game_db_use[,201:300])
summary(game_db_use[,301:400])
summary(game_db_use[,401:500])
summary(game_db_use[,501:553])

library(splitstackshape)

split <-  stratified(game_db_use, c("resp_var"), .80, bothSets = TRUE)
train_strat <- split[[1]]
test_strat <- split[[2]]

library(xgboost)
Gtrain <- xgb.DMatrix(data = as.matrix(train_strat[,c(2:20, 22:544)]), label = as.numeric(train_strat$resp_var))
# Create test matrix
Gtest <- xgb.DMatrix(data = as.matrix(test_strat[, c(2:20, 22:544)]), label = as.numeric(test_strat$resp_var))

set.seed(1984)
Gamebeast <- xgboost(data = Gtrain, 
                     eta = .3,
                     nrounds = 1000, 
                     nthread = 4, 
                     verbose = 1, 
                     print_every_n = 20,
                     
                     objective = "binary:logistic", # Set objective
                     eval_metric = "auc",
                     eval_metric = "error")




boost_preds_1 <- predict(Gamebeast, Gtest) # Create predictions for xgboost model

boost_pred_class <- rep(0, length(boost_preds_1))
boost_pred_class[boost_preds_1 >= .5] <- 1


SmallXGBoost <- table(boost_pred_class, test_strat$resp_var) # Create table
confusionMatrix(SmallXGBoost, positive = "1")

summary(boost_preds_1[test_strat$resp_var == 1])
summary(boost_preds_1[test_strat$resp_var == 0])

plot_dat <- cbind.data.frame(boost_preds_1, test_strat$resp_var)
names(plot_dat) <- c("prediction", "actual")

g_1 <- ggplot(plot_dat, aes(x = prediction, fill = as.factor(actual)))+
  geom_density(alpha = 0.3)
g_1


### run after line 206 should complete the run for XG 
### Run XG then tune then save final check on fucnton in.
### COmplete the team stats in the bracket. 
### alluvium chart geomalluvium or All within ggplot corbybunson github http://corybrunson.github.io/ggalluvial/index.html









































