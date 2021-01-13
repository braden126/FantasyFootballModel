library(tidyverse)
library(rebus)

#gathering data from NFLscrapR for play by play Expected points added and win probability added per play, along with snaps per season
stats2017 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2017.csv"))
stats2016 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2016.csv"))
stats2015 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2015.csv"))
stats2013 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2014.csv"))
stats2012 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2013.csv"))
stats2011 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2012.csv"))
stats2010 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2010.csv"))
stats2009 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2009.csv"))

#joining all years together
Epa <- rbind(stats2017, stats2016, stats2015, stats2013, stats2012, stats2011, stats2010, stats2009)

#creates a column to be used in join with other dataset
Epa <- Epa %>%
  mutate(Year = as.character(format(game_date,"%Y")))

#selects only RBs
rbepa <- Epa %>%
  filter(play_type == "run" & qb_scramble == 0 & !is.na(epa) & !is.na(wpa)) %>% #selects only plays where ball is passed and removes few plays where EPA is not calculated
  group_by(rusher_player_name, Year ) %>%
  summarise(averageEpa = mean(epa), totalEpa = sum(epa), averageWpa = mean(wpa), totalWpa = sum(wpa), snaps = n()) %>%
  arrange(rusher_player_name, Year) 

rbs <- Raw_Data10yrs_v3 %>%
  filter(FantPos == "RB" & PosRank <= 75 & !is.na(PosRank)) %>% #selects top 75 RBs to only select fantasy relevant RBs
  arrange(Name, Year) %>%
  mutate(prev_year_rank = ifelse(lag(Name) != Name, NA, lag(PosRank)), #gathers data from previous years to make prediction using lag function
         prev_year_rankdiff = prev_year_rank - PosRank,
         prev_year_RushingY_A = ifelse(lag(Name) != Name, NA, lag(RushingY_A)),
         prev_year_Rushingatt = ifelse(lag(Name) != Name, NA, lag(RushingAtt)),
         prev_year_RushingTD = ifelse(lag(Name) != Name, NA, lag(RushingTD)),
         prev_year_G = ifelse(lag(Name) != Name, NA, lag(G)),
         prev_year_GS = ifelse(lag(Name) != Name, NA, lag(GS)),
         prev_year_PPRPG = ifelse(lag(Name) != Name, NA, lag(PPRPG)),
         prev_year_ReceivingTgt = ifelse(lag(Name) != Name, NA, lag(ReceivingTgt)),
         prev_year_ReceivingY_R = ifelse(lag(Name) != Name, NA, lag(ReceivingY_R)),
         prev_year_ReceivingTD = ifelse(lag(Name) != Name, NA, lag(ReceivingTD)),
         )

#add new column to match the NFLScrapR name format to be used in joining datasets
rbs$joinname = substr(rbs$Name, 1, 1) 
rbs$joinname = paste(rbs$joinname, str_extract(rbs$Name, pattern = 
                             capture(one_or_more(SPC)) %R%
                             capture(one_or_more(WRD))
))
rbs$joinname = gsub("  ", ".", rbs$joinname)
rbs$Year = as.character(rbs$Year)

#joins fantasy data with EPA, WPA, and snaps per season
rbsfull <- left_join(rbs, rbepa, by = c("joinname" = "rusher_player_name", "Year" = "Year"))

rbsfull <- rbsfull %>%   
  mutate(prev_year_averageEpa = ifelse(lag(Name) != Name, NA, lag(averageEpa)), #gathers data from previous years to make prediction using lag function
                                prev_year_totalEpa = ifelse(lag(Name) != Name, NA, lag(totalEpa)),
                                prev_year_averageWpa = ifelse(lag(Name) != Name, NA, lag(averageWpa)),
                                prev_year_totalWpa = ifelse(lag(Name) != Name, NA, lag(totalWpa)),
                                prev_year_snaps = ifelse(lag(Name) != Name, NA, lag(snaps)),
)

#RB model

#maybe add year2/year3
mod2trimmed <- lm(PPRPG ~ prev_year_PPRPG + prev_year_rank + Age + prev_year_G + prev_year_GS + prev_year_RushingY_A + prev_year_Rushingatt +
                    prev_year_averageEpa + prev_year_totalEpa + prev_year_averageWpa + prev_year_totalWpa + prev_year_RushingTD +
                    prev_year_ReceivingTgt + prev_year_ReceivingY_R + prev_year_ReceivingTD + prev_year_snaps, data = rbsfull)

summary(mod2trimmed)

pred1 <- predict(mod1, rbsfull)

results <- cbind(rbsfull, pred1)


#wr
wrepa <- Epa %>%
  filter(play_type == "pass" & qb_scramble == 0 & !is.na(epa) & !is.na(wpa)) %>% #selects only plays where ball is passed and removes few plays where EPA is not calculated
  group_by(receiver_player_name, Year ) %>%
  summarise(averageEpa = mean(epa), totalEpa = sum(epa), averageWpa = mean(wpa), totalWpa = sum(wpa), snaps = n()) %>%
  arrange(receiver_player_name, Year) 

wrs <- Raw_Data10yrs_v3 %>%
  filter(FantPos == "WR" & PosRank <= 75 & !is.na(PosRank)) %>% #selects top 75 WRs to only select fantasy relevant WRs
  arrange(Name, Year) %>%
  mutate(prev_year_rank = ifelse(lag(Name) != Name, NA, lag(PosRank)), #gathers data from previous years to make prediction using lag function
         prev_year_rankdiff = prev_year_rank - PosRank,
         prev_year_Rushingatt = ifelse(lag(Name) != Name, NA, lag(RushingAtt)),
         prev_year_RushingTD = ifelse(lag(Name) != Name, NA, lag(RushingTD)),
         prev_year_G = ifelse(lag(Name) != Name, NA, lag(G)),
         prev_year_GS = ifelse(lag(Name) != Name, NA, lag(GS)),
         prev_year_PPRPG = ifelse(lag(Name) != Name, NA, lag(PPRPG)),
         prev_year_ReceivingTgt = ifelse(lag(Name) != Name, NA, lag(ReceivingTgt)),
         prev_year_ReceivingY_R = ifelse(lag(Name) != Name, NA, lag(ReceivingY_R)),
         prev_year_ReceivingTD = ifelse(lag(Name) != Name, NA, lag(ReceivingTD)),
  )

#add new column to match the NFLScrapR name format to be used in joining datasets
wrs$joinname = substr(wrs$Name, 1, 1) 
wrs$joinname = paste(wrs$joinname, str_extract(wrs$Name, pattern = 
                                                 capture(one_or_more(SPC)) %R%
                                                 capture(one_or_more(WRD))
))
wrs$joinname = gsub("  ", ".", wrs$joinname)
wrs$Year = as.character(wrs$Year)

#joins fantasy data with EPA, WPA, and snaps per season
wrsfull <- left_join(wrs, wrepa, by = c("joinname" = "receiver_player_name", "Year" = "Year"))

wrsfull <- wrsfull %>%   
  mutate(prev_year_averageEpa = ifelse(lag(Name) != Name, NA, lag(averageEpa)), #gathers data from previous years to make prediction using lag function
         prev_year_totalEpa = ifelse(lag(Name) != Name, NA, lag(totalEpa)),
         prev_year_averageWpa = ifelse(lag(Name) != Name, NA, lag(averageWpa)),
         prev_year_totalWpa = ifelse(lag(Name) != Name, NA, lag(totalWpa)),
         prev_year_snaps = ifelse(lag(Name) != Name, NA, lag(snaps)),
  )


#WR model
mod3 <- lm(PPRPG ~ prev_year_PPRPG + prev_year_rank + Age + prev_year_G + prev_year_GS + prev_year_Rushingatt + prev_year_RushingTD +
                    prev_year_averageEpa + prev_year_totalEpa + prev_year_averageWpa + prev_year_totalWpa + prev_year_ReceivingTgt
                    prev_year_ReceivingY_R + prev_year_ReceivingTD + prev_year_snaps, data = wrsfull)

summary(mod3)

pred2 <- predict(mod3, wrsfull)

results2 <- cbind(wrsfull, pred2)


#qb
qbepa <- Epa %>%
  filter(play_type == "pass" & !is.na(epa) & !is.na(wpa)) %>% #this time qb scrambles do not need to be removed 
  group_by(passer_player_name, Year ) %>%
  summarise(averageEpa = mean(epa), totalEpa = sum(epa), averageWpa = mean(wpa), totalWpa = sum(wpa), snaps = n()) %>%
  arrange(passer_player_name, Year) 

qbs <- Raw_Data10yrs_v3 %>%
  filter(FantPos == "QB" & PosRank <= 75 & !is.na(PosRank)) %>% #selects top 75 Qbs to only select fantasy relevant QBs
  arrange(Name, Year) %>%
  mutate(prev_year_rank = ifelse(lag(Name) != Name, NA, lag(PosRank)), #gathers data from previous years to make prediction using lag function
         prev_year_rankdiff = prev_year_rank - PosRank,
         prev_year_Rushingatt = ifelse(lag(Name) != Name, NA, lag(RushingAtt)),
         prev_year_RushingTD = ifelse(lag(Name) != Name, NA, lag(RushingTD)),
         prev_year_G = ifelse(lag(Name) != Name, NA, lag(G)),
         prev_year_GS = ifelse(lag(Name) != Name, NA, lag(GS)),
         prev_year_PPRPG = ifelse(lag(Name) != Name, NA, lag(PPRPG)),
         prev_year_PassingCmp = ifelse(lag(Name) != Name, NA, lag(PassingCmp)),
         prev_year_PassingAtt = ifelse(lag(Name) != Name, NA, lag(PassingAtt)),
         prev_year_PassingYds = ifelse(lag(Name) != Name, NA, lag(PassingYds)),
         prev_year_PassingTD = ifelse(lag(Name) != Name, NA, lag(PassingTD))
  )

#add new column to match the NFLScrapR name format to be used in joining datasets
qbs$joinname = substr(qbs$Name, 1, 1) 
qbs$joinname = paste(qbs$joinname, str_extract(qbs$Name, pattern = 
                                                 capture(one_or_more(SPC)) %R%
                                                 capture(one_or_more(WRD))
))
qbs$joinname = gsub("  ", ".", qbs$joinname)
qbs$Year = as.character(qbs$Year)

#joins fantasy data with EPA, WPA, and snaps per season
qbsfull <- left_join(qbs, qbepa, by = c("joinname" = "passer_player_name", "Year" = "Year"))

qbsfull <- qbsfull %>%   
  mutate(prev_year_averageEpa = ifelse(lag(Name) != Name, NA, lag(averageEpa)), #gathers data from previous years to make prediction using lag function
         prev_year_totalEpa = ifelse(lag(Name) != Name, NA, lag(totalEpa)),
         prev_year_averageWpa = ifelse(lag(Name) != Name, NA, lag(averageWpa)),
         prev_year_totalWpa = ifelse(lag(Name) != Name, NA, lag(totalWpa)),
         prev_year_snaps = ifelse(lag(Name) != Name, NA, lag(snaps)),
  )


#QB model
mod4 <- lm(PPRPG ~ prev_year_PPRPG + prev_year_rank + Age + prev_year_G + prev_year_GS + prev_year_Rushingatt + prev_year_RushingTD +
             prev_year_averageEpa + prev_year_totalEpa + prev_year_averageWpa + prev_year_totalWpa + prev_year_PassingCmp +
           prev_year_PassingAtt + prev_year_PassingYds + prev_year_PassingTD + prev_year_snaps, data = qbsfull)

summary(mod4)

pred3 <- predict(mod4, qbsfull)

results3 <- cbind(qbsfull, pred3)
