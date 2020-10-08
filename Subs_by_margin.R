library(tidyverse)
library(superNetballR)

Season_id <- "11108"

Game_Register <- list(roundNumber = rep(1:14,each = 4), matchNumber = rep(1:4,times = 14))
Game_Register <- pmap(Game_Register, function(roundNumber,matchNumber) downloadMatch(Season_id,roundNumber,matchNumber))

match_stats <- Game_Register %>% map_df(tidyMatch) %>%
  left_join(
    tibble(homeTeam = "Home",
           squadId = map_dbl(Game_Register,~.x[["matchInfo"]][["homeSquadId"]]),
           matchNumber = map_dbl(Game_Register,~.x[["matchInfo"]][["matchNumber"]]),
           roundNumber = map_dbl(Game_Register,~.x[["matchInfo"]][["roundNumber"]])),
    by = c("squadId" = "squadId",
           "game" = "matchNumber",
           "round" = "roundNumber")) %>% 
  replace_na(list(homeTeam = "Away"))

player_subs <- tibble(Round = map_dbl(Game_Register,~.x$matchInfo$roundNumber),
                     Match = map_dbl(Game_Register,~.x$matchInfo$matchNumber),
                     Subs = map(Game_Register,~.x$playerSubs)) %>% 
  unnest_wider(Subs) %>% unnest_longer(player) %>% unnest_wider(player) %>%
  mutate_at(vars(fromPos,toPos),fct_relevel, c("GS","GA","WA","C","WD","GD","GK")) %>% 
  mutate_at(vars(squadId, playerId),as.character) %>% 
  mutate(sub_combined = paste(fromPos,toPos,sep = "-"),
         scorepoints = NA) %>% 
  select(Round, Match,period,periodSeconds,squadId,sub_combined)

score_flow <- tibble(Round = map_dbl(Game_Register,~.x$matchInfo$roundNumber),
                         Match = map_dbl(Game_Register,~.x$matchInfo$matchNumber),
                         Scores = map(Game_Register,~.x$scoreFlow)) %>% 
  unnest_wider(Scores) %>% unnest_longer(score) %>% unnest_wider(score) %>% 
  filter(scoreName %in% c("goal","2pt Goal")) %>%
  mutate(squadId = as.character(squadId),
         sub_combined = NA) %>%
  select(Round, Match,period,periodSeconds,squadId,scorepoints)
 
home_away <- match_stats %>% 
  distinct(round,game,squadId,squadNickname,homeTeam) %>% 
  transmute(Round = round,
            Match = game,
            squadId = as.character(squadId),
            squadNickname,homeTeam)

subs_with_scores <- bind_rows(score_flow,player_subs) %>%
    left_join(home_away) %>% 
    arrange(Round,Match,period,periodSeconds) %>% 
    group_by(Round,Match) %>% 
    mutate(score_difference = if_else(homeTeam == "Home",scorepoints,-scorepoints),
           score_difference = replace_na(score_difference,0),
           score_difference = cumsum(score_difference)) %>% 
    mutate(score_difference = if_else(homeTeam == "Home",score_difference,-score_difference),
           score_margin = case_when(
             between(score_difference,6,100) ~ "6+ up", 
             between(score_difference,1,5) ~ "1-5 up", 
             score_difference == 0 ~ "Level",
             between(score_difference,-5,-1) ~ "1-5 down", 
             between(score_difference,-100,-6) ~ "6+ down"),
           score_difference = if_else(homeTeam == "Home",score_difference,-score_difference))

subs_with_scores %>%
  filter(periodSeconds > 0,!is.na(sub_combined)) %>% 
  mutate(period_minute = round(periodSeconds/60),
         Super = if_else(period_minute  >= 10,"Power 5","Regulation")) %>% 
  count(period,period_minute,Super,score_margin) %>%
  ggplot(aes(x = period_minute,y = n,fill = Super)) +
  geom_col() +
  facet_grid(fct_relevel(score_margin,c("6+ up","1-5 up","Level","1-5 down","6+ down"))~paste("Quarter",period)) +
  labs(y = "Counts",
       x = "Period Minute (counting up)",
       fill = "Period Type",
       title = "Number of subs by score margin",
       subtitle = "Season 2020, in-quarter subs only") +
  scale_fill_manual(values = c("red","grey60")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))