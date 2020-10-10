library(tidyverse)
library(superNetballR)
library(tidytext)
library(RColorBrewer)

## Load Season 2020 Data
Season_id <- "11108"
game_register <- list(roundNumber = rep(1:14,each = 4), matchNumber = rep(1:4,times = 14))
game_register <- pmap(game_register, function(roundNumber,matchNumber) downloadMatch(Season_id,roundNumber,matchNumber))

## Tidy match stats
match_stats <- game_register %>% map_df(tidyMatch) %>%
  left_join(
    tibble(homeTeam = "Home",
           squadId = map_dbl(game_register,~.x[["matchInfo"]][["homeSquadId"]]),
           matchNumber = map_dbl(game_register,~.x[["matchInfo"]][["matchNumber"]]),
           roundNumber = map_dbl(game_register,~.x[["matchInfo"]][["roundNumber"]])),
    by = c("squadId" = "squadId",
           "game" = "matchNumber",
           "round" = "roundNumber")) %>% 
  replace_na(list(homeTeam = "Away"))

## Collect player subs by match
player_subs <- tibble(Round = map_dbl(game_register,~.x$matchInfo$roundNumber),
                     Match = map_dbl(game_register,~.x$matchInfo$matchNumber),
                     Subs = map(game_register,~.x$playerSubs)) %>% 
  unnest_wider(Subs) %>% unnest_longer(player) %>% unnest_wider(player) %>%
  mutate_at(vars(fromPos,toPos),fct_relevel, c("GS","GA","WA","C","WD","GD","GK")) %>% 
  mutate_at(vars(squadId, playerId),as.character) %>% 
  mutate(sub_combined = paste(fromPos,toPos,sep = "-"),
         scorepoints = NA) %>% 
  select(Round, Match,period,periodSeconds,squadId,sub_combined)

## Collect score flows from each match
score_flow <- tibble(Round = map_dbl(game_register,~.x$matchInfo$roundNumber),
                         Match = map_dbl(game_register,~.x$matchInfo$matchNumber),
                         Scores = map(game_register,~.x$scoreFlow)) %>% 
  unnest_wider(Scores) %>% unnest_longer(score) %>% unnest_wider(score) %>% 
  filter(scoreName %in% c("goal","2pt Goal")) %>%
  mutate(squadId = as.character(squadId),
         sub_combined = NA) %>%
  select(Round, Match,period,periodSeconds,squadId,scorepoints)

## Create table of home and away teams

home_away <- match_stats %>% 
  distinct(round,game,squadId,squadNickname,homeTeam) %>% 
  transmute(Round = round,
            Match = game,
            squadId = as.character(squadId),
            squadNickname,homeTeam)

subs_with_scores <- bind_rows(score_flow,player_subs) %>%
    left_join(home_away,by = c("Round", "Match", "squadId")) %>% 
    arrange(Round,Match,period,periodSeconds) %>% 
    group_by(Round,Match) %>% 
    mutate(score_difference = if_else(homeTeam == "Home",scorepoints,-scorepoints),
           score_difference = replace_na(score_difference,0),
           score_difference = cumsum(score_difference),
           score_difference = if_else(homeTeam == "Home",score_difference,-score_difference),
           score_margin = case_when(
             between(score_difference,6,100) ~ "6+ up", 
             between(score_difference,1,5) ~ "1-5 up", 
           score_difference == 0 ~ "Even",
             between(score_difference,-5,-1) ~ "1-5 down", 
             between(score_difference,-100,-6) ~ "6+ down"),
           score_margin = factor(score_margin,
                                 levels = c("6+ up","1-5 up","Even","1-5 down","6+ down"),
                                 ordered = T),
           score_difference = if_else(homeTeam == "Home",score_difference,-score_difference)) %>% 
  ungroup()

# Organise the subs into a consistent format

# Helper function to sort individual substitution combinations
reorder_subs <- function(x) {
  x = unique(x)
  x = factor(x,level = c("GS","GA","WA","C","WD","GD","GK","S"),ordered = T)
  x = sort(x)
  x = droplevels(x)
  return (paste0(x,collapse = "-"))
}

sub_transitions <- subs_with_scores %>% 
  filter(!is.na(sub_combined)) %>% 
  distinct(Round,Match,period,periodSeconds,squadId) %>%
  ungroup() %>% 
  mutate(id = 1:n()) %>% 
  right_join(filter(subs_with_scores, !is.na(sub_combined))) %>%
  group_by(id) %>% 
  mutate(id,subs = paste0(sub_combined,collapse = "-")) %>% 
  distinct(id,subs,.keep_all = TRUE) %>% 
  ungroup() %>% 
  mutate(subs = str_split(subs,"-"),
         subs = mapply(reorder_subs,subs),
         court_pos = case_when(
           str_detect(subs,"GA|GS") ~ "Shooters",
           str_count(subs,"-") > 1 ~ "Multiple",
           str_detect(subs,"WA|C|WD") ~ "Mid court",
           str_detect(subs,"GD|GK") ~ "Defenders",
           TRUE ~ "Other"))


# Plots -------------------------------------------------------------------

# Plot 1 - Sub count over time with respect to margin and quarter

subs_with_scores %>%
  filter(periodSeconds > 0,!is.na(sub_combined)) %>%
  distinct(Round,Match,period,periodSeconds,squadId,.keep_all = TRUE) %>%
  mutate(period_minute = round(periodSeconds/60),
         Super = if_else(period_minute  >= 10,"Power 5","Regulation")) %>% 
  count(period,period_minute,Super,score_margin) %>%
  ggplot(aes(x = period_minute,y = n,fill = Super)) +
  geom_col() +
  facet_grid(score_margin~paste("Quarter",period)) +
  labs(y = "Counts",
       x = "Period Minute",
       fill = "Period Type",
       title = "Number of subs by score margin",
       subtitle = "Season 2020, in-quarter subs only") +
  scale_fill_manual(values = c("red","grey60")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))


# Plot 2 - Sub count over time with respect to margin and quarter, broken down by sub type

mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(12)

sub_transitions %>% 
  filter(periodSeconds > 0,!is.na(sub_combined),squadNickname == "Vixens") %>%
  distinct(Round,Match,period,periodSeconds,squadId,.keep_all = TRUE) %>% 
  mutate(period_minute = round(periodSeconds/60),
         Super = if_else(period_minute  >= 10,"Power 5","Regulation"),
         subs = fct_lump(factor(subs),8)) %>%
  add_count(subs,name = "num_subs") %>%
  count(period,period_minute,subs,score_margin,num_subs) %>%
  ggplot(aes(x = period_minute,y = n,fill = fct_reorder(subs,desc(num_subs)))) +
  geom_col() +
  facet_grid(score_margin~paste("Quarter",period)) +
  labs(y = "Counts",
       x = "Period Minute",
       fill = "Substitution Type",
       title = "Number of subs by score margin, broken down by sub type",
       subtitle = "Season 2020, Melbourne Vixens") +
  scale_fill_manual(values = mycolors) +
  scale_y_continuous(breaks = 0:3) +
  expand_limits(y = c(0,3)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5,
        colour = "#27AD81FF",
        face = "bold"))

# Plot 3 - Sub count with respect to margin and quarter, broken down by sub type

sub_transitions %>% 
  filter(periodSeconds > 0,!is.na(sub_combined)) %>%
  distinct(Round,Match,period,periodSeconds,squadId,.keep_all = TRUE) %>% 
  mutate(period_minute = round(periodSeconds/60),
         Super = if_else(period_minute  >= 10,"Power 5","Regulation"),
         subs = fct_lump(factor(subs),8)) %>%
  count(period,score_margin,subs) %>% 
  mutate(subs_reordered = reorder_within(subs,n,within = list(period,score_margin)),
         court_pos = case_when(
           str_detect(subs,"GA|GS") ~ "Shooters",
           str_detect(subs,"WA|C|WD") ~ "Mid court",
           str_detect(subs,"GD|GK") ~ "Defenders",
           TRUE ~ "Other")) %>%
  ungroup() %>% 
  ggplot(aes(subs_reordered,n,fill = fct_relevel(court_pos,c("Shooters","Mid court","Defenders","Other")))) +
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(score_margin~paste("Quarter",period),scales = "free_y",ncol = 4) +
  labs(y = "Counts",
       x = "Substitution type",
       fill = "Position Type",
       title = "Total subs in quarter, by court position",
       subtitle = "Season 2020, in-quarter subs only") +
  scale_fill_manual(values = c("Shooters" = "#29BF12","Mid court" = "#574AE2","Defenders" = "#DE1A1A","Other" = "grey60")) +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
