library(tidyverse)
library(superNetballR)
library(gt)

## Load Season 2020 Data
Season_id <- "11108"
game_register <- list(roundNumber = rep(1:14,each = 4), matchNumber = rep(1:4,times = 14))
game_register <- pmap(game_register, function(roundNumber,matchNumber) downloadMatch(Season_id,roundNumber,matchNumber))

squad_reg <- game_register %>% map_df(tidyMatch) %>%
  left_join(
    tibble(homeTeam = "Home",
           squadId = map_dbl(game_register,~.x[["matchInfo"]][["homeSquadId"]]),
           matchNumber = map_dbl(game_register,~.x[["matchInfo"]][["matchNumber"]]),
           roundNumber = map_dbl(game_register,~.x[["matchInfo"]][["roundNumber"]])),
    by = c("squadId" = "squadId",
           "game" = "matchNumber",
           "round" = "roundNumber")) %>% 
  replace_na(list(homeTeam = "Away")) %>% 
  distinct(squadId,squadNickname) %>%
  ungroup() %>% 
  mutate_at(vars(squadId),as.character)

## Collect player subs by match

reorder_subs <- function(x) {
  x = unique(x)
  x = factor(x,level = c("GS","GA","WA","C","WD","GD","GK","S"),ordered = T)
  x = sort(x)
  x = droplevels(x)
  return (paste0(x,collapse = "-"))
}

player_subs <- tibble(Round = map_dbl(game_register,~.x$matchInfo$roundNumber),
                      Match = map_dbl(game_register,~.x$matchInfo$matchNumber),
                      Subs = map(game_register,~.x$playerSubs)) %>% 
  unnest_wider(Subs) %>% unnest_longer(player) %>% unnest_wider(player) %>%
  mutate_at(vars(fromPos,toPos),fct_relevel, c("GS","GA","WA","C","WD","GD","GK")) %>% 
  mutate_at(vars(squadId, playerId),as.character) %>% 
  mutate(sub_combined = paste(fromPos,toPos,sep = "-"),
         scorepoints = NA,
         type = "subs") %>%
  group_by(Round,Match,period,periodSeconds,squadId) %>% 
  mutate(subs = paste0(sub_combined,collapse = "-"),
         subs = str_split(subs,"-"),
         subs = mapply(reorder_subs,subs)) %>%
  distinct(Round,Match,period,periodSeconds,squadId,subs,.keep_all = TRUE) %>%
  ungroup() %>%
    mutate(id = 1:n()) %>% 
  select(Round, Match,period,periodSeconds,squadId,subs,type,id)

## Collect score flows from each match
score_flow <- tibble(Round = map_dbl(game_register,~.x$matchInfo$roundNumber),
                     Match = map_dbl(game_register,~.x$matchInfo$matchNumber),
                     Scores = map(game_register,~.x$scoreFlow)) %>% 
  unnest_wider(Scores) %>% unnest_longer(score) %>% unnest_wider(score) %>% 
  filter(scoreName %in% c("goal","2pt Goal")) %>%
  mutate(squadId = as.character(squadId),
         sub_combined = NA,
         type = "score",
         id = NA) %>%
  select(Round, Match,period,periodSeconds,squadId,scorepoints,type)

next_2 <- bind_rows(score_flow,player_subs) %>%
  filter(periodSeconds > 0) %>% 
  arrange(Round,Match,period,periodSeconds) %>%
  fill(id,.direction = "down") %>%
  group_by(id) %>%
  slice_head(n = 3) %>%
  add_count(id,name = "goals_after_sub") %>%
  mutate(sub_team = if_else(type == "subs",squadId,NA_character_)) %>% 
  fill(sub_team,.direction = "down") %>%
  fill(subs,.direction = "down") %>% 
  mutate(alt_score = if_else(squadId == sub_team,1,-1))
  
  next_2 %>% 
  filter(type == "score",goals_after_sub == 3,!is.na(id)) %>%
  summarise(`Goals after sub` = sum(alt_score)) %>%
  ungroup() %>% count(`Goals after sub`,name = "occurances") %>% 
    gt()
  
  next_2 %>%
    filter(id %in% sample(1:812,4)) %>%
    left_join(squad_reg) %>% 
    mutate(subs = if_else(type == "subs",subs,"-")) %>% 
    ungroup() %>% 
    select(Round,Match,period,periodSeconds,squadNickname,type,scorepoints,subs,alt_score) %>% 
    gt() %>% 
    tab_style(
      style = list(
        cell_fill(color = "grey90")
      ),
      locations = cells_body(
        rows = type == "score")
    ) %>% 
    tab_style(
      style = list(
        cell_fill(color = "grey60")
      ),
      locations = cells_body(
        rows = type == "subs")
    ) %>% 
    tab_style(
      style = list(
        cell_text(color = "red")
      ),
      locations = cells_body(
        columns = vars(alt_score),
        rows = alt_score < 0)
    )
  

  
  