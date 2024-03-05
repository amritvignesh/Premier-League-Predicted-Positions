library(worldfootballR)
library(dplyr)
library(fuzzyjoin)
library(randomForest)
library(vip)
library(caret)
library(ggtext)
library(tidyverse)

total_stats <- data.frame()

for(year in 2018:2024) {
  norm_stats <- fb_big5_advanced_season_stats(season_end_year = year, stat_type = "standard", team_or_player = "player")
  pos_stats <- fb_big5_advanced_season_stats(season_end_year = year, stat_type = "possession", team_or_player = "player")
  shoot_stats <- fb_big5_advanced_season_stats(season_end_year = year, stat_type = "shooting", team_or_player = "player")
  pass_stats <- fb_big5_advanced_season_stats(season_end_year = year, stat_type = "passing", team_or_player = "player")
  def_stats <- fb_big5_advanced_season_stats(season_end_year = year, stat_type = "defense", team_or_player = "player")
  gca_stats <- fb_big5_advanced_season_stats(season_end_year = year, stat_type = "gca", team_or_player = "player")
  
  stats <- inner_join(norm_stats, shoot_stats, by = c("Player", "Squad"))
  stats <- inner_join(stats, pass_stats, by = c("Player", "Squad"))
  stats <- inner_join(stats, pos_stats, by = c("Player", "Squad"))
  stats <- inner_join(stats, def_stats, by = c("Player", "Squad"))
  stats <- inner_join(stats, gca_stats, by = c("Player", "Squad"))  
  
  stats <- stats %>%
    group_by(Squad) %>%
    arrange(-MP_Playing) %>%
    filter(Pos.x != "GK", Comp.x == "Premier League", !is.na(SoT_percent_Standard)) %>%
    slice_head(n = 10) %>%
    ungroup() %>%
    mutate(season = year) %>%
    select(season, team = Squad, comp = Comp.x, Url = Url.x, player = Player, pos = Pos.x, goals = Gls_Standard, npxg = npxG_Expected.x, progdist = PrgDist_Total, cmp = Cmp_Total, ast = Ast.x, xag = xAG, key = KP, finalthird = Final_Third, progpass = PrgP, touches = Touches_Touches, attake = Att_Take, succtake = Succ_percent_Take, progdistcarr = PrgDist_Carries, progcarr = PrgC_Carries, finalthirdcarr = Final_Third_Carries, progrec = PrgR_Receiving, tkls = Tkl_Tackles, attchal = Att_Challenges, blks = Blocks_Blocks, int = Int, sca90 = SCA90_SCA, gca90 = GCA90_GCA)
  
  stats[is.na(stats)] <- 0
  
  league_stats <- fb_season_team_stats("ENG", "M", year, "1st", "league_table") %>%
    select(team = Squad, team_pts = Pts)
  
  stats <- left_join(stats, league_stats, by = "team")
  
  total_stats <- rbind(total_stats, stats)
}

total_stats <- total_stats %>% select(-pos, -team_pts)

total_report <- data.frame()

for (year in 2017:2023) {
  report <- tm_player_market_values(start_year = year, league_url = "https://www.transfermarkt.us/premier-league/startseite/wettbewerb/GB1") %>% mutate(season = season_start_year + 1) %>% select(season, squad, name = player_name, pos = player_position)
  total_report <- rbind(total_report, report)
}

final_stats <- data.frame()
failed <- data.frame()

total_report$squad[which(total_report$squad == "Wolverhampton Wanderers")] <- "Wolves"
total_report$squad[which(total_report$squad == "Brighton & Hove Albion")] <- "Brighton"

total_report$name[which(total_report$name == "Zanka")] <- "Mathias Jørgensen"
total_report$name[which(total_report$name == "Jose Cholevas")] <- "José Holebas"
total_report$name[which(total_report$name == "Chicharito")] <- "Javier Hernández"
total_report$name[which(total_report$name == "Jean Michaël Seri")] <- "Jean Seri"
total_report$name[which(total_report$name == "Mahmoud Trezeguet")] <- "Trézéguet"
total_report$name[which(total_report$name == "Gabriel Magalhães")] <- "Gabriel Dos Santos"
total_report$name[which(total_report$name == "Emerson Royal")] <- "Emerson"
total_report$name[which(total_report$name == "Trincão")] <- "Francisco Trincão"
total_report$name[which(total_report$name == "Tino Livramento")] <- "Valentino Livramento"

total_report <- rbind(total_report, c(2024, "Nottingham Forest", "Orel Mangala", "Central Midfield"))

for (yr in 2018:2024) {
  filt_stats <- total_stats %>% filter(season == yr) 
  filt_report <- total_report %>% filter(season == yr)
  final_stats_szn <- stringdist_left_join(filt_stats, filt_report, by = c("team"="squad", "player"="name"), method = "jw", max_dist = 1, distance_col = 'dist') %>% group_by(team, player) %>% filter(player.dist == min(player.dist), team.dist == min(team.dist))
  final_stats <- rbind(final_stats, final_stats_szn)
  failed_szn <- anti_join(filt_stats, final_stats_szn, by = c("team", "player")) 
  failed <- rbind(failed, failed_szn)
}

final_stats <- final_stats[,c(1:27,31)] %>% rename(season = season.x)

stats_train <- final_stats %>% filter(season <= 2022)
stats_test <- final_stats %>% filter(season >= 2023)

stats_train$pos <- as.factor(stats_train$pos)
stats_test$pos <- as.factor(stats_test$pos)

model_stats_train <- stats_train[,c(6:28)]
model_stats_test <- stats_test[,c(6:28)]

position_rf <- randomForest(pos ~ ., data = model_stats_train)

vip(position_rf)
vi(position_rf)

predictions <- predict(position_rf, newdata = stats_test)
conf_matrix <- confusionMatrix(predictions, stats_test$pos)

comparison <- data.frame(
  season = stats_test$season,
  player = stats_test$player,
  actual_pos = stats_test$pos,
  pred_pos = predictions,
  stringsAsFactors = FALSE
)


pos_abbrs <- c("AM", "CM", "CB", "CF", "DM", "LB", "LM", "LW", "RB", "RM", "RW", "SS")

conf_matrix <- as.data.frame(conf_matrix$table)

abbr_df <- cbind(full = sort(unique(conf_matrix$Prediction)), data.frame(pos_abbrs))

conf_matrix <- conf_matrix %>%
  left_join(abbr_df, by = c("Prediction" = "full")) %>%
  rename(pred = pos_abbrs) %>%
  left_join(abbr_df, by = c("Reference" = "full")) %>%
  rename(act = pos_abbrs)

grid <- ggplot(conf_matrix, aes(pred,act, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="red") +
  labs(x = "Predicted Position", y = "Actual Position", title = "Predicted vs Actual Position in the Premier League (2022/23 & 2023/24)", caption = "Data from **worldfootballR** | Amrit Vignesh | **@avsportsanalyst**") +
  theme_bw() +
  theme(plot.title = element_markdown(size=14, hjust=0.5, face="bold"), axis.title = element_text(face="bold"), plot.caption = element_markdown(hjust = 0.5))

ggsave("grid.png", grid, width = 10, height = 5)

comparison <- comparison %>%
  left_join(abbr_df, by = c("pred_pos" = "full")) %>%
  rename(pred = pos_abbrs) %>%
  left_join(abbr_df, by = c("actual_pos" = "full")) %>%
  rename(act = pos_abbrs)

write_csv(comparison, "comparison.csv")

forwards <- c("CF", "LW", "RW", "SS")
mids <- c("AM", "CM", "DM", "LM", "RM")
defense <- c("LB", "CB", "RB")

forward_pred_defense <- comparison %>% filter(act %in% forwards, pred %in% defense)
defense_pred_forward <- comparison %>% filter(act %in% defense, pred %in% forwards)
