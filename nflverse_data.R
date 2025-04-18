library(nflreadr);library(tidyverse);library(httr);library(jsonlite)
library(dplyr);library(googledrive);library(googlesheets4)
library(readr); library(openxlsx)

########## Reading in csv from google drive ##########
# Selecting and authenticating the google drive account you want to pull the excel file from
drive_auth(use_oob = TRUE)

# reading in excel file from 
file <- drive_get("ff_data.xlsx")

temp_file <- tempfile(fileext = ".xlsx")

# downloading the file
drive_download(file, path = temp_file, overwrite = TRUE)

# Load the Excel file into R
wb <- loadWorkbook(temp_file)  # Open the workbook

# Read each sheet into a data frame
season_stats_all <- read.xlsx(wb, sheet = "Season")
weekly_stats_all <- read.xlsx(wb, sheet = "Weekly")


##### removing all rows from current season so season metrics can be recalculated given this runs every week #####
cur_season <- 2024 # This needs to be updated at the start of each season

##### Uncomment lines 30 & 31 out when running on a weekly basis#####

season_stats_all <- season_stats_all %>% filter(season != cur_season) 
weekly_stats_all <- weekly_stats_all %>% filter(season != cur_season) 



nflreadr::.clear_cache()

##### Run lines 39-42 if running the script for the first time #####
# loading in play by play data from 2021-2024
#pbp2024 <- load_pbp(2024)
#pbp2023 <- load_pbp(2023)
#pbp2022 <- load_pbp(2022)
#pbp2021 <- load_pbp(2021)


##### Run this line when running on a weekly basis (gets data from only this season) #####
pbp_cur_season <- load_pbp(cur_season)

# loading in fantasy football player ids (should make it easier to connect data)
ff_ids <- load_ff_playerids()

ff_ids <- ff_ids %>% 
  select(gsis_id, espn_id, name, age, height, weight, college)



# uncomment this out if running script for the first time
#depth_charts <- load_depth_charts(seasons =  c(2021, 2022, 2023, 2024)) %>%
#  filter(depth_position %in% c("QB", "RB", "TE", "WR")) %>%
#  mutate(depth_pos = paste(depth_position, depth_team, sep = "-")) %>%
#  select(depth_pos, season, week, gsis_id)

depth_charts <- load_depth_charts(seasons =  cur_season) %>%
  filter(depth_position %in% c("QB", "RB", "TE", "WR")) %>%
  mutate(depth_pos = paste(depth_position, depth_team, sep = "-")) %>%
  select(depth_pos, season, week, gsis_id)

# uncomment this if  running script for the first time
#snap_counts <- load_snap_counts(seasons =  c(2021, 2022, 2023, 2024)) %>%
#  filter(position %in% c("QB", "RB", "TE", "WR")) %>%
#  select(season, week, player, team, position, offense_snaps, offense_pct, opponent)


snap_counts <- load_snap_counts(seasons =  cur_season) %>%
  filter(position %in% c("QB", "RB", "TE", "WR")) %>%
  select(season, week, player, team, position, offense_snaps, offense_pct, opponent)


##############################################################

########## Weekly data gathering/calculations ##########

##### Offensive data #####

# uncomment this if running for the first time
#offense_weekly <- load_player_stats(seasons = c(2021,2022,2023,2024)) %>% filter(position %in% c("FB", "QB", "RB", "TE", "WR"))


offense_weekly <- load_player_stats(seasons = cur_season) %>% filter(position %in% c("FB", "QB", "RB", "TE", "WR"))


# getting half ppr calculations
offense_weekly <- offense_weekly %>%
  mutate(half_PPR = (passing_yards * 0.04) + (passing_tds * 4) + (rushing_yards * 0.1) + (rushing_tds * 6) + (receptions * 0.5) + (receiving_yards * 0.1) + (receiving_tds * 6) + ((rushing_fumbles_lost + sack_fumbles_lost + receiving_fumbles_lost + interceptions) * -2) + ((passing_2pt_conversions + rushing_2pt_conversions + receiving_2pt_conversions)*2)) 

offense_weekly <- offense_weekly %>%
  left_join(depth_charts, by = c("player_id" = "gsis_id", "week", "season"))

offense_weekly <- offense_weekly %>%
  left_join(snap_counts, by = c("season", "week", "position", "opponent_team" = "opponent", "player_display_name" = "player", "recent_team" = "team")) 


##### Kicking data #####

# uncomment this if running for the first time
#kicking <- load_player_stats(seasons = c(2021,2022,2023,2024), stat_type = "kicking")

kicking <- load_player_stats(seasons = cur_season, stat_type = "kicking")

# adding extra columns that are the same to help with lining up columns are all data
kicking_weekly <- kicking %>%
  mutate(fantasy_points = case_when(
    fg_att == 0 & pat_att == 0 ~ 0,
    TRUE ~ ((fg_made_0_19 + fg_made_20_29 + fg_made_30_39) * 3) + (fg_made_40_49 * 4) + (fg_made_50_59 * 5) + (fg_made_60_ * 6) + (pat_made) - fg_missed - fg_blocked - pat_missed - pat_blocked
  ),
  fantasy_points_ppr = fantasy_points,
  half_PPR = fantasy_points
  )

##### Defense data #####
# uncomment this if running script for the first time
defense_weekly <- load_player_stats(seasons = c(2021,2022, 2023, 2024), stat_type = "defense")

defense_weekly <- load_player_stats(seasons = cur_season, stat_type = "defense")

kicking_by_game <- kicking_weekly %>%
  mutate(kicking_points = case_when(
    (is.na(fg_made) & is.na(pat_made)) ~ 0,
    (is.na(fg_made) & !is.na(pat_made)) ~ pat_made,
    (!is.na(fg_made) & is.na(pat_made)) ~ (3*fg_made),
    TRUE ~ (fg_made * 3) + pat_made
  ),
  blocked_kicks = case_when(
    (is.na(fg_blocked) & is.na(pat_blocked)) ~ 0,
    (is.na(fg_blocked) & !is.na(pat_blocked)) ~ pat_blocked,
    (!is.na(fg_blocked) & is.na(pat_blocked)) ~ fg_blocked,
    TRUE ~ fg_blocked + pat_blocked)) %>%
  select(player_id, player_display_name, season, week, kicking_points, blocked_kicks)
  
# uncomment this if running for the first time
#depth_charts <- load_depth_charts(seasons = c(2021,2022,2023,2024)) %>%
#  filter(position == "K" & game_type != "SBBYE") %>%
#  select(season, club_code, week, gsis_id, full_name) %>%
#  distinct()


depth_charts <- load_depth_charts(seasons = c(2021,2022,2023,2024)) %>%
  filter(position == "K" & game_type != "SBBYE") %>%
  select(season, club_code, week, gsis_id, full_name) %>%
  distinct()


kicking <- kicking_by_game %>%
  left_join(depth_charts, by = c("player_id" = "gsis_id", "season" = "season", "week" = "week")) %>% 
  select(-full_name) %>%
  filter(kicking_points > 0 | blocked_kicks > 0)

# fixing team codes to allow for successful join
kicking <- kicking %>%
  mutate(
    club_code = case_when(
      (player_display_name == "Robbie Gould" & week %in% c(9,10) & season == 2021) ~ "SF",
      (player_display_name == "Brett Maher" & week %in% c(11,13) & season == 2021) ~ "NO",
      (player_display_name == "Cody Parkey" & week == 5 & season == 2021) ~ "WAS",
      (player_display_name == "Bradley Pinion" & week == 5 & season == 2021) ~ "TB",
      (player_display_name == "Ka'imi Fairbairn" & week == 17 & season == 2021) ~ "HOU",
      (player_display_name == "Aldrick Rosas" & week == 11 & season == 2021) ~ "DET",
      (player_display_name == "Harrison Butker" & week == 17 & season == 2021) ~ "KC",
      (player_display_name == "Zane Gonzalez" & week == 2 & season == 2021) ~ "CAR",
      (player_display_name == "Michael Badgley" & week == 6 & season == 2021) ~ "IND",
      (player_display_name == "Eddy Pineiro" & week == 14 & season == 2021) ~ "NYJ",
      (player_display_name == "Ryan Santoso" & week == 3 & season == 2021) ~ "DET",
      (player_display_name == "Chris Blewitt" & week == 7 & season == 2021) ~ "WAS",
      (player_display_name == "Elliot Fry" & week == 16 & season == 2021) ~ "PIT",
      (player_display_name == "Elliot Fry" & week == 18 & season == 2021) ~ "CLE",
      (player_display_name == "Mitch Wishnowsky" & week == 4 & season == 2021) ~ "SF",
      (player_display_name == "Austin Seibert" & week == 5 & season == 2021) ~ "DET",
      (player_display_name == "Matthew Wright" & week %in% c(4,5,6) & season == 2021) ~ "JAX",
      (player_display_name == "Joey Slye" & week == 5 & season == 2021) ~ "SF",
      (player_display_name == "Joey Slye" & week == 16 & season == 2021) ~ "WAS",
      (player_display_name == "Chase McLaughlin" & week == 17 & season == 2021) ~ "CLE",
      (player_display_name == "Lirim Hajrullahu" & week == 10 & season == 2021) ~ "DAL",
      (player_display_name == "Lirim Hajrullahu" & week == 16 & season == 2021) ~ "CAR",
      (player_display_name == "Dominik Eberle" & week == 16 & season == 2021) ~ "HOU",
      (player_display_name == "Randy Bullock" & week == 2 & season == 2021) ~ "TEN",
      (player_display_name == "Greg Zuerlein" & week == 11 & season == 2021) ~ "DAL",
      (player_display_name == "Chris Naggar" & week == 16 & season == 2021) ~ "CLE",
      (player_display_name == "Brian Johnson" & week %in% c(13,14) & season == 2021) ~ "WAS",
      (player_display_name == "Matt Prater" & week == 11 & season == 2022) ~ "ARI",
      (player_display_name == "Mason Crosby" & week %in% c(1,2) & season == 2022) ~ "GB",
      (player_display_name == "Chris Boswell" & week == 14 & season == 2022) ~ "PIT",
      (player_display_name == "Josh Lambo" & week == 11 & season == 2022) ~ "TEN",
      (player_display_name == "Taylor Bertolet" & week %in% c(5,7) & season == 2022) ~ "LAC",
      (player_display_name == "Michael Badgley" & week == 4 & season == 2022) ~ "CHI",
      (player_display_name == "Justin Reid" & week == 1 & season == 2022) ~ "KC",
      (player_display_name == "Tristan Vizcaino" & week == 10 & season == 2022) ~ "ARI",
      (player_display_name == "Mitch Wishnowsky" & week == 5 & season == 2022) ~ "SF",
      (player_display_name == "Matthew Wright" & week %in% c(4,5) & season == 2022) ~ "KC",
      (player_display_name == "Matthew Wright" & week %in% c(10,11,12) & season == 2022) ~ "PIT",
      (player_display_name == "Dominik Eberlie" & week == 4 & season == 2022) ~ "DET",
      (player_display_name == "Matt Ammendola" & week == 2 & season == 2022) ~ "KC",
      (player_display_name == "Matt Ammendola" & week %in% c(5,6) & season == 2022) ~ "ARI",
      (player_display_name == "Cameron Dicker" & week == 5 & season == 2022) ~ "PHI",
      (player_display_name == "Cameron Dicker" & week %in% c(9,10,11,12) & season == 2022) ~ "LAC",
      (player_display_name == "Caleb Shudak" & week == 12 & season == 2022) ~ "TEN",
      (player_display_name == "Nick Sciba" & week == 8 & season == 2022) ~ "PIT",
      (player_display_name == "Mason Crosby" & week == 16 & season == 2023) ~ "NYG",
      (player_display_name == "Randy Bullock" & week %in% c(10,11,12) & season == 2023) ~ "NYG",
      (player_display_name == "Ka'imi Fairbairn" & week %in% c(15,16,17,18) & season == 2023) ~ "HOU",
      (player_display_name == "Dare Ogunbowale" & week == 9 & season == 2023) ~ "HOU",
      (player_display_name == "Mike Badgley" & week %in% c(15,16) & season == 2023) ~ "DET",
      (player_display_name == "Jamie Gillan" & week == 15 & season == 2023) ~ "NYG",
      (player_display_name == "Austin Seibert" & week == 2 & season == 2023) ~ "NYJ",
      (player_display_name == "Matt Ammendola" & week %in% c(10,11,12,13) & season == 2023) ~ "HOU",
      (player_display_name == "Riley Patterson" & week %in% c(17,18,19) & season == 2023) ~ "CLE",
      (player_display_name == "Cameron Dicker" & week %in% c(1,2) & season == 2023) ~ "LAC",
      (player_display_name == "Lucas Havrisik" & week %in% c(8,9) & season == 2023) ~ "LA",
      (player_display_name == "Graham Gano" & week == 10 & season == 2024) ~ "NYG",
      (player_display_name == "Brandon McManus" & week == 7 & season == 2024) ~ "GB",
      (player_display_name == "Harrison Butker" & week %in% c(15,16) & season == 2024) ~ "KC",
      (player_display_name == "Zane Gonzalez" & week %in% c(10,13,16) & season == 2024) ~ "WAS",
      (player_display_name == "Greg Joseph" & week %in% c(3,4) & season == 2024) ~ "NYJ",
      (player_display_name == "Mitch Wishnowsky" & week == 5 & season == 2024) ~ "SF",
      (player_display_name == "Austin Seibert" & week == 2 & season == 2024) ~ "WAS",
      (player_display_name == "Matthew Wright" & week == 6 & season == 2024) ~ "SF",
      (player_display_name == "Matthew Wright" & week == 13 & season == 2024) ~ "KC",
      (player_display_name == "Matthew Wright" & week %in% c(17,18) & season == 2024) ~ "TEN",
      (player_display_name == "Riley Patterson" & week == 9 & season == 2024) ~ "NYG",
      (player_display_name == "Riley Patterson" & week == 15 & season == 2024) ~ "CLE",
      (player_display_name == "Riley Patterson" & week %in% c(16,17) & season == 2024) ~ "ATL",
      (player_display_name == "John Parker Romo" & week %in% c(11,12,13) & season == 2024) ~ "MIN",
      (player_display_name == "Cade York" & week %in% c(14,15,16,17,18) & season == 2024) ~ "CIN",
      (player_display_name == "Adners Carlson" & week == 11 & season == 2024) ~ "NYJ",
      (player_display_name == "Anders Carlson" & week %in% c(7,8) & season == 2024) ~ "SF",
      (player_display_name == "Chad Ryland" & week %in% c(5,6,7) & season == 2024) ~ "ARI",
      (player_display_name == "Will Reichard" & week == 14 & season == 2024) ~ "MIN",
      (player_display_name == "Spencer Shrader" & week == 10 & season == 2024) ~ "NYJ",
      (player_display_name == "Spencer Shrader" & week %in% c(11,12) & season == 2024) ~ "KC",
      (player_display_name == "Brayden Narveson" & week == 16 & season == 2024) ~ "TEN",
      (player_display_name == "Jude McAtamney" & week == 9 & season == 2024) ~ "NYG",
      TRUE ~ club_code
    )
)

yards_tds <- offense_weekly %>%
  group_by(season, week, recent_team) %>%
  summarize(
    opponent = first(opponent_team),
    total_yards = sum(passing_yards + rushing_yards, na.rm = TRUE),
    total_tds = sum(passing_tds + rushing_tds, na.rm = TRUE),
    interceptions = sum(interceptions, na.rm = TRUE),
    fumbles_recovered = sum(sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost, na.rm = TRUE)
  )

all_defense_weekly <- yards_tds %>% 
  left_join(kicking, by = c("season", "week", "opponent" = "club_code"))

defense_weekly <- defense_weekly %>%
  group_by(season, week, team) %>% 
  summarize(df_tds = sum(def_tds, na.rm = TRUE),
            sacks = sum(def_sacks, na.rm = TRUE))

all_defense_weekly <- all_defense_weekly %>%
  select(-c(recent_team, player_id, player_display_name)) %>%
  rename("team" = "opponent") %>%
  left_join(defense_weekly, by = c("season", "week", "team"))


all_defense_weekly <- all_defense_weekly %>%
  mutate(total_points_allowed = (total_tds * 6) + kicking_points,
         yards_points = case_when(
           total_yards < 100 ~ 5,
           total_yards >= 100 & total_yards < 200 ~ 3,
           total_yards >= 200 & total_yards < 300 ~ 2,
           total_yards >= 300 & total_yards < 400 ~ -1,
           total_yards >= 400 & total_yards < 450 ~ -3,
           total_yards >= 450 & total_yards < 500 ~ -5,
           total_yards >= 500 & total_yards < 560 ~ -6,
           total_yards >= 550 ~ -7
         ),
         points_fp = case_when(
           total_points_allowed == 0 ~ 5,
           total_points_allowed > 0 & total_points_allowed < 7 ~ 4,
           total_points_allowed >= 7 & total_points_allowed < 14 ~ 3,
           total_points_allowed >= 14 & total_points_allowed < 18 ~ 1,
           total_points_allowed >= 18 & total_points_allowed < 28 ~ 0,
           total_points_allowed >= 28 & total_points_allowed < 35 ~ -1,
           total_points_allowed >= 35 & total_points_allowed < 46 ~ -3,
           total_points_allowed >= 46 ~ -5,
         ))

### uncomment this section (lines 297-317) if running script the first time
# pbp2024_tds <- pbp2024 %>%
#   filter(return_touchdown == 1 & play_type %in% c("kickoff", "punt")) %>%
#   select(season, week, td_team)
# 
# pbp2023_tds <- pbp2023 %>%
#   filter(return_touchdown == 1 & play_type %in% c("kickoff", "punt")) %>%
#   select(season, week, td_team)
# 
# pbp2022_tds <- pbp2022 %>%
#   filter(return_touchdown == 1 & play_type %in% c("kickoff", "punt")) %>%
#   select(season, week, td_team)
# 
# pbp2021_tds <- pbp2021 %>%
#   filter(return_touchdown == 1 & play_type %in% c("kickoff", "punt")) %>%
#   select(season, week, td_team)
# 
# pbp_tds <- rbind(pbp2024_tds, pbp2023_tds, pbp2022_tds, pbp2021_tds)
# 
# pbp_tds <- pbp_tds %>%
#   group_by(season, week, td_team) %>%
#   summarize(st_tds = n())

pbp_cur_season_tds <- pbp_cur_season %>%
  filter(return_touchdown == 1 & play_type %in% c("kickoff", "punt")) %>%
  select(season, week, td_team)

pbp_tds <- pbp_cur_season_tds %>%
  group_by(season, week, td_team) %>%
  summarize(st_tds = n())


all_defense_weekly <- all_defense_weekly %>%
  left_join(pbp_tds, by = c("season", "week", "team" = "td_team"))

all_defense_weekly[is.na(all_defense_weekly)] <- 0

  
defense_weekly <- all_defense_weekly %>%
  mutate(fantasy_points = yards_points + points_fp + (2*interceptions) + (2*fumbles_recovered) + (2*blocked_kicks)+ (6*df_tds) + sacks + (6*st_tds))

########## Creating season totals ##########
defense_season <- defense_weekly %>%
  group_by(season, team) %>%
  summarise(
    total_yards = sum(total_yards, na.rm = TRUE),
    avg_ypg_allowed = round(sum(total_yards, na.rm = TRUE)/n(), 2),
    total_points = sum(total_points_allowed, na.rm = TRUE),
    avg_ppg_allowed = round(sum(total_points_allowed, na.rm = TRUE)/n(), 2),
    total_int = sum(interceptions, na.rm = TRUE),
    int_pg = round(sum(interceptions, na.rm = TRUE)/n(), 2),
    total_fumbles = sum(fumbles_recovered, na.rm = TRUE),
    fumbles_pg = round(sum(fumbles_recovered, na.rm = TRUE)/n(), 2),
    total_blocked_kicks = sum(blocked_kicks, na.rm = TRUE),
    blocked_kicks_pg = round(sum(blocked_kicks, na.rm = TRUE)/n(), 2),
    total_def_tds = sum(df_tds, na.rm = TRUE),
    df_tds_pg = round(sum(df_tds, na.rm = TRUE)/n(), 2),
    total_sacks = sum(sacks, na.rm = TRUE),
    sacks_pg = round(sum(sacks, na.rm = TRUE)/n(), 2),
    total_st_tds = sum(st_tds, na.rm = TRUE),
    st_tds_pg = round(sum(st_tds, na.rm = TRUE)/n(), 2),
    total_fp = sum(fantasy_points, na.rm = TRUE),
    fp_pg = round(sum(fantasy_points, na.rm = TRUE)/n(), 2)
  )

opponent_defense <- defense_season %>%
  select(season, team, avg_ypg_allowed, avg_ppg_allowed, int_pg, fumbles_pg, df_tds_pg, sacks_pg) %>%
  rename_with(~ paste0("opp_", .), -c(season, team))

offense_weekly <- offense_weekly %>%
  left_join(opponent_defense, by = c("opponent_team" = "team", "season"))

##### Offense data #####

offense_season <- offense_weekly %>%
  group_by(season, player_id, player_display_name) %>%
  summarise(
    team = paste(unique(recent_team), collapse = ", "),
    position = first(position),
    season = season,
    games_played = n(),
    completions = sum(completions, na.rm = TRUE),
    attempts = sum(attempts, na.rm = TRUE),
    `comp%` = round((completions/attempts)*100,2),
    passing_yards = sum(passing_yards, na.rm = TRUE),
    yards_per_attempt = round(sum(passing_air_yards, na.rm = TRUE)/attempts,2),
    carries = sum(carries, na.rm = TRUE),
    carries_per_game = round(carries/games_played,2),
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    targets = sum(targets, na.rm = TRUE),
    receptions = sum(receptions, na.rm = TRUE),
    receiving_yards = sum(receiving_yards, na.rm = TRUE),
    YAC = sum(receiving_yards_after_catch, na.rm = TRUE),
    avg_YAC = round(YAC/receptions, 2),
    `target_share%` = round(mean(target_share, na.rm = TRUE)*100,2),
    passing_ypg = round(passing_yards/games_played,2),
    rushing_ypg = round(rushing_yards/games_played,2),
    receiving_ypg = round(receiving_yards/games_played,2),
    total_TDs = sum(passing_tds + rushing_tds + receiving_tds, na.rm = TRUE),
    passing_TDs = sum(passing_tds, na.rm = TRUE),
    rushing_TDs = sum(rushing_tds, na.rm = TRUE),
    receiving_TDs = sum(receiving_tds, na.rm = TRUE),
    ints = sum(interceptions, na.rm = TRUE),
    sacks = sum(sacks, na.rm = TRUE),
    sacks_per_game = round(sum(sacks)/games_played,2),
    fumbles = sum(sack_fumbles + rushing_fumbles + receiving_fumbles, na.rm = TRUE),
    fumbles_lost = sum(sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost, na.rm = TRUE),
    avg_pacr = round(mean(pacr, na.rm = TRUE),4),
    avg_dakota = round(mean(dakota, na.rm = TRUE),4),
    avg_wopr = round(mean(wopr, na.rm = TRUE),4),
    avg_racr = round(mean(racr, na.rm = TRUE),4),
    standard_points = sum(fantasy_points, na.rm = TRUE),
    standard_ppg = round(mean(fantasy_points, na.rm = TRUE),2),
    half_PPR_points = sum(half_PPR, na.rm = TRUE),
    half_PPR_ppg = round(mean(half_PPR_points, na.rm = TRUE), 2),
    PPR_points = sum(fantasy_points_ppr, na.rm = TRUE),
    PPR_ppg = round(mean(fantasy_points_ppr, na.rm = TRUE),2),
    avg_off_snaps = round(mean(offense_snaps, na.rm = TRUE), 2),
    avg_off_snap_pct = round(mean(offense_pct, na.rm = TRUE), 2)
  ) %>% 
  distinct()


##### Kicking data #####
kicking_season <- kicking_weekly %>%
  group_by(season, player_id, player_display_name) %>% 
  summarize(
    games_played = n(),
    position = first(position),
    team = paste(unique(team), collapse = ", "), 
    season = season,
    position = position,
    fg_made = sum(fg_made, na.rm = TRUE),
    fg_att = sum(fg_att, na.rm = TRUE),
    fg_pct = ifelse(fg_att == 0, NA, round((fg_made/fg_att) * 100)),
    fg_blocked = sum(fg_blocked, na.rm = TRUE),
    fg_long = ifelse(fg_made == 0, NA, max(fg_long, na.rm = TRUE)),
    fg_made_0_19 = sum(fg_made_0_19, na.rm = TRUE),
    fg_made_20_29 = sum(fg_made_20_29, na.rm = TRUE),
    fg_made_30_39 = sum(fg_made_30_39, na.rm = TRUE),
    fg_made_40_49 = sum(fg_made_40_49, na.rm = TRUE),
    fg_made_50_59 = sum(fg_made_50_59, na.rm = TRUE),
    `fg_made_60+` = sum(fg_made_60_, na.rm = TRUE),
    fg_missed_0_19 = sum(fg_missed_0_19, na.rm = TRUE),
    fg_missed_20_29 = sum(fg_missed_20_29, na.rm = TRUE),
    fg_missed_30_39 = sum(fg_missed_30_39, na.rm = TRUE),
    fg_missed_40_49 = sum(fg_missed_40_49, na.rm = TRUE),
    fg_missed_50_59 = sum(fg_missed_50_59, na.rm = TRUE),
    `fg_missed_60+` = sum(fg_missed_60_, na.rm = TRUE),
    pat_made = sum(pat_made, na.rm = TRUE),
    pat_att = sum(pat_att, na.rm = TRUE),
    pat_blocked = sum(pat_blocked, na.rm = TRUE),
    pat_pct = round((pat_made/pat_att)*100, 2),
    gwfg_made = sum(gwfg_made, na.rm = TRUE),
    total_kicking_points = ((fg_made * 3) + pat_made),
    standard_points = sum(fantasy_points, na.rm = TRUE),
    standard_ppg = ifelse(standard_points == 0, 0,round(mean(fantasy_points, na.rm = TRUE), 2)),
    half_PPR_points = sum(fantasy_points, na.rm = TRUE),
    half_PPR_ppg = ifelse(standard_points == 0, 0,round(mean(fantasy_points, na.rm = TRUE), 2)),
    PPR_points = sum(fantasy_points, na.rm = TRUE),
    PPR_ppg = ifelse(standard_points == 0, 0,round(mean(fantasy_points, na.rm = TRUE), 2))
  ) %>%
  distinct()


########## combining old and new data ##########

##### Standardizing column names #####
all_columns_weekly <- union(names(offense_weekly), union(names(kicking_weekly), names(defense_weekly)))
all_columns_season <- union(names(offense_season), union(names(kicking_season), names(defense_season)))


# Add missing columns with NA values to each dataset
offense_weekly[setdiff(all_columns_weekly, names(offense_weekly))] <- NA
kicking_weekly[setdiff(all_columns_weekly, names(kicking_weekly))] <- NA
defense_weekly[setdiff(all_columns_weekly, names(defense_weekly))] <- NA
offense_season[setdiff(all_columns_season, names(offense_season))] <- NA
kicking_season[setdiff(all_columns_season, names(kicking_season))] <- NA
defense_season[setdiff(all_columns_season, names(defense_season))] <- NA

# Ensure column order is the same
offense_weekly <- offense_weekly[all_columns_weekly]
kicking_weekly <- kicking_weekly[all_columns_weekly]
defense_weekly <- defense_weekly[all_columns_weekly]
offense_season <- offense_season[all_columns_season]
kicking_season <- kicking_season[all_columns_season]
defense_season <- defense_season[all_columns_season]



########## Joining with ff_ids ##########
cur_season_stats_all <- rbind(offense_season, kicking_season, defense_season) %>% 
  distinct()
cur_weekly_stats_all <- rbind(offense_weekly, kicking_weekly, defense_weekly) %>% 
  distinct()



# adding new row to ff_ids for defenses
new_row <- data.frame(
  gsis_id = "defense",
  espn_id = NA,
  name = NA,
  age = NA,
  height = NA, 
  weight = NA,
  college = NA
)

ff_ids <- bind_rows(ff_ids, new_row)

# adding "defense" to player id in season and weekly data for joining purposes
cur_season_stats_all$player_id[is.na(cur_season_stats_all$player_id)] <- "defense"
cur_weekly_stats_all$player_id[is.na(cur_weekly_stats_all$player_id)] <- "defense"




cur_season_stats_all <- cur_season_stats_all %>% 
  left_join(ff_ids, by = c("player_id" = "gsis_id"))

cur_weekly_stats_all <- cur_weekly_stats_all %>% 
  left_join(ff_ids, by = c("player_id" = "gsis_id"))



########## Adding espn_ids for defenses to allow for joining in other code ##########
cur_weekly_stats_all <- cur_weekly_stats_all %>%
  mutate(espn_id = case_when(
    (team == "DEN" & player_id == "defense")~ "-16007", 
    (team == "BUF" & player_id == "defense") ~ "-16002", 
    (team == "CHI" & player_id == "defense") ~ "-16003", 
    (team == "NYJ" & player_id == "defense")~ "-16020",
    (team == "MIA" & player_id == "defense")~ "-16006", 
    (team == "DAL" & player_id == "defense")~ "-16015",
    (team == "LAC" & player_id == "defense")~ "-16024",
    (team == "MIN" & player_id == "defense")~ "-16016",
    (team == "BAL" & player_id == "defense")~ "-16033", 
    (team == "KC" & player_id == "defense")~ "-16012",
    (team == "DET" & player_id == "defense")~ "-16008", 
    (team == "GB" & player_id == "defense")~ "-16009", 
    (team == "TB" & player_id == "defense")~ "-16027",
    (team == "PHI" & player_id == "defense")~ "-16021", 
    (team == "IND" & player_id == "defense")~ "-16011",
    (team == "PIT" & player_id == "defense")~ "-16023", 
    (team == "SF" & player_id == "defense")~ "-16025", 
    (team == "HOU" & player_id == "defense")~ "-16034", 
    (team == "CIN" & player_id == "defense")~ "-16004", 
    (team == "WAS" & player_id == "defense")~ "-16028", 
    (team == "NO" & player_id == "defense")~ "-16018", 
    (team == "ARI" & player_id == "defense")~ "-16022", 
    (team == "CLE" & player_id == "defense")~ "-16005",
    (team == "ATL" & player_id == "defense")~ "-16001", 
    (team == "SEA" & player_id == "defense")~ "-16026", 
    (team == "TEN" & player_id == "defense")~ "-16010",
    (team == "JAX" & player_id == "defense")~ "-16030", 
    (team == "LV" & player_id == "defense")~ "-16013", 
    (team == "LA" & player_id == "defense")~ "-16014",
    (team == "NYG" & player_id == "defense")~ "-16019", 
    (team == "NE" & player_id == "defense")~ "-16017",
    (team == "CAR" & player_id == "defense")~ "-16029",
    TRUE ~ espn_id
  ))


cur_season_stats_all <- cur_season_stats_all %>%
  mutate(espn_id = case_when(
    (team == "DEN" & player_id == "defense") ~ "-16007", 
    (team == "BUF" & player_id == "defense") ~ "-16002", 
    (team == "CHI" & player_id == "defense") ~ "-16003", 
    (team == "NYJ" & player_id == "defense") ~ "-16020",
    (team == "MIA" & player_id == "defense") ~ "-16006", 
    (team == "DAL" & player_id == "defense") ~ "-16015",
    (team == "LAC" & player_id == "defense") ~ "-16024",
    (team == "MIN" & player_id == "defense") ~ "-16016",
    (team == "BAL" & player_id == "defense") ~ "-16033", 
    (team == "KC" & player_id == "defense") ~ "-16012",
    (team == "DET" & player_id == "defense") ~ "-16008", 
    (team == "GB" & player_id == "defense") ~ "-16009", 
    (team == "TB" & player_id == "defense") ~ "-16027",
    (team == "PHI" & player_id == "defense") ~ "-16021", 
    (team == "IND" & player_id == "defense") ~ "-16011",
    (team == "PIT" & player_id == "defense") ~ "-16023", 
    (team == "SF" & player_id == "defense") ~ "-16025", 
    (team == "HOU" & player_id == "defense")  ~ "-16034", 
    (team == "CIN" & player_id == "defense") ~ "-16004", 
    (team == "WAS" & player_id == "defense") ~ "-16028", 
    (team == "NO" & player_id == "defense") ~ "-16018", 
    (team == "ARI" & player_id == "defense") ~ "-16022", 
    (team == "CLE" & player_id == "defense") ~ "-16005",
    (team == "ATL" & player_id == "defense") ~ "-16001", 
    (team == "SEA" & player_id == "defense") ~ "-16026", 
    (team == "TEN" & player_id == "defense") ~ "-16010",
    (team == "JAX" & player_id == "defense") ~ "-16030", 
    (team == "LV" & player_id == "defense") ~ "-16013", 
    (team == "LA" & player_id == "defense") ~ "-16014",
    (team == "NYG" & player_id == "defense") ~ "-16019", 
    (team == "NE" & player_id == "defense") ~ "-16017",
    (team == "CAR" & player_id == "defense") ~ "-16029",
    TRUE ~ espn_id
  ))

# Now you can rbind()
season_stats_all <- rbind(season_stats_all, cur_season_stats_all) %>% distinct()
weekly_stats_all <- rbind(weekly_stats_all, cur_weekly_stats_all) %>% distinct()



########## Uploading data back to google sheets ##########

##### Update the workbook with new data #####
writeData(wb, sheet = "Season", season_stats_all)
writeData(wb, sheet = "Weekly", weekly_stats_all)


saveWorkbook(wb, "ff_data.xlsx", overwrite = TRUE)


##### Deleting current file in google drive #####
drive_rm("ff_data.xlsx")


##### Uploading new xlsx file to google drive #####
drive_upload(
  media = "ff_data.xlsx",
  name = "ff_data.xlsx",
  type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)


