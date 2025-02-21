library(nflreadr);library(tidyverse);library(httr);library(jsonlite)
library(dplyr);library(ffscrapr);library(googledrive);library(googlesheets4)
library(readr); library(openxlsx)

########## Reading in csv from google drive ##########
drive_auth()


file <- drive_get("ff_data.xlsx")  # Replace with your actual filename

temp_file <- tempfile(fileext = ".xlsx")  # Create a temporary file path

drive_download(file, path = temp_file, overwrite = TRUE)  # Download the file

# Load the Excel file into R
wb <- loadWorkbook(temp_file)  # Open the workbook

# Read each sheet into a data frame
season_stats_all <- read.xlsx(wb, sheet = "Sheet1")
weekly_stats_all <- read.xlsx(wb, sheet = "Sheet2")


##### removing all rows from current season so season metrics can be recalculated given this runs every week #####
cur_year <- lubridate::year(Sys.Date())
season_stats_all <- season_stats_all %>% filter(season != cur_year)
weekly_stats_all <- weekly_stats_all %>% filter(season != cur_year)



########## Scraping data from ESPN fantasy football league ##########

# conn <- espn_connect(
#   season = 2024,  # Replace with the desired season
#   league_id = 609898,  # Replace with your league ID
#   swid = "{B8AFA122-A403-4ED0-AFA1-22A4034ED037}",
#   espn_s2 = "AECdBdbuiCM%2Bk03W5lSmfuJ1vEPhdwuH2FmEeuOdFlD3%2B7P%2BZnMZIaqZ77VE5O%2F4t%2BUyzgNuXK4nH5DMZC%2FCUfUTR72qsEWfN5ID4O1w%2FE6zs8mK0W5KTvKkx38XjDx8JudLVOrXTVSZYcIHkUiq6x3oQW%2B9n%2FoV28CkOcXHDlQsqqfa4s%2FvXdFlGAXWbSsN5OmpCmdB29g9efT0eVXo%2Fa8wpQ3AsLXBoWqBZTRRxlEuu8SgP44iuDn5LSuOKMjj3Fd9ltVfqNHhtkmYhz4P3OnP4OKqRE7%2Fbzi2OBt58gLxeI0vaiqO6BRpIhqhz4ly91k%3D"
# )
# 
# 
# rosters <- ff_rosters(conn)
# ######################
# test_url <- "https://fantasy.espn.com/apis/v3/games/ffl/seasons/2024/segments/0/leagues/609898?view=mTeam"
# 
# # Your authentication cookies (copy these exactly from browser)
# espn_s2 <- "AECdBdbuiCM%2Bk03W5lSmfuJ1vEPhdwuH2FmEeuOdFlD3%2B7P%2BZnMZIaqZ77VE5O%2F4t%2BUyzgNuXK4nH5DMZC%2FCUfUTR72qsEWfN5ID4O1w%2FE6zs8mK0W5KTvKkx38XjDx8JudLVOrXTVSZYcIHkUiq6x3oQW%2B9n%2FoV28CkOcXHDlQsqqfa4s%2FvXdFlGAXWbSsN5OmpCmdB29g9efT0eVXo%2Fa8wpQ3AsLXBoWqBZTRRxlEuu8SgP44iuDn5LSuOKMjj3Fd9ltVfqNHhtkmYhz4P3OnP4OKqRE7%2Fbzi2OBt58gLxeI0vaiqO6BRpIhqhz4ly91k%3D"
# swid <- "{B8AFA122-A403-4ED0-AFA1-22A4034ED037}"
# 
# # Make an authenticated request
# test_response <- GET(
#   test_url,
#   set_cookies(
#     SWID = swid,
#     espn_s2 = espn_s2
#   ),
#   user_agent("Mozilla/5.0")  # Mimic a browser request
# )
# 
# # Print response
# print(test_response)
# 
# json_data <- content(test_response, "text", encoding = "UTF-8")
# json_parsed <- jsonlite::fromJSON(json_data)
# print(json_parsed)

nflreadr::.clear_cache()

# loading in play by play data from 2021-2024
pbp2024 <- load_pbp(2024)
pbp2023 <- load_pbp(2023)
pbp2022 <- load_pbp(2022)
pbp2021 <- load_pbp(2021)

# loading in teams (could use for logos on dashboard)
teams <- load_teams()

# loading in fantasy football player ids (should make it easier to connect data)
ff_ids <- load_ff_playerids()


# loading in fantasy rankings
# their ID is the same as fantasypros_id in ff_ids
ff_rankings <- load_ff_rankings()


# loading in injury data from 2021-2024
injuries2024 <- load_injuries(2024)
injuries2023 <- load_injuries(2023)
injuries2022 <- load_injuries(2022)
injuries2021 <- load_injuries(2021)


depth_charts <- load_depth_charts() %>%
  filter(position %in% c("K", "QB", "RB", "TE", "WR"))


NextGenStats <- load_nextgen_stats() %>% filter(season > 2020)

##############################################################

cur_year <- lubridate::year(Sys.Date())

# replace cur_year with 2024 to get data just to test on
weekly_data <- load_player_stats(seasons = cur_year) %>% filter(position %in% c("FB", "QB", "RB", "TE", "WR"))

# getting half ppr calculations
weekly_data <- weekly_data %>%
  mutate(half_PPR = (passing_yards * 0.04) + (passing_tds * 4) + (rushing_yards * 0.1) + (rushing_tds * 6) + (receptions * 0.5) + (receiving_yards * 0.1) + (receiving_tds * 6) + ((rushing_fumbles_lost + sack_fumbles_lost + receiving_fumbles_lost + interceptions) * -2) + ((passing_2pt_conversions + rushing_2pt_conversions + receiving_2pt_conversions)*2)) 


season_stats <- weekly_data %>%
  group_by(player_id, player_display_name) %>%
  summarise(
    position = position,
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
    PPR_ppg = round(mean(fantasy_points_ppr, na.rm = TRUE),2)
  ) %>% 
  distinct()


##### combining old and new data #####
season_stats_all <- rbind(season_stats_all, season_stats) %>%
  distinct()

weekly_stats_all <- rbind(weekly_stats_all, weekly_data) %>%
  distinct()



########## Uploading data back to google sheets ##########
# Adding new sheets if necessary
# Add a worksheet (if it doesn’t exist)
#addWorksheet(wb, "Sheet1")


##### Update the workbook with new data #####
writeData(wb, sheet = "Sheet1", season_stats_all)
writeData(wb, sheet = "Sheet2", weekly_stats_all)


saveWorkbook(wb, "ff_data.xlsx", overwrite = TRUE)


##### Deleting current file in google drive #####
drive_rm("ff_data.xlsx")


##### Uploading new xlsx file to google drive #####
drive_upload(
  media = "ff_data.xlsx",
  name = "ff_data.xlsx",
  type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)



