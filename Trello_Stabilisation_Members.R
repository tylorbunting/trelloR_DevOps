# SETUP ENVIRONMENT ----------------------------------------------------
#
# The key output of this script will be a table with the below variables and various plots
# card variables = id, name, description, status, type, week_complete, day_complete, raised_date, start_date, end_date, member, member_count, dev_effort, test_effort, total_effort, incident_category
#

# INSTALL APPROPRIATE PACKAGES
library("trelloR")
library("httpuv")
library("purrr")
library("scales")
library("lubridate")
library("tidyr")
library("plotly")
library("devtools")
library("dplyr")
library("reshape2")
library("stringr")
library("magrittr")
library("httr")

# create settings list
if(exists("Settings") != TRUE) Settings <- list()

# Debug mode?
Settings$Debug_Mode <- TRUE

# add authentication setting variables
# CHECK AUTHENTICATION VALUES EXISTS
if(exists("Trello_Key") != TRUE) stop("Trello Key value needs to be defined 'Trello_Key'")
if(exists("Trello_Token") != TRUE) stop("Trello Token value needs to be defined 'Trello_Token'")
if(exists("Trello_SecretKey") != TRUE) stop("Trello Secret Key value needs to be defined 'Trello_SecretKey'")
if(exists("TrelloR_Token") != TRUE) stop("Trello Secret Key value needs to be defined 'TrelloR_Token'")

#add setting variables
Settings$Trello_Key <- Trello_Key
Settings$Trello_Token <- Trello_Token
Settings$Trello_SecretKey <- Trello_SecretKey
Settings$TrelloR_Token <- TrelloR_Token

# create key and token string
Settings$Trello_Auth <- paste("key=",Settings$Trello_Key,"&token=",Settings$Trello_Token, sep = "")

# get all the functions needed for this script
source_url("https://raw.githubusercontent.com/tylorbunting/trelloR_DevOps/master/_functions.R")

# SETUP INPUT VALUES
Settings$Board_url <- "https://trello.com/b/5LZ5YvHo/ai-hub-process-automation"

# SET CUSTOMFIELD IDS
Settings$CustomFields_variables <- list(
  Date_Raised = "5c9d92bb98be778d4583c9ef",
  Start_Date = "5c5a1f2925edda7fd322f890",
  End_Date = "5c5a1f3135e79935255e1945",
  Dev_Effort = "5d491bc1f4505576cf5fa483",
  Test_Effort = "5d491c1788a5af71581e130a",
  Incident_Category = "5d491cba205bb14d05f567a6",
  days_blocked = "5dc8b1dbff9b7b286ab0fb36"
)

# SETUP VARIOUS FUNCTIONS
# labels to capture for analysis (assumption is that tickets only have one label assigned)
Settings$labels_for_analysis <- c("Stable", "Stopped", "Unstable")


# 1. GET TRELLO DATA ------------------------------------------------------
# get data using the trellR package method
if(exists("Data_1") != TRUE) {

  #create main list for initial step
  Data_1 <- list()
  
  #get various data
  Data_1$Board_id <- get_id_board(Settings$Board_url, token = Settings$TrelloR_Token)
  Data_1$Board_lists <- get_board_lists(Data_1$Board_id, token = Settings$TrelloR_Token)
  Data_1$Board_cards <- get_board_cards(Data_1$Board_id, Settings$TrelloR_Token, filter = "all", paging = TRUE)
  Data_1$Board_members <- get_board_members(Data_1$Board_id, Settings$TrelloR_Token)
  #expensive query below so it is commented out
  #Data_1$Board_card_members <- map_df(Board_cards$id, get_card_members, token = TrelloR_Token)
  # get an array of Cards on a board (User Story Kanban Board) and include custom fields SOURCE: https://developers.trello.com/docs/getting-started-custom-fields 
  Data_1$Board_cards_with_customFields <- GET(paste("https://api.trello.com/1/boards/5LZ5YvHo?fields=name&cards=all&card_fields=name&customFields=true&card_customFieldItems=true&", Settings$Trello_Auth, sep = ""))
  Data_1$Board_cards_with_customFields <- content(Data_1$Board_cards_with_customFields)
  # extract sub lists from "Board_cards_with_customFields" parent list
  Data_1$Board_customFields <- Data_1$Board_cards_with_customFields$customFields
  Data_1$Board_cards_with_customFields <- Data_1$Board_cards_with_customFields$cards
}


# 2. SET CUSTOM FIELD IDS FOR EACH CARD & REMOVE UNEEDED DATA -----------------------------------
Data_2 <- Data_1

# Card data with "values" for DATE type customFields and "ids" for DROPDOWN type customFields
Data_2$Board_cards_with_customFields <- map_df(Data_2$Board_cards_with_customFields, function(x) {
  data.frame(
    card_id = as.character(extract(x, "id")),
    name = as.character(extract(x, "name"))
  )
})

# Card data with "values" for DROPDOWN type customField as well as DATE type customFields
Data_2$Board_cards_with_customFields <- data.frame(
  card_id = as.character(Data_2$Board_cards_with_customFields[["card_id"]]),
  name = as.character(Data_2$Board_cards_with_customFields[["name"]])
)

# data of all Card Ids and related Member Ids
Data_2$Board_cards_and_members <- map_df(apply(Data_2$Board_cards, 1, get_cards_and_member_ids), rbind.data.frame)

# for some reasons the Board_cards_and_members data has duplicate cards so the below code remove the duplicate rows
Data_2$Board_cards_and_members <- distinct(Data_2$Board_cards_and_members)

# Card data with normal values / non custom fields
Data_2$Board_cards <- data.frame(
    card_id = as.character(Data_2$Board_cards[["id"]]),
    name = as.character(Data_2$Board_cards[["name"]]),
    description = as.character(Data_2$Board_cards[["desc"]]),
    closed = as.logical(Data_2$Board_cards[["closed"]]),
    list_id = as.character(Data_2$Board_cards[["idList"]]),
    url = as.character(Data_2$Board_cards[["url"]]),
    label = map_chr(Data_2$Board_cards[["labels"]], get_label_name_trelloR, Settings$labels_for_analysis),
    member_count = map_chr(Data_2$Board_cards[["idMembers"]], get_card_member_count)
  )

# for some reasons the Board_cards data has duplicate cards so the below code remove the duplicate rows
Data_2$Board_cards <- distinct(Data_2$Board_cards)

# list data for board
Data_2$Board_lists <- data.frame(
  list_id = as.character(Data_2$Board_lists$id),
  list_name = as.character(Data_2$Board_lists$name),
  list_position = as.integer(Data_2$Board_lists$pos)
)

# all cards and members (one card to many members)
Data_2$Board_members <- data.frame(
  member_id = as.character(Data_2$Board_members$id),
  fullname = as.character(Data_2$Board_members$fullName),
  username = as.character(Data_2$Board_members$username)
)


# 3. MERGE MAJOR DATASETS TO CREATE MASTER DATA FOR BOARD ---------------------------
Data_3 <- Data_2

# update Board_cards by merging it with Board_lists to get "list_name" and "list_position" using "list_id"
Data_3$Board_cards <- Data_3$Board_cards %>%
  left_join(Data_3$Board_lists, by = "list_id") %>%
  select(-list_id)

# update Board_cards by merging it with Board_cards_and_members to get "member_id" using "card_id" (full_join)
Data_3$Board_cards <- Data_3$Board_cards %>%
  full_join(Data_3$Board_cards_and_members, by = "card_id")

# update Board_cards by merging it with Board_card_members to get "fullname", "initials", and "username" using "card_id"
Data_3$Board_cards <- Data_3$Board_cards %>%
  left_join(Data_3$Board_members, by = "member_id") %>%
  select(-member_id)

# update Board_cards
#Data_3$Board_cards <- Data_3$Board_cards %>%
#  select(-name) %>%
#  left_join(Data_3$Board_cards_with_customFields, by = "card_id")


# 4. MAKE FINAL CHANGES TO TABLE BEFORE VISUALISING -----------------------
Data_4 <- Data_3

# update the three date related customField values from "factors" to "date" types
#Data_4$Board_cards$date_raised <- ymd(str_extract(Data_4$Board_cards$date_raised, "[0-9-]{10}"))
#Data_4$Board_cards$date_started <- ymd(str_extract(Data_4$Board_cards$date_started, "[0-9-]{10}"))
#Data_4$Board_cards$date_ended <- ymd(str_extract(Data_4$Board_cards$date_ended, "[0-9-]{10}"))

# divide DEV and TEST effort by member_count to reflect actual effort for DEV and TEST per row in the "Board_cards" table 
# Data_4$Board_cards <- Data_4$Board_cards %>%
#   mutate(dev_effort = round(as.numeric(as.character(dev_effort)) / as.numeric(as.character(member_count)), 2)) %>%
#   mutate(test_effort = round(as.numeric(as.character(test_effort)) / as.numeric(as.character(member_count)), 2))
# 
# # update DEV and TEST effort so that the NA's are represented by 0's
# Data_4$Board_cards$dev_effort <- replace_na(Data_4$Board_cards$dev_effort, 0)
# Data_4$Board_cards$test_effort <- replace_na(Data_4$Board_cards$test_effort, 0)
# 
# # create total_effort variables
# Data_4$Board_cards <- Data_4$Board_cards %>%
#   mutate(total_effort = round(dev_effort + test_effort, 2))
# 
# # add avg_wait and avg_resolve times
# Data_4$Board_cards <- Data_4$Board_cards %>%
#   mutate(avg_wait = difftime(date_started, date_raised, units = "days")) %>%
#   mutate(avg_resolution = difftime(date_ended, date_started, units = "days"))
# 
# # add week_ended, day_ended, month_name_ended, month_number_ended, year_ended  variables
# Data_4$Board_cards <- Data_4$Board_cards %>%
#   mutate(week_ended = week(date_ended)) %>%
#   mutate(day_ended = wday(date_ended, label = TRUE)) %>%
#   mutate(month_name_ended = month(date_ended, label = TRUE)) %>%
#   mutate(month_number_ended = month(date_ended, label = FALSE)) %>%
#   mutate(year_ended = year(date_ended))
# 
# # add avg_wait and avg_resolve times
# Data_4$Board_cards <- Data_4$Board_cards %>%
#   mutate(avg_wait = difftime(date_started, date_raised, units = "days")) %>%
#   mutate(avg_resolution = difftime(date_ended, date_started, units = "days"))
# 
# # change wait, resolution, and blocked values to numerics for math operations
# Data_4$Board_cards <- Data_4$Board_cards %>%
#   mutate(avg_wait = as.numeric(avg_wait)) %>%
#   mutate(avg_resolution = as.numeric(avg_resolution)) %>%
#   mutate(days_blocked = as.numeric(as.character(days_blocked)))
# 
# # whatever the "avg_resolution" time is, remove the number of "days_blocked", and set negative "avg_resolution" values to NA
# Data_4$Board_cards <- Data_4$Board_cards %>%
#   mutate(avg_resolution = replace_na(avg_resolution, 0) - replace_na(days_blocked, 0)) %>%
#   mutate(avg_resolution = as.numeric(str_replace(as.character(avg_resolution), "-\\d*|^0", "NA")))

# add bucket variable
Data_4$Board_cards <- Data_4$Board_cards %>%
  mutate(bucket = str_extract(name, "(^.*?)(?= -)"))
# 
# # update missing values in Incident_Category column
# Data_4$Board_cards <- Data_4$Board_cards %>%
#   mutate(incident_category = replace_na(as.character(incident_category), "NA"))

# # Change labels that are "unknown" to instead say "Development"
Data_4$Board_cards <- Data_4$Board_cards %>%
  mutate(label = ifelse(label %in% as.character(Settings$labels_for_analysis), as.character(label), "Unknown"))

# 5. VISUALISE DATA -------------------------------------------------------
Data_5 <- Data_4

if(exists("Plots") != TRUE) Plots <- list()


# 5.1. VISUALISATION OPTIONS ----------------------------------------------
Plots$Options <- list()

# the labels that will be filtered for the visualisations
Plots$Options$labels_for_analysis <- c("Enhancement", "Internal Request", "External Request", "Incident")

# the people that will be filtered for the visualisations
Plots$Options$people_for_analysis <- c("Tylor Bunting", "Sean Xiang", "Richard Yeh", "ambitkumar","Bianca De Jesus","Hannah Jia")


# create table specific for run team
Data_5$Board_cards_Run <- Data_5$Board_cards %>% 
  #filter(label %in% Plots$Options$labels_for_analysis) %>%
  filter(fullname %in% Plots$Options$people_for_analysis)

# how to apply gradient colors to discrete values = https://stackoverflow.com/questions/30352412/how-do-you-create-a-gradient-of-colors-for-a-discrete-variable-in-ggplot2
Plots$Options$color_scale <- seq_gradient_pal("#00BFC4", "#fdd5b4", "Lab")(seq(0,1,length.out = 8))

# 5.2. NUMBER OF PROCESSES PER MEMBER ------------------------------------------------
Plots$processes_per_run_member <- ggplot(Data_5$Board_cards_Run) +
  aes(x = fullname, fill = label) +
  geom_bar() +
  labs(title = "Number of Processes Per Run Member", x = "Process Count", y = "Name", fill = "Status") +
  scale_fill_brewer(palette = "Blues") +
  geom_hline(yintercept = 50, linetype="dashed", color = "red") +
  coord_flip() +
  theme_minimal() 

# 6. CLEAN UP ENVIRONMENT -------------------------------------------------
if(Settings$Debug_Mode != TRUE) {
  rm(Data_1, Data_2, Data_3, Data_4, gA, gB, gC, gD, maxWidth, Settings)
}

  