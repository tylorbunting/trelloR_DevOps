# 0. SETUP ENVIRONMENT AND INPUT VARIABLES --------------------------------

# attempt to install all require packages
source("_package_manager.R")

# get all functions needed for TrelloR_custom
source("_functions.R")

# import libraries
library(httr)
library(jsonlite)
library(stringr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(lubridate)
library(magrittr)
library(purrr)
library(grid)
library(gridExtra)

if(exists("Settings") != TRUE) Settings <- list()

# SETUP INPUT VALUES
Settings$Board_url <- "https://trello.com/b/5LZ5YvHo/ai-hub-process-automation"

# add authentication setting variables
Settings$Trello_Key <- "4f0a4fcacc9b53edd8b79942caa027a3"
Settings$Trello_Token <- "85422bf57716c09c61b1043a2ba52198320d5993842b52a5e849fed26344f1a9"
Settings$Trello_SecretKey <- "e3512e11ac64fd3bd927429a5294506d1d7be2b87236cd011c19eee9f6bc833b"
if(exists("Settings$TrelloR_Token") != TRUE) Settings$TrelloR_Token <- trello_get_token(Settings$Trello_Key, Settings$Trello_SecretKey)

# create key and token string
Settings$Trello_Auth <- paste("key=",Settings$Trello_Key,"&token=",Settings$Trello_Token, sep = "")
rm(Settings$Trello_Key, Settings$Trello_Token)

# IDs for AI Hub Boards
ref_processes_in_stabilisation <- "5bbeb374b05b6d6c1c16480d"
ref_processes_in_production_support <- "5bb3ef682d017081ae3fa687"

# labels to capture for analysis (assumption is that tickets only have one label assigned)
labels_for_analysis <- c("Stopped", "Unstable", "Stable", "Waiting")


# 1. EXTRACT AI HUB BOARD DATA --------------------------------------------

# get an array of Lists on a board (AI Hub Process Automation Board) to check the List IDs we want
ref_board_lists <- GET(paste("https://api.trello.com/1/boards/5LZ5YvHo/lists?", Settings$Trello_Auth, sep = ""))
# get all content for each list
ref_board_lists_1 <- content(ref_board_lists)

# get all cards in the "Processes in Stabilisation" list
STAGED_DATA_1_STABILISING <- GET(paste("https://api.trello.com/1/lists/5bbeb374b05b6d6c1c16480d/cards?", Settings$Trello_Auth, sep = ""))
STAGED_DATA_1_STABILISING <- content(STAGED_DATA_1_STABILISING)

# get all cards in the "Processes in Production Support" list
STAGED_DATA_1_PRODUCTIONSUPPORT <- GET(paste("https://api.trello.com/1/lists/5bb3ef682d017081ae3fa687/cards?", Settings$Trello_Auth, sep = ""))
STAGED_DATA_1_PRODUCTIONSUPPORT <- content(STAGED_DATA_1_PRODUCTIONSUPPORT)

# get board ID for various TrelloR GET functions
Board_id <- get_id_board(Settings$Board_url, token = Settings$TrelloR_Token)

# get members on board
Board_members <- get_board_members(Board_id, Settings$TrelloR_Token)


# 2. TRANSFORM DATA INTO DATAFRAMES INSTEAD OF LISTS  ------------------------------------------

# apply function to get details of cards on the board (SOURCE: https://stackoverflow.com/questions/45310166/extracting-data-from-nested-list-in-r)
STAGED_DATA_2_STABILISING <- map_df(STAGED_DATA_1_STABILISING, function(x){
  data.frame(
    id = as.character(extract(x, "id")),
    name = as.character(extract(x, "name")),
    label_type = get_label_name(x, labels_for_analysis = labels_for_analysis)
  )
})

# add additional column to dataframe representing status of card
STAGED_DATA_2_STABILISING <- STAGED_DATA_2_STABILISING %>%
  mutate(status = "Stabilisation")

# apply function to get details of cards on the board (SOURCE: https://stackoverflow.com/questions/45310166/extracting-data-from-nested-list-in-r)
STAGED_DATA_2_PRODUCTIONSUPPORT <- map_df(STAGED_DATA_1_PRODUCTIONSUPPORT, function(x){
  data.frame(
    id = as.character(extract(x, "id")),
    name = as.character(extract(x, "name")),
    label_type = get_label_name(x, labels_for_analysis = labels_for_analysis)
  )
})

# add additional column to dataframe representing status of card
STAGED_DATA_2_PRODUCTIONSUPPORT <- STAGED_DATA_2_PRODUCTIONSUPPORT %>%
  mutate(status = "Production Support")


# 3. MERGE TABLES INTO FINAL STAGED TABLE ---------------------------------

# combine the stabilisation and production support dataframes
STAGED_DATA_3_COMBINED <- rbind.data.frame(STAGED_DATA_2_STABILISING, STAGED_DATA_2_PRODUCTIONSUPPORT)

# merge STAGED_DATA_3_COMBINED with member data
STAGED_DATA_3_COMBINED %>% 
  left_join(Board_members)


# 4. EXTRACT SUMMARIES AND VISUALISE DATA ---------------------------------

# extract the totals for various label_types and status's
STAGED_DATA_4_TOTALS <- STAGED_DATA_3_COMBINED %>%
  group_by(status, label_type) %>%
  count()

# extract individual stopped, stable, and unable processes
PROCESSES_STOPPED <- STAGED_DATA_4_TOTALS %>%
  filter(label_type == "Stopped") %>%
  group_by(label_type) %>%
  summarise(n = sum(n))

PROCESSES_STABILISING <- STAGED_DATA_4_TOTALS %>%
  filter(status == "Stabilisation" && label_type!= "Stopped") %>%
  group_by(status) %>%
  summarise(n = sum(n))

PROCESSES_PRODUCTIONSUPPORT <- STAGED_DATA_4_TOTALS %>%
  filter(status == "Production Support" && label_type!= "Stopped") %>%
  group_by(status) %>%
  summarise(n = sum(n))

# visualise the data
STAGED_DATA_4_TOTALS_PLOT <- ggplot(STAGED_DATA_4_TOTALS, aes(x = status, y = n, fill = label_type)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("#00BFC4", "#fdd5b4", "#fbeeb8", "#DCDCDC")) +
  scale_x_discrete(limits = c("Stabilisation", "Production Support")) +
  labs(title = "Number of Processes in Stabilisation and Production Support", x = "Support Type", y = "Number of Processes", fill = "Status") +
  theme(plot.title = element_text(hjust = 0.5))

# show plots
STAGED_DATA_4_TOTALS_PLOT
