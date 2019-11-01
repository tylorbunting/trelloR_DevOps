# 0. SETUP ENVIRONMENT AND INPUT VARIABLES --------------------------------
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

# create settings list
if(exists("Settings") != TRUE) Settings <- list()

# Debug mode?
Settings$Debug_Mode <- FALSE

# add authentication setting variables
Trello_Key <- "4f0a4fcacc9b53edd8b79942caa027a3"
Trello_Token <- "85422bf57716c09c61b1043a2ba52198320d5993842b52a5e849fed26344f1a9"
Trello_SecretKey <- "e3512e11ac64fd3bd927429a5294506d1d7be2b87236cd011c19eee9f6bc833b"

# create key and token string
Trello_Auth <- paste("key=",Trello_Key,"&token=",Trello_Token, sep = "")
rm(Trello_Key, Trello_Token)

# get an array of Lists on a board (AI Hub Process Automation Board) to check the List IDs we want
ref_board_lists <- GET(paste("https://api.trello.com/1/boards/5LZ5YvHo/lists?", Trello_Auth, sep = ""))
# get all content for each list
ref_board_lists_1 <- content(ref_board_lists)

# IDs for AI Hub Boards
ref_processes_in_stabilisation <- "5bbeb374b05b6d6c1c16480d"
ref_processes_in_production_support <- "5bb3ef682d017081ae3fa687"

# labels to capture for analysis (assumption is that tickets only have one label assigned)
labels_for_analysis <- c("Stopped", "Unstable", "Stable", "Waiting")


# 1. EXTRACT AI HUB BOARD DATA --------------------------------------------

# get all cards in the "Processes in Stabilisation" list
STAGED_DATA_1_STABILISING <- GET(paste("https://api.trello.com/1/lists/5bbeb374b05b6d6c1c16480d/cards?", Trello_Auth, sep = ""))
STAGED_DATA_1_STABILISING <- content(STAGED_DATA_1_STABILISING)

# get all cards in the "Processes in Production Support" list
STAGED_DATA_1_PRODUCTIONSUPPORT <- GET(paste("https://api.trello.com/1/lists/5bb3ef682d017081ae3fa687/cards?", Trello_Auth, sep = ""))
STAGED_DATA_1_PRODUCTIONSUPPORT <- content(STAGED_DATA_1_PRODUCTIONSUPPORT)



# 2. TRANSFORM DATA INTO DATAFRAMES INSTEAD OF LISTS  ------------------------------------------

# create function to extract first label name found out of labels_for_analysis input variable
get_label_name <- function(x) {
  if(length(x[["labels"]]) > 0) {
    for(index in seq_along(x[["labels"]])) {
      if(as.character(x[["labels"]][[index]][["name"]]) %in% labels_for_analysis) {
        output <- x[["labels"]][[index]][["name"]]
        break
      } else output <- "Unsure"
    }
  } else output <- "Unsure"
  return(output)
}

# apply function to get details of cards on the board (SOURCE: https://stackoverflow.com/questions/45310166/extracting-data-from-nested-list-in-r)
STAGED_DATA_2_STABILISING <- map_df(STAGED_DATA_1_STABILISING, function(x){
  data.frame(
    name = as.character(extract(x, "name")),
    label_type = get_label_name(x)
  )
})

# add additional column to dataframe representing status of card
STAGED_DATA_2_STABILISING <- STAGED_DATA_2_STABILISING %>%
  mutate(status = "Stabilisation")

# apply function to get details of cards on the board (SOURCE: https://stackoverflow.com/questions/45310166/extracting-data-from-nested-list-in-r)
STAGED_DATA_2_PRODUCTIONSUPPORT <- map_df(STAGED_DATA_1_PRODUCTIONSUPPORT, function(x){
  data.frame(
    name = as.character(extract(x, "name")),
    label_type = get_label_name(x)
  )
})

# add additional column to dataframe representing status of card
STAGED_DATA_2_PRODUCTIONSUPPORT <- STAGED_DATA_2_PRODUCTIONSUPPORT %>%
  mutate(status = "Production Support")



# 3. MERGE TABLES INTO FINAL STAGED TABLE ---------------------------------

# combine the stabilisation and production support dataframes
STAGED_DATA_3_COMBINED <- rbind.data.frame(STAGED_DATA_2_STABILISING, STAGED_DATA_2_PRODUCTIONSUPPORT)



# 4. EXTRACT SUMMARIES AND VISUALISE DATA ---------------------------------

if(exists("Plots") != TRUE) Plots <- list()

# extract the totals for various label_types and status's
Plots$STAGED_DATA_4_TOTALS <- STAGED_DATA_3_COMBINED %>%
  group_by(status, label_type) %>%
  count()

# extract individual stopped, stable, and unable processes
Plots$PROCESSES_STOPPED <- Plots$STAGED_DATA_4_TOTALS %>%
  filter(label_type == "Stopped") %>%
  group_by(label_type) %>%
  summarise(n = sum(n))

Plots$PROCESSES_STABILISING <- Plots$STAGED_DATA_4_TOTALS %>%
  filter(status == "Stabilisation" && label_type!= "Stopped") %>%
  group_by(status) %>%
  summarise(n = sum(n))

Plots$PROCESSES_PRODUCTIONSUPPORT <- Plots$STAGED_DATA_4_TOTALS %>%
  filter(status == "Production Support" && label_type!= "Stopped") %>%
  group_by(status) %>%
  summarise(n = sum(n))

# visualise the data
Plots$STAGED_DATA_4_TOTALS_PLOT <- ggplot(Plots$STAGED_DATA_4_TOTALS, aes(x = status, y = n, fill = label_type)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("#00BFC4", "#fdd5b4", "#fbeeb8", "#DCDCDC")) +
  scale_x_discrete(limits = c("Stabilisation", "Production Support")) +
  labs(title = "Number of Processes in Stabilisation and Production Support", x = "Support Type", y = "Number of Processes", fill = "Status") +
  theme(plot.title = element_text(hjust = 0.5))


# CLEAN UP ENVIRONMENT ----------------------------------------------------
if(Settings$Debug_Mode != TRUE) {
  rm(ref_board_lists, ref_board_lists_1,
     STAGED_DATA_1_PRODUCTIONSUPPORT, STAGED_DATA_1_STABILISING, STAGED_DATA_2_PRODUCTIONSUPPORT, STAGED_DATA_2_STABILISING,
     STAGED_DATA_3_COMBINED, Settings)
}

