# 0. SETUP ENVIRONMENT AND INPUT VARIABLES --------------------------------
# attempt to install all require packages
source("C:/Users/tbun2893/Documents/GitHub/trelloR_custom/_package_manager.R")

# get all functions needed for TrelloR_custom
source("C:/Users/tbun2893/Documents/GitHub/trelloR_custom/_functions.R")

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
library(scales)

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

# set file path for output
set_file_path <- "Trello_Operations_Initiatives_Output.csv"

# get an array of Lists on a board (Operational Initiatives Board) to check the List IDs we want
ref_board_lists <- GET(paste("https://api.trello.com/1/boards/jdgMj8dX/lists?", Trello_Auth, sep = ""))
# get all content for each list
ref_board_lists_1 <- content(ref_board_lists)

# labels to capture for analysis (assumption is that tickets only have one label assigned)
labels_for_analysis <- c("PROBLEM", "PROJECT", "BAU")


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

# Set customField values to extract data
customField_value <- "5d5a32602ff4d664177a8b93"
customField_effort <- "5d5a3290c1a7f0448802efb3"
customField_complexity <- "5cd10ed5a0ea1813685e81d7"
customField_date_raised <- "5cb536c4d36ef81bb89bbdda"
customField_date_start <- "5cb536c4d36ef81bb89bbddc"
customField_date_end <- "5ccfe165f48d7678f7f9e57e"

# create function to extract customField DATE VALUES from cards
get_customField_date_values <- function(x, customField_id) {
  if(length(x[["customFieldItems"]]) > 0) {
    for(index in seq_along(x[["customFieldItems"]])) {
      if(as.character(x[["customFieldItems"]][[index]][["idCustomField"]]) == customField_id) {
        output <- as.character(x[["customFieldItems"]][[index]][["value"]][["date"]])
        break
      } else output <- "unknown"
    }
  } else output <- "unknown"
  return(output)
}

# create function to extract customField DROPDOWN VALUES from cards
get_customField_dropdown_values <- function(x, customField_id) {
  if(length(x[["customFieldItems"]]) > 0) {
    for(index in seq_along(x[["customFieldItems"]])) {
      if(as.character(x[["customFieldItems"]][[index]][["idCustomField"]]) == customField_id) {
        # the value DROPDOWN custom field values within the nested idCustomField list is an ID that needs to be mapped 
        output <- as.character(x[["customFieldItems"]][[index]][["idValue"]])
        break
      } else output <- "unknown"
    }
  } else output <- "unknown"
  return(output)
}


# 1. GET DATA -------------------------------------------------------------

# get an array of Cards on a board (Operational Initiatives Board) and include custom fields SOURCE: https://developers.trello.com/docs/getting-started-custom-fields 
board_cards <- GET(paste("https://api.trello.com/1/boards/jdgMj8dX/?fields=name&cards=all&card_fields=name&customFields=true&card_customFieldItems=true&", Trello_Auth, sep = ""))
# get all content for each card in the board
board_cards_1 <- content(board_cards)

# get an array of Cards on a board (Operational Initiatives Board) and include extra card data (e.g. description, members etc.)
board_cards_extra <- GET(paste("https://api.trello.com/1/boards/jdgMj8dX/cards/visible?", Trello_Auth, sep = ""))
# get all content for each card in board
board_cards_extra_1 <- content(board_cards_extra)


# 2. TRANSFORM BOARD DATA FROM LIST FORMAT INTO DATA FRAMES -----------------------------------------------------------------
# create data frames for transforms
board_cards_2 <- board_cards_1
board_cards_extra_2 <- board_cards_extra_1
ref_board_lists_2 <- ref_board_lists_1

# get customFields from 'board_cards_2'
ref_board_cards_customFields_2 <- map_df(board_cards_2[["customFields"]], function(x) {
  data.frame(
    id = extract(x, "id"),
    name = extract(x, "name")
  )
})

# get list values for merging with final output using list ID's (list name etc not included in card data)
ref_board_lists_2 <- map_df(ref_board_lists_2, function(x) {
  data.frame(
    idList = x[["id"]],
    name = x[["name"]]
  )
})

# apply function to get label names as well as other extra details of cards on the board (SOURCE: https://stackoverflow.com/questions/45310166/extracting-data-from-nested-list-in-r)
board_cards_extra_2 <- map_df(board_cards_extra_2, function(x){
  data.frame(
    id = as.character(extract(x, "id")),
    name = as.character(extract(x, "name")),
    desc = as.character(extract(x, "desc")),
    idList = as.character(extract(x, "idList")),
    label_type = get_label_name(x)
  )
})


# apply function to get customField date values as well as other details of cards on the board (SOURCE: https://stackoverflow.com/questions/45310166/extracting-data-from-nested-list-in-r)
board_cards_2 <- map_df(board_cards_2[["cards"]], function(x){
  data.frame(
    id = as.character(extract(x, "id")),
    name = as.character(extract(x, "name")),
    date_raised = get_customField_date_values(x = x, customField_id = customField_date_raised),
    # complexity = get_customField_dropdown_values(x = x, customField_id = customField_complexity),
    effort = get_customField_dropdown_values(x = x, customField_id = customField_effort)
    # value = get_customField_dropdown_values(x = x, customField_id = customField_value)
  )
})


# ######## TEMP
#         # script to show all the cards that use the VALUE custom field
#         map(board_cards_2[["cards"]], function(x) {
#           if(length(x[["customFieldItems"]]) > 0) {
#             for(index in seq_along(x[["customFieldItems"]])) {
#               if(as.character(x[["customFieldItems"]][[index]][["idCustomField"]]) == "5d5a32602ff4d664177a8b93") {
#                 output <- as.character(x[["customFieldItems"]][[index]][["idValue"]])
#                 break
#               } else output <- "unknown"
#             }
#           } else output <- "unknown"
#           return(output)
#         })
#         
#         # script to show all the cards that use the VALUE custom field
#         map(Data_1$User_Story_Board_cards_with_customFields[["cards"]], function(x) {
#           if(length(x[["customFieldItems"]]) > 0) {
#             map(index in seq_along(x[["customFieldItems"]])) {
#               if(as.character(x[["customFieldItems"]][[index]][["idCustomField"]]) == "5d5a32602ff4d664177a8b93") {
#                 output <- as.character(x[["customFieldItems"]][[index]][["idValue"]])
#                 break
#               } else output <- "unknown"
#             }
#           } else output <- "unknown"
#           return(output)
#         })
#       
#         # using below line of code you can get an example of a card that uses the VALUE custom field
#         str(board_cards_2$cards[[164]])
#         
#         # script to extract a table of DROPDOWN value options and associated IDs 
#         map(board_cards_2[["customFields"]], function(x) {
#           for(index in seq_along(x)) {
#             if(as.character(x[[index]][["id"]]) == "5d5a32602ff4d664177a8b93") {
#               for(index2 in seq_along(x[[index]])) {
#                 if(as.character(x[[index]][[index2]][["options"]][[index2]][["id"]] == "5d5a32602ff4d664177a8b94")) {
#                   break  
#                 }
#               } break
#             } else output <- "unknown"
#           }
#           return(output)
#         })
#         # the above scripts show that the DROPDOWN value type custom fields actually use a "idValue" as the output (e.g. MUST)



# 3. MERGE ALL CARD DATA AND REFERENCE DATA -------------------------------
# merge ref_board_lists_2 and board_cards_3 to get names of lists
board_cards_3 <- board_cards_extra_2 %>%
          left_join(ref_board_lists_2, by = "idList") %>%
          select(-idList, -id)

# set the names of the data frame
names(board_cards_3) <- c("card", "description", "type", "status")
        
# merge board_cards_2 and board_cards_extract 2 together using card 'id' values
#board_cards_3 <- merge(x = board_cards_3, y = board_cards_extra_2, by = "id", all = TRUE)

#create output list
Operational_Initatives <- list()

# get additional setting value
Operational_Initatives$board_cards_3 <- board_cards_3 %>%
  mutate(category = str_extract(card, "([aA-zZ0-9])*")) %>%
  mutate(sub_category = str_remove(card, "([aA-zZ0-9])* - ")) %>%
  mutate(status = str_replace(status, "Backlog / Identified", "Backlog"),
         status = str_replace(status, "Blocked / Waiting", "Blocked"),
         status = str_replace(status, "In Progress / Problem Control", "In Progress"),
         status = str_replace(status, "Testing / Error Control", "Testing"))

#
Operational_Initatives$Workload_Viz <- Operational_Initatives$board_cards_3 %>%
  group_by(type, status) %>%
  count() %>%
  filter(status != "Ideas") %>%
  ggplot(aes(x = status, y = n, fill = type)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c( "#DCDCDC", "#00BFC4", "#fdd5b4", "#fbeeb8")) +
  scale_x_discrete(limits = c("Blocked", "Backlog", "In Progress", "Testing", "Completed (wk)", "Completed (old)")) +
  labs(title = "Operational Initiatives Workload", y = "Number of Initiatives", x = "Status", fill = "Workload Type") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 7.5)) 

# how to apply gradient colors to discrete values = https://stackoverflow.com/questions/30352412/how-do-you-create-a-gradient-of-colors-for-a-discrete-variable-in-ggplot2
Settings$color_scale <- seq_gradient_pal("#00BFC4", "#fdd5b4", "Lab")(seq(0,1,length.out = 8))

Operational_Initatives$PROJECTs_Viz <- Operational_Initatives$board_cards_3 %>%
  filter(type == "PROJECT") %>%
  group_by(category, status) %>%
  count() %>%
  filter(status != "Ideas") %>%
  ggplot(aes(x = status, y = n, fill = category)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = Settings$color_scale) +
  scale_x_discrete(limits = c("Blocked", "Backlog", "In Progress", "Testing", "Completed (wk)", "Completed (old)")) +
  labs(title = "Operational Initiatives Workload (PROJECTs)", y = "Number of Initiatives", x = "Status", fill = "Initiative Type") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 7.5))

Operational_Initatives$PROJECTs_table <- Operational_Initatives$board_cards_3 %>%
  filter(type == "PROJECT") %>%
  filter(status != "Ideas") %>%
  select(status, card)


# 4. OUTPUT FINAL TABLE FOR ANALYSIS --------------------------------------
# save file as CSV
write.csv(Operational_Initatives$board_cards_3, set_file_path)


# CLEAN UP ENVIRONMENT ----------------------------------------------------
if(Settings$Debug_Mode != TRUE) {
  rm(board_cards, board_cards_1, board_cards_2, board_cards_3, board_cards_extra, board_cards_extra_1,
     board_cards_extra_2, ref_board_cards_customFields_2, ref_board_lists, ref_board_lists_1, ref_board_lists_2,
     ref_processes_in_production_support, ref_processes_in_stabilisation, Settings)
}
