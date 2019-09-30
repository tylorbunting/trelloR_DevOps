# 0. SETUP ENVIRONMENT ----------------------------------------------------
# INSTALL APPROPRIATE PACKAGES
library("trelloR")
library("tidyr")
library("httpuv")
library("purrr")

# SETUP AUTHENTICATION VALUES
Trello_Key <- "4f0a4fcacc9b53edd8b79942caa027a3"
Trello_Token <- "85422bf57716c09c61b1043a2ba52198320d5993842b52a5e849fed26344f1a9"
Trello_SecretKey <- "e3512e11ac64fd3bd927429a5294506d1d7be2b87236cd011c19eee9f6bc833b"
if(exists("TrelloR_Token") != TRUE) TrelloR_Token <- trello_get_token(Trello_Key, Trello_SecretKey)

# create key and token string
Trello_Auth <- paste("key=",Trello_Key,"&token=",Trello_Token, sep = "")

# SETUP INPUT VALUES
User_Story_Board_url <- "https://trello.com/b/m7Puvg0U/ai-hub-user-story-kanban-board"

# SET CUSTOMFIELD IDS
CustomFields_variables <- list(
  Date_Raised = "5c9d92bb98be778d4583c9ef",
  Start_Date = "5c5a1f2925edda7fd322f890",
  End_Date = "5c5a1f3135e79935255e1945",
  Dev_Effort = "5d491bc1f4505576cf5fa483",
  Test_Effort = "5d491c1788a5af71581e130a",
  Incident_Category = "5d491cba205bb14d05f567a6"
)

# SETUP VARIOUS FUNCTIONS
# labels to capture for analysis (assumption is that tickets only have one label assigned)
labels_for_analysis <- c("Incident", "Request", "Enhancement")
# create function to extract first label name found out of labels_for_analysis input variable
get_label_name <- function(x) {
  if(length(x[["labels"]]) > 0) {
    for(index in seq_along(x[["labels"]])) {
      if(as.character(x[["labels"]][[index]][["name"]]) %in% labels_for_analysis) {
        output <- x[["labels"]][[index]][["name"]]
        break
      } else output <- "unknown"
    }
  } else output <- "unknown"
  return(output)
}
get_label_name_trelloR <- function(x) {
  if(nrow(x) > 0 ) {
    for(index in seq_along(x[["name"]])) {
      if(x[["name"]][[index]] %in% labels_for_analysis) {
        output <- as.character(x[["name"]][[index]])
        break
      } else output <- "unknown"
    }
  } else output <- "unknown"
  return(output)
}
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


# 1. GET TRELLO DATA ------------------------------------------------------
# get data using the trellR package method
if(exists("Data_1") != TRUE) {
User_Story_Board_id <- get_id_board(User_Story_Board_url, token = TrelloR_Token)
User_Story_Board_lists <- get_board_lists(User_Story_Board_id, token = TrelloR_Token)
User_Story_Board_cards <- get_board_cards(User_Story_Board_id, TrelloR_Token)
User_Story_Board_members <- get_board_members(User_Story_Board_id, TrelloR_Token)
User_Story_Board_card_members <- map_df(User_Story_Board_cards$id, get_card_members, token = TrelloR_Token)
# get an array of Cards on a board (User Story Kanban Board) and include custom fields SOURCE: https://developers.trello.com/docs/getting-started-custom-fields 
User_Story_Board_cards_with_customFields <- GET(paste("https://api.trello.com/1/boards/m7Puvg0U/?fields=name&cards=all&card_fields=name&customFields=true&card_customFieldItems=true&", Trello_Auth, sep = ""))
User_Story_Board_cards_with_customFields <- content(User_Story_Board_cards_with_customFields)
# save data for key process step
Data_1 <- list(User_Story_Board_id = User_Story_Board_id,
               User_Story_Board_lists = User_Story_Board_lists,
               User_Story_Board_cards = User_Story_Board_cards,
               User_Story_Board_members = User_Story_Board_members,
               User_Story_Board_card_members = User_Story_Board_card_members,
               User_Story_Board_cards_with_customFields = User_Story_Board_cards_with_customFields$cards,
               User_Story_Board_customFields = User_Story_Board_cards_with_customFields$customFields)
}


# 2. SET CUSTOM FIELD IDS FOR EACH CARD & REMOVE UNEEDED DATA -----------------------------------
Data_2 <- Data_1

# Card data with custom fields
Data_2$User_Story_Board_cards_with_customFields <- map_df(Data_2$User_Story_Board_cards_with_customFields, function(x) {
  data.frame(
    cardID = as.character(extract(x, "id")),
    cardName = as.character(extract(x, "name")),
    cardDevEffort = get_customField_dropdown_values(x, CustomFields_variables$Dev_Effort),
    cardTestEffort = get_customField_dropdown_values(x, CustomFields_variables$Test_Effort),
    cardIncidentCategory = get_customField_dropdown_values(x, CustomFields_variables$Incident_Category),
    cardDateRaised = get_customField_date_values(x, CustomFields_variables$Date_Raised),
    cardDateStarted = get_customField_date_values(x, CustomFields_variables$Start_Date),
    cardDateEnded = get_customField_date_values(x, CustomFields_variables$End_Date)
  )
})

# Card data with normal values / non custom fields
Data_2$User_Story_Board_cards <- data.frame(
    cardID = as.character(Data_2$User_Story_Board_cards[["id"]]),
    cardName = as.character(Data_2$User_Story_Board_cards[["name"]]),
    cardDescription = as.character(Data_2$User_Story_Board_cards[["desc"]]),
    listID = as.character(Data_2$User_Story_Board_cards[["idList"]]),
    cardurl = as.character(Data_2$User_Story_Board_cards[["url"]]),
    cardLabels = map_chr(Data_2$User_Story_Board_cards[["labels"]], get_label_name_trelloR)
  )

# list data for board
Data_2$User_Story_Board_lists <- data.frame(
  listID = as.character(Data_2$User_Story_Board_lists$id),
  listName = as.character(Data_2$User_Story_Board_lists$name),
  listPosition = as.integer(Data_2$User_Story_Board_lists$pos)
)

# all cards and members (one card to many members)
Data_2$User_Story_Board_card_members <- data.frame(
  cardID = as.character(Data_2$User_Story_Board_card_members$id),
  memberFullName = as.character(Data_2$User_Story_Board_card_members$fullName),
  memberInitials = as.character(Data_2$User_Story_Board_card_members$initials),
  userName = as.character(Data_2$User_Story_Board_card_members$username)
)

# all DropDown ("list" type) customFields values for mapping values to customField idValues in User_Story_Board_cards_with_customFields data frame
Data_2$User_Story_Board_DropDown_customFields <- map_df(Data_2$User_Story_Board_customFields, function(x) {
  data.frame(
    
  )
})

# print names of all key variables to console
print(names(Data_2))


# 3. MERGE MAJOR DATASETS TO CREATE MASTER DATA FOR BOARD ---------------------------
Data_3 <- Data_2

Data_3$



# print names of all key variables to console
print(names(Data_3))
