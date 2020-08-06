#install packages
#install.packages("httr")
#install.packages("jsonlite")
#install.packages("trelloR")
#import libraries
library(httr)
library(jsonlite)
library(stringr)
library(purrr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(trelloR)
library(lubridate)

#Clear all current data
rm(list=ls())

# SETTINGS ----------------------------------------------------------------


# CHECK AUTHENTICATION VALUES EXISTS
if(exists("Settings$Trello_Key") != True) stop("Trello Key value needs to be defined 'Settings$Trello_Key'")
if(exists("Settings$Trello_Token") != True) stop("Trello Token value needs to be defined 'Settings$Trello_Token'")
if(exists("Settings$Trello_SecretKey") != True) stop("Trello Secret Key value needs to be defined 'Settings$Trello_SecretKey'")

#add setting variables
Trello_Key <- Settings$Trello_Key
Trello_Token <- Settings$Trello_Token
Trello_SecretKey <- Settings$Trello_SecretKey

#create key and token string
Trello_Auth <- paste("?key=",Trello_Key,"&token=",Trello_Token, sep = "")
rm(Trello_Key, Trello_Token)

#Create vector of process number ID's based on number of processes
number_of_processes <- 36
process_number_id <- as.character(NULL)
i <- 0
for (i in 1:number_of_processes) {
  process_number_id <- c(process_number_id, paste("P",i," ", sep = "")) 
  i <- i + 1
}
rm(i)


# FUNCTIONS ---------------------------------------------------------------

#create function for calling GET on all lists in a board using ID's
get_cards_in_each_list <- function(x, Auth) {
  GET(paste("https://api.trello.com/1/lists/",x, "/cards", Auth, sep = ""))
}

#create function to count number of process number ID's that match cards in list content
#first get the tag headers using the already created get_tag_headers function
content_for_each_list_in_board_extracted_headers <- map_chr(content_for_each_list_in_board_extracted, get_tag_headers_from_content)
#second apply the already created count_number_of_matches function
content_for_each_list_in_board_extracted_headers_count <- map(process_number_id, count_number_of_matches, strings = content_for_each_list_in_board_extracted_headers)

#create function for getting all tag headers in a list 
get_tag_headers <- function(x) {
  tag_header <- x[["name"]]
}

#create function for getting all tag headers in a content of list
get_tag_headers_from_all_lists <- function(x) {
  #get names of all lists
  names_of_lists <- map_chr(content(x), "name")
  #get ID's of all lists
  ids_of_lists <- map_chr(content(x), "id")
  #call function on all IDs in board
  content_for_each_list_in_board <- map(ids_of_lists, get_cards_in_each_list, Auth = Trello_Auth)
  #extract the content of each list in board
  content_for_each_list_in_board_extracted <- map(content_for_each_list_in_board, content)
  tag_header_list <- as.list(NULL)
  for (list in seq_along(content_for_each_list_in_board_extracted)) {
    tag_header_list[[names_of_lists[list]]] <- map_chr(content_for_each_list_in_board_extracted[[list]], get_tag_headers)
  }
  return(tag_header_list)
}
#create board level function for getting issues resolved and issues raised within a specific date period
test <- GET(paste("https://api.trello.com/1/boards/m7Puvg0U/lists", Trello_Auth, sep = ""))
  #get names of all lists
  names_of_lists <- map_chr(content(test), "name")
  #get ID's of all lists
  ids_of_lists <- map_chr(content(test), "id")
  #call function on all IDs in board
  content_for_each_list_in_board <- map(ids_of_lists, get_cards_in_each_list, Auth = Trello_Auth)
  #extract the content of each list in board
  content_for_each_list_in_board_extracted <- map(content_for_each_list_in_board, content)
  tag_header_list <- as.list(NULL)
  for (list in seq_along(content_for_each_list_in_board_extracted)) {
    tag_header_list[[names_of_lists[list]]] <- map_chr(content_for_each_list_in_board_extracted[[list]], get_tag_headers)
  }
  return(tag_header_list)

#create function for counting number of process matches per ID
count_number_of_matches <- function(x, strings) {
  process_number_id_count <- sum(str_count(string = strings, pattern = x))
}
#create function for extracting count of occurances for all process ID's that match cards on a board (rows are process ID's, columns are list names)


# TESTS -------------------------------------------------------------------


#Get all of the default fields for the Boards resource
#test <- GET(paste("https://api.trello.com/1/boards/5LZ5YvHo", Trello_Auth, sep = ""))

#Get an array of Boards on a user
#test <- GET(paste("https://api.trello.com/1/members/me/boards", Trello_Auth, sep = ""))

#Get an array of Cards on a board (AI Hub User Story Kanban Board)
#test <- GET(paste("https://api.trello.com/1/boards/m7Puvg0U/cards", Trello_Auth, sep = ""))

#Get an array of Lists on a board (AI Hub User Story Kanban Board)
test <- GET(paste("https://api.trello.com/1/boards/m7Puvg0U/lists", Trello_Auth, sep = ""))
  #get number of cards in all lists on a board that match the process number ID
  lists_and_cards_on_board <- get_tag_headers_from_all_lists(test)  
  #get names of all lists
  names_of_lists <- map_chr(content(test), "name")  
  #get tag headers of all cards in list
  cards_in_list <- map_chr(content(test), get_tag_headers)
  #count number of matches per process number ID
  #create dummy table and set names
  table_of_processes_matching_cards_in_lists <- data.frame(matrix(ncol = length(names_of_lists), nrow = number_of_processes))
  colnames(table_of_processes_matching_cards_in_lists) <- names_of_lists
  #
  for (list in seq_along(cards_in_list)) {
    table_of_processes_matching_cards_in_lists[[names_of_lists[list]]] <- map_int(process_number_id, count_number_of_matches, strings = lists_and_cards_on_board[[list]])
  }
  #create In Progress column
  table_of_processes_matching_cards_in_lists_2 <- table_of_processes_matching_cards_in_lists %>%
    mutate("RUN - In Progress" = `Backlog` + `Development - In Progress` + `Development - Complete` + `Testing` + `Unit Testing - Complete` + `Deployment - Ready` + `Deployment - Complete`)
  #remove unnessary columns
  table_of_processes_matching_cards_in_lists_3 <- data.frame(process_number_id, table_of_processes_matching_cards_in_lists_2$`RUN - In Progress`, table_of_processes_matching_cards_in_lists_2$`RUN - Complete`)
  colnames(table_of_processes_matching_cards_in_lists_3) <- c("ProcessID", "In Progress", "Complete")
  #set ProcessID as a factor with levels for the Plot
  table_of_processes_matching_cards_in_lists_3$ProcessID <- factor(table_of_processes_matching_cards_in_lists_3$ProcessID, levels = table_of_processes_matching_cards_in_lists_3$ProcessID)
  #filter out processes so only the processes in stablisation period are visible
  processes_in_stabalisation <- c(1,2,3,6,8,7,9,10,11,12,14,15,19,20,21,22,23,24,26,27,28,29,30,31,32,33,34,35,36,13)
  table_of_processes_matching_cards_in_lists_3 <- table_of_processes_matching_cards_in_lists_3[processes_in_stabalisation,]
  #melt table for ggplot
  table_of_processes_matching_cards_in_lists_4 <- melt(table_of_processes_matching_cards_in_lists_3, id.vars = "ProcessID")
  colnames(table_of_processes_matching_cards_in_lists_4) <- c("ProcessID","Status", "value")
  #create visualisation
  D <- ggplot(table_of_processes_matching_cards_in_lists_4, aes(x = ProcessID, y = value, fill = Status)) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = c("#ff392e","GREY")) +
    labs(title = "Production Process Trello Tickets", x = "Production Processes", y = "Total number of Tickets") +
    theme(plot.title = element_text(hjust = 0.5))
  
  #Save Total number of trello tickets report  
  ggsave(filename = "C:/Users/tbun2893/Downloads/Process Performance Reports/Merged/trello_tickets_report.pdf", plot = D) 
  
#Get an array of Cards on a List (AI Hub User STory Kanban Board, RUN - Complete)
test <- GET(paste("https://api.trello.com/1/lists/5bc53441a9922156a2ed3f51/cards", Trello_Auth, sep = ""))
  #get tag headers of all cards in list
  cards_in_list <- map_chr(content(test), get_tag_headers)
  #count number of matches per process number ID
  number_of_matches_per_process <- map(process_number_id, count_number_of_matches, strings = cards_in_list)
  number_of_matches_per_process
  

# SOURCE ------------------------------------------------------------------

  
#Trello API Introduction sourced from https://developers.trello.com/docs/api-introduction


# TEST Area ---------------------------------------------------------------

Run_Complete <- "5bc53441a9922156a2ed3f51"
  
Cards_on_list <- GET(paste("https://api.trello.com/1/lists/", Run_Complete, "/cards", Trello_Auth, sep = ""))

Cards_on_list_content <- content(Cards_on_list)

Cards_on_list_Date_Time <- map_chr(Cards_on_list_content, "dateLastActivity")

Cards_on_list_Date <- str_sub(Cards_on_list_Date_Time, 1, 10)

Cards_on_list_Date <- ymd(Cards_on_list_Date)

Cards_on_list_Date_Unique <- unique(Cards_on_list_Date)

count_matches <- function(match) {
  match_counter <- 0
  for (index in seq_along(Cards_on_list_Date)) {
    if (as.character(match) == as.character(Cards_on_list_Date[index])) {
      match_counter <- match_counter + 1
    } else NULL
  }
  return(match_counter)
}
Cards_on_list_Date_Count <- map(Cards_on_list_Date_Unique, count_matches)

Cards_on_list_df <- as.data.frame(cbind(as.character(Cards_on_list_Date_Unique), Cards_on_list_Date_Count))

colnames(Cards_on_list_df) <- c("Date", "TicketCount")

Cards_on_list_df$Date <- ymd(Cards_on_list_df$Date)
Cards_on_list_df$TicketCount <- as.numeric(Cards_on_list_df$TicketCount)

ggplot(Cards_on_list_df, aes(y = TicketCount, x = Date, group = 1)) + 
  geom_line(color = "BLUE", size = 1) +
  geom_text(label = Cards_on_list_df$TicketCount, size = 3, color = "#696969", nudge_y = 5) +
  labs(title = paste("Production Stabilisation Overtime", sep = ""), size = 2)






