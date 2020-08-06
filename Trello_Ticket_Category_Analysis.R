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

# SETTINGS ----------------------------------------------------------------

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


#create key and token string
Trello_Auth <- paste("key=",Trello_Key,"&token=",Trello_Token, sep = "")
rm(Trello_Key, Trello_Token)

#Create vector of process number ID's based on number of processes
number_of_processes <- 100
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
  GET(paste("https://api.trello.com/1/lists/",x, "/cards?", Auth, sep = ""))
}

#create function for getting all tag headers in a list 
get_tag_headers <- function(x) {
  tag_header <- x[["name"]]
}

#create function for counting number of process matches per ID
count_number_of_matches <- function(x, strings) {
  process_number_id_count <- sum(str_count(string = strings, pattern = x))
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


# 1. EXTRACT TRELLO DATA --------------------------------------------------

#Get an array of Lists on a board (AI Hub User Story Kanban Board)
test <- GET(paste("https://api.trello.com/1/boards/m7Puvg0U/lists?", Trello_Auth, sep = ""))


# 2. CLEAN AND TRANSFORM TRELLO DATA --------------------------------------

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
for (list in seq_along(cards_in_list)) {
  table_of_processes_matching_cards_in_lists[[names_of_lists[list]]] <- map_int(process_number_id, count_number_of_matches, strings = lists_and_cards_on_board[[list]])
}

#create In Progress column
table_of_processes_matching_cards_in_lists_2 <- table_of_processes_matching_cards_in_lists %>%
  mutate("RUN - In Progress" = `Development - In Progress` + `Development - Complete` + `Testing` + `Unit Testing - Complete` + `Deployment - Ready` + `Deployment - Complete`)

#remove unnessary columns
table_of_processes_matching_cards_in_lists_3 <- data.frame(process_number_id, table_of_processes_matching_cards_in_lists_2$`RUN - In Progress`, table_of_processes_matching_cards_in_lists_2$`RUN - Complete`)
colnames(table_of_processes_matching_cards_in_lists_3) <- c("ProcessID", "In Progress", "Complete")

#set ProcessID as a factor with levels for the Plot
table_of_processes_matching_cards_in_lists_3$ProcessID <- factor(table_of_processes_matching_cards_in_lists_3$ProcessID, levels = table_of_processes_matching_cards_in_lists_3$ProcessID)

#filter out processes so only the processes in stablisation period are visible
processes_in_stabalisation <- c(1:52)
table_of_processes_matching_cards_in_lists_3 <- table_of_processes_matching_cards_in_lists_3[processes_in_stabalisation,]

#melt table for ggplot
table_of_processes_matching_cards_in_lists_4 <- melt(table_of_processes_matching_cards_in_lists_3, id.vars = "ProcessID")
colnames(table_of_processes_matching_cards_in_lists_4) <- c("ProcessID","Status", "value")


# 3. VISUALISE TRELLO DATA ------------------------------------------------

#create visualisation
D <- table_of_processes_matching_cards_in_lists_4 %>%
  filter(value != 0) %>%
  ggplot(aes(x = ProcessID, y = value, fill = Status)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("#ff392e","GREY")) +
  labs(title = "Production Process Trello Tickets", x = "Production Processes", y = "Total number of Tickets") +
  theme(plot.title = element_text(hjust = 0.5))

D