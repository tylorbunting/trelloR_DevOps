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
library(pryr) #used to analyse S3 generics and methods

# CHECK AUTHENTICATION VALUES EXISTS
if(exists("Settings$Trello_Key") != True) stop("Trello Key value needs to be defined 'Settings$Trello_Key'")
if(exists("Settings$Trello_Token") != True) stop("Trello Token value needs to be defined 'Settings$Trello_Token'")
if(exists("Settings$Trello_SecretKey") != True) stop("Trello Secret Key value needs to be defined 'Settings$Trello_SecretKey'")

#add setting variables
Trello_Key <- Settings$Trello_Key
Trello_Token <- Settings$Trello_Token
Trello_SecretKey <- Settings$Trello_SecretKey

# add label ID setting variables
date_raised_id <- "5c9d92bb98be778d4583c9ef"
date_started_id <-"5c5a1f2925edda7fd322f890"
date_ended_id <- "5c5a1f3135e79935255e1945"
# add label for number of days to analyse
number_of_days <- 30

# create key and token string
Trello_Auth <- paste("key=",Trello_Key,"&token=",Trello_Token, sep = "")
rm(Trello_Key, Trello_Token)

# labels to capture for analysis (assumption is that tickets only have one label assigned) 
labels_for_analysis <- c("Enhancement", "Incident", "Request")


# 1. EXTRACT DATA OF ALL CARDS ON BOARD --------------------------------------------------------------

# get an array of Lists on a board (AI Hub User Story Kanban Board)
ref_board_lists <- GET(paste("https://api.trello.com/1/boards/m7Puvg0U/lists?", Trello_Auth, sep = ""))
# get all content for each list
ref_board_lists_1 <- content(ref_board_lists)

# get an array of Cards on a board (AI Hub User Story Kanban Board) and include custom fields SOURCE: https://developers.trello.com/docs/getting-started-custom-fields 
board_cards <- GET(paste("https://api.trello.com/1/boards/m7Puvg0U/?fields=name&cards=all&card_fields=name&customFields=true&card_customFieldItems=true&", Trello_Auth, sep = ""))
# get all content for each card in the board
board_cards_1 <- content(board_cards)

# get an array of Cards on a board (AI Hub User Story Kanban Board) and include extra card data (e.g. description, members etc.)
board_cards_extra <- GET(paste("https://api.trello.com/1/boards/m7Puvg0U/cards/all?", Trello_Auth, sep = ""))
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

# create function to extract customField values from cards
get_customField_values <- function(x, customField_id) {
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

# apply function to get customField date values as well as other details of cards on the board (SOURCE: https://stackoverflow.com/questions/45310166/extracting-data-from-nested-list-in-r)
board_cards_2 <- map_df(board_cards_2[["cards"]], function(x){
  data.frame(
    id = as.character(extract(x, "id")),
    name = as.character(extract(x, "name")),
    date_raised = get_customField_values(x = x, customField_id = date_raised_id),
    date_started = get_customField_values(x = x, customField_id = date_started_id),
    date_ended = get_customField_values(x = x, customField_id = date_ended_id)
  )
})


# 3. MERGE ALL CARD DATA AND REFERENCE DATA -------------------------------
# merge board_cards_2 and board_cards_extract 2 together using card 'id' values
board_cards_3 <- merge(x = board_cards_2, y = board_cards_extra_2, by = "id", all = TRUE)

# merge ref_board_lists_2 and board_cards_3 to get names of lists
board_cards_3 <- merge(x = board_cards_3, y = ref_board_lists_2, by = "idList", all = TRUE )

# get additional setting value
board_cards_3 <- board_cards_3 %>%
  mutate(bucket = str_extract(name.x, "([aA-zZ0-9])*"))

# 4. CLEAN AND TRANSFORM VALUES FOR ANALYSIS ------------------------------
# create data frame for clean and transform tasks
board_cards_4 <- data.frame(
  bucket = board_cards_3$bucket,
  name = board_cards_3$name.x,
  description = board_cards_3$desc,
  list = board_cards_3$name,
  label = board_cards_3$label_type,
  date_raised = board_cards_3$date_raised,
  date_started = board_cards_3$date_started,
  date_ended = board_cards_3$date_ended,
  stringsAsFactors = FALSE
)

# extract yyyy-mm-dd values from all date fields and ignore unnessary information
board_cards_4$date_raised <- str_extract(board_cards_4$date_raised, "[0-9-]{10}")
board_cards_4$date_started <- str_extract(board_cards_4$date_started, "[0-9-]{10}")
board_cards_4$date_ended <- str_extract(board_cards_4$date_ended, "[0-9-]{10}")

# set date fields as date data types
board_cards_4$date_raised <- ymd(board_cards_4$date_raised)
board_cards_4$date_started <- ymd(board_cards_4$date_started)
board_cards_4$date_ended <- ymd(board_cards_4$date_ended)

# set appropriate factor fields
board_cards_4$bucket <- as.factor(board_cards_4$bucket)
board_cards_4$list <- as.factor(board_cards_4$list)
board_cards_4$label <- as.factor(board_cards_4$label)

# filter out backlog and blocked tickets
board_cards_4 <- board_cards_4 %>%
  filter(!list == "Backlog") %>%
  filter(!list == "Blocked / Waiting")

# 5. CREATE ANALYSIS OUTPUTS ----------------------------------------------
# create data frames for transforms

board_cards_5 <- board_cards_4

# add avg_wait and avg_resolve times
board_cards_5 <- board_cards_5 %>%
  mutate(avg_wait = difftime(date_started, date_raised, units = "days")) %>%
  mutate(avg_resolution = difftime(date_ended, date_started, units = "days"))

# melt date values for visualisation (SOURCE: http://www.datasciencemadesimple.com/melting-casting-r/)
board_cards_5 <- melt(board_cards_5, id = c("bucket", "name", "description", "list", "label", "avg_wait", "avg_resolution"), variable.name = c("date_type"), value.name = "date")

# add weekly and monthly values
board_cards_5 <- board_cards_5 %>%
  mutate(week_of_year = week(date)) %>%
  mutate(month_of_year = month(date, label = TRUE))

# create dataframe for visualising ticket count weekly
board_cards_5_weekly_count <- board_cards_5 %>%
  filter(label %in% labels_for_analysis) %>%
  filter(!is.na(date)) %>%
  filter(date > today() - number_of_days) %>%
  filter(date < today()) %>%
  filter(date_type %in% c("date_raised", "date_ended")) %>%
  filter(!week_of_year == week(today())) %>%
  group_by(date_type, week_of_year) %>%
  count()

board_cards_5_weekly_count_incidents <- board_cards_5 %>%
  filter(label == "Incident") %>%
  filter(!is.na(date)) %>%
  filter(date > today() - number_of_days) %>%
  filter(date < today()) %>%
  filter(date_type %in% c("date_raised", "date_ended")) %>%
  filter(!week_of_year == week(today())) %>%
  group_by(date_type, week_of_year) %>%
  count()

# melt date values for visualisation (SOURCE: http://www.datasciencemadesimple.com/melting-casting-r/)
board_cards_5 <- melt(board_cards_5, id = c("bucket", "name", "description", "list", "label", "date_type", "date", "week_of_year", "month_of_year"), variable.name = c("processing_time_type"), value.name = "processing_time_avg")

# create dataframe for visualising ticket average processing times weekly
board_cards_5_weekly_avg_time <- board_cards_5 %>%
  filter(label %in% labels_for_analysis) %>%
  #filter(processing_time_type == "avg_resolution") %>%
  filter(!is.na(processing_time_avg)) %>%
  filter(date > today() - number_of_days) %>%
  filter(date < today()) %>%
  filter(!week_of_year == week(today())) %>%
  group_by(processing_time_type, week_of_year) %>%
  summarise(n = mean(processing_time_avg))

board_cards_5_weekly_avg_time_incidents <- board_cards_5 %>%
  filter(label == "Incident") %>%
  filter(!is.na(processing_time_avg)) %>%
  filter(date > today() - number_of_days) %>%
  filter(date < today()) %>%
  filter(!week_of_year == week(today())) %>%
  group_by(processing_time_type, week_of_year) %>%
  summarise(n = mean(processing_time_avg))

# visualise count plot
board_cards_5_weekly_count_plot <- board_cards_5_weekly_count %>% 
  ggplot(aes(x = week_of_year, y = n, color = date_type)) + 
  geom_line(size = 1) +
  #geom_text(label = Data$Volume, size = 3, color = "#696969", nudge_y = 15) +
  #stat_smooth(linetype = "longdash", color = "#696969", method = "loess", formula = "y ~ x") +
  ggtitle(paste("Number of All Trello Tickets Raised and Ended", sep = "")) +
  labs(y = "ticket count", x = "week of year", color = "Date Type")  +
  expand_limits(y = 0) +
  scale_y_continuous(limits = c(0, 100))

# visualise count plot for incidents
board_cards_5_weekly_count_plot_incidents <- board_cards_5_weekly_count_incidents %>% 
  ggplot(aes(x = week_of_year, y = n, color = date_type)) + 
  geom_line(size = 1) +
  #geom_text(label = Data$Volume, size = 3, color = "#696969", nudge_y = 15) +
  #stat_smooth(linetype = "longdash", color = "#696969", method = "loess", formula = "y ~ x") +
  ggtitle(paste("Number of Incident Trello Tickets Raised and Ended", sep = "")) +
  labs(y = "ticket count", x = "week of year", color = "Date Type")  +
  expand_limits(y = 0)  +
  scale_y_continuous(limits = c(0, 100))

# visualise average processing time plot
board_cards_5_weekly_avg_time_plot <- board_cards_5_weekly_avg_time %>% 
  ggplot(aes(y = n, x = week_of_year, fill = processing_time_type)) +
  geom_bar(stat = 'identity') +
  labs(title = paste("Average Processing Time for All Tickets Raised and Ended", sep = ""), x = "week of year", y = "days (mean)", fill = "Processing Type")  +
  scale_y_continuous(limits = c(0, 8))

# visualise average processing time plot for incidents
board_cards_5_weekly_avg_time_plot_incidents <- board_cards_5_weekly_avg_time_incidents %>% 
  ggplot(aes(y = n, x = week_of_year, fill = processing_time_type)) +
  geom_bar(stat = 'identity') +
  labs(title = paste("Average Processing Time for Incident Tickets Raised and Ended", sep = ""), x = "week of year", y = "days (mean)", fill = "Processing Type") +
  scale_y_continuous(limits = c(0, 8))
#theme(legend.position = "bottom", legend.text = element_text(size = 8))

#Setup layout for Volume (A) and Completion Rate (B) Plots
#(Sourced from "https://stackoverflow.com/questions/13294952/left-align-two-graph-edges-ggplot")
gA <- ggplotGrob(board_cards_5_weekly_count_plot)
gB <- ggplotGrob(board_cards_5_weekly_count_plot_incidents)
gC <- ggplotGrob(board_cards_5_weekly_avg_time_plot)
gD <- ggplotGrob(board_cards_5_weekly_avg_time_plot_incidents)

maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5], gC$widths[2:5], gD$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
gC$widths[2:5] <- as.list(maxWidth)
gD$widths[2:5] <- as.list(maxWidth)

#Display Volume and Completion Rate Plots
plot_Trello_Ticket_Analysis_incidents <- grid.arrange(gA, gB, gC, gD, ncol=1,
                                            top = textGrob(paste("Trello Ticket Analysis Over ", number_of_days, " Days", sep = ""),gp=gpar(fontsize=20,font=1)))

# get variables for RMarkdown reporting
var_recent_week <- max(board_cards_5_weekly_count$week_of_year)
var_tickets_raised <- board_cards_5_weekly_count %>%
                      filter(week_of_year == var_recent_week) %>%
                      filter(date_type == "date_raised")
var_tickets_closed <- board_cards_5_weekly_count %>%
                      filter(week_of_year == var_recent_week) %>%
                      filter(date_type == "date_ended")

