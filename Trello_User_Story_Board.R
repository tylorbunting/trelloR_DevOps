# 0. SETUP ENVIRONMENT ----------------------------------------------------
#
# The key output of this script will be a table with the below variables and various plots
# card variables = id, name, desciption, status, type, week_complete, day_complete, raised_date, start_date, end_date, member, member_count, dev_effort, test_effort, total_effort, incident_category
#

# attempt to install all require packages
source("C:/Users/tbun2893/Documents/GitHub/trelloR_DevOps/_package_manager.R")

# get all functions needed for TrelloR_custom
source("C:/Users/tbun2893/Documents/GitHub/trelloR_DevOps/_functions.R")

# INSTALL APPROPRIATE PACKAGES
library("trelloR")
library("tidyr")
library("httpuv")
library("purrr")
library("scales")
library("lubridate")

# create settings list
if(exists("Settings") != TRUE) Settings <- list()

# Debug mode?
Settings$Debug_Mode <- TRUE

# SETUP AUTHENTICATION VALUES
Settings$Trello_Key <- "f9170c56bae48f7f78916d54411f90b9"
Settings$Trello_Token <- "dabd2fd76f594ade872a11b89c5d1bbde63c6d5c7c1adf08460704b86fd5c29b"
Settings$Trello_SecretKey <- "dcfae097b265928f924a7c6772dc05d2dcbb92f32895dd3f5bcce141bbe263b5"
if(exists("Settings$TrelloR_Token") != TRUE) Settings$TrelloR_Token <- trello_get_token(Settings$Trello_Key, Settings$Trello_SecretKey)

# create key and token string
Settings$Trello_Auth <- paste("key=",Settings$Trello_Key,"&token=",Settings$Trello_Token, sep = "")

# SETUP INPUT VALUES
Settings$Board_url <- "https://trello.com/b/m7Puvg0U/ai-hub-user-story-kanban-board"

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
Settings$labels_for_analysis <- c("Incident", "Internal Request", "External Request", "Enhancement")


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
  Data_1$Board_cards_with_customFields <- GET(paste("https://api.trello.com/1/boards/m7Puvg0U/?fields=name&cards=all&card_fields=name&customFields=true&card_customFieldItems=true&", Settings$Trello_Auth, sep = ""))
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
    name = as.character(extract(x, "name")),
    dev_effort_id = get_customField_dropdown_ids(x, Settings$CustomFields_variables$Dev_Effort),
    test_effort_id = get_customField_dropdown_ids(x, Settings$CustomFields_variables$Test_Effort),
    incident_category_id = get_customField_dropdown_ids(x, Settings$CustomFields_variables$Incident_Category),
    date_raised = get_customField_date_values(x, Settings$CustomFields_variables$Date_Raised),
    date_started = get_customField_date_values(x, Settings$CustomFields_variables$Start_Date),
    date_ended = get_customField_date_values(x, Settings$CustomFields_variables$End_Date),
    days_blocked = get_customField_number_values(x, Settings$CustomFields_variables$days_blocked)
  )
})

# Card data with "values" for DROPDOWN type customField as well as DATE type customFields
Data_2$Board_cards_with_customFields <- data.frame(
  card_id = as.character(Data_2$Board_cards_with_customFields[["card_id"]]),
  name = as.character(Data_2$Board_cards_with_customFields[["name"]]),
  dev_effort = map_chr(Data_2$Board_cards_with_customFields[["dev_effort_id"]], get_customField_dropdown_value, Data_2$Board_customFields),
  test_effort = map_chr(Data_2$Board_cards_with_customFields[["test_effort_id"]], get_customField_dropdown_value, Data_2$Board_customFields),
  incident_category = map_chr(Data_2$Board_cards_with_customFields[["incident_category_id"]], get_customField_dropdown_value, Data_2$Board_customFields),
  date_raised = as.character(Data_2$Board_cards_with_customFields[["date_raised"]]),
  date_started = as.character(Data_2$Board_cards_with_customFields[["date_started"]]),
  date_ended = as.character(Data_2$Board_cards_with_customFields[["date_ended"]]),
  days_blocked = as.character(Data_2$Board_cards_with_customFields[["days_blocked"]])
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
Data_3$Board_cards <- Data_3$Board_cards %>%
  select(-name) %>%
  left_join(Data_3$Board_cards_with_customFields, by = "card_id")


# 4. MAKE FINAL CHANGES TO TABLE BEFORE VISUALISING -----------------------
Data_4 <- Data_3

# update the three date related customField values from "factors" to "date" types
Data_4$Board_cards$date_raised <- ymd(str_extract(Data_4$Board_cards$date_raised, "[0-9-]{10}"))
Data_4$Board_cards$date_started <- ymd(str_extract(Data_4$Board_cards$date_started, "[0-9-]{10}"))
Data_4$Board_cards$date_ended <- ymd(str_extract(Data_4$Board_cards$date_ended, "[0-9-]{10}"))

# divide DEV and TEST effort by member_count to reflect actual effort for DEV and TEST per row in the "Board_cards" table 
Data_4$Board_cards <- Data_4$Board_cards %>%
  mutate(dev_effort = round(as.numeric(as.character(dev_effort)) / as.numeric(as.character(member_count)), 2)) %>%
  mutate(test_effort = round(as.numeric(as.character(test_effort)) / as.numeric(as.character(member_count)), 2))

# update DEV and TEST effort so that the NA's are represented by 0's
Data_4$Board_cards$dev_effort <- replace_na(Data_4$Board_cards$dev_effort, 0)
Data_4$Board_cards$test_effort <- replace_na(Data_4$Board_cards$test_effort, 0)

# create total_effort variables
Data_4$Board_cards <- Data_4$Board_cards %>%
  mutate(total_effort = round(dev_effort + test_effort, 2))

# add avg_wait and avg_resolve times
Data_4$Board_cards <- Data_4$Board_cards %>%
  mutate(avg_wait = difftime(date_started, date_raised, units = "days")) %>%
  mutate(avg_resolution = difftime(date_ended, date_started, units = "days"))

# add week_ended, day_ended, month_name_ended, month_number_ended, year_ended  variables
Data_4$Board_cards <- Data_4$Board_cards %>%
  mutate(week_ended = week(date_ended)) %>%
  mutate(day_ended = wday(date_ended, label = TRUE)) %>%
  mutate(month_name_ended = month(date_ended, label = TRUE)) %>%
  mutate(month_number_ended = month(date_ended, label = FALSE)) %>%
  mutate(year_ended = year(date_ended))

# add avg_wait and avg_resolve times
Data_4$Board_cards <- Data_4$Board_cards %>%
  mutate(avg_wait = difftime(date_started, date_raised, units = "days")) %>%
  mutate(avg_resolution = difftime(date_ended, date_started, units = "days"))

# change wait, resolution, and blocked values to numerics for math operations
Data_4$Board_cards <- Data_4$Board_cards %>%
  mutate(avg_wait = as.numeric(avg_wait)) %>%
  mutate(avg_resolution = as.numeric(avg_resolution)) %>%
  mutate(days_blocked = as.numeric(as.character(days_blocked)))

# whatever the "avg_resolution" time is, remove the number of "days_blocked", and set negative "avg_resolution" values to NA
Data_4$Board_cards <- Data_4$Board_cards %>%
  mutate(avg_resolution = replace_na(avg_resolution, 0) - replace_na(days_blocked, 0)) %>%
  mutate(avg_resolution = as.numeric(str_replace(as.character(avg_resolution), "-\\d*|^0", "NA")))

# add bucket variable
Data_4$Board_cards <- Data_4$Board_cards %>%
  mutate(bucket = str_extract(name, "(^.*?)(?= -)"))

# update missing values in Incident_Category column
Data_4$Board_cards <- Data_4$Board_cards %>%
  mutate(incident_category = replace_na(as.character(incident_category), "NA"))

# 5. VISUALISE DATA -------------------------------------------------------
Data_5 <- Data_4

if(exists("Plots") != TRUE) Plots <- list()


# 5.0. VISUALISATION OPTIONS ----------------------------------------------
Plots$Options <- list()

# the labels that will be filtered for the visualisations
Plots$Options$labels_for_analysis <- c("Enhancement", "Internal Request", "External Request", "Incident")

# the people that will be filtered for the visualisations
Plots$Options$people_for_analysis <- c("Tylor Bunting", "Sean Xiang", "Victoria Lu", "ambitkumar","Bianca De Jesus")

# set number of weeks to analysis
Plots$Options$weeks_to_analyse <- 4

# set min weeks number for analysis
if(week(today()) < Plots$Options$weeks_to_analyse){
  Plots$Options$min_week_to_analyse <- 0
} else {
  Plots$Options$min_week_to_analyse <- week(today()) - Plots$Options$weeks_to_analyse
}
  
# the year for analysis
Plots$Options$year_for_anlysis <- 2020

# the facet_count_for_analysis
Plots$Options$row_count_for_analysis <- 4

# create table specific for run team
Data_5$Board_cards_Run <- Data_5$Board_cards %>% 
  #filter(label %in% Plots$Options$labels_for_analysis) %>%
  filter(fullname %in% Plots$Options$people_for_analysis) %>%
  filter(!is.na(week_ended))

# how to apply gradient colors to discrete values = https://stackoverflow.com/questions/30352412/how-do-you-create-a-gradient-of-colors-for-a-discrete-variable-in-ggplot2
Plots$Options$color_scale <- seq_gradient_pal("#00BFC4", "#fdd5b4", "Lab")(seq(0,1,length.out = 8))

# 5.1. TICKET COUNT WEEKLY LINE GRAPH ------------------------------------------------

# melt date values for visualisation (SOURCE: http://www.datasciencemadesimple.com/melting-casting-r/)
Data_5$Board_cards_Run_Melt_Dates <- Data_5$Board_cards_Run %>%
  select(-username, -dev_effort, -test_effort, -incident_category, -bucket, -days_blocked) %>%
  melt(id = c("card_id",
              "name", 
              "url",
              "description", 
              "list_name", 
              "list_position",
              "fullname",
              "member_count",
              "total_effort",
              "label",
              "avg_wait",
              "avg_resolution",
              "week_ended", 
              "day_ended",
              "year_ended", 
              "month_name_ended", 
              "month_number_ended"), 
       variable.name = c("date_type"), 
       value.name = "date")

# total count of all tickets (Enhancements, Request, Incidents)
Data_5$Weekly_Count <- Data_5$Board_cards_Run_Melt_Dates %>%
  filter(year(date) == Plots$Options$year_for_anlysis) %>%
  filter(date_type %in% c("date_raised", "date_ended")) %>%
  group_by(date_type, week_ended) %>%
  count()
Plots$Weekly_Count <- Data_5$Weekly_Count %>%
  ggplot(aes(x = week_ended, y = n, color = date_type)) + 
  geom_line(size = 1) +
  #geom_text(label = Data$Volume, size = 3, color = "#696969", nudge_y = 15) +
  #stat_smooth(linetype = "longdash", color = "#696969", method = "loess", formula = "y ~ x") +
  ggtitle(paste("Number of All Trello Tickets Raised and Ended", sep = "")) +
  labs(y = "ticket count", x = "week of year", color = "Date Type")  +
  expand_limits(y = 0) +
  scale_y_continuous(limits = c(0, 140))

# visualise count plot for incidents
Data_5$Weekly_Count_Incidents <- Data_5$Board_cards_Run_Melt_Dates %>%
  filter(year(date) == Plots$Options$year_for_anlysis) %>%
  filter(label == "Incident") %>%
  filter(date_type %in% c("date_raised", "date_ended")) %>%
  group_by(date_type, week_ended) %>%
  count()
Plots$Weekly_Count_Incidents <- Data_5$Weekly_Count_Incidents %>%
  ggplot(aes(x = week_ended, y = n, color = date_type)) + 
  geom_line(size = 1) +
  #geom_text(label = Data$Volume, size = 3, color = "#696969", nudge_y = 15) +
  #stat_smooth(linetype = "longdash", color = "#696969", method = "loess", formula = "y ~ x") +
  ggtitle(paste("Number of Incident Trello Tickets Raised and Ended", sep = "")) +
  labs(y = "ticket count", x = "week of year", color = "Date Type")  +
  expand_limits(y = 0)  +
  scale_y_continuous(limits = c(0, 140))


# 5.2. PROCESSING TICKET TIME & PER PERSON ----------------------------------------------------

# melt date values for visualisation (SOURCE: http://www.datasciencemadesimple.com/melting-casting-r/)
Data_5$Board_cards_Melt_Processing_Times <- Data_5$Board_cards %>%
  select(-username, -dev_effort, -test_effort, -incident_category, -bucket) %>%
  melt(id = c("card_id",
              "name", 
              "url",
              "description", 
              "list_name", 
              "list_position",
              "fullname",
              "member_count",
              "total_effort",
              "label",
              "date_raised",
              "date_ended",
              "date_started",
              "week_ended", 
              "day_ended", 
              "year_ended", 
              "month_name_ended", 
              "month_number_ended"), 
       variable.name = c("processing_time_type"), 
       value.name = "processing_time_avg")

# processing time for all tickets (Enhancements, Requests, Incidents)
Data_5$Weekly_Processing_Time <- Data_5$Board_cards_Melt_Processing_Times %>%
  filter(fullname %in% Plots$Options$people_for_analysis) %>%
  filter(label %in% Plots$Options$labels_for_analysis) %>%
  #filter(week_ended > week(today()) - Plots$Options$weeks_to_analyse) %>%
  filter(year(date_ended) == Plots$Options$year_for_anlysis) %>%
  group_by(processing_time_type, week_ended) %>%
  summarise(n = mean(processing_time_avg, na.rm = TRUE))
Plots$Weekly_Processing_Time <- Data_5$Weekly_Processing_Time %>%
  ggplot(aes(y = n, x = week_ended, fill = processing_time_type)) +
  geom_bar(stat = 'identity') +
  labs(title = paste("Average Processing Time for All Tickets Completed", sep = ""), x = "week of year", y = "days", fill = "Processing Type")  +
  #scale_y_continuous(limits = c(0, (max(Data_5$Weekly_Processing_Time$n)) + 2)) +
  scale_x_continuous(breaks = unique(c(min(Data_5$Weekly_Processing_Time$week_ended):
                                       max(Data_5$Weekly_Processing_Time$week_ended))))

Data_5$Per_Person_Processing_Time <- Data_5$Board_cards_Melt_Processing_Times %>%
  filter(fullname %in% Plots$Options$people_for_analysis) %>%
  filter(label %in% Plots$Options$labels_for_analysis) %>%
  filter(!is.na(processing_time_avg)) %>%  
  #filter(week_ended > week(today()) - Plots$Options$weeks_to_analyse) %>%
  filter(year(date_ended) == Plots$Options$year_for_anlysis)
Plots$Per_Person_Processing_Time <- Data_5$Per_Person_Processing_Time %>%
  group_by(processing_time_type, fullname, label) %>%
  summarise(n = mean(processing_time_avg , na.rm = TRUE)) %>%
  ggplot(aes(y = n, x = fullname, fill = processing_time_type)) +
  geom_bar(stat = 'identity') +
  labs(title = paste("Average Processing Time for All Tickets Completed", sep = ""), x = "", y = "days", fill = "Processing Type")  +
  scale_y_continuous(limits = c(0,30)) +
  facet_grid(cols = vars(label)) +
  coord_flip()
Plots$Per_Person_Processing_Time_Outliers <- Data_5$Per_Person_Processing_Time %>%
  filter(processing_time_avg > 8) %>%
  select(name, url, fullname, week_ended, processing_time_avg, processing_time_type)

# processing time for all Incident Tickets
Data_5$Weekly_Processing_Time_Incidents <- Data_5$Board_cards_Melt_Processing_Times %>%
  filter(label == "Incident") %>%
  filter(!is.na(processing_time_avg)) %>%  
  filter(year(date_ended) == Plots$Options$year_for_anlysis) %>%
  group_by(processing_time_type, week_ended) %>%
  summarise(processing_time_avg = mean(processing_time_avg , na.rm = TRUE))
# create Plot
Plots$Weekly_Processing_Time_Incidents <- Data_5$Weekly_Processing_Time_Incidents %>%
  ggplot(aes(y = processing_time_avg, x = week_ended, fill = processing_time_type)) +
  geom_bar(stat = 'identity') +
  labs(title = paste("Average Team Processing Time for Resolving Incident Tickets", sep = ""), x = "week of year", y = "days", fill = "Processing Type") +
  scale_x_continuous(limits = c(0, 53), breaks = c(0:53)) + 
  geom_hline(yintercept=2, linetype="dashed", color = "red", size = 1) +
  theme(legend.position="none")

# processing time for all Incident Tickets per person
Data_5$Per_Person_Processing_Time_Incidents <- Data_5$Board_cards_Melt_Processing_Times %>%
  filter(label == "Incident") %>%
  filter(!is.na(processing_time_avg)) %>%  
  filter(year(date_ended) == Plots$Options$year_for_anlysis) %>%
  group_by(week_ended, processing_time_type, fullname) %>%
  summarise(processing_time_avg = mean(processing_time_avg , na.rm = TRUE)) %>%
  filter(processing_time_avg >= 0)

# create table for obtaining rolling_avg
Data_5$Per_Person_Processing_Time_Incidents_rolling_avg <- Data_5$Per_Person_Processing_Time_Incidents %>%
  filter(week_ended >= Plots$Options$min_week_to_analyse) %>%
  group_by(week_ended, fullname) %>%
  summarise(total = sum(processing_time_avg)) %>%
  group_by(fullname) %>%
  summarise(rolling_avg = mean(total)) %>%
  mutate(top_rolling_avg_group = ifelse(rolling_avg >= min(tail(sort(rolling_avg),Plots$Options$row_count_for_analysis)), TRUE, FALSE))
# join table with rolling_avg and original Per_Person_Processing_Time_Incidents
Data_5$Per_Person_Processing_Time_Incidents <- inner_join(Data_5$Per_Person_Processing_Time_Incidents, Data_5$Per_Person_Processing_Time_Incidents_rolling_avg)

# coord_flip version
Plots$Per_Person_Processing_Time_Incidents_flip <- Data_5$Per_Person_Processing_Time_Incidents %>%
  group_by(fullname, processing_time_type) %>%
  summarise(processing_time_avg = mean(processing_time_avg)) %>%
  ggplot(aes(y = processing_time_avg, x = fullname, fill = processing_time_type)) +
  geom_bar(stat = 'identity') +
  labs(title = paste("Average Processing Time for Resolving Incident Tickets", sep = ""), x = "", y = "days", fill = "Processing Type") +
  geom_hline(yintercept=2, linetype="dashed", color = "red", size = 1) +
  coord_flip()

# facet version
Plots$Per_Person_Processing_Time_Incidents_facet <- Data_5$Per_Person_Processing_Time_Incidents %>%
  filter(top_rolling_avg_group == TRUE) %>%
  ggplot(aes(y = processing_time_avg, x = week_ended, fill = processing_time_type)) +
  geom_bar(stat = 'identity') +
  labs(title = paste("Top ",Plots$Options$row_count_for_analysis," Slowest Individuals Over The Past ", Plots$Options$weeks_to_analyse," Weeks", sep = ""), x = "week of year", y = "days", fill = "Processing Type") +
  scale_x_continuous(limits = c(0, 53), breaks = c(0:53)) + 
  geom_hline(yintercept=2, linetype="dashed", color = "red", size = 0.5) +
  facet_grid(rows = vars(fullname)) +
  theme(legend.position="bottom")

# capture outlyers
Plots$Per_Person_Processing_Time_Incidents_Outliers <- Data_5$Board_cards_Melt_Processing_Times %>%
  filter(label == "Incident") %>%
  filter(!is.na(processing_time_avg)) %>%  
  filter(year(date_ended) == Plots$Options$year_for_anlysis) %>%
  filter(processing_time_avg > 1) %>%
  select(name, url, fullname, week_ended, processing_time_avg, processing_time_type)

# 5.3. POINTS OVERTIME PER PERSON ------------------------------------------
# visualise total weekly points and type
Plots$Weekly_Points <- Data_5$Board_cards_Run %>% 
  filter(year(date_ended) == Plots$Options$year_for_anlysis) %>%
  group_by(week_ended, label) %>%
  summarise(total_effort = sum(total_effort)) %>%
  ggplot(aes(y = total_effort, x = week_ended, fill = label)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("#90ee90", "#b19cd9", "#ffc966", "#ffc0cb", "#DCDCDC")) +
  labs(title = paste("Total Team Points Earnt", sep = ""), x = "week of year", y = "points", fill = "Type") +
  scale_x_continuous(limits = c(0, 53), breaks = c(0:53)) +
  theme(legend.position = "none")
  
# visualise total weekly points and people
Plots$Weekly_Points_People <- Data_5$Board_cards_Run %>% 
  filter(year(date_ended) == Plots$Options$year_for_anlysis) %>%
  group_by(week_ended, fullname) %>%
  summarise(total_effort = sum(total_effort)) %>%
  ggplot(aes(y = total_effort, x = week_ended, fill = fullname)) +
  geom_bar(stat = 'identity') +
  labs(title = paste("Total Points Earnt Over Past ", Plots$Options$weeks_to_analyse, " Weeks", sep = ""), x = "week of year", y = "points", fill = "Type") +
  scale_x_continuous(breaks = unique(c(min(Data_5$Board_cards_Run$week_ended):
                                         max(Data_5$Board_cards_Run$week_ended)))) 

# visualise total weekly points and bucket
Data_5$Weekly_Points_Incident_Categories_table <- Data_5$Board_cards %>% 
  filter(label == "Incident") %>%
  filter(total_effort != 0) %>%
  filter(date_ended > today() - 7) %>%
  group_by(incident_category, day_ended, bucket) %>%
  summarise(total_effort = sum(total_effort))

# get data for "Process" category
Data_5$Weekly_Points_Incident_Categories_table_process <- Data_5$Weekly_Points_Incident_Categories_table %>% 
  filter(incident_category == "Process")

# how to apply gradient colors to discrete values = https://stackoverflow.com/questions/30352412/how-do-you-create-a-gradient-of-colors-for-a-discrete-variable-in-ggplot2
Plots$Options$Weekly_Points_Incident_Categories_color_scale <- seq_gradient_pal("#ffae1a", "#ffe4b3", "Lab")(seq(0,1,length.out = length(unique(Data_5$Weekly_Points_Incident_Categories_table$bucket))))

# visualise total weekly points and bucket
Plots$Weekly_Points_Incident_Categories <- Data_5$Weekly_Points_Incident_Categories_table %>% 
  ggplot(aes(y = total_effort, x = day_ended, fill = bucket, label = bucket)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = Plots$Options$Weekly_Points_Incident_Categories_color_scale) +
  labs(title = paste("Total Points Earnt For Each Incident Category", sep = ""), x = "", y = "points", fill = "Process") +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  facet_grid(rows = vars(incident_category)) +
  theme(legend.position="none")

# visualise total weekly points for "Process" category specifically


# visualise total weekly points and people
Plots$Weekly_Points_People_Percentage <- Data_5$Board_cards_Run %>% 
  filter(year(date_ended) == Plots$Options$year_for_anlysis) %>%
  group_by(week_ended, fullname) %>%
  summarise(total_effort = sum(total_effort)) %>%
  ggplot(aes(y = total_effort, x = week_ended, fill = fullname)) +
  geom_bar(position = "fill", stat = 'identity') +
  labs(title = paste("Total Points Earnt Over Past ", Plots$Options$weeks_to_analyse, " Weeks", sep = ""), x = "week of year", y = "points", fill = "Type") +
  scale_x_continuous(breaks = unique(c(min(Data_5$Board_cards_Run$week_ended):
                                         max(Data_5$Board_cards_Run$week_ended)))) +
  scale_y_continuous(labels = scales::percent_format())

# visualise weekly points percentage
Plots$Weekly_Points_Percentage <- Data_5$Board_cards_Run %>% 
  filter(year(date_ended) == Plots$Options$year_for_anlysis) %>%
  group_by(week_ended, label) %>%
  summarise(total_effort = sum(total_effort)) %>%
  ggplot(aes(y = total_effort, x = week_ended, fill = label)) +
  geom_bar(position = "fill", stat = 'identity') +
  scale_fill_manual(values = c("#90ee90", "#b19cd9", "#ffc966", "#ffc0cb", "#DCDCDC")) +
  labs(title = paste("Percentage of Points Earnt Per Type Over Past ", Plots$Options$weeks_to_analyse, " Weeks", sep = ""), x = "week of year", y = "points", fill = "Type") +
  scale_x_continuous(breaks = unique(c(min(Data_5$Board_cards_Run$week_ended):
                                         max(Data_5$Board_cards_Run$week_ended)))) +
  scale_y_continuous(labels = scales::percent_format())

# visualise total weekly points per person
Plots$Weekly_Points_Facet_People <- Data_5$Board_cards_Run %>% 
  filter(year(date_ended) == Plots$Options$year_for_anlysis) %>%
  group_by(fullname, week_ended, label) %>%
  summarise(total_effort = sum(total_effort)) %>%
  ggplot(aes(y = total_effort, x = week_ended, fill = label)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("#90ee90", "#b19cd9", "#ffc966", "#ffc0cb", "#DCDCDC")) +
  labs(title = paste("Total Points Earnt Per Person", sep = ""), x = "week of year", y = "points", fill = "Type") +
  scale_x_continuous(limits = c(0, 53), breaks = c(0:53)) + 
  facet_grid(rows = vars(fullname)) +
  theme(legend.position = "bottom")

# visualise percentage of weekly points per person
Plots$Weekly_Points_Facet_People_Percentage <- Data_5$Board_cards_Run %>% 
  filter(year(date_ended) == Plots$Options$year_for_anlysis) %>%
  group_by(fullname, week_ended, label) %>%
  summarise(total_effort = sum(total_effort)) %>%
  ggplot(aes(y = total_effort, x = week_ended, fill = label)) +
  geom_bar(position = "fill", stat = 'identity') +
  scale_fill_manual(values = c("#90ee90", "#b19cd9", "#ffc966", "#ffc0cb", "#DCDCDC")) +
  labs(title = paste("Percentage of Points Earnt Per Person for Each Category Over Past ", Plots$Options$weeks_to_analyse, " Weeks", sep = ""), x = "week of year", y = "points", fill = "Type") +
  scale_x_continuous(breaks = unique(c(min(Data_5$Board_cards_Run$week_ended):
                                         max(Data_5$Board_cards_Run$week_ended)))) +
  facet_grid(rows = vars(fullname)) +
  scale_y_continuous(labels = scales::percent_format())


# visualise weekly points per person
Plots$Per_Person_Weekly_Points <- Data_5$Board_cards_Run %>% 
  filter(year(date_ended) == Plots$Options$year_for_anlysis) %>%
  group_by(fullname, label) %>%
  summarise(total_effort = sum(total_effort)) %>%
  ggplot(aes(y = total_effort, x = fullname, fill = label)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("#90ee90", "#b19cd9", "#ffc966", "#ffc0cb", "#DCDCDC")) +
  labs(title = paste("Points Earnt Over Past ", Plots$Options$weeks_to_analyse, " Weeks", sep = ""), x = "", y = "points", fill = "Type")  +
  facet_grid(cols = vars(label)) +
  coord_flip()


# visualise weekly points per person
Plots$Per_Person_Weekly_Ticket_Count <- Data_5$Board_cards_Run %>% 
  filter(year(date_ended) == Plots$Options$year_for_anlysis) %>%
  group_by(fullname, label) %>%
  count() %>%
  ggplot(aes(y = n, x = fullname, fill = label)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c("#90ee90", "#b19cd9", "#ffc966", "#ffc0cb", "#DCDCDC")) +
  labs(title = paste("Number of Tickets Completed Over Past ", Plots$Options$weeks_to_analyse, " Weeks", sep = ""), x = "", y = "tickets", fill = "Type")  +
  facet_grid(cols = vars(label)) +
  coord_flip()


# 5.4. SUMMARY PLOTS FOR WEEKLY TICKETS PROCESSING -----------------------------

#Setup layout for Volume (A) and Completion Rate (B) Plots
#(Sourced from "https://stackoverflow.com/questions/13294952/left-align-two-graph-edges-ggplot")
gA <- ggplotGrob(Plots$Weekly_Count)
gB <- ggplotGrob(Plots$Weekly_Count_Incidents)
gC <- ggplotGrob(Plots$Weekly_Processing_Time)
gD <- ggplotGrob(Plots$Weekly_Processing_Time_Incidents)

maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5], gC$widths[2:5], gD$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
gC$widths[2:5] <- as.list(maxWidth)
gD$widths[2:5] <- as.list(maxWidth)


#Display summary
Plots$Summary_Weekly_Processing_Time <- grid.arrange(gA, gB, gC, gD, ncol=1,
                                                      top = textGrob(paste("Trello Ticket Analysis Over 4 Weeks", sep = ""),
                                                                     gp = gpar(fontsize=20,font=1)))


# 5.5. SUMMARY PLOTS FOR WEEKLY TICKETS PROCESSING INCIDENTS --------------

#Setup layout for Volume (A) and Completion Rate (B) Plots
#(Sourced from "https://stackoverflow.com/questions/13294952/left-align-two-graph-edges-ggplot")
gA <- ggplotGrob(Plots$Weekly_Processing_Time_Incidents)
gB <- ggplotGrob(Plots$Per_Person_Processing_Time_Incidents_facet)

maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)


#Display summary
Plots$Summary_Weekly_Processing_Time_Incidents <- grid.arrange(gA, gB, ncol=1,
                                                               heights = c(1,2),
                                                               top = textGrob(paste("Trello Ticket Analysis For Resolving Incident Tickets", sep = ""),
                                                                              gp = gpar(fontsize=20,font=1)))

# 5.6. SUMMARY PLOTS FOR WEEKLY POINTS EARNT ------------------------------
# create summary plots
#(Sourced from "https://stackoverflow.com/questions/13294952/left-align-two-graph-edges-ggplot")
gA <- ggplotGrob(Plots$Weekly_Points)
gB <- ggplotGrob(Plots$Weekly_Points_Facet_People)

maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)


#Display summary
Plots$Summary_Weekly_Total_Points_Earnt <- grid.arrange(gA, gB, ncol=1,
                                                  heights = c(1,2),
                                             top = textGrob(paste("Trello Ticket Analysis of Points Earnt", sep = ""),
                                                            gp = gpar(fontsize=20,font=1)))

# 5.7. SUMMARY VALUES -----------------------------------------------------

# get variables for RMarkdown reporting
var_recent_week <- as.numeric(week(today()))
var_tickets_raised <- Data_5$Weekly_Count %>%
  filter(week_ended == var_recent_week) %>%
  filter(date_type == "date_raised")
var_tickets_closed <- Data_5$Weekly_Count %>%
  filter(week_ended == var_recent_week) %>%
  filter(date_type == "date_ended")


# 6. CLEAN UP ENVIRONMENT -------------------------------------------------
if(Settings$Debug_Mode != TRUE) {
  rm(Data_1, Data_2, Data_3, Data_4, gA, gB, gC, gD, maxWidth, Settings)
}

  