
# create function to extract first label name found out of Settings$labels_for_analysis input variable
get_label_name <- function(x, labels_for_analysis) {
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

# create function to extract first label name found out of labels_for_analysis input variable
get_label_name_trelloR <- function(x, labels_for_analysis) {
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
      } else output <- NA
    }
  } else output <- NA
  return(output)
}

# create function to extract customField DROPDOWN IDs from cards
get_customField_dropdown_ids <- function(x, customField_id) {
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

# create function to extract customFiled DROPDOWN VALUES using value ID and the boards customField options
get_customField_dropdown_value <- function(x, customField_list) {
  stop <- FALSE
  for(index in seq_along(customField_list)) {
    if(stop) break
    #first IF statement aims to check if the custom field acutally is a DropDown type by checking for the "options" sub list
    if(length(customField_list[[index]][["options"]]) > 0) {
      for(sub_index in seq_along(customField_list[[index]][["options"]])) {
        if(as.character(customField_list[[index]][["options"]][[sub_index]][["id"]]) == x) {
          output <- as.character(customField_list[[index]][["options"]][[sub_index]][["value"]][["text"]])
          stop <- TRUE
          break
        } else output <- NA
      }  
    }
  }
  return(output)
}

# create function to count number of members per card
# there are three scenarios that can occur: (1) no members found; (2) one member found; (3) more then one member found.
# the below IF statements handle for all three scenarios
get_card_member_count <- function(x) {
  output <- length(unlist(x))
  return(output)
}

# create funciton to extract members from cards
# there are three scenarios that can occur: (1) no members found; (2) one member found; (3) more then one member found.
# the below IF statements handle for all three scenarios
get_cards_and_member_ids <- function(x) {
  # create the output table that will be the result of the function
  output <- data.frame(
    card_id = as.character(),
    member_id = as.character()
  )
  if(length(unlist(x["idMembers"])) > 1) {
    # handle for the scenario (3) more then one member found
    for(index in seq_along(unlist(x["idMembers"]))) {
      temp_output <- data.frame(
        card_id = as.character(x["id"]),
        member_id = as.character(unlist(x["idMembers"])[index])
      )
      output <- rbind.data.frame(
        output,
        temp_output
      )
    }
  } else if (length(unlist(x["idMembers"])) == 1) {
    # handle for the scenario (2) one member found
    output <- data.frame(
      card_id = as.character(x["id"]),
      member_id = as.character(unlist(x["idMembers"]))
    )
  } else if (length(unlist(x["idMembers"])) == 0) {
    # handle for the scenario (1) no members found
    output <- data.frame(
      card_id = as.character(x["id"]),
      member_id = as.character(NA)
    )
  }
  return(output)
}