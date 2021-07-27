generate_template <- function(cluster_name, questions, indicators) {
  
  
  columns_to_extract <- c(
    "uuid",
    "region",
    "district",
    "idp_settlement",
    map(questions, ~ data %>%
          select(matches(sprintf("((^%s|^.*\\.%s)\\.|(^.*\\.%s|^%s)$)", .x, .x, .x, .x))) %>%
          # select(matches(sprintf("(^%s\\.|^%s$)", .x, .x))) %>%
          colnames()) %>% unlist()
  )
  
  

  template <- data %>% select(all_of(columns_to_extract))
  
  for (indicator in c(indicators, "Final HH Score")) {
    template[[indicator]] <- as.integer(NA)
  }
  
  template$"_uuid" <- template$uuid
  template$Key <- data$strata
  template$Area <- data$district
  template[["Population group"]] <- data$population_group
  
  template
}


percent_response <- function(x, df, ..., x_name = NULL, group = NULL) {
  if (is.null(x_name)) {
    
    x_name <- deparse(substitute(x))
    
  }
  
  group_var <- group_vars(df)
  
  args <- list(...)
  args <- unlist(args)
  args <- paste0("^", args, "$")
  
  
  if (!is.null(group) & nrow(df) == length(x)) {
    x <- x[df[[group_var]] == group]
  }
  
  x <- x[!is.na(x)]
  x <- as.character(x)
  
  
  if (length(x) == 0) {
    NA
  } else {
    pct <- sum(str_detect(x, args))
    pct
  }
}

get_group <- function(df) {
  group <- group_vars(df)
  quo(unique(!!sym(group)))
}


fn_select_one_mode_shelter_IDP <- function(x) {
  
  if(all(is.na(x))){return(NA)}
  
  if (all("none"==na.omit(x))) {
    return("5")
  } else if (all("not_sure"==na.omit(x))) {
    return(NA)
  }  else {
    return(case_when(
      any( c("brick","stone") %in% x ) ~ "1",
      any( c("timer_", "cgi", "mud", "collective","stick") %in% x ) ~ "2",
      any( c("tent", "unfinished") %in% x ) ~ "3",
      any( c("buul") %in% x ) ~ "4"
      
      
    ))
  }
  
}



fn_select_one_mode_shelter_HC <- function(x) {
  
  if(all(is.na(x))){return(NA)}
  
  if (all("none"==na.omit(x))) {
    return("5")
  } else if (all("not_sure"==na.omit(x))) {
    return(NA)
  }  else {
    return(case_when(
      any( c("brick","stone") %in% x ) ~ "1",
      any( c("buul","timer_", "cgi", "mud", "collective","stick") %in% x ) ~ "2",
      any( c("tent", "unfinished") %in% x ) ~ "3"
      
      
    ))
  }
  
}



fn_select_one_mode <- function(x) {
  if(all(is.na(x))){return(NA)}
  
  if (all("none"==na.omit(x))) {
    return("none")
  } else if (all("not_sure"==na.omit(x))) {
    return("not_sure")
  }
  x <- x[x!="not_sure"]
  x <- x[x!="none"]
  uniqx <- unique(na.omit(x))
  
  
  if (length(which(tabulate(match(x, uniqx)) == max(tabulate(match(x, uniqx))))) > 1) {
    return("NC")
  }
  
  uniqx[which.max(tabulate(match(x, uniqx)))]
  
}


