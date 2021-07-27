pin_code_generator <- function(cluster_questions_df,cluster_name,col_name_indicator,col_name_severity_1,col_name_severity_2,col_name_severity_3,col_name_severity_4,col_name_severity_5) {
  cluster_questions_df <- cluster_questions_df %>% select(all_of(c(col_name_indicator,col_name_severity_1,col_name_severity_2,col_name_severity_3,col_name_severity_4,col_name_severity_5)))

  add_spaces <- function(x,n) {
    
  for (i in 1:n) {
    ch=" "
    x <- c(x,ch)
  }
    
    x
    
  }
  
  x <- c()
  ch = "   data %>% mutate("
  x <- c(x,ch)
  
  for (i in 1:nrow(cluster_questions_df)) {
    ch = sprintf("   #### %s indicator %s ####",cluster_name,as.character(i))
    x <- c(x,ch)
    ch = sprintf("   ## %s",cluster_questions_df[i,col_name_indicator])
    x <- c(x,ch)
    
    ch = sprintf("   %s_idicator%s = case_when(",cluster_name,as.character(i))
    x <- c(x,ch)
    
    for (severity in c(col_name_severity_5,col_name_severity_4,col_name_severity_3,col_name_severity_2,col_name_severity_1)) {
     
      if(!is.na(cluster_questions_df[i,severity])){
        ch = sprintf("## %s",cluster_questions_df[i,severity])
        ch =  str_replace_all(ch, "[\r\n]" , "")
        x <- c(x,ch)
        ch = sprintf("TRUE ~ %s,",stringr::str_sub(severity,-1,-1))
        x <- c(x,ch)
        x <- add_spaces(x,1)
        
      }
      
    }
    
    ch = sprintf("),")
    x <- c(x,ch)
    
    x <- add_spaces(x,3)
 
  }
  
  ch = sprintf(")")
  x <- c(x,ch)
  
  y <- x %>% paste(.,sep = '\n')
  
  clipr::write_clip(y)

}



# cluster_questions_df <- import("input/PIN_HNO_Indicators.xlsx",sheet="CCCM Thresholds",na=c("","n/a")) %>% filter(Source == "JMCNA")
# cluster_name= "CCCM"
# col_name_indicator = "Indicator"
# col_name_severity_1 = "SEVERITY 1"
# col_name_severity_2 = "SEVERITY 2"
# col_name_severity_3 = "SEVERITY 3"
# col_name_severity_4 = "SEVERITY 4"
# col_name_severity_5 = "SEVERITY 5"
# 
# 
# 
# code <- pin_code_generator(cluster_questions_df,cluster_name,col_name_indicator,col_name_severity_1,col_name_severity_2,col_name_severity_3,col_name_severity_4,col_name_severity_5)
# 
# rm(list = c("cluster_questions_df","col_name_indicator", "cluster_name", "col_name_severity_1", "col_name_severity_2", "col_name_severity_3", "col_name_severity_4", "col_name_severity_5","code"))
# 





question_description <- function(xml_question) {
  
  x1 = sprintf("Question Label: %s",xml_question)
  x1_1 = sprintf("Question Type: %s",
                 case_when(
                   questionnaire$question_is_select_multiple(xml_question) ~ "Select Multilpe",
                   questionnaire$question_is_select_one(xml_question) ~ "Select One",
                   questionnaire$question_is_numeric(xml_question) ~ "Numeric",
                 )
  )
  x2 = sprintf("Question En: %s",questionnaire$question_get_question_label(xml_question))
  x3 = questionnaire$question_get_choices(xml_question)
  x4 = map(x3,~ questionnaire$question_get_choice_labels(xml_question,responses = .x)) %>% unlist()
  x5 = paste(x3,"==>",x4)
  
  c(x1,x1_1,x2,x5) %>% print()
}


