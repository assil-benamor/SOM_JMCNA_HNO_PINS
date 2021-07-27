create_indicators_placeholder <- function(data, indicators) {
  df <- data

  for (indicator in indicators) {
    df[[paste0(indicator)]] <- as.integer(NA)
  }

  df <- df %>% select(all_of(indicators))

  return(df)
}


compute_indicators <- function(data, cluster, indicators) {
  if (cluster == "Education") {
    compute_indicators_education(data, indicators)
  } else if (cluster == "Wash") {
    compute_indicators_wash(data, indicators)
  } else if (cluster == "Protection") {
    compute_indicators_protection(data, indicators)
  } else if (cluster == "SNFI") {
    compute_indicators_snfi(data, indicators)
  } else if (cluster == "GBV_AoR") {
    compute_indicators_gbv(data, indicators)
  } else if (cluster == "HLP_AoR") {
    compute_indicators_hlp(data, indicators)
  } else if (cluster == "CP_AoR") {
    compute_indicators_cp(data, indicators)
  } else if (cluster == "EH_AoR") {
    compute_indicators_eh(data, indicators)
  } else if (cluster == "CCCM") {
    compute_indicators_cccm(data, indicators)
  } else if (cluster == "Health") {
    compute_indicators_health(data, indicators)
  }
}




compute_indicators_wash <- function(data, indicators) {
  
  df <- create_indicators_placeholder(data, indicators)


  improved_water_source <- c("piped_system", "protected_well_pump", "protected_well_hand", "borehole","water_tank", "water_trucking")
  no_improved_water_source <- c("water_kiosk", "vendor_shops", "unprotected_well")
  surface_water <- c("river_pond")
  
  
  improved_sanitation_facilities <-  c("flush_toilet", "pit_latrine_slap", "pit_vip")
  no_improved_sanitation_facilities <-  c("pit_latrine_without_slap", "open_hole","bucket_toilet", "plastic_bag", "hanging_latrine")
  open_defecation <- c("none_of_the_above")
  

  
  data_wash_prep <- data %>% 
    mutate(
      sharing_sanitation_facilities_yes_int = as.integer(sharing_sanitation_facilities_yes)
    )

  
  data_wash_indicators <- data_wash_prep %>% mutate (

  #### wash indicator 1 ####
  ## % of HHs by type of primary source of drinking water
  wash_idicator1 = case_when(
    ## Water comes directly from rivers, lakes, ponds, etc.
    main_source_water %in% surface_water ~ 5,
  
    ## Water comes from an unimproved water source
    main_source_water %in% no_improved_water_source ~ 4,
  
    ## Water comes from an improved water source but collection time is more than 15 min for a roundtrip, including queuing
    time_fetching_water %in% c("btw_16_30_minutes", "more_31_minutes") & main_source_water %in% improved_water_source ~ 3,
  
    ## Water comes from an improved water source and  collection time is not more than 15 min for a roundtrip, including queuing
    time_fetching_water %in% c("less_5_minutes", "btw_5_15_minutes") & main_source_water %in% improved_water_source ~ 2,
  
    ## HH: Water comes from an improved water source which is located on premises
    time_fetching_water == "water_premises" & main_source_water %in% improved_water_source ~ 1, 
    
    ## dnk
    TRUE ~ 1,
  ),

  #### wash indicator 2 ####
  ## % of HHs using a sanitation facility - by type of sanitation facility used
  wash_idicator2 = case_when(
    ## No access to latrine - open defecation
    sanitation_facility %in% open_defecation ~ 5,
    
    ## HH: Access to unmproved sanitation facilities, shared with more than 50 people
    sanitation_facility %in% no_improved_sanitation_facilities &
     sharing_sanitation_facilities %in% c("yes") &
     sharing_sanitation_facilities_yes_int >=50 ~ 4,
    
    ## HH: Access to unmproved sanitation facilities, shared with more than 20 people
    sanitation_facility %in% no_improved_sanitation_facilities &
     sharing_sanitation_facilities %in% c("yes") &
     sharing_sanitation_facilities_yes_int >=20 ~ 3,
    
    ## Access to unimproved sanitation facilities
    sanitation_facility %in% no_improved_sanitation_facilities ~ 2,
    
    ## Access to improved sanitation facilities
    sanitation_facility %in% improved_sanitation_facilities ~ 1,
    
    ## Dnk
    TRUE ~ 1
    
  ),
  
  #### wash indicator 3 ####
  ## % of HHs with access to soap
  wash_idicator3 = case_when(
    ## Soap is not available at home and no handwashing facility with soap and water on premise
    have_soap %in% c("no") &
      hand_washing_facility %in% c("no_specific","tippy_tap") ~ 4,
    
    ## Soap is available at home BUT no handwashing facility on premises with soap and wate
    (have_soap %in% c("yes") & hand_washing_facility %in% c("no_specific","tippy_tap")) |
      (have_soap %in% c("no") & hand_washing_facility == "dnk" ) ~ 3,
    
    ## EITHER soap is available at home OR latrines used by HH have functional facilities for handwashing
    have_soap %in% c("yes") |
      hand_washing_facility %in% c("buckets_with_taps","sink_tap_water") ~ 2,
    
    ## Soap is available at home AND latrines used by HH have functional facilities for handwashing
    have_soap %in% c("yes") &
      hand_washing_facility %in% c("tippy_tap","buckets_with_taps","sink_tap_water") ~ 1,
    
    ## Dnk
    TRUE ~ 1
    
  ),
  
  
  #### wash indicator 4 ####
  ## % of HHs reporting having enough water for drinking, cooking, bathing and washing
  wash_idicator4 = case_when(
    ## Not enough water for drinking OR domestic uses
    drinking_water == "no" | (domestice_water == "no" & cooking_water == "no" & hygiene_water == "no") ~ 5,
    
    ## EITHER enough water for drinking OR for domestic uses PLUS The HH does not have jerry cans
    (drinking_water == "yes" | (domestice_water == "yes" & cooking_water == "yes" & hygiene_water == "yes") ) & currently_access_nfi.jerrycans_2 == 0 ~ 4,
    
    ## EITHER enough water for drinking OR for domestic uses PLUS The HH has jerry cans
    (drinking_water == "yes" | (domestice_water == "yes" & cooking_water == "yes" & hygiene_water == "yes") ) & currently_access_nfi.jerrycans_2 == 1 ~ 3,
    
    ## Enough water for drinking AND domestic uses (cooking, bathing, and cleaning, not agriculture or livestock) PLUS The HH does not have jerry cans
    (drinking_water == "yes" & domestice_water == "yes" & cooking_water == "yes" & hygiene_water == "yes") & currently_access_nfi.jerrycans_2 == 0 ~ 2,
    
    ## Enough water for drinking AND domestic uses (cooking, bathing, and cleaning, not agriculture or livestock)  PLUS The HH has jerry cans
    (drinking_water == "yes" & domestice_water == "yes" & cooking_water == "yes" & hygiene_water == "yes") & currently_access_nfi.jerrycans_2 == 1 ~ 1,
    
    ##
    TRUE ~ 1
    
  ),
  
  
  #### wash indicator 5 ####
  ## % of HHs with access to a sanitation facility safe for all members to use
  
  wash_idicator5 = case_when(
    ## HH using latrines with incomplete walls AND no door, locks or functioning lighting
      sanitation_features.walls == 0 &
      sanitation_features.doors == 0 &
      sanitation_features.locks == 0 &
      sanitation_features.inside_light == 0 &
      sanitation_features.outside_light == 0 ~ 4,
    
    ## HH: HH using latrines with walls, door, BUT NO locks and functioning lighting
    sanitation_features.doors == 1 &
      sanitation_features.walls == 1 &
      sanitation_features.locks == 0 &
      sanitation_features.inside_light == 0 &
      sanitation_features.outside_light == 0 ~ 3,
    
    ## HH: HH using latrines with walls, door, locks and functioning lighting
    sanitation_features.doors == 1 &
      sanitation_features.walls == 1 &
      sanitation_features.locks == 1 &
      sanitation_features.inside_light == 1 &
      sanitation_features.outside_light == 1~ 1,
    
    TRUE ~ 1,
    
  ),
  
  
  #### wash indicator 6 - temporary 1 ####
  ## % Households using negative coping strategies to access water
  wash_tmp_idicator6_1 = case_when(

    ## 3
    adopt_lack_water.fetch_dangerous_places == 1 | 
      adopt_lack_water.drink_less  == 1 ~ 3,
    
    ## 2
    adopt_lack_water.rely_unimproved_sources_drinking == 1 |
      adopt_lack_water.rely_surface_water_drinking == 1 |
      adopt_lack_water.send_children_fetch  == 1 |
      adopt_lack_water.reduce_water_consumption == 1  ~ 2,
    
    ## 1
    adopt_lack_water.spend_money_water == 1 |
      adopt_lack_water.fetch_further_source == 1 |
      adopt_lack_water.rely_unimproved_sources_for_other == 1 |
      adopt_lack_water.rely_surface_water_other  ~ 1,
    
    ## 
    TRUE ~ 0
  ),
  
  
  #### wash indicator 6 - temporary 2 ####
  ## % Households using negative coping strategies to access sanitation facilities in the past 1 month/30 days
  wash_tmp_idicator6_2 = case_when(

    ## 3
    adapt_sanitation_issues.defecate_open == 1 |
      adapt_sanitation_issues.going_dangerous_place == 1 ~ 3 ,
    
    ## 2
    adapt_sanitation_issues.defecate_plastic_bags == 1 |
      adapt_sanitation_issues.going_night == 1 ~ 2 ,
    
    ## 1
    adapt_sanitation_issues.rely_unhygienic_facilities == 1 |
    adapt_sanitation_issues.rely_comunal_facilities == 1 |
    adapt_sanitation_issues.going_further_latrines == 1 ~ 1 ,
    
    ## 
    TRUE ~ 0,
    
  ),
  
  
  #### wash indicator 6 - temporary 3 ####
  ## % Households using negative coping strategies to access hygienic or menstrual materials
  wash_tmp_idicator6_3 = case_when(
    
    ## 3
    adopt_sanitaiton_items.buyying_nfi_dangerous_market_places == 1 |
      adopt_sanitaiton_items.reduce_nfi_consumption_personal == 1 ~ 3 ,
    
    ## 2
    adopt_sanitaiton_items.reduce_nfi_consumption_other == 1 ~ 2,
      
    
    ## 1
    adopt_sanitaiton_items.rely_less_types_nfi == 1 | 
      adopt_sanitaiton_items.rely_soap_substitutes == 1 |
      adopt_sanitaiton_items.buyying_nfi_market == 1 |
      adopt_sanitaiton_items.borrow_nfi_freind_relative == 1 |
      adopt_sanitaiton_items.spend_money_nfi == 1 ~ 1,
    
    ## 
    TRUE ~ 0
    
  )  ) 
  
  
  data_wash_indicators <- data_wash_indicators %>% mutate(
    wash_tmp_idicator6_sum = rowSums(.[grep("wash_tmp_idicator6_", names(.))], na.rm = TRUE),
  ) 
  
  data_wash_indicators %>% mutate(
    wash_idicator6 = case_when(
      wash_tmp_idicator6_sum == 9 ~ 5,
      wash_tmp_idicator6_sum >= 6 ~ 4,
      wash_tmp_idicator6_sum >= 3 ~ 3,
      wash_tmp_idicator6_sum >= 1 ~ 2,
      wash_tmp_idicator6_sum == 0 ~ 1,
    )
  ) %>% select(starts_with("wash_idicator")) -> res
  
  
  colnames(res) <- colnames(df)
  
  return(res)

}

compute_indicators_education <- function(data, indicators) {
  
  
  df <- create_indicators_placeholder(data, indicators)

  
  data_edu_prep <- data %>%
    rowwise() %>%
    mutate(
      
      hh_adults_int = as.numeric(hh_adults),
      
      school_age_children_cal = sum(males_6_11y,
                                    females_6_11y,
                                    males_12_17y,
                                    females_12_17y,
                                    na.rm = T),
      
      total_enrolled = sum(
        # enrolled_girls_3_5,
        # enrolled_boys_3_5,
        enrolled_girls_6_11,
        enrolled_boys_6_11,
        enrolled_girls_12_17,
        enrolled_boys_12_17,
        na.rm = T
      ),
      total_attend = sum(
        # attend_school_girls_3_5,
        # attend_school_boys_3_5,
        attend_school_girls_6_11,
        attend_school_boys_6_11,
        attend_school_girls_12_17,
        attend_school_boys_12_17,
        na.rm = T
      ),
      total_drop_out = sum(
        # dropped_out_girls_3_5,
        # dropped_out_boys_3_5,
        dropped_out_girls_6_11,
        dropped_out_boys_6_11,
        dropped_out_girls_12_17,
        dropped_out_boys_12_17,
        na.rm = T
      ),
      
      total_remaining = total_enrolled - total_drop_out,
      
      total_edu_level_achieved = sum(tertiary_degree,
        vocational_degree,
        secondary_school,
        middle_school,
        primary_school,
        quranic_school,
        na.rm = T
      )
    ) %>%
    ungroup()



  data_edu_prep %>%
    mutate(
      #### education indicator 1 ####
      ### % of children dropping out of school in the previous year
      edu_idicator1 = case_when(
        ## All school-aged children in the HH dropped out
        total_drop_out > 0 & total_remaining <= 0 ~ 4,
        
        ##Some school-aged children in the HH dropped out
        total_drop_out > 0 & total_remaining > 0 ~ 3,
        
        ##No school-aged children in the HH dropped out 
        total_drop_out == 0 | total_enrolled == 0 | school_age_children_cal == 0 ~ 1,
        

      ),

      #### education indicator 2 ####
      ### % of school-aged children enrolled in school for the 2020-2021 school year
      edu_idicator2 = case_when(
        ##No school-aged children in the households enrolled school
        (school_age_children_cal > 0 & (total_enrolled == 0)) ~ 4,
        
        ##Some school-aged children in the households enrolled school=if possible quantify to less than 50 Per cent enrolled school
        (school_age_children_cal > 0 & (total_enrolled / school_age_children_cal < 0.5)) ~ 3,
        
        ##More than 50 per cent enrolled school
        (school_age_children_cal > 0 & ( (total_enrolled / school_age_children_cal >= 0.5) & (total_enrolled / school_age_children_cal < 1))) ~ 2,
        
        ##All school-aged children in the households attend school
        (school_age_children_cal > 0 & (school_age_children_cal == total_enrolled)) | (school_age_children_cal == 0) ~ 1,

        
      ),

      #### education indicator 3 ####
      ### % of adults by highest education level (primary, secondary and tertiary) achieved
      edu_idicator3 = case_when(
        ##None has achieved
        total_edu_level_achieved == 0 ~ 4,
        
        ##Less than 50 per cent achieved any levels
        (total_edu_level_achieved / hh_adults_int) < 0.5 ~ 3,
        
        ##More than 50 per cent achieved any of the levels
        total_edu_level_achieved != hh_adults_int & (total_edu_level_achieved / hh_adults_int) >= 0.5 ~ 2,
        
        ##All have achieved any education level
        total_edu_level_achieved == hh_adults_int ~ 1
      ),

      #### education indicator 4 ####
      ### % of school-aged children attending school regularly (at least 4 days a week) in the 2020-2021 school year while schools were open, per age and sex group.
      edu_idicator4 = case_when(
        ##No children attend regularly
        school_age_children_cal != 0 & total_attend == 0 ~ 4,
        
        ##Some children attending regularly
        total_attend >0  ~ 3,
        
        
        ##All children attending regularly
        school_age_children_cal == 0 | (school_age_children_cal == total_attend) ~ 1
      ),
      
      
    ) %>% select(starts_with("edu_idicator")) -> res

  colnames(res) <- colnames(df)

  return(res)
}

compute_indicators_protection <- function(data, indicators) {
  
  
  df <- create_indicators_placeholder(data, indicators)
  
  data_protection_prep <- data
  
  loops_data_washington <- import("input/Raw_data_loops.xlsx",sheet="consent_controller_washington_g") 
  
  col_names <- c("consent_controller.washington_group_question.persons_having_difficulty_loop.difficulty_seeing",
                 "consent_controller.washington_group_question.persons_having_difficulty_loop.difficulty_hearing",
                 "consent_controller.washington_group_question.persons_having_difficulty_loop.difficulty_walking",
                 "consent_controller.washington_group_question.persons_having_difficulty_loop.difficulty_remembering", 
                 "consent_controller.washington_group_question.persons_having_difficulty_loop.difficulty_selftcare", 
                 "consent_controller.washington_group_question.persons_having_difficulty_loop.difficult_communication")
  
  data_protection_prep <- left_join(data_protection_prep, loops_data_washington %>% mutate_at(col_names,
          .funs = ~ case_when(
            .x %in% c("a_lot_difficulty", "cannot_do_at_all") ~ 1,
            TRUE ~ 0
          )
        ) %>% mutate(
          member_has_diff = ifelse(rowSums(.[col_names], na.rm = TRUE) >= 1, 1, 0)
        ) %>% group_by(`_parent_index`) %>% summarise(
          hh_members_with_diff = sum(member_has_diff),
        ) %>% select(index = `_parent_index`, hh_members_with_diff)) %>% mutate(
          pct_with_diff = hh_members_with_diff / hh_size
        )


  data_protection_prep <- data_protection_prep %>% 
    mutate( 
      sent_abroad_hh = rowSums(.[grep("main_safety_concerns_.*\\.sent_abroad", names(.))], na.rm = TRUE),
      pysical_harresment_hh = rowSums(.[grep("main_safety_concerns_.*\\.pysical_harresment", names(.))], na.rm = TRUE),
      verbal_haresment_hh = rowSums(.[grep("main_safety_concerns_.*\\.verbal_haresment", names(.))], na.rm = TRUE),
      sexual_harassment_hh = rowSums(.[grep("main_safety_concerns_.*\\.sexual_harassment", names(.))], na.rm = TRUE),
      recruited_armed_hh = rowSums(.[grep("main_safety_concerns_.*\\.recruited_armed", names(.))], na.rm = TRUE),
      forcibly_married_hh = rowSums(.[grep("main_safety_concerns_.*\\.forcibly_married", names(.))], na.rm = TRUE),
      exploited_hh = rowSums(.[grep("main_safety_concerns_.*\\.exploited", names(.))], na.rm = TRUE),
      discriminaition_hh = rowSums(.[grep("main_safety_concerns_.*\\.discriminaition", names(.))], na.rm = TRUE),
      detained_hh = rowSums(.[grep("main_safety_concerns_.*\\.detained", names(.))], na.rm = TRUE),
      fgm_hh = rowSums(.[grep("main_safety_concerns_.*\\.fgm", names(.))], na.rm = TRUE),
      killed_hh = rowSums(.[grep("main_safety_concerns_.*\\.killed", names(.))], na.rm = TRUE),
      mine_uxo_hh = rowSums(.[grep("main_safety_concerns_.*\\.mine_uxo", names(.))], na.rm = TRUE),
      injured_hh = rowSums(.[grep("main_safety_concerns_.*\\.injured", names(.))], na.rm = TRUE),
      dnk_prefer_not_answer_hh = rowSums(.[grep("main_safety_concerns_.*\\.(dnk$|prefer_not_answer$)", names(.))], na.rm = TRUE),
      members_with_diff_hh = rowSums(.[grep("_memb$", names(.))], na.rm = TRUE),
      )  
  


    data_protection_prep %>% mutate(
    #### protection indicator 1 ####
    ## % of HHs reporting concerns from any harm, physical threats or discrimination in the area where they are living.
    protection_idicator1 = case_when(
      ## being killed, Mine/UXOs, Being injured/killed by an explosive hazard
      killed_hh >= 1 | mine_uxo_hh  >= 1 | injured_hh >= 1 ~ 5,
      
      ## Discrimination or persecution , Being detained,Female Genital Mutilation (FGM)
      discriminaition_hh >= 1 | detained_hh  >= 1 |  fgm_hh >= 1 ~ 4,
      
      ## Suffering from physical harassment or violence (not sexual), 
      ## Suffering from verbal harassment Suffering from sexual harassment or violence
      ## Being recruited by armed groupsBeing forcibly marriedBeing exploited Being robbed
      
      pysical_harresment_hh >= 1 | verbal_haresment_hh >= 1 |  sexual_harassment_hh >= 1 |
        recruited_armed_hh >= 1 | forcibly_married_hh  >= 1 |  exploited_hh >= 1 ~ 3,
      
      ## being threatened with violence, Being sent abroad to find work
      sent_abroad_hh >= 1 ~ 2,
      
      ## DNK Prefer not answer (None of the above options is selected)
      TRUE  ~ 1,
      
    ),
    
  
    #### protection indicator 2 ####
    ## % of HHs without access to offical law enforcement authorities and/or judiciary system
    protection_idicator2 = case_when(
      ## 1. Yes – [no formal access to justice or compensation in my location]
      access_justice_denied == "formal_access" ~ 4,
      
      ## 2. Yes and No – [no formal access to justice or compensation in my location, but traditional/informal justice mechanisms available to resolve issues].3. No and Yes – [no access to traditional or informal justice mechanisms but access to formal justice or compensation mechanisms in my location]
      access_justice_denied %in% c("no_formal_access", "no_access") ~ 3,
      
      
      ## 4. No – [no issue linked to access to any justice mechanism arose]5. No – [full access to formal justice mechanisms and fair compensation]
      access_justice_denied %in% c("no_issue", "full_access") ~ 1,
      
    ),
    
    
    
    #### protection indicator 3 ####
    ## % of HHs that have experienced movement restrictions in the last 3 months
    protection_idicator3 = case_when(
      ## Yes
      safety_restrictions == "yes" ~ 4,
      
      ## No
      TRUE ~ 1,
      
      
      
    ),
    
    
    
    #### protection indicator 4 ####
    ## % of HHs by most common barriers to humanitarian aid
    protection_idicator4 = case_when(
      ## Insecurity On Route To Points Of Aid DistributionInsecurity At Site Of Aid DistributionExclusion By Camp Managers/Gatekeepers
      aid_barriers.insecurity_route == 1 |
        aid_barriers.insecurity_site == 1 |
        aid_barriers.exclusion == 1 ~ 4,
      
      ## Lack Of InformationPhysically Unable To Access Points Of Aid Distribution
      aid_barriers.lack_information == 1 |
        aid_barriers.physically_unable_access == 1 ~ 3,
      
      ## No problems faced
      # aid_barriers.no_problem == 1 ~ 1,
      TRUE ~ 1,
    ),
    
    
    
    #### protection indicator 5 ####
    ## % of individiuals in all households with at least one domain reportedly with A LOT OF DIFFICULTY or CANNOT DO AT ALL (disability level 3)
    protection_idicator5 = case_when(
      

      ## If all members of a HH have a disability (a lot of difficulty/cannot do at all)
      pct_with_diff == 1 ~ 5,
      
      ## If at least 50% of HH members have a disability (a lot of difficulty/cannot do at all)
      pct_with_diff >= 0.5 ~ 4 ,
      
      ## Households with at least one member identified as having a disability - any of the 6 difficulties" and answered "a lot of difficulty or cannot do at all"
      pct_with_diff >= 0 ~ 3 ,
      
      TRUE ~ 1
      
    ),
    
  ) %>% select(starts_with("protection_idicator")) -> res
  
  
  colnames(res) <- colnames(df)
  
  return(res)

  
}

compute_indicators_snfi <- function(data, indicators) {
  
  
  df <- create_indicators_placeholder(data, indicators)
  
  shlter_loop <-  import("input/Raw_data_loops.xlsx",sheet="consent_controller_shelter_many")
  
  shelters_repeat <- shlter_loop %>% group_by(`_parent_index`) %>% summarise(
    x1 = paste(consent_controller.shelter.many_shelters.shelter_types,collapse = "#"),
    x2 = fn_select_one_mode (consent_controller.shelter.many_shelters.shelter_types)
  ) 
  
  data_snfi_prep <- left_join(data, shelters_repeat %>% select(index=`_parent_index`,shelter_type=x2) )
  
  shelter_average_size = data.frame(shelter_type = c("brick", "cgi", "buul", "mud", "timer_", "stone", "unfinished", "stick", "collective", "tent", "none", "not_sure"),
                                    avg_size_shelter = c(15, 16, 6, 20, 20, 15, 16, 9, 30, 16, 0, 0),
                                    stringsAsFactors = F
                                    )
  
  data_snfi_prep <- left_join(data_snfi_prep, shelter_average_size )
  
  data_snfi_prep$avg_size_shelter[is.na(data_snfi_prep$avg_size_shelter)] <- 0
  
  shlter_loop <- left_join(shlter_loop,data %>% select(`_parent_index` = index,population_group)) 
  
  
  shelters_repeat_HC <- shlter_loop %>% filter(population_group=="HC") %>% group_by(`_parent_index`) %>% summarise(
    x1 = paste(consent_controller.shelter.many_shelters.shelter_types,collapse = "#"),
    x2 = fn_select_one_mode_shelter_HC (consent_controller.shelter.many_shelters.shelter_types)
  )
  
  shelters_repeat_IDP <- shlter_loop %>% filter(population_group=="IDP") %>% group_by(`_parent_index`) %>% summarise(
    x1 = paste(consent_controller.shelter.many_shelters.shelter_types,collapse = "#"),
    x2 = fn_select_one_mode_shelter_IDP(consent_controller.shelter.many_shelters.shelter_types)
  ) 
  
  shelters_repeat_2 <- rbind(shelters_repeat_HC,shelters_repeat_IDP) %>% select(index =`_parent_index`,snfi_ind2_sev = x2)
  
  data_snfi_prep <- left_join(data_snfi_prep, shelters_repeat_2 )
  
  data_snfi_prep <- data_snfi_prep %>%
    mutate(
     shelt_count.sum_rooms = as.numeric(shelt_count.sum_rooms)) %>%
    mutate(
     surface_per_person = (shelt_count.sum_rooms * avg_size_shelter) / hh_size,
     nb_shelter_issues = rowSums(.[grep("^(shelter_enclosure_issue|shelter_damage|shelter_issues)\\.(?!(none$|dnk$|prefer_not_answer$))",names(.),perl = T)], na.rm = TRUE),
     HLP_problems = ifelse(rowSums(.[grep("^shelter_problems\\.(?!(none$|not_sure$))",names(.),perl = T)], na.rm = TRUE)>=1,"yes","no"),
     nb_available_items = rowSums(.[grep("^currently_access_nfi\\.",names(.))], na.rm = TRUE)
    ) 

  
  data_snfi_prep <- data_snfi_prep %>% mutate(
    #### SNFI indicator 1 ####
    ## SNFI: % of HHs having adequate living space
    SNFI_idicator1 = case_when(
      ## surface per person < 2m2
      how_many_shelters == 0 |
      surface_per_person < 2 ~ 5,
      
      ## 2m2 ≤ surface per person < 2.5m2
      surface_per_person < 2.5 ~ 4,
      
      ## 2.5m2 ≤ surface per person < 3.5m2
      surface_per_person < 3.5 ~ 3,
      
      ## 3.5m2 ≤ surface per person < 4.5m2
      surface_per_person < 4.5 ~ 2,
      
      ## surface per person ≥4.5m2
      surface_per_person >= 4.5 ~ 1,
      
    ), 
    
    
    
    #### SNFI indicator 2 ####
    ## SNFI: % of HHs living in sub-standard shelter
    SNFI_idicator2 = case_when(
      ## None (sleeping in open)OR (Shelter Type =”” AND No. of shelter = 0)
      how_many_shelters == 0  ~ 5,
      
      ## Buul in an IDP SiteORMakeshift shelter
      snfi_ind2_sev == "4" ~ 4,
      
      ## Unfinished / non-enclosed buildingORTent
      snfi_ind2_sev == "3" ~ 3,
      
      ## Timber and plastic sheet with CGI roofOR CGI sheet wall and CGI roofORMud and stick wall and CGI roofORCollective shelter(school, government buildin)ORIf Buul outside an IDP Site / Traditional somali house
      snfi_ind2_sev == "2" ~ 2,
      
      ## Brick and concrete house (solid, finished house or apartment)ORStone/brick wall and CGI roof
      snfi_ind2_sev == "1" ~ 1,
      
    ),
    
    
    
    #### SNFI indicator 3 ####
    ## SNFI: % of HHs living in inadequate shelter conditions.
    SNFI_idicator3 = case_when(
      ## No. of issues>11
      nb_shelter_issues > 11 ~ 5,
      
      ## >8No. of issues≤11
      nb_shelter_issues > 8 ~ 4,
      
      ## >5No. of issues≤8
      nb_shelter_issues > 5 ~ 3,
      
      ## >2 No. of issues≤5
      nb_shelter_issues > 2 ~ 2,
      
      ## <=2 Issues
      nb_shelter_issues <=2 ~ 1,
      
    ),
    
    
    
    #### SNFI indicator 4 ####
    ## SNFI: % of HHs having security of tenure issues
    SNFI_idicator4 = case_when(
      ## Occupancy arrangement  NOT "ownership"AND Has HLP Problem AND Does Not Have written documentation
      shelter_occupancy != "ownership" & HLP_problems == "yes" & shelter_format_doc != "yes"  ~ 3,
      
      ## Occupancy arrangement ="ownership" OR No HLP Problem OR Has written documentation
      shelter_occupancy == "ownership" | HLP_problems == "no" | shelter_format_doc == "yes"  ~ 2,
      
      ## Occupancy arrangement ="ownership" AND No HLP Problem AND Has written documentation
      shelter_occupancy == "ownership" & HLP_problems == "no" & shelter_format_doc == "yes" ~ 1,
      
    ),
    
    
    
    #### SNFI indicator 5 ####
    ## SNFI: % Households owning sufficient basic NFIs
    SNFI_idicator5 = case_when(
      ## Less than 3 items (0, 1 or 2)
      nb_available_items <3 ~ 5,
      
      ## 4 or 3 items present at HH level
      nb_available_items <5 ~ 4,
      
      ## 5 items present at HH level
      nb_available_items == 5  ~ 3,
      
      ## 6 items present at HH level
      nb_available_items == 6 ~ 2,
      
      ## All items available at HH level
      nb_available_items == 7 ~ 1,
      
    ),
    
  
  ) %>% select(starts_with("SNFI_idicator")) -> res
  
  
  colnames(res) <- colnames(df)
  
  return(res)
}

compute_indicators_gbv <- function(data, indicators) {
  
  df <- create_indicators_placeholder(data, indicators)
  
  gbv_cols = c("mental_healths", "gbv_servies", "legal_protec", "livelihoods_s")
  
  data_gbv_prep <- data
  
  binary_gbv_cols <- mutate_at(data_gbv_prep,gbv_cols,~ case_when(
    .x == "yes" ~ 1,
    TRUE ~ 0
  )) %>% rename_at(gbv_cols,~ paste0(.x,"_int")) %>% select(uuid,all_of(paste0(gbv_cols,"_int"))) 
  
  data_gbv_prep <- left_join(data_gbv_prep,binary_gbv_cols)

  
  data_gbv_prep <- data_gbv_prep %>%
    mutate(
      available_srvices_gbv_count = rowSums(.[grep(sprintf("^(%s).*_int$",gsub(" ","|",paste(gbv_cols, collapse=" "))),names(.))], na.rm = TRUE),
      unsafe_locations_girls_women_count = rowSums(.[grep("^unsafe_locations_girls_yes\\.(?!(no_unsafe_areas$|dnk$|decline_answer$))",names(.),perl = T)], na.rm = TRUE),
      barriers_mental_healths_binary = ifelse(rowSums(.[grep("^barriers_mental_healths\\.",names(.))], na.rm = TRUE)>=1,1,0),
      barriers_legal_protec_binary = ifelse(rowSums(.[grep("^barriers_legal_protec\\.",names(.))], na.rm = TRUE)>=1,1,0),
      barriers_livelihoods_s_binary = ifelse(rowSums(.[grep("^barriers_livelihoods_s\\.",names(.))], na.rm = TRUE)>=1,1,0),
      barriers_gbv_servies_binary = ifelse(rowSums(.[grep("^barriers_gbv_servies\\.",names(.))], na.rm = TRUE)>=1,1,0),
      barriere_fear_not_equal_access_shame_selected = ifelse(rowSums(.[grep("^barriers_.*\\.(fear$|not_equal_service$|shame$)",names(.))], na.rm = TRUE)>=1,TRUE,FALSE),
      barriere_lack_trust_selected = ifelse(rowSums(.[grep("^barriers_.*\\.lack_trust$",names(.))], na.rm = TRUE)>=1,TRUE,FALSE),
      
      ) %>% mutate(
         barriers_access_services_gbv_count = rowSums(.[grep(".*_binary$",names(.))], na.rm = TRUE)
       )
  
  # data_gbv_prep %>% select(grep("^barriers_.*\\.(fear$|not_equal_service$|shame$)",names(.)),barriere_fear_not_equal_access_selected) %>%View()
  # 
  # 
  # data %>% filter(gbv_servies == "yes") %>% select(starts_with("barriers_gbv_servies.")) %>% summarise_all(sum, na.rm=T) %>% melt() %>%  View()
  # data %>% filter(mental_healths == "yes") %>% select(starts_with("barriers_mental_healths.")) %>% summarise_all(sum, na.rm=T) %>% melt() %>%  View()
  # data %>% filter(legal_protec == "yes") %>% select(starts_with("barriers_legal_protec.")) %>% summarise_all(sum, na.rm=T) %>% melt() %>%  View()
  # data %>% filter(livelihoods_s == "yes") %>% select(starts_with("barriers_livelihoods_s.")) %>% summarise_all(sum, na.rm=T) %>% melt() %>%  View()
  
# 
#   res %>% select(GBV_idicator1,GBV_idicator2) %>% View()
#   
#   data %>% select(grep("^unsafe_locations_girls_yes\\.(?!(no_unsafe_areas$|dnk$|decline_answer$))",names(.),perl = T)) %>% colnames()
#   
#   
#   res %>% rowwise() %>%  mutate(
#     av = (GBV_idicator1+GBV_idicator2)/2,
#   ) %>% ungroup() %>% pull(av) %>% hist()
#   
#   data$barriers_mental_healths
  
  

  data_gbv_prep %>% mutate(
    #### GBV indicator 1 ####
    ## % of HHs with access to medical, legal and social services for women and girls
    GBV_idicator1 = case_when(
      ## Only 1 service available
      available_srvices_gbv_count <= 1 ~ 4,
      
      ## 2 services available
      available_srvices_gbv_count == 2  ~ 3,
      
      ## 3 services available
      available_srvices_gbv_count == 3 ~ 2,
      
      ## 4 services available
      available_srvices_gbv_count == 4 ~ 1,
      
    ),
    
    
    
    #### GBV indicator 2 ####
    ## % of HHs by most common barriers to accessing GBV services faced by women and girls
    GBV_idicator2 = case_when(
      
      ## Four or more barrier exist
      # barriers_access_services_gbv_count == 4 & 
      barriere_fear_not_equal_access_shame_selected ~ 4,
      
      ## Three barriers identified
      #barriers_access_services_gbv_count == 3 & 
      barriere_lack_trust_selected ~ 3,
      
      ## one or two barriers identified
      barriers_access_services_gbv_count >= 1 ~ 2,
      
      ## No barriers identified
      barriers_access_services_gbv_count == 0 ~ 1,
      
    ),
    
    
    
    #### GBV indicator 3 ####
    ## % of HHs  with women and girls reporting lack of freedom to attend go about their duties/businessess
    GBV_idicator3 = case_when(
      ## No
      women_move_freely == "no" ~ 4,
      
      ## Yes
      women_move_freely == "yes" ~ 1,
      
    ),
    
    
    
    #### GBV indicator 4 ####
    ## % of HH in which women and girls/men and boys avoid areas because they feel unsafe there
    GBV_idicator4 = case_when(
      ## Women and girls avoid four or more area because they feel unsafe
      unsafe_locations_girls_women_count >= 4 ~ 5,
      
      ## Women and girls avoid three areas because they feel unsafe
      unsafe_locations_girls_women_count == 3 ~ 4,
      
      ## Women and girls avoid two areas because they feel unsafe
      unsafe_locations_girls_women_count == 2 ~ 3,
      
      ## Women and girls avoid one area because they feel unsafe
      unsafe_locations_girls_women_count == 1 ~ 2,
      
      ## Women and girls do not avoid areas
      unsafe_locations_girls_yes.no_unsafe_areas == 1 |
        unsafe_locations_girls == "no"  ~ 1,
      
    ),
    
    
  ) %>% select(starts_with("GBV_idicator")) -> res
  

  
  colnames(res) <- colnames(df)
  
  return(res)
  
}

compute_indicators_hlp <- function(data, indicators) {
  
  df <- create_indicators_placeholder(data, indicators)
 
  data_hlp_prep <- data
  
  data_hlp_prep %>% select(starts_with("hlp.land_dispute_yes.")) %>% colnames()
  
  data_hlp_prep <- data %>%
    mutate(
     land_disputes_count = rowSums(.[grep("hlp\\.land_dispute_yes\\.", names(.))], na.rm = TRUE),
        ) 
  

  data_hlp_prep %>% mutate(
    #### HLP indicator 1 ####
    ## % of household reporting HLP disputes
    HLP_idicator1 = case_when(
      ## v.Multiple claims
      hlp.land_dispute == "yes" &
        hlp.land_dispute_yes.multiple_claims == 1 ~ 5,
      
      ## i. encroachment dispute, ii. Boundary dispute,iii. Illegal occupation, iv. Land grabbing
      land_disputes_count >= 1 ~ 4,
      
      ## No issue reported
      TRUE ~ 1
      
    ), 
    
    
    
    #### HLP indicator 2 ####
    ## % of households accessing their housing/shelter with security of tenure
    HLP_idicator2 = case_when(
      ## Yes
      risk_eviction == "yes" ~ 3,
      
      ## No
      TRUE ~ 1
      
    ),
    
    
    
    #### HLP indicator 3 ####
    ## % of HH with at least one HH member without an ID document
    HLP_idicator3 = case_when(
      ## No: None of the persons in the household  have a valid ID document;
      persons_with_valid_ids == "no_one" ~ 3,
      
      ## Maybe: at least one person in the household does not have a valid ID document;
      persons_with_valid_ids == "no" ~ 2,
      
      ## Yes: every person in the household has a valid ID document; DNK or Decline to Answer
      TRUE  ~ 1,
      
    ), ) %>% select(starts_with("HLP_idicator")) -> res
  
  res$HLP_idicator3 %>% table()
  
  colnames(res) <- colnames(df)
  
  return(res)
  
}

compute_indicators_cp <- function(data, indicators) {
  
  df <- create_indicators_placeholder(data, indicators)
  
  data_cp_prep <- data
  
  col_names <- c("mental_health_supports_boys",
    "mental_health_supports_girls",
    "social_services_boys", 
    "social_services_girls", 
    "support_group_activites_boys",
    "support_group_activites_girls")

  
  
  data_cp_prep <- left_join(data_cp_prep,data_cp_prep %>%
              mutate_at(col_names,
                        .funs = ~ case_when(
                          .x == "yes" ~ 1,
                          TRUE ~ 0
                        )
              ) %>%
              mutate(
                mental_health_supports_boys_and_girl =  ifelse(rowSums(.[grep("^mental_health_supports_(boys|girls)$", names(.))], na.rm = TRUE)>=1,1,0),
                social_services_boys_and_girl =  ifelse(rowSums(.[grep("^social_services_(boys|girls)$", names(.))], na.rm = TRUE)>=1,1,0),
                support_group_activites_boys_and_girl =  ifelse(rowSums(.[grep("^support_group_activites_(boys|girls)$", names(.))], na.rm = TRUE)>=1,1,0)) %>%
                mutate(
                cp_services_accesible =  rowSums(.[grep("_boys_and_girl$", names(.))], na.rm = TRUE)
              ) %>% select(uuid,cp_services_accesible))
  


  data_cp_prep <- data_cp_prep %>%
    mutate(
      nb_children_working = rowSums(.[grep("^children_working\\.(?!(number_children_working))",names(.),perl = T)], na.rm = TRUE),
      hh_children_int = as.numeric(hh_children),
      hh_has_protection_barrier_dont_know_services_avialable = ifelse(rowSums(.[grep("^child_prot_barriers_.*\\.dont_know_services_avialable$",names(.))], na.rm = TRUE)>=1,TRUE,FALSE),
      hh_has_protection_barrier_parrents_allow = ifelse(rowSums(.[grep("^child_prot_barriers_.*\\.parrents_allow$",names(.))], na.rm = TRUE)>=1,TRUE,FALSE),
      hh_has_protection_barrier_bussy_hh_chore = ifelse(rowSums(.[grep("^child_prot_barriers_.*\\.bussy_hh_chore$",names(.))], na.rm = TRUE)>=1,TRUE,FALSE),
      hh_has_protection_barrier_difficulties_reach = ifelse(rowSums(.[grep("^child_prot_barriers_.*\\.difficulties_reach$",names(.))], na.rm = TRUE)>=1,TRUE,FALSE),
      hh_has_protection_barrier_too_long_wait = ifelse(rowSums(.[grep("^child_prot_barriers_.*\\.too_long_wait$",names(.))], na.rm = TRUE)>=1,TRUE,FALSE),
      hh_has_protection_barrier_not_good_quality = ifelse(rowSums(.[grep("^child_prot_barriers_.*\\.not_good_quality$",names(.))], na.rm = TRUE)>=1,TRUE,FALSE),
      hh_has_protection_barrier_services_not_accessible = ifelse(rowSums(.[grep("^child_prot_barriers_.*\\.services_not_accessible$",names(.))], na.rm = TRUE)>=1,TRUE,FALSE),
      hh_has_protection_barrier_discriminated = ifelse(rowSums(.[grep("^child_prot_barriers_.*\\.discriminated$",names(.))], na.rm = TRUE)>=1,TRUE,FALSE),
      hh_has_protection_barrier_safety_road = ifelse(rowSums(.[grep("^child_prot_barriers_.*\\.safety_road$",names(.))], na.rm = TRUE)>=1,TRUE,FALSE),
      hh_has_protection_barrier_safety_repisals = ifelse(rowSums(.[grep("^child_prot_barriers_.*\\.safety_repisals$",names(.))], na.rm = TRUE)>=1,TRUE,FALSE),
      hh_has_protection_barrier_safety_trust = ifelse(rowSums(.[grep("^child_prot_barriers_.*\\.safety_trust$",names(.))], na.rm = TRUE)>=1,TRUE,FALSE),
      hh_has_protection_barrier_distance = ifelse(rowSums(.[grep("^child_prot_barriers_.*\\.distance$",names(.))], na.rm = TRUE)>=1,TRUE,FALSE),
      hh_has_protection_barrier_alwasys_services_not_functioning = ifelse(rowSums(.[grep("^child_prot_barriers_.*\\.alwasys_services_not_functioning$",names(.))], na.rm = TRUE)>=1,TRUE,FALSE),
      hh_has_protection_barrier_lack_informaiton = ifelse(rowSums(.[grep("^child_prot_barriers_.*\\.lack_informaiton$",names(.))], na.rm = TRUE)>=1,TRUE,FALSE),
      hh_protection_barrier_count = rowSums(.[grep("child_prot_barriers_.*\\.(?!(other))",names(.),perl = T)], na.rm = TRUE)
      
    ) 
  
  data_cp_prep %>% filter()
  data_cp_prep$hh_protection_barrier_count
  
  data_cp_prep %>% mutate(
    #### CP indicator 1 ####
    ## Availability of child protection services
    CP_idicator1 = case_when(
      ## No Service
      cp_services_accesible == 0 ~ 5,
      
      ## 1 Services
      cp_services_accesible == 1 ~ 4,
      
      ## 2 Services
      cp_services_accesible == 2 ~ 3,
      
      ## 3 Services
      cp_services_accesible == 3 ~ 1,
      
    ),
    
  
    
    #### CP indicator 2 ####
    ## % of HHs by most common barriers to accessing child protection services faced by boys and girls
    CP_idicator2 = case_when(
      ## TODO : update descritions 
      ## Four or more barrier exist
      hh_has_protection_barrier_discriminated | 
        hh_has_protection_barrier_services_not_accessible |
        hh_has_protection_barrier_safety_road |
        hh_has_protection_barrier_safety_repisals |
        hh_has_protection_barrier_safety_trust  ~ 4,
      
      ## Three barriers identified
      hh_has_protection_barrier_lack_informaiton |
        hh_has_protection_barrier_bussy_hh_chore |
        hh_has_protection_barrier_not_good_quality ~ 3,
      
      ## one or two barriers identified
      hh_protection_barrier_count >=1 ~ 2,
      
      ## No barriers identified
      hh_protection_barrier_count == 0 ~ 1,
      
    ),
    
    
    #### CP indicator 3 ####
    ## % of children (17 and under) in HH with permanent, temporary and daily jobs
    ## % of households reporting the presence of children engaged in child labor outside of the home in the past 30 days
    CP_idicator3 = case_when(
      ## All Children(6-14) or (15-17) engaged in Child Labour
      hh_children_int != 0 & (nb_children_working == hh_children_int) ~ 5,
      
      ## Some Children (6-14) or (15-17) engaged in Child Labour
      hh_children_int != 0 & nb_children_working > 1  ~ 4,
      
      ## 1 Child (6-14) or (15-17) engaged in Child Labour
      nb_children_working == 1 ~ 3,
      
      ## None Reported
      TRUE ~ 1,
      
    ),) %>%  select(starts_with("CP_idicator")) -> res

  
  colnames(res) <- colnames(df)
  
  return(res)
  
}

compute_indicators_eh <- function(data, indicators) {
  
  df <- create_indicators_placeholder(data, indicators)
  
  data_eh_prep <- data

  

  
  data_eh_prep %>% mutate(
    #### EH indicator 1 ####
    ## % of HH being affected by explosive ordnance in the last 12 months
    EH_idicator1 = case_when(
      ##- Yes, at least one household member has been injured or killed by an explosive ordnance 
      ##- Yes, the presence of explosive ordnance has affected access to health centers
      ##- Yes, the presence of explosive ordnance has affected the ability of children to go to school
      effect_explosive_hazards_yes.affected_access_school == 1|
        effect_explosive_hazards_yes.affected_access_health == 1|
        effect_explosive_hazards_yes.injured_killed == 1 ~ 5,
      
      ## - Yes, the presence of explosive ordnance has affected livelihoods opportunities
      ## - Yes, the presence of explosive ordnance has affected access to markets
      effect_explosive_hazards_yes.affected_livehood == 1|
        effect_explosive_hazards_yes.affected_access_market == 1 ~ 4,
      
      ## - Yes, the presence of explosive ordnance has affected freedom of movement
      effect_explosive_hazards_yes.affected_freedom == 1 ~ 3,
      
      ## - No, the household has not been affected by the presence of explosive ordnance
      # effect_explosive_hazards_yes.not_been_affected == 1 |
      #   effect_explosive_hazards != "yes" ~ 1,
      TRUE ~ 1 ,
      
      
    ) ) %>%  select(starts_with("EH_idicator")) -> res
  
  
  colnames(res) <- colnames(df)
  
  
  return(res)
  
}

compute_indicators_cccm <- function(data, indicators) {
  
  df <- create_indicators_placeholder(data, indicators)
  
  data_cccm_prep <- data
  

  data_cccm_prep <- data_cccm_prep %>%
    mutate(

    ) 
  
  data_cccm_prep %>% mutate(
    #### CCCM indicator 1 ####
    ## % of households accessing their housing/shelter with security of tenure
    CCCM_idicator1 = case_when(
      ## Yes
      risk_eviction == "yes" ~ 4,
      
      ## No
      TRUE ~ 1,
      
    ),
    
    
    
    #### CCCM indicator 2 ####
    ## % of HHs who received aid in the past 30 days
    CCCM_idicator2 = case_when(
      ## Yes
      receive_aid_past_30_days == "yes" ~ 3,
      
      ## No
      TRUE ~ 1,
      
    ),
    
    
    
    #### CCCM indicator 3 ####
    ## % of population in sites requiring access to functioning complaints and feedback mechanisms
    CCCM_idicator3 = case_when(
      ## No
      know_crm_platform == "yes" ~ 3,
      
      ## Yes
      TRUE ~ 1,
      
    ),
    
    
    
    #### CCCM indicator 4 ####
    ## % of the site population who are satisfied with the opportunities they have to influence site decisions
    CCCM_idicator4 = case_when(
      ## No
      influence_decision == "no" ~ 3,
      
      ## Yes
      TRUE ~ 1,
      
    ),
    
    
  )  %>%  select(starts_with("CCCM_idicator")) -> res
  
  
  colnames(res) <- colnames(df)
  
  return(res)
  
}

compute_indicators_health <- function(data, indicators) {
  
  df <- create_indicators_placeholder(data, indicators)
  
  data_health_prep <- data
  
  
  data_health_prep <- data_health_prep %>%
    mutate(
      
    ) 
  
  data_health_prep %>% mutate(
    #### Health indicator 1 ####
    ## Percentage of population that can access primary healthcare within one hour’s walk from dwellings
    health_idicator1 = case_when(
      ## Access to primary healthcare exceeding 60 minutes’ walk for HH.
      health_transportion == "foot" & time_nearest_health > 60 ~ 3,
      
      ## Access to primary healthcare within 60 minutes’ walk for HH.
      TRUE ~ 1,
      
    )
    
    
  )  %>%  select(starts_with("health_idicator")) -> res
  
  
  colnames(res) <- colnames(df)
  
  return(res)
  
}

