rm(list = ls())

if (!require("pacman")) install.packages("pacman")

p_load(
  rio,
  tidyverse,
  crayon,
  hypegrammaR,
  composr,
  sjmisc
  )

############ Data preparation ############ 

source("./src/functions/pin_functions.R")
source("./src/functions/functions.R")
source("./src/functions/functinos_other.R")

##### To generate the data.RDS file and decrypt the dataset use :
## key <- cyphr::key_sodium(readRDS("key"))
## cyphr::decrypt_file("input/data.RDS.encrypted", key, "input/data.RDS")
##### Make sure cyphr library is installed


clean_data <- readRDS("input/data.RDS") 

data <- clean_data %>% filter(consensus == "yes")

xml_names = c("abdulaziz", "aden_yabaal", "afgooye", "afmadow", "baardheere", "badhaadhe", "badhan", "baidoa", "baki", "balcad", "bandarbayla", "baraawe", "belet_weyne", "belet_xaawo", "berbera", "boondheere", "borama", "bossaso", "buaale", "bulo_burto", "burco", "burtinle", "buuhoodle", "buur_hakaba", "cabudwaaq", "cadaado", "cadale", "caluula", "caynabo", "ceel_afweyn", "ceel_barde", "ceel_buur", "ceel_dheer", "ceel_waaq", "ceerigaabo", "daynile", "dharkenley", "dhuusamarreeb", "diinsoor", "doolow", "eyl", "gaalkacyo_north", "gaalkacyo_south", "galdogob", "garbahaarey", "garowe", "gebiley", "hamar_jaab_jab", "hamar_weyne", "hargeysa", "hawl_wadaag", "heliwa", "hobyo", "hodan", "iskushuban", "jalalaqsi", "jamaame", "jariiban", "jowhar", "kahda", "karaan", "kismayo", "kurtunwaarey", "laas_caanood", "laasqoray", "lughaye", "luuq", "marka", "mataban", "owdweyne", "qandala", "qansax_dheere", "qardho", "qoryooley", "rab_dhuure", "sablaale", "shangaani", "sheikh", "shibis", "taleex", "tayeeglow", "waaberi", "waajid", "wadajir", "wanla_weyn", "wardhiigleey", "xudun", "xudur", "yaaqshiid", "zeylac")
pop_names = c("Banadir", "Adan_Yabaal", "Afgooye", "Afmadow", "Baardheere", "Badhaadhe", "Laasqoray", "Baidoa", "Baki", "Balcad", "Bandarbayla", "Baraawe", "Belet_Weyne", "Belet_Xaawo", "Berbera", "Banadir", "Borama", "Bossaso", "Bu'aale", "Bulo_Burto", "Burco", "Burtinle", "Buuhoodle", "Buur_Hakaba", "Cabudwaaq", "Cadaado", "Cadale", "Caluula", "Caynabo", "Ceel_Afweyn", "Ceel_Barde", "Ceel_Buur", "Ceel_Dheer", "Ceel_Waaq", "Ceerigaabo", "Banadir", "Banadir", "Dhuusamarreeb", "Diinsoor", "Doolow", "Eyl", "Gaalkacyo", "Gaalkacyo", "Galdogob", "Garbahaarey", "Garowe", "Gebiley", "Banadir", "Banadir", "Hargeysa", "Banadir", "Banadir", "Hobyo", "Banadir", "Iskushuban", "Jalalaqsi", "Jamaame", "Jariiban", "Jowhar", "Banadir", "Banadir", "Kismayo", "Kurtunwaarey", "Laas_Caanood", "Laasqoray", "Lughaye", "Luuq", "Marka", "Belet_Weyne", "Owdweyne", "Qandala", "Qansax_Dheere", "Qardho", "Qoryooley", "Rab_Dhuure", "Sablaale", "Banadir", "Sheikh", "Banadir", "Taleex", "Tayeeglow", "Banadir", "Waajid", "Banadir", "Wanla_Weyn", "Xardheere", "Xudun", "Xudur", "Banadir", "Zeylac")

district_names_matching = data.frame(district=xml_names,
                                     pop_names=pop_names,
                                     stringsAsFactors = F)


data$district = left_join(data,district_names_matching) %>% pull(pop_names)


# Convert Logical to Numeric Dummy
data <- mutate_if(data, is.logical, as.numeric)

questions <- import("input/tool/SOM_JMCNA_HH_Tool_2021_v5.xlsx", sheet = "survey") %>%
  select(-1) %>%
  filter(!is.na(name))

choices <- import("input/tool/SOM_JMCNA_HH_Tool_2021_v5.xlsx", sheet = "choices")

questionnaire <- load_questionnaire(
  data = data,
  questions = questions,
  choices = choices,
  choices.label.column.to.use = "label::English"
)

data <- data %>% mutate(
  population_group = case_when(
    idp_settlement == "yes" ~ "IDP",
    idp_settlement == "no" ~ "HC"
  ),
  
  strata = paste0(district,"_",population_group)
)

rm(list = c("clean_data","district_names_matching"))


############ Education ###########
education_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="Education")

indicators = education_template %>% filter(!is.na(Indicators)) %>% pull("Indicators")

education_df <- generate_template(
  cluster_name = education_template[1, 1],
  questions = education_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)

edu_indicators <- compute_indicators(data,"Education",indicators)

education_df <- replace_columns(education_df, edu_indicators ) 

export(education_df, "output/pin_education.xlsx", na = "")


############ Wash ###########
wash_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="Wash")


indicators = wash_template %>% filter(!is.na(Indicators)) %>% pull("Indicators") %>% .[-c(6,7,8)]


wash_df <- generate_template(
  cluster_name = wash_template[1, 1],
  questions = wash_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)

wash_indicators <- compute_indicators(data,"Wash",indicators)

wash_df <- replace_columns(wash_df, wash_indicators ) 

export(wash_df, "output/pin_wash.xlsx", na = "")

############ Protection ###########

protection_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="Protection")

indicators = protection_template %>% filter(!is.na(Indicators)) %>% pull("Indicators")

protection_df <- generate_template(
  cluster_name = protection_template[1, 1],
  questions = protection_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)

protection_indicators <- compute_indicators(data,"Protection",indicators)

protection_df <- replace_columns(protection_df, protection_indicators) 

export(protection_df, "output/pin_protection.xlsx", na = "")

# protection_df_scores <- protection_df %>% select(region,Key,idp_settlement,all_of(indicators)) 
# protection_df_scores <- protection_df_scores %>% mutate(
#   ##### calculation of the HH score goes in here
#   hh_score_ceiling = ceiling(rowMeans(.[-grep("^(region|Key|idp_settlement)$", names(.))], na.rm = T)),
# ) 



############ SNFI ###########

SNFI_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="SNFI")


indicators = SNFI_template %>% filter(!is.na(Indicators)) %>% pull("Indicators")

SNFI_df <- generate_template(
  cluster_name = SNFI_template[1, 1],
  questions = SNFI_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)

SNFI_indicators <- compute_indicators(data,"SNFI",indicators)

SNFI_df <- replace_columns(SNFI_df, SNFI_indicators) 

export(SNFI_df, "output/pin_SNFI.xlsx", na = "")


############ GBV ###########

gbv_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="GBV AoR")


indicators = gbv_template %>% filter(!is.na(Indicators)) %>% pull("Indicators")

gbv_df <- generate_template(
  cluster_name = gbv_template[1, 1],
  questions = gbv_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)


gbv_indicators <- compute_indicators(data,"GBV_AoR",indicators)

gbv_df <- replace_columns(gbv_df, gbv_indicators) 

export(gbv_df, "output/pin_GBV_AoR.xlsx", na = "")

############ HLP ###########

hlp_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="HLP AoR")


indicators = hlp_template %>% filter(!is.na(Indicators)) %>% pull("Indicators")

hlp_df <- generate_template(
  cluster_name = hlp_template[1, 1],
  questions = hlp_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)

hlp_indicators <- compute_indicators(data,"HLP_AoR",indicators)

hlp_df <- replace_columns(hlp_df, hlp_indicators) 

export(hlp_df, "output/pin_HLP.xlsx", na = "")



############ CP ###########

CP_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="CP AoR")


indicators = CP_template %>% filter(!is.na(Indicators)) %>% pull("Indicators")

CP_df <- generate_template(
  cluster_name = CP_template[1, 1],
  questions = CP_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)

CP_indicators <- compute_indicators(data,"CP_AoR",indicators)

CP_df <- replace_columns(CP_df, CP_indicators) 

export(CP_df, "output/pin_CP.xlsx", na = "")




############ EH ###########

EH_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="EH AoR")

indicators = EH_template %>% filter(!is.na(Indicators)) %>% pull("Indicators")

EH_df <- generate_template(
  cluster_name = EH_template[1, 1],
  questions = EH_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)

EH_indicators <- compute_indicators(data,"EH_AoR",indicators)

EH_df <- replace_columns(EH_df, EH_indicators) 

export(EH_df, "output/pin_EH_AoR.xlsx", na = "")



############ CCCM ###########
CCCM_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="CCCM")

indicators = CCCM_template %>% filter(!is.na(Indicators)) %>% pull("Indicators")

CCCM_df <- generate_template(
  cluster_name = CCCM_template[1, 1],
  questions = CCCM_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)

cccm_indicators <- compute_indicators(data,"CCCM",indicators)

CCCM_df <- replace_columns(CCCM_df, cccm_indicators) %>% filter(`Population group` == "IDP")

export(CCCM_df, "output/pin_CCCM.xlsx", na = "")





############ Health ###########
Health_template <- import("input/PIN_HNO_Indicators.xlsx",sheet="Health")

indicators = Health_template %>% filter(!is.na(Indicators)) %>% pull("Indicators")

Health_df <- generate_template(
  cluster_name = Health_template[1, 1],
  questions = Health_template %>% filter(!is.na(Questions))%>% pull("Questions"),
  indicators = indicators
)

Health_indicators <- compute_indicators(data,"Health",indicators)

Health_df <- replace_columns(Health_df, Health_indicators) 

export(Health_df, "output/pin_Health.xlsx", na = "")

