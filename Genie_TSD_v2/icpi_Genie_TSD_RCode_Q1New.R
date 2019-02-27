memory.limit(size=10000000)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Program Details ~~~~~~~====================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Program details 
#   Programmer: Imran Mujawar
#   Date started: 02/11/2019
#   Date updated: 
#   Program: Integrates the following datasets for TSD tool
#           1) Genie Site by IM
#           2) NAT_SUBNAT data
#   NOTE: This program uses the new MSD long structure
#           
# ---------------------------------------------------------------


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Load required packages ~~~~~~~==================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


if(!require(devtools)){
  install.packages("devtools")
}
library(devtools)

if(!require(ICPIutilities)){
  devtools::install_github("ICPI/ICPIutilities")
}
library(ICPIutilities)


if(!require(devtools)){
  install.packages("devtools")
}
library(devtools)


if(!require(eply)){
  install.packages("eply")
}
library(eply)

if(!require(stringr)){
  install.packages("stringr")
}
library(stringr)

if(!require(tidyverse)){
  install.packages("tidyverse")
}
library(tidyverse)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: Load required packages ~~~~~~~==================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Creating basic functions to show top few rows of data
View50 <- function(x){View(x[1:50,])}
View100 <- function(x){View(x[1:100,])}

# Creating the 'not in' function
`%ni%` <- Negate(`%in%`) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ======== Pulling in all NAT_SUBNAT dataset ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Pulling in the Genie data ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Getting the Genie dataset file names
glist <- list.files("../RawData/"
                    , pattern="genie|Genie|DHIS2")


  gfile <- unzip(paste("../RawData/Zipped", glist[1], sep=""), 
                 list = TRUE) %>% .$Name
  #extract file from zipped folder
  unzip(paste("../RawData/", glist[1], sep=""), exdir = "../RawData/unzipped")
  
  gfile_loc <- paste("../RawData/unzipped/", gfile, sep="") 

# Rough data pull to get variable names and assign datatype
foo <- read_tsv(file=gfile_loc, 
                col_names = TRUE, n_max = 0)   

foonames <- tolower(names(foo))

file.remove(gfile_loc)

# Checking whether the zipped file is a Genie file
msdvars <- c("psnu", "psnuuid", "approvallevel")
if(!all(msdvars %in% foonames)){
  note2 <- "File doesn't seem like a Genie file, since it has some variables missing!"
  stop("Fake Genie! This is not a Genie file... Please check uploaded file")
} else {"The Genie is out of the bottle! Make your wishes come true!!"}

rm(foo)

# Creating the vector of column types
colvecx <- as.vector(ifelse(foonames[grepl("fy", foonames)], "d", "c"))

# Creating the vector of column types
colvecx <- as.vector(ifelse(foonames %in% c("fy2018_targets",          
                                            "fy2018q1"      ,           
                                            "fy2018q2"      ,           
                                            "fy2018q3"      ,           
                                            "fy2018q4"      ,           
                                            "fy2018apr"     ,           
                                            "fy2019_targets",           
                                            "fy2019q1"), "d", "c"))
colvec <- paste(colvecx, collapse = '')



# Pulling in the data with correct datatype for variables  
datim <- read_tsv(file=gfile_loc, 
                  col_names = TRUE,
                  col_types = colvec)      # ending if-else for Genie check

names(datim) <- tolower(names(datim))  


# Reading in the reconciled NAT_SUBNAT dataset created by Abe
art <- read_csv("../RawData/ART_Coverage_2018-12-19.csv",
                col_types = cols(region = "c",
                                 operatingunit = "c",
                                 countryname = "c",
                                 snu1 = "c",
                                 psnu = "c",
                                 psnuuid = "c",
                                 HTS_TST_POS = "d",
                                 PLHIV = "d",
                                 TX_CURR = "d",
                                 TX_CURR_SUBNAT = "d",
                                 ART_COVERAGE = "d")) 

natx <- art %>% select(-HTS_TST_POS, -ART_COVERAGE, -TX_CURR)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ======== END: Pulling in all NAT_SUBNAT dataset ~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ======== Pulling in Global Site-IM R dataset ~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



names(datim) <- tolower(names(datim))  

# Pulling in NAT_SUBNAT data ~~~~~~~~~~~~~~~~~~~~~~
# getting the ou variable from the Genie extract
# to subset the global Nat-Subnat dataset
ou <- unique(datim$operatingunit)
natou <- unique(natx$operatingunit)


# Checking if OU names match  

setdiff(ou, natou)
setdiff(natou, ou)


oulist <- ou[c(7:35)]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ======== END: Pulling in Global Site-IM R dataset ============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Setting up date and time variables ~~~~~~~======
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

curr_year <- identifypd(datim, pd_type = "year")
curr_q    <- identifypd(datim, pd_type = "quarter")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: Setting up date and time variables ~~======
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Looping through all OUs  ~~~~~~~======
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for (x in oulist) {

  # # Keeping only data for country of interest
  datimx <- datim %>% filter(operatingunit %in% x) 

  #Vector of string variables
  char_varx <- names(datimx %>% select_if(is.character))
  # getting rid of SNU prioritization
  char_var <- char_varx[!grepl("prioritization", char_varx)]
  
  
  #Vector of numeric variables
  num_var <- names(datimx %>% select(starts_with("fy")))
  curr_fy_res <- names(datimx %>% select(starts_with(paste("fy", curr_year, "q", sep=""))))
  prev_fy_res <- names(datimx %>% select(starts_with(paste("fy", curr_year-1, "q", sep=""))))
  
  yr <- paste("fy", as.character(curr_year), sep="")    
  prev_yr <- paste("fy", as.character(curr_year-1), sep="")
  
  curr_tar <- num_var[grepl(paste(yr, "target", sep="_"), num_var)]
  
  yr_q <- paste("fy", as.character(curr_year), "q", as.character(curr_q), sep="")
  p_yr_q <- paste("fy", as.character(curr_year-1), "q", 4, sep="")
  
  
  # List of indicators in the TSD
  indlist <- c("HTS_TST_POS", 
               "HTS_TST",
               "TX_CURR", 
               "TX_NEW", 
               "TX_RET", 
               "TX_PVLS")
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Restructuring MER dataset    ~~~~~~~============
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  
  #### restructuring FactView dataset into long format 
  datim1 <- datimx %>% 
    # filter for indicators needed for TSD
    filter(indicator %in% indlist) %>%
    select(char_var, num_var) %>% 
    gather(period, value, num_var) %>%
    # Removing null values 
    filter(!is.na(value)) %>% 
    mutate(colvar = case_when(
      indicator %in% c("TX_CURR","TX_RET","TX_PVLS") & period %in% yr_q        ~ "curr_result",
      indicator %in% c("TX_CURR","TX_RET","TX_PVLS") & period %in% p_yr_q      ~ "prev_result",
      indicator %ni% c("TX_CURR","TX_RET","TX_PVLS") & period %in% curr_fy_res ~ "curr_result",
      indicator %ni% c("TX_CURR","TX_RET","TX_PVLS") & period %in% prev_fy_res ~ "prev_result",
      period %in% curr_tar                                                     ~ "curr_target",
      TRUE ~ "X"
    )) %>% 
    filter(colvar!="X") %>%
    select(char_var, colvar, value) %>% 
    group_by_if(is.character) %>% 
    summarize(value = sum(value, na.rm=T)) %>% 
    ungroup()
  
  
  # Creating the F_C variables MCAD and Total N
  fact2c <-  datim1 %>%
    mutate(F_C =  
             case_when(
               indicator %ni% c("TX_RET","TX_PVLS")                                    ~
                 case_when(
                   standardizeddisaggregate %in% c("Total Numerator")                  ~ "N",
                   ismcad   ==    "Y"                                 ~ "M"),
               indicator %in% c("TX_RET","TX_PVLS")          ~
                 case_when(
                   standardizeddisaggregate %in% c("Total Numerator",
                                                   "Total Denominator",
                                                   "12mo/HIVStatus")                ~ "N",
                   standardizeddisaggregate %in% c("Age Aggregated/Sex",
                                                   "Age Aggregated/Sex/Indication/HIVStatus")               ~ "M"))) %>% 
    rename(age = agecoarse) %>% 
    filter(!is.na(F_C))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Semi-fine data 
  fact2sf <- datim1 %>% 
    mutate(F_C =
             case_when(
               indicator %ni% c("TX_RET", "TX_PVLS")          ~
                 case_when(
                   standardizeddisaggregate %in% c("AgeAboveTen/Sex/Positive",  
                                                   "AgeLessThanTen/Positive",  
                                                   "Age/Sex/HIVStatus",
                                                   "Modality/Age/Sex/Result")         ~ "sF"),
               
               indicator %in% c("TX_RET", "TX_PVLS")          ~
                 case_when(
                   standardizeddisaggregate %in% c("Age/Sex",
                                                   "Age/Sex/Indication/HIVStatus")    ~ "sF"))) %>% 
    rename(age = agesemifine) %>% 
    filter(!is.na(F_C))
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fine data 
  fact2f <- datim1 %>% 
    mutate(F_C =
             case_when(
               indicator %ni% c("TX_RET", "TX_PVLS")          ~
                 case_when(
                   standardizeddisaggregate %in% c("AgeAboveTen/Sex/Positive",  
                                                   "AgeLessThanTen/Positive",  
                                                   "Age/Sex/HIVStatus",
                                                   "Modality/Age/Sex/Result")         ~ "F"),
               
               indicator %in% c("TX_RET", "TX_PVLS")          ~
                 case_when(
                   standardizeddisaggregate %in% c("Age/Sex",
                                                   "Age/Sex/Indication/HIVStatus")    ~ "F"))) %>% 
    rename(age = agefine) %>% 
    filter(!is.na(F_C))
  
  # Binding the different bands together
  d2 <- bind_rows(fact2c, fact2sf, fact2f)
  
  
  # Molding the dataset to TSD format 
  d3 <- d2 %>% 
    # Reconciling and recoding Age and Sex variables
    mutate(
      age = 
        case_when(
          F_C=="N"                                      ~ "",
          F_C %in% c("M","F", "sF") & age==""                 ~ "Unknown Age",
          TRUE                                          ~ age)) %>% 
    mutate(  
      sex = 
        case_when(
          F_C=="N"                                      ~ "",
          F_C %in% c("M","F", "sF") &    
            age %in% 
            c("01-04", 
              "01-09", 
              "05-09", 
              "05-14", 
              "10-14",
              "<01", 
              "<05",
              "<15")                                    ~ "Unknown Sex",
          F_C %in% c("M","F", "sF") &    
            age==""                                     ~ "Unknown Sex",
          TRUE                                          ~ sex)
    ) 
  
  
  # Remove previous intermediate dataset to reduce memory space
  rm(d2)
  
  
  
  # Aggregating the data  
  d4 <- d3 %>% 
    group_by(
             orgunituid,
             sitetype,
             sitename,
             region,
             regionuid,
             operatingunit,
             operatingunituid,
             countryname,
             snu1,
             psnu,
             psnuuid,
             fundingagency,
             primepartner,
             implementingmechanismname,
             mechanismid,
             indicator,
             numeratordenom,
             colvar,
             F_C,
             age,
             sex) %>% 
    summarize(val = sum(value, na.rm=T)) %>% 
    ungroup()  
  
  
  rm(d3)
  
  
  d5 <-  d4 %>% 
    mutate(var_suffix = 
             case_when(
               indicator %ni% c("TX_RET", "TX_PVLS") ~
                 case_when (
                   colvar=="curr_result"              ~ "Now_R",
                   colvar=="curr_target"              ~ "Now_T",
                   colvar=="prev_result"              ~ "Prev_R"),
               indicator %in% c("TX_RET", "TX_PVLS") ~
                 case_when (
                   numeratordenom=="N" & colvar=="curr_result"     ~ "Now_N",
                   numeratordenom=="D" & colvar=="curr_result"     ~ "Now_D")
             )
    ) %>% 
    filter(!is.na(var_suffix)) %>%
    ungroup() %>% 
    mutate(varname = paste(indicator, var_suffix, sep="_")) %>% 
    filter(varname %in% c(
      "HTS_TST_POS_Now_R",
      "HTS_TST_POS_Now_T",
      "TX_CURR_Now_R",
      "TX_CURR_Now_T",
      "TX_CURR_Prev_R",
      "TX_NEW_Now_R",
      "TX_NEW_Now_T",
      "TX_RET_Now_D",
      "TX_RET_Now_N", 
      "HTS_TST_Now_R",
      "HTS_TST_Now_T", 
      "TX_PVLS_Now_D",
      "TX_PVLS_Now_N" 
    )) %>% 
    select(
           region,
           regionuid,
           operatingunit,
           operatingunituid,
           countryname,
           snu1,
           psnu,
           psnuuid,
           fundingagency,
           primepartner,
           implementingmechanismname,
           mechanismid,
           F_C,
           age,
           sex,
           orgunituid,
           sitetype,
           sitename,
           varname,
           val
    ) %>% 
    group_by(
             region,
             regionuid,
             operatingunit,
             operatingunituid,
             countryname,
             snu1,
             psnu,
             psnuuid,
             fundingagency,
             primepartner,
             implementingmechanismname,
             mechanismid,
             F_C,
             age,
             sex,
             # fy17snuprioritization,
             orgunituid,
             sitetype,
             sitename,
             varname
    ) %>%
    summarize(vals = sum(val, na.rm=T)) %>% 
    ungroup()
  
  
  d6 <- d5 %>% 
    spread(varname, vals)
  
  rm(d5, d4)
  
  # Creating additional variables to match TSD output
  
  d6a <- d6 %>% 
    mutate(datatype = "MER",
           PLHIV = NA,
           TX_CURR_NAT = NA,
           TX_CURR_SUBNAT = NA) 
  
  dummy_mer2 <- d6[FALSE, FALSE] %>% 
    mutate(PLHIV = NA,
           TX_CURR_NAT = NA,
           TX_CURR_SUBNAT = NA,
           TX_CURR_Prev_R = NA,
           TX_CURR_Now_R = NA,
           TX_CURR_Now_T = NA,
           HTS_TST_POS_Now_R = NA,
           HTS_TST_POS_Now_T = NA,
           TX_NEW_Now_R = NA,
           TX_NEW_Now_T = NA,
           TX_RET_Now_N = NA,
           TX_RET_Now_D = NA,
           HTS_TST_Now_R = NA,
           HTS_TST_Now_T = NA,
           TX_PVLS_Now_D = NA,
           TX_PVLS_Now_N = NA
    ) 
  
  
  d7 <- bind_rows(d6a, dummy_mer2) %>% 
    select(
           region,
           regionuid,
           operatingunit,
           operatingunituid,
           countryname,
           snu1,
           psnu,
           psnuuid,
           fundingagency,
           primepartner,
           implementingmechanismname,
           mechanismid,
           PLHIV,
           TX_CURR_NAT,
           TX_CURR_SUBNAT,
           TX_CURR_Prev_R,
           TX_CURR_Now_R,
           TX_CURR_Now_T,
           HTS_TST_POS_Now_R,
           HTS_TST_POS_Now_T,
           HTS_TST_Now_R,
           HTS_TST_Now_T,
           TX_NEW_Now_R,
           TX_NEW_Now_T,
           TX_RET_Now_N,
           TX_RET_Now_D,
           TX_PVLS_Now_D,
           TX_PVLS_Now_N,
           F_C,
           age,
           sex,
           datatype,
           orgunituid,
           sitetype,
           sitename
    )
  
  
  rm(d6, d6a)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: Restructuring MER dataset ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Restructuring NAT_SUBNAT datasets ~~~===========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nat <- natx %>% filter(operatingunit %in% x)
  
  if(nrow(nat)>0){
    "Woohoo! Epi data is good!"
    cat_vars <- as.vector(names(nat %>% select_if(is.character)))
    # Removing prioritization data
    cat_var <- cat_vars[!grepl("prioritization", cat_vars)]
    
    nat1 <- nat %>% 
      mutate(F_C = "N",
             datatype = "NAT_SUBNAT",
             age = "",
             sex = "")
    
    tsd <- bind_rows(d7, nat1) } else {
      "FYI: You don't have NAT_SUBNAT data for this country, but you can still proceed without it!"
      tsd <- d7
    }
  
  # Ordering the variables to match the tool back-end 
  # and converting the F_C variable
  finaltsd <- tsd %>% 
    mutate(F_C = 
             case_when(
               F_C == "N"   ~ "No disaggregation",
               F_C == "F"   ~ "Fine",
               F_C == "M"   ~ "MCAD/Coarse",
               F_C == "sF"  ~ "Semi-Fine"
             )) 
  
  
  # Adding SNUprioritization for most recent period 
  geo_frame_mer <- datimx %>% 
    select(psnuuid, snuprioritization, curr_fy_res) %>%
    gather(period, value, curr_fy_res) %>% 
    select(psnuuid, snuprioritization, period) %>%
    unique() %>% 
    arrange(psnuuid, desc(period), snuprioritization) %>% 
    group_by(psnuuid) %>% 
    mutate(ordernum = row_number()) %>% 
    filter(ordernum == 1) %>% 
    ungroup() %>% 
    select(psnuuid, snuprioritization) %>% 
    filter(!is.na(psnuuid)) %>% 
    unique()
  
  mer_psnu <- unique(geo_frame_mer$psnuuid)
  
# Adding SNU prioritization 
  finaltsdx <- left_join(finaltsd, geo_frame_mer) 
  
  # Adding facility PR  for most recent period 
  geo_frame_site <- datimx %>% 
    select(orgunituid, facilityprioritization, curr_fy_res) %>%
    gather(period, value, curr_fy_res) %>% 
    select(orgunituid, facilityprioritization, period) %>%
    unique() %>% 
    arrange(orgunituid, desc(period), facilityprioritization) %>% 
    group_by(orgunituid) %>% 
    mutate(ordernum = row_number()) %>% 
    filter(ordernum == 1) %>% 
    ungroup() %>% 
    select(orgunituid, facilityprioritization) %>% 
    filter(!is.na(orgunituid)) %>% 
    unique()
  
  
  # Adding SNU prioritization 
  finaltsdxy<- left_join(finaltsdx, geo_frame_site) 
  
  # Grouping PSNUs that don't have MER data
  nonmer <- finaltsdxy %>% 
    rowwise() %>% 
    mutate(mer = sum(TX_CURR_Prev_R      ,      
                     TX_CURR_Now_R       ,     
                     TX_CURR_Now_T       ,     
                     HTS_TST_POS_Now_R   ,
                     HTS_TST_POS_Now_T   ,
                     TX_NEW_Now_R        ,     
                     TX_NEW_Now_T        ,     
                     TX_RET_Now_N        ,       
                     TX_RET_Now_D ,
                     HTS_TST_Now_T       ,      
                     HTS_TST_Now_R, 
                     TX_PVLS_Now_D,
                     TX_PVLS_Now_N,
                     na.rm=T               )) %>% 
    ungroup() %>% 
    group_by(psnuuid) %>% 
    summarise(merval = sum(mer, na.rm=T)) %>% 
    ungroup() %>% 
    filter(merval==0|is.na(merval))
  
xmerp <- unique(nonmer$psnuuid)

finaltsdxx <- finaltsdxy %>% 
  mutate(approvallevel="") %>% 
  mutate_if(is.logical, funs(as.numeric(.))) 
  

final_mer <-  finaltsdxx %>% 
  filter(psnuuid %ni% xmerp)
  
final_nonmer <-  finaltsdxx %>% 
  filter(psnuuid %in% xmerp) %>% 
  mutate(snu1 = "Non_MER",
         psnu = "Non_MER",
         snuprioritization = "Non_MER",
         sitename = "Non_MER",
         sitetype = "Non_MER",
         facilityprioritization = "Non_MER",
         psnuuid = "xxxxxx",
         orgunituid = "xxxxxx") %>% 
  group_by_if(is.character) %>% 
  summarise_if(is.numeric, funs(sum), na.rm=T) %>% 
  ungroup() 


final1 <- bind_rows(final_mer, final_nonmer) %>% 
  # Putting a space before the age band to avoid conversion to date in Excel
  mutate(age = paste(" ", age, sep="")) %>% 
  # Removing SNU prioritization junk strings
  mutate(snuprioritization = 
           if_else(snuprioritization %in% c("", "~"), 
                   "", snuprioritization)) %>% 
  mutate(facilityprioritization = 
           if_else(facilityprioritization %in% c("", "~"), 
                   "", facilityprioritization)) %>%
    select(operatingunit,
           countryname,
           snu1,
           snuprioritization,
           psnu,
           psnuuid,
           sitetype,
           sitename,
           orgunituid,
           fundingagency,
           primepartner,
           implementingmechanismname,
           mechanismid,
           facilityprioritization,
           F_C,
           age,
           sex,
           datatype,
           approvallevel,
           TX_CURR_NAT  ,
           TX_CURR_SUBNAT      ,    
           PLHIV    ,
           TX_CURR_Prev_R      ,      
           TX_CURR_Now_R       ,     
           TX_CURR_Now_T       ,     
           HTS_TST_POS_Now_R   ,
           HTS_TST_POS_Now_T   ,
           TX_NEW_Now_R        ,     
           TX_NEW_Now_T        ,     
           TX_RET_Now_N        ,       
           TX_RET_Now_D ,
           HTS_TST_Now_T       ,      
           HTS_TST_Now_R,
           TX_PVLS_Now_D,
           TX_PVLS_Now_N) 
  
  
  
  
  
  # # Compressing the Operating Unit names for output file
  # oux <- str_replace_all(ou, "[ ']", "")
  dx <- as.character(Sys.Date())
  dt <- str_replace_all(dx, "[- ]", "_")
  
  oux <- gsub("[ |']", "", x)
  # file name has date and time stamp (time zone sensitive)
  file_name = paste("./Output/", dt, "_Genie_TSD_", oux, ".csv", sep="")
  
  write_csv(final1, file_name, na="")  
  
  keepers <- c("datim",
               "natx",
               "ou",
               "oulist",
               "%ni%",
               "curr_year",
               "curr_q")
  
  rm(list=setdiff(ls(), keepers))
  

  }

# 
# map( .x = oulist, .f = ~tsdrun(.x))




