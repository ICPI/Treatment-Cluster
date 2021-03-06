memory.limit(size=10000000)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Program Details ~~~~~~~====================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Program details 
#   Programmer: Imran Mujawar
#   Modified: Aliyah Abdul-Wakil
#   Date started: 02/11/2019
#   Date updated: 03/19/2019
#   Program: Integrates the following datasets for TSD tool
#           1) MSD Site by IM
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
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating basic functions to show top few rows of data
View50 <- function(x){View(x[1:50,])}
View100 <- function(x){View(x[1:100,])}

# Creating the 'not in' function
`%ni%` <- Negate(`%in%`) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Getting column header data types for MSDs ===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Getting the MSD dataset file names
glist <- list.files("../RawData/"
                    , pattern="MER")


  gfile_loc <- paste("../RawData/", glist[2], sep="") 

# Rough data pull to get variable names and assign datatype
foo <- read_tsv(file=gfile_loc, 
                col_names = TRUE, n_max = 0)   

foonames <- tolower(names(foo))


# # Genie substitute
# genieloc <- list.files("../RawData/Genie_test/"
#                        , pattern="Genie")
# genielocx <- paste("../RawData/Genie_test/", genieloc[1], sep="")
# 
# # Rough data pull to get variable names and assign datatype
# foox <- read_tsv(file=genielocx, 
#                 col_names = TRUE, n_max = 0)   
# 
# foonamesx <- tolower(names(foox))
# 

# # For Genie
# colvecx <- as.vector(ifelse(grepl("fy", foonamesx), "d", "c"))
# colvecxx <- paste(colvecx, collapse = '')
# 
# 
# # Pulling in the data with correct datatype for variables  
# genie <- read_tsv(file=genielocx, 
#                   col_names = TRUE,
#                   col_types = colvecxx)      # ending if-else for Genie check
# 
# names(genie) <- tolower(names(genie))  
# 
# datim <- genie %>% select(-approvallevel, -approvalleveldescription)

# Creating the vector of column types
colvecx <- as.vector(ifelse(grepl("fy", foonames), "d", "c"))

# colvecx <- as.vector(ifelse(foonames %in% c("FILL IN STUFF HERE"  ), "d", "c"))

colvec <- paste(colvecx, collapse = '')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Setting up date and time variables ~~~~~~~======
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

curr_year <- identifypd(foo, pd_type = "year")
curr_q    <- identifypd(foo, pd_type = "quarter")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ======== Pulling in all NAT_SUBNAT dataset ~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
# ============= END: Setting up date and time variables ~~======
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Looping through all OUs  ~~~~~~~======
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tsd_msd_run <- function(x) {
  
  gfile_loc <- paste("../RawData/", x, sep="") 
  
  # Pulling in the data with correct datatype for variables  
  datim <- read_tsv(file=gfile_loc, 
                    col_names = TRUE,
                    col_types = colvec)      # ending if-else for Genie check
  
  names(datim) <- tolower(names(datim))  
  
  
# Getting the OU name
  oux <- read_tsv(file=gfile_loc, 
                    col_names = TRUE,
                    col_types = colvec,
                  n_max = 1) 
                  
  ouname <- as.vector(oux$OperatingUnit)

                      
  #Vector of string variables
  char_varx <- names(datim %>% select_if(is.character))
  # getting rid of SNU prioritization
  char_var <- char_varx[!grepl("prioritization", char_varx)]
  
  
  #Vector of numeric variables
  num_var <- names(datim %>% select(starts_with("fy")))
  curr_fy_res <- names(datim %>% select(starts_with(paste("fy", curr_year, "q", sep=""))))
  prev_fy_res <- names(datim %>% select(starts_with(paste("fy", curr_year-1, "q", sep=""))))
  
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
               "TX_PVLS")
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Restructuring MER dataset    ~~~~~~~============
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  
  #### restructuring FactView dataset into long format 
  datim1 <- datim %>% 
    # filter for indicators needed for TSD
    filter(indicator %in% indlist) %>%
    select(char_var, num_var) %>% 
    gather(period, value, num_var) %>%
    # Removing null values 
    filter(!is.na(value)) %>% 
    mutate(colvar = case_when(
      indicator %in% c("TX_CURR","TX_PVLS") & period %in% yr_q        ~ "curr_result",
      indicator %in% c("TX_CURR","TX_PVLS") & period %in% p_yr_q      ~ "prev_result",
      indicator %ni% c("TX_CURR","TX_PVLS") & period %in% curr_fy_res ~ "curr_result",
      indicator %ni% c("TX_CURR","TX_PVLS") & period %in% prev_fy_res ~ "prev_result",
      period %in% curr_tar                                            ~ "curr_target",
      TRUE ~ "X"
    )) %>% 
    filter(colvar!="X") %>%
    select(char_var, colvar, value) %>% 
    group_by_if(is.character) %>% 
    summarize(value = sum(value, na.rm=T)) %>% 
    ungroup()
  
  
  # Creating the F_C variables MCAD and Total N
  fact2c <-  datim1 %>%
    mutate(F_C = case_when(standardizeddisaggregate 
                           %in% c("Total Numerator",
                                  "Total Denominator")                        ~ "N",
                           standardizeddisaggregate
                           %in% c("Modality/Age Aggregated/Sex/Result",
                                  "Modality/Age/Sex/Result",
                                  "Age Aggregated/Sex/HIVStatus",
                                  "Age/Sex/HIVStatus",
                                  "Age Aggregated/Sex/Indication/HIVStatus",
                                  "Age/Sex/Indication/HIVStatus")             ~ "M")) %>% 
    rename(age = agecoarse) %>% 
    filter(!is.na(F_C))
  

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Semi-fine data 
  fact2sf <- datim1 %>% 
    mutate(F_C = case_when(standardizeddisaggregate
                           %in% c("Modality/Age/Sex/Result",
                                  "Age/Sex/HIVStatus",
                                  "Age/Sex/Indication/HIVStatus")             ~ "sF")) %>% 
    rename(age = agesemifine) %>% 
    filter(!is.na(F_C))
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fine data 
  fact2f <- datim1 %>% 
    mutate(F_C = case_when(standardizeddisaggregate
                           %in% c("Modality/Age/Sex/Result",
                                  "Age/Sex/HIVStatus",
                                  "Age/Sex/Indication/HIVStatus")             ~ "F")) %>% 
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
          F_C %in% c("M","F", "sF") & age==""           ~ "Unknown Age",
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
             indicator %ni% c("TX_PVLS") & colvar=="curr_result"  ~ "Now_R",
             indicator %ni% c("TX_PVLS") & colvar=="curr_target"  ~ "Now_T",
             indicator %ni% c("TX_PVLS") & colvar=="prev_result"  ~ "Prev_R",
             indicator %in% c("TX_PVLS") & numeratordenom=="N" & colvar=="curr_result" ~ "Now_N",
             indicator %in% c("TX_PVLS") & numeratordenom=="D" & colvar=="curr_result" ~ "Now_D")
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
# ============= Restructuring NAT_SUBNAT datasets ~~~===========
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nat <- natx %>% filter(operatingunit %in% ouname)
  
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
  geo_frame_mer <- datim %>% 
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
  geo_frame_site <- datim %>% 
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
  
  ounamex <- gsub("[ |']", "", ouname)
  # file name has date and time stamp (time zone sensitive)
  file_name = paste("../tsd_output/", dt, "_MSD_TSD_", ounamex, ".csv", sep="")
  
  write_csv(final1, file_name, na="")  
  
  keepers <- c("glist",
               "natx",
               "colvec",
               "%ni%",
               "curr_year",
               "curr_q")

  rm(list=setdiff(ls(), keepers))


  }

# Running the function on all datasets using the PURR package in Tidyverse
map( .x = glist[3], .f = ~tsd_msd_run(.x))




