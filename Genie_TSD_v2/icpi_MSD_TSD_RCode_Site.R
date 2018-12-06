memory.limit(size=10000000)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Program Details ~~~~~~~====================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Program details 
#   Programmer: Imran Mujawar
#   Date started: 10/25/2018
#   Date updated: 10/16/2018
#   Program: Integrates the following datasets for TSD tool
#           1) Genie Site by IM
#           2) NAT_SUBNAT data
#           
# ---------------------------------------------------------------


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Load required packages ~~~~~~~==================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!require(readr)){
  install.packages("readr")
}
library(readr)

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

if(!require(tidyverse)){
  install.packages("tidyverse")
}
library(tidyverse)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: Load required packages ~~~~~~~==================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Setting up date and time variables ~~~~~~~======
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Setting the current year and quarter of reference
curr_year <- 2018
curr_q    <- 4
nat_year  <- 2019


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: Setting up date and time variables ~~~~~~~======
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
# ============= Pulling in required datasets ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Looking for the NAT_SUBNAT dataset in folder
impattfile <- list.files("./Genie_TSD_v2/RawData/"
                         , pattern="NAT_SUBNAT")

# If more than one NAT_SUBNAT file in folder then stop code
if(length(impattfile)>1) {
  stop("ERROR: Please keep only NAT_SUBNAT dataset in the folder")
} else {print("Thanks for using the NAT_SUBNAT dataset... woot woot!!")}

if (grepl(".zip", impattfile)) {
  "Zippty doo... you're using a Zipped file!!"
  n_file <- unzip(paste("./Genie_TSD_v2/RawData/", impattfile, sep=""), list = TRUE) %>% .$Name
  #extract file from zipped folder
  unzip(impattfile, exdir = ".")} else {print("You're using the unzipped file... working hard!")
    n_file <- paste("./Genie_TSD_v2/RawData/", impattfile, sep="")}

# Rough data pull to get variable names and assign datatype
nfoo <- read_tsv(file=n_file, 
                 col_names = TRUE)

nfoonames <- tolower(names(nfoo))

# Checking whether the zipped file is a Genie file
natvars <- c("psnu", "psnuuid")
if(!all(natvars %in% nfoonames)){
  stop("ERROR: This is not a NAT_SUBNAT data file! Please check uploaded file")
} else {
  ":) Your dataset is a NAT_SUBNAT dataset! Woo hoo!!"
}

# Creating the vector of column types
ncolvecx <- as.vector(ifelse(grepl("fy20", nfoonames), "d", "c"))
ncolvec <- paste(ncolvecx, collapse = '')

# Reading in the GLOBAL Nat_subnat data in with correct column types
natx <- read_tsv(file=n_file,
                 col_types = ncolvec )

names(natx) <- tolower(names(natx))  


# ===== Pulling in the Genie file list in the folder ======
genie_filelist <- intersect(list.files("./Genie_TSD_v2/RawData/",pattern = "MER_Structured"), 
                            list.files("./Genie_TSD_v2/RawData/",pattern = ".zip$"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Looping through all Genie datasets ~~~~~~~======
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for (x in genie_filelist) {
  g_file <- unzip(paste("./Genie_TSD_v2/RawData/", x, sep=""), list = TRUE) %>% .$Name
  #extract file from zipped folder
  unzip(paste("./Genie_TSD_v2/RawData/", x, sep=""), exdir = "./Genie_TSD_v2/RawData")
  
  # Rough data pull to get variable names and assign datatype
  foo <- read_tsv(file=paste("./Genie_TSD_v2/RawData/", x, sep=""),
                  col_names = TRUE)
  
  foonames <- tolower(names(foo))
  
  # Checking whether the zipped file is a Genie file
  msdvars <- c("psnu", "psnuuid")
  if(!all(msdvars %in% foonames)){
    stop("ERROR: This is not a Genie file! Please check uploaded file")
  } else {"You set the Genie free... woot woot"}
  
  # Creating the vector of column types
  colvecx <- as.vector(ifelse(grepl("fy20", foonames), "d", "c"))
  colvec <- paste(colvecx, collapse = '')
  
  
  # Pulling in the data with correct datatype for variables  
  datim <- read_tsv(file=paste("./Genie_TSD_v2/RawData/", x, sep=""),
                    col_names = TRUE,
                    col_types = colvec)
  
  names(datim) <- tolower(names(datim))  
  
  # Pulling in NAT_SUBNAT data ~~~~~~~~~~~~~~~~~~~~~~
  # getting the ou variable from the Genie extract
  # to subset the global Nat-Subnat dataset
  ou <- unique(datim$operatingunit)
  
  # changing Swaziland in Nat data to Eswatini
  
  natx <- natx %>% 
    mutate(operatingunit = 
             if_else(operatingunit == "Swaziland", "Eswatini", operatingunit)) %>% 
    mutate(countryname =
             if_else(countryname == "Swaziland", "Eswatini", countryname))
  
  # # Keeping only data for country of interest
  nat <- natx  %>% filter(operatingunit %in% ou)
  
  names(nat) <- tolower(names(nat))
  
  
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
               "TX_RET", 
               "TX_PVLS")
  
  #### restructuring FactView dataset into long format 
  datim1 <- datim %>% 
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
  # ============= END: Restructuring datasets ~~~~~~~============
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ============= Restructuring datasets ~~~~~~~============
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(nrow(nat)>0){
    "Woohoo! Epi data is good!"
    cat_vars <- as.vector(names(nat %>% select_if(is.character)))
    # Removing prioritization data
    cat_var <- cat_vars[!grepl("prioritization", cat_vars)]
    
    natyearx = as.vector(paste("fy", nat_year, sep="")) 
    
    nat1 <- nat %>% 
      # filter for indicators needed for TSD
      filter(indicator %in% 
               c("TX_CURR_NAT",
                 "TX_CURR_SUBNAT",
                 "PLHIV",
                 "DIAGNOSED_SUBNAT",
                 "DIAGNOSED_NAT",
                 "VL_SUPPRESSION_NAT",
                 "VL_SUPPRESSION_SUBNAT")  
      ) %>% 
      select(cat_var, natyearx) %>% 
      mutate(F_C = case_when(
        indicator %in% c("PLHIV") & disaggregate %in% c("Age/Sex")             ~ "M",
        indicator %ni% c("PLHIV") & disaggregate %in% c("Age Aggregated/Sex")  ~ "M",
        is.na(disaggregate)                                                    ~ "N"
      )) %>% 
      mutate(age = 
               if_else(is.na(coarse_age) & !is.na(age_as_entered), 
                       age_as_entered, coarse_age)) %>% 
      mutate(
        age = 
          case_when(
            F_C=="N"                                      ~ "",
            F_C %in% c("M") & age==""                 ~ "Unknown Age",
            F_C %in% c("M") &    
              age %in% 
              c("01-04", 
                "01-09", 
                "05-09", 
                "05-14", 
                "10-14",
                "<01", 
                "<05",
                "<15")                                ~ "<15",
            F_C %in% c("M") &    
              age %in% 
              c("15-19", 
                "15-29",
                "20-24", 
                "25-29", 
                "30-49",
                "15+")                                ~ "15+",
            TRUE                                          ~ age)) %>% 
      mutate(  
        sex = 
          case_when(
            F_C=="N"                                      ~ "",
            F_C %in% c("M") &    
              age %in% 
              c("01-04", 
                "01-09", 
                "05-09", 
                "05-14", 
                "10-14",
                "<01", 
                "<05",
                "<15")                                    ~ "Unknown Sex",
            F_C %in% c("M") &    
              age==""                                     ~ "Unknown Sex",
            TRUE                                          ~ sex)
      ) %>% 
      mutate(datatype = "NAT_SUBNAT") %>% 
      group_by (
        region,
        regionuid,
        operatingunit,
        operatingunituid,
        countryname,
        snu1,
        psnu,
        psnuuid,
        F_C,
        age,
        sex,
        datatype,
        indicator 
      ) %>% 
      summarise_at(natyearx, sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      spread(indicator, natyearx)
    
    dummynat <- nat1[FALSE, FALSE] %>% 
      mutate(TX_CURR_NAT = 0 ,
             TX_CURR_SUBNAT = 0,
             PLHIV = 0,
             DIAGNOSED_NAT = 0,
             DIAGNOSED_SUBNAT = 0,
             VL_SUPPRESSION_NAT = 0,
             VL_SUPPRESSION_SUBNAT = 0)
    
    
    nat2 <- bind_rows(nat1, dummynat)
    
    # Getting the NAT_SUBNAT offsets
    offsets <- nat2 %>% 
      mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
      group_by (
        region,
        regionuid,
        operatingunit,
        operatingunituid,
        F_C,
        age,
        sex,
        datatype) %>% 
      summarise_if(is.numeric, funs(sum), na.rm = TRUE) %>% 
      mutate(TX_CURR_NAT_o = ifelse(TX_CURR_NAT>0, TX_CURR_NAT-TX_CURR_SUBNAT, 0),
             DIAGNOSED_NAT_o = ifelse(DIAGNOSED_NAT>0, DIAGNOSED_NAT-DIAGNOSED_SUBNAT, 0),
             VL_SUPPRESSION_NAT_o = ifelse(VL_SUPPRESSION_NAT>0, VL_SUPPRESSION_NAT-VL_SUPPRESSION_SUBNAT, 0)) %>% 
      select(    region,
                 regionuid,
                 operatingunit,
                 operatingunituid,
                 F_C,
                 age,
                 sex,
                 datatype,
                 TX_CURR_NAT_o,
                 DIAGNOSED_NAT_o,
                 VL_SUPPRESSION_NAT_o
      ) %>% 
      rename(TX_CURR_SUBNAT       =  TX_CURR_NAT_o,
             DIAGNOSED_SUBNAT     =  DIAGNOSED_NAT_o,
             VL_SUPPRESSION_SUBNAT = VL_SUPPRESSION_NAT_o) %>% 
      mutate(snu1 = "Non_MER",
             psnu = "Non_MER",
             psnuuid = "xxxxxx")
    
    
    # stacking on the offsets below
    nat_final <- bind_rows(nat2, offsets)
    
    
    
    tsd <- bind_rows(d7, nat2) } else {
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
  
  if(nrow(nat1)>0){
    geo_frame_nat <- nat %>% 
      select(psnuuid, snuprioritization) %>%
      unique() %>% 
      filter(!is.na(snuprioritization)) %>% 
      filter(psnuuid %ni% mer_psnu)} else {"No NAT_SUBNAT data, but you can still carry on!"}
  
  geo_frame <- bind_rows(geo_frame_mer,geo_frame_nat)
  
  
  
  # Adding SNU prioritization 
  finaltsdx <- left_join(finaltsd, geo_frame) 
  
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
  finaltsdxx<- left_join(finaltsdx, geo_frame_site) 
  
  # Grouping PSNUs that don't have MER data
  nonmer <- finaltsdxx %>% 
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
    filter(!is.na(psnuuid) & psnuuid!="?") %>% 
    filter(merval==0)
  
xmerp <- unique(nonmer$psnuuid)

finaltsdxx <- finaltsdxx %>% 
  mutate(approvallevel=NA)

final1 <-  finaltsdxx %>% 
  mutate(snu1 = if_else(psnuuid %in% xmerp, "Non_MER", snu1)) %>% 
  mutate(psnu = if_else(psnuuid %in% xmerp, "Non_MER", psnu)) %>% 
  mutate(psnuuid = if_else(psnuuid %in% xmerp, "xxxxxx", psnuuid)) %>%  
  mutate(snuprioritization = if_else(psnuuid %in% xmerp, 
                                     "Non_MER", snuprioritization)) %>% 
  mutate_if(is.logical, funs(as.numeric(.))) %>% 
  group_by_if(is.character) %>% 
  summarise_if(is.numeric, funs(sum), na.rm=T) %>% 
  ungroup() %>% 
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
           TX_PVLS_Now_N) %>% 
  # Putting a space before the age band to avoid conversion to date in Excel
  mutate(age = paste(" ", age, sep="")) %>% 
  # Removing SNU prioritization junk strings
  mutate(snuprioritization = 
           if_else(snuprioritization %in% c("", "~"), 
                   "", snuprioritization)) %>% 
  mutate(facilityprioritization = 
           if_else(facilityprioritization %in% c("", "~"), 
                   "", facilityprioritization)) 
  
  
  
  
  # # Compressing the Operating Unit names for output file
  # oux <- str_replace_all(ou, "[ ']", "")
  dx <- as.character(Sys.Date())
  dt <- str_replace_all(dx, "[- ]", "_")
  
  
  # file name has date and time stamp (time zone sensitive)
  file_name = paste("./Genie_TSD_v2/Output/", dt, "_MSD_TSD_", ou, ".csv", sep="")
  
  write_csv(final1, file_name, na="")  
  
  keepers <- c("genie_filelist",
               "natx",
               "%ni%",
               "curr_year",
               "curr_q",   
               "nat_year" )
  
  rm(list=setdiff(ls(), keepers))
  

  }





