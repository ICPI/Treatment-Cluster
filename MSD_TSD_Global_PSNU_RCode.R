memory.limit(size=90000)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following code is automated! -------------------------------
# Please select and run subsequent code to generate ouput
# ----------------------------------------------------------------


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Program Details ~~~~~~~====================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Program details 
#   Programmer: Imran Mujawar
#   Date started: 12/11/2017
#   Date updated: 08/24/2018
#   Program: Integrates the following datasets for TSD tool
#           1) DATIM Genie extract 
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

if(!require(tidyverse)){
  install.packages("tidyverse")
}
library(tidyverse)

if(!require(eply)){
  install.packages("eply")
}
library(eply)

if(!require(stringr)){
  install.packages("stringr")
}
library(stringr)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: Load required packages ~~~~~~~==================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Setting up date and time variables ~~~~~~~======
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dx <- as.character(format(Sys.time(), "%Y %b %d"))
t <- as.character(format(Sys.time(), "%X"))
tm <- str_replace_all(t, "[: ]", "_")
dt <- str_replace_all(dx, "[ ]", "")

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
#### Pulling in the global MSD PSNU by IM
GenieExt_name <- "MSD.txt"

datim <- read_tsv(file=GenieExt_name, 
                  col_names = TRUE,
                  col_types = cols(MechanismID = "c",
                                   FY2017Q1 = "d",      
                                   FY2017Q2 = "d",      
                                   FY2017Q3 = "d",      
                                   FY2017Q4 = "d",
                                   FY2017APR = "d",
                                   FY2018Q1 = "d",
                                   FY2018Q2 = "d",
                                   FY2018Q3 = "d",
                                   FY2018Q4 = "d",
                                   FY2018_TARGETS = "d",
                                   ApprovalLevel = "n"))


# Pulling in NAT_SUBNAT data ~~~~~~~~~~~~~~~~~~~~~~
# getting the ou variable from the Genie extract
# to subset the global Nat-Subnat dataset
# ou <- unique(datim$OperatingUnit)


NAT_SUBNAT_name <- "ICPI_FactView_NAT_SUBNAT_20171222_v2_1.txt"

natx <- read_tsv(file=NAT_SUBNAT_name,
                 col_types = cols(FY2016 = "d",
                                  FY2017 = "d") )

# # Keeping only data for country of interest
# nat <- natx  %>% filter(OperatingUnit %in% ou)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: Pulling in required datasets ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Restructuring MER datasets ~~~~~~~==============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating empty dataset with all periods in FY18.
dummy_mer <- datim[FALSE,] %>% 
  mutate(FY2018Q1 = NA,
         FY2018Q2 = NA,
         FY2018Q3 = NA,
         FY2018Q4 = NA,
         ApprovalLevel =  NA, 
         orgUnitUID = NA,
         site_type = NA,
         SiteName = NA) %>% 
  select(Region:isMCAD,ApprovalLevel, 
         orgUnitUID, site_type, SiteName,
         FY2017Q1         ,
         FY2017Q2         ,
         FY2017Q3         ,
         FY2017Q4         ,
         FY2018_TARGETS   ,
         FY2018Q1         ,
         FY2018Q2         ,
         FY2018Q3         ,
         FY2018Q4)


datimz <- bind_rows(dummy_mer, datim)

# converting character quarter columns to numeric from character 
# cols.num <- c(39:49)
# datimz[cols.num] <- sapply(datimz[cols.num],as.numeric)

# datimz <- datimz %>% filter(OperatingUnit == "Angola")

# Treat TX_PVLS as TX_RET 
# Find out Age sex disagg 
# May be able to sub out for each
# Any numeric value should be able to just add to the end of the code

#### restructuring FactView dataset into long format 
datim1 <- datimz %>% 
  # filter for indicators needed for TSD
  filter(indicator %in% 
           c("HTS_TST_POS", "TX_CURR", "TX_NEW", "TX_RET", "HTS_TST")) %>% 
  rowwise() %>% 
  mutate(
    # FY17CUM = sum(FY2017Q1, FY2017Q2, FY2017Q3, FY2017Q4, na.rm=TRUE), # For Genie
    FY17CUM = FY2017APR,
    FY18CUM = sum(FY2018Q1, FY2018Q2, FY2018Q3, FY2018Q4, na.rm=TRUE)) %>% 
  ungroup() %>%
  mutate(prev_result = ifelse(indicator %in% 
                                c("HTS_TST_POS", "TX_NEW", "HTS_TST"), FY17CUM, FY2017Q4),
         curr_result = ifelse(indicator %in% 
                                c("HTS_TST_POS", "TX_NEW", "HTS_TST"), FY18CUM, FY2018Q3)) %>%
  rename(curr_target = FY2018_TARGETS) %>% 
  select(Region:isMCAD,ApprovalLevel,
         orgUnitUID, site_type, SiteName,
         prev_result, curr_target, curr_result) %>%
  gather(period, valuex, prev_result, curr_target, curr_result) %>% 
  # Removing null values 
  filter(!is.na(valuex))


# setting up Epic
epic <- "FY18 cum."


# # Creating the F_C variable with No_disagg, and MCAD choices
# fact1 <- datim1 %>% 
#   # filter for indicators needed for TSD
#   filter(indicator %in% 
#          c("HTS_TST_POS", 
#            "TX_CURR", 
#            "TX_NEW", 
#            "TX_RET")) %>% 
#   # Keeping only the FY16APR and target data
#   select(-c(FY2017Q1, FY2017Q2, FY2017Q3, FY2017Q4, SNUprioritization)) %>% 
#   rowwise() %>% 
#   mutate(FY18CUM = sum(FY2018Q1, FY2018Q2, na.rm=TRUE)) %>% 
#   ungroup() %>%
#   mutate(prev_result = FY2017APR,
#          curr_result = ifelse(indicator %in% 
#                       c("HTS_TST", "HTS_TST_POS", "TX_NEW"), FY18CUM, FY2018Q2)) %>%
#   rename(curr_target = FY2018_TARGETS) %>% 
#   select(orgUnitUID:isMCAD,
#          prev_result, curr_target, curr_result) %>% 
#   gather(period, valuex, c(43:45)) %>% 
#   # Removing null values 
#   filter(!is.na(valuex))

# %>% 
#   # creating variable to differentiate the two SC_STOCK variables
#   mutate(stock_ind = case_when(
#             indicator == "SC_STOCK" & 
#             standardizedDisaggregate == "ObservedCommodity" &
#             otherDisaggregate == "Rapid Test Kit"               ~ "hts",
#             indicator == "SC_STOCK" & 
#             standardizedDisaggregate == "ObservedCommodity" &
#             otherDisaggregate == "First Line ARV"               ~ "txx"))


# Creating the F_C variables MCAD and Total N
fact2c <-  datim1 %>%
  mutate(F_C =  
           case_when(
             indicator %ni% c("TX_RET")                                    ~
               case_when(
                 standardizedDisaggregate %in% c("Total Numerator")                  ~ "N",
                 isMCAD   ==    "Y"                                 ~ "M"),
             indicator %in% c("TX_RET")          ~
               case_when(
                 standardizedDisaggregate %in% c("Total Numerator",
                                                 "Total Denominator")                ~ "N",
                 standardizedDisaggregate %in% c("Age Aggregated/Sex")               ~ "M"))) %>% 
  rename(Age = AgeCoarse) %>% 
  filter(!is.na(F_C))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Semi-fine data 
fact2sf <- datim1 %>% 
  filter(indicator %in% c("HTS_TST_POS", 
                          "TX_CURR", 
                          "TX_NEW", 
                          "TX_RET",
                          "HTS_TST")             & 
           standardizedDisaggregate %ni% c("Total Numerator")) %>% 
  mutate(F_C =
           case_when(
             indicator %ni% c("TX_RET")                                    ~
               case_when(
                 standardizedDisaggregate %in% c("AgeAboveTen/Sex/Positive",  
                                                 "AgeLessThanTen/Positive",  
                                                 "Age/Sex/HIVStatus",
                                                 "Modality/Age Aggregated/Sex/Result",
                                                 "Modality/Age/Sex/Result")         ~ "sF"),
             
             indicator %in% c("TX_RET")          ~
               case_when(
                 standardizedDisaggregate %in% c("Age/Sex")         ~ "sF"))) %>% 
  rename(Age = AgeSemiFine) %>% 
  filter(!is.na(F_C))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fine data 
fact2f <- datim1 %>% 
  filter(indicator %in% c("HTS_TST_POS", 
                          "TX_CURR", 
                          "TX_NEW", 
                          "TX_RET",
                          "HTS_TST")           &
           standardizedDisaggregate %ni% c("Total Numerator")) %>% 
  mutate(F_C =
           case_when(
             indicator %ni% c("TX_RET")                                    ~
               case_when(
                 standardizedDisaggregate %in% c("AgeAboveTen/Sex/Positive",  
                                                 "AgeLessThanTen/Positive",  
                                                 "Age/Sex/HIVStatus",
                                                 "Modality/Age Aggregated/Sex/Result",
                                                 "Modality/Age/Sex/Result")        ~ "F"),
             
             indicator %in% c("TX_RET")          ~
               case_when(
                 standardizedDisaggregate %in% c("Age/Sex")        ~ "F"))) %>% 
  rename(Age = AgeFine) %>% 
  filter(!is.na(F_C))

# Binding the different bands together
d2 <- bind_rows(fact2c, fact2sf, fact2f)


# Molding the dataset to TSD format 
d3 <- d2 %>% 
  # Reconciling and recoding Age and Sex variables
  mutate(
    Age = 
      case_when(
        F_C=="N"                                      ~ "",
        F_C %in% c("M","F", "sF") & Age==""                 ~ "Unknown Age",
        TRUE                                          ~ Age)) %>% 
  mutate(  
    Sex = 
      case_when(
        F_C=="N"                                      ~ "",
        F_C %in% c("M","F", "sF") &    
          Age %in% 
          c("01-04", 
            "01-09", 
            "05-09", 
            "05-14", 
            "10-14",
            "<01", 
            "<05",
            "<15")                                    ~ "Unknown Sex",
        F_C %in% c("M","F", "sF") &    
          Age==""                                     ~ "Unknown Sex",
        TRUE                                          ~ Sex)
  ) 

# %>% this is for the Genie extract 
#   # Creating site_name, site_id and site_type variables  
# mutate(
#   site_type = 
#     case_when(
#       orgUnitUID==FacilityUID   ~ "Facility",
#       orgUnitUID==CommunityUID  ~"Community",
#       orgUnitUID==PSNUuid       ~"Military",
#       TRUE                      ~ "") ) 


rm(d2)

# Aggregating the data for Epoch FY_18 
d4 <- d3 %>% 
  group_by(ApprovalLevel,
           orgUnitUID,
           site_type,
           SiteName,
           Region,
           RegionUID,
           OperatingUnit,
           OperatingUnitUID,
           CountryName,
           SNU1,
           PSNU,
           PSNUuid,
           FundingAgency,
           PrimePartner,
           ImplementingMechanismName,
           MechanismID,
           indicator,
           numeratorDenom,
           period,
           F_C,
           Age,
           Sex) %>% 
  summarize(val = sum(valuex, na.rm=T)) %>% 
  ungroup()

rm(d3)


d5 <-  d4 %>% 
  mutate(var_suffix = 
           case_when(
             indicator != "TX_RET" ~
               case_when (
                 period=="curr_result"              ~ "Now_R",
                 period=="curr_target"              ~ "Now_T",
                 period=="prev_result"              ~ "Prev_R"),
             indicator == "TX_RET" ~
               case_when (
                 numeratorDenom=="N" & period=="prev_result"     ~ "Now_N",
                 numeratorDenom=="D" & period=="prev_result"     ~ "Now_D")
           )
  ) %>% 
  # Removing non-epoch 17 values 
  filter(!is.na(var_suffix)) %>% 
  mutate(Epoch = epic) %>% 
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
    "HTS_TST_Now_T")) %>% 
  select(ApprovalLevel,
         Region,
         RegionUID,
         OperatingUnit,
         OperatingUnitUID,
         CountryName,
         SNU1,
         PSNU,
         PSNUuid,
         FundingAgency,
         PrimePartner,
         ImplementingMechanismName,
         MechanismID,
         F_C,
         Age,
         Sex,
         Epoch,
         # FY17SNUPrioritization,
         orgUnitUID,
         site_type,
         SiteName,
         varname,
         val
  ) %>% 
  group_by(ApprovalLevel,
           Region,
           RegionUID,
           OperatingUnit,
           OperatingUnitUID,
           CountryName,
           SNU1,
           PSNU,
           PSNUuid,
           FundingAgency,
           PrimePartner,
           ImplementingMechanismName,
           MechanismID,
           F_C,
           Age,
           Sex,
           Epoch,
           # FY17SNUPrioritization,
           orgUnitUID,
           site_type,
           SiteName,
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

dummy_mer2 <- d6[FALSE,] %>% 
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
         HTS_TST_Now_T =NA) %>% 
  select(PLHIV,
         TX_CURR_NAT,
         TX_CURR_SUBNAT,
         TX_CURR_Prev_R,
         TX_CURR_Now_R,
         TX_CURR_Now_T,
         HTS_TST_POS_Now_R,
         HTS_TST_POS_Now_T,
         TX_NEW_Now_R,
         TX_NEW_Now_T,
         TX_RET_Now_N,
         TX_RET_Now_D,
         HTS_TST_Now_R,
         HTS_TST_Now_T)

d7 <- bind_rows(d6a, dummy_mer2) %>% 
  select(ApprovalLevel,
         Region,
         RegionUID,
         OperatingUnit,
         OperatingUnitUID,
         CountryName,
         SNU1,
         PSNU,
         PSNUuid,
         FundingAgency,
         PrimePartner,
         ImplementingMechanismName,
         MechanismID,
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
         F_C,
         Age,
         Sex,
         Epoch,
         datatype,
         # FY17SNUPrioritization,
         orgUnitUID,
         site_type,
         SiteName
  )


rm(d6, d6a)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= END: Restructuring datasets ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Restructuring datasets ~~~~~~~============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nat1 <- natx %>% 
  # filter for indicators needed for TSD
  filter(indicator %in% 
           c("TX_CURR_NAT",
             "TX_CURR_SUBNAT",
             "PLHIV") &
           disaggregate %in% 
           c("Total Numerator", "Age/Sex", "") 
  ) %>% 
  select(-c(FY2016)) %>% 
  # Converting to long format by period
  filter(!is.na(FY2017))


# testx <- nat1 %>% 
#   group_by (indicator, disaggregate) %>% 
#   summarize(FY16vals = sum(FY2016, na.rm=T),
#             FY17vals = sum(FY2017, na.rm=T))



# Creating the F_C variable with No_disagg, Fine, MCAD choices
nat2 <-  nat1 %>%
  mutate(F_C = case_when (
    disaggregate == "Total Numerator"     ~ "N", 
    disaggregate == "Age/Sex"             ~ "M")) %>% 
  mutate(Epoch = "FY18 Cum.",
         datatype = "NAT_SUBNAT") %>% 
  group_by (
    Region,
    RegionUID,
    OperatingUnit,
    OperatingUnitUID,
    CountryName,
    SNU1,
    PSNU,
    PSNUuid,
    F_C,
    Age,
    Sex,
    Epoch,
    datatype,
    # CurrentSNUPrioritization,
    indicator) %>% 
  summarize(valuex = sum(FY2017, na.rm = T)) %>% 
  ungroup() %>% 
  spread(indicator, valuex)

nat2$ApprovalLevel <- 2 


tsd <- bind_rows(d7, nat2)


# Ordering the variables to match the tool back-end 
# and converting the F_C variable
finaltsd <- tsd %>% 
  mutate(F_C = 
           case_when(
             F_C == "N"   ~ "No disaggregation",
             F_C == "F"   ~ "Fine",
             F_C == "M"   ~ "MCAD/Coarse",
             F_C == "sF"  ~ "Semi-Fine"
           )) %>% 
  select(OperatingUnit,
         OperatingUnitUID,	
         CountryName,	
         SNU1,	
         PSNU,	
         PSNUuid,	
         FundingAgency,	
         PrimePartner,	
         ImplementingMechanismName,	
         Age,	
         MechanismID,	
         TX_CURR_NAT,	
         TX_CURR_SUBNAT,	
         PLHIV,	
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
         F_C,	
         Sex,	
         Epoch,	
         datatype,	
         # FY17SNUPrioritization,	
         site_type,
         SiteName,
         orgUnitUID,
         ApprovalLevel)


# Adding SNUprioritization for most recent period 
geo_frame_mer <- datim1 %>% 
  select(PSNUuid, SNUprioritization, period) %>%
  unique() %>% 
  arrange(PSNUuid, SNUprioritization, period) %>% 
  group_by(PSNUuid) %>% 
  mutate(ordernum = row_number()) %>% 
  filter(ordernum == 1) %>% 
  ungroup() %>% 
  select(PSNUuid, SNUprioritization) %>% 
  filter(!is.na(PSNUuid))

mer_psnu <- unique(geo_frame_mer$PSNUuid)


geo_frame_nat <- nat1 %>% 
  rename(SNUPrioritization = FY18SNUPrioritization) %>% 
  select(PSNUuid, SNUPrioritization) %>% 
  unique() %>% 
  filter(!is.na(SNUPrioritization)) %>% 
  filter(PSNUuid %ni% mer_psnu)

geo_frame <- bind_rows(geo_frame_mer,geo_frame_nat)

# Adding SNU prioritization 
finaltsdx <- left_join(finaltsd, geo_frame)


finaltsdg <- finaltsdx %>% 
  select(OperatingUnit,
         CountryName,
         SNU1,
         SNUprioritization,
         PSNU,
         PSNUuid,
         site_type,
         SiteName,
         orgUnitUID,
         FundingAgency,
         PrimePartner,
         ImplementingMechanismName,
         MechanismID,
         F_C,
         Age,
         Sex,
         Epoch,
         datatype,
         ApprovalLevel,
         TX_CURR_NAT,
         TX_CURR_SUBNAT,
         PLHIV,
         TX_CURR_Prev_R,
         TX_CURR_Now_R,
         TX_CURR_Now_T,
         HTS_TST_POS_Now_R,
         HTS_TST_POS_Now_T,
         TX_NEW_Now_R,
         TX_NEW_Now_T,
         TX_RET_Now_N,
         TX_RET_Now_D,
         HTS_TST_Now_R,
         HTS_TST_Now_T)



# # Compressing the Operating Unit names for output file
# oux <- str_replace_all(ou, "[ ']", "")


# file name has date and time stamp (time zone sensitive)
file_name = paste(dt, "_TSD_MER_", tm, ".txt", sep="")

write_tsv(finaltsdg, file_name, na="")





