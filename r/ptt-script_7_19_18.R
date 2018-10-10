library(openxlsx)
library(tidyverse)
library(dplyr)
library(data.table)

dffv18 <- fread("MER_Structured_Dataset_PSNU_IM_FY17-18_20180622_v2_1.txt", sep = 'auto', header = T, 
                stringsAsFactors = F,na.strings = 'NULL')

dfct18 <- dffv18  %>% filter( indicator %in% c('TX_CURR' , "TX_NEW"))

saveRDS(dfct18,'MSD_PSNU_IM_TX_17-18.rds')

dffv1516 <- fread('ICPI_MER_Structured_Dataset_PSNU_IM_FY15-16_20180515_v1_1.txt', sep = 'auto', header = T, 
                  stringsAsFactors = F,na.strings = 'NULL')


dfct15 <- dffv1516  %>% filter( indicator %in% c('TX_CURR' , "TX_NEW"), standardizedDisaggregate=='Total Numerator')%>% 
  group_by(OperatingUnit,indicator) %>% 
  summarise_at(vars(FY2015Q2:FY2016APR), sum, na.rm=T)

saveRDS(dfct15,'MSD_PSNU_15-16_TX.rds')

#---------Read RDS files------

dfct15 <- readRDS('MSD_PSNU_15-16_TX.rds')

dfct18 <- readRDS('MSD_PSNU_IM_TX_17-18.rds')

# Adding SNUprioritization for most recent period 
df18_snuP <- dfct18 %>% filter(!is.na(PSNUuid)) %>% 
  select(PSNUuid, SNUPrioritization, FY2017APR, FY2018Q1, FY2018Q2) %>% 
  gather(period, value, 3:5) %>% 
  select(PSNUuid, period, SNUPrioritization) %>%
  unique() %>% 
  arrange(PSNUuid, desc(period), SNUPrioritization) %>% 
  group_by(PSNUuid) %>% 
  mutate(ordernum = row_number()) %>% 
  filter(ordernum == 1) %>% 
  ungroup() %>% 
  select(PSNUuid, SNUPrioritization) %>% 
  filter(!is.na(SNUPrioritization))

# filter on standardized disagg
# check differences in columnnames between dfct15 & dfct18
intersect(colnames(dfct15_lng), colnames(dfct18_lng))

# filter 
dfct15_lng <- dfct15 %>% filter(standardizedDisaggregate=='Total Numerator'| isMCAD=='Y') %>% 
  select(-SNUPrioritization,-categoryOptionComboUID,-dataElementUID) %>% gather(period, value, FY2015Q2:FY2016APR)

dfct18_lng <- dfct18 %>% filter(standardizedDisaggregate=='Total Numerator'| isMCAD=='Y') %>% 
  select(-SNUPrioritization) %>% 
  gather(period, value, FY2017_TARGETS:FY2019_TARGETS)

dfct15_18_lng <- rbind(dfct15_lng,dfct18_lng)

dfct15_18 <- dfct15_18_lng %>% group_by_at(vars(Region:period)) %>% 
  mutate(id=row_number()) %>% 
  spread(period,value) %>% ungroup() 

dfct15_18_grouped <- dfct15_18 %>% select(-id,-categoryOptionComboName,-AgeAsEntered,-AgeFine,-AgeSemiFine) %>% 
  group_by_at(vars(Region:isMCAD)) %>% 
  summarise_all(funs(sum),na.rm=TRUE) %>% ungroup() %>% replace(.=='N/A','')

# create a new variable for Total Num & Age/Sex Disagg, so you can group rows further 
dfct15_18_grouped2 <- dfct15_18_grouped %>% select(-disaggregate,-standardizedDisaggregate) %>% 
  group_by_at(vars(Region:isMCAD)) %>% 
  summarise_all(funs(sum),na.rm=TRUE) %>% ungroup() %>% 
  mutate(disagg=case_when(
    isMCAD=='N'~ 'Total Numerator',
    isMCAD=='Y'~'Age_Sex_disagg'
  ))


# Adding SNU prioritization 
dfct15_18_grouped2_snuP <- left_join(dfct15_18_grouped2, df18_snuP)


# Create tx_net_new & SAPR data

dfnet.new18 <- dfct15_18_grouped2_snuP %>% filter(indicator=='TX_CURR') %>% rowwise() %>% 
  mutate(FY15Q1Q2='',
         FY15Q3Q4=sum(FY2015Q4,-FY2015Q2,na.rm = T),
         FY16Q1Q2=sum(FY2016Q2,-FY2015Q4,na.rm = T),
         FY16Q3Q4=sum(FY2016Q4,-FY2016Q2,na.rm = T),
         FY17Q1Q2=sum(FY2017Q2,-FY2016Q4,na.rm = T),
         FY17Q3Q4=sum(FY2017Q4,-FY2017Q2,na.rm = T),
         FY18Q1Q2=sum(FY2018Q2,-FY2017Q4,na.rm = T),indicator='TX_NET_NEW')


df18_15_fil <- dfct15_18_grouped2_snuP %>% rowwise() %>% mutate(FY15Q1Q2=case_when(
  indicator=='TX_CURR' ~ FY2015Q2,
  indicator=='TX_NEW'~FY2015Q2,
  TRUE ~ 0),
  FY15Q3Q4=case_when(
    indicator=='TX_CURR' ~ FY2015Q4,
    indicator=='TX_NEW'~sum(FY2015Q3,FY2015Q4,na.rm = T),
    TRUE ~ 0),
  FY16Q1Q2=case_when(
    indicator=='TX_CURR' ~ FY2016Q2,
    indicator=='TX_NEW'~sum(FY2016Q1,FY2016Q2,na.rm = T),
    TRUE ~ 0),
  FY16Q3Q4=case_when(
    indicator=='TX_CURR' ~ FY2016Q4,
    indicator=='TX_NEW'~sum(FY2016Q3,FY2016Q4,na.rm = T),
    TRUE ~ 0),
  FY17Q1Q2=case_when(
    indicator=='TX_CURR' ~ FY2017Q2,
    indicator=='TX_NEW'~sum(FY2017Q1,FY2017Q2,na.rm = T),
    TRUE ~ 0),
  FY17Q3Q4=case_when(
    indicator=='TX_CURR' ~ FY2017Q4,
    indicator=='TX_NEW'~sum(FY2017Q3,FY2017Q4,na.rm = T),
    TRUE ~ 0),
  FY18Q1Q2= case_when(
    indicator=='TX_CURR' ~ FY2018Q2,
    indicator=='TX_NEW'~sum(FY2018Q1,FY2018Q2,na.rm = T),
    TRUE ~ 0
  ))

#check if all col names match before appending
setdiff(colnames(df18_15_fil),colnames(dfnet.new18))


df.final18 <- rbind(df18_15_fil,dfnet.new18)

write.xlsx(df.final18,'fy18Q2__clean_psnu_IM.xlsx')

#---- * NAT _ SUBNAT data for PLHIV----

nat_subnat <- read_delim("ICPI_FactView_NAT_SUBNAT_20180215_v4_1.txt", 
                         +     "\t", escape_double = FALSE, trim_ws = TRUE)
plhiv <- nat_subnat %>% filter(indicator=='PLHIV', disaggregate %in% c('Age/Sex', 'Total Numerator'))

write.xlsx(plhiv, 'plhiv_FY17.xlsx')


