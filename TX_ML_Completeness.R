#Ad hoc request: TX_ML Completeness
#Q: What % of TX sites report TX_ML?
#Data pulled: 12 Aug 2020 (Daily SitexIM MSDs)
#Last update: 13 Aug 2020

library(tidyverse)

#merge txt files from DATIM for global analysis
#for replication you will need to download
setwd("C:/Users/qkd0/OneDrive - CDC/Treatment Cluster Work/TX_ML/RawData/Txt Files")
Allfiles = list.files()
View(Allfiles)
for (file in Allfiles){
  if(!exists("df1")){df1 = read_tsv(file, col_names = TRUE)}
  if(exists("df1")){
    df2 = read_tsv(file, col_names = TRUE)
    df1 = rbind(df1, df2)
    rm(df2)
  }
}

#Note - parsing errors for Eswatini and Malawi
#Need to figure out issue with those datafiles. 

#trim dataset, remove duplicates to reduce size 
#note: without filtering system will not run, so cannot look
#at other disags for ML completion. 
df2 <- df1 %>%
  filter(standardizeddisaggregate=="Total Numerator",
         fiscal_year == "2020") %>% 
  unique() %>%
  group_by_at(vars(orgunituid:psnu,fiscal_year,indicator)) %>%
  summarise_at(vars(qtr1:qtr2), sum, na.rm=T) %>%
  ungroup() %>%
  select(-c(snu1uid,operatingunituid)) 

#Long dataset transformation
df_long <- df2 %>%
  gather(prd,val,qtr1:qtr2,factor_key = FALSE) %>%
  #combine FY/Q into single reporting period var
  unite("prd", c("fiscal_year", "prd"), sep = " ") %>%
  #separate out TX_CURR/ML
  pivot_wider(names_from = indicator, values_from = val) %>%
  #add reporting variables; including sites that rpt 0 vals.
  mutate(
    txc_rpt = if_else(!is.na(TX_CURR),1,0),
    txml_rpt = if_else(!is.na(TX_ML),1,0),
    txml_nocurr = if_else(is.na(TX_CURR) & !is.na(TX_ML),1,0)
  ) 

#Summary table at OU level
txml_OU_summary <- df_long %>%
  group_by(operatingunit,prd) %>%
  summarize(txc_rpt_site = sum(txc_rpt, na.rm = T),
            TX_CURR = sum(TX_CURR, na.rm = T),
            txml_rpt_site= sum(txml_rpt, na.rm = T),
            TX_ML = sum(TX_ML, na.rm = T),
            txml_nocurr = sum(txml_nocurr, na.rm = T)) %>%
  mutate(txmlcomp = txml_rpt_site/txc_rpt_site)


#output for dissemination
write_csv(df_long,"../txml_sitelevel.csv", na="")
write_csv(txml_OU_summary,"../txml_OU_summary.csv", na="")

