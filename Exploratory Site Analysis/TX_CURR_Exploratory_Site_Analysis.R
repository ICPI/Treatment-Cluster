#Exploratory Site Analysis
#Characterizing Changes to Sites Reporting Changes to TX_CURR
#Created: 27 May 2020
#Last update: 23 Sep 2020


.libPaths("C:/Users/qkd0/Libraries") #Note - I had to adjust lib paths for R to recognize librariess; future users can remove

library(tidyverse)
library(RcppRoll)

#merge txt files from DATIM for global analysis
#for replication you will need to download
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
#remove duplicates
df1 <- unique(df1)

#output for excel pvt manipulation w full/non-manipulated dataset
#write_tsv(df1,"../tx_curr_eda_merge.txt", na="")

#merge targets and column rows for later pivotting
df2 <- df1 %>%
  group_by_at(vars(orgunituid:fiscal_year)) %>%
  summarise_at(vars(targets:cumulative), sum, na.rm=T) %>%
  filter(fiscal_year != 2018)

#QC test summarizing retained appropriate values
test1 <- df1 %>%
  group_by(orgunituid,fiscal_year) %>%
  filter(indicator == "TX_CURR", fiscal_year != 2018) %>%
  summarize_at(vars(targets:cumulative), sum, na.rm=TRUE) 
  View(test1)
test2 <- df2 %>%
  group_by(orgunituid,fiscal_year) %>%
  filter(indicator == "TX_CURR") %>%
  summarize_at(vars(targets:cumulative), sum, na.rm=TRUE) 
  View(test2)

#Combine FY/Q into single reporting period var
df_long <- df2 %>%
  gather(prd,val,targets:cumulative,factor_key = FALSE) %>%
  unite("prd", c("fiscal_year", "prd"), sep = " ") %>%
  filter(disaggregate=="Total Numerator")

df_long <- df_long %>%
  mutate(
    prdseq = if_else(prd == "2019 qtr1",101,
                     if_else(prd == "2019 qtr2",102,
                             if_else(prd == "2019 qtr3",103,
                                     if_else(prd == "2019 qtr4",104,
                                             if_else(prd == "2020 qtr1",105,
                                                     if_else(prd == "2020 qtr2",106,
                                                             if_else(prd == "2020 qtr3",107,
                                                                     if_else(prd == "2020 qtr4",108,
                                                                             NULL))))))))
  ) #Sequential Prd val for calculation of add/drop/NC

rm(df1,df2,Allfiles,test1,test2)

#Add TX_CURR_P
tx_curr_long <- df_long %>%
  filter(indicator=="TX_CURR", !grepl("targets",prd), !grepl("cumulative",prd), !grepl("2020 qtr4",prd), !grepl("2021",prd)) %>% 
  rename(TX_CURR_R = val) #filter will need to be updated with quarterly data updates

#tx_curr_long_cdc <- filter(tx_curr_long, grepl("CDC",fundingagency))
#tx_curr_long_usaid <- filter(tx_curr_long, grepl("USAID",fundingagency))

tx_curr_long2 <- tx_curr_long %>%
  group_by_at(vars(orgunituid:psnuuid,prd,prdseq)) %>%
  summarize_at(vars(TX_CURR_R), sum, na.rm=TRUE) %>%
  ungroup() %>%
  group_by(countryname,orgunituid) %>%
  mutate(TX_CURR_P=lag(TX_CURR_R,order_by = prdseq)) %>%
  ungroup()
View(tx_curr_long2)

#Add rows for sites w FY19 Q4 data but no FY20 Q1 for full site drop list
txc2020 <- tx_curr_long2 %>%
  filter(grepl("2020 qtr1",prd)) 
sites_2020 = unique(txc2020$orgunituid)
txc2019 <- tx_curr_long2 %>%
  filter(prd == "2019 qtr4" &!(orgunituid %in% sites_2020)) %>%
  mutate(
    prd = "2020 qtr1",
    prdseq = "105",
    TX_CURR_P = TX_CURR_R,
    TX_CURR_R = 0)
tx_curr_long2 <- rbind(tx_curr_long2,txc2019)
rm(txc2020,txc2019,sites_2020)

#Calculate sites dropped, added, no change
tx_curr_long2 <- tx_curr_long2 %>%
  group_by(orgunituid) %>%
  mutate(
    sites = if_else(TX_CURR_R >0, 1,0),
    site_drop = if_else(
      (TX_CURR_R < 1  & TX_CURR_P > 0),1,0
    ),
    site_add = if_else(
      (TX_CURR_R > 0 & (TX_CURR_P < 1 | is.na(TX_CURR_P))),1,0
    ),
    site_nc = if_else(
      (TX_CURR_R >0 & TX_CURR_P >0),1,0
    )
  )%>%
  mutate(
    tx_curr_drop = if_else(
      site_drop == 1, TX_CURR_P, 0
    ), 
    tx_curr_add = if_else(
      site_add == 1, TX_CURR_R, 0
    ), 
    tx_curr_nc = if_else(
      site_nc == 1, TX_CURR_R, 0
    )
  ) %>%
  ungroup()

#Calculate sites reporting 1-3 times past quarter
tx_curr_long3 <- tx_curr_long2 %>%
  mutate(
    datarpt = if_else(
      TX_CURR_R > 0, 1,0)) %>%
  group_by(orgunituid) %>%
  mutate(
    datarpt2 = roll_sum((datarpt),n=4, fill=NA, align="right"))

#NOTE: Site 1x currently doesn't count new sites
#i.e. a site that only reported FY20Q1 because it was new
#is not counted here. If desired, formula will need to be adjusted. 
      
tx_curr_long3 <- tx_curr_long3 %>%
  mutate(
    sites_1x = if_else(
      datarpt2 == 1,1,0
    ),
    sites_2x = if_else(
      datarpt2 == 2,1,0
    ), 
    sites_3x = if_else(
      datarpt2 == 3,1,0
    )
  ) %>%
  group_by(orgunituid) %>%
  mutate(TX_CURR_P2=lag(TX_CURR_P, order_by=prdseq)) %>%
  mutate(TX_CURR_P3=lag(TX_CURR_P2, order_by=prdseq)) %>%
  mutate(
    tx_curr_1x = if_else(
      sites_1x == 0,0,if_else(
        TX_CURR_R >0, TX_CURR_R, if_else(
          TX_CURR_P >0, TX_CURR_P, if_else(
            TX_CURR_P2 >0, TX_CURR_P2, TX_CURR_P3
            )
          )
        )
      ),
    tx_curr_2x = if_else(
      sites_2x == 0,0,if_else(
        TX_CURR_R >0, TX_CURR_R, if_else(
          TX_CURR_P >0, TX_CURR_P, if_else(
            TX_CURR_P2 >0, TX_CURR_P2, TX_CURR_P3
            )
          )
        )
      ),
    tx_curr_3x = if_else(
      sites_3x == 0,0,if_else(
        TX_CURR_R >0, TX_CURR_R, if_else(
          TX_CURR_P >0, TX_CURR_P, if_else(
            TX_CURR_P2 >0, TX_CURR_P2, TX_CURR_P3)
        )
      )
    )) %>%
  ungroup()
#for QC purposes only
#write_csv(tx_curr_long3,"../tx_curr_sitex.csv", na="")

tx_curr_src <- tx_curr_long3 %>%
  group_by(countryname,snu1,psnu,prd) %>%
  summarize_at(vars(sites,TX_CURR_R,site_drop,tx_curr_drop,site_add,
                    tx_curr_add,site_nc,tx_curr_nc,sites_1x:sites_3x,
                    tx_curr_1x:tx_curr_3x), sum, na.rm=T) %>%
  filter(prd != "2019 qtr1")
View(tx_curr_src)
write_csv(tx_curr_src,"../tx_curr_src.csv", na="")

#################################################
#Mech Change Table: ALL CODE IS DRAFT 
#Generate mechN to id sites reporting multiple mechs, transpose wide
mechtest = subset(tx_curr_long, select = -c(pre_rgnlztn_hq_mech_code:modality)) #subset data to summarize TX_CURR at mech level
mechtest2 <- mechtest %>%
  group_by(orgunituid,sitename,countryname,fundingagency,primepartner,
           snu1,psnu,mech_name,mech_code,prd,prdseq) %>%
  summarise(TX_CURR_R = sum(TX_CURR_R)) %>%
  ungroup() %>%
  group_by(orgunituid,prd) %>%
  mutate(
    #merge mech code and name to single variable for ease of use
    mechCodeName = paste(mech_code,"-",mech_name),
    #count distinct mech codes
    mechN = length(unique(mechCodeName))
  ) %>%
  select(-mech_code,-mech_name) %>%
  ungroup()
  
#Convert mechN to sequential count
mechtest3 <- mechtest2 %>%
  group_by(orgunituid,prd) %>%
  mutate(
    mechN2 = paste(1:n())
  ) %>%
  pivot_wider(names_from = mechN2, 
              names_sep = "_",
              values_from = c(fundingagency,primepartner,mechCodeName,TX_CURR_R))

mechtest4 <- mechtest3 %>%
  group_by(orgunituid) %>%
  mutate(
    mechN_P = lag(mechN, order_by = prdseq),
    fundingagency1_P=lag(fundingagency_1, order_by=prdseq),
    fundingagency2_P=lag(fundingagency_2, order_by=prdseq),
    fundingagency3_P=lag(fundingagency_3, order_by=prdseq),
    fundingagency4_P=lag(fundingagency_4, order_by=prdseq),
    fundingagency5_P=lag(fundingagency_5, order_by=prdseq),
    fundingagency6_P=lag(fundingagency_6, order_by=prdseq),
    primepartner1_P=lag(primepartner_1, order_by=prdseq),
    primepartner2_P=lag(primepartner_2, order_by=prdseq),
    primepartner3_P=lag(primepartner_3, order_by=prdseq),
    primepartner4_P=lag(primepartner_4, order_by=prdseq),
    primepartner5_P=lag(primepartner_5, order_by=prdseq),
    primepartner6_P=lag(primepartner_6, order_by=prdseq),
    mechCodeName1_P=lag(mechCodeName_1, order_by=prdseq),
    mechCodeName2_P=lag(mechCodeName_2, order_by=prdseq),
    mechCodeName3_P=lag(mechCodeName_3, order_by=prdseq),
    mechCodeName4_P=lag(mechCodeName_4, order_by=prdseq),
    mechCodeName5_P=lag(mechCodeName_5, order_by=prdseq),
    mechCodeName6_P=lag(mechCodeName_6, order_by=prdseq),
    TX_CURR1_P=lag(TX_CURR_R_1, order_by=prdseq),
    TX_CURR2_P=lag(TX_CURR_R_2, order_by=prdseq),
    TX_CURR3_P=lag(TX_CURR_R_3, order_by=prdseq),
    TX_CURR4_P=lag(TX_CURR_R_4, order_by=prdseq),
    TX_CURR5_P=lag(TX_CURR_R_5, order_by=prdseq),
    TX_CURR6_P=lag(TX_CURR_R_6, order_by=prdseq),
  ) %>%
  ungroup() %>%
  #filter out sites with no previous records for mech change calc
  filter(prd != "2019 qtr1",
         !is.na(fundingagency1_P)&!is.na(primepartner1_P)&
           !is.na(mechCodeName1_P)&!is.na(TX_CURR1_P)) %>%
  #calculate possible mech change 
  mutate(mechchange = case_when(
    mechN != mechN_P ~ 1, 
    mechN == 1 & (mechCodeName_1 != mechCodeName1_P) ~ 1,
    mechN == 2 & (mechCodeName_1 != mechCodeName1_P|mechCodeName_2 != mechCodeName2_P) ~ 1,
    mechN == 3 & (mechCodeName_1 != mechCodeName1_P|mechCodeName_2 != mechCodeName2_P|mechCodeName_3 != mechCodeName3_P) ~ 1,
    mechN == 4 & (mechCodeName_1 != mechCodeName1_P|mechCodeName_2 != mechCodeName2_P|mechCodeName_3 != mechCodeName3_P|mechCodeName_4 != mechCodeName4_P) ~ 1,
    mechN == 5 & (mechCodeName_1 != mechCodeName1_P|mechCodeName_2 != mechCodeName2_P|mechCodeName_3 != mechCodeName3_P|mechCodeName_4 != mechCodeName4_P|mechCodeName_5 != mechCodeName5_P) ~ 1,
    mechN == 6 & (mechCodeName_1 != mechCodeName1_P|mechCodeName_2 != mechCodeName2_P|mechCodeName_3 != mechCodeName3_P|mechCodeName_4 != mechCodeName4_P|mechCodeName_5 != mechCodeName5_P|mechCodeName_6 != mechCodeName6_P) ~ 1
  ))
           
  
#QC Checks - subset by mech count for quick previews


#cut and remerge multi-mechs into single columns
mechchange <- mechtest4 %>%
  filter(mechchange == 1)

mt5 <- mechtest4 %>% 
  #repivot each var and filter N/A's to remove duplicate
  #rows from the reshape
  pivot_longer(c(mechCodeName_1:mechCodeName_6), names_to = "mechseq", values_to="mechCodeName") %>%
  filter(!is.na(mechCodeName)) %>% 
  pivot_longer(c(mechCodeName1_P:mechCodeName6_P), names_to = "mechseqP", values_to="mechCodeName_P") %>%
  filter(!is.na(mechCodeName_P)) 



#output for excel pvt manipulation, etc.
write_csv(mechchange,"../mechchange.csv", na="")

#################################################
#List non-reporting sites by Qtr
tx_curr_nr <- tx_curr_long %>%
  #grouping variables to prevent miscounting by sites
  #with multiple mec's/dedup's
  group_by(orgunituid,sitename,operatingunit,
           countryname,snu1,psnu,prd,prdseq) %>%
  summarise(TX_CURR_R=sum(TX_CURR_R)) %>%
  ungroup() %>%
  group_by(countryname,orgunituid) %>%
  #calculating previous Q TX_CURR counts
  mutate(TX_CURR_P=lag(TX_CURR_R,order_by = prdseq)) %>%
  ungroup() %>%
  mutate(
    nr_site = if_else(
      TX_CURR_R == 0 & 
        (is.na(TX_CURR_P) | TX_CURR_P == 0)
      ,0,1)) %>%
  #filter out data that has no data reported in past two quarters
  #i.e. new sites, sites already counted when dropped in previous
  #reporting period
  filter(TX_CURR_R == 0, nr_site == 1, prd != "2019 qtr1") %>%
  select(countryname,snu1,psnu,sitename,
         prd,TX_CURR_R,TX_CURR_P)

#output for excel pvt manipulation, etc.
write_csv(tx_curr_nr,"../tx_curr_nr_site.csv", na="")
