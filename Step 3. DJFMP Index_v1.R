# DJFMP Annual Report- Salmonid Data manipulation and CPUE calculations----------------------------------------
  
  ### Author- Ryan Mckenzie
  ### Contact- ryan_mckenzie@fws.gov
  ### Date- 1/19/21
  ### R version- 4.0.3

#Enter water year for Current Report
fieldyear<-2024

#Enter years that you want Chipps Island CHN FL distributions for. Last 5 years has been standard.
FL_years<-c("2024","2023","2022","2021","2020")

# Target Dates ------------------------------------------------------------
#Currently use water year, so Oct '18 through Sept '19 is 2019
date_start <- as.Date("1999-08-01", format = "%Y-%m-%d")
date_end <- as.Date(paste(fieldyear,"-07-31",sep=""), format = "%Y-%m-%d")

# Working Environment-------------------------------------------------------

## Load Packages
library("odbc")
library("tidyr")
library("plyr")
library("dplyr")
library("unmarked")
library(lubridate)
library('readxl')
library(purrr)
options(scipen=100)
####################Pull in Data from Database#########################
# Server Information ------------------------------------------------------
sql_driver_str <- "SQL Server Native Client 11.0"
djfmp_user_id <- "djfmpreader"
djfmp_user_password <- "d1fmpR0ad3rPr0d"
first_entry_server_name <- "ifw9bct-sqlha1"


# Open Database Connections -----------------------------------------------
DJFMP_con <- odbc::dbConnect(drv=odbc::odbc(), 
                                 driver=sql_driver_str, 
                                 server=first_entry_server_name,
                                 uid=djfmp_user_id, 
                                 pwd=djfmp_user_password)

# Pull in data tables from Denver and Local DJFMP databases----------------------------------------
#Denver Database Tables
sample_table <-  sprintf(" SELECT *FROM Sample WHERE SampleDate BETWEEN '%s' AND '%s';",date_start,date_end)
Samples <- DBI::dbGetQuery (conn=DJFMP_con, 
                            statement=sample_table)
sample_start <- min(Samples$SampleID)
sample_end <- max(Samples$SampleID)

station_code_table <- "SELECT * FROM ref_station_code;"
Stations<- DBI::dbGetQuery (conn=DJFMP_con, 
                            statement=station_code_table)

catch_table <- sprintf("SELECT * FROM Catch WHERE SampleID BETWEEN '%s' AND '%s';",sample_start,sample_end)
Catch<- DBI::dbGetQuery (conn=DJFMP_con, 
                         statement=catch_table)

organism_code_table <- "SELECT * FROM ref_organism_code;"
Organisms<- DBI::dbGetQuery (conn=DJFMP_con, 
                             statement=organism_code_table)

CWT_code_table <- "SELECT * FROM CodeWireTag;"
CWT_codes<- DBI::dbGetQuery (conn=DJFMP_con, 
                             statement=CWT_code_table)
dbDisconnect(DJFMP_con)




# Format Effort Data------------------------------------------------------------------------
Stations <- Stations %>%
  rename("StationCode"=station_code)

Effort<-left_join(Samples, Stations, by="StationCode")


Effort<- Effort %>% 
  mutate(Samplehour=hour(hms(SampleTime)))%>%
  filter(Samplehour<=18 & Samplehour>=6)%>% ##filters to daylight hours from 6 am to 6 pm
  filter(MethodCode %in% c("KDTR", "MWTR", "SEIN"))%>%
  filter(GearConditionCode %in% c("1","2"))%>% # filters data to only include good samples--i.e. gear condition code 1 and 2
  filter(FlowDebris %in% c("N",NA))%>% # only include samples where no debris was recorded. with flow debris present
  mutate(Volume= case_when(description=="Chipps Island"& MethodCode=="MWTR"~(FlowmeterDifference*18.58*0.026873), 
                           MethodCode=="KDTR"~(FlowmeterDifference*12.54*0.026873), 
                           description=="Sherwood Harbor"& MethodCode=="MWTR"~(FlowmeterDifference*5.08*0.026873), 
                           MethodCode=="SEIN"~(SeineLength*SeineWidth*(SeineDepth/2))))%>%
select(SampleID, SampleDate, description, RegionCode,StationCode,MethodCode,Volume, SeineLength,SeineWidth,SeineDepth)%>%
separate(SampleDate, c("YEAR","MONTH","DAY"))%>%
mutate(MONTH= case_when(MONTH =="08"~"Aug", MONTH =="09"~"Sep", MONTH =="10"~"Oct", MONTH =="11"~"Nov", MONTH =="12"~"Dec", MONTH =="01"~"Jan", 
                          MONTH =="02"~"Feb", MONTH =="03"~"Mar", MONTH =="04"~"Apr",MONTH =="05"~"May", MONTH =="06"~"Jun", MONTH =="07"~"Jul" ))%>%
unique() # unique removes duplicate rows 

Effort$MONTH<-factor(Effort$MONTH, levels = c("Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul"))
Effort$YEAR<-as.numeric(Effort$YEAR)
Effort$FieldYear<- ifelse (Effort$MONTH %in% c("Aug","Sep","Oct","Nov","Dec"), Effort$YEAR+1, Effort$YEAR)
Effort$SampleID<-as.factor(Effort$SampleID)
Effort$DAY<-as.factor(Effort$DAY)



# Subset and clean up Trawl Effort######
Trawl_effort<-subset(Effort, MethodCode %in% c("KDTR","MWTR"))

### Subset Data and Look for Outliers
MWTR_Sherwood<-subset(Trawl_effort, MethodCode=="MWTR" & description=="Sherwood Harbor")
KDTR_Sherwood<-subset(Trawl_effort, MethodCode=="KDTR" & description=="Sherwood Harbor")
MWTR_Chipps<-subset(Trawl_effort, MethodCode=="MWTR" & description=="Chipps Island")
KDTR_Mossdale<-subset(Trawl_effort, MethodCode=="KDTR" & description=="Mossdale")

All_Trawls<- bind_rows(MWTR_Chipps,MWTR_Sherwood,KDTR_Mossdale,KDTR_Sherwood)
### Remove Outliers in Volume of Water Sampled. Outlier criteria for trawls is volumes below 1,000 or above 50,000 cubic meters.
#This cutoff range was determine by exploring the dataset and looking at the normal volume ranges of trawls. 

MWTR_Chipps<-MWTR_Chipps %>% filter (Volume >1000 & Volume<50000)
MWTR_Sherwood<-MWTR_Sherwood %>% filter (Volume >1000 & Volume<50000)
KDTR_Mossdale<-KDTR_Mossdale %>% filter (Volume >1000 & Volume<50000)
KDTR_Sherwood<-KDTR_Sherwood %>% filter (Volume >1000 & Volume<50000)

### Recombine Cleaned Datasets and Format
Trawl_effort<- bind_rows(MWTR_Chipps,MWTR_Sherwood,KDTR_Mossdale,KDTR_Sherwood)

Outlier_Count<- as.numeric(count(All_Trawls))-as.numeric(count(Trawl_effort))
Outliers<-anti_join(All_Trawls, Trawl_effort, by="SampleID")

TrawlEffortFieldyear<-Trawl_effort%>% ##used for plotting unknown fish origins before 2007
  select(SampleID,FieldYear)

#remove unnecessary datasets
rm(KDTR_Mossdale,KDTR_Sherwood,MWTR_Chipps,MWTR_Sherwood)

# Format CWT Data---------------------------------------------------------------
#Pull in CWT tag codes
CWT<-read_xlsx("\\\\IFW8LODI-FS\\Common\\DJFMP Not IEP\\Access\\All_CA_CWT.xlsx",sheet = 1, guess_max=5000)

CWT$cwt_1st_mark_count <- as.numeric(CWT$cwt_1st_mark_count)
CWT$cwt_2nd_mark_count <- as.numeric(CWT$cwt_2nd_mark_count)
CWT$non_cwt_2nd_mark_count <- as.numeric(CWT$non_cwt_2nd_mark_count)
CWT$non_cwt_1st_mark_count <- as.numeric(CWT$non_cwt_1st_mark_count)

## Format CWT Dataset
CWT<-CWT %>%
  map_if(is.numeric,~ifelse(is.na(.x),0,.x))%>%
  as.data.frame()


CWT<-CWT %>%
  group_by(tag_code_or_release_id)%>%
   mutate(MarkedCount=(if_else(cwt_1st_mark %in% c("0000","0009","9000","9009",NA) , 0,cwt_1st_mark_count))+
           (ifelse(cwt_2nd_mark %in% c("0000","0009","9000","9009",NA) , 0, cwt_2nd_mark_count))+
           (ifelse(non_cwt_2nd_mark %in% c("0000","0009","9000","9009",NA), 0, non_cwt_2nd_mark_count))+
           (ifelse(non_cwt_1st_mark %in% c("0000","0009","9000","9009",NA) , 0, non_cwt_1st_mark_count)))%>%
   mutate(UnmarkedCount=(ifelse(cwt_1st_mark %in% c("0000",NA), cwt_1st_mark_count, 0))+
           (ifelse(cwt_2nd_mark %in% c("0000",NA),  cwt_2nd_mark_count, 0))+
           (ifelse(non_cwt_2nd_mark %in% c("0000",NA), non_cwt_2nd_mark_count, 0))+
           (ifelse(non_cwt_1st_mark %in% c("0000",NA), non_cwt_1st_mark_count, 0)))%>%
   mutate(Unknown=(ifelse(cwt_1st_mark %in% c("0009","9000","9009",NA), cwt_1st_mark_count, 0))+
           (ifelse(cwt_2nd_mark %in% c("0009","9000","9009",NA) , cwt_2nd_mark_count, 0))+
           (ifelse(non_cwt_2nd_mark %in% c("0009","9000","9009",NA) , non_cwt_2nd_mark_count, 0))+
           (ifelse(non_cwt_1st_mark %in% c("0009","9000","9009",NA) , non_cwt_1st_mark_count, 0)))%>%
   mutate(Totalcount=cwt_1st_mark_count+cwt_2nd_mark_count+non_cwt_1st_mark_count+non_cwt_2nd_mark_count)%>%
   mutate(RatioUnmarkedvsMarked=(UnmarkedCount/MarkedCount))%>%
   mutate(RaceByCWT=case_when(run %in% '1'~"Spring", run %in% '2'~ "Summer", run %in% '3' ~ "Fall", run %in% '5'~ "Hybrid", 
                              run %in% '6'~ "Landlocked", run %in% '7'~ "LateFall",run %in% '4'~"Winter", run %in% '8'~"LateFall"))%>%
   select(tag_code_or_release_id, RaceByCWT, first_release_date, Totalcount, MarkedCount, UnmarkedCount,Unknown, RatioUnmarkedvsMarked)%>%
   rename(TagCode=tag_code_or_release_id)

# Format Catch Data-------------------------------------------------------------
  # RaceByTag (dbo_CodeWireTag)---field used before 2014
  # RaceByTag (dbo_Catch)---field used after 2014

SalmonidCatch<-subset(Catch, OrganismCode %in% c("CHN","RBT"))

CWT_codes<-na.omit(CWT_codes[,c("CatchID","TagCode","RaceByTag")])

SalmonidCatch<-left_join(SalmonidCatch,CWT_codes, by="CatchID")


summary(as.factor(SalmonidCatch$MarkCode))
summary(as.factor(SalmonidCatch$RaceByLength))
summary(as.factor(SalmonidCatch$RaceByTag.x))
summary(as.factor(SalmonidCatch$RaceByTag.y))


SalmonidCatch<-SalmonidCatch %>% 
  mutate(TagCode=ifelse(is.na(TagCode.x),TagCode.y,TagCode.x))%>%
  filter(MarkCode %in% c("Adclipped","AdClipped","none","None","Plvc","PlvcLft + Ad"))%>%  # remove trawl efficiency fish that have VIE markings and Acoustic tags
  filter(!TagCode %in% c("YeSd(86)","YeSd(77)","YeSd(76)2","YeSd(76)1","ReSd(86)","ReSd(77)2","ReSd(77)1","ReSd(76)2","ReSd(76)2","ReSd(76)1"))%>% # remove the rest of efficiency fish
  mutate(TagCode=ifelse(TagCode %in% c(" ", "n/a","LT","NT","NR","None","none","NoTag", "DT", "LF")|is.na(TagCode),NA, TagCode)) %>%# make tagcode NAs consistent
  mutate(RaceByTag= case_when(RaceByTag.y %in% c("Fall")|RaceByTag.x %in% c("fall","Fall")~"Fall",  ## aggregates columns into one
                              RaceByTag.y %in% c("Hybrid")~"Hybrid", 
                              RaceByTag.y %in% c("LateFall")|RaceByTag.x %in% c("LateFall")~"LateFall", 
                              RaceByTag.y %in% c("Spring")|RaceByTag.x %in% c("spring","Spring")~"Spring", 
                              RaceByTag.y %in% c("Winter")|RaceByTag.x %in% c("winter","Winter")~"Winter",
                              RaceByTag.y %in% c("Not Raced")& RaceByTag.x %in% c("n/p")~""))%>%
  mutate(RaceByLength= as.factor(case_when(RaceByLength %in% c("Fall")~"Fall",              ## replaces n/p and not raced fish with NAs
                                 RaceByLength %in% c("LateFall")~"LateFall",
                                 RaceByLength %in% c("Adult")~"Adult",
                                 RaceByLength %in% c("Spring")~"Spring",
                                 RaceByLength %in% c("Winter")~"Winter"))) %>%
  filter(RaceByLength %in% c("LateFall", "Fall", "Spring" , "Winter")|is.na(RaceByLength)) ##Filters out adults

SalmonidCatchSum<-SalmonidCatch%>%
  select(SampleID,OrganismCode, MarkCode, StageCode,TagCode,RaceByTag,RaceByLength,Count)%>%
  group_by(SampleID,OrganismCode, MarkCode, StageCode,TagCode,RaceByTag,RaceByLength)%>%
  transmute(SumOfCatchCount=sum(Count))%>%
  unique()%>%
  as.data.frame()

SalmonidCatchSum$SumOfCatchCount<-as.numeric(as.character(SalmonidCatchSum$SumOfCatchCount))####Have to properly convert counts from factor to numeric variable
SalmonidCatchSum$SampleID<-as.factor(SalmonidCatchSum$SampleID)

##Make a dataset for Forklength Data
SalmonidFL<-SalmonidCatch%>%
  select(SampleID,OrganismCode, MarkCode, StageCode,TagCode,RaceByTag,RaceByLength,ForkLength,Count)%>%
  group_by(SampleID,OrganismCode, MarkCode, StageCode,TagCode,RaceByTag,RaceByLength, ForkLength)%>%
  transmute(SumOfCatchCount=sum(Count))%>%
  unique()%>%
  as.data.frame()

SalmonidFL$SumOfCatchCount<-as.numeric(as.character(SalmonidFL$SumOfCatchCount))####Have to properly convert counts from factor to numeric variable
SalmonidFL$SampleID<-as.factor(SalmonidFL$SampleID)


#Subset based on Species
CHN_Catch<-subset(SalmonidCatchSum, OrganismCode=="CHN")
RBT_Catch<-subset(SalmonidCatchSum, OrganismCode=="RBT")

CHN_FL<-subset(SalmonidFL, OrganismCode=="CHN")
RBT_FL<-subset(SalmonidFL, OrganismCode=="RBT")

#remove unused dataframes
rm(Catch,CWT_codes,SalmonidCatch,SalmonidCatchSum,SalmonidFL, Samples,Stations,Organisms)
####################CHN Data Manipulation#################################
####CHN Trawls#####
# Step 1. Combine Chinook Catch and CWT Datasets, Assign Race to fish-----------------------------------------------------------

## Add TagCode information (race and ratio of unmarked to marked) to Catch Dataset
CHN_Trawl_Catch<-left_join(CHN_Catch, CWT, by="TagCode")

## Add Sample Year to Catches so we can designate unknown origin Chinook in samples taken before 2008 field year
CHN_Trawl_Catch<-inner_join(TrawlEffortFieldyear, CHN_Trawl_Catch, by="SampleID")


## QC Datset and make corrections
CHN_Trawl_Catch<-CHN_Trawl_Catch %>% 
  mutate(MarkCode=ifelse(MarkCode %in% c("none", "None")|is.na(MarkCode),"None", "Marked"))%>%
  mutate(RaceByTag=ifelse(is.na(RaceByCWT), RaceByTag, RaceByCWT))%>%
  mutate(Race=case_when(is.na(RaceByTag) & MarkCode=="Marked"~"Unknown", 
                        is.na(RaceByTag) & MarkCode == "None" ~as.character(RaceByLength),  
                        !is.na(RaceByTag)~as.character(RaceByTag)))%>%
  select(SampleID,FieldYear, TagCode,RatioUnmarkedvsMarked,Race, MarkCode, SumOfCatchCount)
CHN_Trawl_Catch$RatioUnmarkedvsMarked[is.na(CHN_Trawl_Catch$RatioUnmarkedvsMarked)] <- 0

## Calculate and add columns for Marked and Unmarked Chinook counts for specific tag codes within samples 
CHN_Trawl_Catch<-CHN_Trawl_Catch %>%
  mutate(Marked_Count= case_when(MarkCode %in% c("Marked")~SumOfCatchCount)) %>%  #creat count of marked individuals
  group_by(SampleID,FieldYear, TagCode, RatioUnmarkedvsMarked) %>% 
  mutate(Marked_Count_per_tagcode=case_when(MarkCode %in% c("Marked")&!is.na(TagCode)~sum(Marked_Count))) %>% #sum counts of marked individuals with specific tagcodes within samples
  ungroup()%>%
  mutate(Unmarked_Count_per_tagcode = RatioUnmarkedvsMarked * Marked_Count_per_tagcode) #sum counts of unmarked individuals based on marking ratio and #of marked individuals for specific tag codes
  
CHN_Trawl_Catch$Marked_Count[is.na(CHN_Trawl_Catch$Marked_Count)]<-0
CHN_Trawl_Catch$Marked_Count_per_tagcode[is.na(CHN_Trawl_Catch$Marked_Count_per_tagcode)]<-0
CHN_Trawl_Catch$Unmarked_Count_per_tagcode[is.na(CHN_Trawl_Catch$Unmarked_Count_per_tagcode)]<-0

CHN_taggers_catch<-inner_join(subset(Trawl_effort, FieldYear==fieldyear), CHN_Trawl_Catch, by="SampleID")
# Step 2. Subset Dataset by Races of interest------------------------------------------------------------------------------------
CHNW_Trawl_Catch<-subset(CHN_Trawl_Catch, Race=="Winter")
CHNS_Trawl_Catch<-subset(CHN_Trawl_Catch,Race=="Spring")
CHNLF_Trawl_Catch<-subset(CHN_Trawl_Catch,Race=="LateFall")
CHNF_Trawl_Catch<-subset(CHN_Trawl_Catch, Race == "Fall")

# Step 3. Calculate and summarize chinook catches for marked, unmarked, wild, and unknown individuals per sample for each race -------------------------------

## Winter Run
CHNW_Trawl_Catch_Summary<-CHNW_Trawl_Catch%>%
  group_by(SampleID)%>%
  mutate(Total_CHN= sum(SumOfCatchCount))%>%
  mutate(Marked_Total=sum(Marked_Count))%>%
  mutate(Unmarked_Total= if(Total_CHN-Marked_Total >0){round(sum(Unmarked_Count_per_tagcode))}else{0})%>%
  mutate(Unmarked_Total= if((Total_CHN-Marked_Total) < Unmarked_Total){Total_CHN-Marked_Total}else{Unmarked_Total})%>%
  mutate(Wild= if(Total_CHN-(Marked_Total+Unmarked_Total) >0){round(Total_CHN-(Marked_Total+Unmarked_Total))}else{0})%>%
  gather("Origin", "Count",12:14 )%>%
  select(SampleID, Total_CHN, Origin, Count)%>%
  unique()

### Data QC: You can check to make sure that all fish are accounted for by making sure the counts below match
sum(CHNW_Trawl_Catch_Summary$Total_CHN)/3
sum(CHNW_Trawl_Catch_Summary$Count)

## Spring Run
CHNS_Trawl_Catch_Summary<-CHNS_Trawl_Catch%>%
  group_by(SampleID)%>%
  mutate(Total_CHN= sum(SumOfCatchCount))%>%
  mutate(Marked_Total=sum(Marked_Count))%>%
  mutate(Unmarked_Total= if(Total_CHN-Marked_Total >0){round(sum(Unmarked_Count_per_tagcode))}else{0})%>%
  mutate(Unmarked_Total= if((Total_CHN-Marked_Total) < Unmarked_Total){Total_CHN-Marked_Total}else{Unmarked_Total})%>%
  mutate(Wild= if(Total_CHN-(Marked_Total+Unmarked_Total) >0){round(Total_CHN-(Marked_Total+Unmarked_Total))}else{0})%>%
  gather("Origin", "Count",12:14 )%>%
  select(SampleID, Total_CHN, Origin, Count)%>%
  unique()

### Data QC: You can check to make sure that all fish are accounted for by making sure the counts below match
sum(CHNS_Trawl_Catch_Summary$Total_CHN)/3
sum(CHNS_Trawl_Catch_Summary$Count)

## Late Fall Run Chinook
CHNLF_Trawl_Catch_Summary<-CHNLF_Trawl_Catch%>%
  group_by(SampleID)%>%
  mutate(Total_CHN= sum(SumOfCatchCount))%>%
  mutate(Marked_Total=sum(Marked_Count))%>%
  mutate(Unmarked_Total= if(Total_CHN-Marked_Total >0){round(sum(Unmarked_Count_per_tagcode))}else{0})%>%
  mutate(Unmarked_Total= if((Total_CHN-Marked_Total) < Unmarked_Total){Total_CHN-Marked_Total}else{Unmarked_Total})%>%
  mutate(Wild= if(Total_CHN-(Marked_Total+Unmarked_Total) >0){round(Total_CHN-(Marked_Total+Unmarked_Total))}else{0})%>%
  gather("Origin", "Count",12:14 )%>%
  select(SampleID, Total_CHN, Origin, Count)%>%
  unique()

### Data QC: You can check to make sure that all fish are accounted for by making sure the counts below match
sum(CHNLF_Trawl_Catch_Summary$Total_CHN)/3
sum(CHNLF_Trawl_Catch_Summary$Count)

## All other races combined
CHNF_Trawl_Catch_Summary<-CHNF_Trawl_Catch %>%
  group_by(SampleID)%>%
  mutate(Total_CHN= sum(SumOfCatchCount))%>%
  mutate(Marked_Total=sum(Marked_Count))%>%
  mutate(Unknown= if(FieldYear<=2007){Total_CHN-Marked_Total}else{0})%>%
  mutate(Unmarked_Total= if(Total_CHN-Marked_Total >0){round(sum(Unmarked_Count_per_tagcode))}else{0})%>%
  mutate(Unmarked_Total= if((Total_CHN-Marked_Total) < Unmarked_Total){Total_CHN-Marked_Total}else{Unmarked_Total})%>%
  mutate(Unmarked_Total= if(FieldYear>=2008){Unmarked_Total}else{0})%>%
  mutate(Wild= if(Total_CHN-(Marked_Total+Unmarked_Total) >0){round(Total_CHN-(Marked_Total+Unmarked_Total))}else{0})%>%
  mutate(Wild= if(FieldYear>=2008){Wild}else{0})%>%
  gather("Origin", "Count",12:15 )%>%
  select(SampleID, Total_CHN,Origin, Count)%>%
  unique()

### Data QC: You can check to make sure that all fish are accounted for by making sure the counts below match
sum(CHNF_Trawl_Catch_Summary$Total_CHN)/4
sum(CHNF_Trawl_Catch_Summary$Count)

# Step 4. Combine catch and effort datasets, format, add zero catches, calculate CPUE -----------------------------------------------------------------------------------------------------
#Winter Run Chinook
CHNW_Trawl_CPUE<-left_join(Trawl_effort,CHNW_Trawl_Catch_Summary, by = "SampleID")%>%
  select(SampleID,FieldYear, MONTH, DAY, description, StationCode, Volume,MethodCode,Origin, Count)%>%
  filter(!is.na(Volume))

## Format Dataset
CHNW_Trawl_CPUE$Origin[is.na(CHNW_Trawl_CPUE$Origin)]<-"Wild"
CHNW_Trawl_CPUE$Count[is.na(CHNW_Trawl_CPUE$Count)]<-0 
CHNW_Trawl_CPUE$Origin<-as.factor(CHNW_Trawl_CPUE$Origin)
CHNW_Trawl_CPUE$SampleID<-as.factor(CHNW_Trawl_CPUE$SampleID)
CHNW_Trawl_CPUE$FieldYear<-as.factor(CHNW_Trawl_CPUE$FieldYear)

## Add zeroes for samples with no catch
CHNW_Trawl_CPUE<- CHNW_Trawl_CPUE %>% 
  complete(nesting(SampleID, MONTH, DAY, FieldYear,description, StationCode, Volume, MethodCode), Origin, fill=list(Count=0))%>%
  mutate(CPUE=Count/Volume)%>%
  unique()

#Spring Run Chinook
CHNS_Trawl_CPUE<-left_join(Trawl_effort,CHNS_Trawl_Catch_Summary, by = "SampleID")%>%
  select(SampleID,FieldYear, MONTH, DAY, description, StationCode, Volume,MethodCode,Origin, Count)%>%
  filter(!is.na(Volume))

## Format Dataset
CHNS_Trawl_CPUE$Origin[is.na(CHNS_Trawl_CPUE$Origin)]<-"Wild"
CHNS_Trawl_CPUE$Count[is.na(CHNS_Trawl_CPUE$Count)]<-0 
CHNS_Trawl_CPUE$Origin<-as.factor(CHNS_Trawl_CPUE$Origin)
CHNS_Trawl_CPUE$SampleID<-as.factor(CHNS_Trawl_CPUE$SampleID)
CHNS_Trawl_CPUE$FieldYear<-as.factor(CHNS_Trawl_CPUE$FieldYear)

## Add zeroes for samples with no catch
CHNS_Trawl_CPUE<- CHNS_Trawl_CPUE %>% 
  complete(nesting(SampleID, MONTH, DAY, FieldYear,description, StationCode, Volume, MethodCode), Origin, fill=list(Count=0))%>%
  mutate(CPUE=Count/Volume)%>%
  unique()

#Late Fall Run Chinook
CHNLF_Trawl_CPUE<-left_join(Trawl_effort,CHNLF_Trawl_Catch_Summary, by = "SampleID")%>%
  select(SampleID,FieldYear, MONTH, DAY, description, StationCode, Volume,MethodCode,Origin, Count)%>%
  filter(!is.na(Volume))

## Format Dataset
CHNLF_Trawl_CPUE$Origin[is.na(CHNLF_Trawl_CPUE$Origin)]<-"Wild"
CHNLF_Trawl_CPUE$Count[is.na(CHNLF_Trawl_CPUE$Count)]<-0 
CHNLF_Trawl_CPUE$Origin<-as.factor(CHNLF_Trawl_CPUE$Origin)
CHNLF_Trawl_CPUE$SampleID<-as.factor(CHNLF_Trawl_CPUE$SampleID)
CHNLF_Trawl_CPUE$FieldYear<-as.factor(CHNLF_Trawl_CPUE$FieldYear)

## Add zeroes for samples with no catch
CHNLF_Trawl_CPUE<- CHNLF_Trawl_CPUE %>% 
  complete(nesting(SampleID, MONTH, DAY, FieldYear,description, StationCode, Volume, MethodCode), Origin, fill=list(Count=0))%>%
  mutate(CPUE=Count/Volume)%>%
  unique()

## Fall Run Chinook
CHNF_Trawl_CPUE<-left_join(Trawl_effort, CHNF_Trawl_Catch_Summary, by = "SampleID")%>%
  select(SampleID, FieldYear, MONTH, DAY, description, StationCode, Volume, MethodCode,Origin, Count)%>%
  filter(!is.na(Volume))

## Format Dataset
CHNF_Trawl_CPUE$Origin[is.na(CHNF_Trawl_CPUE$Origin)]<-"Wild"
CHNF_Trawl_CPUE$Count[is.na(CHNF_Trawl_CPUE$Count)]<-0 
CHNF_Trawl_CPUE$Origin<-as.factor(CHNF_Trawl_CPUE$Origin)
CHNF_Trawl_CPUE$SampleID<-as.factor(CHNF_Trawl_CPUE$SampleID)
CHNF_Trawl_CPUE$FieldYear<-as.factor(CHNF_Trawl_CPUE$FieldYear)

## Add zeroes for samples with no catch
CHNF_Trawl_CPUE<- CHNF_Trawl_CPUE %>% 
  complete(nesting(SampleID, FieldYear, MONTH, DAY, description, StationCode, Volume, MethodCode), Origin, fill=list(Count=0))%>%
  mutate(CPUE=Count/Volume)%>%
  unique()

             
# Step 5. Calculate Monthly Means for Regions for Current sampling year-------------------------------------------------------                           


####With kodiak and mid water combined
CHNW_Trawl_Monthly_Means<-CHNW_Trawl_CPUE %>%
  group_by(FieldYear, MONTH, DAY,description,StationCode,Origin) %>% ##mean daily CPUE by Station
  summarise(CPUE_day_station=mean(CPUE, na.rm = TRUE)) %>%
  group_by(FieldYear, MONTH, DAY,description,Origin) %>% ##mean daily CPUE by site
  summarise(CPUE_day_site=mean(CPUE_day_station, na.rm = TRUE)) %>%
  group_by(FieldYear, MONTH, description, Origin)%>%  ##mean monthly CPUE by site
  summarise(CPUE_month_site=mean(CPUE_day_site, na.rm = TRUE))%>%
  filter(!is.nan(CPUE_month_site))

## Monthly Means for Current Year
CHNW_CurrentYear_Trawl_Monthly_Means<-CHNW_Trawl_Monthly_Means %>%
    filter(FieldYear==fieldyear)  


####With kodiak and mid water combined
CHNS_Trawl_Monthly_Means<-CHNS_Trawl_CPUE %>%
  group_by(FieldYear, MONTH, DAY,description,StationCode,Origin) %>% ##mean daily CPUE by Station
  summarise(CPUE_day_station=mean(CPUE, na.rm = TRUE)) %>%
  group_by(FieldYear, MONTH, DAY,description,Origin) %>% ##mean daily CPUE by site
  summarise(CPUE_day_site=mean(CPUE_day_station, na.rm = TRUE)) %>%
  group_by(FieldYear, MONTH, description, Origin)%>%  ##mean monthly CPUE by site
  summarise(CPUE_month_site=mean(CPUE_day_site, na.rm = TRUE))%>%
  filter(!is.nan(CPUE_month_site))

## Monthly Means for Current Year
CHNS_CurrentYear_Trawl_Monthly_Means<-CHNS_Trawl_Monthly_Means %>%
  filter(FieldYear==fieldyear)  

####With kodiak and mid water combined
CHNLF_Trawl_Monthly_Means<-CHNLF_Trawl_CPUE %>%
  group_by(FieldYear, MONTH, DAY,description,StationCode,Origin) %>% ##mean daily CPUE by Station
  summarise(CPUE_day_station=mean(CPUE, na.rm = TRUE)) %>%
  group_by(FieldYear, MONTH, DAY,description,Origin) %>% ##mean daily CPUE by site
  summarise(CPUE_day_site=mean(CPUE_day_station, na.rm = TRUE)) %>%
  group_by(FieldYear, MONTH, description, Origin)%>%  ##mean monthly CPUE by site
  summarise(CPUE_month_site=mean(CPUE_day_site, na.rm = TRUE))%>%
  filter(!is.nan(CPUE_month_site))

## Monthly Means for Current Year
CHNLF_CurrentYear_Trawl_Monthly_Means<-CHNLF_Trawl_Monthly_Means %>%
  filter(FieldYear==fieldyear)  

####With kodiak and mid water combined
CHNF_Trawl_Monthly_Means<-CHNF_Trawl_CPUE %>%
  group_by(FieldYear, MONTH, DAY,description,StationCode,Origin) %>% ##mean daily CPUE by Station
  summarise(CPUE_day_station=mean(CPUE, na.rm = TRUE)) %>%
  group_by(FieldYear, MONTH, DAY,description,Origin) %>% ##mean daily CPUE by site
  summarise(CPUE_day_site=mean(CPUE_day_station, na.rm = TRUE)) %>%
  group_by(FieldYear, MONTH, description, Origin)%>%  ##mean monthly CPUE by site
  summarise(CPUE_month_site=mean(CPUE_day_site, na.rm = TRUE))%>%
  filter(!is.nan(CPUE_month_site))



## Monthly Means for Current Year
CHNF_CurrentYear_Trawl_Monthly_Means<-CHNF_Trawl_Monthly_Means %>%
  filter(FieldYear==fieldyear)  



# Step 6. Calculate Annual Means of CPUE for Regions ------------------------------------------------------------------                           


##Combined kodiak and midwater trawl
CHNW_Trawl_Annual_Means<- CHNW_Trawl_Monthly_Means%>%
  group_by(FieldYear, description,Origin)%>%
  summarise(Mean_year_site=mean(CPUE_month_site, na.rm = TRUE))%>%
  select(FieldYear,description, Origin, Mean_year_site)%>%
  unique()

##Combined kodiak and midwater trawl
CHNS_Trawl_Annual_Means<- CHNS_Trawl_Monthly_Means%>%
  group_by(FieldYear, description,Origin)%>%
  summarise(Mean_year_site=mean(CPUE_month_site, na.rm = TRUE))%>%
  select(FieldYear,description, Origin, Mean_year_site)%>%
  unique()

##Combined kodiak and midwater trawl
CHNLF_Trawl_Annual_Means<- CHNLF_Trawl_Monthly_Means%>%
  group_by(FieldYear, description,Origin)%>%
  summarise(Mean_year_site=mean(CPUE_month_site, na.rm = TRUE))%>%
  select(FieldYear,description, Origin, Mean_year_site)%>%
  unique()

##Combined kodiak and midwater trawl
CHNF_Trawl_Annual_Means<- CHNF_Trawl_Monthly_Means%>%
  group_by(FieldYear, description, Origin)%>%
  summarise(Mean_year_site=mean(CPUE_month_site, na.rm = TRUE))%>%
  select(FieldYear,description, Origin, Mean_year_site)%>%
  unique()

####################RBT Data Manipulation#################################
####RBT Trawls#####
# Step 1. Combine Chinook Catch and CWT Datasets, Assign Race to fish-----------------------------------------------------------

## Add Sample Year to Catches so we can designate unknown origin Chinook in samples taken before 2008 field year
RBT_Trawl_Catch<-inner_join(RBT_Catch,TrawlEffortFieldyear, by="SampleID")

## QC Datset and make corrections
RBT_Trawl_Catch_Summary<-RBT_Trawl_Catch %>% 
  select(SampleID,FieldYear,OrganismCode,MarkCode,SumOfCatchCount)%>%
  mutate(MarkCode=ifelse(MarkCode %in% c("none", "None")|is.na(MarkCode),"None", "Marked"))%>%
  group_by(SampleID)%>%
  mutate(Total_RBT= sum(SumOfCatchCount))%>%
  mutate(Marked_Count= sum(case_when(MarkCode %in% c("Marked")~SumOfCatchCount,
                                     MarkCode %in% c("None")~0))) %>%  #create count of marked individuals
  mutate(Wild= sum(case_when(MarkCode %in% c("None")~SumOfCatchCount,
                             MarkCode %in% c("Marked")~0))) %>%  #create count of marked individuals
  gather("Origin", "Count",7:8 )%>%
  select(SampleID, Total_RBT, Origin, Count)%>%
  unique()

# Step 2. Combine catch and effort datasets, format, add zero catches, calculate CPUE -----------------------------------------------------------------------------------------------------
RBT_Trawl_CPUE<-left_join(Trawl_effort,RBT_Trawl_Catch_Summary, by = "SampleID")%>%
  select(SampleID,FieldYear, MONTH, DAY, description, StationCode, Volume,MethodCode,Origin, Count)%>%
  filter(!is.na(Volume))

## Format Dataset
RBT_Trawl_CPUE$Origin[is.na(RBT_Trawl_CPUE$Origin)]<-"Wild"
RBT_Trawl_CPUE$Count[is.na(RBT_Trawl_CPUE$Count)]<-0 
RBT_Trawl_CPUE$Origin<-as.factor(RBT_Trawl_CPUE$Origin)
RBT_Trawl_CPUE$SampleID<-as.factor(RBT_Trawl_CPUE$SampleID)
RBT_Trawl_CPUE$FieldYear<-as.factor(RBT_Trawl_CPUE$FieldYear)

## Add zeroes for samples with no catch
RBT_Trawl_CPUE<- RBT_Trawl_CPUE %>% 
  complete(nesting(SampleID, MONTH, DAY, FieldYear,description, StationCode, Volume, MethodCode), Origin, fill=list(Count=0))%>%
  mutate(CPUE=Count/Volume)%>%
  unique()


# Step 3. Calculate Monthly Means for Regions for Current sampling year-------------------------------------------------------                           

### Monthly CPUE averages per region were calculated by averaging all samples taken at each station per month. Monthly station averages within each region
### were then averaged together. We found a strong correlation between the average CPUE values calculated this way versus
### the previous way that started by averaging by station per week, then region per week, then region per month. Our new simplified calculations
### streamlined the process and removed the need to create an All date table in Access 

RBT_Trawl_Monthly_Means<-RBT_Trawl_CPUE %>%
  group_by(FieldYear, MONTH, DAY,description,StationCode,Origin) %>% ##mean daily CPUE by Station
  summarise(CPUE_day_station=mean(CPUE, na.rm = TRUE)) %>%
  group_by(FieldYear, MONTH, DAY,description,Origin) %>% ##mean daily CPUE by site
  summarise(CPUE_day_site=mean(CPUE_day_station, na.rm = TRUE)) %>%
  group_by(FieldYear, MONTH, description, Origin)%>%  ##mean monthly CPUE by site
  summarise(CPUE_month_site=mean(CPUE_day_site, na.rm = TRUE))%>%
  filter(!is.nan(CPUE_month_site))

## Monthly Means for Current Year
RBT_CurrentYear_Trawl_Monthly_Means<-RBT_Trawl_Monthly_Means %>%
  filter(FieldYear==fieldyear)  

# Step 4. Calculate Annual Means of CPUE for Regions ------------------------------------------------------------------                           

RBT_Trawl_Annual_Means<- RBT_Trawl_Monthly_Means%>%
  group_by(FieldYear, description,Origin)%>%
  summarise(Mean_year_site=mean(CPUE_month_site, na.rm = TRUE))%>%
  select(FieldYear,description, Origin, Mean_year_site)%>%
  unique()


###################Write CSV###############################################

#Write CSV for final index output
CHNW_Trawl_Annual_Means$Species<-"CHN"
CHNW_Trawl_Annual_Means$Race<-"Winter"
CHNS_Trawl_Annual_Means$Species<-"CHN"
CHNS_Trawl_Annual_Means$Race<-"Spring"
CHNLF_Trawl_Annual_Means$Species<-"CHN"
CHNLF_Trawl_Annual_Means$Race<-"LateFall"
CHNF_Trawl_Annual_Means$Species<-"CHN"
CHNF_Trawl_Annual_Means$Race<-"Fall"
RBT_Trawl_Annual_Means$Species<-"RBT"
RBT_Trawl_Annual_Means$Race<-NA

Annual_Index<-rbind(CHNW_Trawl_Annual_Means,CHNS_Trawl_Annual_Means,CHNLF_Trawl_Annual_Means,CHNF_Trawl_Annual_Means, RBT_Trawl_Annual_Means)%>%
  rename(AnnualCPUE="Mean_year_site", Location="description")%>%
  mutate(AnnualCPUE=AnnualCPUE*10000)%>%
  select(FieldYear,Location,Species, Race, Origin, AnnualCPUE)%>%
  filter(Location %in% c("Chipps Island", "Sherwood Harbor"))%>%
  mutate(Origin=case_when(Origin=="Unmarked_Total"~"UnmarkedHatchery",
                          Origin %in% c("Marked_Total","Marked_Count")~"MarkedHatchery",
                          Origin=="Wild"~"Wild",
                          Origin=="Unknown"~"Unknown"))


write.csv(Annual_Index,file = paste(".\\Data\\",fieldyear,"\\","Annual_Index",fieldyear,".csv",sep=""),row.names = FALSE)






