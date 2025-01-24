# DJFMP Annual Report- Salmonid Data manipulation and CPUE calculations----------------------------------------

# This is the corrected version using the corrected CWT Code Table for hatchery fish captured before 2014. Removed Liberty Island Seines from dataset. 

### Author- Ryan Mckenzie
### Contact- ryan_mckenzie@fws.gov
### Date- 6/14/22
### R version- 4.0.5

#Enter water year for Current Report
fieldyear <- 2024

#Enter years that you want Chipps Island CHN FL distributions for. Last 5 years has been standard.
FL_years <- c("2024", "2023","2022","2021","2020")

# Target Dates ------------------------------------------------------------
#Currently use water year, so Aug '18 through July '19 is 2019
date_start <- as.Date("1999-08-01", format="%Y-%m-%d")
date_end <- as.Date(paste(fieldyear,"-07-31",sep=""), format="%Y-%m-%d")

# Working Environment-------------------------------------------------------

## Load Packages
#library(tidyverse)
library(odbc)
library(tidyr)
library(dplyr)
library(lubridate)
library(readxl)
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
# Denver Database Tables
sample_table <- sprintf("SELECT *FROM Sample WHERE SampleDate BETWEEN '%s' AND '%s';",
                         date_start, date_end)
Samples <- DBI::dbGetQuery(conn=DJFMP_con, statement=sample_table)
sample_start <- min(Samples$SampleID)
sample_end <- max(Samples$SampleID)

station_code_table <- "SELECT * FROM ref_station_code;"
Stations <- DBI::dbGetQuery(conn=DJFMP_con, statement=station_code_table)

catch_table <- sprintf("SELECT * FROM Catch WHERE SampleID BETWEEN '%s' AND '%s';",
                       sample_start, sample_end)
Catch <- DBI::dbGetQuery(conn=DJFMP_con, statement=catch_table)

organism_code_table <- "SELECT * FROM ref_organism_code;"
Organisms <- DBI::dbGetQuery(conn=DJFMP_con, statement=organism_code_table)

dbDisconnect(DJFMP_con)


# Pull in corrected CWT tag codes before 2014

CWT_codes <- read.csv("//IFW8LODI-FS/Common/DJFMP Not IEP/Access/Corrected_CWT_codes.csv", 
                      header=TRUE)


# Format Effort Data------------------------------------------------------------------------

Stations <- Stations %>%
  dplyr::rename("StationCode"=station_code)

Effort <- left_join(Samples, Stations, by="StationCode")

Effort <- Effort %>% 
  mutate(Samplehour=hour(hms(SampleTime))) %>%
  filter(Samplehour<=18 & Samplehour>=6) %>% ##filters to daylight hours from 6 am to 6 pm
  filter(MethodCode %in% c("KDTR", "MWTR", "SEIN")) %>%
  filter(GearConditionCode %in% c("1","2")) %>% # filters data to only include good samples--i.e. gear condition code 1 and 2
  filter(FlowDebris %in% c("N",NA)) %>% # only include samples where no debris was recorded. with flow debris present
  mutate(Volume=case_when(description=="Chipps Island" & MethodCode=="MWTR"~(FlowmeterDifference*18.58*0.026873), 
                           MethodCode=="KDTR"~(FlowmeterDifference*12.54*0.026873), 
                           description=="Sherwood Harbor" & MethodCode=="MWTR"~(FlowmeterDifference*5.08*0.026873), 
                           MethodCode=="SEIN"~(SeineLength*SeineWidth*(SeineDepth/2)))) %>%
  select(SampleID, SampleDate, description, RegionCode, StationCode, MethodCode, Volume, 
         SeineLength, SeineWidth, SeineDepth)

BeforeFY2014 <- Effort %>% filter(SampleDate <= "2014-05-12")
AfterFY2014 <- Effort %>% filter(SampleDate > "2014-05-12")

Effort <- Effort %>% 
  separate(SampleDate, c("YEAR","MONTH","DAY"))%>%
  mutate(MONTH= case_when(MONTH =="08"~"Aug", MONTH =="09"~"Sep", MONTH =="10"~"Oct", 
                          MONTH =="11"~"Nov", MONTH =="12"~"Dec", MONTH =="01"~"Jan", 
                          MONTH =="02"~"Feb", MONTH =="03"~"Mar", MONTH =="04"~"Apr", 
                          MONTH =="05"~"May", MONTH =="06"~"Jun", MONTH =="07"~"Jul")) %>%
  unique() # unique removes duplicate rows 

Effort$MONTH <- factor(Effort$MONTH, levels=c("Aug","Sep","Oct","Nov","Dec","Jan",
                                              "Feb","Mar","Apr","May","Jun","Jul"))
Effort$YEAR <- as.numeric(Effort$YEAR)
Effort$FieldYear <- ifelse(Effort$MONTH %in% c("Aug","Sep","Oct","Nov","Dec"), 
                           Effort$YEAR+1, Effort$YEAR)
Effort$SampleID <- as.factor(Effort$SampleID)
Effort$DAY <- as.factor(Effort$DAY)



# Subset and clean up Trawl Effort######
Trawl_effort <- subset(Effort, MethodCode %in% c("KDTR","MWTR"))

### Subset Data and Look for Outliers
MWTR_Sherwood <- subset(Trawl_effort, MethodCode=="MWTR" & description=="Sherwood Harbor")
KDTR_Sherwood <- subset(Trawl_effort, MethodCode=="KDTR" & description=="Sherwood Harbor")
MWTR_Chipps <- subset(Trawl_effort, MethodCode=="MWTR" & description=="Chipps Island")
KDTR_Mossdale <- subset(Trawl_effort, MethodCode=="KDTR" & description=="Mossdale")

All_Trawls <- bind_rows(MWTR_Chipps, MWTR_Sherwood, KDTR_Mossdale, KDTR_Sherwood)
### Remove Outliers in Volume of Water Sampled. Outlier criteria for trawls is volumes below 1,000 or above 50,000 cubic meters.
#This cutoff range was determined by exploring the data set and looking at the normal volume ranges of trawls. 

MWTR_Chipps <- MWTR_Chipps %>% filter(Volume > 1000 & Volume < 50000)
MWTR_Sherwood <- MWTR_Sherwood %>% filter(Volume > 1000 & Volume < 50000)
KDTR_Mossdale <- KDTR_Mossdale %>% filter(Volume > 1000 & Volume < 50000)
KDTR_Sherwood <- KDTR_Sherwood %>% filter(Volume > 1000 & Volume < 50000)

### Recombine Cleaned Data Sets and Format
Trawl_effort <- bind_rows(MWTR_Chipps, MWTR_Sherwood, KDTR_Mossdale, KDTR_Sherwood)

Outlier_Count <- as.numeric(count(All_Trawls)) - as.numeric(count(Trawl_effort))
Outliers <- anti_join(All_Trawls, Trawl_effort, by="SampleID")

TrawlEffortFieldyear <- Trawl_effort %>% ##used for plotting unknown fish origins before 2007
  select(SampleID, FieldYear)

# remove unnecessary datasets
rm(KDTR_Mossdale, KDTR_Sherwood, MWTR_Chipps, MWTR_Sherwood)



# Subset and clean up Seine Effort##############################################################-----

SEIN_Effort <- subset(Effort, MethodCode %in% c("SEIN"))

# Format Dataset, filter for outliers based on SEIN length, width, depth. Remove Liberty Island Seines
SEIN_Effort <- SEIN_Effort %>% 
  filter(SeineLength <= 15|NA & SeineLength >= 3|NA & SeineWidth <= 15|NA & SeineWidth >= 3|NA & SeineDepth <= 1.5|NA) %>%
  filter(Volume!="NA", RegionCode!="NA", 
         !StationCode %in% c("LI001E", "LI001W","LI002E", "LI002W","LI003E","LI003W",
                             "LI004E","LI004W","LI005E","LI005W","LI006E","LI006W",
                             "LI007E","LI007W","LI008E","LI008W","LI009E","LI009W",
                             "LI010E","LI010W", "LI011E","LI011W")) #Remove Liberty Island Seines

SEINEffortFieldYear <- SEIN_Effort %>% ##used for plotting unknown fish origins before 2007
  select(SampleID, FieldYear)


# Format CWT Data---------------------------------------------------------------
#Pull in CWT tag codes

CWT <- read_xlsx(file.path("Data",fieldyear,"All_CA_CWT.xlsx"), sheet=1, guess_max=5000)

## Format CWT Dataset
CWT <- CWT %>%
  map_if(is.numeric, ~ifelse(is.na(.x),0,.x)) %>%
  as.data.frame()

## Do a quick check to make sure these fields are formatted as text, otherwise the 
## leading zeros are lost:
if(!is.character(CWT$tag_code_or_release_id) || 
   !is.character(CWT$cwt_1st_mark) || 
   !is.character(CWT$cwt_2nd_mark) || 
   !is.character(CWT$non_cwt_1st_mark) || 
   !is.character(CWT$non_cwt_2nd_mark)) {
  cat("The CWT file needs reformatting.\n")
  rm(list=ls())
}

CWT$cwt_1st_mark_count <- as.numeric(CWT$cwt_1st_mark_count)
CWT$cwt_2nd_mark_count <- as.numeric(CWT$cwt_2nd_mark_count)
CWT$non_cwt_2nd_mark_count <- as.numeric(CWT$non_cwt_2nd_mark_count)
CWT$non_cwt_1st_mark_count <- as.numeric(CWT$non_cwt_1st_mark_count)


## Quickly look at the values for any obvious issues:
head(unique(CWT$tag_code_or_release_id))
unique(CWT$cwt_1st_mark)
unique(CWT$cwt_2nd_mark)
unique(CWT$non_cwt_1st_mark)
unique(CWT$non_cwt_2nd_mark)

CWT <- CWT %>%
  group_by(tag_code_or_release_id) %>%
  mutate(MarkedCount=(if_else(cwt_1st_mark %in% c("0000","0009","9000","9009",NA), 0, cwt_1st_mark_count))+
           (ifelse(cwt_2nd_mark %in% c("0000","0009","9000","9009",NA) , 0, cwt_2nd_mark_count))+
           (ifelse(non_cwt_2nd_mark %in% c("0000","0009","9000","9009",NA), 0, non_cwt_2nd_mark_count))+
           (ifelse(non_cwt_1st_mark %in% c("0000","0009","9000","9009",NA) , 0, non_cwt_1st_mark_count))) %>%
  mutate(UnmarkedCount=(ifelse(cwt_1st_mark %in% c("0000",NA), cwt_1st_mark_count, 0))+
           (ifelse(cwt_2nd_mark %in% c("0000",NA),  cwt_2nd_mark_count, 0))+
           (ifelse(non_cwt_2nd_mark %in% c("0000",NA), non_cwt_2nd_mark_count, 0))+
           (ifelse(non_cwt_1st_mark %in% c("0000",NA), non_cwt_1st_mark_count, 0))) %>%
  mutate(Unknown=(ifelse(cwt_1st_mark %in% c("0009","9000","9009",NA), cwt_1st_mark_count, 0))+
           (ifelse(cwt_2nd_mark %in% c("0009","9000","9009",NA) , cwt_2nd_mark_count, 0))+
           (ifelse(non_cwt_2nd_mark %in% c("0009","9000","9009",NA) , non_cwt_2nd_mark_count, 0))+
           (ifelse(non_cwt_1st_mark %in% c("0009","9000","9009",NA) , non_cwt_1st_mark_count, 0))) %>%
  mutate(Totalcount=cwt_1st_mark_count+cwt_2nd_mark_count+non_cwt_1st_mark_count+non_cwt_2nd_mark_count) %>%
  mutate(RatioUnmarkedvsMarked=(UnmarkedCount/MarkedCount)) %>%
  mutate(RaceByCWT=case_when(run %in% '1'~"Spring", run %in% '2'~ "Summer", run %in% '3' ~ "Fall", run %in% '5'~ "Hybrid",
                              run %in% '6'~ "Landlocked", run %in% '7'~ "LateFall",run %in% '4'~"Winter", run %in% '8'~"LateFall")) %>%
  select(tag_code_or_release_id, RaceByCWT, first_release_date, Totalcount, MarkedCount,
         UnmarkedCount, Unknown, RatioUnmarkedvsMarked) %>%
  dplyr::rename(TagCode=tag_code_or_release_id)



# Format Catch Data-------------------------------------------------------------
  # RaceByTag (dbo_CodeWireTag)---field used before 2014
  # RaceByTag (dbo_Catch)---field used after 2014

SalmonidCatch <- subset(Catch, OrganismCode %in% c("CHN","RBT"))

SalmonidCatch <- SalmonidCatch %>% filter(SampleID %in% Effort$SampleID)

# Split Catch up into pre and post 2014 CWT and non-CWT fish
SalmonidCatchAfter2014 <- SalmonidCatch %>%
  filter(SampleID %in% AfterFY2014$SampleID) %>%
  select(SampleID, OrganismCode, MarkCode, StageCode, TagCode, RaceByTag, RaceByLength,
         ForkLength, Count)

CWTCatchBefore2014 <- SalmonidCatch %>%
  filter(SampleID %in% BeforeFY2014$SampleID) %>%
  filter(!MarkCode %in% c("none","None") & OrganismCode %in% c("CHN","RBT")) %>%
  select(SampleID, OrganismCode, MarkCode, StageCode, TagCode, RaceByTag, RaceByLength,
         ForkLength, Count)

NonCWTCatchBefore2014 <- SalmonidCatch %>%
  filter(SampleID %in% BeforeFY2014$SampleID) %>%
  filter(MarkCode %in% c("none","None") & OrganismCode %in% c("CHN","RBT")) %>%
  select(SampleID, OrganismCode, MarkCode, StageCode, TagCode, RaceByTag, RaceByLength,
         ForkLength, Count)

CWT_codes <- CWT_codes %>%
  filter(SampleID %in% BeforeFY2014$SampleID) %>%
  mutate(Count=CWTCount) %>%
  select(SampleID, OrganismCode, MarkCode, StageCode, TagCode, RaceByTag, RaceByLength,
         ForkLength, Count)

#QC Step. Numbers should be within 50. The mismatch is caused by more fish being present in the old CWT code table then what was recorded on the datasheet. 
as.numeric(sum(CWTCatchBefore2014$Count))
as.numeric(sum(CWT_codes$Count))

#2023: sum of CWT catch before 2014 is 55434, should not change drastically over years 
#Recombine catch using the CWT Code table in place of the Catch Table for CWT fish before 2014

SalmonidCatch2 <- bind_rows(CWT_codes, NonCWTCatchBefore2014, SalmonidCatchAfter2014)


#QC Step. Numbers should be within 50. The mismatch is caused by more fish being present in the old cWT code table then what was recorded on the datasheet. 
---------------------------------- 
as.numeric(sum(SalmonidCatch$Count))
as.numeric(sum(SalmonidCatch2$Count))

# Continued Formatting----------------make sure to check that mark code list makes sense i.e new mark codes made 
summary(as.factor(SalmonidCatch2$MarkCode))
summary(as.factor(SalmonidCatch2$RaceByLength))
summary(as.factor(SalmonidCatch2$RaceByTag))
summary(as.factor(SalmonidCatch2$TagCode))

# Should be FALSE:
any(!is.na(SalmonidCatch2$RaceByTag) & SalmonidCatch2$RaceByTag == "Adult")

## Note: using the character "NA" can be confusing with actual NA's. 
## Switching from "NA" to "NA_string" to avoid ambiguity. 
SalmonidCatch2 <- SalmonidCatch2 %>% 
  filter(MarkCode %in% c("Adclipped","AdClipped","none","None","Plvc","PlvcLft + Ad")) %>%  # remove trawl efficiency fish that have VIE markings and Acoustic tags
  filter(!TagCode %in% c("YeSd(86)","YeSd(77)","YeSd(76)2","YeSd(76)1","ReSd(86)",
                         "ReSd(77)2","ReSd(77)1","ReSd(76)2","ReSd(76)2",
                         "ReSd(76)1")) %>% # remove the rest of efficiency fish
  mutate(MarkCode=ifelse(MarkCode %in% c("none", "None"), "None", "Marked")) %>% #Make Mark Code consistent
  mutate(TagCode=ifelse(TagCode %in% c(" ","n/a","LT","NT","NR","None","none","NoTag",
                                       "DT","LF") | is.na(TagCode), NA, TagCode)) %>%# make tagcode NAs consistent
  mutate(RaceByTag=case_when(RaceByTag %in% c("fall","Fall")~"Fall",  ## aggregates columns into one
                             RaceByTag %in% c("Hybrid")~"Hybrid", 
                             RaceByTag %in% c("LateFall")~"LateFall", 
                             RaceByTag %in% c("spring","Spring")~"Spring", 
                             RaceByTag %in% c("winter","Winter")~"Winter",
                             RaceByTag %in% c("Not Raced", "n/p", "", NA)~"NA_string")) %>%
  mutate(RaceByLength=as.factor(case_when(RaceByLength %in% c("Fall")~"Fall", ## replaces n/p and not raced fish with NAs
                                          RaceByLength %in% c("LateFall")~"LateFall",
                                          RaceByLength %in% c("Adult")~"Adult",
                                          RaceByLength %in% c("Spring")~"Spring",
                                          RaceByLength %in% c("Winter")~"Winter",
                                          RaceByLength %in% c("Not Raced", "n/p", "", NA)~"NA_string"))) %>%
  filter(!RaceByLength %in% c("Adult")) ##Filters out adults

unique(SalmonidCatch2$RaceByTag)

summary(as.factor(SalmonidCatch2$MarkCode))
summary(as.factor(SalmonidCatch2$RaceByLength))
summary(as.factor(SalmonidCatch2$RaceByTag))

SalmonidCatch2Sum <- SalmonidCatch2 %>%
  select(SampleID, OrganismCode, MarkCode, StageCode, TagCode, RaceByTag, RaceByLength,
         Count) %>%
  group_by(SampleID, OrganismCode, MarkCode, StageCode, TagCode, RaceByTag, 
           RaceByLength) %>%
  transmute(SumOfCatchCount=sum(Count)) %>%
  unique() %>%
  as.data.frame()

SalmonidCatch2Sum$SumOfCatchCount <- as.numeric(as.character(SalmonidCatch2Sum$SumOfCatchCount))####Have to properly convert counts from factor to numeric variable
SalmonidCatch2Sum$SampleID <- as.factor(SalmonidCatch2Sum$SampleID)

## Make a dataset for Forklength Data
SalmonidFL <- SalmonidCatch2 %>%
  select(SampleID, OrganismCode, MarkCode, StageCode, TagCode, RaceByTag, RaceByLength, 
         ForkLength, Count) %>%
  group_by(SampleID, OrganismCode, MarkCode, StageCode, TagCode, RaceByTag, RaceByLength, 
           ForkLength) %>%
  dplyr::summarize(SumOfCatchCount=sum(Count),
                   .groups="drop") %>%
  arrange(SampleID, OrganismCode, MarkCode, StageCode, TagCode, RaceByTag, RaceByLength, 
          ForkLength) %>%
  as.data.frame()

SalmonidFL$SumOfCatchCount <- as.numeric(as.character(SalmonidFL$SumOfCatchCount))####Have to properly convert counts from factor to numeric variable
SalmonidFL$SampleID <- as.factor(SalmonidFL$SampleID)


#Subset based on Species
CHN_Catch <- subset(SalmonidCatch2Sum, OrganismCode == "CHN")
RBT_Catch <- subset(SalmonidCatch2Sum, OrganismCode == "RBT")

CHN_FL <- subset(SalmonidFL, OrganismCode == "CHN")
RBT_FL <- subset(SalmonidFL, OrganismCode == "RBT")

# remove unused data frames
rm(Catch, CWT_codes, SalmonidCatch2, SalmonidCatch2Sum, SalmonidFL, Samples, Stations,
   Organisms, AfterFY2014, BeforeFY2014)



## Note that there are duplicated tag codes, indicated with asterisks in the CWT file.
## These fish all appear to have RaceByTag values anyhow.
grep("\\*", CWT$TagCode, value=TRUE)

examine_duplicated_tagcodes <- subset(CWT, grepl("\\*", TagCode))
examine_duplicated_tagcodes$a <- sub("(.*)(\\*.*)", "\\1", 
                                     examine_duplicated_tagcodes$TagCode)
tmp <- subset(CHN_Catch, TagCode %in% examine_duplicated_tagcodes$a)
sum(tmp$SumOfCatchCount)
unique(tmp$RaceByTag)   # Spring



####################CHN Data Manipulation#################################
####CHN Trawls#####
# Step 1. Combine Chinook Catch and CWT Data sets, Assign Race to fish-----------------------------------------------------------

## Add TagCode information (race and ratio of unmarked to marked) to Catch Dataset
CHN_Trawl_Catch <- left_join(CHN_Catch, CWT, by="TagCode")

## Add Sample Year to Catches so we can designate unknown origin Chinook in samples taken before 2008 field year
CHN_Trawl_Catch <- inner_join(TrawlEffortFieldyear, CHN_Trawl_Catch, by="SampleID")


## QC Data set and make corrections
CHN_Trawl_Catch <- CHN_Trawl_Catch %>% 
  mutate(RaceByTag=ifelse(is.na(RaceByCWT), RaceByTag, RaceByCWT)) %>%
  mutate(Race=case_when(RaceByTag=="NA_string" & MarkCode=="Marked" ~ "Unknown", 
                        RaceByTag=="NA_string" & MarkCode == "None" ~ as.character(RaceByLength),  
                        !RaceByTag=="NA_string" ~ as.character(RaceByTag))) %>%
  select(SampleID, FieldYear, OrganismCode, TagCode, RatioUnmarkedvsMarked, Race, 
         MarkCode, SumOfCatchCount)
CHN_Trawl_Catch$RatioUnmarkedvsMarked[is.na(CHN_Trawl_Catch$RatioUnmarkedvsMarked)] <- 0

## Calculate and add columns for Marked and Unmarked Chinook counts for specific tag codes within samples 
CHN_Trawl_Catch <- CHN_Trawl_Catch %>%
  mutate(Marked_Count=case_when(MarkCode %in% c("Marked") ~ SumOfCatchCount)) %>%  #create count of marked individuals
  group_by(SampleID, FieldYear, OrganismCode, TagCode, RatioUnmarkedvsMarked) %>% 
  mutate(Marked_Count_per_tagcode=case_when(MarkCode %in% c("Marked")&!is.na(TagCode)~sum(Marked_Count))) %>% #sum counts of marked individuals with specific tag codes within samples
  ungroup() %>%
  mutate(Unmarked_Count_per_tagcode=RatioUnmarkedvsMarked * Marked_Count_per_tagcode) #sum counts of unmarked individuals based on marking ratio and #of marked individuals for specific tag codes
  
CHN_Trawl_Catch$Marked_Count[is.na(CHN_Trawl_Catch$Marked_Count)] <- 0
CHN_Trawl_Catch$Marked_Count_per_tagcode[is.na(CHN_Trawl_Catch$Marked_Count_per_tagcode)] <- 0
CHN_Trawl_Catch$Unmarked_Count_per_tagcode[is.na(CHN_Trawl_Catch$Unmarked_Count_per_tagcode)] <- 0

CHN_taggers_catch <- inner_join(subset(Trawl_effort, FieldYear == fieldyear), 
                                CHN_Trawl_Catch, by="SampleID")


# Step 2. Subset Dataset by Races of interest------------------------------------------------------------------------------------

## Separate out Winter Run and Non-winter Run (grouped by Spring vs. all remaining 
## races combined):
CHNW_Trawl_Catch <- subset(CHN_Trawl_Catch, Race == "Winter") %>%
  dplyr::mutate(CustomRaceGroup=Race)

CHNnotW_Trawl_Catch <- subset(CHN_Trawl_Catch, !(Race %in% c("Winter"))) %>%
  dplyr::mutate(CustomRaceGroup=ifelse(Race == "Spring", "Spring", "Not_SpringWinter"))


# Step 3. Calculate and summarize Chinook catches for marked, unmarked, wild, and unknown individuals per sample for each race -------------------------------

## Winter Run
CHNW_Trawl_Catch_Summary <- CHNW_Trawl_Catch %>%
  dplyr::group_by(SampleID, OrganismCode, CustomRaceGroup) %>%
  dplyr::summarize(Total_CHN=sum(SumOfCatchCount),
									 Marked_Total=sum(Marked_Count),
									 Unmarked_Total_TMP=round(sum(Unmarked_Count_per_tagcode)),
									 .groups="drop") %>%
	dplyr::mutate(Unmarked_Total=ifelse((Total_CHN-Marked_Total) < Unmarked_Total_TMP, 
																			(Total_CHN-Marked_Total),
																			Unmarked_Total_TMP),
								Wild=ifelse((Total_CHN-(Marked_Total+Unmarked_Total)) > 0, 
														round(Total_CHN-(Marked_Total+Unmarked_Total)),
														0)) %>%
	dplyr::select(-Unmarked_Total_TMP) %>%
  tidyr::pivot_longer(cols=c(Marked_Total, Unmarked_Total, Wild), 
											names_to="Origin", 
											values_to="Count")

### Data QC: You can check to make sure that all fish are accounted for by making sure the counts below match
if(sum(CHNW_Trawl_Catch_Summary$Total_CHN)/3 != sum(CHNW_Trawl_Catch_Summary$Count)) {
	stop("There is a problem with CHNW_Trawl_Catch_Summary.\n")
}


## Non-winter Run
CHNnotW_Trawl_Catch_Summary <- CHNnotW_Trawl_Catch %>%
  dplyr::group_by(SampleID, FieldYear, OrganismCode, CustomRaceGroup) %>%
  dplyr::summarize(Total_CHN=sum(SumOfCatchCount),
									 Marked_Total=sum(Marked_Count),
									 Unmarked_Total_TMP=round(sum(Unmarked_Count_per_tagcode)),
									 .groups="drop") %>%
  mutate(Unknown=ifelse(FieldYear <= 2007, (Total_CHN-Marked_Total), 0), 
				 Unmarked_Total=ifelse((Total_CHN-Marked_Total) < Unmarked_Total_TMP,
															 (Total_CHN-Marked_Total),
																Unmarked_Total_TMP),
				 Unmarked_Total=ifelse(FieldYear >= 2008, Unmarked_Total, 0),
				 Wild=ifelse((Total_CHN-(Marked_Total+Unmarked_Total)) > 0,
										 round(Total_CHN-(Marked_Total+Unmarked_Total)),
										 0),
				 Wild=ifelse(FieldYear >= 2008, Wild, 0)) %>%
	dplyr::select(-c(Unmarked_Total_TMP, FieldYear)) %>%
		tidyr::pivot_longer(cols=c(Marked_Total, Unknown, Unmarked_Total, Wild), 
												names_to="Origin", 
												values_to="Count")
if(sum(CHNnotW_Trawl_Catch_Summary$Total_CHN)/4 != sum(CHNnotW_Trawl_Catch_Summary$Count)) {
	stop("There is a problem with CHNnotW_Trawl_Catch_Summary.\n")
}


CHNcustom_Trawl_Catch_Summary <- dplyr::bind_rows(CHNW_Trawl_Catch_Summary,
                                                  CHNnotW_Trawl_Catch_Summary)


# Step 4. Combine catch and effort datasets, format, add zero catches, calculate CPUE -----------------------------------------------------------------------------------------------------

CHNcustom_Trawl_CPUE <- left_join(Trawl_effort, CHNcustom_Trawl_Catch_Summary, 
                                  by="SampleID") %>%
  select(SampleID, FieldYear, MONTH, DAY, description, StationCode, Volume, MethodCode, 
         OrganismCode, CustomRaceGroup, Origin, Count) %>%
  filter(!is.na(Volume))

## Format Dataset
## Placeholder values of "CHN" and "None" for samples with no CHN:
CHNcustom_Trawl_CPUE$OrganismCode[is.na(CHNcustom_Trawl_CPUE$OrganismCode)] <- "CHN"
CHNcustom_Trawl_CPUE$CustomRaceGroup[is.na(CHNcustom_Trawl_CPUE$CustomRaceGroup)] <- "None"
CHNcustom_Trawl_CPUE$Origin[is.na(CHNcustom_Trawl_CPUE$Origin)] <- "Wild"
CHNcustom_Trawl_CPUE$Count[is.na(CHNcustom_Trawl_CPUE$Count)] <- 0 
CHNcustom_Trawl_CPUE$Origin <- as.factor(CHNcustom_Trawl_CPUE$Origin)
CHNcustom_Trawl_CPUE$SampleID <- as.factor(CHNcustom_Trawl_CPUE$SampleID)
CHNcustom_Trawl_CPUE$FieldYear <- as.factor(CHNcustom_Trawl_CPUE$FieldYear)

## Add zeroes for samples with no catch
CHNcustom_Trawl_CPUE <- CHNcustom_Trawl_CPUE %>% 
  complete(nesting(SampleID, FieldYear, MONTH, DAY, description, StationCode, Volume, 
                   MethodCode), OrganismCode, CustomRaceGroup, Origin, 
           fill=list(Count=0)) %>%
  mutate(CPUE=Count/Volume) %>%
  unique()


# Step 5. Calculate Monthly Means for Regions for Current sampling year -------------------------------------------------------                           

## With kodiak and mid water combined
CHNcustom_Trawl_Monthly_Means <- CHNcustom_Trawl_CPUE %>%
  group_by(FieldYear, MONTH, DAY, description, StationCode, OrganismCode, 
           CustomRaceGroup, Origin) %>% ##mean daily CPUE by Station
  summarise(CPUE_day_station=mean(CPUE, na.rm=TRUE),
            .groups="drop") %>%
  group_by(FieldYear, MONTH, DAY, description, OrganismCode, CustomRaceGroup, Origin) %>% ##mean daily CPUE by site
  summarise(CPUE_day_site=mean(CPUE_day_station, na.rm=TRUE),
            .groups="drop") %>%
  group_by(FieldYear, MONTH, description, OrganismCode, CustomRaceGroup, Origin) %>%  ##mean monthly CPUE by site
  summarise(AvgCPUE=mean(CPUE_day_site, na.rm=TRUE),
            .groups="drop") %>%
  filter(!is.nan(AvgCPUE)) %>%
  dplyr::filter(CustomRaceGroup != "None") # drop placeholder for samples with no CHN

## Monthly Means for Current Year
CHNcustom_Trawl_Monthly_Means_CurrentYear <- CHNcustom_Trawl_Monthly_Means %>%
  filter(FieldYear == fieldyear)


# Step 6. Calculate Annual Means of CPUE for Regions ------------------------------------------------------------------                           

CHNcustom_Trawl_Annual_Means <- CHNcustom_Trawl_Monthly_Means %>%
  group_by(FieldYear, description, OrganismCode, CustomRaceGroup, Origin) %>%
  summarise(AvgCPUE=mean(AvgCPUE, na.rm=TRUE),
            .groups="drop")



####CHN SEINES#####
# Step 1. Combine Chinook Catch and CWT Datasets, Assign Race to fish-----------------------------------------------------------

## Add TagCode information (race and ratio of unmarked to marked) to Catch Dataset
CHN_SEIN_Catch <- left_join(CHN_Catch, CWT, by="TagCode")

## Add Sample Year to Catches so we can designate unknown origin Chinook in samples taken before 2008 field year
CHN_SEIN_Catch <- inner_join(CHN_SEIN_Catch, SEINEffortFieldYear, by="SampleID")

## QC Datset and make corrections
CHN_SEIN_Catch <- CHN_SEIN_Catch %>% 
  mutate(MarkCode=ifelse(MarkCode %in% c("none", "None")|is.na(MarkCode),"None", "Marked")) %>%
  mutate(RaceByTag=ifelse(is.na(RaceByCWT), RaceByTag, RaceByCWT)) %>%
  mutate(Race=case_when(RaceByTag=="NA_string" & MarkCode=="Marked"~"Unknown", 
                        RaceByTag=="NA_string" & MarkCode == "None" ~as.character(RaceByLength),  
                        !RaceByTag=="NA_string"~as.character(RaceByTag))) %>%
  select(SampleID, FieldYear, OrganismCode, TagCode, RatioUnmarkedvsMarked, Race, 
         MarkCode, SumOfCatchCount)
CHN_SEIN_Catch$RatioUnmarkedvsMarked[is.na(CHN_SEIN_Catch$RatioUnmarkedvsMarked)] <- 0

## Calculate and add columns for Marked and Unmarked Chinook counts for specific tag codes within samples 
CHN_SEIN_Catch <- CHN_SEIN_Catch %>%
  mutate(Marked_Count= case_when(MarkCode %in% c("Marked")~SumOfCatchCount)) %>%  #create count of marked individuals
  group_by(SampleID, FieldYear, OrganismCode, TagCode, RatioUnmarkedvsMarked) %>% 
  mutate(Marked_Count_per_tagcode=case_when(MarkCode %in% c("Marked")&!is.na(TagCode)~sum(Marked_Count))) %>% #sum counts of marked individuals with specific tagcodes within samples
  ungroup() %>%
  mutate(Unmarked_Count_per_tagcode = RatioUnmarkedvsMarked * Marked_Count_per_tagcode) #sum counts of unmarked individuals based on marking ratio and #of marked individuals for specific tag codes

CHN_SEIN_Catch$Marked_Count[is.na(CHN_SEIN_Catch$Marked_Count)] <- 0
CHN_SEIN_Catch$Marked_Count_per_tagcode[is.na(CHN_SEIN_Catch$Marked_Count_per_tagcode)] <- 0
CHN_SEIN_Catch$Unmarked_Count_per_tagcode[is.na(CHN_SEIN_Catch$Unmarked_Count_per_tagcode)] <- 0


# Step 2. Subset Dataset by Races of interest------------------------------------------------------------------------------------

## Separate out Winter Run and Non-winter Run (grouped by Spring vs. all remaining 
## races combined):
CHNW_SEIN_Catch <- subset(CHN_SEIN_Catch, Race == "Winter") %>%
  dplyr::mutate(CustomRaceGroup=Race)

CHNnotW_SEIN_Catch <- subset(CHN_SEIN_Catch, !(Race %in% c("Winter"))) %>%
  dplyr::mutate(CustomRaceGroup=ifelse(Race == "Spring", "Spring", "Not_SpringWinter"))


# Step 3. Calculate and summarize chinook catches for marked, unmarked, wild, and unknown individuals per sample for each race -------------------------------

## Winter Run
CHNW_SEIN_Catch_Summary <- CHNW_SEIN_Catch %>%
  dplyr::group_by(SampleID, OrganismCode, CustomRaceGroup) %>%
  dplyr::summarize(Total_CHN=sum(SumOfCatchCount), 
									 Marked_Total=sum(Marked_Count),
									 Unmarked_Total_TMP=round(sum(Unmarked_Count_per_tagcode)),
									 .groups="drop") %>%
  dplyr::mutate(Unmarked_Total=ifelse((Total_CHN-Marked_Total) < Unmarked_Total_TMP,
																			(Total_CHN-Marked_Total), 
																			Unmarked_Total_TMP),
								Wild=ifelse((Total_CHN-(Marked_Total+Unmarked_Total)) > 0, 
														round(Total_CHN-(Marked_Total+Unmarked_Total)),
														0)) %>%
	dplyr::select(-Unmarked_Total_TMP) %>%
	tidyr::pivot_longer(cols=c(Marked_Total, Unmarked_Total, Wild), 
											names_to="Origin", 
											values_to="Count")
if(sum(CHNW_SEIN_Catch_Summary$Total_CHN)/3 != sum(CHNW_SEIN_Catch_Summary$Count)) {
	stop("There is a problem with CHNW_SEIN_Catch_Summary.\n")
}


## Non-winter Run
CHNnotW_SEIN_Catch_Summary <- CHNnotW_SEIN_Catch %>%
  dplyr::group_by(SampleID, FieldYear, OrganismCode, CustomRaceGroup) %>%
  dplyr::summarize(Total_CHN=sum(SumOfCatchCount), 
									 Marked_Total=sum(Marked_Count), 
									 Unmarked_Total_TMP=round(sum(Unmarked_Count_per_tagcode)),
									 .groups="drop") %>%
  dplyr::mutate(Unknown=ifelse(FieldYear <= 2007, (Total_CHN-Marked_Total), 0), 
								Unmarked_Total=ifelse((Total_CHN-Marked_Total) < Unmarked_Total_TMP, 
																			(Total_CHN-Marked_Total),
																			Unmarked_Total_TMP), 
								Unmarked_Total=ifelse(FieldYear >= 2008, Unmarked_Total, 0),
								Wild=ifelse((Total_CHN-(Marked_Total+Unmarked_Total)) > 0, 
														round(Total_CHN-(Marked_Total+Unmarked_Total)),
														0), 
								Wild=ifelse(FieldYear >= 2008, Wild, 0)) %>%
	dplyr::select(-c(Unmarked_Total_TMP, FieldYear)) %>%
		tidyr::pivot_longer(cols=c(Marked_Total, Unknown, Unmarked_Total, Wild), 
												names_to="Origin", 
												values_to="Count")
if(sum(CHNnotW_SEIN_Catch_Summary$Total_CHN)/4 != sum(CHNnotW_SEIN_Catch_Summary$Count)) {
	stop("There is a problem with CHNnotW_SEIN_Catch_Summary.\n")
}


CHNcustom_SEIN_Catch_Summary <- dplyr::bind_rows(CHNW_SEIN_Catch_Summary,
                                                 CHNnotW_SEIN_Catch_Summary)


# Step 4. Combine catch and effort datasets, format, add zero catches, calculate CPUE -----------------------------------------------------------------------------------------------------

CHNcustom_SEIN_CPUE <- left_join(SEIN_Effort, CHNcustom_SEIN_Catch_Summary, 
                               by="SampleID") %>%
  select(SampleID, FieldYear, MONTH, DAY, description, RegionCode, StationCode, Volume, 
         MethodCode, OrganismCode, CustomRaceGroup, Origin, Count) %>%
  filter(!is.na(Volume))

## Format Dataset
## Placeholder values of "CHN" and "None" for samples with no CHN:
CHNcustom_SEIN_CPUE$OrganismCode[is.na(CHNcustom_SEIN_CPUE$OrganismCode)] <- "CHN"
CHNcustom_SEIN_CPUE$CustomRaceGroup[is.na(CHNcustom_SEIN_CPUE$CustomRaceGroup)] <- "None"
CHNcustom_SEIN_CPUE$Origin[is.na(CHNcustom_SEIN_CPUE$Origin)] <- "Wild"
CHNcustom_SEIN_CPUE$Count[is.na(CHNcustom_SEIN_CPUE$Count)] <- 0 
CHNcustom_SEIN_CPUE$Origin <- as.factor(CHNcustom_SEIN_CPUE$Origin)
CHNcustom_SEIN_CPUE$SampleID <- as.factor(CHNcustom_SEIN_CPUE$SampleID)
CHNcustom_SEIN_CPUE$FieldYear <- as.factor(CHNcustom_SEIN_CPUE$FieldYear)

## Add zeroes for samples with no catch
CHNcustom_SEIN_CPUE <- CHNcustom_SEIN_CPUE %>% 
  complete(nesting(SampleID, FieldYear, MONTH, DAY, description, RegionCode,
                   StationCode, Volume, MethodCode), 
           OrganismCode, CustomRaceGroup, Origin, fill=list(Count=0)) %>%
  mutate(CPUE=Count/Volume) %>%
  unique()


# Step 5. Calculate Monthly Means for Regions for Current sampling year-------------------------------------------------------                           

### Monthly CPUE averages per region were calculated by averaging all samples taken at each station per month. Monthly station averages within each region
### were then averaged together. We found a strong correlation between the average CPUE values calculated this way versus
### the previous way that started by averaging by station per week, then region per week, then region per month. Our new simplified calculations
### streamlined the process and removed the need to create an All date table in Access 

##  Model output comparing average CPUE values from simplified averaging and past averaging calculations: 
## lm(formula = CHNW_Catch$CPUE_month_region.x ~ CHNW_Catch$CPUE_month_region.y)

##Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.079620 -0.000027 -0.000027 -0.000027  0.065487 

#Coefficients:
#                                Estimate Std. Error t value            Pr(>|t|)    
#(Intercept)                    0.00002719 0.00004591   0.592               0.554    
#CHNW_Catch$CPUE_month_region.y 1.06525543 0.00499180 213.401 <0.0000000000000002 ***
#  ---

# Residual standard error: 0.002673 on 3412 degrees of freedom
# Multiple R-squared:  0.9303,	Adjusted R-squared:  0.9303 
# F-statistic: 4.554e+04 on 1 and 3412 DF,  p-value: < 0.00000000000000022


CHNcustom_SEIN_Monthly_Means <- CHNcustom_SEIN_CPUE %>%
  group_by(FieldYear, MONTH, RegionCode, StationCode, OrganismCode, CustomRaceGroup, 
           Origin) %>% ##mean monthly CPUE by Station
  summarise(CPUE_month_station=mean(CPUE, na.rm=TRUE),
            .groups="drop") %>%
  group_by(FieldYear, MONTH, RegionCode, OrganismCode, CustomRaceGroup, Origin) %>%  ##mean monthly CPUE by Region
  summarise(AvgCPUE=mean(CPUE_month_station, na.rm=TRUE),
            .groups="drop") %>%
  filter(!is.nan(AvgCPUE)) %>%
  dplyr::filter(CustomRaceGroup != "None") # drop placeholder for samples with no CHN

## Monthly Means for Current Year
CHNcustom_SEIN_Monthly_Means_CurrentYear <- CHNcustom_SEIN_Monthly_Means %>%
  filter(FieldYear == fieldyear)


# Step 6. Calculate Annual Means of CPUE for Regions ------------------------------------------------------------------                           

CHNcustom_SEIN_Annual_Means <- CHNcustom_SEIN_Monthly_Means %>%
  group_by(FieldYear, RegionCode, OrganismCode, CustomRaceGroup, Origin) %>%
  summarise(AvgCPUE=mean(AvgCPUE, na.rm=TRUE),
            .groups="drop")



####Annual CHN FL Distribution at Chipps#######################
CHN_Chipps_FLraw <- inner_join(subset(Trawl_effort, description %in% c("Chipps Island")), 
                               CHN_FL, by="SampleID")

CHN_Chipps_FLraw <- CHN_Chipps_FLraw %>% 
  mutate(RaceByLength=as.factor(case_when(RaceByLength %in% c("Fall")~"Fall", ## replaces n/p and not raced fish with NAs
                                          RaceByLength %in% c("LateFall")~"LateFall",
                                          RaceByLength %in% c("Adult")~"Adult",
                                          RaceByLength %in% c("Spring")~"Spring",
                                          RaceByLength %in% c("Winter")~"Winter"))) %>%
  select(SampleID, YEAR, MONTH, DAY, OrganismCode, RaceByTag, RaceByLength, MarkCode,
         TagCode, ForkLength, StageCode, SumOfCatchCount) %>%
  filter(RaceByLength %in% c("LateFall", "Fall", "Spring" , "Winter")|RaceByLength=="NA_string") ##Filters out adults

CHN_Chipps_FLraw$SumOfCatchCount <- as.numeric(as.character(CHN_Chipps_FLraw$SumOfCatchCount))####Have to properly convert counts from factor to numeric variable

CHN_Chipps_FLraw$MONTH <- factor(CHN_Chipps_FLraw$MONTH, 
                                 levels=c("Aug","Sep","Oct","Nov","Dec","Jan","Feb",
                                          "Mar","Apr","May","Jun","Jul"))
CHN_Chipps_FLraw$YEAR <- as.numeric(CHN_Chipps_FLraw$YEAR)
CHN_Chipps_FLraw$FieldYear <- ifelse (CHN_Chipps_FLraw$MONTH %in% c("Aug","Sep","Oct",
                                                                    "Nov","Dec"), 
                                      CHN_Chipps_FLraw$YEAR+1, 
                                      CHN_Chipps_FLraw$YEAR)
CHN_Chipps_FLraw$FieldYear <- as.factor(CHN_Chipps_FLraw$FieldYear) 
CHN_Chipps_FLraw$FieldYear <- factor(CHN_Chipps_FLraw$FieldYear, levels=FL_years)


## QC Datset and make corrections
CHN_Chipps_FL <- CHN_Chipps_FLraw %>% 
  mutate(MarkCode=ifelse(MarkCode %in% c("none", "None"),"None", "Marked")) %>%
  mutate(Race=case_when(RaceByTag=="NA_string" & MarkCode=="Marked"~"Unknown", 
                        RaceByTag=="NA_string" & MarkCode == "None" ~as.character(RaceByLength),  
                        !RaceByTag=="NA_string"~as.character(RaceByTag))) %>%
  select(SampleID, FieldYear, Race, MarkCode, ForkLength, StageCode, SumOfCatchCount) %>%
  filter(!Race %in% c("Unknown","Hybrid")) %>%
  filter(ForkLength>0) %>%
  filter(!is.na(FieldYear)) %>%
  uncount(weights=SumOfCatchCount, .remove=TRUE)%>% # This expands the dataset by removing the aggregated counts for each FL. So each fish is now in a seperate row. 
  add_count(FieldYear, MarkCode, name="N") # sample sizes of marked and unmarked chinook for each field year



####################RBT Data Manipulation#################################
####RBT Trawls#####
# Step 1. Combine Chinook Catch and CWT Datasets, Assign Race to fish-----------------------------------------------------------

## Add Sample Year to Catches so we can designate unknown origin Chinook in samples taken before 2008 field year
RBT_Trawl_Catch <- inner_join(RBT_Catch, TrawlEffortFieldyear, by="SampleID")

## QC Datset and make corrections
RBT_Trawl_Catch_Summary <- RBT_Trawl_Catch %>% 
  select(SampleID, FieldYear, OrganismCode, MarkCode, SumOfCatchCount) %>%
  mutate(MarkCode=ifelse(MarkCode %in% c("none", "None")|is.na(MarkCode),"None", "Marked")) %>%
  group_by(SampleID) %>%
  mutate(Total_RBT= sum(SumOfCatchCount)) %>%
  mutate(Marked_Count= sum(case_when(MarkCode %in% c("Marked")~SumOfCatchCount,
                                     MarkCode %in% c("None")~0))) %>%  #create count of marked individuals
  mutate(Wild= sum(case_when(MarkCode %in% c("None")~SumOfCatchCount,
                             MarkCode %in% c("Marked")~0))) %>%  #create count of marked individuals
  gather("Origin", "Count", 7:8) %>%
  select(SampleID, Total_RBT, OrganismCode, Origin, Count) %>%
  unique()

# Step 2. Combine catch and effort datasets, format, add zero catches, calculate CPUE -----------------------------------------------------------------------------------------------------
RBT_Trawl_CPUE <- left_join(Trawl_effort, RBT_Trawl_Catch_Summary, by="SampleID") %>%
  select(SampleID, FieldYear, MONTH, DAY, description, StationCode, Volume, 
         MethodCode, OrganismCode, Origin, Count) %>%
  filter(!is.na(Volume))

## Format Dataset
## Placeholder value of "RBT" for samples with no RBT:
RBT_Trawl_CPUE$OrganismCode[is.na(RBT_Trawl_CPUE$OrganismCode)] <- "RBT"
RBT_Trawl_CPUE$Origin[is.na(RBT_Trawl_CPUE$Origin)] <- "Wild"
RBT_Trawl_CPUE$Count[is.na(RBT_Trawl_CPUE$Count)] <- 0 
RBT_Trawl_CPUE$Origin <- as.factor(RBT_Trawl_CPUE$Origin)
RBT_Trawl_CPUE$SampleID <- as.factor(RBT_Trawl_CPUE$SampleID)
RBT_Trawl_CPUE$FieldYear <- as.factor(RBT_Trawl_CPUE$FieldYear)

## Add zeroes for samples with no catch
RBT_Trawl_CPUE <- RBT_Trawl_CPUE %>% 
  complete(nesting(SampleID, MONTH, DAY, FieldYear,description, StationCode, Volume, 
                   MethodCode), OrganismCode, Origin, fill=list(Count=0)) %>%
  mutate(CPUE=Count/Volume) %>%
  unique()


# Step 3. Calculate Monthly Means for Regions for Current sampling year-------------------------------------------------------                           

### Monthly CPUE averages per region were calculated by averaging all samples taken at each station per month. Monthly station averages within each region
### were then averaged together. We found a strong correlation between the average CPUE values calculated this way versus
### the previous way that started by averaging by station per week, then region per week, then region per month. Our new simplified calculations
### streamlined the process and removed the need to create an All date table in Access 

RBT_Trawl_Monthly_Means <- RBT_Trawl_CPUE %>%
  group_by(FieldYear, MONTH, DAY, description, StationCode, OrganismCode, Origin) %>% ##mean daily CPUE by Station
  summarise(CPUE_day_station=mean(CPUE, na.rm=TRUE),
            .groups="drop") %>%
  group_by(FieldYear, MONTH, DAY, description, OrganismCode, Origin) %>% ##mean daily CPUE by site
  summarise(CPUE_day_site=mean(CPUE_day_station, na.rm=TRUE),
            .groups="drop") %>%
  group_by(FieldYear, MONTH, description, OrganismCode, Origin) %>%  ##mean monthly CPUE by site
  summarise(AvgCPUE=mean(CPUE_day_site, na.rm=TRUE),
            .groups="drop") %>%
  filter(!is.nan(AvgCPUE))

## Monthly Means for Current Year
RBT_Trawl_Monthly_Means_CurrentYear <- RBT_Trawl_Monthly_Means %>%
  filter(FieldYear == fieldyear)


# Step 4. Calculate Annual Means of CPUE for Regions ------------------------------------------------------------------                           

RBT_Trawl_Annual_Means <- RBT_Trawl_Monthly_Means %>%
  group_by(FieldYear, description, OrganismCode, Origin) %>%
  summarise(AvgCPUE=mean(AvgCPUE, na.rm=TRUE),
            .groups="drop")



####RBT SEINES#####
# Step 1. Combine Chinook Catch and CWT Datasets, Assign Race to fish-----------------------------------------------------------

## Add Sample Year to Catches so we can designate unknown origin Chinook in samples taken before 2008 field year
RBT_SEIN_Catch <- inner_join(RBT_Catch, SEINEffortFieldYear, by="SampleID")

## QC Datset and make corrections
RBT_SEIN_Catch_Summary <- RBT_SEIN_Catch %>% 
  select(SampleID, FieldYear, OrganismCode, MarkCode, SumOfCatchCount) %>%
  mutate(MarkCode=ifelse(MarkCode %in% c("none", "None")|is.na(MarkCode), 
                         "None", "Marked")) %>%
  group_by(SampleID) %>%
  mutate(Total_RBT=sum(SumOfCatchCount)) %>%
  mutate(Marked_Count=sum(case_when(MarkCode %in% c("Marked")~SumOfCatchCount,
                                    MarkCode %in% c("None")~0))) %>%  #create count of marked individuals
  mutate(Wild=sum(case_when(MarkCode %in% c("None")~SumOfCatchCount,
                            MarkCode %in% c("Marked")~0))) %>%  #create count of marked individuals
  gather("Origin", "Count", 7:8) %>%
  select(SampleID, Total_RBT, OrganismCode, Origin, Count) %>%
  unique()


# Step 2. Combine catch and effort datasets, format, add zero catches, calculate CPUE -----------------------------------------------------------------------------------------------------
RBT_SEIN_CPUE <- left_join(SEIN_Effort, RBT_SEIN_Catch_Summary, by="SampleID") %>%
  select(SampleID, FieldYear, MONTH, DAY, description, RegionCode, StationCode, 
         Volume, MethodCode, OrganismCode, Origin, Count) %>%
  filter(!is.na(Volume))

## Format Dataset
## Placeholder value of "RBT" for samples with no RBT:
RBT_SEIN_CPUE$OrganismCode[is.na(RBT_SEIN_CPUE$OrganismCode)] <- "RBT"
RBT_SEIN_CPUE$Origin[is.na(RBT_SEIN_CPUE$Origin)] <- "Wild"
RBT_SEIN_CPUE$Count[is.na(RBT_SEIN_CPUE$Count)] <- 0 
RBT_SEIN_CPUE$Origin <- as.factor(RBT_SEIN_CPUE$Origin)
RBT_SEIN_CPUE$SampleID <- as.factor(RBT_SEIN_CPUE$SampleID)
RBT_SEIN_CPUE$FieldYear <- as.factor(RBT_SEIN_CPUE$FieldYear)

## Add zeroes for samples with no catch
RBT_SEIN_CPUE <- RBT_SEIN_CPUE %>% 
  complete(nesting(SampleID, MONTH, DAY, FieldYear,description, RegionCode, 
                   StationCode, Volume, MethodCode), 
           OrganismCode, Origin, fill=list(Count=0)) %>%
  mutate(CPUE=Count/Volume) %>%
  unique()


# Step 3. Calculate Monthly Means for Regions for Current sampling year-------------------------------------------------------                           

### Monthly CPUE averages per region were calculated by averaging all samples taken at each station per month. Monthly station averages within each region
### were then averaged together. We found a strong correlation between the average CPUE values calculated this way versus
### the previous way that started by averaging by station per week, then region per week, then region per month. Our new simplified calculations
### streamlined the process and removed the need to create an All date table in Access 

RBT_SEIN_Monthly_Means <- RBT_SEIN_CPUE %>%
  group_by(FieldYear, MONTH, RegionCode, StationCode, OrganismCode, Origin) %>% ##mean monthly CPUE by Station
  summarise(CPUE_month_station=mean(CPUE, na.rm=TRUE),
            .groups="drop") %>%
  group_by(FieldYear, MONTH, RegionCode, OrganismCode, Origin) %>%  ##mean monthly CPUE by Region
  summarise(AvgCPUE=mean(CPUE_month_station, na.rm=TRUE),
            .groups="drop") %>%
  filter(!is.nan(AvgCPUE))

## Monthly Means for Current Year
RBT_SEIN_Monthly_Means_CurrentYear <- RBT_SEIN_Monthly_Means %>%
  filter(FieldYear == fieldyear)


# Step 4. Calculate Annual Means of CPUE for Regions ------------------------------------------------------------------                           

RBT_SEIN_Annual_Means <- RBT_SEIN_Monthly_Means %>%
  group_by(FieldYear, RegionCode, OrganismCode, Origin) %>%
  summarise(AvgCPUE=mean(AvgCPUE, na.rm=TRUE),
            .groups="drop")



####Annual RBT FL Distribution at Chipps#######################
RBT_Chipps_FLraw <- inner_join(subset(Trawl_effort, description %in% c("Chipps Island")), 
                               RBT_FL, by="SampleID")

RBT_Chipps_FLraw <- RBT_Chipps_FLraw %>% 
  select(SampleID, YEAR, MONTH, DAY, OrganismCode, MarkCode, ForkLength, StageCode, 
         SumOfCatchCount) 

RBT_Chipps_FLraw$SumOfCatchCount <- as.numeric(as.character(RBT_Chipps_FLraw$SumOfCatchCount))####Have to properly convert counts from factor to numeric variable

RBT_Chipps_FLraw$MONTH <- factor(RBT_Chipps_FLraw$MONTH, 
                                 levels=c("Aug","Sep","Oct","Nov","Dec","Jan","Feb",
                                          "Mar","Apr","May","Jun","Jul"))
RBT_Chipps_FLraw$YEAR <- as.numeric(RBT_Chipps_FLraw$YEAR)
RBT_Chipps_FLraw$FieldYear <- ifelse(RBT_Chipps_FLraw$MONTH %in% c("Aug","Sep","Oct","Nov","Dec"), 
                                     RBT_Chipps_FLraw$YEAR+1, RBT_Chipps_FLraw$YEAR)
RBT_Chipps_FLraw$FieldYear <- as.factor(RBT_Chipps_FLraw$FieldYear) 
RBT_Chipps_FLraw$FieldYear <- factor(RBT_Chipps_FLraw$FieldYear, levels=FL_years)


## QC Datset and make corrections
RBT_Chipps_FL <- RBT_Chipps_FLraw %>% 
  mutate(MarkCode=ifelse(MarkCode %in% c("none", "None"),"None", "Marked")) %>%
  select(SampleID, FieldYear, MarkCode, ForkLength, StageCode, SumOfCatchCount) %>%
  filter(ForkLength > 0 & ForkLength < 350) %>%
  filter(!is.na(FieldYear)) %>%
  uncount(weights=SumOfCatchCount, .remove=TRUE) %>% # This expands the dataset by removing the aggregated counts for each FL. So each fish is now in a seperate row. 
  add_count(FieldYear, MarkCode, name="N") # sample sizes of marked and unmarked chinook for each field year



###################Write CSV###############################################

# Write CSV for final index output

this_out_root <- file.path("Data",fieldyear)

write.csv(CHNcustom_Trawl_Monthly_Means_CurrentYear, 
          file=file.path(this_out_root,"CHNcustom_Trawl_Monthly_Means_CurrentYear.csv"), 
          row.names=FALSE)
write.csv(CHNcustom_Trawl_Annual_Means, 
          file=file.path(this_out_root,"CHNcustom_Trawl_Annual_Means.csv"), 
          row.names=FALSE)

write.csv(CHNcustom_SEIN_Monthly_Means_CurrentYear, 
          file=file.path(this_out_root,"CHNcustom_SEIN_Monthly_Means_CurrentYear.csv"), 
          row.names=FALSE)
write.csv(CHNcustom_SEIN_Annual_Means, 
          file=file.path(this_out_root,"CHNcustom_SEIN_Annual_Means.csv"), 
          row.names=FALSE)

write.csv(RBT_Trawl_Monthly_Means_CurrentYear, 
          file=file.path(this_out_root,"RBT_Trawl_Monthly_Means_CurrentYear.csv"), 
          row.names=FALSE)
write.csv(RBT_Trawl_Annual_Means, 
          file=file.path(this_out_root,"RBT_Trawl_Annual_Means.csv"), row.names=FALSE)

write.csv(RBT_SEIN_Monthly_Means_CurrentYear, 
          file=file.path(this_out_root,"RBT_SEIN_Monthly_Means_CurrentYear.csv"), 
          row.names=FALSE)
write.csv(RBT_SEIN_Annual_Means,
          file=file.path(this_out_root,"RBT_SEIN_Annual_Means.csv"), row.names=FALSE)

write.csv(CHN_Chipps_FL, 
          file=file.path(this_out_root,"CHN_Chipps_FL.csv"), row.names=FALSE)
write.csv(RBT_Chipps_FL, 
          file=file.path(this_out_root,"RBT_Chipps_FL.csv"), row.names=FALSE)


