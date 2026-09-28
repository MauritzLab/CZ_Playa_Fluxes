# Smartflux system is causing issues with rainfall in final processed files
# magnitude is not right, rows are missing.... 

# gather data 2021-2015 from: 
# Dryland CZ Teams folder (Nov 2021-March 2023)
# CZO Data folder (July 2023 - Sep 2024)
# Datahub Data folder (Sep 2024 - Nov 2025)

# load libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
library(stringr)

### Data from DrylandCZ Teams channel! 
setwd("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/Tower Data/JER_Playa/Data/Data_L1/Biomet")

# read column names and import data
biomet.L1.head <- colnames(read.table("Biomet_L1_2021.csv", sep=",", dec=".", header=TRUE,
                                      na.strings=c("NAN","NaN","NA",-9999)))

biomet.L1.2021 <- read.table("Biomet_L1_2021.csv", sep=",", dec=".", skip=2, header=FALSE,
                             col.names = biomet.L1.head, na.strings=c("NAN","NaN","NA",-9999))

biomet.L1.2022 <- read.table("Biomet_L1_2022.csv", sep=",", dec=".", skip=2, header=FALSE,
                             col.names = biomet.L1.head, na.strings=c("NAN","NA",-9999))

biomet.L1.2023 <- read.table("Biomet_L1_2023.csv", sep=",", dec=".", skip=2, header=FALSE,
                             col.names = biomet.L1.head, na.strings=c("NAN","NaN","NA",-9999))

biomet.L1 <- rbind(biomet.L1.2021, biomet.L1.2022,biomet.L1.2023)

# format to long
biomet.long.L1 <- biomet.L1 %>%
  pivot_longer(!c(TIMESTAMP,RECORD), names_to="variable",values_to="value") %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP),
         date_time = floor_date(TIMESTAMP, "30 minutes"))

biomet.30min.L1 <- biomet.long.L1 %>%
  group_by(date_time, variable) %>%
  # Summarize the data (e.g., calculate the mean value)
  summarize(
    MeanValue = mean(value, na.rm = TRUE),
    TotalValue = sum(value, na.rm = TRUE),
    .groups = "drop" # Drop the grouping structure after summarizing
  )%>%
  mutate(year=year(date_time),
         month=month(date_time),
         date = as.Date(date_time))

# graph 30 min rainfall
biomet.30min.L1 %>%
  filter(variable %in% c("P_RAIN_8_19_1_1_1")) %>%
  ggplot(.,aes(date_time, TotalValue))+
  geom_point(size=0.5) 


# calculate monthly summaries

# calculate total monthly rainfall
biomet.month.L1 <- biomet.30min.L1 %>%
  group_by(year,month,variable)%>%
  filter(variable %in% c("P_RAIN_8_19_1_1_1")) %>%
  summarise(P.month = sum(TotalValue,na.rm=TRUE))

# graph monthly rainfall
biomet.month.L1 %>%
  ggplot(.,aes(factor(month), P.month))+
  geom_col() +
  facet_grid(.~year)



### Data from CZO_Data OneDrive
setwd("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/CZO_data/data/RedLake/CR3000/L1/Biomet")

# read column names and import data
biomet.head.cz <- colnames(read.table("RedLake_CR3000_Biomet_L1_2024_20240929_223000.csv", sep=",", dec=".", skip=1, header=TRUE))
biomet.cz.2023 <- read.table("RedLake_CR3000_Biomet_L1_2023.csv", sep=",", dec=".", skip=4, header=FALSE,
                            col.names=biomet.head.cz, na.strings=c("NAN","NaN","NA",-9999))

biomet.cz.2024.1 <- read.table("RedLake_CR3000_Biomet_L1_2024_20240929_223000.csv", sep=",", dec=".", skip=4, header=FALSE,
                     col.names=biomet.head.cz, na.strings=c("NAN","NaN","NA",-9999))

#replcace with file in tempshare
# biomet.2024.2 <- read.table("RedLake_CR3000_Biomet_L1_2024.csv", sep=",", dec=".", skip=4, header=FALSE,
#                            col.names=biomet.head, na.strings=c("NAN","NaN","NA",-9999))

biomet.cz.2025 <- read.table("RedLake_CR3000_Biomet_L1_2025.csv", sep=",", dec=".", skip=4, header=FALSE,
                            col.names=biomet.head.cz, na.strings=c("NAN","NaN","NA",-9999))

# combine mutliple years
biomet.cz <- rbind(biomet.cz.2023, biomet.cz.2024.1, biomet.cz.2025)

# format to long
biomet.long.cz <- biomet.cz %>%
  pivot_longer(!c(TIMESTAMP,RECORD), names_to="variable",values_to="value") %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP),
         date_time = floor_date(TIMESTAMP, "30 minutes"))

biomet.30min.cz <- biomet.long.cz %>%
  group_by(date_time, variable) %>%
  # Summarize the data (e.g., calculate the mean value)
  summarize(
    MeanValue = mean(value, na.rm = TRUE),
    TotalValue = sum(value, na.rm = TRUE),
    .groups = "drop" # Drop the grouping structure after summarizing
  )%>%
  mutate(year=year(date_time),
         month=month(date_time),
         date = as.Date(date_time))

# graph 30 min rainfall
biomet.30min.cz %>%
  filter(variable %in% c("P_RAIN_8_19_1_1_1")) %>%
  ggplot(.,aes(date_time, TotalValue))+
  geom_point(size=0.5) 


# calculate monthly summaries
# calculate total monthly rainfall
biomet.month.cz <- biomet.30min.cz %>%
  group_by(year,month,variable)%>%
  filter(variable %in% c("P_RAIN_8_19_1_1_1")) %>%
  summarise(P.month = sum(TotalValue,na.rm=TRUE))

# graph monthly rainfall
biomet.month.cz %>%
  ggplot(.,aes(factor(month), P.month))+
  geom_col() +
  facet_grid(.~year)


### Data from DataHub/Data folder
setwd("/Volumes/Data/RedLake/CR3000/L1/Biomet")

# read in data
biomet.head.dh <- colnames(read.table("RedLake_CR3000_Biomet_L1_2024.csv", sep=",", dec=".", skip=1, header=TRUE))
biomet.dh.2024 <- read.table("RedLake_CR3000_Biomet_L1_2024.csv", sep=",", dec=".", skip=4, header=FALSE,
                             col.names=biomet.head.dh, na.strings=c("NAN","NaN","NA",-9999))

# biomet.dh.2025.1 <- read.table("RedLake_CR3000_Biomet_L1_2025.csv", sep=",", dec=".", skip=4, header=FALSE,
#                            col.names=biomet.head.dh, na.strings=c("NAN","NaN","NA",-9999))

# use this file, overlaps with start of Biomet_L1_2025 and goes until Nov 27 (instead of Oct)
biomet.dh.2025.2 <- read.table("RedLake_CR3000_Biomet_L1_2025_20251127_042612.csv", sep=",", dec=".", skip=4, header=FALSE,
                               col.names=biomet.head.dh, na.strings=c("NAN","NaN","NA",-9999))

# combine
biomet.dh <- rbind(biomet.dh.2024,biomet.dh.2025.2)

# format to long
biomet.long.dh <- biomet.dh %>%
  pivot_longer(!c(TIMESTAMP,RECORD), names_to="variable",values_to="value") %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP),
         date_time = floor_date(TIMESTAMP, "30 minutes"))

biomet.30min.dh <- biomet.long.dh %>%
  group_by(date_time, variable) %>%
  # Summarize the data (e.g., calculate the mean value)
  summarize(
    MeanValue = mean(value, na.rm = TRUE),
    TotalValue = sum(value, na.rm = TRUE),
    .groups = "drop" # Drop the grouping structure after summarizing
  )%>%
  mutate(year=year(date_time),
         month=month(date_time),
         date = as.Date(date_time))

# graph 30 min rainfall
biomet.30min.dh %>%
  filter(variable %in% c("P_RAIN_8_19_1_1_1")) %>%
  ggplot(.,aes(date_time, TotalValue))+
  geom_point(size=0.5) 


# calculate monthly summaries
# calculate total monthly rainfall
biomet.month.dh <- biomet.30min.dh %>%
  group_by(year,month,variable)%>%
  filter(variable %in% c("P_RAIN_8_19_1_1_1")) %>%
  summarise(P.month = sum(TotalValue,na.rm=TRUE))

# graph monthly rainfall
biomet.month.dh %>%
  ggplot(.,aes(factor(month), P.month))+
  geom_col() +
  facet_grid(.~year)


# stack all 30 minute biomet
biomet.30min.all <- rbind(biomet.30min.L1,biomet.30min.cz,biomet.30min.dh)

# remove duplicates
biomet.30min.all <- biomet.30min.all %>%
  distinct()

# check for duplicates in date_time and variable
checkdups <- biomet.30min.all|>
     dplyr::summarise(n = dplyr::n(), .by = c(date_time, variable)) |>
      dplyr::filter(n > 1L) 

# there are still some duplicates in 2025-02-26 to 2025-02-27
# some are NA and some have values

biomet.30min.all <- biomet.30min.all %>%
  group_by(date_time,variable) %>%
# Arrange so that non-NA 'Value' rows appear first (FALSE < TRUE)
arrange(is.na(MeanValue), .by_group = TRUE) %>%
  # Keep only the first row for each group, keeping all other variables
  distinct(date_time, variable, .keep_all = TRUE) %>%
  ungroup()

# graph 30 min rainfall
biomet.30min.all %>%
  filter(variable %in% c("P_RAIN_8_19_1_1_1")) %>%
  ggplot(.,aes(date_time, TotalValue))+
  geom_point(size=0.5) 

# calculate monthly summaries
# calculate total monthly rainfall
biomet.month.all <- biomet.30min.all %>%
  group_by(year,month,variable)%>%
  filter(variable %in% c("P_RAIN_8_19_1_1_1")) %>%
  summarise(P.month = sum(TotalValue,na.rm=TRUE))

# graph monthly rainfall
biomet.month.all %>%
  ggplot(.,aes(factor(month), P.month))+
  geom_col() +
  facet_grid(.~year)

# Make data wide and keep only total values for rainfall
# rename variables to drop first two numbers in variable names

biomet.30min.all <- biomet.30min.all %>%
 separate_wider_delim(variable, "_", names=c("v1",NA,NA,"v4"),too_many="merge") %>%
  mutate(variable.mod = paste(v1,v4,sep="_"))

# Make data wide
biomet.30min.wide <- biomet.30min.all %>%
  select(date_time,variable.mod,MeanValue,TotalValue)%>%
  pivot_wider(names_from=variable.mod,
              names_glue = "{variable.mod}_{.value}",
              values_from=c(MeanValue,TotalValue))


# select columns
biomet.30min.wide.sub <- biomet.30min.wide %>%
  select(date_time,ends_with("MeanValue") & !(ends_with("P_19")) | (starts_with("P_19") & ends_with("TotalValue"))) %>%
  rename(P_RAIN_1_1_1_TotalValue = P_19_1_1_1_TotalValue )


# graph 30 min rainfall
biomet.30min.wide.sub %>%
  ggplot(.,aes(date_time, P_RAIN_1_1_1_TotalValue))+
  geom_point(size=0.5) 

# save as L2 tables by year
# SAVE TO CZO_Data
setwd("/Users/memauritz/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/CZO_data/data/RedLake/CR3000/L2/Biomet/")

# # save by years
 for (i in 2021:2025){
   # convert to datatable
   dat.save <- as.data.table(biomet.30min.wide.sub)
   # subset each year
   dat.save <- dat.save[year(date_time)==i,]
   date.min <- format(min(dat.save$date_time), "%Y%m%d%H%M%S")
   date.max <- format(max(dat.save$date_time), "%Y%m%d%H%M%S")
#   
   write.table (dat.save,
             file= paste("RedLake_Biomet_L2_",date.min,"_",date.max,
                          ".csv",sep=""),
              sep =',', dec='.', row.names=FALSE, na="-9999", quote=FALSE)
 }

