#####################################################
# Nitrate Methods Investigation
# By: Katey Rein 
#     DWR Quality Assurance Unit

#####################################################
### Load libraries and import data
#####################################################
library(readr)
library(janitor)
library(here)
library(dplyr)
library(lubridate)
library(tidyverse)
library(data.table)
library(writexl)

#Pull in all data individually, different number of columns so can't bind directly

dset1 <- read.csv("NCRO Data.csv")
dset2 <- read.csv("O&M Data_CA Aquaduct Grab Sample_1.csv")
dset3 <- read.csv("O&M Data_CA Aquaduct Grab Sample_2.csv")
dset4 <- read.csv("O&M Data_CA Aquaduct Grab Sample_3.csv")
dset5 <- read.csv("O&M Data_NBA Grab Sample.csv")
dset6 <- read.csv("O&M Data_OFD Grab Sample.csv")
dset7 <- read.csv("O&M Data_SBA Grab Sample.csv")
dset8 <- read.csv("WQA Discrete Data.csv")

dset1<- clean_names(dset1)
dset2<- clean_names(dset2)
dset3<- clean_names(dset3)
dset4<- clean_names(dset4)
dset5<- clean_names(dset5)
dset6<- clean_names(dset6)
dset7<- clean_names(dset7)
dset8<- clean_names(dset8)


#####################################################
### Format and manipulate WDL and Lab data 
#####################################################

names(dset1)
names(dset2)

#remove unnecessary columns from all dataframes

dset1a <- select(dset1, data_owner, data_status, long_station_name, short_station_name,
                 station_number, sample_code, collection_date, analyte, result, rpt_limit,
                 units, method, sample_type, description, notes)

#dset2 doesn't have data_owner or data_status

dset2a <- select(dset2, long_station_name, short_station_name,
                 station_number, sample_code, collection_date, analyte, result, rpt_limit,
                 units, method, sample_type, description, notes)

names(dset3)

dset3a <- select(dset3, long_station_name, short_station_name,
                 station_number, sample_code, collection_date, analyte, result, rpt_limit,
                 units, method, sample_type, description, notes)

dset4a <- select(dset4, long_station_name, short_station_name,
                 station_number, sample_code, collection_date, analyte, result, rpt_limit,
                 units, method, sample_type, description, notes)

dset5a <- select(dset5, long_station_name, short_station_name,
                 station_number, sample_code, collection_date, analyte, result, rpt_limit,
                 units, method, sample_type, description, notes)

dset6a <- select(dset6, long_station_name, short_station_name,
                 station_number, sample_code, collection_date, analyte, result, rpt_limit,
                 units, method, sample_type, description, notes)

dset7a <- select(dset7, long_station_name, short_station_name,
                 station_number, sample_code, collection_date, analyte, result, rpt_limit,
                 units, method, sample_type, description, notes)

dset8a <- select(dset8, long_station_name, short_station_name,
                 station_number, sample_code, collection_date, analyte, result, rpt_limit,
                 units, method, sample_type, description, notes)

#data_owner and data_status don't exist for any dsets other than dset1 

#for now, don't worry about filling this in. 

dset1a <- select(dset1, long_station_name, short_station_name,
                 station_number, sample_code, collection_date, analyte, result, rpt_limit,
                 units, method, sample_type, description, notes)

#bind data

data_bound <- bind_rows(dset1a, dset2a, dset3a, dset4a, dset5a, dset6a, dset7a, dset8a)


#####################################################
### Analyze data
#####################################################

str(data_bound)

#reformat collection date field, result field

#Convert to POSIXct
data_bound$collection_date <- as.POSIXct(
  data_bound$collection_date,
  format = "%m/%d/%Y %H:%M")

data_bound1 <- mutate(data_bound, collection_date = as.Date(collection_date, format = '%m/%d/%Y'))

class(data_bound1$collection_date)


data_bound2 <- mutate(data_bound1, result = as.numeric(result))
#NAs introduced by coersion but not an issue for this purpose. 

unique(data_bound2$analyte)
#two nitrate analytes

#filter out other analytes

data_bound_nitrate <- filter(data_bound1, analyte == "Dissolved Nitrate" | analyte == "Dissolved Nitrate + Nitrite")

unique(data_bound_nitrate$method)
#five different methods

unique(data_bound_nitrate$units)
#two different reporting units, "mg/l as N" and "mg/L"

#create breakdown of methods by date range of usage
method_ranges <- data_bound_nitrate %>%
  group_by(method) %>%
  summarize(
    first_used = min(collection_date, na.rm = TRUE),
    last_used  = max(collection_date, na.rm = TRUE),
    n_records  = n(),
    .groups = "drop"
  )

print(method_ranges)

#method                                    first_used last_used  n_records
#<chr>                                     <date>     <date>         <int>
#1 EPA 300.0 28d Hold [1]*                   2010-01-04 2020-07-15      8234
#2 EPA 300.0 [1]*                            2011-11-14 2024-06-26      1353
#3 EPA 353.2 [1]*                            2021-01-14 2021-01-20         7
#4 Std Method 4500-NO3-F (DWR Modified) [1]* 1999-05-11 2020-10-29      7566
#5 Std Method 4500-NO3-F [1]*                2020-10-29 2024-12-19      2361

#parse out data by method
method_300_28Hold <- filter(data_bound_nitrate, method == "EPA 300.0 28d Hold [1]*" )

method_300 <- filter(data_bound_nitrate, method == "EPA 300.0 [1]*" )

method_353.2 <- filter(data_bound_nitrate, method == "EPA 353.2 [1]*" )

method_4500_DWR <- filter(data_bound_nitrate, method == "Std Method 4500-NO3-F (DWR Modified) [1]*" )

method_4500 <- filter(data_bound_nitrate, method == "Std Method 4500-NO3-F [1]*" )

#investigate units across methods
unique(method_300_28Hold$units)
#just mg/L

unique(method_300$units)
#just mg/L

unique(method_353.2$units)
#both mg/L as N and mg/L

unique(method_4500_DWR$units)
#just mg/L as N

unique(method_4500$units)
#just mg/L as N 

#create breakdown of method 353.2 units by date range of usage
unit_ranges_353.2 <- method_353.2 %>%
  group_by(units) %>%
  summarize(
    first_used = min(collection_date, na.rm = TRUE),
    last_used  = max(collection_date, na.rm = TRUE),
    n_records  = n(),
    .groups = "drop"
  )

print(unit_ranges_353.2)
#units     first_used last_used  n_records
#<chr>     <date>     <date>         <int>
#1 mg/L      2021-01-20 2021-01-20         4
#2 mg/L as N 2021-01-14 2021-01-14         3

##################################################################
### Evaluate paired data for Nitrate as N
##################################################################

dpaired <- read.csv("paired_nitrate_data.csv")
dpaired <- clean_names(dpaired)
names(dpaired)
class(dpaired$result_diss_nitrate)

dpaired_nitrate_adj <- dpaired %>%
  mutate(nitrate_as_N = result_diss_nitrate / 4.3)

#data visualization
ggplot(dpaired_nitrate_adj, aes(x = date)) +
  geom_line(aes(y = result_diss_nitrate, color = "Dissolve Nitrate as NO3"), linewidth = 1) +
  geom_line(aes(y = nitrate_as_N, color = "Dissolve Nitrate as N"), linewidth = 1) +
  geom_line(aes(y = result_diss_nitrate_nitrite, color = "Dissolve Nitrate+Nitrite as N"), linewidth = 1) +
  scale_color_manual(values = c("Dissolve Nitrate as NO3" = "#1f77b4", "Dissolve Nitrate as N" = "#d62728", 
  "Dissolve Nitrate+Nitrite as N" = "#9467bd")) +
  labs(x = "Date", y = "Value", color = "Legend", title = "Overlay of Nitrate Results")
       

ggplot(dpaired_nitrate_adj, aes(x = station_number)) +
  geom_line(aes(y = result_diss_nitrate, color = "Dissolve Nitrate as NO3"), linewidth = 1) +
  geom_line(aes(y = nitrate_as_N, color = "Dissolve Nitrate as N"), linewidth = 1) +
  geom_line(aes(y = result_diss_nitrate_nitrite, color = "Dissolve Nitrate+Nitrite as N"), linewidth = 1) +
  scale_color_manual(values = c("Dissolve Nitrate as NO3" = "#1f77b4", "Dissolve Nitrate as N" = "#d62728", 
                                "Dissolve Nitrate+Nitrite as N" = "#9467bd")) +
  labs(x = "Station Number", y = "Value", color = "Legend", title = "Overlay of Nitrate Results")



dpaired_nitrate_adj <- dpaired_nitrate_adj %>%
  mutate(
    less_flag_new = case_when(
      nitrate_as_N < result_diss_nitrate_nitrite ~ "less",
      TRUE ~ "not less"),
    less_flag_orig = case_when(result_diss_nitrate <result_diss_nitrate_nitrite ~"less",
                               TRUE ~"not less"))
  
nitrate_as_n_summary <- dpaired_nitrate_adj %>%
  group_by(less_flag_new) %>%
  summarize(
    n_records  = n(),
    .groups = "drop"
  )

#With the adjusted nitrate values (to report as N), 
#now 4509 of 6730 samples have a higher value for nitrate than for nitrate + nitrite, 
#so 2221 still show that unexpected relationship.

nitrate_as_n03_summary <- dpaired_nitrate_adj %>%
  group_by(less_flag_orig) %>%
  summarize(
    n_records  = n(),
    .groups = "drop"
  )
#With the original nitrate values when compared to their nitrate +nitrite counterparts, 
#6722 out of 6730 samples had a higher value for nitrate than for nitrate + nitrite 



##################################################################
### Convert reported concentrations of Nitrate to be reported as N
##################################################################

#change result class to numeric
method_300 <- mutate(method_300, result = as.numeric(result))

method_300 <- method_300 %>%
  mutate(result_as_N = result / 4.3)

#not done yet


#look at data affected by being reported out as NO3 rather than N, AKA non-detects vs reported concentration


