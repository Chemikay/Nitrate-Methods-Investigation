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
  mutate(
    date = ymd(date),
    date_time = ymd_hms(date_time),
    nitrate_as_N = result_diss_nitrate / 4.3
  )

#data visualization

ggplot(dpaired_nitrate_adj, aes(x = station_number)) +
  geom_point(
    aes(
      y = result_diss_nitrate,
      color = "Dissolve Nitrate as NO3"
    ),
    alpha = 0.5
  ) +
  geom_point(
    aes(
      y = nitrate_as_N,
      color = "Dissolve Nitrate as N"
    ),
    alpha = 0.5
  ) +
  geom_point(
    aes(
      y = result_diss_nitrate_nitrite,
      color = "Dissolve Nitrate+Nitrite as N"
    ),
    alpha = 0.5
  ) +
  scale_color_manual(
    values = c(
      "Dissolve Nitrate as NO3" = "#1f77b4",
      "Dissolve Nitrate as N" = "#d62728",
      "Dissolve Nitrate+Nitrite as N" = "#9467bd"
    )
  ) +
  labs(
    x = "Station Number",
    y = "Value",
    color = "Legend",
    title = "Overlay of Nitrate Results"
  )

dpaired_nitrate_adj_c <- dpaired_nitrate_adj %>%
  # Remove rows where dissolved nitrate values are <RL
  drop_na(result_diss_nitrate) %>%
  # Replace dissolved nitrate + nitrite values that are <RL with their RL values
  # to get a coarse idea of how they compare to the dissolved nitrate values
  mutate(
    result_diss_nitrate_nitrite = if_else(
      detection_condition_diss_nitrate_nitrite == "Not Detected",
      reporting_limit_diss_nitrate_nitrite,
      result_diss_nitrate_nitrite
    )
  ) %>%
  # Added some new categories here to differentiate N+N values <RL
  mutate(
    less_flag_new = case_when(
      nitrate_as_N <= result_diss_nitrate_nitrite &
        detection_condition_diss_nitrate_nitrite == "Detected" ~ "less",
      nitrate_as_N <= result_diss_nitrate_nitrite &
        detection_condition_diss_nitrate_nitrite ==
          "Not Detected" ~ "less (using RL for N+N)",
      nitrate_as_N > result_diss_nitrate_nitrite &
        detection_condition_diss_nitrate_nitrite == "Detected" ~ "not less",
      nitrate_as_N > result_diss_nitrate_nitrite &
        detection_condition_diss_nitrate_nitrite ==
          "Not Detected" ~ "not less (using RL for N+N)",
      .default = NA_character_
    ),
    less_flag_orig = case_when(
      result_diss_nitrate <= result_diss_nitrate_nitrite &
        detection_condition_diss_nitrate_nitrite == "Detected" ~ "less",
      result_diss_nitrate <= result_diss_nitrate_nitrite &
        detection_condition_diss_nitrate_nitrite ==
          "Not Detected" ~ "less (using RL for N+N)",
      result_diss_nitrate > result_diss_nitrate_nitrite &
        detection_condition_diss_nitrate_nitrite == "Detected" ~ "not less",
      result_diss_nitrate > result_diss_nitrate_nitrite &
        detection_condition_diss_nitrate_nitrite ==
          "Not Detected" ~ "not less (using RL for N+N)",
      .default = NA_character_
    )
  )


#With the adjusted nitrate values (to report as N), 

nitrate_as_n_summary <- dpaired_nitrate_adj_c %>%
  count(less_flag_new, name = "n_records")

nitrate_as_n_summary

#less_flag_new n_records
#1                        less      2223
#2     less (using RL for N+N)        85
#3                    not less      3381
#4 not less (using RL for N+N)        54

#now 3435 of 5743 samples have a higher value for nitrate than for nitrate + nitrite, 
#so only 2308 show the expected relationship of nitrate + nitrite >= nitrate

#now compare with original values for nitrate
nitrate_as_n03_summary <- dpaired_nitrate_adj_c %>%
  count(less_flag_orig, name = "n_records")

nitrate_as_n03_summary

#less_flag_orig n_records
#1                        less         8
#2                    not less      5596
#3 not less (using RL for N+N)       139


# With the original nitrate values when compared to their nitrate + nitrite counterparts,
# 5735 out of 5743 samples had a higher value for nitrate than for nitrate + nitrite


####################################################################################
### Investigate nitrate adjusted values that are still higher than nitrate + nitrite
####################################################################################


#parse out these specific data into new df
nitrate_adj1 <- filter(dpaired_nitrate_adj_c, less_flag_new == "not less" | less_flag_new == "not less (using RL for N+N)")

unique(nitrate_adj1$method_diss_nitrate)
#just shows EPA 300.0 as method, looks like method name standardized to 300.0 even if 300.0 28hold (mod method)

#match up data, build up from scratch. back to data_bound1, to handle "<RL" values like done for dpaired data

unique(data_bound1$analyte)

data_bound2 <- filter(data_bound1, analyte == "Dissolved Nitrate + Nitrite" | analyte == "Dissolved Nitrate" )

# Look for duplicates using analyte, sample code, and collection date as unique identifiers
r = data_bound3 %>% 
  count(sample_code, collection_date, analyte) %>% 
  filter(n > 1)
#2104 duplicates

#some data like sample C0115B0058 appear in both dset3 & dset8 and both normal samples, just remove duplicates from now. 

#pivot wider to group by sample ID, pick first instance of sample ID present to remove these 34 dup values

data_bound_wide <- data_bound2 %>% 
  pivot_wider(id_cols = c(sample_code, collection_date, station_number),
              names_from = analyte, values_from = c(rpt_limit, result, method),
              values_fill = NA, values_fn = first)

r1 = data_bound_wide %>% 
  count(sample_code, collection_date) %>% 
  filter(n > 1)
#no more duplicates

data_bound_wide <- clean_names(data_bound_wide)

#clean up data by turning results into number they're less than

data_bound_wide1 <- mutate(data_bound_wide, result_dissolved_nitrate = as.numeric(str_remove(result_dissolved_nitrate, "<")))
#NAs introduced by coersion but not an issue for this purpose.

data_bound_wide2 <- mutate(data_bound_wide1, result_dissolved_nitrate_nitrite = as.numeric(str_remove(result_dissolved_nitrate_nitrite, "<")))
#NAs introduced by coersion but not an issue for this purpose.

#remove NAs
data_bound_wide3 <- data_bound_wide2 %>%
  drop_na(result_dissolved_nitrate) %>%
  drop_na(result_dissolved_nitrate_nitrite)

dpaired_kr <- data_bound_wide3 %>%
  mutate(nitrate_as_N = result_dissolved_nitrate / 4.3)

#evaluate data pairs (from scratch) by applying the flags as done for dpaired data

dpaired_kr2 <- dpaired_kr %>%
  mutate(
    detection_condition_diss_nitrate_nitrite = 
      if_else(result_dissolved_nitrate_nitrite > rpt_limit_dissolved_nitrate_nitrite, "Detected",
              "Not Detected"))


dpaired_kr3 <- dpaired_kr2 %>%
  mutate(
    detection_condition_diss_nitrate_adj= 
      if_else(nitrate_as_N > rpt_limit_dissolved_nitrate, "Detected",
              "Not Detected"))
  
dpaired_kr4<- dpaired_kr3 %>%
  # Added some new categories here to differentiate N+N values <RL
  mutate(
    less_flag_new = case_when(
      nitrate_as_N <= result_dissolved_nitrate_nitrite &
        detection_condition_diss_nitrate_nitrite == "Detected" ~ "less",
      nitrate_as_N <= result_dissolved_nitrate_nitrite &
        detection_condition_diss_nitrate_nitrite ==
        "Not Detected" ~ "less (using RL for N+N)",
      nitrate_as_N > result_dissolved_nitrate_nitrite &
        detection_condition_diss_nitrate_nitrite == "Detected" ~ "not less",
      nitrate_as_N > result_dissolved_nitrate_nitrite &
        detection_condition_diss_nitrate_nitrite ==
        "Not Detected" ~ "not less (using RL for N+N)",
      .default = NA_character_
    ),
    less_flag_orig = case_when(
      result_dissolved_nitrate <= result_dissolved_nitrate_nitrite &
        detection_condition_diss_nitrate_nitrite == "Detected" ~ "less",
      result_dissolved_nitrate <= result_dissolved_nitrate_nitrite &
        detection_condition_diss_nitrate_nitrite ==
        "Not Detected" ~ "less (using RL for N+N)",
      result_dissolved_nitrate > result_dissolved_nitrate_nitrite &
        detection_condition_diss_nitrate_nitrite == "Detected" ~ "not less",
      result_dissolved_nitrate > result_dissolved_nitrate_nitrite &
        detection_condition_diss_nitrate_nitrite ==
        "Not Detected" ~ "not less (using RL for N+N)",
      .default = NA_character_
    )
  )


nitrate_as_n_summary_kr <- dpaired_kr4 %>%
  group_by(less_flag_new) %>%
  summarize(
    n_records  = n(),
    .groups = "drop"
  )

#less_flag_new               n_records
#<chr>                           <int>
#1 less                             2281
#2 less (using RL for N+N)           423
#3 not less                         3481
#4 not less (using RL for N+N)       572
 

#With the adjusted nitrate values (to report as N) from my recreation of paired dataframe
#now 4053 of 6757 samples have a higher value for nitrate than for nitrate + nitrite, 
#so only 2704 show that expected relationship.(40%)


nitrate_as_no3_summary_kr <- dpaired_kr4 %>%
  group_by(less_flag_orig) %>%
  summarize(
    n_records  = n(),
    .groups = "drop"
  )

#less_flag_orig              n_records
#<chr>                           <int>
#1 less                               23
#2 less (using RL for N+N)             1
#3 not less                         5739
#4 not less (using RL for N+N)       994


#With the original nitrate values when compared to their nitrate +nitrite counterparts, from my recreation of paired dataframe
#6733 out of 6757 samples had a higher value for nitrate than for nitrate + nitrite
#only 24 samples show expected relationship (0.3%)


#parse out these specific data with adjusted nitrate values (to report as N) that are still higher into new df
nitrate_as_n_kr_higher <- filter(dpaired_kr4, less_flag_new == "not less" | less_flag_new == "not less (using RL for N+N)")
#4053, matches with above

#summarize
nitrate_as_n_kr_higher_summary <- nitrate_as_n_kr_higher %>%
  group_by(method_dissolved_nitrate) %>%
  summarize(
    first_used = min(collection_date, na.rm = TRUE),
    last_used  = max(collection_date, na.rm = TRUE),
    n_records  = n(),
    .groups = "drop"
  )

#method_dissolved_nitrate first_used last_used  n_records
#<chr>                    <date>     <date>         <int>
#1 EPA 300.0 28d Hold [1]*  2010-01-04 2020-06-29      3763
#2 EPA 300.0 [1]*           2011-11-14 2024-06-26       290
 

#summarize, comparing overall method prevalence
nitrate_summary_kr <- dpaired_kr4 %>%
  group_by(method_dissolved_nitrate) %>%
  summarize(
    first_used = min(collection_date, na.rm = TRUE),
    last_used  = max(collection_date, na.rm = TRUE),
    n_records  = n(),
    .groups = "drop"
  )

#method_dissolved_nitrate first_used last_used  n_records
#<chr>                    <date>     <date>         <int>
#1 EPA 300.0 28d Hold [1]*  2010-01-04 2020-07-15      5646
#2 EPA 300.0 [1]*           2011-11-14 2024-06-26      1111

#16% of samples analyzed by EPA 300.0 (unmodified). For those with a higher adj nitrate as N value,
#7% were run by 300.0 (unmodified)

summary(dpaired_kr4$nitrate_as_N)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.06977 0.23488 0.40248 0.53488 6.16279 

summary(nitrate_as_n_kr_higher$nitrate_as_N)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.02326 0.09302 0.30233 0.45971 0.60465 5.34884 


#visualize over time
ggplot(dpaired_kr4, aes(x = collection_date)) +
  geom_freqpoly()+
  labs(
    title = "Frequency of total paired sample submissions",
    x = "Collection Date",
    y = "Sample Count"
  )

ggplot(nitrate_as_n_kr_higher, aes(x = collection_date)) +
  geom_freqpoly()+
  labs(
    title = "Frequency of total paired sample submissions, nitrate as n higher",
    x = "Collection Date",
    y = "Sample Count"
  )


# plot nitrate as n, still higher
ggplot(nitrate_as_n_kr_higher, aes(x = collection_date)) +
  geom_point(
    aes(
      y = result_dissolved_nitrate,
      color = "Dissolve Nitrate as NO3"
    ),
    alpha = 0.5
  ) +
  geom_point(
    aes(
      y = nitrate_as_N,
      color = "Dissolve Nitrate as N"
    ),
    alpha = 0.5
  ) +
  geom_point(
    aes(
      y = result_dissolved_nitrate_nitrite,
      color = "Dissolve Nitrate+Nitrite as N"
    ),
    alpha = 0.5
  ) +
  scale_color_manual(
    values = c(
      "Dissolve Nitrate as NO3" = "#1f77b4",
      "Dissolve Nitrate as N" = "#d62728",
      "Dissolve Nitrate+Nitrite as N" = "#9467bd"
    )
  ) +
  labs(
    x = "Collection Date",
    y = "Value",
    color = "Legend",
    title = "Overlay of Nitrate Results, where Nitrate as N > N+N"
  )


