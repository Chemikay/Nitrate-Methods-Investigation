#####################################################
# Nitrate Methods Investigation (unvalidated data)
# By: Katey Rein &  Taylor Rohlin
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

#make small function to remove duplicates for TKN analysis
remove_dups <- function(df) {
  df %>% filter(is.na(lab_dup) | lab_dup != "Y")
}

#apply function to data sets
dset1 <- remove_dups(dset1)
dset2 <- remove_dups(dset2)
dset3 <- remove_dups(dset3)
dset4 <- remove_dups(dset4)
dset5 <- remove_dups(dset5)
dset6 <- remove_dups(dset6)
dset7 <- remove_dups(dset7)
dset8 <- remove_dups(dset8)



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
#NAs introduced by coersion but not an issue for this purpose.values were either under RL, N.S. or blank in data_bound1
#data_bound2 <- mutate(data_bound1, result = as.numeric(result))
#NAs introduced by coersion but not an issue for this purpose.

unique(data_bound2$analyte)
#two nitrate analytes

#filter out other analytes

data_bound_nitrate <- filter(data_bound2, analyte == "Dissolved Nitrate" | analyte == "Dissolved Nitrate + Nitrite" | analyte == "Total Kjeldahl Nitrogen")


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
    nitrate_as_N = result_diss_nitrate / 4.3,
    nitrate_RL_as_N = reporting_limit_diss_nitrate / 4.3
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
r = data_bound2 %>% 
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
  mutate(nitrate_as_N = result_dissolved_nitrate / 4.3,
         nitrate_RL_as_N = rpt_limit_dissolved_nitrate / 4.3)

#evaluate data pairs (from scratch) by applying the flags as done for dpaired data

dpaired_kr2 <- dpaired_kr %>%
  mutate(
    detection_condition_diss_nitrate_nitrite = 
      if_else(result_dissolved_nitrate_nitrite > rpt_limit_dissolved_nitrate_nitrite, "Detected",
              "Not Detected"))


dpaired_kr3 <- dpaired_kr2 %>%
  mutate(
    detection_condition_diss_nitrate_adj= 
      if_else(nitrate_as_N > nitrate_RL_as_N, "Detected",
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

#match up data, build up from scratch. back to data_bound1, to handle "<RL" values like done for dpaired data

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

#visualize over concentration
ggplot(dpaired_kr4, aes(x = nitrate_as_N)) +
  geom_freqpoly()+
  labs(
    title = "Frequency of total paired samples by concentration",
    x = "Nitrate as N",
    y = "Sample Count"
  )

ggplot(nitrate_as_n_kr_higher, aes(x = nitrate_as_N)) +
  geom_freqpoly()+
  labs(
    title = "Frequency of paired sample submissions by concentration where nitrate as n higher",
    x = "Nitrate as N",
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

#absolute difference calculation

nitrate_as_n_kr_higher_ad <- nitrate_as_n_kr_higher %>%
  mutate(
    absolute_diff = 
      nitrate_as_N - result_dissolved_nitrate_nitrite)

#nitrate+nitrite data have two different reporting limits, separate data by RL

ad_nn1 <- filter(nitrate_as_n_kr_higher_ad, rpt_limit_dissolved_nitrate_nitrite == "0.05" )
#475 samples

ad_nn2 <- filter(nitrate_as_n_kr_higher_ad, rpt_limit_dissolved_nitrate_nitrite == "0.01" )
#3578 samples

#diff on y axis, nitrate nitrite result on x axis
a <- ggplot(ad_nn1, aes(x = result_dissolved_nitrate_nitrite)) +
  geom_point(
    aes(
      y = absolute_diff,
          ),
    alpha = 0.5) +
  geom_vline(aes(xintercept = rpt_limit_dissolved_nitrate_nitrite))+
  labs(
    x = "Result N+N",
    y = "Absolute Diff (N adj)",
    title = "Absolute Difference NO3 to N+N (N+N RL = 0.05 as N)"
  )+
  xlim(0, 1)+
  ylim(0,0.25)
#axis limits remove 42 data points

a

b <- ggplot(ad_nn2, aes(x = result_dissolved_nitrate_nitrite)) +
  geom_point(
    aes(
      y = absolute_diff,
    ),
    alpha = 0.5) +
  geom_vline(aes(xintercept = rpt_limit_dissolved_nitrate_nitrite))+
  labs(
    x = "Result N+N",
    y = "Absolute Diff (N adj)",
    title = "Absolute Difference NO3 to N+N (N+N RL = 0.01 as N)"
  )+
  xlim(0,2)+
  ylim(0,1)
b
#axis limits remove 60 data points


#********************************************


#summarize nitrate RLs, 
nitrate_summary_RL <- nitrate_as_n_kr_higher_ad %>%
  group_by(nitrate_RL_as_N) %>%
  summarize(
    first_used = min(collection_date, na.rm = TRUE),
    last_used  = max(collection_date, na.rm = TRUE),
    n_records  = n(),
    .groups = "drop"
  )


#nitrate_RL_as_N first_used last_used  n_records
#<dbl> <date>     <date>         <int>
#1          0.0233 2010-01-04 2024-06-26      3743
#2          0.0465 2012-03-05 2018-12-17         8
#3          0.0581 2010-10-05 2015-02-17        12
#4          0.0698 2013-08-19 2023-11-21         7
#5          0.0774 2010-01-05 2010-09-07        18
#6          0.0872 2023-04-17 2023-04-17         1
#7          0.116  2010-01-05 2024-03-19       158
#8          0.174  2022-11-14 2024-02-20         9
#9          0.233  2011-08-02 2024-04-17        61
#10          0.349  2014-07-21 2018-02-20         3
#11          0.465  2011-10-06 2015-10-21        19
#12          0.581  2010-07-06 2018-06-05         8
#13          1.16   2011-11-07 2018-08-20         3
#14         NA      2018-03-27 2018-03-27         3

  
#having issues with pulling in based on adjusted nitrate RL for some reason
#may be due to very long numerical sequence and truncation
#pulling in based on nitrate RL original (as NO3)

ad_no3a <- filter(nitrate_as_n_kr_higher_ad, rpt_limit_dissolved_nitrate == "0.1" )
#3743 samples

ad_no3b <- filter(nitrate_as_n_kr_higher_ad, rpt_limit_dissolved_nitrate == "0.5")
#158 samples

ad_no3c <- filter(nitrate_as_n_kr_higher_ad, rpt_limit_dissolved_nitrate == "1")
#61 samples


#diff on y axis, nitrate (as N) result on x axis
c <- ggplot(ad_no3a, aes(x = nitrate_as_N)) +
  geom_point(
    aes(
      y = absolute_diff,
    ),
    alpha = 0.5) +
  geom_vline(aes(xintercept = nitrate_RL_as_N))+
  labs(
    x = "Result Nitrate (as N)",
    y = "Absolute Diff (N adj)",
    title = "Absolute Difference NO3 to N+N (Nitrate RL = 0.023 as N)"
  )+
  xlim(0,2)+
  ylim(0,1)

c 
#axis limits remove 75 data points

#diff on y axis, nitrate (as N) result on x axis
d <- ggplot(ad_no3b, aes(x = nitrate_as_N)) +
  geom_point(
    aes(
      y = absolute_diff,
    ),
    alpha = 0.5) +
  geom_vline(aes(xintercept = nitrate_RL_as_N))+
  labs(
    x = "Result Nitrate (as N)",
    y = "Absolute Diff (N adj)",
    title = "Absolute Difference NO3 to N+N (Nitrate RL = 0.11 as N)"
  )+
  xlim(0,2)+
  ylim(0,0.5)


d
#axis limits remove 15 data points


#diff on y axis, nitrate (as N) result on x axis
e <- ggplot(ad_no3c, aes(x = nitrate_as_N)) +
  geom_point(
    aes(
      y = absolute_diff,
    ),
    alpha = 0.5) +
  geom_vline(aes(xintercept = nitrate_RL_as_N))+
  labs(
    x = "Result Nitrate (as N)",
    y = "Absolute Diff (N adj)",
    title = "Absolute Difference NO3 to N+N (Nitrate RL = 0.23 as N)"
  )

e + scale_x_continuous(breaks = seq(0, 4, by = 0.25))

data_bound2 <- filter(data_bound1, analyte == "Dissolved Nitrate + Nitrite" | analyte == "Dissolved Nitrate" )

# Look for duplicates using analyte, sample code, and collection date as unique identifiers
r = data_bound2 %>% 
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
  mutate(nitrate_as_N = result_dissolved_nitrate / 4.3,
         nitrate_RL_as_N = rpt_limit_dissolved_nitrate / 4.3)

#evaluate data pairs (from scratch) by applying the flags as done for dpaired data

dpaired_kr2 <- dpaired_kr %>%
  mutate(
    detection_condition_diss_nitrate_nitrite = 
      if_else(result_dissolved_nitrate_nitrite > rpt_limit_dissolved_nitrate_nitrite, "Detected",
              "Not Detected"))


dpaired_kr3 <- dpaired_kr2 %>%
  mutate(
    detection_condition_diss_nitrate_adj= 
      if_else(nitrate_as_N > nitrate_RL_as_N, "Detected",
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

#visualize over concentration
ggplot(dpaired_kr4, aes(x = nitrate_as_N)) +
  geom_freqpoly()+
  labs(
    title = "Frequency of total paired samples by concentration",
    x = "Nitrate as N",
    y = "Sample Count"
  )

ggplot(nitrate_as_n_kr_higher, aes(x = nitrate_as_N)) +
  geom_freqpoly()+
  labs(
    title = "Frequency of paired sample submissions by concentration where nitrate as n higher",
    x = "Nitrate as N",
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

#absolute difference calculation

nitrate_as_n_kr_higher_ad <- nitrate_as_n_kr_higher %>%
  mutate(
    total_nitrogen = total_kjeldahl_nitrogen +
      dissolved_nitrate_nitrite,
    tn_vs_nox = case_when(
      total_nitrogen > dissolved_nitrate ~ "greater",
      total_nitrogen < dissolved_nitrate ~ "less",
      total_nitrogen == dissolved_nitrate ~ "equal",
      TRUE ~ NA_character_
    ),
    alpha = 0.5) +
  geom_vline(aes(xintercept = nitrate_RL_as_N))+
  labs(
    x = "Result Nitrate (as N)",
    y = "Absolute Diff (N adj)",
    title = "Absolute Difference NO3 to N+N (Nitrate RL = 0.23 as N)"
  )

e + scale_x_continuous(breaks = seq(0, 4, by = 0.25))


#export to share 
write.csv(tkn_wide, "tkn_wide.csv", row.names = FALSE)


####################
### USGS Comparison ************************************************************
####################

#plotting libraries
library(ggplot2)
library(plotly)
library(htmlwidgets)


#data import
#DWR data 
dwr1 <- read.csv("df1.csv") %>%
  filter(sample_type == "Normal Sample") %>% #keep only Normal Samples
  filter(!grepl("N\\.S\\.|<", result)) %>% #remove rows where result contains N.S. or <
  mutate(
    result = as.numeric(result)   #convert result field to numeric
  ) %>%
  filter(result >= rpt_limit) %>% #remove rows where result is less than rpt_limit 
  mutate(agency = "DWR") #add agency column

#filter to just station C10 Vernalis data
dwr2 <- dwr1 %>%
  filter(short_station_name == "C10A")

#filter to just station C3A Hood data
dwr3 <- dwr1 %>%
  filter(short_station_name == "C3A - Hood")

#USGS data
#vector of nutrient specific USGS pcodes that are similar to DWR
usgs_pcodes  <- c(
  618, 631
)

#USGS Vernalis data ******************************************************
usgs1 <- read.csv("USGS Vernalis.csv") %>%
  #keep only nitrogen pcodes that are similar to the two methods in the DWR data.
  filter(USGSpcode %in% usgs_pcodes) %>%
  
  #keep relevant columns
  select(
    Location_Identifier,
    Location_Name,
    Activity_StartDate,
    Activity_StartTime,
    Result_Characteristic,
    Result_Measure,
    Result_MeasureUnit,
    Result_MeasureStatusIdentifier,
    Result_MeasureType,
    USGSpcode
  ) %>%
  rename(
    collection_date = Activity_StartDate,
    analyte = Result_Characteristic,
    result = Result_Measure,
    units = Result_MeasureUnit,
    long_station_name = Location_Name
  ) %>%
  mutate(agency = "USGS")
#make a sample_code column for usgs data
usgs1 <- usgs1 %>%
  mutate(
    sample_code = paste(Location_Identifier, collection_date, sep = "_")
  )
#correct units column based on USGS p codes
usgs1$units <- "mg/L as N"

###Freeport discrete data*****************************************************
usgs2 <- read.csv("USGS Freeport.csv")%>%
  #keep only nitrogen pcodes that are similar to the two methods in the DWR data.
  filter(USGSpcode %in% usgs_pcodes) %>%
  
  #keep relevant columns
  select(
    Location_Identifier,
    Location_Name,
    Activity_StartDate,
    Activity_StartTime,
    Result_Characteristic,
    Result_Measure,
    Result_MeasureUnit,
    Result_MeasureStatusIdentifier,
    Result_MeasureType,
    USGSpcode
  ) %>%
  rename(
    collection_date = Activity_StartDate,
    analyte = Result_Characteristic,
    result = Result_Measure,
    units = Result_MeasureUnit,
    long_station_name = Location_Name
  ) %>%
  mutate(agency = "USGS")
#make a sample_code column for usgs data
usgs2 <- usgs2 %>%
  mutate(
    sample_code = paste(Location_Identifier, collection_date, sep = "_")
  )
#correct units column based on USGS p codes
usgs2$units <- "mg/L as N"

#remove NA values
usgs2 <- usgs2 %>% 
  filter(!is.na(result))

###Freeport realtime data ****************************************************

#create import specs for columns
colspec <- cols(
  id = col_character(),
  time_series_id = col_character(),
  monitoring_location_id = col_character(),
  statistic_id = col_character(),
  unit_of_measure = col_character(),
  approval_status = col_character(),
  qualifier = col_character(),
  parameter_code = col_double(),
  value = col_double(),
  x = col_logical(),
  y = col_logical(),
  time = col_character(),          
  last_modified = col_character()  
)
#make list
df_list <- list(
  read_csv("Freeport 1.csv", col_types = colspec),
  read_csv("Freeport 2.csv", col_types = colspec),
  read_csv("Freeport 3.csv", col_types = colspec),
  read_csv("Freeport 4.csv", col_types = colspec)
)
#bind into one df
freeport <- bind_rows(df_list)
#change date/time
freeport <- freeport %>%
  mutate(time = ymd_hms(time, tz = "UTC")) %>%
  arrange(time)

#rename and reformat dates
freeport <- freeport %>%
  mutate(collection_date = as.character(as.Date(time)))
#rename
freeport <- freeport %>%
  rename(
    long_station_name = monitoring_location_id,
    analyte = statistic_id,
    result = value,
    units = unit_of_measure
  )
#agency column
freeport <- freeport %>%
  mutate(agency = "USGS")

freeport <- freeport %>%
  mutate(analyte = "Nitrate + Nitrite mg/L as N (insitu)")


#reduce real time data down to daily averages
freeport2 <- freeport %>%
  group_by(collection_date) %>%
  summarise(
    long_station_name = first(long_station_name),
    analyte = first(analyte),
    result = first(result),
    units = first(units),
    agency = first(agency),
    daily_avg_value = mean(result, na.rm = TRUE),
    .groups = "drop"
  )

####################
#Vernalis comparison *********************************************************
####################

#how many dates appear in both datasets
shared_dates <- intersect(dwr2$collection_date, usgs1$collection_date)

dwr_match <- dwr2 %>%
  filter(collection_date %in% shared_dates)

usgs_match <- usgs1 %>%
  filter(collection_date %in% shared_dates)

#combine matched data onto one df
combo1 <- bind_rows(
  dwr_match %>%
    select(long_station_name, collection_date, sample_code, analyte, result, units, agency),
  
  usgs_match %>%
    select(long_station_name, collection_date, sample_code, analyte, result, units, agency)
)

#factor agency and analyte for plotting
combo1 <- combo1 %>%
  mutate(
    agency = factor(agency, levels = c("USGS", "DWR")),
    analyte = factor(analyte)
  )
################################################
#Vernalis plot with converted DWR nitrate values*****
################################################

#prepare data
combo_plot <- combo1 %>%
  mutate(
    analyte_label = paste0(analyte, " (", units, ")"),
    analyte_label = factor(analyte_label),
    analyte = factor(analyte),
    agency = factor(agency, levels = c("USGS", "DWR"))
  )

#create converted DWR nitrate rows
converted_rows <- combo_plot %>%
  filter(agency == "DWR", analyte == "Dissolved Nitrate") %>%
  mutate(
    result = result / 4.3,          
    agency = "DWR_Nitrate/4.3"        
  )

combo_plot2 <- bind_rows(combo_plot, converted_rows) %>%
  filter(!(agency == "DWR" & analyte == "Dissolved Nitrate")) %>%
  mutate(
    agency = factor(agency, levels = c("USGS", "DWR", "DWR_Nitrate/4.3"))
  )


#build dot plot
vernalis <- ggplot(combo_plot2, aes(
  x = collection_date,
  y = result,
  shape = analyte_label,
  color = agency
)) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c(
      "USGS" = "darkorchid",
      "DWR"  = "dodgerblue",
      "DWR_Nitrate/4.3" = "dodgerblue"   
    )
  ) +
  scale_shape_manual(values = c(5, 4, 3, 1)) +
  labs(
    x = "Collection Date",
    y = "Result Values",
    color = "Agency",
    shape = "Analyte (Units)",
    title = "DWR & USGS Nitrogen Measurements at Vernalis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


vernalis

#Interactive plot
#make into an interactive plot for a quick and easy way to explore the data
vp <- ggplotly(
  vernalis,
  tooltip = c("collection_date", "result", "analyte_label", "agency", "sample_code")
)

#save to temporary HTML and open in browser
tmpfile <- tempfile(fileext = ".html")
saveWidget(vp, tmpfile, selfcontained = TRUE)
browseURL(tmpfile)

##############################
###Hood & Freeport comparison ************************************************
##############################

#Discrete 
#how many dates appear in both datasets
shared_dates2 <- intersect(dwr3$collection_date, usgs2$collection_date)

dwr_match2 <- dwr3 %>%
  filter(collection_date %in% shared_dates2) #35 records

usgs_match2 <- usgs2 %>%
  filter(collection_date %in% shared_dates2) #44 records 

#combine matched data onto one df
combo2 <- bind_rows(
  dwr_match2 %>%
    select(long_station_name, collection_date, analyte, result, units, agency),
  
  usgs_match2 %>%
    select(long_station_name, collection_date, analyte, result, units, agency)
)

#factor agency and analyte for plotting
combo2 <- combo2 %>%
  mutate(
    agency = factor(agency, levels = c("USGS", "DWR")),
    analyte = factor(analyte)
  )
##############
#discrete plot*****
##############

#prepare data
combo_plot3 <- combo2 %>%
  mutate(
    analyte_label = paste0(analyte, " (", units, ")"),
    analyte_label = factor(analyte_label),
    analyte = factor(analyte),
    agency = factor(agency, levels = c("USGS", "DWR"))
  )

combo2_check <- combo2 %>%
  group_by(collection_date) %>%
  summarise(
    agencies_present = n_distinct(agency),
    agency_list = paste(sort(unique(agency)), collapse = ", "),
    .groups = "drop"
  )

#create converted DWR nitrate rows
converted_rows2 <- combo_plot3 %>%
  filter(agency == "DWR", analyte == "Dissolved Nitrate") %>%
  mutate(
    result = result / 4.3,          
    agency = "DWR_Nitrate/4.3"        
  )
combo_plot4 <- bind_rows(combo_plot3, converted_rows2) %>%
  filter(!(agency == "DWR" & analyte == "Dissolved Nitrate")) %>%
  mutate(
    agency = factor(agency, levels = c("USGS", "DWR", "DWR_Nitrate/4.3"))
  )

#build dot plot
hood_freeport <- ggplot(combo_plot4, aes(
  x = collection_date,
  y = result,
  shape = analyte_label,
  color = agency
)) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c(
      "USGS" = "darkorchid",
      "DWR"  = "dodgerblue",
      "DWR_Nitrate/4.3" = "dodgerblue"   
    )
  ) +
  scale_shape_manual(values = c(5, 4, 3, 1)) +
  labs(
    x = "Collection Date",
    y = "Result Values",
    color = "Agency",
    shape = "Analyte (Units)",
    title = "DWR & USGS Nitrogen Measurements at Hood & Freeport"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


hood_freeport

#interactive plot
hfp <- ggplotly(
  hood_freeport,
  tooltip = c("collection_date", "result", "analyte_label", "agency", "sample_code")
)

#save to temporary HTML and open in browser
tmpfile <- tempfile(fileext = ".html")
saveWidget(hfp, tmpfile, selfcontained = TRUE)
browseURL(tmpfile)

#######################################################
#real time data (USGS real time nitrate + nitrite data)*************************
#######################################################

#how many dates appear in both datasets
shared_dates3 <- intersect(dwr3$collection_date, freeport2$collection_date)

dwr_match3 <- dwr3 %>%
  filter(collection_date %in% shared_dates3) #167

usgs_match3 <- freeport2 %>%
  filter(collection_date %in% shared_dates3) #94

#combine matched data onto one df
combo3 <- bind_rows(
  dwr_match3 %>%
    select(long_station_name, collection_date, analyte, result, units, agency),
  
  usgs_match3 %>%
    select(long_station_name, collection_date, analyte, result, units, agency)
)

#factor agency and analyte for plotting
combo3 <- combo3 %>%
  mutate(
    agency = factor(agency, levels = c("USGS", "DWR")),
    analyte = factor(analyte)
  )
###########################################
#realtime (freeport) & discrete (Hood) plot*****
###########################################
#prepare data
combo_plot5 <- combo3 %>%
  mutate(
    analyte_label = paste0(analyte, " (", units, ")"),
    analyte_label = factor(analyte_label),
    analyte = factor(analyte),
    agency = factor(agency, levels = c("USGS", "DWR"))
  )

combo3_check <- combo3 %>%
  group_by(collection_date) %>%
  summarise(
    agencies_present = n_distinct(agency),
    agency_list = paste(sort(unique(agency)), collapse = ", "),
    .groups = "drop"
  )

#create converted DWR nitrate rows
converted_rows3 <- combo_plot5 %>%
  filter(agency == "DWR", analyte == "Dissolved Nitrate") %>%
  mutate(
    result = result / 4.3,          
    agency = "DWR_Nitrate/4.3"        
  )
combo_plot6 <- bind_rows(combo_plot5, converted_rows2) %>%
  filter(!(agency == "DWR" & analyte == "Dissolved Nitrate")) %>%
  mutate(
    agency = factor(agency, levels = c("USGS", "DWR", "DWR_Nitrate/4.3"))
  )


#build point plot
hood_freeport2 <- ggplot(combo_plot6, aes(
  x = collection_date,
  y = result,
  shape = analyte_label,
  color = agency
)) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c(
      "USGS" = "darkorchid",
      "DWR"  = "dodgerblue",
      "DWR_Nitrate/4.3" = "dodgerblue"   
    )
  ) +
  scale_shape_manual(values = c(5, 4, 3, 1)) +
  labs(
    x = "Collection Date",
    y = "Result Values",
    color = "Agency",
    shape = "Analyte (Units)",
    title = "DWR & USGS Nitrogen measurements at Hood & Freeport (USGS realtime daily averages)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


hood_freeport2

#interactive plot
hfp2 <- ggplotly(
  hood_freeport2,
  tooltip = c("collection_date", "result", "analyte_label", "agency", "sample_code")
)

#save to temporary HTML and open in browser
tmpfile <- tempfile(fileext = ".html")
saveWidget(hfp2, tmpfile, selfcontained = TRUE)
browseURL(tmpfile)
#look at data affected by being reported out as NO3 rather than N, AKA non-detects vs reported concentration

