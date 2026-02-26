#####################################################
# Nitrate Methods Investigation
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


#look at data affected by being reported out as NO3 rather than N, AKA non-detects vs reported concentration


################################################################################
### Total nitrogen via tkn analysis
################################################################################

                ################
                ###data prep 
                ################

#group samples together and put in chronological order
df1 <- data_bound_nitrate %>%
  arrange(collection_date) %>%
  group_by(short_station_name, station_number, sample_code, collection_date)

#change any result values that are below reporting limit to NA. then remove any NA records.
df2 <- df1 %>%
  mutate(result = if_else(result < rpt_limit, NA_real_, result)) %>%
  filter(!is.na(result))

#retain samples that use tkn and are "normal samples" 
tkn <- df2 %>%
  ungroup() %>%
  filter(
    any(analyte == "Total Kjeldahl Nitrogen" &
          sample_type == "Normal Sample"),
    .by = c(station_number, collection_date)
  ) %>%
  filter(sample_type == "Normal Sample")

#specify required analytes
required_analytes <- c(
  "Dissolved Nitrate + Nitrite",
  "Total Kjeldahl Nitrogen",
  "Dissolved Nitrate"
)



#new df that retains just the required analytes for this analysis
tkn2 <- tkn %>%
  group_by(short_station_name, station_number, sample_code, collection_date) %>% #grouping by sample, so three analytes/rows per sample
  filter(all(required_analytes %in% analyte)) %>% 
  ungroup() %>%
  filter(analyte %in% required_analytes) 

#double check the numbers, should be 3 analytes per sample. anything extra needs to be reviewed
tkn2 %>%
  count(short_station_name, station_number, sample_code, collection_date) %>%
  filter(n != 3) 
#129 records

#create list of sample specific analyte duplicates 
dupes <- tkn2 %>%
  group_by(short_station_name, station_number, sample_code, collection_date, analyte) %>%
  filter(n() > 1) 
#363 records

#create df of duplicates with == result values
matching_dupes <- dupes %>%
  group_by(short_station_name, station_number, sample_code, collection_date, analyte) %>%
  filter(n_distinct(result) == 1)
#283 records 

#examine the odd pairing in matching samples
unpaired <- matching_dupes %>%
  count(sample_code) %>%        
  filter(n %% 2 == 1)           
#sample CR0712B0939 has three dissolved nitrate records, all with the same value. no problem.

#create df of duplicates with not exactly matching result values
nonmatching_dupes <- dupes %>%
  group_by(short_station_name, station_number, sample_code, collection_date, analyte) %>%
  filter(n_distinct(result) > 1)
#80 records -___-

#create sample keys for nonmatching duplicates
non_keys <- nonmatching_dupes %>%
  distinct(short_station_name, station_number, sample_code, collection_date)

#remove samples associated with nonmatching dupes
tkn3 <- tkn2 %>%
  anti_join(non_keys,
            by = c("short_station_name", "station_number", "sample_code", "collection_date"))

#keep one record per analyte dupe of the dupes that match
matching_dupes2 <- matching_dupes %>%
  group_by(short_station_name, station_number, sample_code, collection_date, analyte) %>%
  slice(1) %>%   
  ungroup()

#remove original dupe records
tkn_no_dupes <- tkn3 %>%
  anti_join(dupes,
            by = c("short_station_name", "station_number", "sample_code", "collection_date", "analyte"))

#add back in the reduced matching dupes
tkn4 <- bind_rows(tkn_no_dupes, matching_dupes2) %>%
  arrange(short_station_name, station_number, collection_date, analyte)

#confirm only 3 analytes per sample
tkn4 %>%
  count(short_station_name, station_number, sample_code, collection_date) %>%
  filter(n != 3) #two samples that had both matching and non matching dupes slipped through the cracks

#retain records with only the 3 analytes per sample.
tkn5 <- tkn4 %>%
  group_by(sample_code, collection_date) %>%
  filter(n() == 3) %>%
  ungroup()

###########
# the process above omits nonmatching duplicates(40) and their associated samples from this analysis.
# I did this because I am short on time and the number of samples was negligible (about 300 records ~ 100 samples) :')
# so roughly 5,463 samples to run the total nitrogen using tkn analysis.
###########

#data collected under EPA method 300 will be divided by a conversion factor of 4.3
#this new value will be placed in a new column called result_convert and data outside of the 300 method
#will retain their result values and be transferred to result_convert as is.
tkn5 <- tkn5 %>%
  mutate(
    result_convert = case_when(
      method %in% c("EPA 300.0 [1]*", "EPA 300.0 28d Hold [1]*") ~ result / 4.3,
      TRUE ~ result
    ),
    result_convert = round(result_convert, 3) #three decimal places
  ) %>%
  relocate(result_convert, .after = result)


#make a wide version
tkn_wide <- tkn5 %>%
  select(
    station_number,
    short_station_name,
    collection_date,
    sample_code,
    analyte,
    result_convert
  ) %>%
  pivot_wider(
    names_from = analyte,
    values_from = result_convert
  )
#clean names
tkn_wide <- tkn_wide %>%
  janitor::clean_names()

#calculate total nitrogen, create comparison (tn_vs_nox) column & difference column
tkn_wide <- tkn_wide %>%
  mutate(
    total_nitrogen = dissolved_total_kjeldahl_nitrogen +
      dissolved_nitrate_nitrite,
    tn_vs_nox = case_when(
      total_nitrogen > dissolved_nitrate ~ "greater",
      total_nitrogen < dissolved_nitrate ~ "less",
      total_nitrogen == dissolved_nitrate ~ "equal",
      TRUE ~ NA_character_
    ),
    difference = total_nitrogen - dissolved_nitrate
  )
#summary
tkn_wide %>%
  count(tn_vs_nox)
# greater      5411
# less           50
# equal           2

# so after dividing relevant samples by a 4.3 conversion factor and then calculating total nitrogen (tkn +(n03 +n02))
# majority (5411) of samples had a calculated total nitrogen value that was greater than the dissolved nitrate value
# 50 samples had a calculated total nitrogen value that was less than the dissolved nitrate value
# 2 samples had equal values


#export to share 
write.csv(tkn_wide, "tkn_wide.csv", row.names = FALSE)
