#####################################################
# Nitrate Methods Investigation - validated data
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

#make small function to remove laboratory duplicates
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
### Validate data for analysis
#####################################################

str(data_bound)

#reformat collection date field, result field

#Convert to POSIXct
data_bound$collection_date <- as.POSIXct(
  data_bound$collection_date,
  format = "%m/%d/%Y %H:%M")

data_bound1 <- mutate(data_bound, collection_date = as.Date(collection_date, format = '%m/%d/%Y'))

class(data_bound1$collection_date)

#remove rows with a blank result
data_bound1a <- filter(data_bound1, result != "")
#removes 14 rows

#remove rows with data not sampled (AKA N.S.)
data_bound1b <- filter(data_bound1a, result != "N.S.")
#removes 177 rows

#create breakdown of description comments left that may impact usability of data
descriptions <- data_bound1b %>%
  group_by(description) %>%
  summarize(
    first_used = min(collection_date, na.rm = TRUE),
    last_used  = max(collection_date, na.rm = TRUE),
    n_records  = n(),
    .groups = "drop"
  )

#export to more easily evaluate 
write.csv(descriptions, "descriptions for nitrate investigation data validation.csv", row.names = FALSE)

#evaluated descriptions and identified two description categories that invalidate nitrate + nitrite data (54 records affected, all within 2022),
#five description categories that invalidate TKN data (126 records affected, 2021-2024)
#three description categories that identify data as coming from TestAmerica (36 records, all within 2021)

#create breakdown of notes left that may impact usability of data
notes <- data_bound1b %>%
  group_by(notes) %>%
  summarize(
    first_used = min(collection_date, na.rm = TRUE),
    last_used  = max(collection_date, na.rm = TRUE),
    n_records  = n(),
    .groups = "drop"
  )

#export to more easily evaluate 
write.csv(notes, "notes for nitrate investigation data validation.csv", row.names = FALSE)

#evaluated notes and identified two description categories that invalidate nitrate + nitrite data (54 records affected, all within 2022),
#one note category invalidated nitrate data (holding time violation) (6 records affected, all within 2023),
#two description categories that identify data as coming from TestAmerica (3 records, all within 2020)


#remove data that I have evaluated as invalid for this investigation's purposes

#remove N+N dup failure at station OSJ
data_bound1c <- filter(data_bound1b, sample_code != "CF0422B0520" &
                         sample_code != "CF0422B0521" &
                         sample_code != "CF0422B0522")
#removed 32 rows, correct

#remove N+N dup failure at station TWA
data_bound1d <- filter(data_bound1c, sample_code != "CF0922B0940" &
                         sample_code != "CF0922B0939" &
                         sample_code != "CF0922B0938"&
                         sample_code != "CF0922B0937"&
                         sample_code != "CF0922B0936")
#removed 22 rows, correct


#remove TKN dup failure at station FAL
data_bound1e <- filter(data_bound1d, sample_code != "CF0822B0915" &
                         sample_code != "CF0822B0914" &
                         sample_code != "CF0822B0913"&
                         sample_code != "CF0822B0912"&
                         sample_code != "CF0822B0911")
#removed 50 rows, correct

#remove TKN dup failure associated with parent sample CF1122B1229
data_bound1f <- filter(data_bound1e, sample_code != "CF1122B1231" &
                         sample_code != "CF1122B1230" &
                         sample_code != "CF1122B1229"&
                         sample_code != "CF1122B1228")
#removed 39 rows, correct

#remove TKN dup failure associated with station RSCC
data_bound1g <- filter(data_bound1f, sample_code != "CF0124B0023" &
                         sample_code != "CF0124B0019")
#removed 22 rows, correct

#remove TKN dup failure associated with parent sample CF1021B0897
data_bound1h <- filter(data_bound1g, sample_code != "CF1021B0901" &
                         sample_code != "CF1021B0897")
#removed 15 rows, correct

#remove Nitrate holding time violation flagged data
data_bound1i <- filter(data_bound1h, sample_code != "S1223B0456" &
                         sample_code != "OR1223B0162"&
                         sample_code != "OR1223B0161"&
                         sample_code != "OR1223B0160"&
                         sample_code != "OR1223B0159"&
                         sample_code != "OR1223B0158")
#removed 36 rows (6 rows for these samples with nitrate, plus the other data for these samples), correct


#evaluated all data identified as subbed to TestAmerica (subcontractor), none 
#are nitrate data (N+N and other nutrients only), so no concerns over unit inconsistency

#remove irrelevant analytes
data_bound2 <- filter(data_bound1i, analyte == "Dissolved Nitrate" | analyte == "Dissolved Nitrate + Nitrite" | 
                        analyte == "Total Kjeldahl Nitrogen" |
                        analyte == "Field Specific Conductance"
)

#clean up results reported as <MDL for easier analysis
data_bound2$result[data_bound2$result=="< MDL, MDL = 0.010"]<-"<0.010"

data_bound2$result[data_bound2$result=="< MDL, MDL = 0.10"]<-"<0.10"


#clean up results reported as "<RL" for easier analysis
data_bound2$result[data_bound2$result=="< R.L. 0.1"]<-"<0.1"


#remove the two rows with result = "<0" or "0"
data_bound3 <- filter(data_bound2, result != "<0" & result != "0")

#removed 7 rows, correct

#look for any duplicate data
r = data_bound3 %>% 
  count(sample_code, collection_date, analyte) %>% 
  filter(n > 1)
#868 duplicate sets, spans about 10 years of data. looked closer and appears to be across dsets, so likely
#due to pull ins from across projects in WDL so duplicates in the pulls
#some data like sample C0115B0058 appear in both dset3 & dset8 and both normal samples.
#fix after splitting data based on max and non-detects for easier clean up

#make results numeric class, remove "<" to assume result = RL (over estimate)
data_bound_max <- mutate(data_bound3, result = as.numeric(result))

#other dataframe to remove non-detects fully (reported as <RL) 
data_bound_nd <- data_bound3 %>%
  filter(!grepl("<", result, ignore.case = TRUE))

data_bound_nd <- mutate(data_bound_nd, result = as.numeric(result))

#remove data within data_bound_nd where numerical result = RL
data_bound_nd1 <- filter(data_bound_nd, result > rpt_limit)
#removed 937 rows

#pivot wider to group by sample ID, pick first instance of sample ID present to remove these dup values

data_bound_max_wide <- data_bound_max %>% 
  pivot_wider(id_cols = c(sample_code, collection_date, station_number),
              names_from = analyte, values_from = c(rpt_limit, result, method),
              values_fill = NA, values_fn = first)


data_bound_max_wide<- clean_names(data_bound_max_wide)


r1 = data_bound_max_wide %>% 
  count(sample_code, collection_date) %>% 
  filter(n > 1)
#no more duplicates

#remove samples without both dissolved nitrate & dissolved nitrate + nitrite
data_bound_max_wide1 <- data_bound_max_wide %>%
  drop_na(result_dissolved_nitrate) %>%
  drop_na(result_dissolved_nitrate_nitrite)

#adjust nitrate results and RL, all methods (EPA 300.0 and EPA 300.0 Mod) are reported with data "as NO3"
data_bound_max_wide1_adj <- data_bound_max_wide1 %>%
  mutate(nitrate_as_N = result_dissolved_nitrate / 4.3,
         nitrate_RL_as_N = rpt_limit_dissolved_nitrate / 4.3)


#pivot wider to group by sample ID, pick first instance of sample ID present to remove these dup values
data_bound_nd1_wide <- data_bound_nd1 %>% 
  pivot_wider(id_cols = c(sample_code, collection_date, station_number),
              names_from = analyte, values_from = c(rpt_limit, result, method),
              values_fill = NA, values_fn = first)

data_bound_nd1_wide<- clean_names(data_bound_nd1_wide)

r2 = data_bound_nd1_wide %>% 
  count(sample_code, collection_date) %>% 
  filter(n > 1)
#no more duplicates

#remove samples without both dissolved nitrate & dissolved nitrate + nitrite
data_bound_nd1_wide1 <- data_bound_nd1_wide %>%
  drop_na(result_dissolved_nitrate) %>%
  drop_na(result_dissolved_nitrate_nitrite)

#adjust nitrate results and RL, all methods (EPA 300.0 and EPA 300.0 Mod) are reported with data "as NO3"
data_bound_nd1_wide1_adj <- data_bound_nd1_wide1 %>%
  mutate(nitrate_as_N = result_dissolved_nitrate / 4.3,
         nitrate_RL_as_N = rpt_limit_dissolved_nitrate / 4.3)


####################################################################################
### Analyze the data
####################################################################################

npair_max <- data_bound_max_wide1_adj %>%
  mutate(
    detection_condition_diss_nitrate_nitrite = 
      if_else(result_dissolved_nitrate_nitrite > rpt_limit_dissolved_nitrate_nitrite, "Detected",
              "Not Detected"))


npair_nd <- data_bound_nd1_wide1_adj %>%
  mutate(
    detection_condition_diss_nitrate_nitrite = 
      if_else(result_dissolved_nitrate_nitrite > rpt_limit_dissolved_nitrate_nitrite, "Detected",
              "Not Detected"))
#check good, no data should be appearing here since non detects removed previously


npair_max1<- npair_max %>%
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


npair_nd1<- npair_nd %>%
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


#summarize data (max results)
nitrate_as_n_max_summary <- npair_max1 %>%
  group_by(less_flag_new) %>%
  summarize(
    n_records  = n(),
    .groups = "drop"
  )

#less_flag_new               n_records
#<chr>                           <int>
#1 less                             2229
#2 less (using RL for N+N)             5
#3 not less                         3368
#4 not less (using RL for N+N)        15
 

#With the adjusted nitrate values (to report as N) 
#now 3383 of 5617 samples have a higher value for nitrate than for nitrate + nitrite, 
#so only 2234 show that expected relationship.(~40%)

nitrate_as_no3_max_summary <- npair_max1 %>%
  group_by(less_flag_orig) %>%
  summarize(
    n_records  = n(),
    .groups = "drop"
  )

#less_flag_orig              n_records
#<chr>                           <int>
#1 less                                9
#2 not less                         5588
#3 not less (using RL for N+N)        20
 

#With the original nitrate values when compared to their nitrate + nitrite counterparts, 
#5608 out of 5617 samples had a higher value for nitrate than for nitrate + nitrite
#only 9 samples show expected relationship (~0.02%)


#summarize data (nd's removed)
nitrate_as_n_nd_summary <- npair_nd1 %>%
  group_by(less_flag_new) %>%
  summarize(
    n_records  = n(),
    .groups = "drop"
  )

#less_flag_new n_records
#<chr>             <int>
#1 less               2106
#2 not less           3338

#With the adjusted nitrate values (to report as N) 
#now 3338 of 5444 samples have a higher value for nitrate than for nitrate + nitrite, 
#so only 2106 show that expected relationship.(~39%)

nitrate_as_no3_nd_summary<- npair_nd1 %>%
  group_by(less_flag_orig) %>%
  summarize(
    n_records  = n(),
    .groups = "drop"
  )

#less_flag_orig n_records
#<chr>              <int>
#1 less                   8
#2 not less            5436

#With the original nitrate values when compared to their nitrate + nitrite counterparts, 
#5436 out of 5444 samples had a higher value for nitrate than for nitrate + nitrite
#only 8 samples show expected relationship (~0.01%)


#parse out these specific data with adjusted nitrate values (to report as N) that are still higher into new df
#use df with nds removed

nitrate_as_n_higher <- filter(npair_nd1, less_flag_new == "not less")
#3338, matches with above

#summarize
nitrate_as_n_higher_summary <- nitrate_as_n_higher %>%
  group_by(method_dissolved_nitrate) %>%
  summarize(
    first_used = min(collection_date, na.rm = TRUE),
    last_used  = max(collection_date, na.rm = TRUE),
    n_records  = n(),
    .groups = "drop"
  )

#method_dissolved_nitrate first_used last_used  n_records
#<chr>                    <date>     <date>         <int>
#1 EPA 300.0 28d Hold [1]*  2010-01-04 2020-06-29      3064
#2 EPA 300.0 [1]*           2020-07-21 2024-06-26       274


#summarize, comparing overall method prevalence for nitrate, 
nitrate_summary <- npair_nd1 %>%
  group_by(method_dissolved_nitrate) %>%
  summarize(
    first_used = min(collection_date, na.rm = TRUE),
    last_used  = max(collection_date, na.rm = TRUE),
    n_records  = n(),
    .groups = "drop"
  )

#method_dissolved_nitrate first_used last_used  n_records
#<chr>                    <date>     <date>         <int>
#1 EPA 300.0 28d Hold [1]*  2010-01-04 2020-07-15      4659
#2 EPA 300.0 [1]*           2020-07-20 2024-06-26       785

#go back to before samples were removed due to no N+N data

#remove samples without dissolved nitrate 
df <- data_bound_max_wide %>%
  drop_na(result_dissolved_nitrate)

nitrate_summary2 <- df %>%
  group_by(method_dissolved_nitrate) %>%
  summarize(
    first_used = min(collection_date, na.rm = TRUE),
    last_used  = max(collection_date, na.rm = TRUE),
    n_records  = n(),
    .groups = "drop"
  )

#method_dissolved_nitrate first_used last_used  n_records
#<chr>                    <date>     <date>         <int>
#1 EPA 300.0 28d Hold [1]*  2010-01-04 2020-07-15      6003
#2 EPA 300.0 [1]*           2011-11-15 2024-06-26       893


#remove samples without both dissolved nitrate & dissolved nitrate + nitrite
nitrate_summary3 <- data_bound_max_wide1_adj  %>%
  group_by(method_dissolved_nitrate) %>%
  summarize(
    first_used = min(collection_date, na.rm = TRUE),
    last_used  = max(collection_date, na.rm = TRUE),
    n_records  = n(),
    .groups = "drop"
  )

#method_dissolved_nitrate first_used last_used  n_records
#<chr>                    <date>     <date>         <int>
#1 EPA 300.0 28d Hold [1]*  2010-01-04 2020-07-15      4827
#2 EPA 300.0 [1]*           2011-11-15 2024-06-26       790

#14% of samples analyzed for both Nitrate & N+N overall were run by 300.0 (unmodified)
#13% of samples analyzed for Nitrate were run by EPA 300.0 (unmodified). 
#For those with a higher adj nitrate as N value, where both Nitrate & N+N was run, 
  # 8% of samples were run by EPA 300.0 (unmodified)

summary(npair_nd1$nitrate_as_N)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.02791 0.16279 0.32558 0.48952 0.62791 6.16279 

summary(nitrate_as_n_higher$nitrate_as_N)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.04326 0.18605 0.41861 0.54817 0.69767 5.34884  

##ranges are similar from overall as to when nitrate value is higher

#visualize over time
ggplot(npair_nd1, aes(x = collection_date)) +
  geom_freqpoly()+
  labs(
    title = "Frequency of total paired sample submissions",
    x = "Collection Date",
    y = "Sample Count"
  )

ggplot(nitrate_as_n_higher, aes(x = collection_date)) +
  geom_freqpoly()+
  labs(
    title = "Frequency of total paired sample submissions, nitrate as n higher",
    x = "Collection Date",
    y = "Sample Count"
  )

#visualize over concentration
ggplot(npair_nd1, aes(x = nitrate_as_N)) +
  geom_freqpoly()+
  labs(
    title = "Frequency of total paired samples by concentration",
    x = "Nitrate as N",
    y = "Sample Count"
  )

ggplot(nitrate_as_n_higher, aes(x = nitrate_as_N)) +
  geom_freqpoly()+
  labs(
    title = "Frequency of paired sample submissions by concentration where nitrate as n higher",
    x = "Nitrate as N",
    y = "Sample Count"
  )


# plot nitrate as n, still higher
ggplot(nitrate_as_n_higher, aes(x = collection_date)) +
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
####################################################################################
### Analyze the data - nitrate RLs
####################################################################################


#summarize nitrate RLs, 
nitrate_summary_RL <- nitrate_as_n_higher %>%
  group_by(nitrate_RL_as_N) %>%
  summarize(
    first_used = min(collection_date, na.rm = TRUE),
    last_used  = max(collection_date, na.rm = TRUE),
    n_records  = n(),
    .groups = "drop"
  )


#nitrate_RL_as_N first_used last_used  n_records
#<dbl> <date>     <date>         <int>
#1          0.0233 2010-01-04 2024-06-26      3051
#2          0.0465 2012-03-05 2018-12-17         8
#3          0.0581 2010-10-05 2015-02-17        12
#4          0.0698 2013-08-19 2023-11-21         7
#5          0.0774 2010-01-05 2010-09-07        18
#6          0.0872 2023-04-17 2023-04-17         1
#7          0.116  2010-01-05 2024-03-19       156
#8          0.174  2022-11-14 2024-02-20         9
#9          0.233  2013-03-04 2024-04-17        60
#10         0.349  2014-05-05 2018-02-20         4
#11         0.465  2011-10-06 2015-10-21         6
#12         0.581  2014-03-19 2018-06-05         5
#13         1.16   2018-08-20 2018-08-20         1



################################################################################
### Total nitrogen via tkn + (nitrate + nitrite) analysis
################################################################################

#remove samples with no TKN data
#use df with non-detects removed
total_n <- filter(data_bound_nd1_wide1_adj, result_total_kjeldahl_nitrogen != "")
#5278 samples


#remove tkn data where numerical result = RL
total_n1 <- filter(total_n, result_total_kjeldahl_nitrogen > rpt_limit_total_kjeldahl_nitrogen)
#no removals, all above RL


#calculate total nitrogen, create comparison (tn_vs_nox) column & difference column
total_n2 <- total_n1 %>%
  mutate(
    total_nitrogen = result_total_kjeldahl_nitrogen +
      result_dissolved_nitrate_nitrite,
    tn_vs_no3 = case_when(
      total_nitrogen > nitrate_as_N ~ "greater",
      total_nitrogen < nitrate_as_N ~ "less",
      total_nitrogen == nitrate_as_N ~ "equal",
      TRUE ~ NA_character_
    ),
    difference = total_nitrogen - nitrate_as_N
  )
#summary
total_n2 %>%
  count(tn_vs_no3)
# greater      5231
# less           47
# equal           0

# so with the adjusted Nitrate as N results and then calculating total nitrogen (tkn +(n03 +n02))
# majority (5231) of samples had a calculated total nitrogen value that was greater than the dissolved nitrate value
# 47 samples had a calculated total nitrogen value that was less than the dissolved nitrate value
# 0 samples had equal values


#export to Git
write.csv(total_n2, "total_nitrogen.csv", row.names = FALSE)
