# HCAI data explotation and validation

library(tidyverse)
library(here)
library(skimr)

# loading data - need to be on pulse secure VPN for these to open
data.ed <- read.csv("/Volumes/jkeenan1_OSHPD_data_shared/EDD/dm_ED2020.csv")
data.ed
skim(data.ed) # 278501 obsservations/ED visits for 2020

data.pdd <- read.csv("/Volumes/jkeenan1_OSHPD_data_shared/PDD/dm_PDD2020.csv")
data.pdd
skim(data.pdd) # 175481 observations for 2020

# joining the two together
data <- full_join(data.ed, data.pdd)
skim(data) # 453982 = 278501 + 175481


# was the data cut correctly? i.e., are there other ICD codes than the ones I requested
# I did not request stroke so if the data is uncut there should be visits with stroke (I63)
data %>% 
  filter(grepl("I63",diag_p)) # ~9000 stroke visits

# how about appendicitis (K35)?
data %>% 
  filter(grepl("K35", diag_p)) #~375 cases of appendicitis in 2020 seems low...


# seeing how codes compare to: 
# https://data.chhs.ca.gov/dataset/hospital-emergency-department-characteristics-by-facility-pivot-profile/resource/34bdefc5-8eab-462a-a717-46fbe03e031b

# this says 10 million ED visits in 2020, which is a big discrepancy compared to 278,501

# comparing by sex
data %>% 
  group_by(sex) %>% 
  summarise(n=n())

# A tibble: 4 × 2
# sex        n
# <chr>  <int>
#   1 -          1
# 2 F     223624
# 3 M     230335
# 4 U         22

# reports from CHHS are 
# Male = 4,716,510
# Female = 5,306,443


# lookig specifically at eye related visits
data %>% 
  filter(grepl("H",diag_p,ignore.case = T)) %>% 
  as_tibble() %>% 
  ggplot(data=., aes(x=serv_dt)) +
  geom_histogram(stat="count")

#### repeating for new EDD as of March 22 23 ####data
data.ed2 <- read.csv("/Volumes/jkeenan1_OSHPD_data_shared/EDD2_Mar_22_23/dm_ED2020.csv")
skim(data.ed2)
dim(data.ed2) #10 million visits, much better




