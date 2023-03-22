# downloading zip code level SES and demographic data from censuse ACS survey

# nice tutorial here: https://walker-data.com/umich-workshop-2022/acs-data/#21
# and ACS webpage: https://www.census.gov/data.html

# packages
library(tidycensus)
library(tidyverse)
library(here)

# API key
# 5bb5410f81a6290236f006904648e30c2f75d7aa
census_api_key("5bb5410f81a6290236f006904648e30c2f75d7aa", install = TRUE)

# what variables do I want

# list of all data profiles
# Data Profiles have the most frequently requested social, economic, housing, and demographic data.
allvars <- load_variables(2017, "acs5/profile")
view(allvars)

# creating vector of variables I want
want_vars <- c("DP02_0001","DP02_0001P", # total households and percent total households
               # householder living alone and ≥65 -- interesting for eye trauma
               "DP02_0011","DP02_0011P","DP02_0012","DP02_0012P",
               # estimates for children 3+ yo in school -- SES, can look at more granular ones (kinder,elementary, HS, college)
               "DP02_0052","DP02_0052P",
               # educational achievement (pop over 25 and wiwth 9th, 12th, HS, college)
               "DP02_0058","DP02_0058P","DP02_0059","DP02_0059P","DP02_0060","DP02_0060P","DP02_0061","DP02_0061P",
               "DP02_0062","DP02_0062P","DP02_0063","DP02_0063P","DP02_0064","DP02_0064P","DP02_0065","DP02_0065P",
               "DP02_0066","DP02_0066P","DP02_0067","DP02_0063P",
               # place of birth (total, native, born in US, born outside US)
               "DP02_0086","DP02_0086P","DP02_0087","DP02_0087P","DP02_0088","DP02_0088P","DP02_0089","DP02_0089P",
               "DP02_0090","DP02_0090P","DP02_0091","DP02_0091P","DP02_0092","DP02_0092P",
               # language spoken at home -- just spanish for now but there are others to look at
               "DP02_0110","DP02_0110P","DP02_0111","DP02_0111P","DP02_0112","DP02_0112P","DP02_0114","DP02_0114P",
               # computer and internet access -- telemedicine?
               "DP02_0150","DP02_0150P","DP02_0151","DP02_0151P","DP02_0152","DP02_0152P",
               # employment status -- looking at unemployed, armed forces (more K ulcers or trauma?)
               "DP03_0005","DP03_0005P","DP03_0006","DP03_0006P",
               # occupation - natural resources, construction, maintenance, agriculture
               "DP03_0030","DP03_0030P","DP03_0033","DP03_0033P","DP03_0034","DP03_0034P",
               # mean and median household income, cash puclic assistance income, and food stamps/SNAP
               "DP03_0062","DP03_0062P","DP03_0063","DP03_0063P","DP03_0072","DP03_0072P","DP03_0074","DP03_0074P",
               # mean and median family income
               "DP03_0086","DP03_0086P","DP03_0087","DP03_0087P",
               # poverty level
               "DP03_0119","DP03_0119P",
               # heating fuel type
               "DP04_0063","DP04_0063P","DP04_0064","DP04_0064P","DP04_0065","DP04_0065P","DP04_0066","DP04_0066P",
               "DP04_0067","DP04_0067P","DP04_0068","DP04_0068P","DP04_0069","DP04_0069P","DP04_0070","DP04_0070P",
               "DP04_0071","DP04_0071P")
               # also kitchen, plumbing and crowding per room available (Dp04_0073-0079)
               # also race makrs but figured I can omit these since HCAI data has race markers

want_vars

# getting ACS 5 year zip code level data
acs2017 <- get_acs(geography = "zcta",
        variables = want_vars,
        # table = ,
        year = 2017,
        output = "tidy", # "wide"
        state = "california",
        # zcta = c(), # if I dont specify this then I get all ZCTAs for CA
        geometry = FALSE, # if TRUE then you will get tibble with SF geometry
        key = "5bb5410f81a6290236f006904648e30c2f75d7aa",
        survey = "acs5")

acs2017
write_csv(acs2017, here("data/acs/acs2017.csv"))

# start here next time and figure out why some variables have NA data

# adding the labels from all vars so I know what the numbers mean
acs2017_labs <- left_join(acs2017, allvars, by = c("variable"="name")) %>% 
  select(-concept, -moe, -variable) %>% 
  # filtering just the proportions
  filter(grepl("Percent",label)) %>% 
  # pivoting so each row is a zip code 
  pivot_wider(names_from = label, values_from = estimate) %>% 
  # getting rid of the total households
  select(-"Percent Estimate!!HOUSEHOLDS BY TYPE!!Total households",
         -"Percent Estimate!!EDUCATIONAL ATTAINMENT!!Population 25 years and over",
         -"Percent Estimate!!PLACE OF BIRTH!!Total population",
         -"Percent Estimate!!COMPUTERS AND INTERNET USE!!Total households")

acs2017_labs
view(acs2017_labs)

# some of these have NA values.. esp poverty level and education
acs2017 %>% filter(is.na(estimate)) %>% select(-GEOID, -NAME) %>% unique()
acs2017_labs %>% filter(is.na(estimate)) %>% select(-GEOID, -NAME) %>% unique()

# cleaning up column names
names(acs2017_labs) <- sub('[^.]*\\!', "", colnames(acs2017_labs))  
view(acs2017_labs)

acs2017_labs %>% 
  # removing columns with all NA values
  select(-"Percent high school graduate or higher",
         -"Percent bachelor's degree or higher",
         -"Median household income (dollars)",
         -"Mean household income (dollars)",
         )
  rename(alone_65plus="65 years and over")



sub('Percent Estimate!!', "", colnames(acs2017_labs))  
sub('[^.]*\\!', "", colnames(acs2017_labs))  


acs2017_labs
view(acs2017_labs)


# list of all variables 
allvars <- load_variables(2017, "acs5")
as_tibble(unique(allvars$concept)) %>% view(.)

# selecting the variables I want
want_vars <- c("AGGREGATE CONTRACT RENT (DOLLARS)", 
               "AGGREGATE EARNINGS IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS) FOR HOUSEHOLDS",
               "AGGREGATE FAMILY INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)",
               "AGGREGATE GROSS RENT (DOLLARS)",
               "AGGREGATE HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)",
               "AGGREGATE INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)",
               "AGGREGATE NUMBER OF ROOMS",
               "ALLOCATION OF ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER",
               "ALLOCATION OF CITIZENSHIP STATUS",
               "ALLOCATION OF CLASS OF WORKER FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER",
               "ALLOCATION OF CLASS OF WORKER FOR THE FULL-TIME, YEAR-ROUND CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER",
               "ALLOCATION OF EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER",
               "ALLOCATION OF FOOD STAMPS/SNAP RECEIPT",
               "ALLOCATION OF GRADE ENROLLED FOR THE POPULATION 3 YEARS AND OVER ENROLLED IN SCHOOL",
               
               
               
               
               "TOTAL POPULATION","RACE","NATIVITY AND CITIZENSHIP STATUS IN THE UNITED STATES",
               "PLACE OF BIRTH BY NATIVITY AND CITIZENSHIP STATUS",
               "MEDIAN INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS) BY PLACE OF BIRTH IN THE UNITED STATES",
               "GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY AGE FOR CURRENT RESIDENCE IN THE UNITED STATES",
               "MOVERS BETWEEN REGIONS IN THE UNITED STATES",)
allvars %>% 
  filter(concept == "RACE" & 
           concept == "TOTAL POPULATION" &
           concept == "NATIVITY AND CITIZENSHIP STATUS IN THE UNITED STATES" &
           concept == "PLACE OF BIRTH BY NATIVITY AND CITIZENSHIP STATUS" &
           concept %in% 
         )
  

view(vars)

# 

# getting ACS 5 year zip code level data
get_acs(geography = "zcta",
        variables = "B19013_001",
        # table = ,
        year = 2017,
        output = "tidy", # "wide"
        state = "california",
        # zcta = c(), # if I dont specify this then I get all ZCTAs for CA
        geometry = FALSE, # if TRUE then you will get tibble with SF geometry
        key = "5bb5410f81a6290236f006904648e30c2f75d7aa",
        survey = "acs5")
