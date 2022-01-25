# Top commands ----
# https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
detachAllPackages <- function() {
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
        
}
detachAllPackages()

rm(list=ls(all=TRUE))

# FOLDERS - ADAPT THIS PATHWAY
setwd("/Users/jonathanlatner/Google Drive/SECCOPA/projects/distribution_contyp/")
# setwd("/Users/jonathanlatner/Documents/GitHub/distribution_contyp/")

data_files = "data_files/eu_silc/cross_sectional/"

# LIBRARY
library(tidyverse)
library(car)
library(data.table)
library(countrycode)

# Load data ----

df_eu_silc_0 <- readRDS(file = paste0(data_files,"df_eu_silc_xs.rds"))

# Filter ----

# drop observations with missing values of interest
# keep prime age workers
df_eu_silc_1 <- df_eu_silc_0 %>%
        mutate(age = year - birthy) %>%
        rename(weight_xc = weight_xc_base) %>%
        filter(weight_xc>0) %>%
        filter(age >= 25 & age < 55) %>%
        filter(!is.na(isced)) %>%
        filter(!is.na(sex)) %>%
        filter(!is.na(contyp)) 

# Employment status (keep only employed) ----

df_eu_silc_2 <- df_eu_silc_1

# Clean employment status
df_eu_silc_2$empst_1_new <- recode(df_eu_silc_2$empst_1, "1:2=1; 3:hi=0")  # not working = 0, employed = 1
df_eu_silc_2$empst_2_new <- recode(df_eu_silc_2$empst_2, "1:4=1; 5:hi=0")  # not working = 0, employed = 1

# Clean employment status
# Between 2009 and 2010, SILC data transitioned from one definition of employment status to another.
# Moving from PL030 to PL031.  The difference is that PL031 accounts for self-employed
# To maintain consistency, we include self-employed as employed, conditional being employed with an observable work contract
# Therefore, three distinct definitions of employment status: 1) panel == 2008; 2) 2008 < panel < 2011; 3) panel >= 2011
# It is crucial to use the indicator variable

df_empst_2009_2010 <- df_eu_silc_2 %>%
        filter(year == 2009 | year == 2010) %>%
        select(country,pid,year,matches("empst")) %>% 
        mutate(empst_1_new = ifelse(empst_1_f==-5, yes = empst_2_new, # if old indicator says to do so, then replace old with new variable 
                                    ifelse(empst_1_f == 1, yes = empst_1_new, no = NA)), # else use old variable
        ) %>% # next step below, if employment status is missing in BOTH variables, then drop observations 
        mutate(test = ifelse(is.na(empst_1_new) & is.na(empst_2_new), yes = 1, no = 0)) %>% 
        filter(test == 0) %>%
        select(-test)

with(df_empst_2009_2010,table(empst_1_new,empst_2_new, useNA = "ifany"))

# fix issue where observations are missing values 
df_empst_2009_2010 <- df_empst_2009_2010 %>%
        mutate(empst_1_new = ifelse(!is.na(empst_2_new)&is.na(empst_1_new), yes = empst_2_new, no = empst_1_new),
        ) 

with(df_empst_2009_2010,table(empst_1_new,empst_2_new, useNA = "ifany"))

# make new employment status variable for these years
df_empst_2009_2010 <- df_empst_2009_2010 %>%
        mutate(empst_2009_2010 = empst_2_new) %>%
        select(country,pid,year,empst_2009_2010)

df_eu_silc_2 <- merge(data.table(df_eu_silc_2),data.table(df_empst_2009_2010), all.x = TRUE)

rm(df_empst_2009_2010)

df_eu_silc_2 <- df_eu_silc_2 %>%
        mutate(empst = ifelse(year < 2009, yes = empst_1_new,
                              ifelse(year == 2009 | year == 2010, yes = empst_2009_2010,
                                     ifelse(year > 2010, yes = empst_2_new, no = NA))))

# keep only employed
df_eu_silc_2 <- df_eu_silc_2 %>%
        filter(empst==1)

# Select variables ----

df_eu_silc_3 <- df_eu_silc_2 %>%
        select(country,year,pid,sex,age,isced,empst,contyp,weight_xc)

# Code country/region ----

df_eu_silc_3$country[df_eu_silc_3$country == 'EL'] <- 'GR'
df_eu_silc_3$country[df_eu_silc_3$country == 'UK'] <- 'GB'

table(df_eu_silc_3$country)

df_eu_silc_3$country_name <- countrycode(df_eu_silc_3$country, 'genc2c', 'country.name')

country_name <- c("Germany", "Switzerland", "Luxembourg", "Belgium", "Austria", "Netherlands", "France", 
                  "United Kingdom", "Ireland", 
                  "Malta", "Greece", "Italy", "Cyprus", "Portugal", "Spain", 
                  "Romania", "Poland", "Croatia", "Hungary", "Czechia", "Bulgaria", "Slovenia", "Slovakia", "Lithuania", "Estonia", "Latvia", "Serbia", "Turkey",
                  "Sweden", "Denmark", "Norway", "Finland", "Iceland")
region <- c("Continental", "Continental", "Continental", "Continental", "Continental", "Continental", "Continental", 
            "Anglophone", "Anglophone", 
            "Southern", "Southern", "Southern", "Southern", "Southern", "Southern", 
            "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern",
            "Nordic", "Nordic", "Nordic", "Nordic", "Nordic")

geography <- cbind(country_name, region)

df_eu_silc_4 <- merge(df_eu_silc_3,geography) %>%
        arrange(country, year, pid)

rm(country_name, region, geography)

table(df_eu_silc_4$country_name, useNA = "ifany")
table(df_eu_silc_4$region, useNA = "ifany")
with(df_eu_silc_4,table(country_name,region, useNA = "ifany"))

# Save ----

saveRDS(df_eu_silc_4, file = paste0(data_files, "df_eu_silc_xs_clean.rds"))
