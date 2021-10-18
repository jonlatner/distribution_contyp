<<<<<<< HEAD
# Top commands ----
=======
# Top commands --------------------------------------------------------------
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e
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
<<<<<<< HEAD
setwd("/Users/jonathanlatner/Google Drive/SECCOPA/projects/distribution_contyp/")
# setwd("/Users/jonathanlatner/Documents/GitHub/distribution_contyp/")
=======
setwd("/Users/jonathanlatner/Documents/GitHub/distribution_contyp/")
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e

data_files = "data_files/eu_silc/"

# LIBRARY
library(tidyverse)
library(car) #recode
<<<<<<< HEAD
library(countrycode)
library(beepr)

# Load data ----

df_eu_silc <- readRDS(file = paste0(data_files,"df_eu_silc_filter.rds"))

# Clean data ----
=======
library(zoo) #na.locf
library(dummies)

# Load data --------------------------------------------------------------

df_eu_silc <- readRDS(file = paste0(data_files,"df_eu_silc_filter.rds"))

table(df_eu_silc$year, useNA = "ifany")

# Clean data --------------------------------------------------------------
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e

df_eu_silc$country_name <- countrycode(df_eu_silc$country, 'genc2c', 'country.name')

df_eu_silc <- df_eu_silc %>%
<<<<<<< HEAD
        rename(weight_long = weight_long_4) %>%
=======
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e
        mutate(ftc = contyp-1) %>%
        mutate(ftc = ifelse(empst == 0, yes = 0, no = ftc)) # if unemployed, then contract type == 0

# age
df_eu_silc$age_cat <- recode(df_eu_silc$age, "25:34 = 1; 35:44=2; 45:54=3")

<<<<<<< HEAD
# Count number of FTC and Employed ----
# control (i.e. ftc_num_c, c for control) for the fact that an individual with FTC in consecutive periods did or did not change jobs
# if an individual did have FTC in consecutive periods (ftc==1 & lag(ftc,1)==1), but did not change jobs (job_change == 2), then they do not have a new FTC
# ftc_num_sum_c is the sum of the number of unique ftc contracts
=======
# Count number of FTC and Employed --------------------------------------------------------------
# control (i.e. ftc_num_c, c for control) for the fact that an individual with FTC in consecutive periods did or did not change jobs
# if an individual did have FTC in consecutive periods (ftc==1 & lag(ftc,1)==1), but did not change jobs (job_change == 2), then they do not have a new FTC

# View(select(df_eu_silc,panel,country,pid,ftc,empst,job_change,matches("ftc")))
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e

df_eu_silc <- df_eu_silc %>%
        group_by(panel, country, pid) %>%
        mutate(ftc_num = ifelse(ftc==1 & empst == 1, yes = 1, no = NA),
               ftc_num_sum = sum(ftc_num, na.rm = TRUE),
               ftc_num_c = ifelse(row_number() > 1 & ftc==1 & lag(ftc,1)==1 & job_change == 2, yes = 0, no = ftc_num),
               ftc_num_sum_c = sum(ftc_num_c, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        arrange(panel, country, pid, year)

<<<<<<< HEAD
select(df_eu_silc,country,panel,pid,year,ftc,job_change,ftc_num,ftc_num_c,ftc_num_sum,ftc_num_sum_c) %>% filter(ftc_num_sum_c != ftc_num_sum)

# Spells Duration of FTC ----
# if current job is FTC, but previous job is not FTC, then new job is started (start == 1)
# if current and previous job is FTC, but they did change jobs (job_change == 1), then new job is started (start == 1)
# otherwise no new job is started (start == 0)
# job spell is the cumsum of each new job start
# within each new job spell, calculate duration if FTC == 1

=======
# View(select(df_eu_silc,panel,country,pid,ftc,empst,job_change,matches("ftc")) %>% filter(ftc_num_sum_c != ftc_num_sum))

summary(df_eu_silc)

# View(select(df_eu_silc, country, panel, pid, year, empst, matches("ftc"), job_change) %>% filter(ftc_num_sum>1))
# View(select(df_eu_silc, country, panel, pid, year, empst, matches("ftc"), job_change) %>% filter(pid==90040001))

# Spells Duration of FTC --------------------------------------------------------------
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e
df_eu_silc <- df_eu_silc %>%
        group_by(panel, country, pid) %>%
        mutate(start = ifelse(ftc != lag(ftc) & row_number() > 1, yes = 1, 
                              ifelse(ftc == lag(ftc) & job_change == 1 & row_number() > 1, yes = 1, no = 0)),
               spell = cumsum(start)) %>%
        group_by(panel, country, pid, spell) %>%
<<<<<<< HEAD
        mutate(ftc_dur = ifelse(ftc == 1, yes = cumsum(ftc), no = 0)) %>%
        ungroup()

with(df_eu_silc,table(ftc_num_sum_c,ftc_dur))
select(df_eu_silc,country,panel,pid,year,ftc,spell,job_change,ftc_dur,ftc_num_sum_c) %>% filter(ftc_num_sum_c==1&ftc_dur==4)

# Clean variable names, recode values, etc. ----

# create dependent variables
df_eu_silc$ftc_ever <- recode(df_eu_silc$ftc_num_sum_c, "1:hi = 1")
df_eu_silc$ftc_num <- recode(df_eu_silc$ftc_num_sum_c, "2:hi = 2")
df_eu_silc$ftc_dur <- recode(df_eu_silc$ftc_dur, "2:hi = 2")

df_eu_silc <- df_eu_silc %>% 
        mutate(edu_cat=isced_cat_2,
               country = as.factor(as.character(country)),
               country_name = as.factor(as.character(country_name)),
               male = as.factor(male),
               edu_cat = as.factor(edu_cat),
               age_cat = as.factor(age_cat)) %>%
        arrange(country, panel, pid, year)

df_eu_silc <- within(df_eu_silc, edu_cat <- relevel(edu_cat, ref = "2"))
df_eu_silc <- within(df_eu_silc, age_cat <- relevel(age_cat, ref = "2"))

# Code country/region ----

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

df_eu_silc <- merge(df_eu_silc,geography) %>%
        arrange(country, panel, pid, year)

rm(country_name, region, geography)

table(df_eu_silc$country_name, useNA = "ifany")
table(df_eu_silc$region, useNA = "ifany")
with(df_eu_silc,table(country_name,region))

# Save descriptives data ----
# this dataset is necessary for do file: "04_descriptive_tables_country_panel_years.R"
saveRDS(df_eu_silc, file = paste0(data_files, "df_eu_silc_clean.rds"))

# Save number data (individual) ----

# Collapse panel data into cross sectional

df_eu_silc_xc <- df_eu_silc %>%
        select(region, country, country_name, panel, pid, year, edu_cat, male, age_cat, ftc, ftc_ever, ftc_num, ftc_dur, weight_long) %>%
=======
        mutate(ftc_duration = ifelse(ftc == 1, yes = cumsum(ftc), no = 0)) %>%
        ungroup()

# View(select(df_eu_silc, country, panel, pid, year, empst, matches("ftc"), job_change, start, spell, ftc_duration) %>% filter(ftc_num_sum>1))
# View(select(df_eu_silc, country, panel, pid, year, empst, matches("ftc"), matches("empst"), job_change, start, spell, ftc_duration) %>% filter(pid==81720002))

# Rename --------------------------------------------------------------

df_eu_silc <- df_eu_silc %>%
        select(-ftc_num, -ftc_num_c) %>% # drop irrelevant vars
        rename(ftc_raw = ftc,
               ftc = ftc_num_sum,
               ftc_c = ftc_num_sum_c) %>%
        arrange(country, panel, pid, year)

summary(df_eu_silc)

# Count number of FTC by employed --------------------------------------------------------------

df_eu_silc$ftc_raw <- df_eu_silc$ftc
df_eu_silc$ftc_c_raw <- df_eu_silc$ftc_c

df_eu_silc$ftc_c <- recode(df_eu_silc$ftc_c, "2:hi = 2")
df_eu_silc$ftc_ever_c <- recode(df_eu_silc$ftc_c, "1:hi = 1")

table(df_eu_silc$ftc_c)

#tranform ftc_num_sum & ftc_num_sum_c into categorical dummy variables
df_dummy <- dummy(x = df_eu_silc$ftc_c, sep = "_")
df_eu_silc <- cbind(df_eu_silc, df_dummy)
rm(df_dummy)

# Select variables --------------------------------------------------------------
df_eu_silc_1 <- df_eu_silc %>% 
        mutate(edu_cat=isced_cat_2,
               country = as.factor(as.character(country)),
               country_name = as.factor(as.character(country_name)),
               male = as.factor(male),
               edu_cat = as.factor(edu_cat),
               age_cat = as.factor(age_cat)) %>%
        arrange(country, panel, pid, year)

saveRDS(df_eu_silc_1, file = paste0(data_files, "df_eu_silc_clean.rds"))

# Save number data (individual) --------------------------------------------------------------

# Collapse panel data into cross sectional

df_eu_silc_xc <- df_eu_silc_1 %>%
        select(country, country_name, panel, pid, year, edu_cat, male, age_cat, job_change, matches("empst"), matches("ftc"), weight_long_4) %>%
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e
        arrange(country, panel, pid, year) %>%
        group_by(country, panel, pid) %>%
        filter(row_number() == n()) %>%
        ungroup()

df_eu_silc_xc <- droplevels(df_eu_silc_xc)
saveRDS(df_eu_silc_xc, file = paste0(data_files, "df_eu_silc_clean_xc.rds"))

<<<<<<< HEAD
# Save duration data (spell) ----

# Filter data 
# person-spell data for employed individuals
df_eu_silc_panel <- df_eu_silc %>%
        filter(empst != 0) %>%
        select(region, country, country_name, panel, pid, spell, year, edu_cat, male, age_cat, ftc, ftc_ever, ftc_num, ftc_dur, weight_long)
df_eu_silc_panel <- droplevels(df_eu_silc_panel)

# Save new data: This is necessary to create a temporary employment rate from panel data for comparison to LFS data
=======
# Save duration data (spell) --------------------------------------------------------------

df_eu_silc_panel <- df_eu_silc_1 %>%
        select(country, country_name, panel, pid, year, edu_cat, male, age_cat, empst, ftc_ever_c, job_change, ftc_duration, spell, weight_long_4)
df_eu_silc_panel <- droplevels(df_eu_silc_panel)

# Filter data 
# person-spell data for employed individuals
# must be employed
df_eu_silc_panel <- df_eu_silc_panel %>%
        filter(empst != 0)

>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e
saveRDS(df_eu_silc_panel, file = paste0(data_files, "df_eu_silc_clean_panel_year.rds"))

# keep last observation per spell
df_eu_silc_panel <- df_eu_silc_panel %>%
        group_by(country_name,panel,pid,spell) %>%
        filter(row_number()==n()) %>%
        ungroup()

<<<<<<< HEAD
saveRDS(df_eu_silc_panel, file = paste0(data_files, "df_eu_silc_clean_panel.rds"))

beep()
=======
df_eu_silc_panel$ftc_duration_raw <- df_eu_silc_panel$ftc_duration
df_eu_silc_panel$ftc_duration <- recode(df_eu_silc_panel$ftc_duration, "3:hi = 3")

saveRDS(df_eu_silc_panel, file = paste0(data_files, "df_eu_silc_clean_panel.rds"))

# beep()
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e

