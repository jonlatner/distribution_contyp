# Top commands -----------------------------------------
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
setwd("~/GitHub/distribution_contyp/")

orig_data_files = "data_files/eu_silc/"
data_files = "data_files/eu_silc/2_year_panel/"

# LIBRARY
library(tidyverse)
library(data.table) # faster merging
library(beepr)
library(car) # recode
library(zoo) # na.locf

# step 0: Load data -----------------------------------------

df_eu_silc_0 <- readRDS(file = paste0(orig_data_files,"01_df_eu_silc_clean_empst.rds"))

# https://newbedev.com/r-keep-first-observation-per-group-identified-by-multiple-variables-stata-equivalent-bys-var1-var2-keep-if-n-1
step_0 <- df_eu_silc_0 %>%
        select(country,panel,pid) %>%
        mutate(total = sum(n())) 
step_0 <- data.table(step_0, key = "country,panel,pid")
step_0 <- step_0[, head(.SD, 1), by = key(step_0)]
# step_0 <- step_0[, .I[1], by = key(step_0)] # this also works and is slightly faster
step_0 <- data.table(step_0, key = "country,panel")
step_0 <- step_0[, .(count = .N), by = key(step_0)]
step_0$step <- 0

# step 1: Panel level filters ----

df_eu_silc_1 <- df_eu_silc_0

df_eu_silc_1$country <- as.factor(df_eu_silc_1$country) 
levels(df_eu_silc_1$country)[levels(df_eu_silc_1$country)=="EL"] <- "GR"
levels(df_eu_silc_1$country)[levels(df_eu_silc_1$country)=="UK"] <- "GB"

# Panel must be no longer than 4 years long - Appears to be a problem in LU

# with(df_eu_silc_1,table(panel,year))
# df_test <- filter(df_eu_silc_1,panel==2010&year==2003)
# table(df_test$country)
# rm(df_test)

df_eu_silc_1 <- df_eu_silc_1 %>%
        mutate(test = ifelse(panel == 2007 & year < 2004, yes = 1, 
                             ifelse(panel == 2008 & year < 2005, yes = 1,
                                    ifelse(panel == 2009 & year < 2006, yes = 1, 
                                           ifelse(panel == 2010 & year < 2007, yes = 1, no = 0))))) %>%
        filter(test == 0) %>%
        select(-test)

# drop panel == 2007 because only 5 countries with 4 year panel period
# df_test <- df_eu_silc_1 %>%
#         filter(weight_long_2 > 0)
# with(df_test,table(country,panel))

df_eu_silc_1 <- df_eu_silc_1 %>%
        filter(panel>2007)

# Must have non-missing, temporary employment rate in a given country, panel, year > 0
# Remember, contyp is coded 1 or 2, so rate of 1 = rate of 0
df_test_1 <- df_eu_silc_1 %>%
        group_by(country, panel, year) %>%
        summarise(mean = mean(contyp, na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(mean)

# Appears to only affect Denmark in years between 2005 and 2010
# table(subset(df_test_1,mean==1)$country)
# table(subset(df_test_1,mean==1&country=="DK")$panel,subset(df_test_1,mean==1&country=="DK")$year)

df_test_2 <- df_test_1 %>%
        filter(mean>1)
df_eu_silc_1 <- merge(data.table(df_eu_silc_1),data.table(df_test_2),all.y = TRUE)
rm(df_test_1, df_test_2)

# Must be present for at least 2 panels in order to have a trend
# This appears to be only a problem for Germany
df_test_1 <- data.frame(with(df_eu_silc_1,table(country,panel)))
df_test_2 <- df_test_1 %>%
        arrange(country,panel) %>%
        filter(Freq>0) %>%
        group_by(country) %>%
        summarise(count = n()) %>%
        ungroup()
filter(df_test_2,count<3)
df_test_3 <- df_test_2 %>%
        filter(count>2) %>%
        select(-count)
df_eu_silc_1 <- merge(data.table(df_eu_silc_1),data.table(df_test_3),by=c("country"),all.y = TRUE)
rm(df_test_1, df_test_2, df_test_3)
df_eu_silc_1 <- droplevels(df_eu_silc_1)

step_1 <- df_eu_silc_1 %>%
        select(country,panel,pid) %>%
        mutate(total = sum(n())) 
step_1 <- data.table(step_1, key = "country,panel,pid")
step_1 <- step_1[, head(.SD, 1), by = key(step_1)]
step_1 <- data.table(step_1, key = "country,panel")
step_1 <- step_1[, .(count = .N), by = key(step_1)]
step_1$step <- 1

# step 2: Individual-level filters -----------------------------------------

df_eu_silc_2 <- df_eu_silc_1

# df_base_weight <- df_eu_silc_2 %>%
#         select(country,pid,year,panel,matches("weight")) %>%
#         group_by(country,panel,year) %>%
#         summarise(mean = mean(weight_personal_base)) %>%
#         ungroup()
# df_base_weight %>% filter(mean==0) %>% arrange(country,panel) %>% print(n=40) 

# for some reason, in the NL, if panel>=2016, first year has weight_personal_base==0
# therefore, to keep a 4 year panel, we weight these to 1
# df_base_weight <- df_eu_silc_2 %>%
#         filter(country=="NL") %>%
#         filter(panel==2019) %>%
#         select(country,pid,year,panel,matches("weight")) %>%
#         arrange(country,pid,year,panel,weight_personal_base)
# table(df_base_weight$year)

# for some reason, in NO, if panel>=2010, only last observation in panel period has weight_personal_base>0
# therefore, to keep a 4 year panel, we weight these to 1
# df_base_weight <- df_eu_silc_2 %>%
#         filter(country=="NO") %>%
#         select(country,panel,pid,year,matches("weight")) %>%
#         arrange(country,panel,pid,year,weight_personal_base) %>%
#         group_by(panel,pid) %>%
#         mutate(count=row_number(),
#                max=max(count)) %>%
#         filter(max==2) %>%
#         filter(row_number()==1) %>%
#         group_by(panel) %>%
#         summarise(mean=mean(weight_personal_base)) %>%
#         ungroup()
# df_base_weight

# Code full time
df_eu_silc_2 <- df_eu_silc_2 %>%
        mutate(ftime = ifelse(empst==1, yes = 1,
                              ifelse(empst==2, yes = 0, no = NA)))

# Recode employment status
# 0 = unemployed
# 1 = employed (full-time or part-time)
# NA = NILF
df_eu_silc_2$empst <- recode(df_eu_silc_2$empst, "1:2=1; 0=0; else=NA")

# Clean contract type
# There are lots of examples where individuals are listed as unemployed (empst_1 == 3 | empst_2 == 5), but have a contract
# How do we treat these?  
# The answer is contract type variable (PL140) refers to current or last situation.
# If currently employed (PL031 = 1,2,3,4 or PL030 = 1,2), then current contract type
# If currently unemployed (PL031 > 4 or PL030 > 2), then previous situation
# Therefore, if you are unemployed, then you do not have contract type
df_eu_silc_2 <- df_eu_silc_2 %>%
        mutate(contyp = ifelse(empst==0, yes = NA, no = contyp))

df_eu_silc_2 <- df_eu_silc_2 %>%
        mutate(age = year - birthy) %>%
        mutate(weight_personal_base=ifelse(country == "NL" & weight_personal_base == 0 & (panel==2016|panel==2017|panel==2018|panel==2019), yes = 1, no = weight_personal_base)) %>%
        mutate(weight_personal_base=ifelse(country == "NO" & weight_personal_base == 0 & (panel>=2010), yes = 1, no = weight_personal_base)) %>%
        filter(weight_personal_base>0) %>% # remove duplicates
        filter(age >= 25 & age < 55) %>%
        filter(empst == 0 | empst == 1) %>% # employed or unemployed
        select(country, pid, year, weight_long_2, everything()) %>%
        arrange(country, pid, year)

step_2 <- df_eu_silc_2 %>%
        select(country,panel,pid) %>%
        mutate(total = sum(n())) 
step_2 <- data.table(step_2, key = "country,panel,pid")
step_2 <- step_2[, head(.SD, 1), by = key(step_2)]
step_2 <- data.table(step_2, key = "country,panel")
step_2 <- step_2[, .(count = .N), by = key(step_2)]
step_2$step <- 2

# step 3: Must be employed at least once -----------------------------------------

df_eu_silc_3 <- df_eu_silc_2

df_eu_silc_3 <- df_eu_silc_3 %>%
        select(-matches("empst_")) %>%
        group_by(panel, country, pid) %>%
        mutate(empst_num = sum(empst, na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(panel, country, pid, year) %>%
        filter(empst_num > 0)

step_3 <- df_eu_silc_3 %>%
        select(country,panel,pid) %>%
        mutate(total = sum(n())) 
step_3 <- data.table(step_3, key = "country,panel,pid")
step_3 <- step_3[, head(.SD, 1), by = key(step_3)]
step_3 <- data.table(step_3, key = "country,panel")
step_3 <- step_3[, .(count = .N), by = key(step_3)]
step_3$step <- 3

# step 4: Case wise deletion on IV of interest -----------------------------------------

df_eu_silc_4 <- df_eu_silc_3

# education
# 1 - primary, 2 - secondary (lower and upper), 3 - post-secondary (non-tertiary), 4 - tertiary
df_eu_silc_4$isced_cat_1 <- recode(df_eu_silc_4$isced, "0:2=1; 3=2; 4=3; 5:6=4;
                                 34=3; 35=3; 
                                 100:200=1; 300:354=2; 400:499=3; 500:900=4")

# 1 - less than secondary, 2 - secondary, 3 - more than secondary
df_eu_silc_4$isced_cat_2 <- recode(df_eu_silc_4$isced_cat_1, "1=1; 2=2; 3:4=3")

# Clean education
# Please note: this code is slow.  there is probably a way to speed it up, but i don't know how to implement it
# Replace all observations with last, non-missing observation - this reduces missing cases in the last observation (see example below) 
# Replace all observations with last, observed value - this makes sure that individuals edu status does not change within panel period
# df_eu_silc_4 %>% select(panel,country,pid,year,isced_cat_1) %>% filter(pid==418710001 & country == "PL" & panel == 2008)
# https://stackoverflow.com/questions/37060211/efficiently-locf-by-groups-in-a-single-r-data-table

df_eu_silc_4 <- df_eu_silc_4 %>%
        group_by(panel, country, pid) %>%
        mutate(isced_cat_1 = na.locf(isced_cat_1, na.rm=FALSE),
               isced_cat_1 = last(isced_cat_1)) %>%
        mutate(isced_cat_2 = na.locf(isced_cat_2, na.rm=FALSE),
               isced_cat_2 = last(isced_cat_2)) %>%
        ungroup()

# male = 1; female = 0
df_eu_silc_4$male <- recode(df_eu_silc_4$sex, "1=1; 2=0")

# Casewise delete missing observations
df_eu_silc_4 <- subset(df_eu_silc_4, !is.na(isced_cat_2)) # small number of cases with missing education
df_eu_silc_4 <- subset(df_eu_silc_4, !is.na(male)) # small number of cases with missing gender

# drop if contract information is missing and employed 
df_eu_silc_4 <- df_eu_silc_4 %>%
        mutate(missing = ifelse(empst == 1 & is.na(contyp), yes = 1, no = 0)) %>%
        filter(missing == 0) %>%
        select(-missing)

step_4 <- df_eu_silc_4 %>%
        select(country,panel,pid) %>%
        mutate(total = sum(n())) 
step_4 <- data.table(step_4, key = "country,panel,pid")
step_4 <- step_4[, head(.SD, 1), by = key(step_4)]
step_4 <- data.table(step_4, key = "country,panel")
step_4 <- step_4[, .(count = .N), by = key(step_4)]
step_4$step <- 4

# step 5: Individuals in each year of 2 year panel period -----------------------------------------

df_eu_silc_5 <- df_eu_silc_4

df_eu_silc_5 <- df_eu_silc_5 %>%
        arrange(country, panel, pid, year) %>%
        group_by(country, panel, pid) %>%
        mutate(number = row_number(),
               count = max(number)) %>% 
        filter(count >= 2 & number <= 2) %>%
        mutate(test = ifelse(row_number()==2, yes = year - lag(year), no = NA),
               test = last(test)) %>%
        filter(test == 1) %>%
        mutate(weight_long_2 = last(weight_long_2)) %>%
        ungroup() %>%
        select(country, panel, pid, year, everything()) %>%
        arrange(country, panel, pid, year)

step_5 <- df_eu_silc_5 %>%
        select(country,panel,pid) %>%
        mutate(total = sum(n())) 
step_5 <- data.table(step_5, key = "country,panel,pid")
step_5 <- step_5[, head(.SD, 1), by = key(step_5)]
step_5 <- data.table(step_5, key = "country,panel")
step_5 <- step_5[, .(count = .N), by = key(step_5)]
step_5$step <- 5

# step 6: Drop without longitudinal weight -----------------------------------------

df_eu_silc_6 <- df_eu_silc_5

df_eu_silc_6 <- df_eu_silc_6 %>%
        filter(!is.na(weight_long_2) & weight_long_2>0)

step_6 <- df_eu_silc_6 %>%
        select(country,panel,pid) %>%
        mutate(total = sum(n())) 
step_6 <- data.table(step_6, key = "country,panel,pid")
step_6 <- step_6[, head(.SD, 1), by = key(step_6)]
step_6 <- data.table(step_6, key = "country,panel")
step_6 <- step_6[, .(count = .N), by = key(step_6)]
step_6$step <- 6

# Save -----------------------------------------

df_eu_silc_7 <- df_eu_silc_6

df_eu_silc_7 <- df_eu_silc_7 %>%
        select(country,panel,pid,year,weight_long_2,male,job_change,isced_cat_2,empst,contyp,age,empst,empst_num)

saveRDS(df_eu_silc_7, file = paste0(data_files, "02_df_eu_silc_filter.rds"))

df_filter_steps <- rbind(step_0,step_1,step_2,step_3,step_4,step_5,step_6)
saveRDS(df_filter_steps, file = paste0(data_files, "02_df_eu_silc_filter_steps.rds"))

beep()
