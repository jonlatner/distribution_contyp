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

# FOLDERS
setwd("/Users/jonathanlatner/Google Drive/")
# setwd("C:/Users/ba1ks6/Google Drive/")

orig_data_files = "SECCOPA/projects/distribution_contyp/data_files/eu_silc/"
data_files = "SECCOPA/projects/distribution_contyp/data_files/eu_silc/2_year_panel/"

# LIBRARY
library(tidyverse)
library(data.table)
library(beepr)
library(reshape2)
library(car)
library(zoo)

# step 0: Load data -----------------------------------------

df_eu_silc_0 <- readRDS(file = paste0(orig_data_files,"df_eu_silc.rds"))

step0 <- data.frame(with(df_eu_silc_0, table(country,panel)))
step0 <- filter(step0, Freq>0) %>% 
        arrange(panel,country)  %>%
        mutate(total = sum(Freq))

df_unique <- df_eu_silc_0 %>%
        select(country,panel,pid) %>%
        group_by(country,panel,pid) %>%
        slice(1) %>%
        group_by(country,panel) %>%
        tally() %>%
        ungroup()

step0 <- merge(step0,df_unique) %>%
        mutate(step = 0)
head(step0)

# step 1: Clean panel data ----

df_eu_silc_1 <- df_eu_silc_0

df_eu_silc_1$country <- as.factor(df_eu_silc_1$country) 
levels(df_eu_silc_1$country)[levels(df_eu_silc_1$country)=="EL"] <- "GR"
levels(df_eu_silc_1$country)[levels(df_eu_silc_1$country)=="UK"] <- "GB"

# Clean panel
df_eu_silc_1 <- df_eu_silc_1 %>%
        mutate(test = ifelse(panel == 2007 & year < 2004, yes = 1, 
                             ifelse(panel == 2008 & year < 2005, yes = 1,
                                    ifelse(panel == 2009 & year < 2006, yes = 1, 
                                           ifelse(panel == 2010 & year < 2007, yes = 1, no = 0))))) %>%
        filter(test == 0) %>%
        select(-test)

# drop panel == 2007
df_eu_silc_1 <- df_eu_silc_1 %>%
        filter(panel>2007)

# Must have non-missing, temporary employment rate in a given country, panel, year > 0
df_test_1 <- df_eu_silc_1 %>%
        group_by(country, panel, year) %>%
        summarise(mean = mean(contyp, na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(mean)

df_test_2 <- df_test_1 %>%
        filter(mean>1)
df_eu_silc_1 <- merge(data.table(df_eu_silc_1),data.table(df_test_2),all.y = TRUE)
saveRDS(df_test_1, file = paste0(data_files, "df_eu_silc_filter_steps_country.rds"))

step1 <- data.frame(with(df_eu_silc_1, table(country,panel)))
step1 <- filter(step1, Freq>0) %>% 
        arrange(panel,country)  %>%
        mutate(total = sum(Freq))
df_unique <- df_eu_silc_1 %>%
        select(country,panel,pid) %>%
        group_by(country,panel,pid) %>%
        slice(1) %>%
        group_by(country,panel) %>%
        tally() %>%
        ungroup()

step1 <- merge(step1,df_unique) %>%
        mutate(step = 1)

head(step1)

# step 2: Individual-level filters -----------------------------------------

# for some reason, in the NL, if panel>=2016, first year has weight_personal_base==0
# therefore, to keep a 4 year panel, we weight these to 1
# df_base_weight <- df_eu_silc_1 %>%
#         filter(country=="NL") %>%
#         filter(panel==2019) %>%
#         select(country,pid,year,panel,matches("weight")) %>%
#         arrange(country,pid,year,panel,weight_personal_base)
# table(df_base_weight$year)

df_eu_silc_2 <- df_eu_silc_1 %>%
        mutate(age = year - birthy) %>%
        mutate(weight_personal_base=ifelse(country == "NL" & weight_personal_base == 0 & (panel==2016|panel==2017|panel==2018|panel==2019), yes = 1, no = weight_personal_base)) %>%
        filter(weight_personal_base>0) %>% # remove duplicates
        filter(age >= 25 & age < 55) %>%
        select(country, pid, year, weight_long_2, everything()) %>%
        arrange(country, pid, year)

# Clean employment status
df_eu_silc_2$empst_1_new <- recode(df_eu_silc_2$empst_1, "1=1; 2=2; 3=0; 4:hi=3; else = NA")  # unemployed = 0, employed = 1, else = NA
df_eu_silc_2$empst_2_new <- recode(df_eu_silc_2$empst_2, "c(1,3)=1; c(2,4)=2; 5=0; 6:hi = 3; else = NA")  # unemployed = 0, employed = 1, else = NA

# employment status 2009 - 2012
df_empst_2009_2012 <- df_eu_silc_2 %>%
        filter(panel > 2008 & panel < 2013) %>%
        mutate(empst_1_new = ifelse(empst_1_f==-5, yes = empst_2_new, 
                                    ifelse(empst_1_f == 1, yes = empst_1_new, no = NA)),
               empst_2_new = ifelse(empst_2_f==-5, yes = empst_1_new, 
                                    ifelse(empst_2_f == 1, yes = empst_2_new, no = NA)),
               empst_2009_2012 = ifelse(empst_1_f==-5, yes = empst_2_new,
                                        ifelse(empst_1_f == 1, yes = empst_1_new,no = NA)),
               empst_2009_2012 = ifelse(is.na(empst_1_f)&empst_2_f == 1, yes = empst_2_new, no = empst_2009_2012),
               empst_2009_2012 = ifelse(empst_1_f==0&empst_2_f == 1&country=="PL"&year==2009, yes = empst_2_new, no = empst_2009_2012),
               empst_2009_2012 = ifelse(empst_1_f==-1&empst_2_f == 1&country=="LT"&year==2011, yes = empst_2_new, no = empst_2009_2012)) %>%
        select(country,panel,pid,year,empst_2009_2012)

df_empst_2009_2012 <- unique(df_empst_2009_2012)

df_eu_silc_2 <- merge(data.table(df_eu_silc_2),data.table(df_empst_2009_2012), all.x = TRUE)

rm(df_empst_2009_2012)

df_eu_silc_2 <- df_eu_silc_2 %>%
        mutate(empst = ifelse(panel < 2009, yes = empst_1_new,
                              ifelse((panel > 2008 & panel < 2013), yes = empst_2009_2012,
                                     ifelse(panel > 2012, yes = empst_2_new, no = NA))))

df_eu_silc_2 <- df_eu_silc_2 %>%
        mutate(ftime = ifelse(empst==1, yes = 1,
                              ifelse(empst==2, yes = 0, no = NA)),
               contyp = ifelse(empst==0, yes = NA, no = contyp))

df_eu_silc_2$empst <- recode(df_eu_silc_2$empst, "1:2=1; 0=0; else=NA") #employed or unemployed

# Clean contract type
df_eu_silc_2 <- df_eu_silc_2 %>%
        mutate(contyp = ifelse(empst==0, yes = NA, no = contyp))

df_eu_silc_2 <- df_eu_silc_2 %>%
        filter(empst == 0 | empst == 1) # employed or unemployed

step2 <- data.frame(with(df_eu_silc_2, table(country,panel)))

step2 <- filter(step2, Freq>0) %>% 
        arrange(panel,country)  %>%
        mutate(total = sum(Freq))
step2a <- dcast(step2, country~panel)
df_unique <- df_eu_silc_2 %>%
        select(country,panel,pid) %>%
        group_by(country,panel,pid) %>%
        slice(1) %>%
        group_by(country,panel) %>%
        tally() %>%
        ungroup()
step2 <- merge(step2,df_unique) %>%
        mutate(step = 2)
head(step2)

# step 3: Must be employed at least once -----------------------------------------

df_eu_silc_3 <- df_eu_silc_2 %>%
        select(-matches("empst_")) %>%
        group_by(panel, country, pid) %>%
        mutate(empst_num = sum(empst, na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(panel, country, pid, year) %>%
        filter(empst_num > 0)

step3 <- data.frame(with(df_eu_silc_3, table(country,panel)))
step3 <- filter(step3, Freq>0) %>% 
        arrange(panel,country)  %>%
        mutate(total = sum(Freq))
step3a <- dcast(step3, country~panel)
df_unique <- df_eu_silc_3 %>%
        select(country,panel,pid) %>%
        group_by(country,panel,pid) %>%
        slice(1) %>%
        group_by(country,panel) %>%
        tally() %>%
        ungroup()
step3 <- merge(step3,df_unique) %>%
        mutate(step = 3)
head(step3)

# step 4: Case wise deletion on IV of interest -----------------------------------------

df_eu_silc_4 <- df_eu_silc_3

# education
# 1 - primary, 2 - secondary (lower and upper), 3 - post-secondary (non-tertiary), 4 - tertiary
df_eu_silc_4$isced_cat_1 <- recode(df_eu_silc_4$isced, "0:2=1; 3=2; 4=3; 5:6=4;
                                 34=3; 35=3; 
                                 100:200=1; 300:354=2; 400:499=3; 500:900=4")

# 1 - less than secondary, 2 - secondary, 3 - more than secondary
df_eu_silc_4$isced_cat_2 <- recode(df_eu_silc_4$isced_cat_1, "1=1; 2=2; 3:4=3")
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

step4 <- data.frame(with(df_eu_silc_4, table(country,panel)))
step4 <- filter(step4, Freq>0) %>% 
        arrange(panel,country)  %>%
        mutate(total = sum(Freq))
step4a <- dcast(step4, country~panel)
df_unique <- df_eu_silc_4 %>%
        select(country,panel,pid) %>%
        group_by(country,panel,pid) %>%
        slice(1) %>%
        group_by(country,panel) %>%
        tally() %>%
        ungroup()
step4 <- merge(step4,df_unique) %>%
        mutate(step = 4)
head(step4)

# step 5: Individuals in each year of 2 year panel period -----------------------------------------

df_eu_silc_5 <- df_eu_silc_4 %>%
        arrange(country, panel, pid, year) %>%
        group_by(country, panel, pid) %>%
        mutate(weight_long_2 = last(weight_long_2)) %>%
        mutate(number = row_number(),
               count = max(number)) %>% 
        filter(count >= 2 & number <= 2) %>%
        select(-count, -number) %>%
        ungroup() %>%
        select(country, panel, pid, year, everything()) %>%
        arrange(country, panel, pid, year)

step5 <- data.frame(with(df_eu_silc_5, table(country,panel)))
step5 <- filter(step5, Freq>0) %>% 
        arrange(panel,country)  %>%
        mutate(total = sum(Freq))
step5a <- dcast(step5, country~panel)
df_unique <- df_eu_silc_5 %>%
        select(country,panel,pid) %>%
        group_by(country,panel,pid) %>%
        slice(1) %>%
        group_by(country,panel) %>%
        tally() %>%
        ungroup()
step5 <- merge(step5,df_unique) %>%
        mutate(step = 5)
head(step5)

# step 6: Drop without longitudinal weight -----------------------------------------

df_eu_silc_6 <- df_eu_silc_5 %>%
        filter(!is.na(weight_long_2) & weight_long_2>0)

step6 <- data.frame(with(df_eu_silc_6, table(country,panel)))
step6 <- filter(step6, Freq>0) %>% 
        arrange(panel,country)  %>%
        mutate(total = sum(Freq))
step6a <- dcast(step6, country~panel)
df_unique <- df_eu_silc_6 %>%
        select(country,panel,pid) %>%
        group_by(country,panel,pid) %>%
        slice(1) %>%
        group_by(country,panel) %>%
        tally() %>%
        ungroup()
step6 <- merge(step6,df_unique) %>%
        mutate(step = 6)
head(step6)

# Save -----------------------------------------

df_eu_silc_7 <- df_eu_silc_6 %>%
        select(country,panel,pid,year,weight_long_2,male,job_change,isced_cat_2,empst,contyp,age,empst,empst_num)

saveRDS(df_eu_silc_7, file = paste0(data_files, "df_eu_silc_filter.rds"))

df_filter_steps <- rbind(step0,step1,step2,step3,step4,step5,step6)
saveRDS(df_filter_steps, file = paste0(data_files, "df_eu_silc_filter_steps.rds"))

beep()
