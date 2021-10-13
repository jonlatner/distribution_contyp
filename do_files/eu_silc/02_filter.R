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
setwd("/Users/jonathanlatner/Documents/GitHub/distribution_contyp/")

data_files = "data_files/eu_silc/"

# LIBRARY
library(tidyverse)
library(data.table) # faster merging
library(beepr)
library(reshape2)
library(car) # recode
library(zoo) # na.locf

# step 0: Load data -----------------------------------------

df_eu_silc_0 <- readRDS(file = paste0(data_files,"df_eu_silc.rds"))

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
#         filter(weight_long_4 > 0)
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
        select(country, pid, year, weight_long_4, everything()) %>%
        arrange(country, pid, year)

# Clean employment status
df_eu_silc_2$empst_1_new <- recode(df_eu_silc_2$empst_1, "1=1; 2=2; 3=0; 4:hi=3; else = NA")  # unemployed = 0, employed = 1, else = NA
df_eu_silc_2$empst_2_new <- recode(df_eu_silc_2$empst_2, "c(1,3)=1; c(2,4)=2; 5=0; 6:hi = 3; else = NA")  # unemployed = 0, employed = 1, else = NA

# Clean employment status
# Between 2009 and 2012, SILC data transitioned from one definition of employment status to another.
# Moving from PL030 to PL031.  The difference is that PL031 accounts for self-employed
# To maintain consistency, we include self-employed as employed, conditional being employed with an observable work contract
# Therefore, three distinct definitions of employment status: 1) panel == 2008; 2) 2008 < panel < 2013; 3) panel >= 2013
# It is crucial to use the indicator variable
# Recode 1 = employed FT; 2 = employed PT; 0 = unemployed; 3 = not in labor force (NILF) 

df_empst_2009_2012 <- df_eu_silc_2 %>%
        filter(panel > 2008 & panel < 2013,
               # country == "PL",
               ) %>%
        select(country,panel,pid,year,matches("empst")) %>% 
        mutate(empst_1_new = ifelse(empst_1_f==-5, yes = empst_2_new, # if old indicator says to do so, then replace old with new variable 
                                    ifelse(empst_1_f == 1, yes = empst_1_new, no = NA)), # else use old variable
               empst_2_new = ifelse(empst_2_f==-5, yes = empst_1_new, #  if new indicator says to do so, then replace new with old variable
                                    ifelse(empst_2_f == 1, yes = empst_2_new, no = NA)), # else use new variable
        ) %>% # next step below, if employment status is missing in BOTH variables, then drop observations 
        mutate(test = ifelse(is.na(empst_1_new) & is.na(empst_2_new), yes = 1, no = 0)) %>% 
        filter(test == 0) %>%
        select(-test)

with(df_empst_2009_2012,table(empst_1_new,empst_2_new, useNA = "ifany"))

# In some countries (NL), this coding is enough
with(subset(df_empst_2009_2012,country=="NL"),table(empst_1_new,empst_2_new, useNA = "ifany"))

# In some countries (PL), this coding is not enough
with(subset(df_empst_2009_2012,country=="PL"),table(empst_1_new,empst_2_new, useNA = "ifany"))
with(subset(df_empst_2009_2012,country=="PL"),table(empst_1_f,empst_2_new, useNA = "ifany"))

# PL is also the only country where empst_1_f == 0, which is not nice because there is no value label for this code in that or any year
# LT and SI are also the only countries where empst_1_f == -1
with(df_empst_2009_2012,table(country,empst_1_f, useNA = "ifany"))
with(subset(df_empst_2009_2012,country=="PL"),table(empst_1_f,year, useNA = "ifany"))
with(subset(df_empst_2009_2012,country=="LT"),table(empst_1_f,year, useNA = "ifany"))
with(subset(df_empst_2009_2012,country=="SI"),table(empst_1_f,year, useNA = "ifany"))

with(subset(df_empst_2009_2012,country=="PL"),table(empst_1_f,empst_2_f, useNA = "ifany"))
with(subset(df_empst_2009_2012,country=="LT"),table(empst_1_f,empst_2_f, useNA = "ifany"))
with(subset(df_empst_2009_2012,country=="SI"),table(empst_1_f,empst_2_f, useNA = "ifany"))

# fix coding issue with PL, LT, SI, as described above
df_empst_2009_2012 <- df_empst_2009_2012 %>%
        mutate(empst_1_new = ifelse(empst_1_f==0&empst_2_f == 1&country=="PL", yes = empst_2_new, no = empst_1_new),
               empst_1_new = ifelse(empst_1_f==-1&empst_2_f == 1&country=="LT", yes = empst_2_new, no = empst_1_new),
               empst_1_new = ifelse(empst_1_f==-1&empst_2_f == 1&country=="SI", yes = empst_2_new, no = empst_1_new),
        )

with(df_empst_2009_2012,table(empst_1_new,empst_2_new, useNA = "ifany"))

# the remaining problem with matching old/new employment status is with observations where is.na(empst_1_f) or empst_2_f==-1
df_test <- filter(df_empst_2009_2012,!is.na(empst_2_new) & is.na(empst_1_new))
df_test <- droplevels(df_test)
summary(df_test)

df_test <- filter(df_empst_2009_2012,is.na(empst_2_new) & !is.na(empst_1_new))
df_test <- droplevels(df_test)
summary(df_test)

# fix issue where observations are missing values, as described above 
df_empst_2009_2012 <- df_empst_2009_2012 %>%
        mutate(empst_1_new = ifelse(is.na(empst_1_f)&empst_2_f == 1, yes = empst_2_new, no = empst_1_new),
               empst_2_new = ifelse(empst_2_f==-1&empst_1_f == 1, yes = empst_1_new, no = empst_2_new),
        ) 

with(df_empst_2009_2012,table(empst_1_new,empst_2_new, useNA = "ifany"))

# make new employment status variable for these years
df_empst_2009_2012 <- df_empst_2009_2012 %>%
        mutate(empst_2009_2012 = empst_2_new) %>%
        select(country,panel,pid,year,empst_2009_2012)

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

# step 5: Individuals in each year of 4 year panel period -----------------------------------------

df_eu_silc_5 <- df_eu_silc_4 %>%
        arrange(country, panel, pid, year) %>%
        group_by(country, panel, pid) %>%
        mutate(weight_long_4 = last(weight_long_4)) %>%
        mutate(number = row_number(),
               count = max(number)) %>% 
        filter(count >= 4 & number <= 4) %>%
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
        filter(!is.na(weight_long_4) & weight_long_4>0)

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
        select(country,panel,pid,year,weight_long_4,male,job_change,isced_cat_2,empst,contyp,age,empst,empst_num)

saveRDS(df_eu_silc_7, file = paste0(data_files, "df_eu_silc_filter.rds"))

beep()

head(step0) # raw data
head(step1) # "Country panel filters: Every panel period can only have four years.  Each country, panel, year must have non-missing temporary employment rate $>$ 0.  Each country must be in at least 3 periods (i.e. trend)."
head(step2) # "Individual filters: prime age (25 - 54), active labor market participation (employed or unemployed), personal weight $>$ 0"
head(step3) # "Must be employed at least once"
head(step4) # "Case-wise deletion of missing variables on education, gender, age, and contract type"
head(step5) # "Individuals in each year of 4 year panel period"
head(step6) # "Must have 4 year longitudinal weight"

df_filter_steps <- rbind(step0,step1,step2,step3,step4,step5,step6)
saveRDS(df_filter_steps, file = paste0(data_files, "df_eu_silc_filter_steps.rds"))

