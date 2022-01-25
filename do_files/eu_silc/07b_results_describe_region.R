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
setwd("~/GitHub/distribution_contyp/")

graphs = "graphs/eu_silc/"
results = "results/eu_silc/"

# LIBRARY
library(tidyverse)

options(scipen=999)

# Load ----

df_ever <- readRDS(file = paste0(results,"df_yhat_glm_ever_region_wt.rds"))
df_num <- readRDS(file = paste0(results,"df_yhat_glm_num_region_wt.rds"))
df_dur <- readRDS(file = paste0(results,"df_yhat_glm_dur_region_wt.rds"))

# Clean ever temporary ----

df_ever_2008_2019 <- df_ever %>%
        select(geography,panel,fit,lwr,upr) %>%
        mutate(diff_2008_2019 = (last(fit) - first(fit)),
               sig_up = last(lwr) - first(upr),
               sig_dn = last(upr) - first(lwr)) %>%
        ungroup() %>%
        mutate(sig_2008_2019 = ifelse(diff_2008_2019 < 0 & sig_dn < 0, yes = "yes",
                                      ifelse(diff_2008_2019 > 0 & sig_up > 0, yes = "yes", no = "no"))) %>%
        select(-sig_up, -sig_dn)

df_ever_2008_2013 <- df_ever %>%
        select(geography,panel,fit,lwr,upr) %>%
        filter(panel < 2014) %>%
        mutate(diff_2008_2013 = (last(fit) - first(fit)),
               sig_up = last(lwr) - first(upr),
               sig_dn = last(upr) - first(lwr)) %>%
        filter(row_number()==1) %>%
        mutate(sig_2008_2013 = ifelse(diff_2008_2013 < 0 & sig_dn < 0, yes = "yes",
                                      ifelse(diff_2008_2013 > 0 & sig_up > 0, yes = "yes", no = "no"))) %>%
        select(diff_2008_2013, sig_2008_2013)

df_ever_2013_2019 <- df_ever %>%
        select(geography,panel,fit,lwr,upr) %>%
        filter(panel > 2013) %>%
        mutate(diff_2013_2019 = (last(fit) - first(fit)),
               sig_up = last(lwr) - first(upr),
               sig_dn = last(upr) - first(lwr)) %>%
        filter(row_number()==1) %>%
        mutate(sig_2013_2019 = ifelse(diff_2013_2019 < 0 & sig_dn < 0, yes = "yes",
                                      ifelse(diff_2013_2019 > 0 & sig_up > 0, yes = "yes", no = "no"))) %>%
        select(diff_2013_2019, sig_2013_2019)

df_ever <- merge(df_ever_2008_2019,df_ever_2008_2013)
df_ever <- merge(df_ever,df_ever_2013_2019)
rm(df_ever_2008_2013,df_ever_2013_2019,df_ever_2008_2019)

df_ever <- df_ever %>%
        mutate(fit = round(fit, 3),
               lwr = round(lwr, 3),
               upr = round(upr, 3),
               diff_2008_2019 = round(diff_2008_2019, 3),
               diff_2008_2013 = round(diff_2008_2013, 3),
               diff_2013_2019 = round(diff_2013_2019, 3))

# Clean contract number ----

df_num_2008_2019 <- df_num %>%
        select(geography,panel,fit,lwr,upr) %>%
        mutate(diff_2008_2019 = (last(fit) - first(fit)),
               sig_up = last(lwr) - first(upr),
               sig_dn = last(upr) - first(lwr)) %>%
        ungroup() %>%
        mutate(sig_2008_2019 = ifelse(diff_2008_2019 < 0 & sig_dn < 0, yes = "yes",
                                      ifelse(diff_2008_2019 > 0 & sig_up > 0, yes = "yes", no = "no"))) %>%
        select(-sig_up, -sig_dn)

df_num_2008_2013 <- df_num %>%
        select(geography,panel,fit,lwr,upr) %>%
        filter(panel < 2014) %>%
        mutate(diff_2008_2013 = (last(fit) - first(fit)),
               sig_up = last(lwr) - first(upr),
               sig_dn = last(upr) - first(lwr)) %>%
        filter(row_number()==1) %>%
        mutate(sig_2008_2013 = ifelse(diff_2008_2013 < 0 & sig_dn < 0, yes = "yes",
                                      ifelse(diff_2008_2013 > 0 & sig_up > 0, yes = "yes", no = "no"))) %>%
        select(diff_2008_2013, sig_2008_2013)

df_num_2013_2019 <- df_num %>%
        select(geography,panel,fit,lwr,upr) %>%
        filter(panel > 2013) %>%
        mutate(diff_2013_2019 = (last(fit) - first(fit)),
               sig_up = last(lwr) - first(upr),
               sig_dn = last(upr) - first(lwr)) %>%
        filter(row_number()==1) %>%
        mutate(sig_2013_2019 = ifelse(diff_2013_2019 < 0 & sig_dn < 0, yes = "yes",
                                      ifelse(diff_2013_2019 > 0 & sig_up > 0, yes = "yes", no = "no"))) %>%
        select(diff_2013_2019, sig_2013_2019)

df_num <- merge(df_num_2008_2019,df_num_2008_2013)
df_num <- merge(df_num,df_num_2013_2019)
rm(df_num_2008_2013,df_num_2013_2019,df_num_2008_2019)

df_num <- df_num %>%
        mutate(fit = round(fit, 3),
               lwr = round(lwr, 3),
               upr = round(upr, 3),
               diff_2008_2019 = round(diff_2008_2019, 3),
               diff_2008_2013 = round(diff_2008_2013, 3),
               diff_2013_2019 = round(diff_2013_2019, 3))

# Clean contract duration ----

df_dur_2008_2019 <- df_dur %>%
        select(geography,panel,fit,lwr,upr) %>%
        mutate(diff_2008_2019 = (last(fit) - first(fit)),
               sig_up = last(lwr) - first(upr),
               sig_dn = last(upr) - first(lwr)) %>%
        ungroup() %>%
        mutate(sig_2008_2019 = ifelse(diff_2008_2019 < 0 & sig_dn < 0, yes = "yes",
                                      ifelse(diff_2008_2019 > 0 & sig_up > 0, yes = "yes", no = "no"))) %>%
        select(-sig_up, -sig_dn)

df_dur2008_2013 <- df_dur %>%
        select(geography,panel,fit,lwr,upr) %>%
        filter(panel < 2014) %>%
        mutate(diff_2008_2013 = (last(fit) - first(fit)),
               sig_up = last(lwr) - first(upr),
               sig_dn = last(upr) - first(lwr)) %>%
        filter(row_number()==1) %>%
        mutate(sig_2008_2013 = ifelse(diff_2008_2013 < 0 & sig_dn < 0, yes = "yes",
                                      ifelse(diff_2008_2013 > 0 & sig_up > 0, yes = "yes", no = "no"))) %>%
        select(diff_2008_2013, sig_2008_2013)

df_dur2013_2019 <- df_dur %>%
        select(geography,panel,fit,lwr,upr) %>%
        filter(panel > 2013) %>%
        mutate(diff_2013_2019 = (last(fit) - first(fit)),
               sig_up = last(lwr) - first(upr),
               sig_dn = last(upr) - first(lwr)) %>%
        filter(row_number()==1) %>%
        mutate(sig_2013_2019 = ifelse(diff_2013_2019 < 0 & sig_dn < 0, yes = "yes",
                                      ifelse(diff_2013_2019 > 0 & sig_up > 0, yes = "yes", no = "no"))) %>%
        select(diff_2013_2019, sig_2013_2019)

df_dur <- merge(df_dur_2008_2019,df_dur2008_2013)
df_dur <- merge(df_dur,df_dur2013_2019)
rm(df_dur2008_2013,df_dur2013_2019,df_dur_2008_2019)

df_dur <- df_dur %>%
        mutate(fit = round(fit, 3),
               lwr = round(lwr, 3),
               upr = round(upr, 3),
               diff_2008_2019 = round(diff_2008_2019, 3),
               diff_2008_2013 = round(diff_2008_2013, 3),
               diff_2013_2019 = round(diff_2013_2019, 3))

# results -----

df_dur %>% 
        filter(geography == "Southern")

df_num
df_dur

