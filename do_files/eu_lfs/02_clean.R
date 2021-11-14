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
setwd("/Users/jonathanlatner/Google Drive/SECCOPA/projects/distribution_contyp/")
# setwd("/Users/jonathanlatner/Documents/GitHub/distribution_contyp/")

data_files = "data_files/eu_lfs/"

# LIBRARY
library(tidyverse)
library(car) # recode
library(data.table)
library(countrycode)

# Load data -----------------------------------------

df_eu_lfs_0 <- readRDS(file = paste0(data_files, "df_eu_lfs.rds"))

# Clean data -----------------------------------------

df_eu_lfs_1 <- df_eu_lfs_0 %>%
        select(country,year,ilostat,age,temp,sex,hatlev1d,coeff) %>%
        filter(year >= 1996) %>% 
        filter(ilostat==1) %>% # employed
        filter(age >=25 & age <=54) %>%
        filter(temp == 1 | temp == 2) %>%
        mutate(temp = temp - 1) %>%
        mutate(female = sex - 1) %>%
        rename(edu_cat = hatlev1d,
               weight = coeff) %>%
        filter(weight>0) %>%
        filter(edu_cat=="L" | edu_cat=="M" | edu_cat=="H")
df_eu_lfs_1 <- droplevels(df_eu_lfs_1)

df_eu_lfs_1$age_cat <- recode(df_eu_lfs_1$age, "25:34 = 1; 35:44=2; 45:54=3")
df_eu_lfs_1$age_cat <- factor(df_eu_lfs_1$age_cat, labels=c("< 35", "35-44", "> 45"))
df_eu_lfs_1$female <- factor(df_eu_lfs_1$female, labels=c("Male", "Female"))
df_eu_lfs_1$edu_cat <- factor(df_eu_lfs_1$edu_cat, 
                              levels = c("L","M","H"), 
                              labels=c("< Secondary", "Secondary", "> Secondary"))

# Geography -----------------------------------------

country <- c("Anglophone", "Continental", "Eastern", "Nordic", "Southern",
             "DE", "CH", "LU", "BE", "AT", "NL", "FR", 
             "GB", "IE", 
             "MT", "GR", "IT", "CY", "PT", "ES", "RO", "PL", 
             "HR", "HU", "CZ", "BG", "SI", "SK", "LT", "EE", "LV", "RS", 
             "SE", "DK", "NO", "FI", "IS")
region <- c("Anglophone", "Continental", "Eastern", "Nordic", "Southern",
            "Continental", "Continental", "Continental", "Continental", "Continental", "Continental", "Continental", 
            "Anglophone", "Anglophone", 
            "Southern", "Southern", "Southern", "Southern", "Southern", "Southern", 
            "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", 
            "Nordic", "Nordic", "Nordic", "Nordic", "Nordic")

region <- cbind(country, region)
rm(country)

df_eu_lfs_1 <- merge(data.table(df_eu_lfs_1),data.table(region), by = c("country"), all.x = TRUE)

df_eu_lfs_1$country_name <- countrycode(df_eu_lfs_1$country, 'genc2c', 'country.name')

# Save sample -----------------------------------------

saveRDS(df_eu_lfs_1, file = paste0(data_files, "df_eu_lfs_clean.rds"))
