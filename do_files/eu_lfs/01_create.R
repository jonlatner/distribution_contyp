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

# FOLDERS
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/")

# orig_data_files = "/Volumes/seccopa.empsoz/Data/EU-LFS/"
orig_data_files = "data/EU_LFS_2019/YearlyFiles_83_2019/"
project_data_files = "projects/distribution_contyp/data_files/eu_lfs/"

# LIBRARY
library(tidyverse)
library(data.table)
library(beepr)
library(countrycode)

# Variables data ----

# Demographic data
# AGE - Age of interviewed person
# SEX - Sex
# MARSTAT - Marital status
# COUNTRYB - Country of birth
# ILOSTAT - Labour status during the reference week
# FTPT - Full-time / Part-time distinction
# HATLEVEL - Highest educational attainment level
# INCDECIL - Monthly (take home) pay from main job (deciles) *
# COEFFY - Yearly weighting factor (also called COEFF in yearly files)
# TEMP - Permanency of the job

# Load data ----

df_country <- data.frame() # create empty data frame

folders <- list.files(path = orig_data_files, pattern="_YEAR_1983_", full.names=FALSE) # scan all files in a folder
for(x in seq_along(folders)) {
        df_country_pre_1998 <- data.frame() # create empty data frame
        print(paste0(orig_data_files,folders[x])) 
        files <- list.files(path = paste0(orig_data_files,folders[x]), pattern=".csv", full.names=FALSE) # scan all files in a folder
        for(y in seq_along(files)) {
                print(paste0(orig_data_files,folders[x],"/",files[y]))
                df <- read.csv(file = paste0(orig_data_files,folders[x],"/",files[y]))
                df <- select(df, COUNTRY, YEAR, TEMP, AGE, SEX, MARSTAT, COUNTRYB, ILOSTAT, FTPT, HATLEV1D, COEFF)
                c <- df$COUNTRY[1]
                df_country_pre_1998 <- rbind(df_country_pre_1998,df)
                df_country_pre_1998 <- df_country_pre_1998
        }
        df_country <- rbind(df_country,df_country_pre_1998)
}
rm(df_country_pre_1998)

folders <- list.files(path = orig_data_files, pattern="_YEAR_1998_", full.names=FALSE) # scan all files in a folder
for(x in seq_along(folders)) {
        df_country_pst_1998 <- data.frame() # create empty data frame
        print(paste0(orig_data_files,folders[x])) 
        files <- list.files(path = paste0(orig_data_files,folders[x]), pattern=".csv", full.names=FALSE) # scan all files in a folder
        for(y in seq_along(files)) {
                print(paste0(orig_data_files,folders[x],"/",files[y]))
                df <- read.csv(file = paste0(orig_data_files,folders[x],"/",files[y]))
                df <- select(df, COUNTRY, YEAR, TEMP, AGE, SEX, MARSTAT, COUNTRYB, ILOSTAT, FTPT, HATLEV1D, COEFF)
                c <- df$COUNTRY[1]
                df_country_pst_1998 <- rbind(df_country_pst_1998,df)
        }
        df_country <- rbind(df_country,df_country_pst_1998)
}
rm(df_country_pst_1998)

# Clean ----

df_eu_lfs <- df_country %>%
        mutate(COUNTRY = ifelse(COUNTRY == "UK", yes = "GB", no = COUNTRY))

df_eu_lfs$COUNTRY_NAME <- countrycode(sourcevar = df_eu_lfs$COUNTRY, destination = "country.name", origin = "genc2c")

names(df_eu_lfs) <- tolower(names(df_eu_lfs))

# Save ----

saveRDS(df_eu_lfs, file = paste0(project_data_files, "df_eu_lfs.rds"))

# Save sample ----

set.seed(1234)

df_eu_lfs_sample_10 <- df_eu_lfs %>%
        group_by(country, year) %>%
        sample_frac(.1) %>%
        ungroup()

saveRDS(df_eu_lfs_sample_10, file = paste0(project_data_files, "df_eu_lfs_sample_10.rds"))

df_eu_lfs_sample_1 <- df_eu_lfs %>%
        group_by(country, year) %>%
        sample_frac(.01) %>%
        ungroup()

saveRDS(df_eu_lfs_sample_1, file = paste0(project_data_files, "df_eu_lfs_sample_1.rds"))

beep()


