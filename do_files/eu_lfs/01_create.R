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
setwd("Users/jonathanlatner/Documents/GitHub/distribution_contyp/")

orig_data_files = "orig_data_files/eu_lfs/"
project_data_files = "data_files/eu_lfs/"

# LIBRARY
library(dplyr)
library(data.table)
library(countrycode)

# Variables data -----------------------------------------

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

# Load data -----------------------------------------

country <- c("EE","PL","RO")
year <- c(1997)
for(c in seq_along(country)) {
        test <- data.frame()
        print(country[c])
        for(yr in seq_along(year)) {
                print(year[yr])
                df <- read.csv(file = paste0(orig_data_files,"YearlyFiles_1983_1997/",country[c],"_YEAR_1983_1997/",country[c],year[yr],"_y.csv"))
                df <- select(df, TEMP, AGE, SEX, MARSTAT, COUNTRYB, ILOSTAT, FTPT, HATLEV1D, COEFF)
                df$year <- year[yr]
                df$country <- country[c]
                test <- rbind(test,df)
        }
        assign(paste0("df_pre_1998_",country[c]), test)
        rm(test,df)
}

country <- c("AT","BE","CH","DE","DK","ES","FI","FR","GR","HU","IE","IS","IT","LU","NL","NO","PT","SE","SI","UK")
year <- seq(1996,1997,1)
for(c in seq_along(country)) {
        test <- data.frame()
        print(country[c])
        for(yr in seq_along(year)) {
                print(year[yr])
                df <- read.csv(file = paste0(orig_data_files,"YearlyFiles_1983_1997/",country[c],"_YEAR_1983_1997/",country[c],year[yr],"_y.csv"))
                df <- select(df, TEMP, AGE, SEX, MARSTAT, COUNTRYB, ILOSTAT, FTPT, HATLEV1D, COEFF)
                df$year <- year[yr]
                df$country <- country[c]
                test <- rbind(test,df)
        }
        assign(paste0("df_pre_1998_",country[c]), test)
        rm(test,df)
}

country <- c("AT","BE","CH","DE","DK","EE","ES","FI","FR","GR","HU","IE","IS","IT","LU","NL","NO","PL","PT","RO","SE","SI","UK")
year <- seq(1998,2018,1)
for(c in seq_along(country)) {
        test <- data.frame()
        print(country[c])
        for(yr in seq_along(year)) {
                print(year[yr])
                df <- read.csv(file = paste0(orig_data_files,"YearlyFiles_1998_2018/",country[c],"_YEAR_1998_onwards/",country[c],year[yr],"_y.csv"))
                df <- select(df, TEMP, AGE, SEX, MARSTAT, COUNTRYB, ILOSTAT, FTPT, HATLEV1D, COEFF)
                df$year <- year[yr]
                df$country <- country[c]
                test <- rbind(test,df)
        }
        assign(paste0("df_post_1997_",country[c]), test)
        rm(test,df)
}

country <- c("LT","LV","SK")
year <- seq(1998,2018,1)
for(c in seq_along(country)) {
        test <- data.frame()
        print(country[c])
        for(yr in seq_along(year)) {
                print(year[yr])
                df <- read.csv(file = paste0(orig_data_files,"YearlyFiles_1998_2018/",country[c],"_YEAR_1998_onwards/",country[c],year[yr],"_y.csv"))
                df <- select(df, TEMP, AGE, SEX, MARSTAT, COUNTRYB, ILOSTAT, FTPT, HATLEV1D, COEFF)
                df$year <- year[yr]
                df$country <- country[c]
                test <- rbind(test,df)
        }
        assign(paste0("df_",country[c]), test)
        rm(test,df)
}

country <- c("CY")
year <- seq(1999,2018,1)
for(c in seq_along(country)) {
        test <- data.frame()
        print(country[c])
        for(yr in seq_along(year)) {
                print(year[yr])
                df <- read.csv(file = paste0(orig_data_files,"YearlyFiles_1998_2018/",country[c],"_YEAR_1998_onwards/",country[c],year[yr],"_y.csv"))
                df <- select(df, TEMP, AGE, SEX, MARSTAT, COUNTRYB, ILOSTAT, FTPT, HATLEV1D, COEFF)
                df$year <- year[yr]
                df$country <- country[c]
                test <- rbind(test,df)
        }
        assign(paste0("df_",country[c]), test)
        rm(test,df)
}

country <- c("HR")
year <- seq(2002,2018,1)
for(c in seq_along(country)) {
        test <- data.frame()
        print(country[c])
        for(yr in seq_along(year)) {
                print(year[yr])
                df <- read.csv(file = paste0(orig_data_files,"YearlyFiles_1998_2018/",country[c],"_YEAR_1998_onwards/",country[c],year[yr],"_y.csv"))
                df <- select(df, TEMP, AGE, SEX, MARSTAT, COUNTRYB, ILOSTAT, FTPT, HATLEV1D, COEFF)
                df$year <- year[yr]
                df$country <- country[c]
                test <- rbind(test,df)
        }
        assign(paste0("df_",country[c]), test)
        rm(test,df)
}
country <- c("MT")
year <- seq(2009,2018,1)
for(c in seq_along(country)) {
        test <- data.frame()
        print(country[c])
        for(yr in seq_along(year)) {
                print(year[yr])
                df <- read.csv(file = paste0(orig_data_files,"YearlyFiles_1998_2018/",country[c],"_YEAR_1998_onwards/",country[c],year[yr],"_y.csv"))
                df <- select(df, TEMP, AGE, SEX, MARSTAT, COUNTRYB, ILOSTAT, FTPT, HATLEV1D, COEFF)
                df$year <- year[yr]
                df$country <- country[c]
                test <- rbind(test,df)
        }
        assign(paste0("df_",country[c]), test)
        rm(test,df)
}

# Append -----------------------------------------
country <- c("AT","BE","CH","DE","DK","EE","ES","FI","FR","GR","HU","IE","IS","IT","LU","NL","NO","PL","PT","RO","SE","SI","UK")
for(c in seq_along(country)) {
        test_1 <- get(paste0("df_pre_1998_",country[c]))
        test_2 <- get(paste0("df_post_1997_",country[c]))
        test <- rbind(test_1,test_2)
        assign(paste0("df_",country[c]), test)
}

country <- c("AT","BE","CH","CY","DE","DK","EE","ES","FI","FR","GR","HR","HU","IE","IS","IT","LT","LU","LV","MT","NL","NO","PL","PT","RO","SE","SI","SK","UK")
df_eu_lfs <- data.frame()
for(c in seq_along(country)) {
        test <- get(paste0("df_",country[c]))
        df_eu_lfs <- rbind(df_eu_lfs,test)
}


# Clean -----------------------------------------

df_eu_lfs <- df_eu_lfs %>%
        mutate(country = ifelse(country == "UK", yes = "GB", no = country))

df_eu_lfs$country_name <- countrycode(sourcevar = df_eu_lfs$country, destination = "country.name", origin = "genc2c")

# Save -----------------------------------------

saveRDS(df_eu_lfs, file = paste0(project_data_files, "df_eu_lfs.rds"))

# Save sample -----------------------------------------

df_eu_lfs_sample_10 <- df_eu_lfs %>%
        group_by(country, year) %>%
        sample_frac(.1) %>%
        ungroup()

saveRDS(df_eu_lfs_sample_10, file = paste0(project_data_files, "df_eu_lfs_sample_10.rds"))

