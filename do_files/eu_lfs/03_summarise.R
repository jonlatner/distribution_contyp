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

data_files = "data_files/eu_lfs/"
graphs = "graphs/eu_lfs/"

# LIBRARY
library(tidyverse)
library(Hmisc) # wtd.mean
library(forcats) # fct_relevel
library(cowplot) # plot_grid
library(grid) # y.grob
library(gridExtra) # grid.arrange


# Load data ----

# df_eu_lfs_0 <- readRDS(file = paste0(data_files, "df_eu_lfs_clean.rds"))
df_eu_lfs_0 <- readRDS(file = paste0(data_files, "df_eu_lfs_sample_1_clean.rds"))

# Summarize temporarey employment rate by year and geography (country, region, eu) ----

df_countries <- df_eu_lfs_0 %>%
        group_by(region, country_name, year) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Countries")

df_regions <- df_eu_lfs_0 %>%
        group_by(region, year) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country_name = region)

df_eu <- df_eu_lfs_0 %>%
        group_by(year) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "Europe", region = "Europe", level = "Region")

# Combine
df_summary_aggregate <- rbind(df_countries,df_regions,df_eu)
rm(df_countries,df_regions,df_eu)

# Summarize temporarey employment rate by year, demographic group, and geography (country, region, eu) ----

# Gender

df_countries <- df_eu_lfs_0 %>%
        group_by(region, country_name, year, female) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Countries")

df_regions <- df_eu_lfs_0 %>%
        group_by(region, year, female) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country_name = region)

df_eu <- df_eu_lfs_0 %>%
        group_by(year,female) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "Europe", region = "Europe", level = "Region")

df_gender <- rbind(df_countries,df_regions,df_eu) %>%
        rename(variable=female) %>%
        mutate(factor = "Gender")

# Age 

df_countries <- df_eu_lfs_0 %>%
        group_by(region, country_name, year, age_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Countries")

df_regions <- df_eu_lfs_0 %>%
        group_by(region, year, age_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country_name = region)

df_eu <- df_eu_lfs_0 %>%
        group_by(year,age_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "Europe", region = "Europe", level = "Region")

df_age <- rbind(df_countries,df_regions,df_eu) %>%
        rename(variable=age_cat) %>%
        mutate(factor = "Age")

# Edu 

df_countries <- df_eu_lfs_0 %>%
        group_by(region, country_name, year, edu_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Countries")

df_regions <- df_eu_lfs_0 %>%
        group_by(region, year, edu_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country_name = region)

df_eu <- df_eu_lfs_0 %>%
        group_by(year,edu_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "Europe", region = "Europe", level = "Region")

df_edu <- rbind(df_countries,df_regions,df_eu) %>%
        rename(variable=edu_cat) %>%
        mutate(factor = "Education")

# Combine
df_summary_disaggregate <- rbind(df_gender,df_age,df_edu)
rm(df_countries,df_regions,df_eu,df_gender,df_age,df_edu)

# Save ----

saveRDS(df_summary_aggregate, file = paste0(data_files, "df_summary.rds"))
saveRDS(df_summary_disaggregate, file = paste0(data_files, "df_summary_groups.rds"))
