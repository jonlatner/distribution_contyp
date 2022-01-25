# Top commands --------------------------------------------------------------

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
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/distribution_contyp/")
# setwd("~/GitHub/distribution_contyp/")

graphs = "graphs/eu_silc/"
results = "results/eu_silc/"

# LIBRARY
library(tidyverse)
library(writexl)

options(scipen=999)

# Load ----
# ever
df_table_eu_ever <- readRDS(file = paste0(results,"df_table_glm_ever_eu_wt.rds"))
df_table_region_ever <- readRDS(file = paste0(results,"df_table_glm_ever_region_wt.rds"))
df_table_country_ever <- readRDS(file = paste0(results,"df_table_glm_ever_country_wt.rds"))

# single contract
df_table_eu_num <- readRDS(file = paste0(results,"df_table_glm_num_eu_wt.rds"))
df_table_region_num <- readRDS(file = paste0(results,"df_table_glm_num_region_wt.rds"))
df_table_country_num <- readRDS(file = paste0(results,"df_table_glm_num_country_wt.rds"))

# multi-year 
df_table_eu_dur <- readRDS(file = paste0(results,"df_table_glm_dur_eu_wt.rds"))
df_table_region_dur <- readRDS(file = paste0(results,"df_table_glm_dur_region_wt.rds"))
df_table_country_dur <- readRDS(file = paste0(results,"df_table_glm_dur_country_wt.rds"))

# Clean ----

df_table_ever <- rbind(df_table_eu_ever,df_table_region_ever,df_table_country_ever)
df_table_num <- rbind(df_table_eu_num,df_table_region_num,df_table_country_num)
df_table_dur <- rbind(df_table_eu_dur,df_table_region_dur,df_table_country_dur)

rm(df_table_eu_ever,df_table_region_ever,df_table_country_ever,
   df_table_eu_num,df_table_region_num,df_table_country_num,
   df_table_eu_dur,df_table_region_dur,df_table_country_dur)

# Save ----

write_xlsx(list("ever" = df_table_ever, "dur" = df_table_dur, "num" = df_table_num), paste0(results,"00_results.xlsx"))

