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
setwd("Users/jonathanlatner/Documents/GitHub/distribution_contyp/")

stata_data_files = "orig_data_files/eu_silc/data_files/stata/"
r_data_files = "orig_data_files/eu_silc/data_files/"

# LIBRARY
library(dplyr)
library(haven)

# Load/save data -----------------------------------------

year <- seq(2005,2019,1)

# Personal data
for(yr in seq_along(year)) {
        print(year[yr])
        df_stata <- read_dta(paste0(stata_data_files,"pers_data_",year[yr],".dta"))
        df_stata <- zap_formats(df_stata)
        df_stata <- zap_label(df_stata)
        df_stata <- zap_labels(df_stata)
        attr(df_stata, "label") <- NULL
        saveRDS(df_stata,paste0(r_data_files,"pers_data_",year[yr],".rds"))
}

# Household data
for(yr in seq_along(year)) {
        print(year[yr])
        df_stata <- read_dta(paste0(stata_data_files,"hhld_data_",year[yr],".dta"))
        df_stata <- zap_formats(df_stata)
        df_stata <- zap_label(df_stata)
        df_stata <- zap_labels(df_stata)
        attr(df_stata, "label") <- NULL
        saveRDS(df_stata,paste0(r_data_files,"hhld_data_",year[yr],".rds"))
}



# Personal register data
for(yr in seq_along(year)) {
        print(year[yr])
        df_stata <- read_dta(paste0(stata_data_files,"pers_reg_data_",year[yr],".dta"))
        df_stata <- zap_formats(df_stata)
        df_stata <- zap_label(df_stata)
        df_stata <- zap_labels(df_stata)
        attr(df_stata, "label") <- NULL
        saveRDS(df_stata,paste0(r_data_files,"pers_reg_data_",year[yr],".rds"))
}
