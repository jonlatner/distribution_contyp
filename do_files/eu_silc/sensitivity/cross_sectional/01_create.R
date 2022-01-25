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
setwd("~/GitHub/distribution_contyp/")

orig_data_files = "data/EU_SILC_2019/Cross_2004_19/data_files/"
project_data_files = "projects/distribution_contyp/data_files/eu_silc/cross_sectional/"

# LIBRARY
library(tidyverse)

# Variables data -----------------------------------------

# Personal data
# PB010 year
# PB020 country
# PB030 pid
# PB040 personal base weight
# PB140 birthy
# PB150 sex
# PE040 isced
# PL030 empst
# PL031 empst
# PL140 contyp

# Load personal data -----------------------------------------

year <- c(2004,2005,2006,2007,2008)
for(yr in seq_along(year)) {
        print(year[yr])
        df <- readRDS(file = paste0(orig_data_files,"pers_data_",year[yr],".rds"))
        df <- df %>%
                select(PB010, PB020, PB030, PB040, PB140, PB150, PE040, PL030, PL140) %>%
                mutate(PB010 = as.numeric(as.character(PB010)),
                       PL030_F = NA,
                       PL031 = NA,
                       PL031_F = NA)
        assign(paste0("df_pers_data_yr_",year[yr]), df)
}

year <- c(2009,2010)
for(yr in seq_along(year)) {
        print(year[yr])
        df <- readRDS(file = paste0(orig_data_files,"pers_data_",year[yr],".rds"))
        df <- df %>%
                select(PB010, PB020, PB030, PB040, PB140, PB150, PE040, PL030, PL030_F, PL031, PL031_F, PL140) %>%
                mutate(PB010 = as.numeric(as.character(PB010)))
        assign(paste0("df_pers_data_yr_",year[yr]), df)
}

year <- c(2011,2012,2013,2014,2015,2016,2017,2018,2019)
for(yr in seq_along(year)) {
        print(year[yr])
        df <- readRDS(file = paste0(orig_data_files,"pers_data_",year[yr],".rds"))
        df <- df %>%
                select(PB010, PB020, PB030, PB040, PB140, PB150, PE040, PL031, PL140) %>%
                mutate(PB010 = as.numeric(as.character(PB010)),
                       PL030 = NA,
                       PL030_F = NA,
                       PL031_F = NA
                )
        assign(paste0("df_pers_data_yr_",year[yr]), df)
}

# Append personal data -----------------------------------------

year <- c(2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
df_eu_silc = data.frame()
for (yr in seq_along(year)) {
        df <- get(paste0("df_pers_data_yr_", year[yr]))
        df_eu_silc <- rbind(df_eu_silc,df)
}

table(df_eu_silc$PB010,useNA = "ifany")

# Rename vars -----------------------------------------

df_eu_silc_1 <- df_eu_silc %>%
        rename(year = PB010,
               country = PB020,
               pid = PB030,
               weight_xc_base = PB040,
               birthy = PB140,
               sex = PB150,
               isced = PE040,
               empst_1 = PL030,
               empst_1_f = PL030_F,
               empst_2 = PL031,
               empst_2_f = PL031_F,
               contyp = PL140
               )

# Save -----------------------------------------

saveRDS(df_eu_silc_1, file = paste0(project_data_files, "df_eu_silc_xs.rds"))
