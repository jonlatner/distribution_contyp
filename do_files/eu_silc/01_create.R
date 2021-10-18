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
<<<<<<< HEAD
setwd("/Users/jonathanlatner/Google Drive/SECCOPA/")
# setwd("C:/Users/ba1ks6/Google Drive/")

orig_data_files = "data/EU_SILC_2019/Long_2005_19/data_files/"
project_data_files = "projects/distribution_contyp/data_files/eu_silc/"
=======
setwd("/Users/jonathanlatner/Documents/GitHub/distribution_contyp/")

orig_data_files = "orig_data_files/eu_silc/"
project_data_files = "data_files/eu_silc/data_files/"
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e

# LIBRARY
library(dplyr)
library(data.table)

# Variables data -----------------------------------------

# Personal data
# PB010 year
# PB020 country
# PB030 pid
# PX030 household id
# PB050 personal base weight
# PB140 birthy
# PB150 sex
# PE040 isced
# PL030 empst
# PL031 empst
# PL140 contyp
# PY010G income
# PB180 partner_id
# PB190 marstat
# PL160 Change of job since last year

# Personal register
# RB010 year
# RB020 country
# RB030 pid
# RB060 weight_personal
# RB064 weight_long_4_yr_dur

# Load personal data -----------------------------------------

year <- c(2007,2008)
for(yr in seq_along(year)) {
        print(year[yr])
        df <- readRDS(file = paste0(orig_data_files,"pers_data_",year[yr],".rds"))
        df <- df %>%
                select(PB010, PB020, PB030, PB050, PB140, PB150, PL160, PE040, PL030, PL030_F, PL040, PL140, PY010G, PB180, PB190) %>%
                mutate(panel = year[yr],
                       PB010 = as.numeric(as.character(PB010)),
                       PL031 = NA,
                       PL031_F = NA)
        assign(paste0("df_pers_data_yr_",year[yr]), df)
}

year <- c(2009,2010,2011,2012)
for(yr in seq_along(year)) {
        print(year[yr])
        df <- readRDS(file = paste0(orig_data_files,"pers_data_",year[yr],".rds"))
        df <- df %>%
                select(PB010, PB020, PB030, PB050, PB140, PB150, PL160, PE040, PL030, PL030_F, PL031, PL031_F, PL040, PL140, PY010G, PB180, PB190) %>%
                mutate(panel = year[yr],
                       PB010 = as.numeric(as.character(PB010)))
        assign(paste0("df_pers_data_yr_",year[yr]), df)
}

year <- c(2013,2014,2015,2016,2017,2018,2019)
for(yr in seq_along(year)) {
        print(year[yr])
        df <- readRDS(file = paste0(orig_data_files,"pers_data_",year[yr],".rds"))
        df <- df %>%
                select(PB010, PB020, PB030, PB050, PB140, PB150, PL160, PE040, PL031, PL031_F, PL040, PL140, PY010G, PB180, PB190) %>%
                mutate(panel = year[yr],
                       PB010 = as.numeric(as.character(PB010)),
                       PL030 = NA,
                       PL030_F = NA)
        assign(paste0("df_pers_data_yr_",year[yr]), df)
}

# Append personal data -----------------------------------------

year <- c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
df_eu_silc_pers = data.frame()
for (yr in seq_along(year)) {
        df <- get(paste0("df_pers_data_yr_", year[yr]))
        df_eu_silc_pers <- rbind(df_eu_silc_pers,df)
}

table(df_eu_silc_pers$PB010,useNA = "ifany")

# rm(list=ls(pattern="df_pers_data")) # remove

# Load register data -----------------------------------------

year <- c(2007)
for(yr in seq_along(year)) {
        print(year[yr])
        df <- readRDS(file = paste0(orig_data_files,"pers_reg_data_",year[yr],".rds"))
        df <- df %>%
                select(RB010, RB020, RB030, RB040, RB060, RB062, RB063, RB064) %>%
                rename(PB010 = RB010,
                       PB020 = RB020,
                       PB030 = RB030,
                       RB064 = RB064) %>%
                mutate(panel = year[yr],
                       PB010 = as.numeric(as.character(PB010)))
        assign(paste0("df_pers_reg_data_yr_",year[yr]), df)
}

year <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
for(yr in seq_along(year)) {
        print(year[yr])
        df <- readRDS(file = paste0(orig_data_files,"pers_reg_data_",year[yr],".rds"))
        df <- df %>%
                select(RB010, RB020, RB030, RB040, RB060, RB062, RB063, RB064) %>%
                rename(PB010 = RB010,
                       PB020 = RB020,
                       PB030 = RB030) %>%
                mutate(panel = year[yr],
                       PB010 = as.numeric(as.character(PB010)))
        assign(paste0("df_pers_reg_data_yr_",year[yr]), df)
}

# Append register data -----------------------------------------

df_eu_silc_pers_reg = data.frame()
year <- c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
for (yr in seq_along(year)) {
        df <- get(paste0("df_pers_reg_data_yr_", year[yr]))
        df_eu_silc_pers_reg <- rbind(df_eu_silc_pers_reg,df)
}
table(df_eu_silc_pers_reg$PB010,useNA = "ifany")

# rm(list=ls(pattern="df_pers_reg_data")) # remove

# Merge and clean data environment -----------------------------------------

df_eu_silc <- merge(data.table(df_eu_silc_pers_reg),data.table(df_eu_silc_pers),by = c("PB010","PB020","PB030","panel"))

# rm(df,df_eu_silc_pers_reg,df_eu_silc_pers)

# Rename vars -----------------------------------------

df_eu_silc <- df_eu_silc %>%
        rename(year = PB010,
               country = PB020,
               pid = PB030,
               weight_xc_base = PB050,
               weight_personal_base = RB060,
               weight_long_2 = RB062,
               weight_long_3 = RB063,
               weight_long_4 = RB064, 
               birthy = PB140,
               sex = PB150,
               job_change = PL160,
               isced = PE040,
               empst_1 = PL030,
               empst_1_f = PL030_F,
               empst_2 = PL031,
               empst_2_f = PL031_F,
               self_empst = PL040,
               contyp = PL140,
               income = PY010G,
               marstat = PB190,
               partner_id = PB180,
               hid = RB040
               )

# Save -----------------------------------------

saveRDS(df_eu_silc, file = paste0(project_data_files, "df_eu_silc.rds"))
