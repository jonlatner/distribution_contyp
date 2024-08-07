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

data_files = "data_files/eu_silc/"
results = "results/eu_silc/"

# LIBRARY
library(tidyverse)
library(car) # recode
library(margins)
library(broom) # tidy
library(prediction)
library(survey)

options(scipen=999)

# Load data ----

df_eu_silc <- readRDS(file = paste0(data_files,"03_df_eu_silc_clean_panel.rds"))

# Clean data  -----------------------------------------
table(df_eu_silc$ftc_dur)

# Condition on ever having a temporary employment contract
df_eu_silc <- df_eu_silc %>%
  filter(ftc_dur > 0)

#tranform ftc_dur into categorical dummy variables
df_eu_silc$ftc_dur <- recode(df_eu_silc$ftc_dur, "1=0;2:hi=1")

# LPM ----

# Country
country <- unique(sort(df_eu_silc$country_name))
country <- droplevels(country)
df_table_country = data.frame() # output
df_yhat_country = data.frame() # predict
df_mfx_country = data.frame() # mfx
for(c in country) {
  df_country <- filter(df_eu_silc, country_name == c)
  print(c)
  
  t <- data.frame(with(df_country,table(panel)))
  t$country <- c
  
  # It is necessary to drop a few country, years because model does not run due to small number of observations that combine age,edu,gender
  t <- t %>%
    mutate(test = ifelse((country == "Denmark" & (panel == 2014|panel == 2018))
                         | (country == "Estonia" & (panel == 2008|panel == 2014|panel==2019))
                         | (country == "Ireland" & (panel == 2014))
                         | (country == "Latvia" & (panel == 2015|panel == 2017|panel == 2018|panel==2019))
                         | (country == "Malta" & (panel == 2009))
                         | (country == "Sweden" & (panel == 2014|panel==2016))
                         | (country == "United Kingdom" & (panel == 2012|panel==2013))
                         | (country == "Norway" & (panel==2015|panel==2017))
                         ,
                         yes = 0, no = 1)) %>%
    filter(test == 1) %>%
    mutate(panel = as.numeric(as.character(panel)))
  t <- droplevels(t)
  
  panel <- unique(sort(t$panel))
  for(p in panel) {
    df_panel <- filter(df_country, panel == p)
    print(p)
    
    df_new_data_yhat <- data.frame(
      unique(
        data.frame(
          male = "0",
          age_cat = "2",
          edu_cat = "2")), 
      row.names = NULL)
    
    my_svy <- svydesign(data = df_panel, 
                        ids = ~1, # no clusters 
                        weights =~ weight_long)
    beta <- svyglm(ftc_dur ~ male + age_cat + edu_cat, my_svy, family = "binomial")
    
    # summary table
    table <- tidy(beta)
    table$geography <- c
    table$panel <- p
    df_table_country <- rbind(df_table_country,table)
    
    # predict - country level change
    yhat <- prediction(beta, data = df_new_data_yhat, calculate_se = TRUE)
    yhat <- yhat %>%
      select(fitted, se.fitted) %>%
      rename(fit = fitted) %>%
      mutate(lwr = fit - 1.96*se.fitted,
             upr = fit + 1.96*se.fitted) %>%
      select(-se.fitted)
    yhat <- cbind(df_new_data_yhat,yhat)
    yhat$geography <- c
    yhat$panel <- p
    df_yhat_country <- rbind(df_yhat_country,yhat)
    
    # margins
    mfx <- summary(margins(beta,
                           design = my_svy
    ))
    mfx$geography <- c
    mfx$panel <- p
    df_mfx_country <- rbind(df_mfx_country,mfx)
  }
}

rm(beta,table,yhat,mfx,df_panel,df_new_data_yhat,p,panel,my_svy,t,df_country,c,country)


# EU
df_table_eu = data.frame() # output
df_yhat_eu = data.frame() # predict
df_mfx_eu = data.frame() # mfx
panel <- unique(df_eu_silc$panel)
for(p in panel) {
  df_panel <- filter(df_eu_silc, panel == p)

  df_new_data_yhat <- data.frame(
    unique(
      data.frame(
        male = "0",
        age_cat = "2",
        edu_cat = "2")), 
    row.names = NULL)
  
  my_svy <- svydesign(data = df_panel, 
                      ids = ~1, # no clusters 
                      weights =~ weight_long)
  beta <- svyglm(ftc_dur ~ male + age_cat + edu_cat, my_svy, family = "binomial")
  
  # summary table
  table <- tidy(beta)
  table$geography <- "EU-SILC"
  table$panel <- p
  df_table_eu <- rbind(df_table_eu,table)
  
  # predict - country level change
  yhat <- prediction(beta, data = df_new_data_yhat, calculate_se = TRUE)
  yhat <- yhat %>%
    select(fitted, se.fitted) %>%
    rename(fit = fitted) %>%
    mutate(lwr = fit - 1.96*se.fitted,
           upr = fit + 1.96*se.fitted) %>%
    select(-se.fitted)
  yhat <- cbind(df_new_data_yhat,yhat)
  yhat$panel <- p
  yhat$geography <- "EU-SILC"
  yhat
  
  df_yhat_eu <- rbind(df_yhat_eu,yhat)
  
  # margins
  mfx <- summary(margins(beta,
                         design = my_svy
  ))
  mfx$panel <- p
  mfx$geography <- "EU-SILC"
  df_mfx_eu <- rbind(df_mfx_eu,mfx)
  print(p)
}

rm(beta,table,yhat,mfx,df_panel,df_new_data_yhat,p,panel,my_svy)

# Regions
region <- unique(df_eu_silc$region)
df_table_region = data.frame() # output
df_yhat_region = data.frame() # predict
df_mfx_region = data.frame() # mfx
for(r in region) {
  df_region <- filter(df_eu_silc, region == r)
  print(r)
  
  panel <- unique(df_region$panel)
  for(p in panel) {
    df_panel <- filter(df_region, panel == p)
    print(p)
    
    df_new_data_yhat <- data.frame(
      unique(
        data.frame(
          male = "0",
          age_cat = "2",
          edu_cat = "2")), 
      row.names = NULL)
    
    my_svy <- svydesign(data = df_panel, 
                        ids = ~1, # no clusters 
                        weights =~ weight_long)
    beta <- svyglm(ftc_dur ~ male + age_cat + edu_cat, my_svy, family = "binomial")
  
    # summary table
    table <- tidy(beta)
    table$geography <- r
    table$panel <- p
    df_table_region <- rbind(df_table_region,table)
    
    # predict - country level change
    yhat <- prediction(beta, data = df_new_data_yhat, calculate_se = TRUE)
    yhat <- yhat %>%
      select(fitted, se.fitted) %>%
      rename(fit = fitted) %>%
      mutate(lwr = fit - 1.96*se.fitted,
             upr = fit + 1.96*se.fitted) %>%
      select(-se.fitted)
    yhat <- cbind(df_new_data_yhat,yhat)
    yhat$geography <- r
    yhat$panel <- p
    df_yhat_region <- rbind(df_yhat_region,yhat)
    
    # margins
    mfx <- summary(margins(beta,
                           design = my_svy
    ))
    mfx$geography <- r
    mfx$panel <- p
    df_mfx_region <- rbind(df_mfx_region,mfx)
  }  
}

rm(beta,table,yhat,mfx,df_panel,df_new_data_yhat,p,panel,my_svy,df_region,region,r)

# Save ----

saveRDS(df_yhat_eu, file = paste0(results, "df_yhat_glm_dur_eu_wt.rds"))
saveRDS(df_yhat_region, file = paste0(results, "df_yhat_glm_dur_region_wt.rds"))
saveRDS(df_yhat_country, file = paste0(results, "df_yhat_glm_dur_country_wt.rds"))

saveRDS(df_table_eu, file = paste0(results, "df_table_glm_dur_eu_wt.rds"))
saveRDS(df_table_region, file = paste0(results, "df_table_glm_dur_region_wt.rds"))
saveRDS(df_table_country, file = paste0(results, "df_table_glm_dur_country_wt.rds"))

saveRDS(df_mfx_eu, file = paste0(results, "df_mfx_glm_dur_eu_wt.rds"))
saveRDS(df_mfx_region, file = paste0(results, "df_mfx_glm_dur_region_wt.rds"))
saveRDS(df_mfx_country, file = paste0(results, "df_mfx_glm_dur_country_wt.rds"))

# beep()
