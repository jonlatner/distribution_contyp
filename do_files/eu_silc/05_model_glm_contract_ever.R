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
<<<<<<< HEAD
setwd("/Users/jonathanlatner/Google Drive/SECCOPA/projects/distribution_contyp/")
# setwd("/Users/jonathanlatner/Documents/GitHub/distribution_contyp/")
=======
setwd("/Users/jonathanlatner/Documents/GitHub/distribution_contyp/")
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e

data_files = "data_files/eu_silc/"
results = "results/eu_silc/"

# LIBRARY
library(dplyr)
<<<<<<< HEAD
library(car) # recode
=======
library(car)
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e
library(margins)
library(broom) # tidy
library(prediction)
library(survey)

options(scipen=999)

<<<<<<< HEAD
# Load data --------------------------------------------------------------

df_eu_silc <- readRDS(file = paste0(data_files,"df_eu_silc_clean_xc.rds"))

# LPM --------------------------------------------------------------

# Country
country <- unique(df_eu_silc$country_name)
df_table_country = data.frame() # output
df_yhat_country = data.frame() # predict
df_mfx_country = data.frame() # mfx
for(c in country) {
  df_country <- filter(df_eu_silc, country_name == c)
  print(c)
  
  panel <- unique(sort(df_country$panel))
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
    beta <- svyglm(ftc_ever ~ male + age_cat + edu_cat, my_svy, family = "binomial")
    
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

rm(my_svy,beta,table,yhat,mfx,df_panel,df_new_data_yhat,p,panel,country,c,df_country)
=======
# McFadden's R2
# https://stats.stackexchange.com/questions/82105/mcfaddens-pseudo-r2-interpretation

# AUC
# https://pdfs.semanticscholar.org/5c2f/4494f7ff8b93433f3432a4ce271586f6d56f.pdf
# http://gim.unmc.edu/dxtests/roc3.htm

# Load data --------------------------------------------------------------

df_eu_silc <- readRDS(file = paste0(data_files,"df_eu_silc_clean_xc.rds"))
table(df_eu_silc$panel)
t <- table(df_eu_silc$country)

# Clean data  -----------------------------------------

df_eu_silc <- within(df_eu_silc, edu_cat <- relevel(edu_cat, ref = "2"))
df_eu_silc <- within(df_eu_silc, age_cat <- relevel(age_cat, ref = "2"))

df_eu_silc <- df_eu_silc %>%
  mutate(ftc_c_0=ftc_ever_c)

df_eu_silc <- df_eu_silc %>%
  select(country_name, panel, year, edu_cat, male, age_cat, matches("ftc_c"), weight_long_4) %>%
  rename(country=country_name)

df_eu_silc <- droplevels(df_eu_silc)

country <- c("Germany", "Switzerland", "Luxembourg", "Belgium", "Austria", "Netherlands", "France", 
             "United Kingdom", "Ireland", 
             "Malta", "Greece", "Italy", "Cyprus", "Portugal", "Spain", 
             "Romania", "Poland", "Croatia", "Hungary", "Czechia", "Bulgaria", "Slovenia", "Slovakia", "Lithuania", "Estonia", "Latvia", "Serbia", "Turkey",
             "Sweden", "Denmark", "Norway", "Finland", "Iceland")
country_code <- c("DE", "CH", "LU", "BE", "AT", "NL", "FR", 
                  "GB", "IE", 
                  "MT", "GR", "IT", "CY", "PT", "ES", 
                  "RO", "PL", "HR", "HU", "CZ", "BG", "SI", "SK", "LT", "EE", "LV", "RS", "TK",
                  "SE", "DK", "NO", "FI", "IS")
region <- c("Continental", "Continental", "Continental", "Continental", "Continental", "Continental", "Continental", 
            "Anglophone", "Anglophone", 
            "Southern", "Southern", "Southern", "Southern", "Southern", "Southern", 
            "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern",
            "Nordic", "Nordic", "Nordic", "Nordic", "Nordic")

geography <- cbind(country, country_code, region)

df_eu_silc <- merge(df_eu_silc,geography)

rm(country, country_code, region)

table(df_eu_silc$country, useNA = "ifany")
table(df_eu_silc$region, useNA = "ifany")

# Sample --------------------------------------------------------------
set.seed(1234)

n <- c(0)

# LPM --------------------------------------------------------------
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e

# EU
df_table_eu = data.frame() # output
df_yhat_eu = data.frame() # predict
df_mfx_eu = data.frame() # mfx
<<<<<<< HEAD
panel <- unique(sort(df_eu_silc$panel))
=======
df_model_fit_eu <- data.frame(auc= numeric(0), psuedo_r2= integer(0))
panel <- unique(df_eu_silc$panel)
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e
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
<<<<<<< HEAD
                      weights =~ weight_long)
  beta <- svyglm(ftc_ever ~ male + age_cat + edu_cat, my_svy, family = "binomial")
  
  # summary table
  table <- tidy(beta)
=======
                      weights =~ weight_long_4)
  beta <- svyglm(ftc_c_0 ~ male + age_cat + edu_cat, my_svy, family = "binomial")
  
  # model fit
  # psuedo_r2 = pR2(beta)["McFadden"]
  # df_panel$yhat <- predict(beta,type="response")
  # auc <- roc(df_panel$ftc_c_0, df_panel$yhat)$auc
  # model_fit <- data.frame(cbind(auc,psuedo_r2))
  # rownames(model_fit) <- NULL
  # model_fit$num <- n
  # model_fit$geography <- "EU-SILC"
  # model_fit$panel <- p
  # df_model_fit_eu <- rbind(df_model_fit_eu,model_fit)
  # rm(auc,psuedo_r2,model_fit)
  
  # summary table
  table <- tidy(beta)
  table$num <- n
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e
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
<<<<<<< HEAD
=======
  yhat$num <- n
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e
  yhat$panel <- p
  yhat$geography <- "EU-SILC"
  yhat
  
  df_yhat_eu <- rbind(df_yhat_eu,yhat)
  
  # margins
  mfx <- summary(margins(beta,
                         design = my_svy
  ))
<<<<<<< HEAD
=======
  mfx$num <- n
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e
  mfx$panel <- p
  mfx$geography <- "EU-SILC"
  df_mfx_eu <- rbind(df_mfx_eu,mfx)
  print(p)
}

<<<<<<< HEAD
rm(my_svy,beta,table,yhat,mfx,df_panel,df_new_data_yhat,p,panel)
=======
df_model_fit_eu

rm(beta,table,yhat,mfx,df_panel,df_new_data_yhat,p,panel,df_panel,psuedo_r2,auc)
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e

df_mfx_eu <- arrange(df_mfx_eu,factor,panel)

# Regions
region <- unique(df_eu_silc$region)
df_table_region = data.frame() # output
df_yhat_region = data.frame() # predict
df_mfx_region = data.frame() # mfx
<<<<<<< HEAD
=======
df_model_fit_region <- data.frame(auc= numeric(0), psuedo_r2= integer(0))
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e
for(r in region) {
  df_region <- filter(df_eu_silc, region == r)
  print(r)
  
<<<<<<< HEAD
  panel <- unique(sort(df_region$panel))
=======
  panel <- unique(df_region$panel)
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e
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
<<<<<<< HEAD
                        weights =~ weight_long)
    beta <- svyglm(ftc_ever ~ male + age_cat + edu_cat, my_svy, family = "binomial")
    
    # summary table
    table <- tidy(beta)
=======
                        weights =~ weight_long_4)
    beta <- svyglm(ftc_c_0 ~ male + age_cat + edu_cat, my_svy, family = "binomial")
    
    # model fit
    # psuedo_r2 = pR2(beta)["McFadden"]
    # df_panel$yhat <- predict(beta,type="response")
    # auc <- roc(df_panel$ftc_c_0, df_panel$yhat)$auc
    # model_fit <- data.frame(cbind(auc,psuedo_r2))
    # rownames(model_fit) <- NULL
    # model_fit$num <- n
    # model_fit$geography <- r
    # model_fit$panel <- p
    # df_model_fit_region <- rbind(df_model_fit_region,model_fit)
    # rm(auc,psuedo_r2,model_fit)

    # summary table
    table <- tidy(beta)
    table$num <- n
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e
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
<<<<<<< HEAD
=======
    yhat$num <- n
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e
    yhat$geography <- r
    yhat$panel <- p
    df_yhat_region <- rbind(df_yhat_region,yhat)
    
    # margins
    mfx <- summary(margins(beta,
                           design = my_svy
    ))
<<<<<<< HEAD
=======
    mfx$num <- n
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e
    mfx$geography <- r
    mfx$panel <- p
    df_mfx_region <- rbind(df_mfx_region,mfx)
  }  
}

<<<<<<< HEAD
rm(my_svy,beta,table,yhat,mfx,df_panel,df_new_data_yhat,p,panel,df_region,r,region)

=======
rm(beta,table,yhat,mfx,df_region,r,region,p,panel,df_panel,df_new_data_yhat)

# Country
country <- unique(df_eu_silc$country)
df_table_country = data.frame() # output
df_yhat_country = data.frame() # predict
df_mfx_country = data.frame() # mfx
df_model_fit_country <- data.frame(auc= numeric(0), psuedo_r2= integer(0))
for(c in country) {
  df_country <- filter(df_eu_silc, country == c)
  print(c)
  
  panel <- unique(df_country$panel)
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
                        weights =~ weight_long_4)
    beta <- svyglm(ftc_c_0 ~ male + age_cat + edu_cat, my_svy, family = "binomial")
    
    # model fit
    # psuedo_r2 = pR2(beta)["McFadden"]
    # df_panel$yhat <- predict(beta,type="response")
    # auc <- roc(df_panel$ftc_c_0, df_panel$yhat)$auc
    # model_fit <- data.frame(cbind(auc,psuedo_r2))
    # rownames(model_fit) <- NULL
    # model_fit$num <- n
    # model_fit$geography <- c
    # model_fit$panel <- p
    # df_model_fit_country <- rbind(df_model_fit_country,model_fit)
    # rm(auc,psuedo_r2,model_fit)

    # summary table
    table <- tidy(beta)
    table$num <- n
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
    yhat$num <- n
    yhat$geography <- c
    yhat$panel <- p
    df_yhat_country <- rbind(df_yhat_country,yhat)

    # margins
    mfx <- summary(margins(beta,
                           design = my_svy
    ))
    mfx$num <- n
    mfx$geography <- c
    mfx$panel <- p
    df_mfx_country <- rbind(df_mfx_country,mfx)
  }
}

rm(beta,table,yhat,mfx,df_country,c,country,df_panel,p,panel,df_new_data_yhat)
>>>>>>> c13cc683d58889aaba8f5471ecad5c7a881d390e

# Save --------------------------------------------------------------

saveRDS(df_yhat_eu, file = paste0(results, "df_yhat_glm_ever_eu_wt.rds"))
saveRDS(df_yhat_region, file = paste0(results, "df_yhat_glm_ever_region_wt.rds"))
saveRDS(df_yhat_country, file = paste0(results, "df_yhat_glm_ever_country_wt.rds"))

saveRDS(df_table_eu, file = paste0(results, "df_table_glm_ever_eu_wt.rds"))
saveRDS(df_table_region, file = paste0(results, "df_table_glm_ever_region_wt.rds"))
saveRDS(df_table_country, file = paste0(results, "df_table_glm_ever_country_wt.rds"))

saveRDS(df_mfx_eu, file = paste0(results, "df_mfx_glm_ever_eu_wt.rds"))
saveRDS(df_mfx_region, file = paste0(results, "df_mfx_glm_ever_region_wt.rds"))
saveRDS(df_mfx_country, file = paste0(results, "df_mfx_glm_ever_country_wt.rds"))
