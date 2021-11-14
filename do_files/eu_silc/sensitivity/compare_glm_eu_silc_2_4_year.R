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
setwd("/Users/jonathanlatner/Google Drive/SECCOPA/projects/distribution_contyp/")
# setwd("/Users/jonathanlatner/Documents/GitHub/distribution_contyp/")

data_files = "data_files/eu_silc/"
results_4_year = "results/eu_silc/"
results_2_year = "results/eu_silc/2_year_panel/"
graphs = "graphs/eu_silc/sensitivity/"

# LIBRARY
library(tidyverse)
library(ggplot2)
library(forcats) #fct_reorder
library(beepr)
library(Hmisc)

options(scipen=999)

# Load/clean (4 year - unadjusted) --------------------------------------------------------------

df_eu_silc_clean_xc <- readRDS(file = paste0(data_files, "df_eu_silc_clean_xc.rds"))
df_eu_silc_clean_panel <- readRDS(file = paste0(data_files, "df_eu_silc_clean_panel.rds"))

df_eu_ever_unadjusted <- df_eu_silc_clean_xc %>%
        group_by(panel) %>%
        summarise(fit = mean(ftc_ever)) %>%
        ungroup() %>%
        mutate(geography = "EU-SILC", 
               source="4 year (unadjusted)")

df_region_ever_unadjusted <- df_eu_silc_clean_xc %>%
        group_by(panel,region) %>%
        summarise(fit = mean(ftc_ever)) %>%
        ungroup() %>%
        mutate(geography = region, 
               source="4 year (unadjusted)")

df_eu_num_unadjusted <- df_eu_silc_clean_xc %>%
        filter(ftc_num > 0) %>%
        mutate(ftc_num=ftc_num-1) %>%
        group_by(panel) %>%
        summarise(fit = mean(ftc_num)) %>%
        ungroup() %>%
        mutate(geography = "EU-SILC", 
               source="4 year (unadjusted)")

df_region_num_unadjusted <- df_eu_silc_clean_xc %>%
        filter(ftc_num > 0) %>%
        mutate(ftc_num=ftc_num-1) %>%
        group_by(panel,region) %>%
        summarise(fit = mean(ftc_num)) %>%
        ungroup() %>%
        mutate(geography = region, 
               source="4 year (unadjusted)")

df_eu_dur_unadjusted <- df_eu_silc_clean_panel %>%
        filter(ftc_dur > 0) %>%
        mutate(ftc_dur=ftc_dur-1) %>%
        group_by(panel) %>%
        summarise(fit = mean(ftc_dur)) %>%
        ungroup() %>%
        mutate(geography = "EU-SILC", 
               source="4 year (unadjusted)")

df_region_dur_unadjusted <- df_eu_silc_clean_panel %>%
        filter(ftc_dur > 0) %>%
        mutate(ftc_dur=ftc_dur-1) %>%
        group_by(panel,region) %>%
        summarise(fit = mean(ftc_dur)) %>%
        ungroup() %>%
        mutate(geography = region, 
               source="4 year (unadjusted)")

# Load (4 year) --------------------------------------------------------------
df_yhat_eu_ever_4_year <- readRDS(file = paste0(results_4_year,"df_yhat_glm_ever_eu_wt.rds")) %>%
        mutate(source="4 year")
df_yhat_region_ever_4_year <- readRDS(file = paste0(results_4_year,"df_yhat_glm_ever_region_wt.rds")) %>%
        mutate(source="4 year")
df_yhat_country_ever_4_year <- readRDS(file = paste0(results_4_year,"df_yhat_glm_ever_country_wt.rds")) %>%
        mutate(source="4 year")

df_yhat_eu_dur_4_year <- readRDS(file = paste0(results_4_year,"df_yhat_glm_dur_eu_wt.rds")) %>%
        mutate(source="4 year")
df_yhat_region_dur_4_year <- readRDS(file = paste0(results_4_year,"df_yhat_glm_dur_region_wt.rds")) %>%
        mutate(source="4 year")
df_yhat_country_dur_4_year <- readRDS(file = paste0(results_4_year,"df_yhat_glm_dur_country_wt.rds")) %>%
        mutate(source="4 year")

df_yhat_eu_num_4_year <- readRDS(file = paste0(results_4_year,"df_yhat_glm_num_eu_wt.rds")) %>%
        mutate(source="4 year")
df_yhat_region_num_4_year <- readRDS(file = paste0(results_4_year,"df_yhat_glm_num_region_wt.rds")) %>%
        mutate(source="4 year")
df_yhat_country_num_4_year <- readRDS(file = paste0(results_4_year,"df_yhat_glm_num_country_wt.rds")) %>%
        mutate(source="4 year")

# Load (2 year) --------------------------------------------------------------
df_yhat_eu_ever_2_year <- readRDS(file = paste0(results_2_year,"df_yhat_glm_ever_eu_wt.rds")) %>%
        mutate(source="2 year")
df_yhat_region_ever_2_year <- readRDS(file = paste0(results_2_year,"df_yhat_glm_ever_region_wt.rds")) %>%
        mutate(source="2 year")
df_yhat_country_ever_2_year <- readRDS(file = paste0(results_2_year,"df_yhat_glm_ever_country_wt.rds")) %>%
        mutate(source="2 year")

df_yhat_eu_dur_2_year <- readRDS(file = paste0(results_2_year,"df_yhat_glm_dur_eu_wt.rds")) %>%
        mutate(source="2 year")
df_yhat_region_dur_2_year <- readRDS(file = paste0(results_2_year,"df_yhat_glm_dur_region_wt.rds")) %>%
        mutate(source="2 year")
df_yhat_country_dur_2_year <- readRDS(file = paste0(results_2_year,"df_yhat_glm_dur_country_wt.rds")) %>%
        mutate(source="2 year")

df_yhat_eu_num_2_year <- readRDS(file = paste0(results_2_year,"df_yhat_glm_num_eu_wt.rds")) %>%
        mutate(source="2 year")
df_yhat_region_num_2_year <- readRDS(file = paste0(results_2_year,"df_yhat_glm_num_region_wt.rds")) %>%
        mutate(source="2 year")
df_yhat_country_num_2_year <- readRDS(file = paste0(results_2_year,"df_yhat_glm_num_country_wt.rds")) %>%
        mutate(source="2 year")

# Clean ever temporary --------------------------------------------------------------
df_yhat_eu_ever <- rbind(df_yhat_eu_ever_2_year,df_yhat_eu_ever_4_year)
df_yhat_region_ever <- rbind(df_yhat_region_ever_2_year,df_yhat_region_ever_4_year)
df_yhat_country_ever <- rbind(df_yhat_country_ever_2_year,df_yhat_country_ever_4_year)

df_yhat_eu_ever <- bind_rows(df_yhat_eu_ever,df_eu_ever_unadjusted)
df_yhat_region_ever <- bind_rows(df_yhat_region_ever,df_region_ever_unadjusted)
rm(df_yhat_eu_ever_2_year,df_yhat_eu_ever_4_year,df_yhat_region_ever_2_year,df_yhat_region_ever_4_year,df_yhat_country_ever_2_year,df_yhat_country_ever_4_year,df_eu_ever_unadjusted,df_region_ever_unadjusted)

df_yhat_eu_ever <- df_yhat_eu_ever %>%
        select(geography,fit,lwr,upr,panel,source) 

df_yhat_region_ever <- df_yhat_region_ever %>%
        select(geography,fit,lwr,upr,panel,source) 

df_yhat_country_ever <- df_yhat_country_ever %>%
        select(geography,fit,lwr,upr,panel,source) 

# Clean num temporary --------------------------------------------------------------
df_yhat_eu_num <- rbind(df_yhat_eu_num_2_year,df_yhat_eu_num_4_year)
df_yhat_region_num <- rbind(df_yhat_region_num_2_year,df_yhat_region_num_4_year)
df_yhat_country_num <- rbind(df_yhat_country_num_2_year,df_yhat_country_num_4_year)

df_yhat_eu_num <- bind_rows(df_yhat_eu_num,df_eu_num_unadjusted)
df_yhat_region_num <- bind_rows(df_yhat_region_num,df_region_num_unadjusted)
rm(df_yhat_eu_num_2_year,df_yhat_eu_num_4_year,df_yhat_region_num_2_year,df_yhat_region_num_4_year,df_yhat_country_num_2_year,df_yhat_country_num_4_year,df_eu_num_unadjusted,df_region_num_unadjusted)

df_yhat_eu_num <- df_yhat_eu_num %>%
        select(geography,fit,lwr,upr,panel,source) 

df_yhat_region_num <- df_yhat_region_num %>%
        select(geography,fit,lwr,upr,panel,source) 

df_yhat_country_num <- df_yhat_country_num %>%
        select(geography,fit,lwr,upr,panel,source) 

# Clean dur temporary --------------------------------------------------------------
df_yhat_eu_dur <- rbind(df_yhat_eu_dur_2_year,df_yhat_eu_dur_4_year)
df_yhat_region_dur <- rbind(df_yhat_region_dur_2_year,df_yhat_region_dur_4_year)
df_yhat_country_dur <- rbind(df_yhat_country_dur_2_year,df_yhat_country_dur_4_year)

df_yhat_eu_dur <- bind_rows(df_yhat_eu_dur,df_eu_dur_unadjusted)
df_yhat_region_dur <- bind_rows(df_yhat_region_dur,df_region_dur_unadjusted)
rm(df_yhat_eu_dur_2_year,df_yhat_eu_dur_4_year,df_yhat_region_dur_2_year,df_yhat_region_dur_4_year,df_yhat_country_dur_2_year,df_yhat_country_dur_4_year,df_eu_dur_unadjusted,df_region_dur_unadjusted)

df_yhat_eu_dur <- df_yhat_eu_dur %>%
        select(geography,fit,lwr,upr,panel,source) 

df_yhat_region_dur <- df_yhat_region_dur %>%
        select(geography,fit,lwr,upr,panel,source) 

df_yhat_country_dur <- df_yhat_country_dur %>%
        select(geography,fit,lwr,upr,panel,source) 

# Graph yhat ever --------------------------------------------------------------

df_yhat_ever <- rbind(df_yhat_eu_ever, df_yhat_region_ever)
df_yhat_ever$type <- 1
df_yhat_num <- rbind(df_yhat_eu_num, df_yhat_region_num)
df_yhat_num$type <- 2
df_yhat_dur <- rbind(df_yhat_eu_dur, df_yhat_region_dur)
df_yhat_dur$type <- 3

df_graph_yhat <- rbind(df_yhat_ever,df_yhat_num,df_yhat_dur)
rm(df_yhat_ever,df_yhat_num,df_yhat_dur)

df_graph_yhat$type <- factor(df_graph_yhat$type, labels=c("At least 1 (Ref: 0)", "Multiple contracts (Ref: 1)", "Multiple year (Ref: 1)"))

df_graph_yhat$geography <- fct_relevel(df_graph_yhat$geography, "Anglophone", after = Inf) # forcats
df_graph_yhat$geography <- fct_relevel(df_graph_yhat$geography, "Eastern", after = Inf) # forcats
df_graph_yhat$geography <- fct_relevel(df_graph_yhat$geography, "Continental", after = Inf) # forcats
df_graph_yhat$geography <- fct_relevel(df_graph_yhat$geography, "Nordic", after = Inf) # forcats
df_graph_yhat$geography <- fct_relevel(df_graph_yhat$geography, "Southern", after = Inf) # forcats
df_graph_yhat$geography <- fct_relevel(df_graph_yhat$geography, "EU-SILC", after = 0) # forcats

ggplot(data = df_graph_yhat, aes(x = panel, y = fit, linetype = source, color = source, group = source)) +
        facet_grid(type~geography, scales = "free_y") + 
        geom_line() +
        scale_x_continuous(breaks = c(2008,2013,2019), limits = c(2006, 2021)) +
        scale_linetype_manual(values = c("solid","solid","dashed")) +
        scale_color_manual(values = c("gray50","black","black")) +
        geom_errorbar(aes(ymin = lwr,
                          ymax = upr
        ),
        width=.2) +
        ylab("Predicted probability") +
        xlab("Panel wave ending") + 
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.box.margin=margin(-10,0,0,0),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.box = "vertical",
              legend.position = "bottom"
        ) 

ggsave(filename = paste0(graphs,"graph_eu_silc_compare_2_4_year_panel.pdf"), plot = last_plot(), height = 6, width = 9, units = "in")
