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
setwd("/Users/jonathanlatner/Documents/GitHub/distribution_contyp/")

results_4_year = "results/eu_silc/"
results_2_year = "results/eu_silc/2_year_panel/"
graphs = "graphs/eu_silc/sensitivity/"

# LIBRARY
library(tidyverse)
library(ggplot2)
library(forcats) #fct_reorder
library(beepr)

options(scipen=999)

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

rm(df_yhat_eu_ever_2_year,df_yhat_eu_ever_4_year,df_yhat_region_ever_2_year,df_yhat_region_ever_4_year,df_yhat_country_ever_2_year,df_yhat_country_ever_4_year)

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

rm(df_yhat_eu_num_2_year,df_yhat_eu_num_4_year,df_yhat_region_num_2_year,df_yhat_region_num_4_year,df_yhat_country_num_2_year,df_yhat_country_num_4_year)

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

rm(df_yhat_eu_dur_2_year,df_yhat_eu_dur_4_year,df_yhat_region_dur_2_year,df_yhat_region_dur_4_year,df_yhat_country_dur_2_year,df_yhat_country_dur_4_year)

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

df_graph_yhat$type <- factor(df_graph_yhat$type, labels=c("Ever (Ref: Never)", "Multiple contracts (Ref: Single)", "Multiple year (Ref: Single)"))

df_graph_yhat$geography <- fct_relevel(df_graph_yhat$geography, "Anglophone", after = Inf) # forcats
df_graph_yhat$geography <- fct_relevel(df_graph_yhat$geography, "Eastern", after = Inf) # forcats
df_graph_yhat$geography <- fct_relevel(df_graph_yhat$geography, "Continental", after = Inf) # forcats
df_graph_yhat$geography <- fct_relevel(df_graph_yhat$geography, "Nordic", after = Inf) # forcats
df_graph_yhat$geography <- fct_relevel(df_graph_yhat$geography, "Southern", after = Inf) # forcats
df_graph_yhat$geography <- fct_relevel(df_graph_yhat$geography, "EU-SILC", after = 0) # forcats

ggplot(data = df_graph_yhat, aes(x = panel, y = fit, color = source)) +
        facet_grid(type~geography, scales = "free_y") + 
        geom_line() +
        scale_x_continuous(breaks = c(2008,2013,2019), limits = c(2006, 2021)) +
        scale_color_manual(values = c("gray50","black")) +
        geom_errorbar(aes(ymin = lwr,
                          ymax = upr
        ),
        width=.2) +
        ylab("Predicted probability") +
        xlab("Panel wave ending") + 
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              axis.text.x = element_text(size=7),
              legend.key.width = unit(2,"cm"),
              legend.box = "vertical",
              legend.position = "bottom"
        ) +
        geom_text(data = df_graph_yhat, 
                  size = 2, 
                  aes(x = panel, y = ifelse(panel %in% c(2008,2013,2019) & source=="4 year", yes = fit, no = NA),
                      vjust=-3,
                      label=sprintf(fit, fmt = '%#.3f'))) +
        geom_text(data = df_graph_yhat, 
                  size = 2, 
                  aes(x = panel, y = ifelse(panel %in% c(2008,2013,2019) & source=="2 year", yes = fit, no = NA),
                      vjust=+3,
                      label=sprintf(fit, fmt = '%#.3f')))
        
ggsave(filename = paste0(graphs,"graph_eu_silc_compare_2_4_year_panel.pdf"), plot = last_plot(), height = 8, width = 6, units = "in")
