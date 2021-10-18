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

# FOLDERS
setwd("/Users/jonathanlatner/Google Drive/")
# setwd("C:/Users/ba1ks6/Google Drive/")

results = "SECCOPA/projects/distribution_contyp/results/"
results_2_year = "SECCOPA/projects/distribution_contyp/results/2_year_panel/"
graphs = "SECCOPA/projects/distribution_contyp/graphs/"

# LIBRARY
library(tidyverse)
library(ggplot2)
library(forcats) #fct_reorder

options(scipen=999)

# Load (4 year) --------------------------------------------------------------
# ever
df_yhat_eu_ever_4_year <- readRDS(file = paste0(results,"df_yhat_glm_ever_eu_wt.rds")) %>%
        mutate(source="4 year")
df_yhat_region_ever_4_year <- readRDS(file = paste0(results,"df_yhat_glm_ever_region_wt.rds")) %>%
        mutate(source="4 year")
df_yhat_country_ever_4_year <- readRDS(file = paste0(results,"df_yhat_glm_ever_country_wt.rds")) %>%
        mutate(source="4 year")

# Load (2 year) --------------------------------------------------------------
# ever
df_yhat_eu_ever_2_year <- readRDS(file = paste0(results_2_year,"df_yhat_glm_ever_eu_wt.rds")) %>%
        mutate(source="2 year")
df_yhat_region_ever_2_year <- readRDS(file = paste0(results_2_year,"df_yhat_glm_ever_region_wt.rds")) %>%
        mutate(source="2 year")
df_yhat_country_ever_2_year <- readRDS(file = paste0(results_2_year,"df_yhat_glm_ever_country_wt.rds")) %>%
        mutate(source="2 year")


# Combine --------------------------------------------------------------
df_yhat_eu_ever <- rbind(df_yhat_eu_ever_2_year,df_yhat_eu_ever_4_year)
df_yhat_region_ever <- rbind(df_yhat_region_ever_2_year,df_yhat_region_ever_4_year)
df_yhat_country_ever <- rbind(df_yhat_country_ever_2_year,df_yhat_country_ever_4_year)

rm(df_yhat_eu_ever_2_year,df_yhat_eu_ever_4_year,df_yhat_region_ever_2_year,df_yhat_region_ever_4_year,df_yhat_country_ever_2_year,df_yhat_country_ever_4_year)

# Clean ever temporary --------------------------------------------------------------

df_yhat_eu_ever <- df_yhat_eu_ever %>%
        select(geography,fit,lwr,upr,panel,source) 

df_yhat_region_ever <- df_yhat_region_ever %>%
        select(geography,fit,lwr,upr,panel,source) 

df_yhat_country_ever <- df_yhat_country_ever %>%
        select(geography,fit,lwr,upr,panel,source) 

# Graph yhat ever --------------------------------------------------------------

df_graph <- rbind(df_yhat_eu_ever, df_yhat_region_ever,df_yhat_country_ever)

df_graph$geography <- fct_relevel(df_graph$geography, "Southern", after = 0) # forcats
df_graph$geography <- fct_relevel(df_graph$geography, "Nordic", after = 0) # forcats
df_graph$geography <- fct_relevel(df_graph$geography, "Eastern", after = 0) # forcats
df_graph$geography <- fct_relevel(df_graph$geography, "Continental", after = 0) # forcats
df_graph$geography <- fct_relevel(df_graph$geography, "Anglophone", after = 0) # forcats
df_graph$geography <- fct_relevel(df_graph$geography, "EU-SILC", after = 0) # forcats
table(df_graph$geography)

df_graph_wide <- df_graph %>%
        select(geography,panel,source,fit) %>%
        pivot_wider(names_from = c(source), values_from = c(fit)) %>%
        group_by(geography) %>%
        mutate(cor = round(cor(`2 year`,`4 year`, use = "complete.obs"),3)) %>%
        slice(1) %>%
        ungroup() %>%
        select(geography,cor)
df_graph_wide

df_graph <- merge(df_graph,df_graph_wide)

df_graph_2 <- df_graph %>%
        filter(geography!="Serbia",
               # geography!="Switzerland",
               geography!="Norway",
        )
ggplot(data = df_graph_2, aes(x = panel, y = fit, color = source)) +
        facet_wrap(vars(geography), scales = "free_y") + 
        geom_line(size=1) +
        scale_x_continuous(breaks = c(2008, 2013, 2019), limits = c(2007, 2020)) +
        scale_color_manual(values = c("gray","black")) +
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
        geom_text(mapping = aes(x = -Inf, y = -Inf, label = cor),
                  color="black", 
                  size = 2,
                  hjust   = -0.1,
                  vjust   = -1, 
        )

ggsave(filename = paste0(graphs,"graph_eu_silc_compare_2_4_year_panel.pdf"), plot = last_plot(), height = 8, width = 10, units = "in")
