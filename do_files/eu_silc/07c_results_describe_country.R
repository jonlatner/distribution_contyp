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

graphs = "graphs/eu_silc/"
results = "results/eu_silc/"

# LIBRARY
library(tidyverse)
library(forcats)

options(scipen=999)

# Load ----

df_ever <- readRDS(file = paste0(results,"df_yhat_glm_ever_country_wt.rds"))
df_num <- readRDS(file = paste0(results,"df_yhat_glm_num_country_wt.rds"))
df_dur <- readRDS(file = paste0(results,"df_yhat_glm_dur_country_wt.rds"))


df_ever_region <- readRDS(file = paste0(results,"df_yhat_glm_ever_region_wt.rds")) %>%
        select(fit,lwr,upr,geography,panel) %>%
        rename(region=geography)
df_num_region <- readRDS(file = paste0(results,"df_yhat_glm_num_region_wt.rds")) %>%
        select(fit,lwr,upr,geography,panel) %>%
        rename(region=geography)
df_dur_region <- readRDS(file = paste0(results,"df_yhat_glm_dur_region_wt.rds")) %>%
        select(fit,lwr,upr,geography,panel) %>%
        rename(region=geography)

# region
geography <- c("Germany", "Switzerland", "Luxembourg", "Belgium", "Austria", "Netherlands", "France", 
             "United Kingdom", "Ireland", 
             "Malta", "Greece", "Italy", "Cyprus", "Portugal", "Spain", 
             "Romania", "Poland", "Croatia", "Hungary", "Czechia", "Bulgaria", "Slovenia", "Slovakia", "Lithuania", "Estonia", "Latvia", "Serbia", "Turkey",
             "Sweden", "Denmark", "Norway", "Finland", "Iceland")
region <- c("Continental", "Continental", "Continental", "Continental", "Continental", "Continental", "Continental", 
            "Anglophone", "Anglophone", 
            "Southern", "Southern", "Southern", "Southern", "Southern", "Southern", 
            "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern",
            "Nordic", "Nordic", "Nordic", "Nordic", "Nordic")

geography <- cbind(geography, region)

df_ever <- merge(df_ever,geography)
df_num <- merge(df_num,geography)
df_dur <- merge(df_dur,geography)

rm(region,geography)

# Clean ever temporary ----

df_ever_2008_2019 <- df_ever %>%
        select(region,geography,panel,fit,lwr,upr) %>%
        mutate(diff_2008_2019 = (last(fit) - first(fit)),
               sig_up = last(lwr) - first(upr),
               sig_dn = last(upr) - first(lwr)) %>%
        ungroup() %>%
        mutate(sig_2008_2019 = ifelse(diff_2008_2019 < 0 & sig_dn < 0, yes = "yes",
                                      ifelse(diff_2008_2019 > 0 & sig_up > 0, yes = "yes", no = "no"))) %>%
        select(-sig_up, -sig_dn)

df_ever_2008_2013 <- df_ever %>%
        select(region,geography,panel,fit,lwr,upr) %>%
        filter(panel < 2014) %>%
        mutate(diff_2008_2013 = (last(fit) - first(fit)),
               sig_up = last(lwr) - first(upr),
               sig_dn = last(upr) - first(lwr)) %>%
        filter(row_number()==1) %>%
        mutate(sig_2008_2013 = ifelse(diff_2008_2013 < 0 & sig_dn < 0, yes = "yes",
                                      ifelse(diff_2008_2013 > 0 & sig_up > 0, yes = "yes", no = "no"))) %>%
        select(diff_2008_2013, sig_2008_2013)

df_ever_2013_2019 <- df_ever %>%
        select(region,geography,panel,fit,lwr,upr) %>%
        filter(panel > 2013) %>%
        mutate(diff_2013_2019 = (last(fit) - first(fit)),
               sig_up = last(lwr) - first(upr),
               sig_dn = last(upr) - first(lwr)) %>%
        filter(row_number()==1) %>%
        mutate(sig_2013_2019 = ifelse(diff_2013_2019 < 0 & sig_dn < 0, yes = "yes",
                                      ifelse(diff_2013_2019 > 0 & sig_up > 0, yes = "yes", no = "no"))) %>%
        select(diff_2013_2019, sig_2013_2019)

df_ever <- merge(df_ever_2008_2019,df_ever_2008_2013)
df_ever <- merge(df_ever,df_ever_2013_2019)
rm(df_ever_2008_2013,df_ever_2013_2019,df_ever_2008_2019)

df_ever <- df_ever %>%
        mutate(fit = round(fit, 3),
               lwr = round(lwr, 3),
               upr = round(upr, 3),
               diff_2008_2019 = round(diff_2008_2019, 3),
               diff_2008_2013 = round(diff_2008_2013, 3),
               diff_2013_2019 = round(diff_2013_2019, 3))

# Clean contract number ----

df_num_2008_2019 <- df_num %>%
        select(region,geography,panel,fit,lwr,upr) %>%
        mutate(diff_2008_2019 = (last(fit) - first(fit)),
               sig_up = last(lwr) - first(upr),
               sig_dn = last(upr) - first(lwr)) %>%
        ungroup() %>%
        mutate(sig_2008_2019 = ifelse(diff_2008_2019 < 0 & sig_dn < 0, yes = "yes",
                                      ifelse(diff_2008_2019 > 0 & sig_up > 0, yes = "yes", no = "no"))) %>%
        select(-sig_up, -sig_dn)

df_num_2008_2013 <- df_num %>%
        select(region,geography,panel,fit,lwr,upr) %>%
        filter(panel < 2014) %>%
        mutate(diff_2008_2013 = (last(fit) - first(fit)),
               sig_up = last(lwr) - first(upr),
               sig_dn = last(upr) - first(lwr)) %>%
        filter(row_number()==1) %>%
        mutate(sig_2008_2013 = ifelse(diff_2008_2013 < 0 & sig_dn < 0, yes = "yes",
                                      ifelse(diff_2008_2013 > 0 & sig_up > 0, yes = "yes", no = "no"))) %>%
        select(diff_2008_2013, sig_2008_2013)

df_num_2013_2019 <- df_num %>%
        select(region,geography,panel,fit,lwr,upr) %>%
        filter(panel > 2013) %>%
        mutate(diff_2013_2019 = (last(fit) - first(fit)),
               sig_up = last(lwr) - first(upr),
               sig_dn = last(upr) - first(lwr)) %>%
        filter(row_number()==1) %>%
        mutate(sig_2013_2019 = ifelse(diff_2013_2019 < 0 & sig_dn < 0, yes = "yes",
                                      ifelse(diff_2013_2019 > 0 & sig_up > 0, yes = "yes", no = "no"))) %>%
        select(diff_2013_2019, sig_2013_2019)

df_num <- merge(df_num_2008_2019,df_num_2008_2013)
df_num <- merge(df_num,df_num_2013_2019)
rm(df_num_2008_2013,df_num_2013_2019,df_num_2008_2019)

df_num <- df_num %>%
        mutate(fit = round(fit, 3),
               lwr = round(lwr, 3),
               upr = round(upr, 3),
               diff_2008_2019 = round(diff_2008_2019, 3),
               diff_2008_2013 = round(diff_2008_2013, 3),
               diff_2013_2019 = round(diff_2013_2019, 3))

# Clean contract duration ----

df_dur_2008_2019 <- df_dur %>%
        select(region,geography,panel,fit,lwr,upr) %>%
        mutate(diff_2008_2019 = (last(fit) - first(fit)),
               sig_up = last(lwr) - first(upr),
               sig_dn = last(upr) - first(lwr)) %>%
        ungroup() %>%
        mutate(sig_2008_2019 = ifelse(diff_2008_2019 < 0 & sig_dn < 0, yes = "yes",
                                      ifelse(diff_2008_2019 > 0 & sig_up > 0, yes = "yes", no = "no"))) %>%
        select(-sig_up, -sig_dn)

df_dur2008_2013 <- df_dur %>%
        select(region,geography,panel,fit,lwr,upr) %>%
        filter(panel < 2014) %>%
        mutate(diff_2008_2013 = (last(fit) - first(fit)),
               sig_up = last(lwr) - first(upr),
               sig_dn = last(upr) - first(lwr)) %>%
        filter(row_number()==1) %>%
        mutate(sig_2008_2013 = ifelse(diff_2008_2013 < 0 & sig_dn < 0, yes = "yes",
                                      ifelse(diff_2008_2013 > 0 & sig_up > 0, yes = "yes", no = "no"))) %>%
        select(diff_2008_2013, sig_2008_2013)

df_dur2013_2019 <- df_dur %>%
        select(region,geography,panel,fit,lwr,upr) %>%
        filter(panel > 2013) %>%
        mutate(diff_2013_2019 = (last(fit) - first(fit)),
               sig_up = last(lwr) - first(upr),
               sig_dn = last(upr) - first(lwr)) %>%
        filter(row_number()==1) %>%
        mutate(sig_2013_2019 = ifelse(diff_2013_2019 < 0 & sig_dn < 0, yes = "yes",
                                      ifelse(diff_2013_2019 > 0 & sig_up > 0, yes = "yes", no = "no"))) %>%
        select(diff_2013_2019, sig_2013_2019)

df_dur <- merge(df_dur_2008_2019,df_dur2008_2013)
df_dur <- merge(df_dur,df_dur2013_2019)
rm(df_dur2008_2013,df_dur2013_2019,df_dur_2008_2019)

df_dur <- df_dur %>%
        mutate(fit = round(fit, 3),
               lwr = round(lwr, 3),
               upr = round(upr, 3),
               diff_2008_2019 = round(diff_2008_2019, 3),
               diff_2008_2013 = round(diff_2008_2013, 3),
               diff_2013_2019 = round(diff_2013_2019, 3))

# look at results -----

df_dur %>% 
        filter(region=="Continental") %>%
        filter(panel==2016|panel==2017|panel==2018|panel==2019)

# graph ever results -----

df_graph_country <- df_ever %>%
        select(region,geography,panel,fit,lwr,upr) %>%
        mutate(type = "Country") %>%
        filter(geography!="Romania") # drop single country to make graph fit better on the page

df_graph_region <- df_graph_country %>%
        select(region,geography,panel)

df_graph_region <- merge(df_graph_region,df_ever_region,all.x = TRUE) %>%
        mutate(type = "Region")

df_graph <- rbind(df_graph_country,df_graph_region)
df_graph$type <- factor(df_graph$type,
                                  levels = c("Country","Region"))

df_graph_country <- df_graph %>%
        filter(type == "Country")
df_graph_country <- droplevels(df_graph_country)

ggplot() +
        facet_wrap(region~geography) +
        geom_line(data = df_graph, aes(x = panel, y = fit, group = type, color = type)) +
        scale_x_continuous(breaks = c(2008, 2013, 2019), limits = c(2006, 2021)) +
        geom_errorbar(data = df_graph,
                      aes(x = panel, 
                          ymin = lwr,
                          ymax = upr,
                          color = type
        ), 
        width=.2, 
        position=position_dodge(0.05)) +
        theme_bw() +
        ylab("Predicted probability") +
        xlab("4-year panel wave ending") + 
        scale_color_manual(values = c("black", "gray75")) +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              legend.box.margin=margin(-10,0,0,0),
              axis.text.x = element_text(size=7.5),
              legend.key.width = unit(2,"cm"),
              legend.box = "vertical",
              legend.position = "bottom"
        ) +
        geom_text(data = df_graph_country, 
                  size = 3, 
                  aes(x = panel, y = ifelse(panel %in% c(2008,2013,2019), yes = fit, no = NA),
                      vjust=-2.5,
                      label=fit))

ggsave(filename = paste0(graphs,"graph_eu_silc_glm_yhat_ever_country.pdf"), plot = last_plot(), height = 9, width = 7, units = "in")

# graph dur results -----

df_graph_country <- df_dur %>%
        select(region,geography,panel,fit,lwr,upr) %>%
        mutate(type = "Country") %>%
        filter(geography!="Romania") # drop single country to make graph fit better on the page

df_graph_region <- df_graph_country %>%
        select(region,geography,panel)

df_graph_region <- merge(df_graph_region,df_dur_region,all.x = TRUE) %>%
        mutate(type = "Region")

df_graph <- rbind(df_graph_country,df_graph_region)
df_graph$type <- factor(df_graph$type,
                        levels = c("Country","Region"))

df_graph_country <- df_graph %>%
        filter(type == "Country")
df_graph_country <- droplevels(df_graph_country)

ggplot() +
        facet_wrap(region~geography) +
        geom_line(data = df_graph, aes(x = panel, y = fit, group = type, color = type)) +
        scale_x_continuous(breaks = c(2008, 2013, 2019), limits = c(2006, 2021)) +
        geom_errorbar(data = df_graph,
                      aes(x = panel, 
                          ymin = lwr,
                          ymax = upr,
                          color = type
                      ), 
                      width=.2, 
                      position=position_dodge(0.05)) +
        theme_bw() +
        ylab("Predicted probability") +
        xlab("4-year panel wave ending") + 
        scale_color_manual(values = c("black", "gray75")) +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              legend.box.margin=margin(-10,0,0,0),
              axis.text.x = element_text(size=7.5),
              legend.key.width = unit(2,"cm"),
              legend.box = "vertical",
              legend.position = "bottom"
        ) +
        geom_text(data = df_graph_country, 
                  size = 3, 
                  aes(x = panel, y = ifelse(panel %in% c(2008,2013,2019), yes = fit, no = NA),
                      vjust=-2.5,
                      label=fit))

ggsave(filename = paste0(graphs,"graph_eu_silc_glm_yhat_dur_country.pdf"), plot = last_plot(), height = 9, width = 7, units = "in")

# graph num results -----

df_graph_country <- df_num %>%
        select(region,geography,panel,fit,lwr,upr) %>%
        mutate(type = "Country") %>%
        filter(geography!="Romania") # drop single country to make graph fit better on the page

df_graph_region <- df_graph_country %>%
        select(region,geography,panel)

df_graph_region <- merge(df_graph_region,df_num_region,all.x = TRUE) %>%
        mutate(type = "Region")

df_graph <- rbind(df_graph_country,df_graph_region)
df_graph$type <- factor(df_graph$type,
                        levels = c("Country","Region"))

df_graph_country <- df_graph %>%
        filter(type == "Country")
df_graph_country <- droplevels(df_graph_country)

ggplot() +
        facet_wrap(region~geography) +
        geom_line(data = df_graph, aes(x = panel, y = fit, group = type, color = type)) +
        scale_x_continuous(breaks = c(2008, 2013, 2019), limits = c(2006, 2021)) +
        geom_errorbar(data = df_graph,
                      aes(x = panel, 
                          ymin = lwr,
                          ymax = upr,
                          color = type
                      ), 
                      width=.2, 
                      position=position_dodge(0.05)) +
        theme_bw() +
        ylab("Predicted probability") +
        xlab("4-year panel wave ending") + 
        scale_color_manual(values = c("black", "gray75")) +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              legend.box.margin=margin(-10,0,0,0),
              axis.text.x = element_text(size=7.5),
              legend.key.width = unit(2,"cm"),
              legend.box = "vertical",
              legend.position = "bottom"
        ) +
        geom_text(data = df_graph_country, 
                  size = 3, 
                  aes(x = panel, y = ifelse(panel %in% c(2008,2013,2019), yes = fit, no = NA),
                      vjust=-2.5,
                      label=fit))

ggsave(filename = paste0(graphs,"graph_eu_silc_glm_yhat_num_country.pdf"), plot = last_plot(), height = 9, width = 7, units = "in")

