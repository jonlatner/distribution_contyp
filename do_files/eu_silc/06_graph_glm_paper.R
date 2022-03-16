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
library(forcats) #fct_reorder

options(scipen=999)

# Load ----
# ever
df_yhat_eu_ever <- readRDS(file = paste0(results,"df_yhat_glm_ever_eu_wt.rds"))
df_yhat_region_ever <- readRDS(file = paste0(results,"df_yhat_glm_ever_region_wt.rds"))
df_yhat_country_ever <- readRDS(file = paste0(results,"df_yhat_glm_ever_country_wt.rds"))

df_mfx_eu_ever <- readRDS(file = paste0(results,"df_mfx_glm_ever_eu_wt.rds"))
df_mfx_region_ever <- readRDS(file = paste0(results,"df_mfx_glm_ever_region_wt.rds"))
df_mfx_country_ever <- readRDS(file = paste0(results,"df_mfx_glm_ever_country_wt.rds"))

# single contract
df_yhat_eu_num <- readRDS(file = paste0(results,"df_yhat_glm_num_eu_wt.rds"))
df_yhat_region_num <- readRDS(file = paste0(results,"df_yhat_glm_num_region_wt.rds"))
df_yhat_country_num <- readRDS(file = paste0(results,"df_yhat_glm_num_country_wt.rds"))

df_mfx_eu_num <- readRDS(file = paste0(results,"df_mfx_glm_num_eu_wt.rds"))
df_mfx_region_num <- readRDS(file = paste0(results,"df_mfx_glm_num_region_wt.rds"))
df_mfx_country_num <- readRDS(file = paste0(results,"df_mfx_glm_num_country_wt.rds"))

# multi-year 
df_yhat_eu_dur <- readRDS(file = paste0(results,"df_yhat_glm_dur_eu_wt.rds"))
df_yhat_region_dur <- readRDS(file = paste0(results,"df_yhat_glm_dur_region_wt.rds"))
df_yhat_country_dur <- readRDS(file = paste0(results,"df_yhat_glm_dur_country_wt.rds"))

df_mfx_eu_dur <- readRDS(file = paste0(results,"df_mfx_glm_dur_eu_wt.rds"))
df_mfx_region_dur <- readRDS(file = paste0(results,"df_mfx_glm_dur_region_wt.rds"))
df_mfx_country_dur <- readRDS(file = paste0(results,"df_mfx_glm_dur_country_wt.rds"))

# region
country <- c("Germany", "Switzerland", "Luxembourg", "Belgium", "Austria", "Netherlands", "France", 
             "United Kingdom", "Ireland", 
             "Malta", "Greece", "Italy", "Cyprus", "Portugal", "Spain", 
             "Romania", "Poland", "Croatia", "Hungary", "Czechia", "Bulgaria", "Slovenia", "Slovakia", "Lithuania", "Estonia", "Latvia", "Serbia", "Turkey",
             "Sweden", "Denmark", "Norway", "Finland", "Iceland")
region <- c("Continental", "Continental", "Continental", "Continental", "Continental", "Continental", "Continental", 
            "Anglophone", "Anglophone", 
            "Southern", "Southern", "Southern", "Southern", "Southern", "Southern", 
            "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern",
            "Nordic", "Nordic", "Nordic", "Nordic", "Nordic")

geography <- cbind(country, region)
rm(country, region)

# Clean ever temporary ----

# Clean (AME - group level interaction) 

df_mfx_eu_ever <- df_mfx_eu_ever %>%
        mutate(region = "EU-SILC",
               geography = "Region",
               country = "EU-SILC") %>%
        mutate(country = as.factor(country),
               geography = as.factor(geography),
               region = as.factor(region))

df_mfx_region_ever <- df_mfx_region_ever %>%
        rename(region=geography) %>%
        mutate(geography = "Region",
               country = region) %>%
        mutate(country = as.factor(country),
               geography = as.factor(geography),
               region = as.factor(region))

df_mfx_country_ever <- df_mfx_country_ever %>%
        rename(country=geography) %>%
        mutate(geography = "Country")
df_mfx_country_ever <- merge(df_mfx_country_ever,geography)
df_mfx_country_ever <- df_mfx_country_ever %>%
        mutate(country = as.factor(country),
               geography = as.factor(geography),
               region = as.factor(region))

df_yhat_eu_ever <- df_yhat_eu_ever %>%
        mutate(region = "EU-SILC",
               geography = "Region",
               country = "EU-SILC") %>%
        mutate(country = as.factor(country),
               geography = as.factor(geography),
               region = as.factor(region))

df_yhat_region_ever <- df_yhat_region_ever %>%
        rename(region=geography) %>%
        mutate(geography = "Region",
               country = region) %>%
        mutate(country = as.factor(country),
               geography = as.factor(geography),
               region = as.factor(region))

df_yhat_country_ever <- df_yhat_country_ever %>%
        rename(country=geography) %>%
        mutate(geography = "Country")

df_yhat_country_ever <- merge(df_yhat_country_ever,geography)

df_yhat_country_ever <- df_yhat_country_ever %>%
        mutate(country = as.factor(country),
               geography = as.factor(geography),
               region = as.factor(region)) %>%
        arrange(country,panel)

# Clean contract_num contracts ----

# Clean (AME - group level interaction) 

df_mfx_eu_num <- df_mfx_eu_num %>%
        mutate(region = "EU-SILC",
               geography = "Region",
               country = "EU-SILC") %>%
        mutate(country = as.factor(country),
               geography = as.factor(geography),
               region = as.factor(region))

df_mfx_region_num <- df_mfx_region_num %>%
        rename(region=geography) %>%
        mutate(geography = "Region",
               country = region) %>%
        mutate(country = as.factor(country),
               geography = as.factor(geography),
               region = as.factor(region))

df_mfx_country_num <- df_mfx_country_num %>%
        rename(country=geography) %>%
        mutate(geography = "Country")
df_mfx_country_num <- merge(df_mfx_country_num,geography)
df_mfx_country_num <- df_mfx_country_num %>%
        mutate(country = as.factor(country),
               geography = as.factor(geography),
               region = as.factor(region))

# Clean (yhat - country level time trends) 

df_yhat_eu_num <- df_yhat_eu_num %>%
        mutate(region = "EU-SILC",
               geography = "Region",
               country = "EU-SILC") %>%
        mutate(country = as.factor(country),
               geography = as.factor(geography),
               region = as.factor(region))

df_yhat_region_num <- df_yhat_region_num %>%
        rename(region=geography) %>%
        mutate(geography = "Region",
               country = region) %>%
        mutate(country = as.factor(country),
               geography = as.factor(geography),
               region = as.factor(region))

df_yhat_country_num <- df_yhat_country_num %>%
        rename(country=geography) %>%
        mutate(geography = "Country")

df_yhat_country_num <- merge(df_yhat_country_num,geography)

df_yhat_country_num <- df_yhat_country_num %>%
        mutate(country = as.factor(country),
               geography = as.factor(geography),
               region = as.factor(region)) %>%
        arrange(country,panel)

# Clean contract_dur contracts ----

# Clean (AME - group level interaction) 

df_mfx_eu_dur <- df_mfx_eu_dur %>%
        mutate(region = "EU-SILC",
               geography = "Region",
               country = "EU-SILC") %>%
        mutate(country = as.factor(country),
               geography = as.factor(geography),
               region = as.factor(region))

df_mfx_region_dur <- df_mfx_region_dur %>%
        rename(region=geography) %>%
        mutate(geography = "Region",
               country = region) %>%
        mutate(country = as.factor(country),
               geography = as.factor(geography),
               region = as.factor(region))

df_mfx_country_dur <- df_mfx_country_dur %>%
        rename(country=geography) %>%
        mutate(geography = "Country")
df_mfx_country_dur <- merge(df_mfx_country_dur,geography)
df_mfx_country_dur <- df_mfx_country_dur %>%
        mutate(country = as.factor(country),
               geography = as.factor(geography),
               region = as.factor(region))


# Clean (yhat - country level time trends) 

df_yhat_eu_dur <- df_yhat_eu_dur %>%
        mutate(region = "EU-SILC",
               geography = "Region",
               country = "EU-SILC") %>%
        mutate(country = as.factor(country),
               geography = as.factor(geography),
               region = as.factor(region))

df_yhat_region_dur <- df_yhat_region_dur %>%
        rename(region=geography) %>%
        mutate(geography = "Region",
               country = region) %>%
        mutate(country = as.factor(country),
               geography = as.factor(geography),
               region = as.factor(region))

df_yhat_country_dur <- df_yhat_country_dur %>%
        rename(country=geography) %>%
        mutate(geography = "Country")

df_yhat_country_dur <- merge(df_yhat_country_dur,geography)

df_yhat_country_dur <- df_yhat_country_dur %>%
        mutate(country = as.factor(country),
               geography = as.factor(geography),
               region = as.factor(region)) %>%
        arrange(country,panel)


# Graph yhat ----

df_yhat_ever <- rbind(df_yhat_eu_ever, df_yhat_region_ever, df_yhat_country_ever)
df_yhat_ever$type <- 1
df_yhat_num <- rbind(df_yhat_eu_num, df_yhat_region_num, df_yhat_country_num)
df_yhat_num$type <- 2
df_yhat_dur <- rbind(df_yhat_eu_dur, df_yhat_region_dur, df_yhat_country_dur)
df_yhat_dur$type <- 3

df_graph_yhat <- rbind(df_yhat_ever,df_yhat_num,df_yhat_dur)
rm(df_yhat_ever,df_yhat_num,df_yhat_dur)

df_graph_yhat$geography <- factor(df_graph_yhat$geography,
                                  levels = c("Region","Country"))

df_graph_yhat <- df_graph_yhat %>%
        select(-male,-age_cat,-edu_cat) %>%
        arrange(type,geography,region,country,panel)

df_graph_yhat$type <- factor(df_graph_yhat$type, labels=c("At least 1 (Ref: 0)", "Multiple contracts (Ref: 1)", "Multiple year (Ref: 1)"))

df_graph_yhat$country <- fct_relevel(df_graph_yhat$country, "Anglophone", after = Inf) # forcats
df_graph_yhat$country <- fct_relevel(df_graph_yhat$country, "Eastern", after = Inf) # forcats
df_graph_yhat$country <- fct_relevel(df_graph_yhat$country, "Continental", after = Inf) # forcats
df_graph_yhat$country <- fct_relevel(df_graph_yhat$country, "Nordic", after = Inf) # forcats
df_graph_yhat$country <- fct_relevel(df_graph_yhat$country, "Southern", after = Inf) # forcats
df_graph_yhat$region <- fct_relevel(df_graph_yhat$region, "EU-SILC", after = 0) # forcats

df_graph_yhat_region <- df_graph_yhat %>%
        filter(geography == "Region")
df_graph_yhat_region <- droplevels(df_graph_yhat_region)

ggplot() +
        facet_grid(type~region) + 
        geom_line(data = df_graph_yhat, aes(x = panel, y = fit, color = geography, group = country), size = .5) +
        geom_errorbar(data = df_graph_yhat,
                      aes(x = panel,
                          ymin = lwr,
                          ymax = upr,
                          color = geography
                      ),
                      width=.2) +
        geom_errorbar(data = df_graph_yhat_region,
                      aes(x = panel,
                          ymin = lwr,
                          ymax = upr,
                          color = geography
                      ),
                      width=.2) +
        geom_line(data = df_graph_yhat_region, aes(x = panel, y = fit, group = country), size = .5) +
        scale_x_continuous(breaks = c(2008,2013,2019), limits = c(2006, 2021)) +
        scale_y_continuous(breaks = c(seq(0, 1, by = .25)), limits = c(-.1, 1.1)) +
        scale_color_manual(values = c("black", "gray80")) +
        ylab("Predicted probability") +
        xlab("4-year panel wave ending") + 
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.box.margin=margin(-10,0,0,0),
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              axis.title.x = element_blank(),
              # axis.text.x = element_text(size=7),
              legend.box = "vertical",
              legend.position = "bottom"
        ) +
        geom_text(data = df_graph_yhat_region, 
                  size = 3, 
                  aes(x = panel, y = ifelse(panel %in% c(2008,2013,2019), yes = fit, no = NA),
                      vjust=-2,
                      label=sprintf(fit, fmt = '%#.2f')))


ggsave(filename = paste0(graphs,"graph_eu_silc_glm_yhat_wt.pdf"), plot = last_plot(), height = 6, width = 9, units = "in")

# Graph mfx ever ----

df_graph_mfx <- bind_rows(df_mfx_eu_ever, df_mfx_region_ever, df_mfx_country_ever)

df_graph_mfx$geography <- factor(df_graph_mfx$geography,
                                 levels = c("Region","Country"))

df_graph_mfx$factor <- factor(df_graph_mfx$factor,
                              levels=c("male1",
                                       "age_cat1","age_cat3",
                                       "edu_cat1","edu_cat3"
                              ),
                              labels=c("Male",
                                       "Age (25-34)", "Age (45-54)",
                                       "< Secondary", "> Secondary"
                              ))

df_graph_mfx$country <- fct_relevel(df_graph_mfx$country, "Anglophone", after = Inf) # forcats
df_graph_mfx$country <- fct_relevel(df_graph_mfx$country, "Eastern", after = Inf) # forcats
df_graph_mfx$country <- fct_relevel(df_graph_mfx$country, "Continental", after = Inf) # forcats
df_graph_mfx$country <- fct_relevel(df_graph_mfx$country, "Nordic", after = Inf) # forcats
df_graph_mfx$country <- fct_relevel(df_graph_mfx$country, "Southern", after = Inf) # forcats
df_graph_mfx$region <- fct_relevel(df_graph_mfx$region, "EU-SILC", after = 0) # forcats

df_graph_mfx_region <- df_graph_mfx %>%
        filter(geography == "Region")
df_graph_mfx_region <- droplevels(df_graph_mfx_region)
df_graph_mfx <- data.frame(df_graph_mfx)

ggplot() +
        facet_grid(factor~region) + 
        geom_line(data = df_graph_mfx, aes(x = panel, y = AME, color = geography, group = country), size = .5) +
        geom_errorbar(data = df_graph_mfx,
                      aes(x = panel,
                          ymin = lower,
                          ymax = upper,
                          color = geography
                      ),
                      width=.2) +
        geom_errorbar(data = df_graph_mfx_region,
                      aes(x = panel,
                          ymin = lower,
                          ymax = upper,
                          color = geography
                      ),
                      width=.2) +
        geom_line(data = df_graph_mfx_region, aes(x = panel, y = AME, group = country), size = .5) +
        scale_x_continuous(breaks = c(2008,2013,2019), limits = c(2006, 2021)) +
        scale_y_continuous(breaks = c(seq(-.5, .5, by = .25)), limits = c(-.5, .5)) +
        scale_color_manual(values = c("black", "gray80")) +
        geom_hline(yintercept=0) +
        ylab("Average marginal effect") +
        xlab("4-year panel wave ending") + 
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              axis.title.x = element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.box.margin=margin(-10,0,0,0),
              # axis.text.x = element_text(size=7),
              legend.box = "vertical",
              legend.position = "bottom"
        ) +
        geom_text(data = df_graph_mfx_region, 
                  size = 3, 
                  aes(x = panel, y = ifelse(panel %in% c(2008,2013,2019), yes = AME, no = NA),
                      vjust=-2,
                      label=sprintf(AME, fmt = '%#.2f')))

ggsave(filename = paste0(graphs,"graph_eu_silc_glm_mfx_ever_wt.pdf"), plot = last_plot(), height = 6, width = 9, units = "in")

# Graph mfx contract num ----

df_graph_mfx <- bind_rows(df_mfx_eu_num, df_mfx_region_num, df_mfx_country_num)

df_graph_mfx$geography <- factor(df_graph_mfx$geography,
                                 levels = c("Region","Country"))

df_graph_mfx$factor <- factor(df_graph_mfx$factor,
                              levels=c("male1",
                                       "age_cat1","age_cat3",
                                       "edu_cat1","edu_cat3"
                              ),
                              labels=c("Male",
                                       "Age (25-34)", "Age (45-54)",
                                       "< Secondary", "> Secondary"
                              ))

df_graph_mfx$country <- fct_relevel(df_graph_mfx$country, "Anglophone", after = Inf) # forcats
df_graph_mfx$country <- fct_relevel(df_graph_mfx$country, "Eastern", after = Inf) # forcats
df_graph_mfx$country <- fct_relevel(df_graph_mfx$country, "Continental", after = Inf) # forcats
df_graph_mfx$country <- fct_relevel(df_graph_mfx$country, "Nordic", after = Inf) # forcats
df_graph_mfx$country <- fct_relevel(df_graph_mfx$country, "Southern", after = Inf) # forcats
df_graph_mfx$region <- fct_relevel(df_graph_mfx$region, "EU-SILC", after = 0) # forcats

df_graph_mfx_region <- df_graph_mfx %>%
        filter(geography == "Region")
df_graph_mfx_region <- droplevels(df_graph_mfx_region)
df_graph_mfx <- data.frame(df_graph_mfx)

ggplot() +
        facet_grid(factor~region) + 
        geom_line(data = df_graph_mfx, aes(x = panel, y = AME, color = geography, group = country), size = .5) +
        geom_errorbar(data = df_graph_mfx,
                      aes(x = panel,
                          ymin = lower,
                          ymax = upper,
                          color = geography
                      ),
                      width=.2) +
        geom_errorbar(data = df_graph_mfx_region,
                      aes(x = panel,
                          ymin = lower,
                          ymax = upper,
                          color = geography
                      ),
                      width=.2) +
        geom_line(data = df_graph_mfx_region, aes(x = panel, y = AME, group = country), size = .5) +
        scale_x_continuous(breaks = c(2008,2013,2019), limits = c(2006, 2021)) +
        scale_y_continuous(breaks = c(seq(-.5, .5, by = .25)), limits = c(-.5, .5)) +
        scale_color_manual(values = c("black", "gray80")) +
        geom_hline(yintercept=0) +
        ylab("Average marginal effect") +
        xlab("4-year panel wave ending") + 
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              legend.box.margin=margin(-10,0,0,0),
              legend.key.width = unit(2,"cm"),
              axis.title.x = element_blank(),
              # axis.text.x = element_text(size=7),
              legend.box = "vertical",
              legend.position = "bottom"
        ) +
        geom_text(data = df_graph_mfx_region, 
                  size = 3, 
                  aes(x = panel, y = ifelse(panel %in% c(2008,2013,2019), yes = AME, no = NA),
                      vjust=-2,
                      label=sprintf(AME, fmt = '%#.2f')))

ggsave(filename = paste0(graphs,"graph_eu_silc_glm_mfx_num_wt.pdf"), plot = last_plot(), height = 6, width = 9, units = "in")

# Graph mfx contract dur ----

df_graph_mfx <- bind_rows(df_mfx_eu_dur, df_mfx_region_dur, df_mfx_country_dur)
summary(df_graph_mfx)

df_graph_mfx$geography <- factor(df_graph_mfx$geography,
                                 levels = c("Region","Country"))

df_graph_mfx$factor <- factor(df_graph_mfx$factor,
                              levels=c("male1",
                                       "age_cat1","age_cat3",
                                       "edu_cat1","edu_cat3"
                              ),
                              labels=c("Male",
                                       "Age (25-34)", "Age (45-54)",
                                       "< Secondary", "> Secondary"
                              ))

df_graph_mfx$country <- fct_relevel(df_graph_mfx$country, "Anglophone", after = Inf) # forcats
df_graph_mfx$country <- fct_relevel(df_graph_mfx$country, "Eastern", after = Inf) # forcats
df_graph_mfx$country <- fct_relevel(df_graph_mfx$country, "Continental", after = Inf) # forcats
df_graph_mfx$country <- fct_relevel(df_graph_mfx$country, "Nordic", after = Inf) # forcats
df_graph_mfx$country <- fct_relevel(df_graph_mfx$country, "Southern", after = Inf) # forcats
df_graph_mfx$region <- fct_relevel(df_graph_mfx$region, "EU-SILC", after = 0) # forcats

df_graph_mfx_region <- df_graph_mfx %>%
        filter(geography == "Region")
df_graph_mfx_region <- droplevels(df_graph_mfx_region)
df_graph_mfx <- data.frame(df_graph_mfx)

ggplot() +
        facet_grid(factor~region) + 
        geom_line(data = df_graph_mfx, aes(x = panel, y = AME, color = geography, group = country), size = .5) +
        geom_errorbar(data = df_graph_mfx,
                      aes(x = panel,
                          ymin = lower,
                          ymax = upper,
                          color = geography
                      ),
                      width=.2) +
        geom_errorbar(data = df_graph_mfx_region,
                      aes(x = panel,
                          ymin = lower,
                          ymax = upper,
                          color = geography
                      ),
                      width=.2) +
        geom_line(data = df_graph_mfx_region, aes(x = panel, y = AME, group = country), size = .5) +
        scale_x_continuous(breaks = c(2008,2013,2019), limits = c(2006, 2021)) +
        scale_y_continuous(breaks = c(seq(-.75, .75, by = .5)), limits = c(-.8, .8)) +
        scale_color_manual(values = c("black", "gray80")) +
        geom_hline(yintercept=0) +
        ylab("Average marginal effect") +
        xlab("4-year panel wave ending") + 
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.box.margin=margin(-10,0,0,0),
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              axis.title.x = element_blank(),
              # axis.text.x = element_text(size=7),
              legend.box = "vertical",
              legend.position = "bottom"
        ) +
        geom_text(data = df_graph_mfx_region, 
                  size = 3, 
                  aes(x = panel, y = ifelse(panel %in% c(2008,2013,2019), yes = AME, no = NA),
                      vjust=-2,
                      label=sprintf(AME, fmt = '%#.2f')))

ggsave(filename = paste0(graphs,"graph_eu_silc_glm_mfx_dur_wt.pdf"), plot = last_plot(), height = 6, width = 9, units = "in")
