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
# ADAPT THIS PATHWAY
setwd("/Users/jonathanlatner/Documents/GitHub/distribution_contyp/")

data_files = "data_files/eu_lfs/"
graphs = "graphs/eu_lfs/"

# LIBRARY
library(tidyverse)
library(Hmisc) # wtd.mean
library(car) # recode
library(forcats) # fct_relevel
library(cowplot) # plot_grid

# Load data -----------------------------------------

df_eu_lfs_0 <- readRDS(file = paste0(data_files, "df_eu_lfs_sample_10.rds"))
# df_eu_lfs_0 <- readRDS(file = paste0(data_files, "df_eu_lfs.rds"))

t <- unique(df_eu_lfs_0$country)

country <- c("Anglophone", "Continental", "Eastern", "Nordic", "Southern",
             "DE", "CH", "LU", "BE", "AT", "NL", "FR", 
             "GB", "IE", 
             "MT", "GR", "IT", "CY", "PT", "ES", "RO", "PL", 
             "HR", "HU", "CZ", "BG", "SI", "SK", "LT", "EE", "LV", "RS", 
             "SE", "DK", "NO", "FI", "IS")
region <- c("Anglophone", "Continental", "Eastern", "Nordic", "Southern",
            "Continental", "Continental", "Continental", "Continental", "Continental", "Continental", "Continental", 
            "Anglophone", "Anglophone", 
            "Southern", "Southern", "Southern", "Southern", "Southern", "Southern", 
            "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", 
            "Nordic", "Nordic", "Nordic", "Nordic", "Nordic")

region <- cbind(country, region)
rm(country)

# Clean data -----------------------------------------
df_eu_lfs_0 <- droplevels(df_eu_lfs_0)

names(df_eu_lfs_0) <- tolower(names(df_eu_lfs_0))
str(df_eu_lfs_0)

df_eu_lfs_1 <- df_eu_lfs_0 %>%
        filter(year >= 1996) %>%
        filter(ilostat==1 | ilostat == 2) %>%
        filter(age >=25 & age <=54) %>%
        filter(temp != 9) %>%
        mutate(temp = temp - 1) %>%
        mutate(female = sex - 1) %>%
        rename(edu_cat = hatlev1d,
               weight = coeff) %>%
        filter(weight>0) %>%
        filter(edu_cat=="L" | edu_cat=="M" | edu_cat=="H")
df_eu_lfs_1 <- droplevels(df_eu_lfs_1)
summary(df_eu_lfs_1)
nrow(df_eu_lfs_1)*10

t <- unique(df_eu_lfs_1$country)

with(df_eu_lfs_1,table(year,edu_cat, useNA = "ifany"))

df_eu_lfs_1 <- merge(df_eu_lfs_1,region, by = c("country"), all.x = TRUE)

# Graph percent FTC -----------------------------------------

df_graph_countries <- df_eu_lfs_1 %>%
        group_by(region, country_name, year) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Countries")

df_graph_regions <- df_eu_lfs_1 %>%
        group_by(region, year) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country_name = region)

df_graph_eu <- df_eu_lfs_1 %>%
        group_by(year) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "Europe", region = "Europe", level = "Region")

df_graph <- rbind(df_graph_countries,df_graph_regions,df_graph_eu)
rm(df_graph_countries,df_graph_regions,df_graph_eu)

df_graph$level <- factor(df_graph$level,
                         levels = c("Region", "Countries"))

df_graph$country_name <- fct_relevel(df_graph$country_name, "Southern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Nordic", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Eastern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Continental", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Anglophone", after = Inf) # forcats
df_graph$region <- fct_relevel(df_graph$region, "Europe", after = 0) # forcats
df_graph$level <- fct_relevel(df_graph$level, "Countries", after = 0) # forcats
df_graph$level <- fct_relevel(df_graph$level, "Region", after = 0) # forcats

df_graph_1 <- df_graph %>%
        filter(year <= 2007) %>%
        mutate(period = 1)
df_graph_1$period <- factor(df_graph_1$period,
                          labels = c("1996 - 2007"))

df_graph_2 <- df_graph %>%
        filter(year >= 2007) %>%
        mutate(period = 2)
df_graph_2$period <- factor(df_graph_2$period,
                          labels = c("2007 - 2018"))

df_graph_region_1 <- df_graph_1 %>%
        filter(level != "Countries")

df_graph_region_2 <- df_graph_2 %>%
        filter(level != "Countries")

p1 <- ggplot(df_graph_1, aes(x = year, y = avg, group = country_name, color = level, size = level)) +
        facet_grid(period ~ region) +
        scale_size_manual(values = c(1,.5)) +
        scale_color_manual(values = c("black", "gray")) +
        geom_line() +
        scale_x_continuous(breaks = c(1996,2001,2007), limits = c(1994,2009)) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.title=element_blank(),
              axis.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "none",
              # axis.text.x = element_blank(),
              axis.text = element_text(size=7),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        ) +
        geom_text(data = df_graph_region_1,
                  show.legend = FALSE,
                  size = 2.5,
                  aes(x = year, y = ifelse(year %in% c(1996,2007), yes = avg, no = NA),
                      vjust=-2,
                      label=sprintf(avg, fmt = '%#.3f')))

p2 <- ggplot(df_graph_2, aes(x = year, y = avg, group = country_name, color = level, size = level)) +
        facet_grid(period ~ region) +
        scale_size_manual(values = c(1,.5)) +
        scale_color_manual(values = c("black", "gray")) +
        geom_line() +
        scale_x_continuous(breaks = c(2007,2012,2018), limits = c(2005,2020)) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.title=element_blank(),
              axis.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "none",
              axis.text = element_text(size=7),
              # axis.text.x = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        ) +
        geom_text(data = df_graph_region_2,
                  show.legend = FALSE,
                  size = 2.5,
                  aes(x = year, y = ifelse(year %in% c(2007,2018), yes = avg, no = NA),
                      vjust=-2,
                      label=sprintf(avg, fmt = '%#.3f')))
# extract a legend that is laid out horizontally
legend_b <- get_legend(
        p1 + 
                guides(color = guide_legend(nrow = 1)) +
                theme(legend.position = "bottom")
)

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).

prow <- plot_grid(p1,p2,nrow = 2,ncol = 1)
plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .1))

ggsave(filename = paste0(graphs,"graph_rate_region.pdf"), height = 4, width = 6, units = "in")
