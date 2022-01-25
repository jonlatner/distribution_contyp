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

data_files = "data_files/eu_lfs/"
graphs = "graphs/eu_lfs/sensitivity/"

# LIBRARY
library(tidyverse)
library(countrycode)
library(Hmisc) # wtd.mean
library(car) # recode
library(forcats) # fct_relevel
library(cowplot) # plot_grid
library(grid) # y.grob
library(gridExtra) # grid.arrange

# Load data ----

# df_eu_lfs_0 <- readRDS(file = paste0(data_files, "df_eu_lfs_sample_10_clean.rds"))
# df_eu_lfs_lmp <- readRDS(file = paste0(data_files, "df_eu_lfs_sample_10.rds"))

df_eu_lfs_0 <- readRDS(file = paste0(data_files, "df_eu_lfs_clean.rds"))
df_eu_lfs_lmp <- readRDS(file = paste0(data_files, "df_eu_lfs.rds"))

# Clean data ----

names(df_eu_lfs_lmp) <- tolower(names(df_eu_lfs_lmp))

df_eu_lfs_lmp <- df_eu_lfs_lmp %>%
        select(country,year,ilostat,age,temp,sex,hatlev1d,coeff) %>%
        filter(year >= 1996) %>% 
        filter(ilostat==1 | ilostat == 2) %>% # employed or unemplyed
        filter(!is.na(temp)) %>%
        filter(age >=25 & age <=54) %>%
        mutate(female = sex - 1) %>%
        rename(edu_cat = hatlev1d,
               weight = coeff) %>%
        filter(weight>0) %>%
        filter(edu_cat=="L" | edu_cat=="M" | edu_cat=="H")
df_eu_lfs_lmp <- droplevels(df_eu_lfs_lmp)

with(df_eu_lfs_lmp,table(ilostat,temp, useNA = "ifany"))

df_eu_lfs_lmp$temp <- recode(df_eu_lfs_lmp$temp, "c(1,9)=0; 2 = 1")

df_eu_lfs_lmp$age_cat <- recode(df_eu_lfs_lmp$age, "25:34 = 1; 35:44=2; 45:54=3")
df_eu_lfs_lmp$age_cat <- factor(df_eu_lfs_lmp$age_cat, labels=c("< 35", "35-44", "> 45"))
df_eu_lfs_lmp$female <- factor(df_eu_lfs_lmp$female, labels=c("Male", "Female"))
df_eu_lfs_lmp$edu_cat <- factor(df_eu_lfs_lmp$edu_cat, 
                                levels = c("L","M","H"), 
                                labels=c("< Secondary", "Secondary", "> Secondary"))

# Geography ----

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
df_eu_lfs_lmp <- merge(df_eu_lfs_lmp,region, by = c("country"), all.x = TRUE)
rm(country,region)

df_eu_lfs_lmp$country_name <- countrycode(df_eu_lfs_lmp$country, 'genc2c', 'country.name')

# Calculate temporary employment rate from employed ----

df_graph_regions <- df_eu_lfs_0 %>%
        group_by(region, year) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Employed")

df_graph_eu <- df_eu_lfs_0 %>%
        group_by(year) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(region = "Europe", 
               level = "Employed")

df_graph <- rbind(df_graph_regions,df_graph_eu)

# Calculate temporary employment rate from LMP ----

df_graph_regions <- df_eu_lfs_lmp %>%
        group_by(region, year) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        ungroup() %>%
        mutate(level = "LMP")

df_graph_eu <- df_eu_lfs_lmp %>%
        group_by(year) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(region = "Europe", 
               level = "LMP")

df_graph_lmp <- rbind(df_graph_regions,df_graph_eu)
rm(df_graph_regions,df_graph_eu)

# Combine ----

df_graph <- rbind(df_graph,df_graph_lmp)

df_graph$level <- factor(df_graph$level,
                         levels = c("Employed", "LMP"))

df_graph$avg <- round((df_graph$avg)*100,1)

df_graph$region <- fct_relevel(df_graph$region, "Europe", after = 0) # forcats

df_graph_1 <- df_graph %>%
        filter(year <= 2007) %>%
        mutate(period = 1)

df_graph_2 <- df_graph %>%
        filter(year >= 2007) %>%
        mutate(period = 2)

p1 <- ggplot(df_graph_1, aes(x = year, y = avg, group = level, color = level)) +
        facet_wrap( ~ region, nrow=1) +
        scale_color_manual(values = c("black", "gray75")) +
        geom_line() +
        scale_y_continuous(breaks = seq(0,30,10), limits = c(0,31)) +
        scale_x_continuous(breaks = c(1996,2002,2007), limits = c(1994,2009)) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.title=element_blank(),
              axis.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "none",
              axis.text = element_text(size=8),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

p2 <- ggplot(df_graph_2, aes(x = year, y = avg, group = level, color = level)) +
        facet_wrap( ~ region, nrow=1) +
        scale_color_manual(values = c("black", "gray75")) +
        geom_line() +
        scale_y_continuous(breaks = seq(0,30,10), limits = c(0,31)) +
        scale_x_continuous(breaks = c(2007,2013,2019), limits = c(2005,2021)) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(),
              legend.title=element_blank(),
              axis.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "none",
              axis.text = element_text(size=8),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )


# extract a legend that is laid out horizontally
legend_b <- get_legend(
        p1 + 
                guides(color = guide_legend(nrow = 1)) +
                theme(legend.position = "bottom")
)

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).

prow <- plot_grid(p1,p2,nrow = 2,ncol = 1)
p <- plot_grid(prow, legend_b, ncol = 1, rel_heights = c(5, 1))

#create common x and y labels

y.grob <- textGrob("Temporary employment rate x 100", rot=90,
                   gp=gpar(fontsize=10))

p <- grid.arrange(arrangeGrob(p, left = y.grob))
p

ggsave(p, filename = paste0(graphs,"graph_rate_region_compare_lmp.pdf"), height = 4, width = 6, units = "in")
