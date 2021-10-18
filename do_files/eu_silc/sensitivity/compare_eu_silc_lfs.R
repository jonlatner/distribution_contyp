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
setwd("/Users/jonathanlatner/Google Drive/SECCOPA/projects/distribution_contyp/")
# setwd("/Users/jonathanlatner/Documents/GitHub/distribution_contyp/")

data_files_silc_xs = "data_files/eu_silc/cross_sectional/"
data_files_silc_2 = "data_files/eu_silc/2_year_panel/"
data_files_silc_4 = "data_files/eu_silc/"
data_files_lfs = "data_files/eu_lfs/"
graphs = "graphs/eu_silc/sensitivity/"

# LIBRARY
library(tidyverse)
library(Hmisc) # wtd.mean
library(forcats) #fct_reorder
library(beepr)

options(scipen=999)

# Load data ----

df_eu_silc_xs_0 <- readRDS(file = paste0(data_files_silc_xs,"df_eu_silc_xs_clean.rds"))
df_eu_silc_2_year_0 <- readRDS(file = paste0(data_files_silc_2,"df_eu_silc_clean_panel_year.rds"))
df_eu_silc_4_year_0 <- readRDS(file = paste0(data_files_silc_4,"df_eu_silc_clean_panel_year.rds"))
df_eu_lfs_0 <- readRDS(file = paste0(data_files_lfs, "df_eu_lfs_sample_10_clean.rds"))

# Clean and summarize SILC (Cross-sectional) data ----

df_eu_silc_xs <- df_eu_silc_xs_0 %>%
  select(region,country_name,year,contyp,weight_xc) %>%
  rename(country = country_name,
         temp=contyp) %>%
  mutate(temp = temp - 1)

df_eu_silc_xs_temp_country <- df_eu_silc_xs %>%
  group_by(country, year) %>%
  summarise(avg=wtd.mean(temp,weight_xc)) %>%
  ungroup() %>%
  rename(geography=country) %>%
  mutate(source="SILC (XS)")

df_eu_silc_xs_temp_region <- df_eu_silc_xs %>%
  group_by(region, year) %>%
  summarise(avg=wtd.mean(temp,weight_xc)) %>%
  ungroup() %>%
  rename(geography=region) %>%
  mutate(source="SILC (XS)")

df_eu_silc_xs_temp_EU <- df_eu_silc_xs %>%
  group_by(year) %>%
  summarise(avg=wtd.mean(temp,weight_xc)) %>%
  ungroup() %>% 
  mutate(geography="EU") %>%
  mutate(source="SILC (XS)")

df_eu_silc_xs_temp <- rbind(df_eu_silc_xs_temp_EU,df_eu_silc_xs_temp_region,df_eu_silc_xs_temp_country)
rm(df_eu_silc_xs_temp_EU,df_eu_silc_xs_temp_region,df_eu_silc_xs_temp_country)

# df_eu_silc_xs_temp <- df_eu_silc_xs_temp %>%
#   filter(year>2004,
#          avg>0)

# Clean and summarize LFS data ----

df_eu_lfs <- df_eu_lfs_0 %>%
  select(region,country_name,year,temp,weight) %>%
  rename(country = country_name,
         weight_xc = weight) 

df_eu_lfs_temp_country <- df_eu_lfs %>%
  group_by(country, year) %>%
  summarise(avg=wtd.mean(temp,weight_xc)) %>%
  ungroup() %>%
  rename(geography=country) %>%
  mutate(source="LFS")

df_eu_lfs_temp_region <- df_eu_lfs %>%
  group_by(region, year) %>%
  summarise(avg=wtd.mean(temp,weight_xc)) %>%
  ungroup() %>%
  rename(geography=region) %>%
  mutate(source="LFS")

df_eu_lfs_temp_EU <- df_eu_lfs %>%
  group_by(year) %>%
  summarise(avg=wtd.mean(temp,weight_xc)) %>%
  ungroup() %>% 
  mutate(geography="EU") %>%
  mutate(source="LFS")

df_eu_lfs_temp <- rbind(df_eu_lfs_temp_EU,df_eu_lfs_temp_region,df_eu_lfs_temp_country)
rm(df_eu_lfs_temp_EU,df_eu_lfs_temp_region,df_eu_lfs_temp_country)

# Clean and summarize SILC (4 year) data ----

df_eu_silc_4_year <- df_eu_silc_4_year_0 %>%
  mutate(temp=ftc_ever)

df_eu_silc_4_year <- df_eu_silc_4_year %>%
  select(region, country_name, panel, year, temp, weight_long) %>%
  rename(country=country_name)

df_eu_silc_4_year_temp_country <- df_eu_silc_4_year %>%
  group_by(country, year) %>%
  summarise(avg=wtd.mean(temp,weight_long)) %>%
  ungroup() %>%
  filter(year<2019) %>%
  rename(geography=country) %>%
  mutate(source="SILC (4 year)") 

df_eu_silc_4_year_temp_region <- df_eu_silc_4_year %>%
  group_by(region, year) %>%
  summarise(avg=wtd.mean(temp,weight_long)) %>%
  ungroup() %>%
  filter(year<2019) %>%
  rename(geography=region) %>%
  mutate(source="SILC (4 year)") 

df_eu_silc_4_year_temp_EU <- df_eu_silc_4_year %>%
  group_by(year) %>%
  summarise(avg=wtd.mean(temp,weight_long)) %>%
  ungroup() %>%
  filter(year<2019) %>%
  mutate(geography="EU") %>%
  mutate(source="SILC (4 year)") 

df_eu_silc_4_year_temp <- rbind(df_eu_silc_4_year_temp_EU,df_eu_silc_4_year_temp_region,df_eu_silc_4_year_temp_country)
rm(df_eu_silc_4_year_temp_EU,df_eu_silc_4_year_temp_region,df_eu_silc_4_year_temp_country)

# Clean and summarize SILC (2 year) data ----

df_eu_silc_2_year <- df_eu_silc_2_year_0 %>%
  mutate(temp=ftc_ever)

df_eu_silc_2_year <- df_eu_silc_2_year %>%
  select(region,country_name, panel, year, temp, weight_long) %>%
  rename(country=country_name)


df_eu_silc_2_year_temp_country <- df_eu_silc_2_year %>%
  group_by(country, year) %>%
  summarise(avg=wtd.mean(temp,weight_long)) %>%
  ungroup() %>%
  filter(year<2019) %>%
  rename(geography=country) %>%
  mutate(source="SILC (2 year)") 

df_eu_silc_2_year_temp_region <- df_eu_silc_2_year %>%
  group_by(region, year) %>%
  summarise(avg=wtd.mean(temp,weight_long)) %>%
  ungroup() %>%
  filter(year<2019) %>%
  rename(geography=region) %>%
  mutate(source="SILC (2 year)") 

df_eu_silc_2_year_temp_EU <- df_eu_silc_2_year %>%
  group_by(year) %>%
  summarise(avg=wtd.mean(temp,weight_long)) %>%
  ungroup() %>%
  filter(year<2019) %>%
  mutate(geography="EU") %>%
  mutate(source="SILC (2 year)") 

df_eu_silc_2_year_temp <- rbind(df_eu_silc_2_year_temp_EU,df_eu_silc_2_year_temp_region,df_eu_silc_2_year_temp_country)
rm(df_eu_silc_2_year_temp_EU,df_eu_silc_2_year_temp_region,df_eu_silc_2_year_temp_country)

# Graph ----

df_graph <- rbind(df_eu_lfs_temp,df_eu_silc_4_year_temp,df_eu_silc_2_year_temp,df_eu_silc_xs_temp)
df_graph <- df_graph %>%
  filter(year>2004&year<2019,
         avg>0, # affects Denmark
         ) %>%
  filter(geography!="Germany",
         geography!="Luxembourg",
         )

df_graph$source <- fct_relevel(df_graph$source, "SILC (4 year)", after = 0) # forcats
df_graph$source <- fct_relevel(df_graph$source, "SILC (2 year)", after = 0) # forcats
df_graph$source <- fct_relevel(df_graph$source, "SILC (XS)", after = 0) # forcats
df_graph$source <- fct_relevel(df_graph$source, "LFS", after = 0) # forcats
table(df_graph$source)

df_graph$geography <- fct_relevel(df_graph$geography, "Southern", after = 0) # forcats
df_graph$geography <- fct_relevel(df_graph$geography, "Nordic", after = 0) # forcats
df_graph$geography <- fct_relevel(df_graph$geography, "Eastern", after = 0) # forcats
df_graph$geography <- fct_relevel(df_graph$geography, "Continental", after = 0) # forcats
df_graph$geography <- fct_relevel(df_graph$geography, "Anglophone", after = 0) # forcats
df_graph$geography <- fct_relevel(df_graph$geography, "EU", after = 0) # forcats

ggplot(df_graph, aes(x = year, y = avg, color=source, linetype=source)) +
  facet_wrap( ~ geography) +
  scale_x_continuous(breaks = c(2005,2011,2018), limits = c(2004, 2019)) +
  scale_color_manual(values=c("black","gray50","black","gray50")) +
  scale_linetype_manual(values=c("dashed","solid","solid","dashed")) +
  ylab("Temporary employment rate") +
  geom_line() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.title=element_blank(),
        axis.title.x=element_blank(),
        legend.key.width = unit(1.5,"cm"),
        legend.position = "bottom",
        axis.line.y = element_line(color="black", size=.5),
        axis.line.x = element_line(color="black", size=.5)
  )

ggsave(filename = paste0(graphs,"graph_eu_silc_compare_SILC_LFS.pdf"), plot = last_plot(), height = 8, width = 6, units = "in")
beep()
