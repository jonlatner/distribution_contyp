# Top commands--------------------------------------------------------------

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
tables = "tables/eu_silc/"

# LIBRARY
library(tidyverse)
library(xtable)

options(scipen=999)

# Load data--------------------------------------------------------------

df_eu_silc <- readRDS(file = paste0(data_files,"03_df_eu_silc_clean.rds"))

# Geography data--------------------------------------------------------------

country_name <- c("Switzerland", "Luxembourg", "Belgium", "Austria", "Netherlands", "France", 
                  "United Kingdom", "Ireland", 
                  "Malta", "Greece", "Italy", "Cyprus", "Portugal", "Spain", 
                  "Romania", "Poland", "Croatia", "Hungary", "Czechia", "Bulgaria", "Slovenia", "Slovakia", "Lithuania", "Estonia", "Latvia", "Serbia", "Turkey",
                  "Sweden", "Denmark", "Norway", "Finland", "Iceland")
region <- c("Continental", "Continental", "Continental", "Continental", "Continental", "Continental", 
            "Anglophone", "Anglophone", 
            "Southern", "Southern", "Southern", "Southern", "Southern", "Southern", 
            "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern",
            "Nordic", "Nordic", "Nordic", "Nordic", "Nordic")

geography <- cbind(country_name, region)

rm(country_name, region)

# Country --------------------------------------------------------------

df_country_panel <- data.frame(with(df_eu_silc, table(country_name,panel)))
df_country_panel <- filter(df_country_panel, Freq>0) %>% arrange(panel,country_name)

# Total by panel year
df_country_panel_1 <- df_country_panel %>%
        group_by(panel) %>%
        summarize(total=sum(Freq)) %>%
        ungroup() %>%
        mutate(country_name="Total") %>%
        rename(Freq=total)

df_country_panel <- rbind(df_country_panel,df_country_panel_1)

# Total observations, by country
df_country_panel_2 <- df_country_panel %>%
        group_by(country_name) %>%
        summarize(total=sum(Freq)) %>%
        ungroup()

# Total number of panels, by country
df_country_panel_3 <- df_country_panel %>%
        group_by(country_name) %>%
        tally() %>%
        ungroup() %>%
        filter(country_name != "Total")


# Total number of panels, by region
df_country_panel_3 <- df_country_panel %>%
  group_by(country_name) %>%
  tally() %>%
  ungroup() %>%
  filter(country_name != "Total")

# Total across countries
df_country_panel_4 <- df_country_panel_3 %>%
  summarize(n=sum(n)) %>%
  mutate(country_name = "Total")
df_country_panel_3 <- rbind(df_country_panel_3,df_country_panel_4)

# Reshape
df_country_panel <- pivot_wider(df_country_panel, names_from = panel, values_from = Freq)

# Merge totals
df_country_panel_clean <- merge(df_country_panel,df_country_panel_2)
df_country_panel_clean <- merge(df_country_panel_clean,df_country_panel_3)

rm(df_country_panel_1,df_country_panel_2,df_country_panel_3,df_country_panel_4,df_country_panel)

# Region --------------------------------------------------------------
df_region_panel <- merge(df_eu_silc,geography)
df_region_panel <- data.frame(with(df_region_panel, table(region,panel)))
df_region_panel <- filter(df_region_panel, Freq>0) %>% arrange(panel,region)

# Total by panel year
df_region_panel_1 <- df_region_panel %>%
  group_by(panel) %>%
  summarize(total=sum(Freq)) %>%
  ungroup() %>%
  mutate(region="Total") %>%
  rename(Freq=total)

df_region_panel <- rbind(df_region_panel,df_region_panel_1)

# Total observations, by region
df_region_panel_2 <- df_region_panel %>%
  group_by(region) %>%
  summarize(total=sum(Freq)) %>%
  ungroup()

# Total number of panels, by region
df_region_panel_3 <- df_region_panel %>%
  group_by(region) %>%
  tally() %>%
  ungroup() %>%
  filter(region != "Total")


# Total number of panels, by region
df_region_panel_3 <- df_region_panel %>%
  group_by(region) %>%
  tally() %>%
  ungroup() %>%
  filter(region != "Total")

# Total across countries
df_region_panel_4 <- df_region_panel_3 %>%
  summarize(n=sum(n)) %>%
  mutate(region = "Total")
df_region_panel_3 <- rbind(df_region_panel_3,df_region_panel_4)

# Reshape
df_region_panel <- pivot_wider(df_region_panel, names_from = panel, values_from = Freq)

# Merge totals
df_region_panel_clean <- merge(df_region_panel,df_region_panel_2)
df_region_panel_clean <- merge(df_region_panel_clean,df_region_panel_3)

rm(df_region_panel_1,df_region_panel_2,df_region_panel_3,df_region_panel_4,df_region_panel,geography)

df_region_panel_clean <- df_region_panel_clean %>%
  rename(country_name=region) %>%
  filter(country_name!="Total")

# Create table --------------------------------------------------------------

df_table <- rbind(df_country_panel_clean,df_region_panel_clean)

table(df_table$country_name)

# Clean table 
# order country name 
df_table$country_name <- factor(df_table$country_name,
                                levels = c("Anglophone", "Ireland", "United Kingdom", # Anglophone
                                           "Continental",  "Austria", "Belgium", "France", "Germany", "Luxembourg", "Netherlands", "Switzerland", # Continental
                                           "Eastern", "Bulgaria", "Croatia", "Czechia", "Estonia", "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Serbia", "Slovakia", "Slovenia", # Eastern
                                           "Nordic",  "Denmark", "Finland", "Iceland", "Norway", "Sweden", # Nordic
                                           "Southern",  "Cyprus", "Greece", "Italy", "Malta", "Portugal", "Spain", # Southern 
                                           "Total" # Total
                                ))

df_table <- arrange(df_table, country_name)

# Create table --------------------------------------------------------------

df_table$country_name <- NULL
        
# VARIABLE LABLES
rownames(df_table) <- c(
  "\\\\[-1.8ex]
  Anglophone countries:",
  "\\hspace{5mm} Ireland", 
  "\\hspace{5mm} United Kingdom",
  
  "\\multicolumn{14}{l}{\\phantom{empty}} \\\\
  Continental countries:",
  "\\hspace{5mm} Austria", 
  "\\hspace{5mm} Belgium", 
  "\\hspace{5mm} France", 
  "\\hspace{5mm} Luxembourg", 
  "\\hspace{5mm} Netherlands", 
  "\\hspace{5mm} Switzerland", 
  
  "\\multicolumn{14}{l}{\\phantom{empty}} \\\\
  Eastern countries:",
  "\\hspace{5mm} Bulgaria", 
  "\\hspace{5mm} Croatia", 
  "\\hspace{5mm} Czechia", 
  "\\hspace{5mm} Estonia", 
  "\\hspace{5mm} Hungary", 
  "\\hspace{5mm} Latvia", 
  "\\hspace{5mm} Lithuania", 
  "\\hspace{5mm} Poland", 
  "\\hspace{5mm} Romania", 
  "\\hspace{5mm} Serbia", 
  "\\hspace{5mm} Slovakia", 
  "\\hspace{5mm} Slovenia", 
  
  "\\multicolumn{14}{l}{\\phantom{empty}} \\\\
  Nordic countries:",
  "\\hspace{5mm} Denmark", 
  "\\hspace{5mm} Finland", 
  "\\hspace{5mm} Iceland", 
  "\\hspace{5mm} Norway", 
  "\\hspace{5mm} Sweden", 
  
  "\\multicolumn{14}{l}{\\phantom{empty}} \\\\
  Southern countries:",
  "\\hspace{5mm} Cyprus", 
  "\\hspace{5mm} Greece", 
  "\\hspace{5mm} Italy", 
  "\\hspace{5mm} Malta", 
  "\\hspace{5mm} Portugal", 
  "\\hspace{5mm} Spain", 
  
  "\\multicolumn{14}{l}{\\phantom{empty}} \\\\
  EU-SILC" # Total
)

header <- c("[-1.8ex] \\multicolumn{1}{l}{Country} & \\multicolumn{12}{l}{Four-year panel period ending} & \\multicolumn{2}{l}{Total} \\\\ \n
                    \\cmidrule(r){1-1} \\cmidrule(r){2-13} \\cmidrule(r){14-15} \n
            & 2008 & 2009 & 2010 & 2011 & 2012 & 2013 & 2014 & 2015 & 2016 & 2017 & 2018 & 2019 & Observations & Periods \\\\ \n")
hline_top <- ("\\\\[-1.8ex]\\hline \\\\ \n")
hline_bot <- c("\\hline\\hline \\\\[-1.8ex] \n") 

t <- xtable(df_table)
align(t) <- c("l", 
              "r", "r", "r", 
              "r", "r", "r", 
              "r", "r", "r", 
              "r", "r", "r",
              "r", "r")

print(t, 
      file = paste0(tables,"descriptives_table_country_panel_num.tex"),
      include.rownames = TRUE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      format.args = list(big.mark = ".", decimal.mark = ","),
      hline.after = FALSE,
      add.to.row = list(
              pos = list(0,0,37),
              command = c(hline_top,
                          header,
                          hline_bot)),
      comment = FALSE
)
