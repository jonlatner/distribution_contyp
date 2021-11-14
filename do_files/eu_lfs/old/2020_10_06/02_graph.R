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
setwd("/Users/jonathanlatner/Google Drive/")
setwd("C:/Users/ba1ks6/Google Drive/")

data_files = "SECCOPA/projects/distribution_contyp/data_files/eu_lfs/"
graphs = "SECCOPA/projects/distribution_contyp/graphs/"

# LIBRARY
library(dplyr)
library(countrycode)
library(ggplot2)
library(Hmisc)
library(car)
library(forcats)
library(beepr)

# Load data -----------------------------------------

df_eu_lfs <- readRDS(file = paste0(data_files, "df_eu_lfs_sample_10.rds"))

# Clean data -----------------------------------------
df_eu_lfs <- droplevels(df_eu_lfs)

names(df_eu_lfs) <- tolower(names(df_eu_lfs))

df_eu_lfs <- df_eu_lfs %>%
        filter(age >=25 & age <=54) %>%
        filter(temp != 9) %>%
        mutate(temp = temp - 1) %>%
        mutate(female = sex - 1) %>%
        rename(edu_cat = hatlev1d,
               weight = coeff) 
        
df_eu_lfs$age_cat <- recode(df_eu_lfs$age, "25:34 = 1; 35:44=2; 45:54=3")
df_eu_lfs$age_cat <- factor(df_eu_lfs$age_cat, labels=c("Younger (< 35)", "Middle (35-44)", "Older (> 45)"))
df_eu_lfs$female <- factor(df_eu_lfs$female, labels=c("Male", "Female"))
df_eu_lfs$edu_cat <- factor(df_eu_lfs$edu_cat, 
                            levels = c("L","M","H"), 
                            labels=c("Lower edu (< Secondary)", "Middle (Secondary)", "Higher (> Secondary)"))

# Percent FTC -----------------------------------------
df_graph <- df_eu_lfs %>%
        group_by(country_name, country, year) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup()

df_graph_eu <- df_eu_lfs %>%
        group_by(year) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "EU-LFS", country = "EU")

df_graph <- rbind(df_graph,df_graph_eu)
rm(df_graph_eu)

df_graph <- df_graph %>%
        mutate(highlight_flag = ifelse(country_name == "EU-LFS", yes = "EU-LFS", "Countries"),
               highlight_flag = as.factor(highlight_flag),
               country_name = as.factor(country_name),
               highlight_flag = factor(highlight_flag, levels = rev(levels(highlight_flag))))
df_graph$country_name <- fct_relevel(df_graph$country_name, "EU-LFS", after = Inf) # forcats

ggplot() +
        geom_line(aes(x = year, y = avg, group = country_name, color = highlight_flag, size = highlight_flag), data = df_graph) +
        scale_color_manual(values = c("black", "gray")) + 
        scale_size_manual(values = c(1.5,.5)) +
        ylab("Temporary employment as % of total emp (25-54)") +
        xlab("Year") + 
        scale_x_continuous(breaks = c(seq(1996, 2011, by = 5), 2017), limits = c(1996, 2017)) +
        scale_y_continuous(breaks = seq(0, .5, by = .1), limits = c(0, .5)) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom"
        )

# df_test <- df_graph %>%
#         filter(country_name == "EU-LFS")
# View(df_test)

ggsave(filename = paste0(graphs,"graph_eu_lfs_pct_temp.png"), plot = last_plot(), height = 4.75, width = 8, units = "in", scale = 1)

df_graph$type <- "all"
df_graph$cat <- "Population"
df_graph_all <- df_graph

# Age -----------------------------------------
df_graph <- df_eu_lfs %>%
        group_by(country_name, country, year, age_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup()

df_graph_eu <- df_eu_lfs %>%
        group_by(year, age_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "EU-LFS", country = "EU")

df_graph <- rbind(df_graph,df_graph_eu)
rm(df_graph_eu)

df_graph <- df_graph %>%
        mutate(highlight_flag = ifelse(country_name == "EU-LFS", yes = "EU-LFS", "Countries"),
               highlight_flag = as.factor(highlight_flag),
               highlight_flag = factor(highlight_flag, levels = rev(levels(highlight_flag))))
df_graph$country_name <- fct_relevel(df_graph$country_name, "EU-LFS", after = Inf) # forcats

ggplot() +
        geom_line(aes(x = year, y = avg, group = country_name, color = highlight_flag, size = highlight_flag), data = df_graph) +
        scale_color_manual(values = c("black", "gray")) + 
        scale_size_manual(values = c(1.5,.5)) +
        facet_grid(~age_cat) +
        ylab("Temporary employment as % of total emp (25-54)") +
        xlab("Year") + 
        scale_x_continuous(breaks = c(seq(1996, 2011, by = 5), 2017), limits = c(1996, 2017)) +
        scale_y_continuous(breaks = seq(0, .5, by = .1), limits = c(0, .5)) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom"
        )

ggsave(filename = paste0(graphs,"graph_eu_lfs_pct_temp_by_age.png"), plot = last_plot(), height = 4.75, width = 8, units = "in", scale = 1)

df_graph$type <- "age"
df_graph <- rename(df_graph,cat = age_cat)
df_graph_age <- df_graph

# Education -----------------------------------------
df_graph <- df_eu_lfs %>%
        filter(!is.na(edu_cat)) %>%
        group_by(country_name, country, year, edu_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup()

df_graph_eu <- df_eu_lfs %>%
        filter(!is.na(edu_cat)) %>%
        group_by(year, edu_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "EU-LFS", country = "EU")

df_graph <- rbind(df_graph,df_graph_eu)
rm(df_graph_eu)

df_graph <- df_graph %>%
        mutate(highlight_flag = ifelse(country_name == "EU-LFS", yes = "EU-LFS", "Countries"),
               highlight_flag = as.factor(highlight_flag),
               highlight_flag = factor(highlight_flag, levels = rev(levels(highlight_flag))))
df_graph$country_name <- fct_relevel(df_graph$country_name, "EU-LFS", after = Inf) # forcats

# Graphs
ggplot() +
        geom_line(aes(x = year, y = avg, group = country_name, color = highlight_flag, size = highlight_flag), data = df_graph) +
        scale_color_manual(values = c("black", "gray")) + 
        scale_size_manual(values = c(1.5,.5)) +
        facet_grid(~edu_cat) +
        ylab("Temporary employment as % of total emp (25-54)") +
        xlab("Year") + 
        scale_x_continuous(breaks = c(seq(1996, 2011, by = 5), 2017), limits = c(1996, 2017)) +
        scale_y_continuous(breaks = seq(0, .5, by = .1), limits = c(0, .5)) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom"
        )

ggsave(filename = paste0(graphs,"graph_eu_lfs_pct_temp_by_edu.png"), plot = last_plot(), height = 4.75, width = 8, units = "in", scale = 1)

df_graph$type <- "edu"
df_graph <- rename(df_graph,cat = edu_cat)
df_graph_edu <- df_graph

# Gender -----------------------------------------
df_graph <- df_eu_lfs %>%
        group_by(country_name, country, year, female) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup()

df_graph_eu <- df_eu_lfs %>%
        group_by(year, female) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "EU-LFS", country = "EU")

df_graph <- rbind(df_graph,df_graph_eu)
rm(df_graph_eu)

df_graph <- df_graph %>%
        mutate(highlight_flag = ifelse(country_name == "EU-LFS", yes = "EU-LFS", "Countries"),
               highlight_flag = as.factor(highlight_flag),
               highlight_flag = factor(highlight_flag, levels = rev(levels(highlight_flag))))
df_graph$country_name <- fct_relevel(df_graph$country_name, "EU-LFS", after = Inf) # forcats

# Graphs
ggplot() +
        geom_line(aes(x = year, y = avg, group = country_name, color = highlight_flag, size = highlight_flag), data = df_graph) +
        scale_color_manual(values = c("black", "gray")) + 
        scale_size_manual(values = c(1.5,.5)) +
        facet_grid(~female) +
        ylab("Temporary employment as % of total emp (25-54)") +
        xlab("Year") + 
        scale_x_continuous(breaks = c(seq(1996, 2011, by = 5), 2017), limits = c(1996, 2017)) +
        scale_y_continuous(breaks = seq(0, .5, by = .1), limits = c(0, .5)) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom"
        )

ggsave(filename = paste0(graphs,"graph_eu_lfs_pct_temp_by_sex.png"), plot = last_plot(), height = 4.75, width = 8, units = "in", scale = 1)

df_graph$type <- "sex"
df_graph <- rename(df_graph,cat = female)
df_graph_sex <- df_graph

# Gender (v2) -----------------------------------------
df_graph <- df_eu_lfs %>%
        filter(temp == 1) %>%
        group_by(country_name, country, year) %>%
        summarise(avg = wtd.mean(as.numeric(female)-1,weight)) %>%
        ungroup()

df_graph_eu <- df_eu_lfs %>%
        filter(temp == 1) %>%
        group_by(year) %>%
        summarise(avg = wtd.mean(as.numeric(female)-1,weight)) %>%
        ungroup() %>%
        mutate(country_name = "EU-LFS", country = "EU")

df_graph <- rbind(df_graph,df_graph_eu)
rm(df_graph_eu)

df_graph <- df_graph %>%
        mutate(highlight_flag = ifelse(country_name == "EU-LFS", yes = "EU-LFS", "Countries"),
               highlight_flag = as.factor(highlight_flag),
               highlight_flag = factor(highlight_flag, levels = rev(levels(highlight_flag))))
df_graph$country_name <- fct_relevel(df_graph$country_name, "EU-LFS", after = Inf) # forcats

# Graphs
ggplot() +
        geom_line(aes(x = year, y = avg, group = country_name, color = highlight_flag, size = highlight_flag), data = df_graph) +
        scale_color_manual(values = c("black", "gray")) + 
        scale_size_manual(values = c(1.5,.5)) +
        ylab("% of temporary employment that is female") +
        xlab("Year") + 
        scale_x_continuous(breaks = c(seq(1996, 2011, by = 5), 2017), limits = c(1996, 2017)) +
        scale_y_continuous(breaks = seq(0, 1, by = .2), limits = c(0, 1)) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom"
        )

ggsave(filename = paste0(graphs,"graph_eu_lfs_pct_female.png"), plot = last_plot(), height = 4.75, width = 8, units = "in", scale = 1)

# beep()

# combine -----------------------------------------

df_combo <- rbind(df_graph_all,df_graph_sex,df_graph_edu,df_graph_age)

df_graph <- df_graph %>%
        mutate(highlight_flag = ifelse(country_name == "EU-LFS", yes = "EU-LFS", "Countries"),
               highlight_flag = as.factor(highlight_flag),
               highlight_flag = factor(highlight_flag, levels = rev(levels(highlight_flag))))
df_combo$highlight_flag <- fct_relevel(df_graph$country_name, "EU-LFS", after = Inf) # forcats

df_combo$highlight_flag <- factor(df_combo$highlight_flag,
                       levels=c("EU-LFS", "Countries"),
                       labels=c("Group avg. (EU-LFS)", "Countries")
                       )

df_combo$type <- factor(df_combo$type,
                       levels=c("all","sex","age","edu"),
                       labels=c("Population", "Gender", "Age group", "Education")
                       )

df_combo$cat <- factor(df_combo$cat,
                       levels=c("Population",
                                "Male","Female",
                                "Younger (< 35)", "Middle (35-44)", "Older (> 45)",
                                "Lower edu (< Secondary)", "Middle (Secondary)", "Higher (> Secondary)"
                       ),
                       labels=c("(a) Population",
                                "(b) Male","c) Female",
                                "(d) Younger age (< 35)", "(e) Middle age (35-44)", "(f) Older age (> 45)",
                                "(g) Lower edu (< Secondary)", "(h) Middle edu (Secondary)", "(i) Higher edu (> Secondary)"
                                         
                       )
)

df_pop_avg <- df_graph_all %>%
        filter(country_name == "EU-LFS") %>%
        select(year, avg) %>%
        rename(pop_avg = avg)

ggplot() +
        geom_line(aes(x = year, y = avg, group = country_name, color = highlight_flag, size = highlight_flag), data = df_combo) +
        geom_line(data = df_pop_avg, aes(x = year, y = pop_avg, color = "Population avg. (EU-LFS)", size = "Population avg. (EU-LFS)")) +
        scale_color_manual(values = c("gray", "black", "black")) + 
        scale_size_manual(values = c(.5, 1.5, .5)) +
        ylab("Temporary employment rate (%)") +
        facet_wrap(~cat, ncol = 3) +
        xlab("Year") + 
        scale_x_continuous(breaks = c(seq(1996, 2011, by = 5), 2017), limits = c(1996, 2017)) +
        scale_y_continuous(breaks = seq(0, .5, by = .1), limits = c(0, .5)) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom"
        )

ggsave(filename = paste0(graphs,"graph_eu_lfs_pct_combo.png"), plot = last_plot(), height = 9.5, width = 6.75, units = "in", scale = 1)
ggsave(filename = paste0(graphs,"graph_eu_lfs_pct_combo.pdf"), plot = last_plot(), height = 9.5, width = 6.75, units = "in", scale = 1)
