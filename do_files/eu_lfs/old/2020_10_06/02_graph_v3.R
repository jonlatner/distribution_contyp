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
# setwd("C:/Users/ba1ks6/Google Drive/")

data_files = "SECCOPA/projects/distribution_contyp/data_files/eu_lfs/"
graphs = "SECCOPA/projects/distribution_contyp/graphs/"

# LIBRARY
library(dplyr)
library(ggplot2)
library(Hmisc)
library(car)
library(forcats)
library(beepr)
library(data.table)

## The function to get overlapping strip labels
OverlappingStripLabels = function(plot) {
        
        # Get the ggplot grob
        pg = ggplotGrob(plot)
        
        ### Collect some information about the strips from the plot
        # Get a list of strips
        stripr = lapply(grep("strip-r", pg$layout$name), function(x) {pg$grobs[[x]]})
        
        stript = lapply(grep("strip-t", pg$layout$name), function(x) {pg$grobs[[x]]})
        
        # Number of strips
        NumberOfStripsr = sum(grepl(pattern = "strip-r", pg$layout$name))
        NumberOfStripst = sum(grepl(pattern = "strip-t", pg$layout$name))
        
        # Number of columns
        NumberOfCols = length(stripr[[1]])
        NumberOfRows = length(stript[[1]])
        
        # Panel spacing
        plot_theme <- function(p) {
                plyr::defaults(p$theme, theme_get())
        }
        PanelSpacing = plot_theme(plot)$panel.spacing
        
        # Map the boundaries of the new strips
        Nlabelr = vector("list", NumberOfCols)
        mapr = vector("list", NumberOfCols)
        for(i in 1:NumberOfCols) {
                
                for(j in 1:NumberOfStripsr) {
                        Nlabelr[[i]][j] = getGrob(grid.force(stripr[[j]]$grobs[[i]]), gPath("GRID.text"), grep = TRUE)$label
                }
                
                mapr[[i]][1] = TRUE
                for(j in 2:NumberOfStripsr) {
                        mapr[[i]][j] = as.character(Nlabelr[[i]][j]) != as.character(Nlabelr[[i]][j-1])#Nlabelr[[i]][j] != Nlabelr[[i]][j-1]
                }
        }
        
        # Map the boundaries of the new strips
        Nlabelt = vector("list", NumberOfRows)
        mapt = vector("list", NumberOfRows)
        for(i in 1:NumberOfRows) {
                
                for(j in 1:NumberOfStripst) {
                        Nlabelt[[i]][j] = getGrob(grid.force(stript[[j]]$grobs[[i]]), gPath("GRID.text"), grep = TRUE)$label
                }
                
                mapt[[i]][1] = TRUE
                for(j in 2:NumberOfStripst) {
                        mapt[[i]][j] = as.character(Nlabelt[[i]][j]) != as.character(Nlabelt[[i]][j-1])#Nlabelt[[i]][j] != Nlabelt[[i]][j-1]
                }
        }
        
        
        ## Construct gtable to contain the new strip
        newStripr  = gtable(heights = unit.c(rep(unit.c(unit(1, "null"), PanelSpacing), NumberOfStripsr-1), unit(1, "null")), 
                            widths = stripr[[1]]$widths)
        ## Populate the gtable  
        seqTop = list()
        for(i in NumberOfCols:1) {  
                Top = which(mapr[[i]] == TRUE)
                seqTop[[i]] = if(i == NumberOfCols) 2*Top - 1 else  sort(unique(c(seqTop[[i+1]], 2*Top - 1)))  
                seqBottom = c(seqTop[[i]][-1] -2, (2*NumberOfStripsr-1))
                newStripr = gtable_add_grob(newStripr, lapply(stripr[(seqTop[[i]]+1)/2], function(x) x[[1]][[i]]), l = i, t = seqTop[[i]], b = seqBottom)
        }
        
        mapt <- mapt[NumberOfRows:1]
        Nlabelt <- Nlabelt[NumberOfRows:1]
        ## Do the same for top facets
        newStript  = gtable(heights = stript[[1]]$heights,
                            widths = unit.c(rep(unit.c(unit(1, "null"), PanelSpacing), NumberOfStripst-1), unit(1, "null")))
        seqTop = list()
        for(i in NumberOfRows:1) {  
                Top = which(mapt[[i]] == TRUE)
                seqTop[[i]] = if(i == NumberOfRows) 2*Top - 1 else  sort(unique(c(seqTop[[i+1]], 2*Top - 1)))  
                seqBottom = c(seqTop[[i]][-1] -2, (2*NumberOfStripst-1))
                # newStript = gtable_add_grob(newStript, lapply(stript[(seqTop[[i]]+1)/2], function(x) x[[1]][[i]]), l = i, t = seqTop[[i]], b = seqBottom)
                newStript = gtable_add_grob(newStript, lapply(stript[(seqTop[[i]]+1)/2], function(x) x[[1]][[(NumberOfRows:1)[i]]]), t = (NumberOfRows:1)[i], l = seqTop[[i]], r = seqBottom)
        }
        
        ## Put the strip into the plot
        # Get the locations of the original strips
        posr = subset(pg$layout, grepl("strip-r", pg$layout$name), t:r)
        post = subset(pg$layout, grepl("strip-t", pg$layout$name), t:r)
        
        ## Use these to position the new strip
        pgNew = gtable_add_grob(pg, newStripr, t = min(posr$t), l = unique(posr$l), b = max(posr$b))
        pgNew = gtable_add_grob(pgNew, newStript, l = min(post$l), r = max(post$r), t=unique(post$t))
        grid.draw(pgNew)
        
        return(pgNew)
}

# Load data -----------------------------------------

df_eu_lfs <- readRDS(file = paste0(data_files, "df_eu_lfs_sample_10.rds"))
# df_eu_lfs <- readRDS(file = paste0(data_files, "df_eu_lfs.rds"))

t <- unique(df_eu_lfs$country)

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
df_eu_lfs <- droplevels(df_eu_lfs)

names(df_eu_lfs) <- tolower(names(df_eu_lfs))

df_eu_lfs <- df_eu_lfs %>%
        filter(year >= 1996) %>%
        filter(age >=25 & age <=54) %>%
        filter(temp != 9) %>%
        mutate(temp = temp - 1) %>%
        mutate(female = sex - 1) %>%
        rename(edu_cat = hatlev1d,
               weight = coeff) 

df_eu_lfs <- merge(data.table(df_eu_lfs),data.table(region), by = c("country"), all.x = TRUE)

df_eu_lfs$age_cat <- recode(df_eu_lfs$age, "25:34 = 1; 35:44=2; 45:54=3")
df_eu_lfs$age_cat <- factor(df_eu_lfs$age_cat, labels=c("< 35", "35-44", "> 45"))
df_eu_lfs$female <- factor(df_eu_lfs$female, labels=c("Male", "Female"))
df_eu_lfs$edu_cat <- factor(df_eu_lfs$edu_cat, 
                            levels = c("L","M","H"), 
                            labels=c("< Secondary", "Secondary", "> Secondary"))

# Graph percent FTC -----------------------------------------

df_graph_countries <- df_eu_lfs %>%
        group_by(region, country_name, year) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Countries") %>%
        mutate(period = ifelse(year<2007, yes = 0, no = 1),
               year2 = ifelse(period == 0, 
                              yes = year - 1995,
                              no = year - 2006))

df_graph_regions <- df_eu_lfs %>%
        group_by(region, year) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country_name = region) %>%
        mutate(period = ifelse(year<2007, yes = 0, no = 1),
               year2 = ifelse(period == 0, 
                              yes = year - 1995,
                              no = year - 2006))


df_graph_eu <- df_eu_lfs %>%
        group_by(year) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "Europe", region = "Europe", level = "Europe") %>%
        mutate(period = ifelse(year<2007, yes = 0, no = 1),
               year2 = ifelse(period == 0, 
                              yes = year - 1995,
                              no = year - 2006))

df_grand_region_mean <- data.frame()
region = unique(df_graph_regions$region)
for(r in region) {
        df_test <- df_graph_eu
        df_test$region <- r
        df_grand_region_mean <- rbind(df_grand_region_mean,df_test)
        
}

rm(df_test,r,region)

df_graph <- rbind(df_graph_countries,df_graph_regions,df_grand_region_mean)
rm(df_graph_countries,df_graph_regions,df_grand_region_mean,df_graph_eu)

df_graph$level <- factor(df_graph$level,
                         levels = c("Europe", "Region", "Countries"))

df_graph$period <- factor(df_graph$period,
                          levels = c("0", "1"),
                          labels = c("1996-2006", "2007-2017"))

table(df_graph$level)
table(df_graph$region)

df_graph$country_name <- fct_relevel(df_graph$country_name, "Southern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Nordic", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Eastern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Continental", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Anglophone", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Europe", after = Inf) # forcats

df_graph$label_region <- "Region"
df_graph$label_period <- "Time period"

library(grid)
library(gtable)
library(plyr)

p <- ggplot(df_graph, aes(x = year2, y = avg, group = country_name, linetype = level, color = level, size = level)) +
        facet_grid(label_period + period ~ label_region + region) +
        scale_size_manual(values = c(1,1,.5)) +
        scale_linetype_manual(values = c("solid","dashed","solid")) +
        scale_color_manual(values = c("black", "black", "gray")) +
        geom_line() +
        scale_x_continuous(breaks = c(seq(1, 11, by = 2)), limits = c(1, 11)) +
        # scale_y_continuous(breaks = seq(0, .5, by = .1), limits = c(0, .5)) +
        ylab("Temporary employment as % of total emp (25-54)") +
        xlab("Period") + 
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom",
              axis.title.y = element_text(size = 9),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

## Draw the plot
grid.newpage()
grid.draw(OverlappingStripLabels(p))

cairo_pdf(paste0(graphs,"graph_ftc_rate_region_country.pdf"), height = 4, width = 6)
# png(paste0(graphs,"graph_ftc_rate_region_country.png"), height = 4, width = 6, units = "in", res=600)
grid.draw(OverlappingStripLabels(p))
dev.off()
detach("package:plyr", unload = TRUE)
detach("package:data.table", unload = TRUE)

# Gender -----------------------------------------

df_graph_countries <- df_eu_lfs %>%
        filter(year>2006) %>%
        group_by(region, country_name, year, female) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Countries")

df_graph_regions <- df_eu_lfs %>%
        filter(year>2006) %>%
        group_by(region, year, female) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country_name = region)

df_graph_eu <- df_eu_lfs %>%
        filter(year>2006) %>%
        group_by(year,female) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "Europe", region = "Europe", level = "Europe")

df_grand_region_mean <- data.frame()
region = unique(df_graph_regions$region)
for(r in region) {
        df_test <- df_graph_eu
        df_test$region <- r
        df_grand_region_mean <- rbind(df_grand_region_mean,df_test)
        
}
rm(df_test,r,region)

df_graph <- rbind(df_graph_countries,df_graph_regions,df_grand_region_mean)
rm(df_graph_countries,df_graph_regions,df_grand_region_mean,df_graph_eu)

df_graph_gender <- df_graph %>%
        rename(variable=female) %>%
        mutate(factor = "Gender")

# Age -----------------------------------------

df_graph_countries <- df_eu_lfs %>%
        filter(year>2006) %>%
        group_by(region, country_name, year, age_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Countries")

df_graph_regions <- df_eu_lfs %>%
        filter(year>2006) %>%
        group_by(region, year, age_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country_name = region)

df_graph_eu <- df_eu_lfs %>%
        filter(year>2006) %>%
        group_by(year,age_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "Europe", region = "Europe", level = "Europe")

df_grand_region_mean <- data.frame()
region = unique(df_graph_regions$region)
for(r in region) {
        df_test <- df_graph_eu
        df_test$region <- r
        df_grand_region_mean <- rbind(df_grand_region_mean,df_test)
        
}
rm(df_test,r,region)

df_graph <- rbind(df_graph_countries,df_graph_regions,df_grand_region_mean)
rm(df_graph_countries,df_graph_regions,df_grand_region_mean,df_graph_eu)

df_graph_age <- df_graph %>%
        rename(variable=age_cat) %>%
        mutate(factor = "Age")

# Edu -----------------------------------------

df_graph_countries <- df_eu_lfs %>%
        filter(!is.na(edu_cat)) %>%
        filter(year>2006) %>%
        group_by(region, country_name, year, edu_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Countries")

df_graph_regions <- df_eu_lfs %>%
        filter(!is.na(edu_cat)) %>%
        filter(year>2006) %>%
        group_by(region, year, edu_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country_name = region)

df_graph_eu <- df_eu_lfs %>%
        filter(!is.na(edu_cat)) %>%
        filter(year>2006) %>%
        group_by(year,edu_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "Europe", region = "Europe", level = "Europe")

df_grand_region_mean <- data.frame()
region = unique(df_graph_regions$region)
for(r in region) {
        df_test <- df_graph_eu
        df_test$region <- r
        df_grand_region_mean <- rbind(df_grand_region_mean,df_test)
        
}
rm(df_test,r,region)

df_graph <- rbind(df_graph_countries,df_graph_regions,df_grand_region_mean)
rm(df_graph_countries,df_graph_regions,df_grand_region_mean,df_graph_eu)

df_graph_edu <- df_graph %>%
        rename(variable=edu_cat) %>%
        mutate(factor = "Education")

# Combine graphs -----------------------------------------

df_graph <- rbind(df_graph_gender,df_graph_age,df_graph_edu)

rm(df_graph_gender,df_graph_age,df_graph_edu)

df_graph$level <- factor(df_graph$level,
                         levels = c("Europe", "Region", "Countries"))

df_test <- df_graph %>%
        filter(year>=2007) %>%
        filter(level== "Region") %>%
        group_by(variable,region) %>%
        arrange(variable,region,year) %>%
        summarise(first = first(avg),
                  last = last(avg),
                  diff=last-first,
                  change = last(avg)/first(avg)-1) %>%
        ungroup() %>%
        filter(diff < -.03 | diff > .03)

filter(change < -.3 | change >.3)

df_graph$country_name <- fct_relevel(df_graph$country_name, "Southern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Nordic", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Eastern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Continental", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Anglophone", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Europe", after = Inf) # forcats
df_graph$label_region <- "Region"

library(grid)
library(gtable)
library(plyr)

p <- ggplot(df_graph, aes(x = year, y = avg, group = country_name, linetype = level, color = level, size = level)) +
        facet_grid(factor + variable ~ label_region + region,scales = "free_y") +
        scale_size_manual(values = c(1,1,.5)) +
        scale_linetype_manual(values = c("solid","dashed","solid")) +
        scale_color_manual(values = c("black", "black", "gray")) +
        geom_line() +
        scale_x_continuous(breaks = c(seq(2007, 2013, by = 3), 2017), limits = c(2006.5, 2017.5)) +
        # scale_y_continuous(breaks = seq(0, .5, by = .1), limits = c(0, .5)) +
        ylab("Temporary employment as % of total emp (25-54)") +
        xlab("Period") + 
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom",
              axis.title.y = element_text(size = 9),
              axis.text.x = element_text(size=7),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

## Draw the plot
grid.newpage()
grid.draw(OverlappingStripLabels(p))

cairo_pdf(paste0(graphs,"graph_ftc_rate_region_country_group_period_2.pdf"), height = 8, width = 6)
# png(paste0(graphs,"graph_ftc_rate_region_country.png"), height = 4, width = 6, units = "in", res=600)
grid.draw(OverlappingStripLabels(p))
dev.off()

detach("package:plyr", unload = TRUE)


# Gender -----------------------------------------

df_graph_countries <- df_eu_lfs %>%
        filter(year>2006) %>%
        group_by(region, country_name, year, female) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Countries")

df_graph_regions <- df_eu_lfs %>%
        filter(year>2006) %>%
        group_by(region, year, female) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country_name = region)

df_graph_eu <- df_eu_lfs %>%
        filter(year>2006) %>%
        group_by(year,female) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "Europe", region = "Europe", level = "Europe")

df_grand_region_mean <- data.frame()
region = unique(df_graph_regions$region)
for(r in region) {
        df_test <- df_graph_eu
        df_test$region <- r
        df_grand_region_mean <- rbind(df_grand_region_mean,df_test)
        
}
rm(df_test,r,region)

df_graph <- rbind(df_graph_countries,df_graph_regions,df_grand_region_mean)
rm(df_graph_countries,df_graph_regions,df_grand_region_mean,df_graph_eu)

df_graph_gender <- df_graph %>%
        rename(variable=female) %>%
        mutate(factor = "Gender")

# Age -----------------------------------------

df_graph_countries <- df_eu_lfs %>%
        filter(year>2006) %>%
        group_by(region, country_name, year, age_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Countries")

df_graph_regions <- df_eu_lfs %>%
        filter(year>2006) %>%
        group_by(region, year, age_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country_name = region)

df_graph_eu <- df_eu_lfs %>%
        filter(year>2006) %>%
        group_by(year,age_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "Europe", region = "Europe", level = "Europe")

df_grand_region_mean <- data.frame()
region = unique(df_graph_regions$region)
for(r in region) {
        df_test <- df_graph_eu
        df_test$region <- r
        df_grand_region_mean <- rbind(df_grand_region_mean,df_test)
        
}
rm(df_test,r,region)

df_graph <- rbind(df_graph_countries,df_graph_regions,df_grand_region_mean)
rm(df_graph_countries,df_graph_regions,df_grand_region_mean,df_graph_eu)

df_graph_age <- df_graph %>%
        rename(variable=age_cat) %>%
        mutate(factor = "Age")

# Edu -----------------------------------------

df_graph_countries <- df_eu_lfs %>%
        filter(!is.na(edu_cat)) %>%
        filter(year>2006) %>%
        group_by(region, country_name, year, edu_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Countries")

df_graph_regions <- df_eu_lfs %>%
        filter(!is.na(edu_cat)) %>%
        filter(year>2006) %>%
        group_by(region, year, edu_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country_name = region)

df_graph_eu <- df_eu_lfs %>%
        filter(!is.na(edu_cat)) %>%
        filter(year>2006) %>%
        group_by(year,edu_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "Europe", region = "Europe", level = "Europe")

df_grand_region_mean <- data.frame()
region = unique(df_graph_regions$region)
for(r in region) {
        df_test <- df_graph_eu
        df_test$region <- r
        df_grand_region_mean <- rbind(df_grand_region_mean,df_test)
        
}
rm(df_test,r,region)

df_graph <- rbind(df_graph_countries,df_graph_regions,df_grand_region_mean)
rm(df_graph_countries,df_graph_regions,df_grand_region_mean,df_graph_eu)

df_graph_edu <- df_graph %>%
        rename(variable=edu_cat) %>%
        mutate(factor = "Education")

# Combine graphs -----------------------------------------

df_graph <- rbind(df_graph_gender,df_graph_age,df_graph_edu)

rm(df_graph_gender,df_graph_age,df_graph_edu)

df_graph$level <- factor(df_graph$level,
                         levels = c("Europe", "Region", "Countries"))


df_graph$country_name <- fct_relevel(df_graph$country_name, "Southern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Nordic", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Eastern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Continental", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Anglophone", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Europe", after = Inf) # forcats
df_graph$label_region <- "Region"

library(grid)
library(gtable)
library(plyr)

p <- ggplot(df_graph, aes(x = year, y = avg, group = country_name, linetype = level, color = level, size = level)) +
        facet_grid(factor + variable ~ label_region + region,scales = "free_y") +
        scale_size_manual(values = c(1,1,.5)) +
        scale_linetype_manual(values = c("solid","dashed","solid")) +
        scale_color_manual(values = c("black", "black", "gray")) +
        geom_line() +
        scale_x_continuous(breaks = c(seq(2007, 2013, by = 3), 2017), limits = c(2006.5, 2017.5)) +
        # scale_y_continuous(breaks = seq(0, .5, by = .1), limits = c(0, .5)) +
        ylab("Temporary employment as % of total emp (25-54)") +
        xlab("Period") + 
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom",
              axis.title.y = element_text(size = 9),
              axis.text.x = element_text(size=7),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

## Draw the plot
grid.newpage()
grid.draw(OverlappingStripLabels(p))

cairo_pdf(paste0(graphs,"graph_ftc_rate_region_country_group_period_2.pdf"), height = 8, width = 6)
# png(paste0(graphs,"graph_ftc_rate_region_country.png"), height = 4, width = 6, units = "in", res=600)
grid.draw(OverlappingStripLabels(p))
dev.off()

detach("package:plyr", unload = TRUE)


# Gender -----------------------------------------

df_graph_countries <- df_eu_lfs %>%
        filter(year<2007) %>%
        group_by(region, country_name, year, female) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Countries")

df_graph_regions <- df_eu_lfs %>%
        filter(year<2007) %>%
        group_by(region, year, female) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country_name = region)

df_graph_eu <- df_eu_lfs %>%
        filter(year<2007) %>%
        group_by(year,female) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "Europe", region = "Europe", level = "Europe")

df_grand_region_mean <- data.frame()
region = unique(df_graph_regions$region)
for(r in region) {
        df_test <- df_graph_eu
        df_test$region <- r
        df_grand_region_mean <- rbind(df_grand_region_mean,df_test)
        
}
rm(df_test,r,region)

df_graph <- rbind(df_graph_countries,df_graph_regions,df_grand_region_mean)
rm(df_graph_countries,df_graph_regions,df_grand_region_mean,df_graph_eu)

df_graph_gender <- df_graph %>%
        rename(variable=female) %>%
        mutate(factor = "Gender")

# Age -----------------------------------------

df_graph_countries <- df_eu_lfs %>%
        filter(year<2007) %>%
        group_by(region, country_name, year, age_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Countries")

df_graph_regions <- df_eu_lfs %>%
        filter(year<2007) %>%
        group_by(region, year, age_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country_name = region)

df_graph_eu <- df_eu_lfs %>%
        filter(year<2007) %>%
        group_by(year,age_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "Europe", region = "Europe", level = "Europe")

df_grand_region_mean <- data.frame()
region = unique(df_graph_regions$region)
for(r in region) {
        df_test <- df_graph_eu
        df_test$region <- r
        df_grand_region_mean <- rbind(df_grand_region_mean,df_test)
        
}
rm(df_test,r,region)

df_graph <- rbind(df_graph_countries,df_graph_regions,df_grand_region_mean)
rm(df_graph_countries,df_graph_regions,df_grand_region_mean,df_graph_eu)

df_graph_age <- df_graph %>%
        rename(variable=age_cat) %>%
        mutate(factor = "Age")

# Edu -----------------------------------------

df_graph_countries <- df_eu_lfs %>%
        filter(!is.na(edu_cat)) %>%
        filter(year<2007) %>%
        group_by(region, country_name, year, edu_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Countries")

df_graph_regions <- df_eu_lfs %>%
        filter(!is.na(edu_cat)) %>%
        filter(year<2007) %>%
        group_by(region, year, edu_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country_name = region)

df_graph_eu <- df_eu_lfs %>%
        filter(!is.na(edu_cat)) %>%
        filter(year<2007) %>%
        group_by(year,edu_cat) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "Europe", region = "Europe", level = "Europe")

df_grand_region_mean <- data.frame()
region = unique(df_graph_regions$region)
for(r in region) {
        df_test <- df_graph_eu
        df_test$region <- r
        df_grand_region_mean <- rbind(df_grand_region_mean,df_test)
        
}
rm(df_test,r,region)

df_graph <- rbind(df_graph_countries,df_graph_regions,df_grand_region_mean)
rm(df_graph_countries,df_graph_regions,df_grand_region_mean,df_graph_eu)

df_graph_edu <- df_graph %>%
        rename(variable=edu_cat) %>%
        mutate(factor = "Education")

# Combine graphs -----------------------------------------

df_graph <- rbind(df_graph_gender,df_graph_age,df_graph_edu)

rm(df_graph_gender,df_graph_age,df_graph_edu)

df_graph$level <- factor(df_graph$level,
                         levels = c("Europe", "Region", "Countries"))


df_graph$country_name <- fct_relevel(df_graph$country_name, "Southern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Nordic", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Eastern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Continental", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Anglophone", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Europe", after = Inf) # forcats
df_graph$label_region <- "Region"

library(grid)
library(gtable)
library(plyr)

p <- ggplot(df_graph, aes(x = year, y = avg, group = country_name, linetype = level, color = level, size = level)) +
        facet_grid(factor + variable ~ label_region + region,scales = "free_y") +
        scale_size_manual(values = c(1,1,.5)) +
        scale_linetype_manual(values = c("solid","dashed","solid")) +
        scale_color_manual(values = c("black", "black", "gray")) +
        geom_line() +
        scale_x_continuous(breaks = c(seq(1996, 2002, by = 3), 2006), limits = c(1995.5, 2006.5)) +
        # scale_y_continuous(breaks = seq(0, .5, by = .1), limits = c(0, .5)) +
        ylab("Temporary employment as % of total emp (25-54)") +
        xlab("Period") + 
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom",
              axis.title.y = element_text(size = 9),
              axis.text.x = element_text(size=7),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

## Draw the plot
grid.newpage()
grid.draw(OverlappingStripLabels(p))

cairo_pdf(paste0(graphs,"graph_ftc_rate_region_country_group_period_1.pdf"), height = 8, width = 6)
# png(paste0(graphs,"graph_ftc_rate_region_country.png"), height = 4, width = 6, units = "in", res=600)
grid.draw(OverlappingStripLabels(p))
dev.off()

detach("package:plyr", unload = TRUE)
