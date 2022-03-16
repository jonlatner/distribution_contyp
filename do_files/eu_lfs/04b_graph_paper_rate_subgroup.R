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

# FOLDERS - ADAPT THIS PATHWAY
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/distribution_contyp/")
# setwd("/Users/jonathanlatner/Documents/GitHub/distribution_contyp/")

data_files = "data_files/eu_lfs/"
graphs = "graphs/eu_lfs/"

# LIBRARY
library(tidyverse)
library(Hmisc) # wtd.mean
library(forcats) # fct_relevel

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

df_eu_lfs_0 <- readRDS(file = paste0(data_files, "df_summary_groups.rds"))

# Combine graphs -----------------------------------------

df_graph <- df_eu_lfs_0 %>%
        filter(year>=1996 & year<=2007)

df_graph$avg <- round((df_graph$avg)*100,1)

df_graph$level <- factor(df_graph$level,
                         levels = c("Region", "Countries"))

df_graph$country_name <- fct_relevel(df_graph$country_name, "Southern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Nordic", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Eastern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Continental", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Anglophone", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Europe", after = Inf) # forcats
df_graph$region <- fct_relevel(df_graph$region, "Europe", after = 0) # forcats
df_graph$label_region <- "Region"

df_graph_region <- df_graph %>%
        filter(level != "Countries")

df_graph_period_change <- df_graph_region %>%
        # select(country_name,variable,year,avg,) %>%
        arrange(country_name,variable,year) %>%
        group_by(country_name,variable) %>%
        mutate(pct_change = last(avg)/first(avg)-1,
               num_change = last(avg) - first(avg)) %>%
        ungroup() %>%
        filter(pct_change>.1 & num_change>1)

library(grid)
library(gtable)
library(plyr)

p1 <- ggplot(df_graph, aes(x = year, y = avg, group = country_name, color = level, size = level)) +
        facet_grid(factor + variable ~ label_region + region) +
        scale_size_manual(values = c(1,.5)) +
        scale_color_manual(values = c("black", "gray")) +
        geom_line() +
        scale_x_continuous(breaks = c(1996, 2002, 2007), limits = c(1994, 2009)) +
        scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0, 45)) +
        ylab("Temporary employment rate x 100") +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom",
              legend.box.margin=margin(-10,0,0,0),
              axis.title.x = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        ) +
        geom_rect(data = df_graph_period_change,
                  xmin = -Inf,xmax = Inf,
                  ymin = -Inf,ymax = Inf,
                  alpha = 0.01, 
                  inherit.aes=FALSE) +
        geom_text(data = df_graph_region,
                  show.legend = FALSE,
                  size = 3, 
                  aes(x = year, y = ifelse(year %in% c(1996, 2007), yes = avg, no = NA),
                      vjust=-.5,
                      label=sprintf(avg, fmt = '%#.1f')))

## Draw the plot
grid.newpage()
grid.draw(OverlappingStripLabels(p1))

cairo_pdf(paste0(graphs,"graph_ftc_rate_region_country_group_period_1.pdf"), height = 6, width = 9)
grid.draw(OverlappingStripLabels(p1))
dev.off()

detach("package:plyr", unload=TRUE)

rm(df_graph_gender,df_graph_age,df_graph_edu)

# Combine graphs -----------------------------------------

df_graph <- df_eu_lfs_0 %>%
        filter(year>=2007)

df_graph$avg <- round((df_graph$avg)*100,1)

df_graph$level <- factor(df_graph$level,
                         levels = c("Region", "Countries"))

df_graph$country_name <- fct_relevel(df_graph$country_name, "Southern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Nordic", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Eastern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Continental", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Anglophone", after = Inf) # forcats
df_graph$region <- fct_relevel(df_graph$region, "Europe", after = 0) # forcats
df_graph$label_region <- "Region"

df_graph_region <- df_graph %>%
        filter(level != "Countries")

df_graph_period_change <- df_graph_region %>%
        arrange(country_name,variable,year) %>%
        group_by(country_name,variable) %>%
        mutate(pct_change = last(avg)/first(avg)-1,
               num_change = last(avg) - first(avg)) %>%
        ungroup() %>%
        filter(pct_change>.1 & num_change>1)

library(grid)
library(gtable)
library(plyr)

p2 <- ggplot(df_graph, aes(x = year, y = avg, group = country_name, color = level, size = level)) +
        facet_grid(factor + variable ~ label_region + region) +
        scale_size_manual(values = c(1,.5)) +
        scale_color_manual(values = c("black", "gray")) +
        geom_line() +
        scale_x_continuous(breaks = c(2007, 2012, 2019), limits = c(2005, 2021)) +
        scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0, 45)) +
        ylab("Temporary employment rate x 100") +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.box.margin=margin(-10,0,0,0),
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom",
              axis.title.x = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        ) +
        geom_rect(data = df_graph_period_change,
                  xmin = -Inf,xmax = Inf,
                  ymin = -Inf,ymax = Inf,
                  alpha = 0.01, 
                  inherit.aes=FALSE) +
        geom_text(data = df_graph_region,
                  show.legend = FALSE,
                  size = 3, 
                  aes(x = year, y = ifelse(year %in% c(2007,2019), yes = avg, no = NA),
                      vjust=-.5,
                      label=sprintf(avg, fmt = '%#.1f')))

## Draw the plot
grid.newpage()
grid.draw(OverlappingStripLabels(p2))

cairo_pdf(paste0(graphs,"graph_ftc_rate_region_country_group_period_2.pdf"), height = 6, width = 9)
grid.draw(OverlappingStripLabels(p2))
dev.off()

detach("package:plyr", unload=TRUE)

rm(df_graph_gender,df_graph_age,df_graph_edu)
