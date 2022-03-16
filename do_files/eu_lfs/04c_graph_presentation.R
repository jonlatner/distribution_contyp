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
library(Hmisc) #wtd.mean
library(car) # recode
library(forcats) # fct_relevel

## The function to get overlapping strip labels
'%!in%' <- function(x,y)!('%in%'(x,y))

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

df_summary <- readRDS(file = paste0(data_files, "df_summary.rds"))
df_summary_groups <- readRDS(file = paste0(data_files, "df_summary_groups.rds"))

# Graph percent by country/region and period and demographic group -----------------------------------------

df_graph_1 <- df_summary_groups %>%
        filter(year <= 2007) %>%
        mutate(period = 1,
               year = year - 1995)

df_graph_2 <- df_summary_groups %>%
        filter(year >= 2007) %>%
        mutate(period = 2,
               year = year - 2006)

df_graph <- rbind(df_graph_1,df_graph_2)
rm(df_graph_1,df_graph_2)

df_graph$period <- factor(df_graph$period,
                          labels = c("1996 - 2007", "2007 - 2018"))

df_graph$region <- fct_relevel(df_graph$region, "Europe", after = 0) # forcats
df_graph_region <- df_graph %>%
        filter(level != "Countries")

df_graph_period_change <- df_graph_region %>%
        select(-country_name) %>%
        group_by(period,region,variable) %>%
        summarise(pct_change = last(avg)/first(avg)-1) %>%
        ungroup() 

df_graph_period_change$period <- factor(df_graph_period_change$period,
                                        labels = c("1996 -\n2007", "2007 -\n2018"))
df_graph_period_change$label_region <- "Region"

ggplot(data=df_graph_period_change, aes(x=period, y=pct_change)) +
        facet_grid(variable ~ region) +
        scale_y_continuous(breaks = seq(-1,5,1), limits = c(-1,5.5)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=sprintf(pct_change, fmt = '%#.3f')), 
                  vjust=ifelse(df_graph_period_change$pct_change>0, yes = -.5, no = 1.1), 
                  size=3)  +
        theme(
                panel.grid.minor = element_blank(),
              # panel.background = element_rect(fill = "white", colour = NA),
              # panel.grid.major = element_line(colour = "grey90", size = 0.2),
              legend.title=element_blank(),
              axis.text.x = element_text(size = 7),
              axis.title=element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(filename = paste0(graphs,"graph_pct_change_region_subgroup_period.pdf"), height = 8, width = 10)

# Graph percent FTC -----------------------------------------

df_graph <- df_summary

df_graph$level <- factor(df_graph$level,
                         levels = c("Region", "Countries"))

df_graph$country_name <- fct_relevel(df_graph$country_name, "Southern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Nordic", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Eastern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Continental", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Anglophone", after = Inf) # forcats
df_graph$region <- fct_relevel(df_graph$region, "Europe", after = 0) # forcats

df_graph_1 <- df_graph %>%
        filter(year <= 2007) %>%
        mutate(period = 1,
               year = year - 1995)

df_graph_2 <- df_graph %>%
        filter(year >= 2007) %>%
        mutate(period = 2,
               year = year - 2006)

df_graph <- rbind(df_graph_1,df_graph_2)
rm(df_graph_1,df_graph_2)

df_graph$period <- factor(df_graph$period,
                          labels = c("1996 - 2007", "2007 - 2018"))

df_graph_region <- df_graph %>%
        filter(level != "Countries") %>%
        mutate(levels=level)

df_graph <- df_graph %>%
        mutate(levels = ifelse(country_name == "Italy", yes = "Italy",
                               ifelse(country_name == "Poland", yes = "Poland",
                                      ifelse(country_name == "Netherlands", yes = "Netherlands",
                                             ifelse(country_name == "France", yes = "France", 
                                                    ifelse(country_name == "Portugal", yes = "Portugal", 
                                                           ifelse(country_name == "Spain", yes = "Spain", 
                                                                  ifelse(level == "Countries", yes = "Countries",
                                                                         ifelse(level == "Region", yes = "Region",
                                                                                no = NA)))))))))

df_graph$levels <- fct_relevel(df_graph$levels, "Countries", after = 0) # forcats
df_graph$levels <- fct_relevel(df_graph$levels, "Region", after = 0) # forcats

table(df_graph$levels)

ggplot(df_graph, aes(x = year, y = avg, group = country_name, color = levels, size = levels)) +
        facet_grid(period ~ region) +
        scale_size_manual(values = c(1,.25,.5,.5,.5,.5,.5,.5)) +
        scale_color_manual(values = c("black", "gray", "blue", "darkgreen", "orange", "red", "purple", "gold")) +
        geom_line() +
        scale_x_continuous(breaks = c(1,6,12), limits = c(-.5,13.5)) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.title=element_blank(),
              axis.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom",
              axis.text.x = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        ) +
        geom_text(data = df_graph_region,
                  show.legend = FALSE,
                  size =3, 
                  aes(x = year, y = ifelse(year %in% c(1,6,12), yes = avg, no = NA),
                      vjust=-2,
                      label=sprintf(avg, fmt = '%#.3f')))

## Save the plot
ggsave(filename = paste0(graphs,"graph_rate_region_presentation.pdf"), height = 3, width = 6, units = "in")

df_graph_period_change <- df_graph_region %>%
        select(-country_name) %>%
        group_by(period,region) %>%
        summarise(pct_change = last(avg)/first(avg)-1) %>%
        ungroup() %>%
        mutate(label = "Percentage change")

df_graph_period_change$period <- factor(df_graph_period_change$period,
                                        labels = c("1996 -\n2007", "2007 -\n2018"))

ggplot(data=df_graph_period_change, aes(x=period, y=pct_change)) +
        facet_grid(label ~ region) +
        scale_y_continuous(breaks = seq(-.5,2.5,.5), limits = c(-.5,2.5)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=sprintf(pct_change, fmt = '%#.3f')), 
                  vjust=ifelse(df_graph_period_change$pct_change>0, yes = -1, no = 1.1), 
                  size=2.5)+
        theme_bw() +
        theme(panel.grid.minor = element_blank(), 
              legend.title=element_blank(),
              axis.text.x = element_text(size = 7),
              axis.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom",
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(filename = paste0(graphs,"graph_pct_change_region_presentation.pdf"), height = 2, width = 6, units = "in")
