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
library(cowplot) # plot_grid
library(grid) # y.grob
library(gridExtra) # grid.arrange


# Load data -----------------------------------------

df_eu_lfs_0 <- readRDS(file = paste0(data_files, "df_summary.rds"))

# Graph -----------------------------------------

df_graph <- df_eu_lfs_0
rm(df_graph_countries,df_graph_regions,df_graph_eu)

df_graph$level <- factor(df_graph$level,
                         levels = c("Region", "Countries"))

df_graph$avg <- round((df_graph$avg)*100,1)

df_graph$country_name <- fct_relevel(df_graph$country_name, "Southern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Nordic", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Eastern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Continental", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Anglophone", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Europe", after = Inf) # forcats
df_graph$region <- fct_relevel(df_graph$region, "Europe", after = 0) # forcats

df_graph <- df_graph %>%
        mutate(levels = ifelse(country_name == "Italy", yes = "Italy",
                               ifelse(country_name == "Poland", yes = "Poland",
                                      ifelse(country_name == "Netherlands", yes = "Netherlands",
                                             # ifelse(country_name == "France", yes = "France", 
                                                    # ifelse(country_name == "Portugal", yes = "Portugal", 
                                                           ifelse(country_name == "Spain", yes = "Spain", 
                                                                  ifelse(level == "Countries", yes = "Countries",
                                                                         ifelse(level == "Region", yes = "Region",
                                                                                no = NA)))))))

df_graph$levels <- fct_relevel(df_graph$levels, "Countries", after = 0) # forcats
df_graph$levels <- fct_relevel(df_graph$levels, "Region", after = 0) # forcats

df_graph_1 <- df_graph %>%
        filter(year <= 2007) %>%
        mutate(period = 1)

df_graph_2 <- df_graph %>%
        filter(year >= 2007) %>%
        mutate(period = 2)

df_graph_region_1 <- df_graph_1 %>%
        filter(level != "Countries")

df_graph_region_2 <- df_graph_2 %>%
        filter(level != "Countries")

p1 <- ggplot(df_graph_1, aes(x = year, y = avg, group = country_name, color = levels, size = levels, shape = levels)) +
        facet_wrap( ~ region, nrow=1) +
        scale_size_manual(values = c(1,.25,.5,.5,.5,.5,.5,.5)) +
        scale_color_manual(values = c("black", "gray", "black", "black", "black", "black", "black", "black")) +
        scale_shape_manual(values = c(32,32,1,16,2,17,0,15)) +
        geom_line() +
        geom_point(size=2) +
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
        ) +
        geom_text(data = df_graph_region_1,
                  show.legend = FALSE,
                  size = 2.5,
                  aes(x = year, y = ifelse(year %in% c(1996,2007), yes = avg, no = NA),
                      vjust=ifelse(region=="Continental", yes = +1.5, no = -.5),
                      label=sprintf(avg, fmt = '%#.1f')))

p2 <- ggplot(df_graph_2, aes(x = year, y = avg, group = country_name, color = levels, size = levels, shape = levels)) +
        facet_wrap( ~ region, nrow = 1) +
        scale_size_manual(values = c(1,.25,.5,.5,.5,.5,.5,.5)) +
        scale_color_manual(values = c("black", "gray", "black", "black", "black", "black", "black", "black")) +
        scale_shape_manual(values = c(32,32,1,16,2,17,0,15)) +
        geom_line() +
        geom_point(size=2) +
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
        ) +
        geom_text(data = df_graph_region_2,
                  show.legend = FALSE,
                  size = 2.5,
                  aes(x = year, y = ifelse(year %in% c(2007,2019), yes = avg, no = NA),
                      vjust=ifelse(region=="Continental", yes = +1.5, no = -.5),
                      label=sprintf(avg, fmt = '%#.1f')))


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

ggsave(p, filename = paste0(graphs,"graph_rate_region.pdf"), height = 4, width = 6, units = "in")
