
# Gender -----------------------------------------

df_graph_countries <- df_eu_lfs %>%
        group_by(region, country_name, year, female) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Countries") %>%
        mutate(period = ifelse(year<2007, yes = 0, no = 1),
               year2 = ifelse(period == 0, 
                              yes = year - 1995,
                              no = year - 2006))

df_graph_regions <- df_eu_lfs %>%
        group_by(region, year, female) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country_name = region) %>%
        mutate(period = ifelse(year<2007, yes = 0, no = 1),
               year2 = ifelse(period == 0, 
                              yes = year - 1995,
                              no = year - 2006))


df_graph_eu <- df_eu_lfs %>%
        group_by(year,female) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "EU-LFS", region = "EU-LFS", level = "EU-LFS") %>%
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
                         levels = c("EU-LFS", "Region", "Countries"))

df_graph$period <- factor(df_graph$period,
                          levels = c("0", "1"),
                          labels = c("1996-2006", "2007-2017"))


df_graph$country_name <- fct_relevel(df_graph$country_name, "Southern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Nordic", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Eastern", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Continental", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "Anglophone", after = Inf) # forcats
df_graph$country_name <- fct_relevel(df_graph$country_name, "EU-LFS", after = Inf) # forcats

df_graph$label_region <- "Region"
df_graph$label_period <- "Time period"

library(grid)
library(gtable)
library(plyr)

p <- ggplot(df_graph, aes(x = year2, y = avg, group = country_name, linetype = level, color = level, size = level)) +
        facet_grid(label_period + period + female ~ label_region + region) +
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

p

## Draw the plot
grid.newpage()
grid.draw(OverlappingStripLabels(p))

cairo_pdf(paste0(graphs,"graph_ftc_rate_region_country.pdf"), height = 4, width = 6)
# png(paste0(graphs,"graph_ftc_rate_region_country.png"), height = 4, width = 6, units = "in", res=600)
grid.draw(OverlappingStripLabels(p))
dev.off()

detach("package:plyr", unload = TRUE)
