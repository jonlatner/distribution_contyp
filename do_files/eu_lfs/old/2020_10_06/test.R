
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
