# Graph percent FTC -----------------------------------------

df_graph_countries <- df_eu_lfs_1 %>%
        group_by(region, country_name, year) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Countries")

df_graph_regions <- df_eu_lfs_1 %>%
        group_by(region, year) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(level = "Region", 
               country_name = region)

df_graph_eu <- df_eu_lfs_1 %>%
        group_by(year) %>%
        summarise(avg = wtd.mean(temp,weight)) %>%
        ungroup() %>%
        mutate(country_name = "Europe", region = "Europe", level = "Region")

df_graph <- rbind(df_graph_countries,df_graph_regions,df_graph_eu)
rm(df_graph_countries,df_graph_regions,df_graph_eu)

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
                  size = 2, 
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

df_graph_period_change_country <- df_graph %>%
        filter(level == "Countries") %>%
        group_by(country_name, period) %>%
        summarise(pct_change = last(avg)/first(avg)-1) %>%
        ungroup() %>%
        mutate(label = "Percentage change",
               direction = ifelse(pct_change > .10, yes = "+1", 
                                  ifelse(pct_change < -.10, yes = "-1", no = "0")))

df_match <- df_graph %>%
        select(region,country_name)
df_match <- unique(df_match)

df_graph_period_change_country <- merge(df_graph_period_change_country,df_match) %>%
        arrange(region)
