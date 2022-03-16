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
setwd("/Users/jonathanlatner/OneDrive/SECCOPA/projects/distribution_contyp/")
# setwd("/Users/jonathanlatner/Documents/GitHub/distribution_contyp/")

data_files = "data_files/eu_lfs/"
tables = "tables/eu_lfs/"

# LIBRARY
library(tidyverse)
library(xtable)

# Load data ----

df_eu_lfs_0 <- readRDS(file = paste0(data_files, "df_eu_lfs.rds"))

# Clean data ----

names(df_eu_lfs_0) <- tolower(names(df_eu_lfs_0))

df_eu_lfs_1 <- df_eu_lfs_0 %>%
        select(country,year,ilostat,age,temp,sex,hatlev1d,coeff) %>%
        rename(edu_cat = hatlev1d,
               weight = coeff) %>%
        filter(year >= 1996) %>% 
        filter(ilostat==1) %>% # employed
        filter(age >=25 & age <=54)

df_eu_lfs_2 <- df_eu_lfs_1 %>%
        filter(temp == 1 | temp == 2)

df_eu_lfs_3 <- df_eu_lfs_2 %>%
        filter(edu_cat=="L" | edu_cat=="M" | edu_cat=="H")

df_eu_lfs_4 <- df_eu_lfs_3 %>%
        filter(weight>0)

1-nrow(df_eu_lfs_4)/nrow(df_eu_lfs_1)
1-nrow(df_eu_lfs_2)/nrow(df_eu_lfs_1)

# Steps ----

step_0 <- data.frame(with(df_eu_lfs_0, table(country,year)))
step_0 <- filter(step_0, Freq>0) %>%
        arrange(year,country)  %>%
        mutate(total = sum(Freq)) 
step_0$n <- nrow(step_0)
step_0 <- step_0 %>%
        filter(row_number()==1) %>%
        select(n, total) %>%
        mutate(step = 0)

step_1 <- data.frame(with(df_eu_lfs_1, table(country,year)))
step_1 <- filter(step_1, Freq>0) %>%
        arrange(year,country)  %>%
        mutate(total = sum(Freq)) 
step_1$n <- nrow(step_1)
step_1 <- step_1 %>%
        filter(row_number()==1) %>%
        select(n, total) %>%
        mutate(step = 1)


step_2 <- data.frame(with(df_eu_lfs_2, table(country,year)))
step_2 <- filter(step_2, Freq>0) %>%
        arrange(year,country)  %>%
        mutate(total = sum(Freq)) 
step_2$n <- nrow(step_2)
step_2 <- step_2 %>%
        filter(row_number()==1) %>%
        select(n, total) %>%
        mutate(step = 2)

step_3 <- data.frame(with(df_eu_lfs_3, table(country,year)))
step_3 <- filter(step_3, Freq>0) %>%
        arrange(year,country)  %>%
        mutate(total = sum(Freq)) 
step_3$n <- nrow(step_3)
step_3 <- step_3 %>%
        filter(row_number()==1) %>%
        select(n, total) %>%
        mutate(step = 3)

step_4 <- data.frame(with(df_eu_lfs_4, table(country,year)))
step_4 <- filter(step_4, Freq>0) %>%
        arrange(year,country)  %>%
        mutate(total = sum(Freq)) 
step_4$n <- nrow(step_4)
step_4 <- step_4 %>%
        filter(row_number()==1) %>%
        select(n, total) %>%
        mutate(step = 4)

# Table ----

df_table <- rbind(step_0, step_1, step_2, step_3, step_4)

df_table <- df_table %>%
        mutate(diff = paste0(round((total/lag(total,1)-1)*100,0),"\\%"),
               diff = ifelse(row_number()==1, yes = "", no = diff)) %>%
        mutate(notes = ifelse(step == 0, yes = "Raw data",
                              ifelse(step == 1, yes = "Year $>=$ 1996, employed, prime age (25 - 54)",
                                     ifelse(step == 2, yes = "Observable contract type",
                                                   ifelse(step == 3, yes = "Observable education",
                                                          ifelse(step == 4, yes = "Personal weight $>$ 0", no = NA))))))

df_table <- df_table %>%
        select(step,n,total,diff,notes)


# VARIABLE LABLES

columns <- c("[-1.8ex]
\\multicolumn{1}{l}{Step} & 
\\multicolumn{1}{>{\\raggedright\\arraybackslash}p{1in}}{Country, year periods} &
\\multicolumn{1}{l}{Observations} & 
\\multicolumn{1}{l}{\\% $\\Delta$} & 
\\multicolumn{1}{l}{Notes} 
\\\\  \n
"
)

hline_top <- ("\\\\[-1.8ex]\\hline \\\\ \n")
hline_bot <- c("\\hline \n")

t <- xtable(df_table, digits = 0)

align(t) <- c("l", #first
              "l", #step
              ">{\\raggedright\\arraybackslash}p{1in}", #country panel periods
              "l", #n
              "l", #diff.
              ">{\\raggedright\\arraybackslash}p{4in}" # notes
) 

t

print(t, 
      sanitize.colnames.function = identity, 
      file = paste0(tables,"descriptives_tables_steps_lfs.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      format.args = list(big.mark = ".", decimal.mark = ","),
      hline.after = FALSE,
      add.to.row = list(
              pos = list(0,0,5),
              command = c(hline_top,
                          columns,
                          hline_bot)),
      comment = FALSE
)

