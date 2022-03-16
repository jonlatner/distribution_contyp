# Top commands----

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


# Load data----

df_filter <- readRDS(file = paste0(data_files,"02_df_eu_silc_filter_steps.rds"))

df_ever <- readRDS(file = paste0(data_files,"03_df_eu_silc_clean_xc.rds"))

df_num <- df_ever %>%
  filter(ftc_num > 0)

df_dur <- readRDS(file = paste0(data_files,"03_df_eu_silc_clean_panel.rds"))
df_dur <- df_dur %>%
  filter(ftc_dur > 0)

# Summarize data ----

df_filter <- df_filter %>%
  group_by(step) %>%
  mutate(periods = last(row_number()),
         total = sum(count)) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  arrange(step) %>%
  select(step,periods,total)

df_filter

# Clean data----

df_filter <- df_filter %>%
  mutate(obs_diff = paste0(round((total/lag(total,1)-1)*100,0),"\\%"),
         obs_diff = ifelse(row_number()==1, yes = "", no = obs_diff),
  )
df_filter

df_filter <- df_filter %>%
        mutate(notes = ifelse(step == 0, yes = "Raw data",
                              ifelse(step == 1, yes = "Country panel filters: Drop 2007 panel period.  Every panel period can only have four years.  Each country, panel, year must have non-missing temporary employment rate $>$ 0",
                                     ifelse(step == 2, yes = "Individual filters: prime age (25 - 54), active labor market participation (employed or unemployed), personal weight $>$ 0",
                                            ifelse(step == 3, yes = "Must be employed at least once",
                                                   ifelse(step == 4, yes = "Case-wise deletion of missing variables on education, gender, age, and contract type",
                                                          ifelse(step == 5, yes = "Individuals in each year of 4 year panel period",
                                                                 ifelse(step == 6, yes = "Must have 4 year longitudinal weight (`Main dataset')", no = NA))))))))


# Table -----------------------------------------

df_table <- df_filter 

df_dataset_a <-data.frame(7,"Dataset \\emph{A}", nrow(df_ever), NA, "Main data:  One observation per individual, panel wave (ever)")
df_dataset_b <-data.frame(8,"Dataset \\emph{B}", nrow(df_num), NA, "Dataset \\emph{A}:$|>= 1$ temporary contract (number)")
df_dataset_c <-data.frame(9,"Dataset \\emph{C}", nrow(df_dur), NA, "Main data:$|>= 1$ temporary contract, one observation per spell, panel wave (duration)")
x <- colnames(df_table)
colnames(df_dataset_a) <- x
colnames(df_dataset_b) <- x
colnames(df_dataset_c) <- x

df_table <- rbind(df_table, df_dataset_a,df_dataset_b,df_dataset_c)

rm(df_filter, df_dataset_a,df_dataset_b,df_dataset_c,df_dur,df_ever,df_num)

df_table

# VARIABLE LABLES

columns <- c("[-1.8ex]
\\multicolumn{1}{l}{Step} & 
\\multicolumn{1}{>{\\raggedright\\arraybackslash}p{1in}}{Country, panel periods} &
\\multicolumn{1}{>{\\raggedright\\arraybackslash}p{1in}}{Unique observations} &
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
      file = paste0(tables,"descriptives_tables_steps_silc_4.tex"),
      include.rownames = FALSE, 
      include.colnames = FALSE,
      sanitize.text.function = identity,
      floating="FALSE",
      format.args = list(big.mark = ".", decimal.mark = ","),
      hline.after = FALSE,
      add.to.row = list(
              pos = list(0,0,7,10,10),
              command = c(hline_top,
                          columns,
                          hline_bot,
                          hline_bot,
                          hline_bot)),
      comment = FALSE
)
