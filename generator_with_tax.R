
# Other than parameters change, should commit changes via the link below, 
# there is a pencil icon, clikc it, modify it or paste your new code from Rstudio, and click commit, it will automatically 
# cross compare the changes so that we do not need to upload it to google drive repeatedly
# https://github.com/SherwinZ/Practicum/blob/master/generator_with_tax.R
rm(list=ls())
###########################

library(tidyverse)
library(lubridate)
# library(plyr)
library(timeDate)

setwd("C:/WFU/Cash_Forecasting_By_Wake/data/final")

df<- read_csv("MFC_Cashbox_Denomination_Totals2014.csv")

#reformat the EFFDATE into date

df$EFFDATE<-mdy(df$EFFDATE)

df <- df %>%
  filter(BRANCH_NUMBER == "925DB2345B1534FC5428002D64BFE93DCDCB703285B1A871B6277B4D08BCD2B4") %>%
  select(-BRANCH_NAME, -CASHBOXNBR, -CASHBOXDESC) %>%
  group_by(BRANCH_NUMBER, EFFDATE) %>%
  summarise_all(sum) %>%
  ungroup()

df$DATE<-df$EFFDATE
df$year<-year(df$EFFDATE)

#Part 1- Weekend Dummy
#Get the day of week for each obs.Note:No Sunday opening. cccccccccccccc
#Weekend function c
wkds<-function(df, wkds) {
  df$day<-wday(df$EFFDATE)
  ifelse(df$day==7, 1, 0)
}
df$if_Weekends<-wkds(df, df$day)

####################### Part 2-Holiday Dummy ########################
# Easter

###### Record Holidays
# 
easter <-c("2011/04/24","2012/04/08","2013/03/31","2014/04/20")

new_year<-c("2011/01/01","2012/01/01","2013/01/01","2014/01/01")

christmas<-c("2011/12/25","2012/12/25","2013/12/25","2014/12/25")
#Valentines
valentine <-c("2011/02/14","2012/02/14","2013/02/14","2014/02/14")

#Independence Day
indep <-c("2011/07/04","2012/07/04","2013/07/04","2014/07/04")

#Veterns Day
veteran <-c("2011/11/11","2012/12/12","2013/11/11","2014/11/11")

#Columbus
columbus <-c("2011/10/10","2012/10/08","2013/10/14","2014/10/13")

#Memorial Day
memorial <-c("2011/05/30","2012/05/28","2013/05/27","2014/05/26")

#thanksgiving
tks<-c("2011/11/24","2012/11/22","2013/11/28","2014/11/27")

#Tax Season
tax <-c("2011/04/15", "2012/04/15", "2013/04/15", "2014/04/15")

shift <- 0
n_last <- 10


######### Get the date set for every year


######### step #################
df_step <- function(df, holidays, n_last, shift, varname){
  
  start_dates <- as.Date(holidays) + shift
  
  # Initialization
  step_dates<-c()

  for (i in seq_along(start_dates)) {
    step_dates <- c(step_dates, 
                   c(seq(start_dates[i], by = "day", length.out = n_last))
                  )
  }
  
  # mutate the new column
  df[[varname]] <- ifelse(df$DATE %in% step_dates, 1, 0)
  
  # return the result
  return(df)
}

###########################


######### shock #################
df_shock <- function(df, holidays, shift, varname) {
  ##############################################################
  # the 0 shift need to be adjust (advanced or lagged) because #
  # the date is holiday and the branch does not open ###########
  ##############################################################
  return(df_step(df, holidays, 1, shift, varname))
}
##############



################# Ramp #################

df_ramp <- function(df, holidays, n_last, shift, varname){
  
  start_dates <- as.Date(holidays) + shift
  
  # Initialization
  step_dates<-c()
  
  for (i in seq_along(start_dates)) {
    step_dates <- c(step_dates, 
                    c(seq(start_dates[i], by = "day", length.out = n_last))
    )
  }
  # Initialization
  ramp_val <- c()
  for (i in seq_along(start_dates)){
    ramp_val <- c(ramp_val, 1:n_last)
  }
  
  ramp_tbl <- tibble(DATE = as.Date(step_dates, origin = '1970-01-01'), !!quo_name(varname) := ramp_val)
  # print(ramp_tbl)
  
  df <- df %>%
    left_join(ramp_tbl, by = "DATE")
  
  df[[varname]][is.na(df[[varname]])] <- 0
  
  return(df)
}


# easter
df <- df_shock(df,easter,shift,'easter_shock')

df <- df_step(df, easter, n_last, shift, 'easter_step')

df <- df_ramp(df, easter, n_last, shift, 'easter_ramp')

gc()
#new year
df <- df_step(df, new_year, n_last, shift, 'new_year_step')

df <- df_shock(df,new_year,shift,'new_year_shock')

df <- df_ramp(df, new_year, n_last, shift, 'new_year_ramp')

gc()
#Christmas
df <- df_step(df, christmas, n_last, shift, 'christmas_step')

df <- df_shock(df,christmas,shift,'christmas_shock')

df <- df_ramp(df, christmas, n_last, shift, 'christmas_ramp')

gc()
#valentine
df <- df_step(df, valentine, n_last, shift, 'valentine_step')

df <- df_shock(df,valentine,shift,'valentine_shock')

df <- df_ramp(df, valentine, n_last, shift, 'valentine_ramp')

gc()
#Independence
df <- df_step(df, indep, n_last, shift, 'indep_step')

df <- df_shock(df,indep,shift,'indep_shock')

df <- df_ramp(df, indep, n_last, shift, 'indep_ramp')

gc()
#veteran
df <- df_step(df, veteran, n_last, shift, 'veteran_step')

df <- df_shock(df,veteran, shift,'veteran_shock')

df <- df_ramp(df, veteran, n_last, shift, 'veteran_ramp')

gc()
#columbus
df <- df_step(df, columbus, n_last, shift, 'columbus_step')

df <- df_shock(df,columbus,shift,'columbus_shock')

df <- df_ramp(df, columbus, n_last, shift, 'columbus_ramp')

gc()
#memorial
df <- df_step(df, memorial, n_last, shift, 'memorial_step')

df <- df_shock(df,memorial,shift,'memorial_shock')

df <- df_ramp(df, memorial, n_last, shift, 'memorial_ramp')

gc()
#thanksgiving
df <- df_step(df, tks, n_last, shift, 'tks_step')

df <- df_shock(df,tks,shift,'tks_shock')

df <- df_ramp(df, tks, n_last, shift, 'tks_ramp')

gc()
#Tax Season
df <- df_step(df, tax, n_last, shift, 'tax_step')

df <- df_shock(df, tax, shift,'tax_shock')

df <- df_ramp(df, tax, n_last, shift, 'tax_ramp')

## Fed

fed <- read_csv("Fed_Rates.csv", col_types = cols(DATE = col_date(format = "%m/%d/%Y"))) %>%
  rename(monetary_volume = `VOLUME(US$BILLIONS)`, fed_rates = `EFFR(PERCENT)`) %>%
  mutate(m_multiplier = 1/fed_rates)


df <- df %>%
  left_join(fed, by = c('EFFDATE'='DATE')) 


# impute

library(zoo)
df$monetary_volume <- na.locf(df$monetary_volume, fromLast = T)
df$fed_rates <- na.locf(df$fed_rates, fromLast = T)
df$m_multiplier <- na.locf(df$m_multiplier, fromLast = T)




write_csv(df, "final.csv")
