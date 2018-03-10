setwd("/Users/hanadate/work/slack")
list.files("dat/")

library(readr)
library(dplyr)
library(tidyr)
library(plotly)

p <- plotly(username="ShuheiHanadate", key="ufi8CXWK9Dmk71crAWYH")

dat <- readr::read_csv("dat/CbD_Japan_Slack_20171130.csv") %>% 
  dplyr::rename(NAME=`Name`, WHAT_I_DO=`What I Do`,
                ACCOUNT_TYPE=`Account Type`, CHATS_SENT=`chats_sent`,
                DAYS_ACTIVE=`Days Active - Last 30 Days`) %>% 
  dplyr::mutate(WHAT_I_DO_NCHAR=nchar(WHAT_I_DO)) %>% 
  tidyr::replace_na(replace=list(WHAT_I_DO_NCHAR=0))
  
glimpse(dat)

dat.selected <- dat %>% 
  dplyr::select(NAME, CHATS_SENT, DAYS_ACTIVE, ACCOUNT_TYPE) %>% 
  dplyr::mutate(ACCOUNT_TYPE=as.factor(ACCOUNT_TYPE))

unique(dat.selected$ACCOUNT_TYPE)

pal <- c("orange", "gray", "cyan")
p <- plot_ly(data = dat.selected, x = ~DAYS_ACTIVE, y = ~CHATS_SENT, 
            color = ~ACCOUNT_TYPE, colors = pal, text = ~NAME)
p
