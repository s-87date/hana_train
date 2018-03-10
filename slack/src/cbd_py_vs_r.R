library(tidyverse)
library(plotly)

setwd("/Users/hanadate/work/slack/")
list.files("dat/")
list.files("dat/CbD Japan Slack export Feb 28 2018/general/")

msg <- jsonlite::read_json("dat/CbD Japan Slack export Feb 28 2018/general/2018-02-27.json")
msg[[1]]$reactions

msg.df <- jsonlite::fromJSON("dat/CbD Japan Slack export Feb 28 2018/general/2018-02-27.json")
glimpse(msg.df)
View(msg.df)

member <- readr::read_csv("dat/slack-cbdjapan-members.csv")
glimpse(member)

r_py.df <- msg.df[1,]$reactions %>% as.data.frame()

r.users <- r_py.df %>% 
  dplyr::filter(name=="r_language") %>% 
  .$users %>% 
  .[[1]]
r.users.df <- data.frame(userid=r.users) %>% 
  dplyr::mutate(lang="R") %>% 
  dplyr::mutate_if(is.factor, as.character)

py.users <- r_py.df %>% 
  dplyr::filter(name=="python") %>% 
  .$users %>% 
  .[[1]]
py.users.df <- data.frame(userid=py.users) %>% 
  mutate(lang="Python") %>% 
  dplyr::mutate_if(is.factor, as.character)

r_py.fullname <- 
  dplyr::bind_rows(r.users.df, py.users.df) %>% 
  dplyr::inner_join(., member, by=c("userid")) %>% 
  dplyr::group_by(userid) %>% 
  dplyr::mutate(CNT=n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(lang=if_else(CNT==2, "PyR", lang)) %>% 
  dplyr::select(fullname, email, lang) %>% 
  dplyr::distinct()
  
glimpse(r_py.fullname)
# readr::write_csv(output/r_py.fullname, "r_py_cbder.csv")

r_py.fullname4pie <- r_py.fullname %>% 
  dplyr::group_by(lang) %>% 
  dplyr::summarise(count=n()) %>% 
  plot_ly(labels = ~lang, values = ~count, type="pie",textinfo = 'label+percent',insidetextfont = list(color = '#FFFFFF')) %>%
  layout(title = "R vs Python at CbD",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
r_py.fullname4pie


