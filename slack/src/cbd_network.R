setwd("/Users/hanadate/work/slack")
list.files("dat/")

library(tidyverse)
library(igraph)
library(visNetwork)
library(doParallel)


users <- jsonlite::fromJSON("dat/CbD Japan Slack export Feb 28 2018/users.json") %>% 
  select(id, real_name)

dat.dir <- list.files("dat/CbD Japan Slack export Feb 28 2018") %>% 
  .[!str_detect(.,".json")]

# text にthreadでreplyしたらweight
# text に@でmentionしたらweight
# threadの場合は同日中のみ

# i <- 17
slack.from.to.all <- foreach(i=seq(length(dat.dir)), .combine = "rbind") %do% {
  # slack.from.to.all <- foreach(i=seq(35), .combine = "rbind") %do% {
  dir.path <- paste0("dat/CbD Japan Slack export Feb 28 2018/",dat.dir[i],"/")
  json.paths <- list.files(dir.path, full.names = TRUE)
  # slack.json <- lapply(list.files(dir.path, full.names = TRUE), jsonlite::fromJSON)
  # slack.jsons <- do.call(rbind, slack.json)
  # glimpse(slack.jsons)
  # k <- 6
  if(length(json.paths)!=0){
    to.from.user.dir <- foreach(k=seq(length(json.paths)), .combine = "rbind") %do% {
      msg.df <- jsonlite::fromJSON(json.paths[k])
      # msg.df <- jsonlite::fromJSON("dat/CbD Japan Slack export Feb 28 2018/general/2018-02-27.json")
      glimpse(msg.df)
      
      msg.df$user
      msg.df$text
      msg.df$replies
      msg.df$reactions
      
      # text 書いたヒト
      if(!is.null(msg.df$user)){
        text.user <- data.frame(user=msg.df$user, text=msg.df$text)
      }else{
        text.user <- data.frame(user=NA, text=NA)
      }
      
      # thread での reply
      # =====
      if(!is.null(msg.df$replies)){
        replyers <- map(msg.df$replies, ~ .$user)
        # glimpse(text.user)
        text.user.reply <- text.user %>% 
          dplyr::mutate(from_user=replyers)
        # View(text.user.reply)
        user.reply.df.all <- foreach(i=seq(nrow(text.user.reply)), .combine = "rbind") %do% {
          if(!is.null(text.user.reply[i,]$from_user[[1]])){
            user.reply.df <- data.frame(from_user=text.user.reply[i,]$from_user[[1]]) %>% 
              dplyr::mutate(to_user=text.user.reply[i,]$user) %>% 
              dplyr::select(from_user, to_user) %>% 
              # tidyr::drop_na() %>% 
              dplyr::mutate_if(is.factor, as.character)
          }
        }
        # glimpse(user.reply.df.all)
        # View(user.reply.df.all)
      } else {
        user.reply.df.all <- data.frame(from_user=NA, to_user=NA)
      }
      # =====
      
      # @でのmention
      # ====
      user.mention.all <- text.user %>% 
        dplyr::mutate(to_user= str_extract(text, "\\<!.*\\>")) %>% 
        dplyr::mutate(to_user= str_replace_all(to_user, "\\<!|\\>", "")) %>% 
        dplyr::rename(from_user=user) %>% 
        dplyr::select(from_user, to_user) %>% 
        # tidyr::drop_na() %>% 
        dplyr::mutate_if(is.factor, as.character)
      glimpse(user.mention.all)
      
      to.from.user <- dplyr::bind_rows(user.reply.df.all, user.mention.all) %>% 
        dplyr::group_by(from_user, to_user) %>% 
        dplyr::summarise(COUNT=n()) %>% 
        dplyr::ungroup()
      
      glimpse(to.from.user)
      return(to.from.user)
    }
  }
  return(to.from.user.dir)
}

slack.from.to.all.name <- slack.from.to.all %>% 
  dplyr::filter(from_user!=to_user) %>% 
  dplyr::inner_join(., users, by=c("from_user"="id")) %>% 
  dplyr::rename(from_user_real=real_name) %>% 
  dplyr::inner_join(., users, by=c("to_user"="id")) %>% 
  dplyr::rename(to_user_real=real_name) %>% 
  dplyr::select(from_user_real, to_user_real, COUNT) %>% 
  tidyr::drop_na()
  
glimpse(slack.from.to.all.name)  
# View(slack.from.to.all.name)  
# apply(slack.from.to.all.name,2,anyNA)

# graph
# =====
cbd.slack.igraph <- graph.data.frame(slack.from.to.all.name)
cbd.slack.net <- cbd.slack.igraph %>% 
  visIgraph(., idToLabel = TRUE, layout = "layout_nicely",
                           physics = TRUE, smooth = TRUE, type = "full", randomSeed = NULL,
                           layoutMatrix = NULL) %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
cbd.slack.net




