# ---
# title: "PubMedNetwork.R"
# author: "Naoya Kishimoto"
# date: "1/8/2018"
# ---
#-----------------------------------------
library(tidyverse)
library(wordcloud)
library(tm)
library(RISmed)
library(visNetwork)

setwd("~/Documents/Rscript/Network/")

# search_topic <- 'Shinya Yamanaka'
search_topic <- 'Yoshinori Yoshida'
# search_topic <- 'Masanori Abe'
search_mindate <- 2012
search_maxdate <- 2017
network_size <- 5

# ##########################
# Co-Author Network
# ##########################

search_query <- EUtilsSummary(search_topic, retmax=100, mindate=search_mindate, maxdate=search_maxdate)
summary(search_query)

#see the ids of our returned query
QueryId(search_query)
records<- EUtilsGet(search_query)
#There are many additional tags that can be called: Year, Month, Day, Author, ISSN, Language, PublicationStatus, ArticleId, CopyrightInformation, Country, GrantID.
pubmed_data <- c()
pubmed_data <- data.frame('Title'=ArticleTitle(records),'Abstract'=AbstractText(records))
wordcloud(pubmed_data$Title,
          max.words=500,
          min.freq = 15,colors = "red")

#Function to count co-authorship
CoAuthorCount <- function (search_author,search_query,top_n=network_size){
  
  Authors<-Author(EUtilsGet(search_query))
  AuthorsFullNames <-data_frame(FullName=character())
  for (f in 1:length(Authors)){
    Authors[f][[1]] <- Authors[f][[1]] %>% 
      dplyr::mutate(FullName=paste(ForeName, LastName))
    AuthorsFullNames <- bind_rows(AuthorsFullNames, 
                                  data.frame(FullName=Authors[f][[1]]$FullName))
  }
  print(AuthorsFullNames)
  AuthorsFullNames <- AuthorsFullNames %>%
    dplyr::count(FullName) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::filter(FullName!=search_author) %>%
    dplyr::slice(1:top_n)
  return(AuthorsFullNames)
}

#Co-author list to given main author
AuthorsFullNames_main <- CoAuthorCount(search_topic,search_query)
edges_and_nodes <- AuthorsFullNames_main %>%
  dplyr::slice(1:network_size) %>%
  dplyr::mutate(from=search_topic) %>%
  dplyr::select(from, everything()) %>%
  dplyr::rename(to=FullName, value=n)

#Co-author list to key co-authors of given main author
for (i in c(1:network_size)){
  if(i==1){AuthorsFullNames_sub<-list()}
  search_topic_tmp = AuthorsFullNames_main$FullName[i]
  search_query_tmp <- EUtilsSummary(search_topic_tmp, retmax=100, mindate=search_mindate, maxdate=search_maxdate)
  AuthorsFullNames_sub[[i]] <- CoAuthorCount(search_topic_tmp,search_query_tmp)
  edges_and_nodes <- AuthorsFullNames_sub[[i]] %>%
    dplyr::slice(1:network_size) %>%
    dplyr::mutate(from=search_topic_tmp) %>%
    dplyr::select(from, everything()) %>%
    dplyr::rename(to=FullName, value=n) %>%
    dplyr::bind_rows(edges_and_nodes) %>%
    dplyr::filter(to!=search_topic)
  
}

#Create Nodes File
nodes<- edges_and_nodes %>%
  dplyr::distinct(to) %>%
  dplyr::bind_rows(., data_frame(to=search_topic)) %>%
  dplyr::mutate(id=1:nrow(distinct(.,to))) %>%
  # dplyr::mutate_at(c("id"), as.numeric) %>%
  dplyr::mutate(group= ifelse(to==search_topic,"main", "sub")) %>%
  dplyr::rename(label=to)

#Create Edges File by Replacing name strings to name IDs
for (i in c(1:nrow(nodes))) {
  d <- edges_and_nodes
  if (i!=1) { d <- edges  }
  edges<- d %>%
    dplyr::mutate(from = replace(from, which(from==nodes$label[i]),nodes$id[i])) %>%
    dplyr::mutate(to = replace(to, which(to==nodes$label[i]),nodes$id[i])) 
}
edges <-  edges %>% 
  dplyr::mutate_at(c(1:ncol(edges)), as.numeric) 

#Add network weight to each author
nodes <- edges %>%
  dplyr::group_by(to) %>%
  dplyr::summarise(value=sum(value)) %>%
  dplyr::rename(id=to) %>%
  dplyr::left_join(nodes, ., by="id")

#Visualization
visNetwork(nodes, edges) %>%
  visLayout(randomSeed = "1") %>%
  visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE),
             collapse = list(enabled = TRUE, clusterOptions = list(shape = "square")),
             nodesIdSelection = list(enabled = FALSE,
                                     selected = nodes[which(nodes$group=="main"), "id"] %>%
                                       as.numeric),
             clickToUse = FALSE
             ) 
  # visGroups(groupname = "main", shape = "icon", icon = list(code = "f0f0", color = "green")) %>%
  # addFontAwesome()

#---------------

# ###################
# # visNetwork Appendix
# ###################
# nodes <- data.frame(id = 1:15, label = paste("Label", 1:15),
#                     group = sample(LETTERS[1:3], 15, replace = TRUE))
# 
# edges <- data.frame(from = trunc(runif(15)*(15-1))+1,
#                     to = trunc(runif(15)*(15-1))+1)
# 
# # ###################
# # # highlight nearest
# # ###################
# 
# visNetwork(nodes, edges) %>% visOptions(highlightNearest = TRUE)
# visNetwork(nodes, edges) %>% visOptions(highlightNearest = list(enabled = TRUE, degree = 2))
# 
# # also when hover a node ?
# visNetwork(nodes, edges) %>% visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE))
# 
# # don't show nodes/edges
# visNetwork(nodes, edges) %>% visOptions(highlightNearest = list(enabled = TRUE,
#                                                                 hover = TRUE, hideColor = 'rgba(200,200,200,0)'))
# 
# # Using hierarchical information
# nodes = data.frame(id = 1:6, level = c(1, 2, 3, 3, 4, 2))
# edges = data.frame(from = c(1, 2, 2, 4, 6), to = c(2, 3, 4, 5, 4))
# 
# visNetwork(nodes, edges) %>% visHierarchicalLayout() %>% visEdges(arrows = "to") %>%
#   visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical"))
# 
# visNetwork(nodes, edges) %>% visHierarchicalLayout() %>% visEdges(arrows = "to") %>%
#   visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical",
#                                      degree = list(from = 0, to = 2)))
# 
# # ##########################
# # # nodesIdSelection
# # ##########################
# 
# visNetwork(nodes, edges) %>%
#   visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
# 
# # add a default selected node ?
# visNetwork(nodes, edges) %>%
#   visOptions(highlightNearest = TRUE,
#              nodesIdSelection = list(enabled = TRUE, selected = "1"))
# 
# # subset on id values & don't use labels ?
# visNetwork(nodes, edges) %>%
#   visOptions(highlightNearest = TRUE,
#              nodesIdSelection = list(enabled = TRUE,
#                                      selected = "2", values = c(2:10), useLabels = FALSE))
# 
# # some style
# visNetwork(nodes, edges) %>%
#   visOptions(highlightNearest = TRUE,
#              nodesIdSelection = list(enabled = TRUE, style = 'width: 200px; height: 26px;
#                                      background: #f8f8f8;
#                                      color: darkblue;
#                                      border:none;
#                                      outline:none;'))
#
# ##########################
# # collapse
# ##########################
#
# nodes <- data.frame(id = 1:15, label = paste("Label", 1:15),
#                     group = sample(LETTERS[1:3], 15, replace = TRUE))
#
# edges <- data.frame(from = trunc(runif(15)*(15-1))+1,
#                     to = trunc(runif(15)*(15-1))+1)
#
# # keeping all parent node attributes
# visNetwork(nodes, edges) %>% visEdges(arrows = "to") %>%
#   visOptions(collapse = TRUE)
#
# # setting some properties
# visNetwork(nodes, edges) %>% visEdges(arrows = "to") %>%
#   visOptions(collapse = list(enabled = TRUE, clusterOptions = list(shape = "square")))
#
# # enable / disable open cluster (proxy only) :
# # visEvents(type = "off", doubleClick = "networkOpenCluster")
# # visEvents(type = "on", doubleClick = "networkOpenCluster")
#
# ##########################
# # selectedBy
# ##########################
# nodes <- data.frame(id = 1:15, label = paste("Label", 1:15),
#                     group = sample(LETTERS[1:3], 15, replace = TRUE))
#
# edges <- data.frame(from = trunc(runif(15)*(15-1))+1,
#                     to = trunc(runif(15)*(15-1))+1)
#
# visNetwork(nodes, edges) %>%
#   visOptions(selectedBy = "group")
#
# # add a default value ?
# visNetwork(nodes, edges) %>%
#   visOptions(selectedBy = list(variable = "group", selected = "A"))
#
# # subset on values ?
# visNetwork(nodes, edges) %>%
#   visOptions(selectedBy = list(variable = "group",
#                                selected = "C",
#                                values = c("A", "C")))
#
# # add some style
# visNetwork(nodes, edges) %>%
#   visOptions(selectedBy = list(variable = "group", style = 'width: 200px; height: 26px;
#                                background: #f8f8f8;
#                                color: darkblue;
#                                border:none;
#                                outline:none;'))
#
# # can also be on new column
# nodes$sample <- sample(c("sample 1", "sample 2"), nrow(nodes), replace = TRUE)
# visNetwork(nodes, edges) %>%
#   visOptions(selectedBy = "sample")
#
# # and with multiple groups ?
# nodes$group <- sample(c("group 1", "group 2", "group 1, group 2, group 3"),
#                       nrow(nodes), replace = TRUE)
#
# visNetwork(nodes, edges) %>%
#   visOptions(selectedBy = list(variable = "group", multiple = TRUE))
#
# ##########################
# # collapse
# ##########################
# visNetwork(nodes, edges) %>%
#   visEdges(arrows = "to") %>%
#   visOptions(collapse = list(enabled = TRUE,
#                              clusterOptions = list(shape = "square")))

