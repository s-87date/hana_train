library(rpart)
library(partykit)
library(dplyr)
library(tidyr)
library(stringr)

tmp <- data.frame(Titanic)
df <- data.frame(
  Class = rep(tmp$Class, tmp$Freq),
  Sex = rep(tmp$Sex, tmp$Freq),
  Age = rep(tmp$Age, tmp$Freq),
  Survived = rep(tmp$Survived, tmp$Freq)
)
ct <- rpart(Survived ~ Class + Sex + Age, data = df, method = "class")
ct.party <- as.party(ct)

cond <- partykit:::.list.rules.party(ct.party, i = nodeids(ct.party))
cond.df <- data.frame(INDEX=1:length(cond), CONDITIONS=cond)

cond.df.split <- cond.df %>%
  tidyr::separate(col=CONDITIONS, 
                  into=paste("CONDITIONS",as.character(c(1:(1+max(str_count(.$CONDITIONS,(" & ")))))),sep=""), 
                  sep=(" & "), remove=TRUE, extra='merge', fill='right')%>% 
  tidyr::gather(key.con, CONDITIONS, -INDEX) %>% 
  dplyr::filter(CONDITIONS != "NA") %>% 
  dplyr::arrange(INDEX) %>% 
  tidyr::separate(col=CONDITIONS, 
                  into=c("CONDITIONS", "SIGN", "VALUE"),
                  sep=(" "), remove=TRUE, extra='merge', fill='right') %>% 
  dplyr::mutate(rVALUE= ifelse(test= SIGN=="<" | SIGN=="<=",
                               yes= VALUE,
                               no= Inf)) %>% 
  dplyr::mutate(lVALUE= ifelse(test= SIGN==">" | SIGN==">=" | SIGN=="%in%",
                               yes= VALUE,
                               no= -Inf)) %>% 
  dplyr::group_by(INDEX, CONDITIONS) %>% 
  dplyr::summarise(rVALUE_min=min(rVALUE), lVALUE_max=max(lVALUE)) %>% 
  dplyr::ungroup(.) %>% 
  dplyr::mutate(VALUE= str_c(lVALUE_max, rVALUE_min, sep="~")) %>% 
  dplyr::mutate(VALUE= str_replace_all(VALUE, c("-Inf"="","Inf"="","c[(]"="","[)]~"=""))) %>% 
  dplyr::select(INDEX, CONDITIONS, VALUE)
