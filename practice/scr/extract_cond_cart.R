library(rpart)
library(partykit)
library(dplyr)
library(tidyr)
library(stringr)

# mtcarsを読込確認
tmp <- data.frame(mtcars)

# シリンダー数,V/S,オートマ・マニュアル,ギア数,キャブ数をファクター化
df <- tmp %>% 
  mutate_each_(funs(as.factor), list("cyl","vs","am","gear","carb"))

# 燃費を目的変数にして回帰木生成
ct <- rpart(mpg ~ ., data = df, method = "anova")
(ct.party <- as.party(ct))

# 条件をベクトルで抜き出し
(cond <- partykit:::.list.rules.party(ct.party, i = nodeids(ct.party)))

# 決定木分岐条件一覧表に整形
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
  dplyr::mutate(VALUE= str_replace_all(VALUE, c("-Inf"="","Inf"="","c[(]"="","[)]~"="","\""=""))) %>% 
  dplyr::select(INDEX, CONDITIONS, VALUE)
(cond.df.split)
