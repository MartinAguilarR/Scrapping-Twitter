rm(list=ls())

library(rtweet)
library(tidytext)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(wordcloud)
library(reshape2)
library(tm)
library(ggplot2)

######################## EN CASO DE HACER SCRAPP EN ESPAÑOL(ES) EJECUTAR ESTE CODIGO ########################
                        ##############                            ##############
                                     # EN SENTIDO CONTRARIO SKIP #


download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")

afinn <- read.csv("C:/Users/marti/OneDrive/Documentos/lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

afinn 

#############################################################################################################


#ABRIENDO API CREADA

api_key <- "XXXXXXXXX"
api_secret_key <- "XXXXXXXXX"
access_token <- "XXXXXXXXX"
access_token_secret <- "XXXXXXXXX"
app_name <- "XXXXXXXXX"

#Guardar token

token <- create_token(
  app = app_name,
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

tweets <- search_tweets(q="#digitaleconomy",  #### SOLO SE CAMBIA ESTA VARIABLE Y SE CORRE EL MODELO ####
                        n= 1000,
                        include_rts = FALSE,
                        lang = "en")

dim(tweets)
names(tweets)

#EXTRACCIÓN DATA

tweets_token <- unnest_tokens(tbl= tweets,
                              output = "word",
                              input = "text",
                              token = "words")

tweets_token <- anti_join(x=tweets_token,
                          y= stop_words,
                          by="word")
count(tweets_token,
      word,
      sort = TRUE)

tweets_token <- anti_join(x=tweets_token,
                          y= stop_words,
                          by="word")

tweets_token <- filter(tweets_token, word!="amp" & word!="https" & word!="t.co" & word!="it's" & word!="don't")

count(tweets_token,
      word,
      sort = TRUE)

#VISUALIZACIÓN INFORMACIÓN ENCONTRADA

tweets_token %>%
  count(word, sort = TRUE) %>%
  filter(n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_text(aes(label=n), hjust= -0.2) +
  geom_col() +
  xlab(NULL) +
  coord_flip()+
  theme_minimal()


####### ANALISIS DE SENTIMIENTOS  #######

get_sentiments("bing")
get_sentiments("nrc")

tweets_bing <- tweets_token %>%
  inner_join(get_sentiments("bing"))

tweets_nrc <- tweets_token %>%
  inner_join(get_sentiments("nrc"))

tweets_bing %>%
  count(word,sentiment,sort=TRUE)

tweets_nrc %>%
  count(word,sentiment,sort=TRUE)



tweets_bing %>%                                   
  count(word,sentiment,sort=TRUE) %>%              
  group_by(sentiment) %>%                          
  top_n(10) %>%     #Probar con intervalos 5; 10 ; 15                                         
  ungroup() %>%                                  
  mutate(word=reorder(word,n)) %>%                  
  ggplot(aes(word,n,fill=sentiment))+                 
  geom_col(show.legend = FALSE)+              
  geom_text(aes(label=n), hjust= 1.2) +       
  facet_wrap(~sentiment,scales = "free_y") +  
  coord_flip() +                              
  xlab(NULL)  

tweets_nrc %>%
  filter(sentiment!="negative" & sentiment!="positive") %>%
  count(word,sentiment,sort=TRUE) %>%             
  group_by(sentiment) %>%                        
  top_n(10) %>%                                                     
  ungroup() %>%                                   
  mutate(word=reorder(word,n)) %>%                
  ggplot(aes(word,n,fill=sentiment))+           
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=n), hjust= 0) +
  facet_wrap(~sentiment,scales = "free_y")+  
  coord_flip() +
  xlab(NULL)

tweets_bing %>%
  filter(sentiment=="negative" | sentiment=="positive") %>%
  count(word,sentiment,sort=TRUE) %>%             
  group_by(sentiment) %>%                        
  top_n(10) %>%                                                    
  ungroup() %>%                                   
  mutate(word=reorder(word,n)) %>%                
  ggplot(aes(word,n,fill=sentiment))+           
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=n), hjust= 0) +
  facet_wrap(~sentiment,scales = "free_y")+  
  coord_flip() +
  xlab(NULL)

tweets_nrc %>%
  filter(sentiment=="negative" | sentiment=="positive") %>%
  count(word,sentiment,sort=TRUE) %>%             
  group_by(sentiment) %>%                        
  top_n(10) %>%                                                    
  ungroup() %>%                                   
  mutate(word=reorder(word,n)) %>%                
  ggplot(aes(word,n,fill=sentiment))+           
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=n), hjust= 0) +
  facet_wrap(~sentiment,scales = "free_y")+  
  coord_flip() +
  xlab(NULL)



######    VISUALIZACIÓN GRÁFICOS    ######


# Palabras positivas por bing

tweets_bing %>%
  count(word,sentiment) %>%
  filter(sentiment=="positive") %>%  
  with(wordcloud(words=word,
                 freq=n,
                 max.words = 250,
                 scale = c(3,1),
                 rot.per = 0.3,
                 random.order = FALSE,
                 colors=brewer.pal(6,"Dark2")))

tweets_bing%>%
  count(word,sentiment) %>%
  filter(sentiment=="negative") %>%  
  with(wordcloud(words=word,
                 freq=n,
                 max.words = 250,
                 scale = c(3,1),
                 rot.per = 0.3,
                 random.order = FALSE,
                 colors=brewer.pal(6,"Dark2")))

# Comparar positivo y negativo

tweets_bing %>%
  count(word,sentiment,sort=TRUE) %>%
  acast(word~sentiment,value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red","green"), 
                   max.words = 300,
                   title.size = 2)


tweets_nrc %>%
  count(word,sentiment,sort=TRUE) %>%
  filter(sentiment!="positive" & sentiment!="negative") %>% 
  acast(word~sentiment,value.var = "n", fill = 0) %>%
  comparison.cloud(title.size = 1.5)
