#############################################################################################
### file: Wordclouds_naturalness.R
### author: Christine Nussbaum
### contact: christine.Nussbaum@uni-jena.de
### date: 01.2024

# clear directory
rm(list=ls())

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load required packages
library(tidyverse)
library(readxl)
library(wordcloud)
library(wordcloud2)

#load data
D <- read_excel("Word_Cloud_input.xlsx", sheet= "Tabelle1")

D <- D[,1:2]


#--------------------------------------------------------------------------------------
# Data preparation

#get table for synonyms: 
syn <- as.data.frame(table(D$Synonyms))
names(syn) <- c("synonyms", "N")
syn <- syn %>% filter(synonyms != "")


#get table for related concepts: 
rel <- as.data.frame(table(D$Relatedconcepts))
names(rel) <- c("related", "N")
rel <- rel %>% filter(related != "")



#seem like we have to tweek the N a little bit 
syn$N2 <- ifelse(syn$N >= 2, sqrt(syn$N), syn$N)

#remove "crippled"
syn <- syn %>% filter(synonyms != "crippled") 



#--------------------------------------------------------------------------------------
# creating the word clouds

#Synonyms
set.seed(42)


# 
# p <- wordcloud(words = syn$synonyms, freq = syn$N2, min.freq = 1,
#           max.words=26,
#           random.order=FALSE, 
#           random.color = TRUE,
#           rot.per=0, scale=c(3.5,0.25),
#           colors=brewer.pal(8, "Dark2"))
# 
# ggsave(filename = "wordcloud_synonyms.png", width = 8, height =8, dpi =300)
# 
# 
# 
# #related concepts
# rel$N2 <- ifelse(rel$N >= 11, 8, rel$N)
# set.seed(1265) # for reproducibility 
# #seem like we have to tweek the N a little bit again
# wordcloud(words = rel$related, freq = rel$N2, min.freq = 1,
#           max.words=601,
#           random.order=FALSE, 
#           #random.color = TRUE,
#           rot.per=0, scale=c(2.5,0.4),
#           colors=brewer.pal(8, "Dark2"))



#--------------------------------------------------------------------------------------
# creating the word clouds with ggwordcloud

#HELP: https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

library(ggwordcloud)

set.seed(42)

filename = paste0("plots/wordcloud_synonyms.png")

p <- ggplot(syn, aes(label = synonyms, size= N2, color = N2)) +
  geom_text_wordcloud_area() + #shape = "cardioid"
  scale_radius(range = c(0, 50), limits = c(0, NA)) +
  theme_bw()

ggsave(filename, width = 5, height = 5, dpi =300)




#--------------------------------------------------------------------------------------
# creating the word cloud from ChatGPT

#load data)

D2 <- read_excel("Word_Cloud_input.xlsx", sheet= "WordCloudChatGPT")


set.seed(42)

filename = paste0("plots/wordcloud_synonyms_ChatGPT.png")

p <- ggplot(D2, aes(label = synonyms, size= N^3, color = N^3)) +
  geom_text_wordcloud_area() + #shape = "cardioid"
  scale_radius(range = c(0, 40), limits = c(0, NA)) +
  theme_bw()

ggsave(filename, width = 5, height = 5, dpi =300)