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
D <- read.csv(file = "Word_Cloud_input.csv", header= TRUE, sep = ";", row.names = NULL)

D <- D[,1:2]


#--------------------------------------------------------------------------------------
# Data preparation

#get table for synonyms: 
syn <- as.data.frame(table(D$Synonyms))
names(syn) <- c("synonyms", "N")
syn <- syn %>% filter(synonyms != "")


#get table for related concepts: 
rel <- as.data.frame(table(D$Related.concepts))
names(rel) <- c("related", "N")
rel <- rel %>% filter(related != "")



#--------------------------------------------------------------------------------------
# creating the word clouds

#Synonyms


#seem like we have to tweek the N a little bit 
syn$N2 <- ifelse(syn$N >= 8, syn$N/2, syn$N)


wordcloud(words = syn$synonyms, freq = syn$N2, min.freq = 1,
          max.words=26,
          random.order=FALSE, 
          random.color = TRUE,
          rot.per=0, scale=c(3.5,0.25),
          colors=brewer.pal(8, "Dark2"))





#related concepts
rel$N2 <- ifelse(rel$N >= 11, 8, rel$N)
set.seed(1265) # for reproducibility 
#seem like we have to tweek the N a little bit again
wordcloud(words = rel$related, freq = rel$N2, min.freq = 1,
          max.words=601,
          random.order=FALSE, 
          #random.color = TRUE,
          rot.per=0, scale=c(2.5,0.4),
          colors=brewer.pal(8, "Dark2"))


