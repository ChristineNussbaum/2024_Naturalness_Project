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


#HELP: https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html
library(ggwordcloud)


#load data
L <- read_excel("C:/Users/Christine Nussbaum/Documents/Arbeit/Forschungsprojekte/2024_Naturalness_Project/Literature_overview.xlsx", sheet= "Tabelle1")


#include only the selected literatur
D <- L %>% filter(`Include in Tics-MiniReview?` == "yes")
rm(L)


synonyms <- D$Synonyms 
synonyms <- str_split(synonyms, ", ")
wordlist_long <- unlist(synonyms)

#remove all spaces
wordlist_long <- gsub("^ ", "", wordlist_long, fixed = TRUE)


#fix a few options to reduce the number of words a bit
wordlist_long <- ifelse(wordlist_long == "artificiality", "artificial", wordlist_long)
wordlist_long <- ifelse(wordlist_long == "bizarre", "bizarreness", wordlist_long)
wordlist_long <- ifelse(wordlist_long == "monotony", "monotonous", wordlist_long)
wordlist_long <- ifelse(wordlist_long == "normalcy", "normal", wordlist_long)
wordlist_long <- ifelse(wordlist_long == "robotic", "roboticness", wordlist_long)


#check number of words
wordlist_unique <- as.data.frame(unique(wordlist_long))
wordlist_unique


wordlist_n <- data.frame(table(wordlist_long)) 
names(wordlist_n) <- c("Word", "N")
wordlist_n <-wordlist_n %>% filter(Word != "-")

#fix the N-ratio a bit
wordlist_n$N2 <- ifelse(wordlist_n$N >= 2, sqrt(wordlist_n$N), wordlist_n$N)


#--------------------------------------------------------------------------------------
# creating the word clouds with ggwordcloud

set.seed(43)

filename = paste0("plots/wordcloud_synonyms.png")

p <- ggplot(wordlist_n, aes(label = Word, size= N2, color = N2)) +
  geom_text_wordcloud_area() + #shape = "cardioid"
  scale_radius(range = c(0, 50), limits = c(0, NA)) +
  theme_bw()

ggsave(filename, width = 5, height = 5, dpi =300)




#--------------------------------------------------------------------------------------
# 2.  creating the word cloud from ChatGPT

#load data)

D2 <- read_excel("Word_Cloud_input.xlsx", sheet= "WordCloudChatGPT")


set.seed(42)

filename = paste0("plots/wordcloud_synonyms_ChatGPT.png")

p <- ggplot(D2, aes(label = synonyms, size= N^3, color = N^3)) +
  geom_text_wordcloud_area() + #shape = "cardioid"
  scale_radius(range = c(0, 45), limits = c(0, NA)) +
  theme_bw()

ggsave(filename, width = 5, height = 5, dpi =300)
