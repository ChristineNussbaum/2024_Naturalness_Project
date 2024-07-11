#############################################################################################
### file: Wordclouds_naturalness.R
### author: Christine Nussbaum
### contact: christine.Nussbaum@uni-jena.de
### date: 07.2024

# clear directory
rm(list=ls())

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load required packages
library(tidyverse)
library(readxl)
library(ggwordcloud)

#----------------------------------------------------------------------
## [1] Wordcloud with synonym extracted from the scientific literature

#load data from the literature review -> 72 included papers
L <- read_excel("Literature_overview_72_publications.xlsx", sheet= "naturalness_publications")



# extract the synonym
synonyms <- L$Synonyms 
synonyms <- str_split(synonyms, ", ")
wordlist_long <- unlist(synonyms)

#remove all spaces
wordlist_long <- gsub("^ ", "", wordlist_long, fixed = TRUE)


#fix a few redundant options to reduce the number of words a bit
wordlist_long <- ifelse(wordlist_long == "artificiality", "artificial", wordlist_long)
wordlist_long <- ifelse(wordlist_long == "bizarre", "bizarreness", wordlist_long)
wordlist_long <- ifelse(wordlist_long == "monotony", "monotonous", wordlist_long)
wordlist_long <- ifelse(wordlist_long == "normalcy", "normal", wordlist_long)
wordlist_long <- ifelse(wordlist_long == "robotic", "roboticness", wordlist_long)


#convert into a dataframe
wordlist_n <- data.frame(table(wordlist_long)) 
names(wordlist_n) <- c("Word", "N")
wordlist_n <-wordlist_n %>% filter(Word != "-") # there are 40 words now

#fix the N-ratio a bit. Reason: Some words are a lot more frequent then others. 
# If we choose a linear mapping between frequency and size in the wordclouds, it will disrupt the figure
# so we go for a non-linear mapping, but preserving the rank between words
wordlist_n$N2 <- ifelse(wordlist_n$N >= 2, sqrt(wordlist_n$N), wordlist_n$N)


#--------------------------------------------------------------------------------------
# creating the word cloud with ggwordcloud

set.seed(42)

filename = ("wordcloud_synonym_from_literature.png")

p <- ggplot(wordlist_n, aes(label = Word, size= N2, color = N2)) +
  geom_text_wordcloud_area() + #shape = "cardioid"
  scale_radius(range = c(0, 50), limits = c(0, NA)) +
  theme_bw()

ggsave(filename, width = 5, height = 5, dpi =300)




#--------------------------------------------------------------------------------------
# 2.  creating the word cloud from ChatGPT

#load data)

D <- read_excel("ChatGPT_WordCloud_input.xlsx", sheet= "WordCloudChatGPT") #22 words


set.seed(42)

filename = paste0("wordcloud_synonyms_ChatGPT.png")

p <- ggplot(D, aes(label = synonyms, size= N^3, color = N^3)) + # note: N is also adjusted, but ranked order is preserved
  geom_text_wordcloud_area() + #shape = "cardioid"
  scale_radius(range = c(0, 45), limits = c(0, NA)) +
  theme_bw()

ggsave(filename, width = 5, height = 5, dpi =300)

##End of Script