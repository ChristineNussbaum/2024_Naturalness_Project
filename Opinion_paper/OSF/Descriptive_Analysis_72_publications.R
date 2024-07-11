#############################################################################################
### file: Descriptive_Analysis_72_publications.R
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


#load data
L <- read_excel("Literature_overview_72_publications.xlsx", sheet= "naturalness_publications")


#----------------------------------------------------------------------------
# extract descriptive information

#year range 
hist(L$Year)

#How many were published in the last 5 years?
L %>% filter(Year >2018) %>% count(N = length(Year), prop = N/72)


#Which kind of publications
table(L$Type)


#which kind of voice material
table(L$`Type of voices (synthetic, human-pathological, human-manipulated, human-healthy, mixture)`)


#which types of data
table(L$`Type of data`)

#definition found in the publication?
table(L$`Definition given?`)

# conceptualization of naturalness (implicit or explicit)
table(L$`Conceptuaization: Human-Likeness; Deviation-based; Both`)

#how many publications included keywords?
sum(table(L$Keywords))

#How many of these had keywords are related to naturalness or any of its synonyms?
sum(table(L$Target_Keywords))

##End of Script