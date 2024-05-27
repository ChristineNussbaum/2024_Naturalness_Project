#############################################################################################
### file: Analysis_Excel_Literature.R
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
library(webr)
library(ggiraphExtra)

# load relevant functions
source("functions/mySummary.R") 

#--------------------------------------------------------------------------------------
# Keywords

#load data
L <- read_excel("C:/Users/Christine Nussbaum/Documents/Arbeit/Forschungsprojekte/2024_Naturalness_Project/Literature_overview.xlsx", sheet= "Tabelle1")

#include only the selected literatur
D <- L %>% filter(`Include in Tics-MiniReview?` == "yes")
rm(L)


#year range 
table(D$Year)

#sum published after 2015
N_10years <- D %>% filter(Year >2014) %>% count(N = length(Year),
                                               propm = N/66)


#paper type
table(D$Type)
table(D$Type)/66


# voice type 
table(D$`Type of voices (synthetic, human-pathological, human-manipulated, human-healthy, mixture)`)


#type of data
table(D$`Type of data`)

#definition
table(D$`Definition given?  ToDo`)

# conceptualization

table(D$`Concept: Human-Likeness (1) vs. Deviation (2); Both (3) ToDo`)

#N keywords

sum(table(D$Keywords))

#Ntarget keywords

sum(table(D$Target_Keywords))
