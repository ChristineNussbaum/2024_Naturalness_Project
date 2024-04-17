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


hist(L$Year)


plot(L$Year, L$`Citations Google Scholar`)

L$`Category 1 Name 2022`

#how much has changed over the years
table(L$`Category 1 Quartile 2022`, L$`Category 1 Quartile Year of Publication`)



#--------------------------
#create category plot 
L_category <- L %>% group_by(L$`Category 1 Name Year of Publication`,`Category 1 Quartile Year of Publication`) %>% summarise(N = length(Year))
names(L_category) <- c("Category", "Quartile", "N")


#fix a few categories
L_category$Category <- ifelse(str_detect(L_category$Category, pattern = "Psychology"), "Psychology", L_category$Category)
L_category$Category <- ifelse(str_detect(L_category$Category, pattern = "Computer Science"), "Computer Science", L_category$Category)

#remove missings
L_category <- L_category %>% filter(!is.na(Category))


#now get a list of categories that exist only once
Category_N<- data.frame(table(L_category$Category))
Category_N <- Category_N %>% filter(Freq == 1)
Category_1 <- Category_N$Var1
rm(Category_N)

#fix one entry
L_category$Quartile <- ifelse(L_category$Quartile == "Q4 (2021)", "Q4", L_category$Quartile)


#now recode all the single ones as "other"
L_category$Donut <- ifelse(L_category$Category %in% Category_1, paste0(L_category$Category, " (", L_category$Quartile, ")"), L_category$Quartile)


L_category$Category <- ifelse(L_category$Category %in% Category_1, "Other", L_category$Category)

L_category$Category <- ifelse(L_category$Category == "Audiology & Speech-Language Pathology", "Audiology & \n Speech-Language \n Pathology", L_category$Category)
L_category$Category <- ifelse(L_category$Category == "Computer Science", "Computer \n Science", L_category$Category)
L_category$Category <- ifelse(L_category$Category == "Clinical Neurology", "Clinical \n Neurology", L_category$Category)
L_category$Category <- ifelse(L_category$Category == "Multidisciplinary Sciences", "Multidisciplinary \n Sciences", L_category$Category)


L_category <- L_category %>% filter(!is.na(Quartile))

#working on data again
L_category <- L_category %>% group_by(Category, Donut) %>% summarise(N = sum(N))

L_category$Category <- factor(L_category$Category, levels = c( "Multidisciplinary \n Sciences",
                                                               "Audiology & \n Speech-Language \n Pathology",
                                                               "Acoustics",
                                                               "Computer \n Science",
                                                               "Business",
                                                               "Psychology",
                                                               "Clinical \n Neurology",
                                                               "Other")) 



#create a plot with the number of N repetitions 
L_category2 <- as.data.frame(lapply(L_category, rep, L_category$N))



#actualy plotting
p <- ggPieDonut(L_category2 ,aes(pies=Category,donuts=Donut),
           showRatioDonut = FALSE, 
           showRatioPie = FALSE, 
           labelposition = 3) 


ggsave("fighting_with_plots.png", p, width = 15, height = 10, dpi =300)



# #old stuff - made me g crazy
#  p <- PieDonut(L_category, aes(Category, Donut,   count = N), title = "Category Overview, N = 63",
#                showRatioPie = FALSE, showRatioDonut = FALSE ,
#                r0 = getOption("PieDonut.r0", 0.1),
#                #r1 = getOption("PieDonut.r1", 1),
#                #r2 = getOption("PieDonut.r2", 1.2),
#                #labelpositionThreshold = 0.05,
#                showPieName = FALSE,
#                pieLabelSize = 3)
# 
#  


######################################################################

#lets make a world map



#now we play around with the countries
countries <- L$`Nationality Affiliation 1st Author`

table(countries)
