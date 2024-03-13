#############################################################################################
### file: Keyword analysis.R
### author: Christine Nussbaum
### contact: christine.Nussbaum@uni-jena.de
### date: 01.2024

# clear directory
rm(list=ls())

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load required packages
library(tidyverse)
library(wordcloud)
library(wordcloud2)
library(readxl)
library(stringdist)
library(ggsankey)

#--------------------------------------------------------------------------------------
# Keywords

#load data
K <- read.delim(file = "Keywords_61_papers_1occurences.txt", header= TRUE, sep = "\t", row.names = NULL)

K$total.link.strength <- NULL


#--------------------------
#check for similarities 

#create all possible combinationa
Combinations <- expand.grid(K$keyword, K$keyword)

#now calculate similarities
Combinations$sim <- stringsim(as.character(Combinations$Var1), as.character(Combinations$Var2))

#remove the rows with similaritiy = 1 because thats the ones with identical entries
Combinations <- Combinations %>% filter(sim < 1)

#order them by similarity
Combinations <- Combinations %>% arrange(desc(sim))

#keep always every second row, because the are mirrowed
Combinations<- Combinations[seq(1, length(Combinations$sim), 2), ]


#--------------------------
# replace obvious spelling differences

K$keyword <- str_replace(K$keyword, "conversational agents", "conversational agent")
K$keyword <- str_replace(K$keyword, "fundamental-frequency", "fundamental frequency")
K$keyword <- str_replace(K$keyword, "synthesised speech", "synthesized speech")
K$keyword <- str_replace(K$keyword, "voice assistants", "voice assistant")
K$keyword <- str_replace(K$keyword, "synthesized voices", "synthesized voice")
K$keyword <- str_replace(K$keyword, "copresence", "co-presence")
K$keyword <- str_replace(K$keyword, "experiences", "experience")
K$keyword <- str_replace(K$keyword, "^inconsistency", "(in)consistency")
K$keyword <- str_replace(K$keyword, "^consistency", "(in)consistency")
K$keyword <- str_replace(K$keyword, "agent$", "agents")
K$keyword <- str_replace(K$keyword, "models$", "model")
K$keyword <- str_replace(K$keyword, "robots$", "robot")
K$keyword <- str_replace(K$keyword, "sounds$", "sound")
K$keyword <- str_replace(K$keyword, "laryngectomy speech rehabilitation", "laryngectomy rehabilitation")
K$keyword <- str_replace(K$keyword, "laryngectomee rehabilitation", "laryngectomy rehabilitation")
K$keyword <- str_replace(K$keyword, "emotions$", "emotion")
K$keyword <- str_replace(K$keyword, "^vowel formant frequencies", "formant frequencies")
K$keyword <- str_replace(K$keyword, "^embodied conversational agents", "embodied agents")
K$keyword <- str_replace(K$keyword, "tracheoesophageal$", "tracheoesophageal speech")
K$keyword <- str_replace(K$keyword, "treatment efficacy", "treatment")

K <- K %>% group_by(keyword) %>% summarise(occurrences = sum(occurrences))


#this got us so far. Now we fix things further manually


# we create a backup copy for that
K$keywords2 <- K$keyword




#---------------------------------------------------------------------------------------------------------
#1. acoustics 
K$keyword <- ifelse(K$keyword %in% c("acoustic characteristics",
                                             "acoustic cue weighting",
                                             "acoustic differences",
                                             "acoustical measurements",
                                             "judgments-acoustic measures",
                                             "acoustics"), "acoustics", K$keyword)

#2. acceptability 
K$keyword <- ifelse(K$keyword %in% c("acceptability",
                                                     "acceptability ratings",
                                                     "user acceptance",
                                                     "acceptance"), "acceptability", K$keyword)

#3. agent
K$keyword <- ifelse(K$keyword %in% c("agents",
                                              "computer agent",
                                              "agent perception",
                                              "conversational agent",
                                              "agent"), "agent", K$keyword)

# #4. emotion  -> UNSURE about these
# K$keyword <- ifelse(K$keyword %in% c("emotional robot speech" ,
#                                        "emotion transplantation",
#                                        "emotive voices", 
#                                        "emotions",
#                                        "emotion",
#                                        "vocal emotions",
#                                        "vocal affect",
#                                        "negative affect",
#                                        "affective prosody"), "emotion", K$keyword)


#5. fundamental-frequency
K$keyword <- ifelse(K$keyword %in% c("frequency",
                                       "f0",
                                       "fundamental frequency",
                                       "speaking fundamental-frequency",
                                       "vocal pitch",
                                       "frequency-characteristics",
                                     "speaking fundamental frequency",
                                     "pitch"), "fundamental frequency" , K$keyword)

#6. Human-machine-interaction
K$keyword <- ifelse(K$keyword %in% c("human-robot interaction",
                                       "human-computer interaction (hci)",
                                       "human-robot interaction (hri)",
                                       "human-machine communication (hmc)",
                                       "human-agent interaction",
                                       "human-agent interaction (hai)" ), "human-agent interaction"  , K$keyword)

#7. Naturalness
K$keyword <- ifelse(K$keyword %in% c("naturalness perception",
                                       "natural speech",
                                       "naturalness",
                                       "speech naturalness" ), "naturalness"  , K$keyword)

#8. human-likeness
K$keyword <- ifelse(K$keyword %in% c("humanness",
                                       "humanness perception",
                                       "voice human-likeness",
                                       "human-likeness"), "human-likeness" , K$keyword)



#9. quality
K$keyword <- ifelse(K$keyword %in% c("speech quality",
                                       "voice quality",
                                       "voice quality assessment",
                                       "sound quality", 
                                       "quality"), "voice quality" , K$keyword)

#9. virtual
K$keyword <- ifelse(K$keyword %in% c("virtual agents",
                                      "virtual humans",
                                      "virtual reality"), "virtual" , K$keyword)

#10. synthesized voices
K$keyword <- ifelse(K$keyword %in% c("synthesized voice",
                                     "synthesised speech",
                                     "synthesized speech",
                                     "synthetic speech",
                                     "synthesized voices",
                                     "synthetic voice",
                                     "synthesized voices",
                                    "speech synthesis",
                                     "computer-synthesized speech",
                                    "quality speech synthesis"), "synthesized voices" , K$keyword)

#11. perception
K$keyword <- ifelse(K$keyword %in% c("subjective perception",
                                                     "voice perception",
                                                     "speech perception",
                                                     "social perception",
                                                     "perception"), "perception" , K$keyword)

#12. agent
K$keyword <- ifelse(K$keyword %in% c("subjective perception",
                                     "voice perception",
                                     "speech perception",
                                     "social perception",
                                     "mind perception",
                                     "user perception",
                                     "perceptual evaluation",
                                     "perception"), "perception" , K$keyword)

#13. stuttering
K$keyword <- ifelse(K$keyword %in% c("nonstutterers",
                                       "stutterers"), "stuttering" , K$keyword)

#14. adaptation
K$keyword <- ifelse(K$keyword %in% c("auditory adaptation",
                                     "rapid adaptation",
                                     "cascade adaptation",
                                     "sensorimotor adaptation",
                                     "adaptation"), "adaptation" , K$keyword)

#15. autism
K$keyword <- ifelse(K$keyword %in% c("asd",
                                     "autism spectrum disorder",
                                     "autism"), "autism" , K$keyword)

#16. discrimination
K$keyword <- ifelse(K$keyword %in% c("talker discrimination",
                                     "auditory discrimination",
                                     "discrimination"), "discrimination" , K$keyword)

#16. intelligibility
K$keyword <- ifelse(K$keyword %in% c("intelligibility scores",
                                     "intelligibility"), "intelligibility" , K$keyword)


#16. intelligibility
K$keyword <- ifelse(K$keyword %in% c("older-people",
                                     "older-adults"), "older-adults" , K$keyword)


K <- K %>% group_by(keyword) %>% summarise(occurrences = sum(occurrences))


#now we save it to assign categories
write.csv(K, file="keywords_tuned.csv")



#--------------------------------------------------------------------------------------
# creating of the groups was done manually in excel

#https://rpubs.com/oomiwale1/926103

sankey<- read_excel("keywords_tuned.xlsx", sheet= "keywords_tuned")

sankey <- sankey %>% filter(occurrences >1)



#order columns and entries
sankey <- sankey[, c(3,4,5,1,2)]

sankey$First <- factor(sankey$First, levels = c("HPP",  "Acoustic Signals",   "Persons", "Disorders/conditions", "Methods/software",  "Technical terms/other")) 

sankey$Second <- factor(sankey$Second, levels = c("Impression", "Human-Computer-Interaction(HCI)", "Emotion", "Perception", "…", "….",
                                      "Acoustic features", "Voice Synthesis", "....",
                                      "Groups", "Characterstics",
                                      "Treatment", "..", "other2","other" ))

sankey$keyword <- factor(sankey$keyword)

#create a plot with the number of repetitions in each "occurences"


sankey_rep <- as.data.frame(lapply(sankey, rep, sankey$occurrences))

#create data for the sankey plot
df <- sankey_rep %>% make_long(First, Second)


df$node <- factor(df$node, levels = c(rev(levels(sankey$First)), rev(levels(sankey$Second))))

df$next_node <- factor(df$next_node, levels = c(rev(levels(sankey$First)), rev(levels(sankey$Second))))


#sankey$First <- factor(sankey$First, levels = c("HPP", "Acoustic Signals", "Persons", "Disorders/conditions", "Methods/software", "Technical terms/other"))


pl <- ggplot(df, aes(x = x,                        
                     next_x = next_x,                                     
                     node = node,
                     next_node = next_node,        
                     fill = factor(node),
                     label = node))   +                # This Creates a label for each node
               geom_sankey(flow.alpha = 0.5,          #This Creates the transparency of your node 
              node.color = "black",                   # This is your node color        
              show.legend = TRUE) + 
               geom_sankey(flow.alpha = 0.5,          #This Creates the transparency of your node 
                      node.color = "black",          # This is your node color        
                      show.legend = TRUE)  +         # This determines if you want your legend to show
               theme(legend.position = 'none') + 
               geom_sankey_label(size = 3, 
                             color = "black", 
                             fill = "white") + # This specifies the Label format for each node 
               theme_bw() + 
               theme(legend.position = 'none')+
               theme(axis.title = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks = element_blank(),
                 panel.grid = element_blank())

pl


############# do one with a third layer, but for HPP only


sankey_rep$keyword2 <- ifelse(sankey_rep$First == "HPP", as.character(sankey_rep$keyword), NA) 

sankey_rep<- arrange(sankey_rep, First, Second)

#create data for the sankey plot
df <- sankey_rep %>% make_long(First, Second, keyword2) %>% filter(!is.na(node))

df$node <- factor(df$node, levels = c(rev(levels(sankey$First)), rev(levels(sankey$Second)), rev(unique(sankey_rep$keyword2))))

df$next_node <- factor(df$next_node, levels = c(rev(levels(sankey$First)), rev(levels(sankey$Second)), rev(unique(sankey_rep$keyword2))))



pl <- ggplot(df, aes(x = x,                        
                     next_x = next_x,                                     
                     node = node,
                     next_node = next_node,        
                     fill = factor(node),
                     label = node))   +                # This Creates a label for each node
  geom_sankey(flow.alpha = 0.5,          #This Creates the transparency of your node 
              node.color = "black",                   # This is your node color        
              show.legend = TRUE) + 
  geom_sankey(flow.alpha = 0.5,          #This Creates the transparency of your node 
              node.color = "black",          # This is your node color        
              show.legend = TRUE)  +         # This determines if you want your legend to show
  theme(legend.position = 'none') + 
  geom_sankey_label(size = 3, 
                    color = "black", 
                    fill = "white") + # This specifies the Label format for each node 
  theme_bw() + 
  theme(legend.position = 'none')+
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

pl





