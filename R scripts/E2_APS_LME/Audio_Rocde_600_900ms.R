# Peiyun Zhou
# youneverleave@gmail.com
# R scripts --Data Analysis for APS Experiment 2 P600 Window (600-900ms)

library(stringr)
library(ez)
library(car)
library(lme4)
library(lmtest)
library(lmerTest)

setwd("Set working directory")
win600_900<-read.csv("Audio_600_900.csv",sep=",", comment.char ="", quote='"', na.strings ="", header=T)
str(win600_900) #str stands for structure
names(win600_900)
summary(win600_900)
table(win600_900$chlabel)
table(win600_900$binlabel)
summary(win600_900)
names(win600_900)

#Subset for middle channels analysis
win600_900$chlabel<-str_trim(win600_900$chlabel) #Delete the space before and after the targe words.
table(win600_900$chlabel)
channel<-subset(win600_900,chlabel %in% c("PZ","CZ","FZ"))
table(channel$binlabel,channel$bini)
names(channel)
summary(channel)
channel$Vividness_Audio<-as.numeric(channel$Vividness_Audio)
channel$reading.speed<-as.numeric(channel$reading.speed)
channel$comprehension.score<-as.numeric(channel$comprehension.score)
channel$Control_Audio<-as.numeric(channel$Control_Audio)
channel$chlabel<-as.factor(channel$chlabel)
channel$speech<-as.factor(channel$speech)
channel$type<-as.factor(channel$type)
channel$grammar<-as.factor(channel$grammar)

# English/ Chinese Subject Verb Disagreement /Pronoun Lateral Analysis
all4q<-subset(win600_900,bini %in% c(1,2,3,4,5,6,7,8))
summary(all4q$binlabel)
all4q$chlabel<-as.factor(all4q$chlabel)

all4q<-subset(all4q,chlabel %in% c("LF_5s","LP_5s","RF_5s","RP_5s"))
all4q$chlabel<-gsub("_5s","",all4q$chlabel) # delete _5s, replace it with empty
all4q$position1<-sapply(strsplit(all4q$chlabel,split=""),function(x) x[[1]])
all4q$position1
all4q$position2<-sapply(strsplit(all4q$chlabel,split=""),function(x) x[[2]])
all4q$position2
table(all4q$position1)
table(all4q$position2)
table(all4q$binlabel)
tapply(channel$value,list(channel$grammar,channel$chlabel, channel$type, channel$speech),data=channel,mean)


all4q$value<-as.numeric(all4q$value)
all4q$comprehension.score<-as.numeric(all4q$comprehension.score)
all4q$reading.speed<-as.numeric(all4q$reading.speed)
all4q$speech<-as.factor(all4q$speech)
all4q$grammar<-as.factor(all4q$grammar)
all4q$type<-as.factor(all4q$type)
all4q$position1<-as.factor(all4q$position1)
all4q$position2<-as.factor(all4q$position2)
summary(all4q$type)
summary(all4q$grammar)
summary(all4q$speech)
summary(all4q$position1)
summary(all4q$position2)


### Use Anove TYPE X Speech x Grammar subject-verb disagreement ####
whole4q_model1<-aov(value~speech*type*position1*grammar*position2+Error(ERPset/(speech+type+Anteriority+grammar+position2|ERPset),data=all4q)whole4q_model1<-aov(value~speech*position1*grammar*position2*type+Error(ERPset/(speech*grammar)),data=all4q)
                    summary(whole4q_model1)
                    
### Use LME modeling with maximal structure TYPE X Speech x Grammar subject-verb disagreement#####
P6_model2<-lmer(scale(value)~speech*type*grammar*position1*position2+(1+speech+grammar+type|ERPset),data=all4q)
summary(P6_model2) #Without Offline Data
P6_model3<-lmer(scale(value)~grammar*speech*type*position2+position1+scale(comprehension.score)+scale(reading.speed)+(1+position1+position2+grammar+type|ERPset),data=all4q)
summary(P6_model3) #Best Model without interaction with position1
P6_model4<-lmer(scale(value)~grammar*speech*type*position2*position1+scale(comprehension.score)+scale(reading.speed)+(1+position1+position2+grammar+type|ERPset),data=all4q)
summary(P6_model4) #Whole Model, Not very good
P6_model5<-lmer(scale(value)~grammar*speech*type*position2+speech*position1+grammar*position1+type*position1+scale(comprehension.score)+scale(reading.speed)+(1+position1+position2+grammar+type|ERPset),data=all4q)
summary(P6_model5) #Add 2-way interaction with position1
P6_model6<-lmer(scale(value)~grammar*speech*type*position2+grammar*speech*type*position1+scale(comprehension.score)+scale(reading.speed)+(1+position1+position2+grammar+type|ERPset),data=all4q)
summary(P6_model6) # Add 3-way Interaction with position1
print(P6_model6, correlation=TRUE) 
                    
                    
#MidLine Analysis
all4q<-subset(win600_900,bini %in% c(1,2,3,4,5,6,7,8))
table(all4q$binlabel,all4q$bini)
summary(all4q)
summary(all4q$chlabel)
str(all4q)
all4q$value<-as.numeric(all4q$value)
all4q$chlabel<-as.factor(all4q$chlabel)
all4q$speech<-as.factor(all4q$speech)
all4q$grammar<-as.factor(all4q$grammar)
all4q$ERPset<-as.factor(all4q$ERPset)
all4q_Middle<-subset(all4q,chlabel %in% c("FZ","CZ","PZ"))
all4q_Middle$chlabel<-relevel(all4q_Middle$chlabel,ref="FZ")
summary(all4q_Middle$chlabel)
summary(all4q_Middle$speech)
summary(all4q_Middle$ERPset)
                    
#LM Model for MIDDLE LIne
audio_modelmiddle2<-lmer(scale(value)~speech*grammar*type*chlabel+scale(log(comprehension.score))+scale(log(reading.speed))+(1+grammar+type+chlabel||ERPset),data=all4q_Middle)
summary(audio_modelmiddle2) 
audio_modelmiddle3<-lmer(scale(value)~speech*grammar*type*chlabel+(1+grammar+type+chlabel||ERPset),data=all4q_Middle)
summary(audio_modelmiddle3) # Without offline measures
audio_modelmiddle4<-lmer(scale(value)~speech*grammar*type*chlabel+scale(log(comprehension.score))+scale(log(reading.speed))+(1+speech+grammar+type+chlabel|ERPset),data=all4q_Middle)
summary(audio_modelmiddle4)                    
print(audio_modelmiddle4, correlation=TRUE)               
                    
 #Verb(Good vs. Bad) x Speech (English vs. Chinese)
                    verb<-subset(channel,bini %in% c(1,3,5,7))
                    verb$value<-as.numeric(verb$value)
                    verb$chlabel<-as.factor(verb$chlabel)
                    verb$chlabel<-relevel(verb$chlabel,ref="FZ")
                    #Middleline ANOVA & LMM
                    verb_middle1<-aov(value~grammar*speech,data=verb)
                    summary(verb_middle1)
                    verb_middle2<-lmer(scale(value)~speech*chlabel+grammar*chlabel+scale(log(reading.speed))+scale(log(comprehension.score))+(1+speech+grammar+chlabel|ERPset),data=verb)
                    summary(verb_middle2)
                    
                    
                    
                    #Subset for PZ channels analysis
                    win600_900$chlabel<-str_trim(win600_900$chlabel) #Delete the space before and after the targe words.
                    table(win600_900$chlabel)
                    channel<-subset(win600_900,chlabel %in% c("PZ"))
                    table(channel$binlabel,channel$bini)
                    names(channel)
                    channel$Vividness_Audio<-as.numeric(channel$Vividness_Audio)
                    channel$reading.speed<-as.numeric(channel$reading.speed)
                    channel$comprehension.score<-as.numeric(channel$comprehension.score)
                    channel$Control_Audio<-as.numeric(channel$Control_Audio)
                    channel$speech<-as.factor(channel$speech)
                    channel$type<-as.factor(channel$type)
                    channel$grammar<-as.factor(channel$grammar)
                    
                    #Verb(Good vs. Bad) x Speech (English vs. Chinese)
                    verb<-subset(channel,bini %in% c(1,3,5,7))
                    verb$value<-as.numeric(verb$value)
                    verb$chlabel<-as.factor(verb$chlabel)
                    verb$chlabel<-relevel(verb$chlabel,ref="FZ")
                    #Middleline ANOVA & LMM
                    verb_middle1<-aov(value~grammar*speech,data=verb)
                    summary(verb_middle1)
                    verb_middle2<-lmer(scale(value)~speech+grammar+scale(log(reading.speed))+scale(log(comprehension.score))+(1+speech+grammar|ERPset),data=verb)
                    summary(verb_middle2)
                    
                    
                    
                    
                    
                    
                    #### English subjet-verb disagreement
                    english_verb<-subset(channel,bini %in% c(1,3))
                    english_verb$value<-as.numeric(english_verb$value)
                    english_verb$chlabel<-as.factor(english_verb$chlabel)
                    english_verb$chlabel<-relevel(english_verb$chlabel,ref="FZ")
                    #english_verb$comprehension_score<-as.numeric(english_verb$comprehension_score)
                    #english_verb$reading_speed<-as.numeric(english_verb$reading_speed)
                    english_verb_middle1<-aov(scale(value)~grammar*chlabel,data=english_verb)
                    summary(english_verb_middle1)
                    
                    english_verb_middle2<-lmer(scale(value)~grammar*chlabel+scale(log(reading.speed))+scale(log(comprehension.score))+scale(log(Control_Audio))+scale(log(Vividness_Audio))+(1+binlabel|ERPset),data=english_verb)
                    summary(english_verb_middle2)
                    
                    #### Chinese subjet-verb disagreement
                    Chinese_verb<-subset(channel,bini %in% c(5,7))
                    Chinese_verb$value<-as.numeric(Chinese_verb$value)
                    Chinese_verb$chlabel<-as.factor(Chinese_verb$chlabel)
                    summary(Chinese_verb)
                    Chinese_verb$chlabel<-relevel(Chinese_verb$chlabel,ref="FZ")
                    #english_verb$comprehension_score<-as.numeric(english_verb$comprehension_score)
                    #english_verb$reading_speed<-as.numeric(english_verb$reading_speed)
                    Chinese_verb_middle1<-aov(scale(value)~grammar*chlabel,data=Chinese_verb)
                    summary(Chinese_verb_middle1)
                    Chinese_verb_middle2<-lmer(scale(value)~grammar*chlabel+scale(log(reading.speed))+scale(log(comprehension.score))+scale(log(Control_Audio))+scale(log(Vividness_Audio))+(1+binlabel+chlabel|ERPset),data=Chinese_verb)
                    summary(Chinese_verb_middle2)
                    
                    #Lateral Analysis for English/ Chinese Subject Verb Disagreement/PRonoun Mismatch Lateral Analysis
                    setwd("/Volumes/ManqiHD/APS2_Audio/Data Analysis/data/Datasets/R analysis")
                    win600_900<-read.csv("Audio_600_900.csv",sep=",", comment.char ="", quote='"', na.strings ="", header=T)
                    str(win600_900) #str stands for structure
                    names(win600_900)
                    summary(win600_900)
                    table(win600_900$chlabel)
                    table(win600_900$binlabel)
                    
                    win600_900$chlabel<-str_trim(win600_900$chlabel) #Delete the space before and after the targe words.
                    table(win600_900$chlabel)
                    #channel<-subset(win600_900,chlabel %in% c("LF","RF","LP","RP","LC","RC","LF_5s","LP_5s","RF_5s","RP_5s"))
                    channel<-subset(win600_900,chlabel %in% c("LF_5s","LP_5s","RF_5s","RP_5s"))
                    table(channel$chlabel)
                    summary(channel)
                    
                    
                    
                    # English/ Chinese Subject Verb Disagreement Lateral Analysis
                    whole4q<-subset(channel,bini %in% c(1,3,5,7))
                    summary(whole4q$binlabel)
                    whole4q<-subset(whole4q,chlabel %in% c("LF_5s","LP_5s","RF_5s","RP_5s"))
                    whole4q$chlabel<-gsub("_5s","",whole4q$chlabel) # delete _5s, replace it with empty
                    whole4q$position1<-sapply(strsplit(whole4q$chlabel,split=""),function(x) x[[1]])
                    whole4q$position1
                    whole4q$position2<-sapply(strsplit(whole4q$chlabel,split=""),function(x) x[[2]])
                    whole4q$position2
                    table(whole4q$position1)
                    table(whole4q$position2)
                    table(whole4q$binlabel)
                    
                    whole4q$position1<-as.factor(whole4q$position1)
                    whole4q$position2<-as.factor(whole4q$position2)
                    whole4q$value<-as.numeric(whole4q$value)
                    whole4q$comprehension.score<-as.numeric(whole4q$comprehension.score)
                    whole4q$reading.speed<-as.numeric(whole4q$reading.speed)
                    whole4q$speech<-as.factor(whole4q$speech)
                    whole4q$grammar<-as.factor(whole4q$grammar)
                    whole4q$type<-as.factor(whole4q$type)
                    summary(whole4q$type)
                    
                    ### Use Anove Speech x Grammar subject-verb disagreement ####
                    whole4q_model1<-aov(value~speech*position1+speech*position2+grammar*position1+grammar*position2,data=whole4q)
                    summary(whole4q_model1)
                    ### Use LME modeling with maximal structure  Speech x Grammar subject-verb disagreement#####
                    verb_model2<-lmer(scale(value)~speech*position1+speech*position2+grammar*position1+grammar*position2+scale(comprehension.score)+scale(reading.speed)+(1+binlabel+position1+position2+scale(comprehension.score)+scale(reading.speed)|ERPset),data=whole4q)
                    summary(verb_model2) #Best Model
                    
                    
                    
                    
                    
                    #English Subject Verb Disagreement
                    
                    verba<-subset(channel,bini %in% c(1,3))
                    verb<-subset(verba,chlabel %in% c("LF_5s","LP_5s","RF_5s","RP_5s"))
                    verb$chlabel<-gsub("_5s","",verb$chlabel) # delete _5s, replace it with empty
                    verb$position1<-sapply(strsplit(verb$chlabel,split=""),function(x) x[[1]])
                    verb$position1
                    verb$position2<-sapply(strsplit(verb$chlabel,split=""),function(x) x[[2]])
                    verb$position2
                    table(verb$position1)
                    table(verb$position2)
                    
                    verb$position1<-as.factor(verb$position1)
                    verb$position2<-as.factor(verb$position2)
                    verb$value<-as.numeric(verb$value)
                    verb$comprehension.score<-as.numeric(verb$comprehension.score)
                    verb$reading.speed<-as.numeric(verb$reading.speed)
                    #verb$Sex<-as.factor(verb$Sex)
                    # verb$Accuracy<-as.numeric(verb$Accuracy)
                    
                    
                    ### Use Anove####
                    verb_model1<-aov(value~binlabel+position1+position2+Error(ERPset),data=verb)
                    summary(verb_model1)
                    ### Use LME modeling with maximal structure #####
                    verb_model2<-lmer(scale(value)~binlabel*position2+binlabel*position1+scale(comprehension.score)+scale(reading.speed)+(1+binlabel+position1+position2+scale(comprehension.score)+scale(reading.speed)|ERPset),data=verb)
                    summary(verb_model2) #Best Model
                    
                    
                    #Chinese Subject Verb Disagreement
                    
                    verba<-subset(channel,bini %in% c(5,7))
                    verb<-subset(verba,chlabel %in% c("LF_5s","LP_5s","RF_5s","RP_5s"))
                    verb$chlabel<-gsub("_5s","",verb$chlabel) # delete _5s, replace it with empty
                    verb$position1<-sapply(strsplit(verb$chlabel,split=""),function(x) x[[1]])
                    verb$position1
                    verb$position2<-sapply(strsplit(verb$chlabel,split=""),function(x) x[[2]])
                    verb$position2
                    table(verb$position1)
                    table(verb$position2)
                    
                    verb$position1<-as.factor(verb$position1)
                    verb$position2<-as.factor(verb$position2)
                    verb$value<-as.numeric(verb$value)
                    verb$comprehension.score<-as.numeric(verb$comprehension.score)
                    verb$reading.speed<-as.numeric(verb$reading.speed)
                    #verb$Sex<-as.factor(verb$Sex)
                    # verb$Accuracy<-as.numeric(verb$Accuracy)
                    
                    ### Use Anove####
                    verb_model1<-aov(value~binlabel+position1+position2+Error(ERPset),data=verb)
                    summary(verb_model1)
                    ### Use LME modeling with maximal structure #####
                    verb_model2<-lmer(scale(value)~binlabel*position2+binlabel*position1+scale(comprehension.score)+scale(reading.speed)+(1+binlabel+position1+position2+scale(comprehension.score)+scale(reading.speed)|ERPset),data=verb)
                    summary(verb_model2) #Best Model
                    
                    # English/ Chinese Pronoun Lateral Analysis
                    setwd("/Volumes/ManqiHD/APS2_Audio/Data Analysis/data/Datasets/R analysis")
                    win600_900<-read.csv("Audio_600_900.csv",sep=",", comment.char ="", quote='"', na.strings ="", header=T)
                    str(win600_900) #str stands for structure
                    names(win600_900)
                    summary(win600_900)
                    table(win600_900$chlabel)
                    table(win600_900$binlabel)
                    summary(win600_900)
                    names(win600_900)
                    
                    whole4q<-subset(channel,bini %in% c(2,4,6,8))
                    summary(whole4q$binlabel)
                    whole4q<-subset(whole4q,chlabel %in% c("LF_5s","LP_5s","RF_5s","RP_5s"))
                    whole4q$chlabel<-gsub("_5s","",whole4q$chlabel) # delete _5s, replace it with empty
                    whole4q$position1<-sapply(strsplit(whole4q$chlabel,split=""),function(x) x[[1]])
                    whole4q$position1
                    whole4q$position2<-sapply(strsplit(whole4q$chlabel,split=""),function(x) x[[2]])
                    whole4q$position2
                    table(whole4q$position1)
                    table(whole4q$position2)
                    table(whole4q$binlabel)
                    
                    whole4q$position1<-as.factor(whole4q$position1)
                    whole4q$position2<-as.factor(whole4q$position2)
                    whole4q$value<-as.numeric(whole4q$value)
                    whole4q$comprehension.score<-as.numeric(whole4q$comprehension.score)
                    whole4q$reading.speed<-as.numeric(whole4q$reading.speed)
                    whole4q$speech<-as.factor(whole4q$speech)
                    whole4q$grammar<-as.factor(whole4q$grammar)
                    whole4q$type<-as.factor(whole4q$type)
                    summary(whole4q$type)
                    
                    ### Use Anove Speech x Grammar subject-pronoun disagreement ####
                    whole4q_model1<-aov(value~speech*position1+speech*position2+grammar*position1+grammar*position2,data=whole4q)
                    summary(whole4q_model1)
                    ### Use LME modeling with maximal structure  Speech x Grammar subject-pronoun disagreement#####
                    pronoun_model2<-lmer(scale(value)~speech*position1+speech*position2+grammar*position1+grammar*position2+scale(comprehension.score)+scale(reading.speed)+(1+binlabel+position1+position2+scale(comprehension.score)+scale(reading.speed)|ERPset),data=whole4q)
                    summary(pronoun_model2) #Best Model
                    
                    #English Pronoun Mismatch
                    pronouna<-subset(channel,bini %in% c(2,4))
                    pronoun<-subset(pronouna,chlabel %in% c("LF_5s","LP_5s","RF_5s","RP_5s"))
                    pronoun$chlabel<-gsub("_5s","",pronoun$chlabel) # delete _5s, replace it with empty
                    pronoun$position1<-sapply(strsplit(pronoun$chlabel,split=""),function(x) x[[1]])
                    pronoun$position1
                    pronoun$position2<-sapply(strsplit(pronoun$chlabel,split=""),function(x) x[[2]])
                    pronoun$position2
                    table(pronoun$position1)
                    table(pronoun$position2)
                    
                    pronoun$position1<-as.factor(pronoun$position1)
                    pronoun$position2<-as.factor(pronoun$position2)
                    pronoun$value<-as.numeric(pronoun$value)
                    pronoun$comprehension.score<-as.numeric(pronoun$comprehension.score)
                    pronoun$reading.speed<-as.numeric(pronoun$reading.speed)
                    #pronoun$Sex<-as.factor(pronoun$Sex)
                    # pronoun$Accuracy<-as.numeric(pronoun$Accuracy)
                    
                    ### Use Anove####
                    pronoun_model1<-aov(value~binlabel+position1+position2+Error(ERPset),data=pronoun)
                    summary(pronoun_model1)
                    ### Use LME modeling with maximal structure #####
                    pronoun_model2<-lmer(scale(value)~binlabel*position2+binlabel*position1+scale(comprehension.score)+scale(reading.speed)+(1+binlabel+position1+position2+scale(comprehension.score)+scale(reading.speed)|ERPset),data=pronoun)
                    summary(pronoun_model2) #Best Model
                    
                    #Chinese Pronoun Mismatch
                    pronouna<-subset(channel,bini %in% c(6,8))
                    pronoun<-subset(pronouna,chlabel %in% c("LF_5s","LP_5s","RF_5s","RP_5s"))
                    pronoun$chlabel<-gsub("_5s","",pronoun$chlabel) # delete _5s, replace it with empty
                    pronoun$position1<-sapply(strsplit(pronoun$chlabel,split=""),function(x) x[[1]])
                    pronoun$position1
                    pronoun$position2<-sapply(strsplit(pronoun$chlabel,split=""),function(x) x[[2]])
                    pronoun$position2
                    table(pronoun$position1)
                    table(pronoun$position2)
                    
                    pronoun$position1<-as.factor(pronoun$position1)
                    pronoun$position2<-as.factor(pronoun$position2)
                    pronoun$value<-as.numeric(pronoun$value)
                    pronoun$comprehension.score<-as.numeric(pronoun$comprehension.score)
                    pronoun$reading.speed<-as.numeric(pronoun$reading.speed)
            
                    ### Use Anove####
                    pronoun_model1<-aov(value~binlabel+position1+position2+Error(ERPset),data=pronoun)
                    summary(pronoun_model1)
                    ### Use LME modeling with maximal structure #####
                    pronoun_model2<-lmer(scale(value)~binlabel*position2+binlabel*position1+scale(comprehension.score)+scale(reading.speed)+(1+binlabel+position1+position2+scale(comprehension.score)+scale(reading.speed)|ERPset),data=pronoun)
                    summary(pronoun_model2) #Best Model
                    
                    