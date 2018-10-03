# Peiyun Zhou
# youneverleave@gmail.com
# R script--Analyze the APS data in E1 silent reading for P600 window

# load packages
library(languageR)
library(stringr)
library(ez)
library(car)
library(lme4)
library(stringr)
library(lmerTest)
library(afex)

# Silent_600_900 

setwd("Set working directory")
Silent_win600_900<-read.csv("APS1_600_900_43subjects.csv",sep=",", comment.char ="", quote='"', na.strings ="", header=T)
table(Silent_win600_900$bini)
str(Silent_win600_900)
names(Silent_win600_900)
summary(Silent_win600_900)
table(Silent_win600_900$chlabel)
table(Silent_win600_900$binlabel)

Silent_win600_900$chlabel<-str_trim(Silent_win600_900$chlabel) #Delete the space before and after the targe words.
Silent_win600_900$value<-as.numeric(Silent_win600_900$value)
Silent_win600_900$chlabel<-as.factor(Silent_win600_900$chlabel)
Silent_win600_900$binlabel<-as.factor(Silent_win600_900$binlabel)
Silent_win600_900$grammar<-as.factor(Silent_win600_900$grammar)
Silent_win600_900$word_type<-as.factor(Silent_win600_900$word_type)
Silent_win600_900$comprehension_score<-as.numeric(Silent_win600_900$comprehension_score)
Silent_win600_900$reading_speed<-as.numeric(Silent_win600_900$reading_speed)
#Verb_silent$Sex<-as.factor(Verb_silent$Sex)
Silent_win600_900$Accuracy<-as.numeric(Silent_win600_900$Accuracy)
table(Silent_win600_900$chlabel)
table(Silent_win600_900$binlabel)
table(Silent_win600_900$binlabel,Silent_win600_900$bini)


#Models for Midline analysis for Subject_Verb and pronoun Disagreement
VerbPro_silent<-subset(Silent_win600_900,bini %in% c(1,2,3,4))
summary(VerbPro_silent)
VerbPro_silent_Middle<-subset(VerbPro_silent,chlabel %in% c("FZ","CZ","PZ"))
VerbPro_silent_Middle$chlabel<-relevel(VerbPro_silent_Middle$chlabel,ref="FZ")
summary(VerbPro_silent_Middle$chlabel)
summary(VerbPro_silent_Middle)
summary(VerbPro_silent_Middle$subjectID)

N2_modelmiddle3<-lmer(scale(value)~chlabel*grammar*word_type+scale(log(comprehension_score))+scale(log(reading_speed))+(1+grammar*word_type+chlabel|ERPset),data=VerbPro_silent_Middle)
summary(N2_modelmiddle3) #Cant converge
N2_modelmiddle3<-lmer(scale(value)~chlabel*grammar*word_type+scale(log(comprehension_score))+scale(log(reading_speed))+(1+grammar*word_type|ERPset),data=VerbPro_silent_Middle)
summary(N2_modelmiddle3) 

tapply(VerbPro_silent_Middle$value, VerbPro_silent_Middle$chlabel,VerbPro_silent_Middle$grammar,VerbPro_silent_Middle$word_type,mean)
tapply(VerbPro_silent_Middle$value, VerbPro_silent_Middle$chlabel,mean)
tapply(VerbPro_silent_Middle$value, VerbPro_silent_Middle$grammar,mean)


rtMean<-tapply(VerbPro_silent_Middle$value,list(VerbPro_silent_Middle$grammar,VerbPro_silent_Middle$chlabel,VerbPro_silent_Middle$word_type),mean)
rtMean

summary(VerbPro_silent_Middle$grammar)
VerbPro_silent_Middle$chlabel<-relevel(VerbPro_silent_Middle$chlabel,ref="FZ")
N2_modelmiddle2<-lmer(scale(value)~grammar*word_type*chlabel+scale(log(Accuracy))+scale(log(comprehension_score))+scale(log(reading_speed))+scale(log(Accuracy))+(1+grammar+word_type+chlabel||ERPset),data=VerbPro_silent_Middle)
summary(N2_modelmiddle2) #Best Model


#Models for Middleline analysis for Subject_Verb Disagreement
Verb_silent<-subset(Silent_win600_900,bini %in% c(1,2))
summary(Verb_silent)
Verb_silent_Middle<-subset(Verb_silent,chlabel %in% c("FZ","CZ","PZ"))
summary(Verb_silent_Middle$chlabel)


N2_modelmiddle1<-aov(value~grammar,data=Verb_silent_Middle)
summary(N2_modelmiddle1)
N2_modelmiddle2<-aov(value~grammar*chlabel,data=Verb_silent_Middle)
summary(N2_modelmiddle2)

#LMM resutls for middleline
summary(Verb_silent_Middle$grammar)
Verb_silent_Middle$chlabel<-relevel(Verb_silent_Middle$chlabel,ref="FZ")
N2_modelmiddle2<-lmer(scale(value)~grammar*chlabel+scale(log(Accuracy))+scale(log(comprehension_score))+scale(log(reading_speed))+scale(log(Accuracy))+scale(log(comprehension_score))+(1+grammar+chlabel|ERPset),data=Verb_silent_Middle)
summary(N2_modelmiddle2) #
N2_modelmiddle3<-lmer(scale(value)~grammar*chlabel+scale(log(Accuracy))+scale(log(comprehension_score))+grammar*scale(log(reading_speed))+scale(log(Accuracy))+scale(log(comprehension_score))+(1+grammar+chlabel|ERPset),data=Verb_silent_Middle)
summary(N2_modelmiddle3) #Best Model 3


#Models for Middleline analysis for Pronoun Mismatch
Pronoun_silent<-subset(Silent_win600_900,bini %in% c(3,4))
summary(Pronoun_silent)
Pronoun_silent_Middle<-subset(Pronoun_silent,chlabel %in% c("FZ","CZ","PZ"))
summary(Pronoun_silent_Middle$chlabel)
#ANOVA resutls, bin label was marginally significant 
# Two Within Factors W1 W2, Two Between Factors B1 B2 
#fit <- aov(y~(W1*W2*B1*B2)+Error(Subject/(W1*W2))+(B1*B2),data=mydataframe) 
pro_modelmiddle1<-aov(value~grammar,data=Pronoun_silent_Middle)
summary(pro_modelmiddle1)
pro_modelmiddle2<-aov(value~grammar*chlabel,data=Pronoun_silent_Middle)
summary(pro_modelmiddle2)

#LMM resutls for middleline
summary(Pronoun_silent_Middle$grammar)
Pronoun_silent_Middle$chlabel<-relevel(Pronoun_silent_Middle$chlabel,ref="FZ")
Pronoun_modelmiddle2<-lmer(scale(value)~grammar*chlabel+scale(log(Accuracy))+scale(log(comprehension_score))+scale(log(reading_speed))+scale(log(Accuracy))+scale(log(comprehension_score))+(1+grammar+chlabel|ERPset),data=Pronoun_silent_Middle)
summary(Pronoun_modelmiddle2) #
Pronoun_modelmiddle3<-lmer(scale(value)~grammar*chlabel+scale(log(Accuracy))+scale(log(comprehension_score))+grammar*scale(log(reading_speed))+scale(log(Accuracy))+scale(log(comprehension_score))+(1+grammar+chlabel|ERPset),data=Pronoun_silent_Middle)
summary(Pronoun_modelmiddle3) #Best Model1

# LATERALAnalysis
#LMM subset the channels for verb and Pronoun analysis
Silent_win600_900$chlabel<-str_trim(Silent_win600_900$chlabel) #Delete the space before and after the targe words.
table(Silent_win600_900$chlabel)
Silent_channel<-subset(Silent_win600_900,chlabel %in% c("LF","RF","LP","RP","LC","RC","LF_5s","LP_5s","RF_5s","RP_5s"))
table(Silent_channel$chlabel)
table(Silent_channel$binlabel,Silent_channel$bini)


#Models for 4 Qudrants analysis for Subject_Verb and Pronoun Disagreement
Verb_silenta<-subset(Silent_win600_900,bini %in% c(1,2,3,4))
summary(Verb_silenta$chlabel)
Verb_silent<-subset(Verb_silenta,chlabel %in% c("LF_5s","LP_5s","RF_5s","RP_5s"))
summary(Verb_silent$chlabel)
Verb_silent$chlabel<-gsub("_5s","",Verb_silent$chlabel) # delete _5s, replace it with empty
Verb_silent$position1<-sapply(strsplit(Verb_silent$chlabel,split=""),function(x) x[[1]])
Verb_silent$position1
Verb_silent$position2<-sapply(strsplit(Verb_silent$chlabel,split=""),function(x) x[[2]])
Verb_silent$position2
table(Verb_silent$position1)
table(Verb_silent$position2)

#Set varaibles
Verb_silent$grammar<-as.factor(Verb_silent$grammar)
Verb_silent$word_type<-as.factor(Verb_silent$word_type)
Verb_silent$position1<-as.factor(Verb_silent$position1)
Verb_silent$position2<-as.factor(Verb_silent$position2)
Verb_silent$value<-as.numeric(Verb_silent$value)
Verb_silent$comprehension_score<-as.numeric(Verb_silent$comprehension_score)
Verb_silent$reading_speed<-as.numeric(Verb_silent$reading_speed)
#Verb_silent$Sex<-as.factor(Verb_silent$Sex)
Verb_silent$Accuracy<-as.numeric(Verb_silent$Accuracy)

### Use LME modeling with maximal structure #####
P600_Verb_silent_model2<-lmer(scale(value)~grammar*word_type*position2*position1+scale(reading_speed)+scale(comprehension_score)+(1+grammar*word_type+position1+position2|ERPset),data=Verb_silent)
summary(P600_Verb_silent_model2) #4 way interactions
P600_Verb_silent_model3<-lmer(scale(value)~grammar*word_type*position1+grammar*word_type*position2+scale(reading_speed)+scale(comprehension_score)+(1+grammar*word_type+position1+position2|ERPset),data=Verb_silent)
summary(P600_Verb_silent_model3) #3 way interactions

P600_Verb_silent_model5<-lmer(scale(value)~grammar*word_type*position2*position1+scale(Accuracy)+(1+grammar*word_type+position1+position2|ERPset),data=Verb_silent)
summary(P600_Verb_silent_model5) # Remove offline measures
P600_Verb_silent_model6<-lmer(scale(value)~grammar*word_type+grammar*position2*position1+word_type*position1*position2+scale(Accuracy)+grammar*scale(reading_speed)+scale(comprehension_score)+(1+grammar*word_type+position1+position2|ERPset),data=Verb_silent)
summary(P600_Verb_silent_model) # 2-way interactions

### Use Anove for 4 Qudruants ####
P600_Verb_silent_model1<-aov(value~(word_type*grammar+position1+position2)+Error(ERPset/(word_type*grammar+position1+position2)),data=Verb_silent)
summary(P600_Verb_silent_model1)
P600_Verb_silent_model2<-aov(value~(word_type*grammar+grammar*position1+grammar*position2+word_type*position1+word_type*position2)+Error(ERPset/(word_type*grammar+position1+position2)),data=Verb_silent)
summary(P600_Verb_silent_model2)

P600_Verb_silent_model3<-aov(value~word_type*grammar*position2*position1+Error(ERPset/word_type*grammar*position2*position1),data=Verb_silent)
summary(P600_Verb_silent_model3)
P600_Verb_silent_model3<-aov(value~word_type*grammar*position2*position1+Error(ERPset/word_type+grammar+position2+position1),data=Verb_silent)
summary(P600_Verb_silent_model3)

#### Subject Verb Disagreement########################################
Silent_win600_900$chlabel<-str_trim(Silent_win600_900$chlabel) #Delete the space before and after the targe words.
table(Silent_win600_900$chlabel)
Silent_channel<-subset(Silent_win600_900,chlabel %in% c("LF","RF","LP","RP","LC","RC","LF_5s","LP_5s","RF_5s","RP_5s"))
table(Silent_channel$chlabel)
table(Silent_channel$binlabel,Silent_channel$bini)


#Models for 4 Qudrants analysis for Subject_Verb Disagreement
V_silenta<-subset(Silent_channel,bini %in% c(1,2))
summary(V_silenta$chlabel)
V_silent<-subset(V_silenta,chlabel %in% c("LF_5s","LP_5s","RF_5s","RP_5s"))
summary(V_silent$chlabel)
V_silent$chlabel<-gsub("_5s","",V_silent$chlabel) # delete _5s, replace it with empty
V_silent$position1<-sapply(strsplit(V_silent$chlabel,split=""),function(x) x[[1]])
V_silent$position1
V_silent$position2<-sapply(strsplit(V_silent$chlabel,split=""),function(x) x[[2]])
V_silent$position2
table(V_silent$position1)
table(V_silent$position2)

#Set varaibles
V_silent$grammar<-as.factor(V_silent$grammar)
V_silent$word_type<-as.factor(V_silent$word_type)
V_silent$position1<-as.factor(V_silent$position1)
V_silent$position2<-as.factor(V_silent$position2)
V_silent$value<-as.numeric(V_silent$value)
V_silent$comprehension_score<-as.numeric(V_silent$comprehension_score)
V_silent$reading_speed<-as.numeric(V_silent$reading_speed)
#Verb_silent$Sex<-as.factor(Verb_silent$Sex)
V_silent$Accuracy<-as.numeric(V_silent$Accuracy)

### Use Anove for 4 Qudruants ####
P600_V_silent_model1<-aov(value~grammar+position1+position2,data=V_silent)
summary(P600_V_silent_model1)
P600_V_silent_model2<-aov(value~grammar*position1+grammar*position2,data=V_silent)
summary(P600_V_silent_model2)

### Use LME modeling with maximal structure #####
P600_V_silent_model2<-lmer(scale(value)~grammar+grammar*position2+grammar*position1+position1+position2+scale(Accuracy)+grammar*scale(reading_speed)+scale(comprehension_score)+(1+grammar+position1+position2|ERPset),data=V_silent)
summary(P600_V_silent_model2) 
P600_V_silent_model3<-lmer(scale(value)~grammar+grammar*position2+grammar*position1+position1+position2+scale(Accuracy)+grammar*scale(reading_speed)+scale(comprehension_score)+(1+grammar+position1+position2|ERPset),data=V_silent)
summary(P600_V_silent_model3) #Best grammar and positions interaction,  grammar and offline reading speed interaction

#### Pronoun Case mismatch########################################
P_silenta<-subset(Silent_win600_900,bini %in% c(3,4))
summary(P_silenta$chlabel)
P_silent<-subset(P_silenta,chlabel %in% c("LF_5s","LP_5s","RF_5s","RP_5s"))
summary(P_silent$chlabel)
P_silent$chlabel<-gsub("_5s","",P_silent$chlabel) # delete _5s, replace it with empty
P_silent$position1<-sapply(strsplit(P_silent$chlabel,split=""),function(x) x[[1]])
P_silent$position1
P_silent$position2<-sapply(strsplit(P_silent$chlabel,split=""),function(x) x[[2]])
P_silent$position2
table(P_silent$position1)
table(P_silent$position2)

#Set varaibles
P_silent$grammar<-as.factor(P_silent$grammar)
P_silent$word_type<-as.factor(P_silent$word_type)
P_silent$position1<-as.factor(P_silent$position1)
P_silent$position2<-as.factor(P_silent$position2)
P_silent$value<-as.numeric(P_silent$value)
P_silent$comprehension_score<-as.numeric(P_silent$comprehension_score)
P_silent$reading_speed<-as.numeric(P_silent$reading_speed)
#Perb_silent$Sex<-as.factor(Perb_silent$Sex)
P_silent$Accuracy<-as.numeric(P_silent$Accuracy)

### Use Anove for 4 Qudruants ####
P600_P_silent_model1<-aov(value~grammar+position1+position2,data=P_silent)
summary(P600_P_silent_model1)
P600_P_silent_model2<-aov(value~grammar*position1+grammar*position2,data=P_silent)
summary(P600_P_silent_model2)

### Use LME modeling with maximal structure #####
P600_P_silent_model2<-lmer(scale(value)~grammar+grammar*position2+grammar*position1+position1+position2+scale(Accuracy)+grammar*scale(reading_speed)+scale(comprehension_score)+(1+grammar+position1+position2|ERPset),data=P_silent)
summary(P600_P_silent_model2) 
P600_P_silent_model3<-lmer(scale(value)~grammar+grammar*position2+grammar*position1+position1+position2+scale(Accuracy)+grammar*scale(reading_speed)+scale(comprehension_score)+(1+grammar+position1+position2|ERPset),data=P_silent)
summary(P600_P_silent_model3) #Best grammar and positions interaction,  grammar and offline reading speed interaction


