# Peiyun Zhou
# youneverleave@gmail.com
# R scripts--compare the silent reading in E1 and APS of native and non-native speech in E2

# load the packages 
library(languageR)
library(stringr)
library(ez)
library(car)
library(lme4)
library(stringr)
library(lmerTest)

######## Silent_300_500 #############

setwd("Set working directory here")
Silent_win300_500<-read.csv("APS1_300_500_43subjects.csv",sep=",", comment.char ="", quote='"', na.strings ="", header=T)
table(Silent_win300_500$bini)
str(Silent_win300_500)
names(Silent_win300_500)
summary(Silent_win300_500)
table(Silent_win300_500$chlabel)
table(Silent_win300_500$binlabel)

Silent_win300_500$chlabel<-str_trim(Silent_win300_500$chlabel) #Delete the space before and after the targe words.
Silent_win300_500$value<-as.numeric(Silent_win300_500$value)
Silent_win300_500$chlabel<-as.factor(Silent_win300_500$chlabel)
Silent_win300_500$binlabel<-as.factor(Silent_win300_500$binlabel)
Silent_win300_500$grammar<-as.factor(Silent_win300_500$grammar)
Silent_win300_500$word_type<-as.factor(Silent_win300_500$word_type)
Silent_win300_500$comprehension_score<-as.numeric(Silent_win300_500$comprehension_score)
Silent_win300_500$reading_speed<-as.numeric(Silent_win300_500$reading_speed)

Silent_win300_500$Accuracy<-as.numeric(Silent_win300_500$Accuracy)
table(Silent_win300_500$chlabel)
table(Silent_win300_500$binlabel)
table(Silent_win300_500$binlabel,Silent_win300_500$bini)

#Models for Middleline analysis for Subject_Verb and pronoun Disagreement
VerbPro_silent<-subset(Silent_win300_500,bini %in% c(1,2,3,4))
summary(VerbPro_silent)
VerbPro_silent_Middle<-subset(VerbPro_silent,chlabel %in% c("FZ","CZ","PZ"))
summary(VerbPro_silent_Middle$chlabel)
N2_modelmiddle1<-aov(value~(grammar*word_type)+Error(ERPset/(grammar*word_type)),data=VerbPro_silent_Middle)
summary(N2_modelmiddle1)
N2_modelmiddle2<-aov(value~grammar,data=VerbPro_silent_Middle)
summary(N2_modelmiddle2)

#LMM resutls for middleline
summary(VerbPro_silent_Middle$grammar)
VerbPro_silent_Middle$chlabel<-relevel(VerbPro_silent_Middle$chlabel,ref="FZ")
N2_modelmiddle2<-lmer(scale(value)~grammar*word_type+scale(log(Accuracy))+scale(log(comprehension_score))+scale(log(reading_speed))+scale(log(Accuracy))+scale(log(comprehension_score))+(1+grammar*word_type|ERPset),data=VerbPro_silent_Middle)
summary(N2_modelmiddle2) #Best Model


#Models for Middleline analysis for Subject_Verb Disagreement
Verb_silent<-subset(Silent_win300_500,bini %in% c(1,2))
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
summary(N2_modelmiddle2) 
N2_modelmiddle3<-lmer(scale(value)~grammar*chlabel+scale(log(Accuracy))+scale(log(comprehension_score))+grammar*scale(log(reading_speed))+scale(log(Accuracy))+scale(log(comprehension_score))+(1+grammar+chlabel|ERPset),data=Verb_silent_Middle)
summary(N2_modelmiddle3) 


#Models for Middleline analysis for Pronoun Mismatch
Pronoun_silent<-subset(Silent_win300_500,bini %in% c(3,4))
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
summary(Pronoun_modelmiddle2) #Best Model1
Pronoun_modelmiddle3<-lmer(scale(value)~grammar*chlabel+scale(log(Accuracy))+scale(log(comprehension_score))+grammar*scale(log(reading_speed))+scale(log(Accuracy))+scale(log(comprehension_score))+(1+grammar+chlabel|ERPset),data=Pronoun_silent_Middle)
summary(Pronoun_modelmiddle3) #Best Model1


#LMM subset the channels for verb and Pronoun analysis
Silent_win300_500$chlabel<-str_trim(Silent_win300_500$chlabel) #Delete the space before and after the targe words.
table(Silent_win300_500$chlabel)
Silent_channel<-subset(Silent_win300_500,chlabel %in% c("LF","RF","LP","RP","LC","RC","LF_5s","LP_5s","RF_5s","RP_5s"))
table(Silent_channel$chlabel)
table(Silent_channel$binlabel,Silent_channel$bini)


#Models for 4 Qudrants analysis for Subject_Verb and Pronoun Disagreement
Verb_silenta<-subset(Silent_channel,bini %in% c(1,2,3,4))
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

### Use Anove for 4 Qudruants 
N400_Verb_silent_model1<-aov(value~(word_type*grammar+position1+position2)+Error(ERPset/(word_type*grammar+position1+position2)),data=Verb_silent)
summary(N400_Verb_silent_model1)
N400_Verb_silent_model1<-aov(value~(word_type*grammar+position1+position2)+Error(ERPset/(word_type*grammar+position1+position2)),data=Verb_silent)
summary(N400_Verb_silent_model1)

### Use LME modeling with maximal structure 
N400_Verb_silent_model2<-lmer(scale(value)~grammar*word_type+grammar*position2+grammar*position1+word_type*position1+word_type*position2+scale(Accuracy)+scale(reading_speed)+scale(comprehension_score)+(1+grammar*word_type+position1+position2|ERPset),data=Verb_silent)
summary(N400_Verb_silent_model2) #Best grammar and positions interaction, word_type and positions interaction, grammar and word_type interaction

N400_Verb_silent_model3<-lmer(scale(value)~binlabel*position1*position2+(1+binlabel*position1*position2|ERPset),data=Verb_silent)
summary(N400_Verb_silent_model3) 
N400_Verb_silent_model4<-lmer(scale(value)~binlabel*position1+position2+(1+binlabel*position2+position1|ERPset),data=Verb_silent)
summary(N400_Verb_silent_model4) 
N400_Verb_silent_model5<-lmer(scale(value)~binlabel+position1*position2+(1+binlabel+position2*position1|ERPset),data=Verb_silent)
summary(N400_Verb_silent_model5) 

#### Subject Verb Disagreement
Silent_win300_500$chlabel<-str_trim(Silent_win300_500$chlabel) #Delete the space before and after the targe words.
table(Silent_win300_500$chlabel)
Silent_channel<-subset(Silent_win300_500,chlabel %in% c("LF","RF","LP","RP","LC","RC","LF_5s","LP_5s","RF_5s","RP_5s"))
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
N400_V_silent_model1<-aov(value~grammar+position1+position2,data=V_silent)
summary(N400_V_silent_model1)
N400_V_silent_model2<-aov(value~grammar*position1+grammar*position2,data=V_silent)
summary(N400_V_silent_model2)

### Use LME modeling with maximal structure #####
N400_V_silent_model2<-lmer(scale(value)~grammar+grammar*position2+grammar*position1+position1+position2+scale(Accuracy)+grammar*scale(reading_speed)+scale(comprehension_score)+(1+grammar+position1+position2|ERPset),data=V_silent)
summary(N400_V_silent_model2) 
N400_V_silent_model3<-lmer(scale(value)~grammar+grammar*position2+grammar*position1+position1+position2+scale(Accuracy)+grammar*scale(reading_speed)+scale(comprehension_score)+(1+grammar+position1+position2|ERPset),data=V_silent)
summary(N400_V_silent_model3) #Best grammar and positions interaction,  grammar and offline reading speed interaction


#### Pronoun Case mismatch########################################
P_silenta<-subset(Silent_channel,bini %in% c(3,4))
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
N400_P_silent_model1<-aov(value~grammar+position1+position2,data=P_silent)
summary(N400_P_silent_model1)
N400_P_silent_model2<-aov(value~grammar*position1+grammar*position2,data=P_silent)
summary(N400_P_silent_model2)

### Use LME modeling with maximal structure #####
N400_P_silent_model2<-lmer(scale(value)~grammar+grammar*position2+grammar*position1+position1+position2+scale(Accuracy)+grammar*scale(reading_speed)+scale(comprehension_score)+(1+grammar+position1+position2|ERPset),data=P_silent)
summary(N400_P_silent_model2) 
N400_P_silent_model3<-lmer(scale(value)~grammar+grammar*position2+grammar*position1+position1+position2+scale(Accuracy)+grammar*scale(reading_speed)+scale(comprehension_score)+(1+grammar+position1+position2|ERPset),data=P_silent)
summary(N400_P_silent_model3) #Best grammar and positions interaction,  grammar and offline reading speed interaction


