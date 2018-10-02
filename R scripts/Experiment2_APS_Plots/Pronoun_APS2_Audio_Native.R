# Peiyun Zhou
# youneverleave@gmail.com
# R codes--the ERP plot for the Pronouns in Experiment 2 Native APS condition
# The figure presents the readers' responses to grammtical pronouns and ungrammtical pronouns
# when they imagining the Native English speaker's voice

setwd("Set working directory here")
cond1<-read.csv("APS2_Bin1_English Correct Verb in Good Pronoun.csv",sep=",", comment.char ="", quote='"', na.strings ="", header=T)
cond3<-read.csv("APS2_Bin5_English Good Pronoun in correct Verb.csv",sep=",", comment.char ="", quote='"', na.strings ="", header=T)
cond4<-read.csv("APS2_Bin6_English Bad Pronoun in Correct Verb.csv",sep=",", comment.char ="", quote='"', na.strings ="", header=T)

presample 	<- 	-100		#How many miliseconds before stimulus onset would you like to plot? (negative values mean prestimulus)
epoch 		<- 	990		#How many miliseconds poststimulus would you like to plot (i.e., where you want the plot to end)?

base_begin 	<- 	-100		#Enter the beginning of the baseline interval (negative values mean prestimulus)
base_end 	<- 	0			#Enter the end of the baseline interval (negative values mean prestimulus)

cond1_color <- "black"		#What color do you want cond1's line color to be?
cond2_color <- "black"		#What color do you want cond2's line color to be?
cond3_color <- "black"		#What color do you want cond1's line color to be?
cond4_color <- "black"		#What color do you want cond2's line color to be?


cond3_type <- 1				#What type of line do you want cond1's line to be?
cond4_type <- 5				#What type of line do you want cond2's line to be?

cond1_name <- "Grammatical Verb"		#What do you want cond1 to be called in the legend?
cond2_name <- "Ungrammatical Verb"	#What do you want cond2 to be called in the legend?
cond3_name <- "Grammatical Pronoun"		#What do you want cond1 to be called in the legend?
cond3_name <- "Ungrammatical Pronoun"	#What do you want cond2 to be called in the legend?

# Some graphic parameters that you might want to change, but probably won't need to. 
# If you don't like the scale of your plot, the text sizes, or line thicknesses, play with these values until you get something you like.

y_size <- 15			#How many µV do you want the total area of each subplot to be?						
#Changing the y_size value will change the x-y scale of your plot

cal_size <- 5			#How many µV do you want your y-axis calibration bars to show? 3 or 5µV should be good for most applications.
text_size <- .65			#How big do you want the text on the labels? Play with a value that will look good when you scale your final plots.
line_width <- .9		#How thick do you want your ERP wave lines to be?  Play with values that look good when you scale your plots

##############################################################################################################
#########     The stuff below this does the work.                         ####################################
##############################################################################################################

start <- match(presample, cond1$time)		#Finds the row number of the starting point
stop <- match(epoch, cond1$time)			#Finds the row number of the end point
base1 <- match(base_begin, cond1$time)		#Finds the row number of the beginning of the baseline
base2 <- match(base_end, cond1$time)		#Finds the row number of the end of the baseline
base3 <- match(base_begin, cond1$time)		#Finds the row number of the beginning of the baseline
base4 <- match(base_end, cond1$time)		#Finds the row number of the end of the baseline


################################################
#	Plot F3 Condition 1
################################################
# Calculate the mean amplitude in the grammatical condition for the baseline interval for the electrode you're plotting and then rebaseline
#base_mean_cond1 <- mean(cond1$F3[base1:base2])
#rebase_cond1 <- cond1$F3 - base_mean_cond1

# Plot cond1. If your ERPs are very large, you might need to expand the values in the ylim argument above.    
par(fig=c(0,.44,.5,1))
base_mean_cond3 <- mean(cond3$F3[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond3 <- cond3$F3 - base_mean_cond3  
plot(cond3$time[start:stop], rebase_cond3[start:stop], las=1, xlab=NA, ylab=NA, xlim=c(base_begin-50, epoch), axes=FALSE, frame.plot=FALSE, type="l", lwd=line_width, lty=cond3_type,col=cond3_color, ylim=rev(c(-y_size, y_size)) )

# Generate some axes and an electrode label.
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=-.01)
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=.01)
axis(side = 1, at=seq(presample,epoch, by=.1), pos=0, labels=NA, tck=0)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=-.01)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=.01)
text(-120, -cal_size, "F3", cex=text_size)


#	Clean up some variables
#rm(base_mean_cond1,rebase_cond1,base_mean_cond2,rebase_cond2)

##############################################################################################################
#### Rebaseline and plot F3 cond4.
############################################################################################################## 
base_mean_cond4 <- mean(cond4$F3[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond4 <- cond4$F3 - base_mean_cond4         #Re-baseline that condition
lines(cond4$time[start:stop], rebase_cond4[start:stop], type="l",lwd=line_width, lty=cond4_type, col=cond4_color)   #Plot it

#	Clean up some variables
#rm(base_mean_cond1,rebase_cond1,base_mean_cond2,rebase_cond2,base_mean_cond3,rebase_cond3,base_mean_cond4,rebase_cond4)

################################################
#	Plot Fz Condition 1
################################################
# Calculate the mean amplitude in the grammatical condition for the baseline interval for the electrode you're plotting and then rebaseline
base_mean_cond1 <- mean(cond1$FZ[base1:base2])
rebase_cond1 <- cond1$FZ - base_mean_cond1

# Plot cond1. If your ERPs are very large, you might need to expand the values in the ylim argument above.    
par(fig=c(.28,.72,.5,1), new=T)
base_mean_cond3 <- mean(cond3$FZ[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond3 <- cond3$FZ - base_mean_cond3  
plot(cond3$time[start:stop], rebase_cond3[start:stop], las=1, xlab=NA, ylab=NA, xlim=c(base_begin-50, epoch), axes=FALSE, frame.plot=FALSE, type="l", lwd=line_width, lty=cond3_type,col=cond3_color, ylim=rev(c(-y_size, y_size)) )

# Generate some axes and an electrode label.
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=-.01)
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=.01)
axis(side = 1, at=seq(presample,epoch, by=.1), pos=0, labels=NA, tck=0)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=-.01)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=.01)
text(-120, -cal_size, "Fz", cex=text_size)


##############################################################################################################
#### Rebaseline and plot Fz cond4.
############################################################################################################## 
base_mean_cond4 <- mean(cond4$FZ[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond4 <- cond4$FZ - base_mean_cond4         #Re-baseline that condition
lines(cond4$time[start:stop], rebase_cond4[start:stop], type="l",lwd=line_width, lty=cond4_type, col=cond4_color)   #Plot it
lines(cond4$time[start:stop], rebase_cond4[start:stop], type="l",lwd=line_width, lty=cond4_type, col=cond4_color)   #Plot it


# Plot cond1. If your ERPs are very large, you might need to expand the values in the ylim argument above.    
par(fig=c(.56,1,.5,1), new=T)
base_mean_cond3 <- mean(cond3$F4[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond3 <- cond3$F4 - base_mean_cond3   
plot(cond3$time[start:stop], rebase_cond3[start:stop], las=1, xlab=NA, ylab=NA, xlim=c(base_begin-50, epoch), axes=FALSE, frame.plot=FALSE, type="l", lwd=line_width, lty=cond3_type,col=cond3_color, ylim=rev(c(-y_size, y_size)) )

# Generate some axes and an electrode label.
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=-.01)
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=.01)
axis(side = 1, at=seq(presample,epoch, by=.1), pos=0, labels=NA, tck=0)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=-.01)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=.01)
text(-120, -cal_size, "F4", cex=text_size)


##############################################################################################################
#### Rebaseline and plot F4 cond4.
############################################################################################################## 
base_mean_cond4 <- mean(cond4$F4[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond4 <- cond4$F4 - base_mean_cond4         #Re-baseline that condition
lines(cond4$time[start:stop], rebase_cond4[start:stop], type="l",lwd=line_width, lty=cond4_type, col=cond4_color)   #Plot it

################################################
#	Plot FC3 Condition 1
################################################
# Calculate the mean amplitude in the grammatical condition for the baseline interval for the electrode you're plotting and then rebaseline
#base_mean_cond1 <- mean(cond1$FC3[base1:base2])
#rebase_cond1 <- cond1$FC3 - base_mean_cond1

# Plot cond1. If your ERPs are very large, you might need to expand the values in the ylim argument above.    
par(fig=c(.13,.57,.375,.875), new=T)
base_mean_cond3 <- mean(cond3$FC3[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond3 <- cond3$FC3 - base_mean_cond3  
plot(cond3$time[start:stop], rebase_cond3[start:stop], las=1, xlab=NA, ylab=NA, xlim=c(base_begin-50, epoch), axes=FALSE, frame.plot=FALSE, type="l", lwd=line_width, lty=cond3_type,col=cond3_color, ylim=rev(c(-y_size, y_size)) )

# Generate some axes and an electrode label.
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=-.01)
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=.01)
axis(side = 1, at=seq(presample,epoch, by=.1), pos=0, labels=NA, tck=0)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=-.01)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=.01)
text(-120, -cal_size, "FC3", cex=text_size)

##############################################################################################################
#### Rebaseline and plot FC3 cond2.
############################################################################################################## 
#base_mean_cond2 <- mean(cond2$FC3[base1:base2])     #Calculate mean amplitude in baseline interval
#rebase_cond2 <- cond2$FC3 - base_mean_cond2         #Re-baseline that condition
#lines(cond2$time[start:stop], rebase_cond2[start:stop], type="l",lwd=line_width, lty=cond2_type, col=cond2_color)   #Plot it


##############################################################################################################
#### Rebaseline and plot FC3 cond3.
############################################################################################################## 
#base_mean_cond3 <- mean(cond3$FC3[base1:base2])     #Calculate mean amplitude in baseline interval
#rebase_cond3 <- cond3$FC3 - base_mean_cond3         #Re-baseline that condition
#lines(cond3$time[start:stop], rebase_cond3[start:stop], type="l",lwd=line_width, lty=cond3_type, col=cond3_color)   #Plot it

#	Clean up some variables
#rm(base_mean_cond1,rebase_cond1,base_mean_cond2,rebase_cond2)

##############################################################################################################
#### Rebaseline and plot FC3 cond4.
############################################################################################################## 
base_mean_cond4 <- mean(cond4$FC3[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond4 <- cond4$FC3 - base_mean_cond4         #Re-baseline that condition
lines(cond4$time[start:stop], rebase_cond4[start:stop], type="l",lwd=line_width, lty=cond4_type, col=cond4_color)   #Plot it


#	Clean up some variables
#rm(base_mean_cond1,rebase_cond1,base_mean_cond2,rebase_cond2,base_mean_cond3,rebase_cond3,base_mean_cond4,rebase_cond4)

################################################
#	Plot FC4 Condition 1
################################################
# Calculate the mean amplitude in the grammatical condition for the baseline interval for the electrode you're plotting and then rebaseline
#base_mean_cond1 <- mean(cond1$FC4[base1:base2])
#rebase_cond1 <- cond1$FC4 - base_mean_cond1

# Plot cond1. If your ERPs are very large, you might need to expand the values in the ylim argument above.    
par(fig=c(.41,.85,.375,.875), new=T)
base_mean_cond3 <- mean(cond3$FC4[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond3 <- cond3$FC4 - base_mean_cond3 
plot(cond3$time[start:stop], rebase_cond3[start:stop], las=1, xlab=NA, ylab=NA, xlim=c(base_begin-50, epoch), axes=FALSE, frame.plot=FALSE, type="l", lwd=line_width, lty=cond3_type,col=cond3_color, ylim=rev(c(-y_size, y_size)) )

# Generate some axes and an electrode label.
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=-.01)
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=.01)
axis(side = 1, at=seq(presample,epoch, by=.1), pos=0, labels=NA, tck=0)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=-.01)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=.01)
text(-120, -cal_size, "FC4", cex=text_size)

##############################################################################################################
#### Rebaseline and plot FC4 cond2.
############################################################################################################## 
#base_mean_cond2 <- mean(cond2$FC4[base1:base2])     #Calculate mean amplitude in baseline interval
#rebase_cond2 <- cond2$FC4 - base_mean_cond2         #Re-baseline that condition
#lines(cond2$time[start:stop], rebase_cond2[start:stop], type="l",lwd=line_width, lty=cond2_type, col=cond2_color)   #Plot it

##############################################################################################################
#### Rebaseline and plot FC4 cond3.
############################################################################################################## 
#base_mean_cond3 <- mean(cond3$FC4[base1:base2])     #Calculate mean amplitude in baseline interval
#rebase_cond3 <- cond3$FC4 - base_mean_cond3         #Re-baseline that condition
#lines(cond3$time[start:stop], rebase_cond3[start:stop], type="l",lwd=line_width, lty=cond3_type, col=cond3_color)   #Plot it

##############################################################################################################
#### Rebaseline and plot FC4 cond4.
############################################################################################################## 
base_mean_cond4 <- mean(cond4$FC4[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond4 <- cond4$FC4 - base_mean_cond4         #Re-baseline that condition
lines(cond4$time[start:stop], rebase_cond4[start:stop], type="l",lwd=line_width, lty=cond4_type, col=cond4_color)   #Plot it


################################################
#	Plot C3 Condition 1
################################################
# Calculate the mean amplitude in the grammatical condition for the baseline interval for the electrode you're plotting and then rebaseline
#base_mean_cond1 <- mean(cond1$C3[base1:base2])
#rebase_cond1 <- cond1$C3 - base_mean_cond1

# Plot cond1. If your ERPs are very large, you might need to expand the values in the ylim argument above.    
par(fig=c(0,.44,.25,.75), new=T)
base_mean_cond3 <- mean(cond3$C3[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond3 <- cond3$C3 - base_mean_cond3 
plot(cond3$time[start:stop], rebase_cond3[start:stop], las=1, xlab=NA, ylab=NA, xlim=c(base_begin-50, epoch), axes=FALSE, frame.plot=FALSE, type="l", lwd=line_width, lty=cond3_type,col=cond3_color, ylim=rev(c(-y_size, y_size)) )

# Generate some axes and an electrode label.
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=-.01)
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=.01)
axis(side = 1, at=seq(presample,epoch, by=.1), pos=0, labels=NA, tck=0)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=-.01)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=.01)
text(-120, -cal_size, "C3", cex=text_size)

##############################################################################################################
#### Rebaseline and plot C3 cond2.
############################################################################################################## 
#base_mean_cond2 <- mean(cond2$C3[base1:base2])     #Calculate mean amplitude in baseline interval
#rebase_cond2 <- cond2$C3 - base_mean_cond2         #Re-baseline that condition
#lines(cond2$time[start:stop], rebase_cond2[start:stop], type="l",lwd=line_width, lty=cond2_type, col=cond2_color)   #Plot it

##############################################################################################################
#### Rebaseline and plot C3  cond3.
############################################################################################################## 
#base_mean_cond3 <- mean(cond3$C3[base1:base2])     #Calculate mean amplitude in baseline interval
#rebase_cond3 <- cond3$C3 - base_mean_cond3         #Re-baseline that condition
#lines(cond3$time[start:stop], rebase_cond3[start:stop], type="l",lwd=line_width, lty=cond3_type, col=cond3_color)   #Plot it

##############################################################################################################
#### Rebaseline and plot C3 cond4.
############################################################################################################## 
base_mean_cond4 <- mean(cond4$C3[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond4 <- cond4$C3 - base_mean_cond4         #Re-baseline that condition
lines(cond4$time[start:stop], rebase_cond4[start:stop], type="l",lwd=line_width, lty=cond4_type, col=cond4_color)   #Plot it


################################################
#	Plot Cz Condition 1
################################################
# Calculate the mean amplitude in the grammatical condition for the baseline interval for the electrode you're plotting and then rebaseline
base_mean_cond1 <- mean(cond1$CZ[base1:base2])
#rebase_cond1 <- cond1$CZ - base_mean_cond1

# Plot cond1. If your ERPs are very large, you might need to expand the values in the ylim argument above.    
par(fig=c(.28,.72, .25,.75), new=T)
base_mean_cond3 <- mean(cond3$CZ[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond3 <- cond3$CZ - base_mean_cond3  
plot(cond3$time[start:stop], rebase_cond3[start:stop], las=1, xlab=NA, ylab=NA, xlim=c(base_begin-50, epoch), axes=FALSE, frame.plot=FALSE, type="l", lwd=line_width, lty=cond3_type,col=cond3_color, ylim=rev(c(-y_size, y_size)) )

# Generate some axes and an electrode label.
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=-.01)
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=.01)
axis(side = 1, at=seq(presample,epoch, by=.1), pos=0, labels=NA, tck=0)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=-.01)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=.01)
text(-120, -cal_size, "Cz", cex=text_size)


##############################################################################################################
#### Rebaseline and plot CZ cond4.
############################################################################################################## 
base_mean_cond4 <- mean(cond4$CZ[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond4 <- cond4$CZ- base_mean_cond4         #Re-baseline that condition
lines(cond4$time[start:stop], rebase_cond4[start:stop], type="l",lwd=line_width, lty=cond4_type, col=cond4_color)   #Plot it


################################################
#	Plot C4 Condition 1
################################################
# Calculate the mean amplitude in the grammatical condition for the baseline interval for the electrode you're plotting and then rebaseline
#base_mean_cond1 <- mean(cond1$C4[base1:base2])
#rebase_cond1 <- cond1$C4 - base_mean_cond1

# Plot cond1. If your ERPs are very large, you might need to expand the values in the ylim argument above.    
par(fig=c(.56,1,.25,.75), new=T)
base_mean_cond3 <- mean(cond3$C4[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond3 <- cond3$C4 - base_mean_cond3  
plot(cond3$time[start:stop], rebase_cond3[start:stop], las=1, xlab=NA, ylab=NA, xlim=c(base_begin-50, epoch), axes=FALSE, frame.plot=FALSE, type="l", lwd=line_width, lty=cond3_type,col=cond3_color, ylim=rev(c(-y_size, y_size)) )

# Generate some axes and an electrode label.
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=-.01)
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=.01)
axis(side = 1, at=seq(presample,epoch, by=.1), pos=0, labels=NA, tck=0)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=-.01)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=.01)
text(-120, -cal_size, "C4", cex=text_size)

##############################################################################################################
#### Rebaseline and plot C4 cond2.
############################################################################################################## 
#base_mean_cond2 <- mean(cond2$C4[base1:base2])     #Calculate mean amplitude in baseline interval
#rebase_cond2 <- cond2$C4 - base_mean_cond2         #Re-baseline that condition
#lines(cond2$time[start:stop], rebase_cond2[start:stop], type="l",lwd=line_width, lty=cond2_type, col=cond2_color)   #Plot it

##############################################################################################################
#### Rebaseline and plot C4 cond3.
############################################################################################################## 
#base_mean_cond3 <- mean(cond3$C4[base1:base2])     #Calculate mean amplitude in baseline interval
#rebase_cond3 <- cond3$C4 - base_mean_cond3         #Re-baseline that condition
#lines(cond3$time[start:stop], rebase_cond3[start:stop], type="l",lwd=line_width, lty=cond3_type, col=cond3_color)   #Plot it

##############################################################################################################
#### Rebaseline and plot C4 cond4.
############################################################################################################## 
base_mean_cond4 <- mean(cond4$C4[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond4 <- cond4$C4 - base_mean_cond4         #Re-baseline that condition
lines(cond4$time[start:stop], rebase_cond4[start:stop], type="l",lwd=line_width, lty=cond4_type, col=cond4_color)   #Plot it


################################################
#	Plot CP3 Condition 1
################################################
# Calculate the mean amplitude in the grammatical condition for the baseline interval for the electrode you're plotting and then rebaseline
#base_mean_cond1 <- mean(cond1$CP3[base1:base2])
#rebase_cond1 <- cond1$CP3 - base_mean_cond1

# Plot cond1. If your ERPs are very large, you might need to expand the values in the ylim argument above.    
par(fig=c(.13,.57,.125,.625), new=T)
base_mean_cond3 <- mean(cond3$CP3[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond3 <- cond3$CP3 - base_mean_cond3   
plot(cond3$time[start:stop], rebase_cond3[start:stop], las=1, xlab=NA, ylab=NA, xlim=c(base_begin-50, epoch), axes=FALSE, frame.plot=FALSE, type="l", lwd=line_width, lty=cond3_type,col=cond1_color, ylim=rev(c(-y_size, y_size)) )

# Generate some axes and an electrode label.
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=-.01)
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=.01)
axis(side = 1, at=seq(presample,epoch, by=.1), pos=0, labels=NA, tck=0)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=-.01)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=.01)
text(-120, -cal_size, "CP3", cex=text_size)

##############################################################################################################
#### Rebaseline and plot CP3 cond2.
############################################################################################################## 
#base_mean_cond2 <- mean(cond2$CP3[base1:base2])     #Calculate mean amplitude in baseline interval
#rebase_cond2 <- cond2$CP3 - base_mean_cond2         #Re-baseline that condition
#lines(cond2$time[start:stop], rebase_cond2[start:stop], type="l",lwd=line_width, lty=cond2_type, col=cond2_color)   #Plot it

##############################################################################################################
#### Rebaseline and plot CP3 cond3.
############################################################################################################## 
#base_mean_cond3 <- mean(cond3$CP3[base1:base2])     #Calculate mean amplitude in baseline interval
#rebase_cond3 <- cond3$CP3 - base_mean_cond3         #Re-baseline that condition
#lines(cond3$time[start:stop], rebase_cond3[start:stop], type="l",lwd=line_width, lty=cond3_type, col=cond3_color)   #Plot it

##############################################################################################################
#### Rebaseline and plot CP3 cond4.
############################################################################################################## 
base_mean_cond4 <- mean(cond4$CP3[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond4 <- cond4$CP3 - base_mean_cond4         #Re-baseline that condition
lines(cond4$time[start:stop], rebase_cond4[start:stop], type="l",lwd=line_width, lty=cond4_type, col=cond4_color)   #Plot it


################################################
#	Plot CP4 Condition 1
################################################
# Calculate the mean amplitude in the grammatical condition for the baseline interval for the electrode you're plotting and then rebaseline
#base_mean_cond1 <- mean(cond1$CP4[base1:base2])
#rebase_cond1 <- cond1$CP4 - base_mean_cond1

# Plot cond1. If your ERPs are very large, you might need to expand the values in the ylim argument above.    
par(fig=c(.41,.85, .125,.625), new=T)
base_mean_cond3 <- mean(cond3$CP4[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond3 <- cond3$CP4 - base_mean_cond3  
plot(cond3$time[start:stop], rebase_cond3[start:stop], las=1, xlab=NA, ylab=NA, xlim=c(base_begin-50, epoch), axes=FALSE, frame.plot=FALSE, type="l", lwd=line_width, lty=cond3_type,col=cond3_color, ylim=rev(c(-y_size, y_size)) )

# Generate some axes and an electrode label.
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=-.01)
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=.01)
axis(side = 1, at=seq(presample,epoch, by=.1), pos=0, labels=NA, tck=0)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=-.01)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=.01)
text(-120, -cal_size, "CP4", cex=text_size)


##############################################################################################################
#### Rebaseline and plot CP4 cond4.
############################################################################################################## 
base_mean_cond4 <- mean(cond4$CP4[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond4 <- cond4$CP4 - base_mean_cond4         #Re-baseline that condition
lines(cond4$time[start:stop], rebase_cond4[start:stop], type="l",lwd=line_width, lty=cond4_type, col=cond4_color)   #Plot it


################################################
#	Plot P3 Condition 1
################################################
# Calculate the mean amplitude in the grammatical condition for the baseline interval for the electrode you're plotting and then rebaseline
#base_mean_cond1 <- mean(cond1$P3[base1:base2])
#rebase_cond1 <- cond1$P3 - base_mean_cond1

# Plot cond1. If your ERPs are very large, you might need to expand the values in the ylim argument above.    
par(fig=c(0,.44,.0,.5), new=T)
base_mean_cond3 <- mean(cond3$P3[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond3 <- cond3$P3 - base_mean_cond3 
plot(cond3$time[start:stop], rebase_cond3[start:stop], las=1, xlab=NA, ylab=NA, xlim=c(base_begin-50, epoch), axes=FALSE, frame.plot=FALSE, type="l", lwd=line_width, lty=cond3_type,col=cond3_color, ylim=rev(c(-y_size, y_size)) )

# Generate some axes and an electrode label.
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=-.01)
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=.01)
axis(side = 1, at=seq(presample,epoch, by=.1), pos=0, labels=NA, tck=0)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=-.01)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=.01)
text(-120, -cal_size, "P3", cex=text_size)


##############################################################################################################
#### Rebaseline and plot P3 cond4.
############################################################################################################## 
base_mean_cond4 <- mean(cond4$P3[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond4 <- cond4$P3 - base_mean_cond4         #Re-baseline that condition
lines(cond4$time[start:stop], rebase_cond4[start:stop], type="l",lwd=line_width, lty=cond4_type, col=cond4_color)   #Plot it

################################################
#	Plot Pz Condition 1
################################################
# Calculate the mean amplitude in the grammatical condition for the baseline interval for the electrode you're plotting and then rebaseline
#base_mean_cond1 <- mean(cond1$PZ[base1:base2])
#rebase_cond1 <- cond1$PZ - base_mean_cond1

# Plot cond1. If your ERPs are very large, you might need to expand the values in the ylim argument above.    
par(fig=c(.28,.72,.0,.5), new=T)
base_mean_cond3 <- mean(cond3$PZ[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond3 <- cond3$PZ - base_mean_cond3 
plot(cond3$time[start:stop], rebase_cond3[start:stop], las=1, xlab=NA, ylab=NA, xlim=c(base_begin-50, epoch), axes=FALSE, frame.plot=FALSE, type="l", lwd=line_width, lty=cond3_type,col=cond3_color, ylim=rev(c(-y_size, y_size)) )

# Generate some axes and an electrode label.
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=-.01)
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=.01)
axis(side = 1, at=seq(presample,epoch, by=.1), pos=0, labels=NA, tck=0)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=-.01)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=.01)
text(-120, -cal_size, "Pz", cex=text_size)

##############################################################################################################
#### Rebaseline and plot PZ cond4.
############################################################################################################## 
base_mean_cond4 <- mean(cond4$PZ[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond4 <- cond4$PZ - base_mean_cond4         #Re-baseline that condition
lines(cond4$time[start:stop], rebase_cond4[start:stop], type="l",lwd=line_width, lty=cond4_type, col=cond4_color)   #Plot it


################################################
#	Plot P4 Condition 1
################################################
# Calculate the mean amplitude in the grammatical condition for the baseline interval for the electrode you're plotting and then rebaseline
#base_mean_cond1 <- mean(cond1$P4[base1:base2])
#rebase_cond1 <- cond1$P4 - base_mean_cond1

# Plot cond1. If your ERPs are very large, you might need to expand the values in the ylim argument above.    
par(fig=c(.56,1,.0,.5), new=T)
base_mean_cond3 <- mean(cond3$P4[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond3 <- cond3$P4 - base_mean_cond3 
plot(cond3$time[start:stop], rebase_cond3[start:stop], las=1, xlab=NA, ylab=NA, xlim=c(base_begin-50, epoch), axes=FALSE, frame.plot=FALSE, type="l", lwd=line_width, lty=cond3_type,col=cond3_color, ylim=rev(c(-y_size, y_size)) )

# Generate some axes and an electrode label.
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=-.01)
axis(side = 2, at=c(-cal_size, cal_size), pos=0, labels=NA, tck=.01)
axis(side = 1, at=seq(presample,epoch, by=.1), pos=0, labels=NA, tck=0)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=-.01)
axis(side = 1, at=seq(100*trunc(presample/100), epoch, by = 100), pos=0, labels=NA, tck=.01)
text(-120, -cal_size, "P4", cex=text_size)


##############################################################################################################
#### Rebaseline and plot P4 cond4.
############################################################################################################## 
base_mean_cond4 <- mean(cond4$P4[base1:base2])     #Calculate mean amplitude in baseline interval
rebase_cond4 <- cond4$P4 - base_mean_cond4         #Re-baseline that condition
lines(cond4$time[start:stop], rebase_cond4[start:stop], type="l",lwd=line_width, lty=cond4_type, col=cond4_color)   #Plot it


# Save plot as pdf
dev.copy(pdf, "APS2 Pronoun English dashline.pdf", width=7.5, height=9)
dev.off()


