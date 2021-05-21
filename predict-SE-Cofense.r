##########################################################################################################################################################
#Load needed libraries 
##########################################################################################################################################################

if(1==1){
	# high-quality plots
	if(!require(ggplot2)){
			install.packages("ggplot2")
	}
	# animation of k-means
	if(!require(animation)){
			install.packages("animation")
	}
	if(!require(fpc)){
			install.packages("fpc")
	}
	if(!require(e1071)){
			install.packages("e1071")
	}
	if(!require(caret)){
			install.packages("caret")
	}		
	if(!require(VIM)){
			install.packages("VIM")
			}	
	if(!require(mice)){
			install.packages("mice")
			}	
	if(!require(vioplot)){
			install.packages("vioplot")
			}	
	if(!require(datasets)){
			install.packages("datasets")
			}	
	if(!require(lubridate)){
			install.packages("lubridate")
			}	
	if(!require(stringi)){
			install.packages("stringi")
	}
	if(!require(arules)){
			install.packages("arules")
	}
	if(!require(arulesViz)){
			install.packages("arulesViz", dependencies = TRUE)
	}
	if(!require(igraph)){
			install.packages("igraph")
	}
	if(!require(visNetwork)){
			install.packages("visNetwork")
	}
	if(!require(plyr)){
			install.packages("plyr")
	}
	###   decision tree
	if(!require(C50)){
			install.packages("C50")
	}
	###   NeuroNET
	if(!require(neuralnet)){
			install.packages("neuralnet")
	}
	if(!require(caTools)){
			install.packages("caTools")
	}
	if(!require(ISLR)){
			install.packages("ISLR")
	}
	###   ROC curve
	if(!require(pROC)){
			install.packages("pROC")
	}
#	if(!require(urltools)){
#			install.packages("urltools", dependencies = TRUE)
#	}
	if(!require(tidyverse)){
			install.packages("tidyverse")
	}

	library(caTools)
	library(ISLR)
	library(neuralnet)
	library(C50)
	library(pROC)
	library(plyr)
	library(visNetwork)
	library(igraph)
	library(arulesViz)
	library(arules)
	library(stringi)
	library(lubridate)
	library(datasets)
	library(VIM)
	library(caret)
	library(vioplot)
	library(mice)
	library(ggplot2)
	library(fpc)
	library(e1071)
	library(animation) 
library(stringi)
#library(urltools)
library(dplyr)
library(purrr)		
library(tidyr)
#library(exploratory) 
}

##########################################################################################################################################################
#GET TO KNOW THE DATA
##########################################################################################################################################################
#  Set working directory 

setwd("/joined")
getwd()



raw7 <- read.csv("Wednesday-workingHours.pcap_ISCX.csv", na.strings= "0") 
#head(raw7)
#write.csv(raw_1,"stg1.csv",row.names = FALSE)

raw8 <- read.csv("Friday-02-03-2018_TrafficForML_CICFlowMeter.csv", na.strings= "0") 
#head(raw8) # notice From have na now 

#str(raw7)

#understand column names
 colnames(raw7)

#Number of rows/observations in this dataset
nrow(raw7)
nrow(raw8)
#[1] 86474



##########################################################################################################################################################
#DATA PREPROCESSING - Cleaning/scrubbing data
##########################################################################################################################################################


#-------------
#Check NA data
#-------------

#check Na data, remember i turned all zeros into na for this excersise
map(raw7, ~sum(is.na(.)))
map(raw8, ~sum(is.na(.)))

#check infinite numbers 
map(raw7, ~sum(is.infinite(.)))
map(raw8, ~sum(is.infinite(.)))


#Check percent of missing data for problem columns 
map(raw7, ~mean(is.na(.)))
map(raw8, ~mean(is.na(.)))




#check completeness for each 
mean(!complete.cases(raw7))
#[1] 1 # means there is missing data in each row given at least once 
mean(!complete.cases(raw88))





#add up records of missing data total from targeted columns
sum(is.na(raw_triageDf$From)) +   sum(is.na(raw_triageDf$Recipes.Analyst.Matched)) 
#[1] 24107, total records to be deleted out of 86474
 
#Net the loss 
nrow(raw_triageDf) - ( sum(is.na(raw_triageDf$From)) +   sum(is.na(raw_triageDf$Recipes.Analyst.Matched))  )
#[1] 62367, target rows i should have after clearing it all up, UNLESS SOME OVERLAP WITH NAs IN BOTH COLUMNS ON THE SAME ROW! 


#find percent of missing data 
mean(is.na(raw_triageDf$Recipes.Analyst.Matched)) +  mean(is.na(raw_triageDf$From))
#[1] 0.2787774 , will delete 27% of data leaving more than 70% of data still! good!
 


#md.pattern BEFORE
md.pattern(raw7) 
aggr(raw7, prop = FALSE, numbers = TRUE)

#correlation of missing data 
disofNa <- as.data.frame(abs(is.na(raw7)))
head(disofNa) 
corrNa <- disofNa[names(disofNa)] 
cor(corrNa) #DONE CHECK SCREENSHOT , no correlation! very low 



#Eliminate the unused columns/useless columns not needed for this analysis 
sum(is.na(raw_triageDf$Tags)) / nrow(raw_triageDf)

raw_triageDf$Tags <- NULL # not needed, in beta

#Eliminate rows with missing data from FROM/RECIPE columns
#nrow(raw_triageDf[complete.cases(raw_triageDf[ , c("From","Recipes.Analyst.Matched")]),]) # WORKS COOL 
#[1] 62422 , WORKS !!! 

#ALSO WORKS AND MORE MANUAL BUT CAN SEE IT STEP BY STEP 
cc=is.na(raw_triageDf$From)
m=which(cc==c("TRUE"))
nrow(as.data.frame(m)) #17132!!!! YESSSSS
raw_triageDf=raw_triageDf[-m,]
#Repeat for Recipes
#whats in recipes now
sum(is.na(raw_triageDf$Recipes.Analyst.Matched))
#[1] 6920 
cc=is.na(raw_triageDf$Recipes.Analyst.Matched)
m=which(cc==c("TRUE"))
nrow(as.data.frame(m)) #!!!! YESSSSS
raw_triageDf=raw_triageDf[-m,]

str(raw_triageDf)

nrow(raw_triageDf)
[1] 62422 , ok! 

#-------------
#Check Incomplete/Complete Cases
#-------------

#Number of Incomplete cases 
 head(raw_triageDf[!complete.cases(raw_triageDf),], n=10)  
 nrow(raw_triageDf[!complete.cases(raw_triageDf),])  # 0


#Number of Complete cases 
 head(raw_triageDf[complete.cases(raw_triageDf),],  n=10) 
 nrow(raw_triageDf[complete.cases(raw_triageDf),]) #62422



#md.pattern AFTER
md.pattern(raw_triageDf) 
str(raw_triageDf) # ok! 
aggr(raw_triageDf, prop = FALSE, numbers = TRUE) # ok! 


#Add to new easier variable 
triageDf <- raw_triageDf 


#Write the cleaned data out for backup 
write.csv(triageDf ,"RcleanedPP.csv",row.names = FALSE)
#Finished Cleaning the Data Here ###############################################






################################################################################
# DATA TRANSFORMATION 
################################################################################

#Transform/enrich data for attribute Received (Time)
if(2.0==2.0){

	#parse out Time only from the datetime fields like received,processed, and reported


	#logic for extracting/parsing time only (hh:mm:ss)
	regmatches(triageDf$Received, regexpr("\\d+\\:\\d+\\:\\d+", triageDf$Received))

	#logic for extracting/parsing time (hh:mm) only 
	regmatches(triageDf$Received, regexpr("\\d+\\:\\d+", triageDf$Received))

	#assign to variable for later
	rec_time <- regmatches(triageDf$Received, regexpr("\\d+\\:\\d+\\:\\d+", triageDf$Received))
	rec_time_hhmm <- regmatches(triageDf$Received, regexpr("\\d+\\:\\d+", triageDf$Received))
	rec_time_hh <- regmatches(triageDf$Received, regexpr("\\s\\d\\d", triageDf$Received))


	#add new column to your main data frame/dataset 
	triageDf$Received.Time <- rec_time
	triageDf$Received.Time.hhmm <- rec_time_hhmm
	triageDf$Received.Time.hh <- rec_time_hh
	triageDf$Received.Time.hh <- as.numeric(triageDf$Received.Time.hh) #change to numeric for future calculations 


	#view it 
	colnames(triageDf)
	triageDf[c(2,15)]
	head(triageDf[c(2,15)]) # good it matches 
	triageDf[c(2,15,16)]
	head(triageDf[c(2,15,16)]) # good it matches

	rec_input_times <- hour(hm(triageDf$Received.Time.hhmm))
	time_breaks <- hour(hm("00:00", "05:59", "11:59", "17:,59", "23:59"))
	time_labels <- c("Night", "Morning", "Afternoon", "Evening")
	cut(x=rec_input_times, breaks=time_breaks, labels=time_labels, include.lowest=TRUE)

	#put new labels into a variable for adding to data frame next 
	rec_time_labled <- cut(x=rec_input_times, breaks=time_breaks, labels=time_labels, include.lowest=TRUE)

	# add to original data frame/dataset 
	triageDf$Received.TOD = rec_time_labled

	#check/verify the new column
	head(triageDf[, c("Received", "Received.Time", "Received.Time.hhmm", "Received.Time.hh", "Received.TOD")],n=20) # good it matches

}


#Transform/enrich data for attribute Reported (time)
if(2.1==2.1){

	#parse out Time only from the datetime fields like Reported,processed, and reported

	#logic for extracting/parsing time only (hh:mm:ss)
	regmatches(triageDf$Reported, regexpr("\\d+\\:\\d+\\:\\d+", triageDf$Reported))

	#logic for extracting/parsing time (hh:mm) only
	regmatches(triageDf$Reported, regexpr("\\d+\\:\\d+", triageDf$Reported))


	#assign to variable for later
	rep_time <- regmatches(triageDf$Reported, regexpr("\\d+\\:\\d+\\:\\d+", triageDf$Reported))
	rep_time_hhmm <- regmatches(triageDf$Reported, regexpr("\\d+\\:\\d+", triageDf$Reported))
	rep_time_hh <- regmatches(triageDf$Reported, regexpr("\\s\\d\\d", triageDf$Reported))




	#add new column to your main data frame/dataset
	triageDf$Reported.Time <- rep_time
	triageDf$Reported.Time.hhmm <- rep_time_hhmm
	triageDf$Reported.Time.hh <- rep_time_hh
	triageDf$Reported.Time.hh <- as.numeric(triageDf$Reported.Time.hh) #change to numeric for future calculations 


	#view it
	colnames(triageDf)
	triageDf[c(2,15)]
	head(triageDf[c(2,15)]) # good it matches
	triageDf[c(2,15,16)]
	head(triageDf[c(2,15,16)]) # good it matches TOD and actual time


	#Perfrom enrichment to dataset by adding categories TOD for Report time
	rep_input_times <- hour(hm(triageDf$Reported.Time.hhmm))
	time_breaks <- hour(hm("00:00", "05:59", "11:59", "17:,59", "23:59"))
	time_labels <- c("Night", "Morning", "Afternoon", "Evening")
	cut(x=rep_input_times, breaks=time_breaks, labels=time_labels, include.lowest=TRUE)


	#put new labels into a variable for adding to data frame next
	rep_time_labled <- cut(x=rep_input_times, breaks=time_breaks, labels=time_labels, include.lowest=TRUE)


	# add to original data frame/dataset
	triageDf$Reported.TOD = rep_time_labled


	#check/verify the new column
	head(triageDf[, c("Reported", "Reported.Time", "Reported.Time.hhmm", "Reported.TOD")],n=20) # good it matches

}


#Transform/enrich data for attribute Processed (time)
if(2.1.1==2.1.1){

	#parse out Time only from the datetime fields like Processed,processed, and Processed

	#logic for extracting/parsing time only (hh:mm:ss)
	regmatches(triageDf$Processed, regexpr("\\d+\\:\\d+\\:\\d+", triageDf$Processed))

	#logic for extracting/parsing time (hh:mm) only
	regmatches(triageDf$Processed, regexpr("\\d+\\:\\d+", triageDf$Processed))


	#assign to variable for later
	proc_time <- regmatches(triageDf$Processed, regexpr("\\d+\\:\\d+\\:\\d+", triageDf$Processed))
	proc_time_hhmm <- regmatches(triageDf$Processed, regexpr("\\d+\\:\\d+", triageDf$Processed))
	proc_time_hh <- regmatches(triageDf$Processed, regexpr("\\s\\d\\d", triageDf$Processed))




	#add new column to your main data frame/dataset
	triageDf$Processed.Time <- proc_time
	triageDf$Processed.Time.hhmm <- proc_time_hhmm
	triageDf$Processed.Time.hh <- proc_time_hh
	triageDf$Processed.Time.hh <- as.numeric(triageDf$Processed.Time.hh) #change to numeric for future calculations 


	#view it
	head(triageDf[, c("Processed", "Processed.Time", "Processed.Time.hhmm")],n=20) # good it matches



	#Perfrom enrichment to dataset by adding categories TOD for procort time
	proc_input_times <- hour(hm(triageDf$Processed.Time.hhmm))
	time_breaks <- hour(hm("00:00", "05:59", "11:59", "17:,59", "23:59"))
	time_labels <- c("Night", "Morning", "Afternoon", "Evening")
	cut(x=proc_input_times, breaks=time_breaks, labels=time_labels, include.lowest=TRUE)


	#put new labels into a variable for adding to data frame next
	proc_time_labled <- cut(x=proc_input_times, breaks=time_breaks, labels=time_labels, include.lowest=TRUE)


	# add to original data frame/dataset
	triageDf$Processed.TOD = proc_time_labled


	#check/verify the new column
	head(triageDf[, c("Processed", "Processed.Time", "Processed.Time.hhmm", "Processed.TOD")],n=20) # good it matches

}


#Transform/enrich data for URL (Contains.URL)
if(2.2==2.2){
	#note: subset not good for this solution as it will pick and choose depending on logic
	# its better to use For loop with condition and feed in your binary true false table and ...
	# use if condition to make the my custom categorical value name... 
	# or just use the true false table and add that to your dataset like below 
	 	#ra <- as.data.frame(centers$Murder^2 + centers$Murder^2)        
   		#nominal <- c()
      	#for(i in (1:nrow(ra))){
            	#  nominal[i] <- i
  	
	#Perform logic that categorizes (binary simplification) 
	triageDf$Contains.Url <- triageDf$URLs >= 1
	
	#view/confirm new column visually     
	head(triageDf[, c("URLs", "Contains.Url")], n=20)

	#Done
}	



#Transform/enrich data for Attachments (Contains.Attachment), i.e yes, no
if(2.3==2.3){
	#Repeat steps from 2.2 

	#Perform logic that categorizes (binary simplification) 
	triageDf$Contains.Attachment <- triageDf$Attachments >= 1
	
	#view/confirm new column visually     
	head(triageDf[, c("Attachments", "Contains.Attachment")], n=20)

	#Done
}


#Transform/enrich data for Subject (Subject.Length), i.e 15,5,25 
#note for future, could make sentiment analysis for this later on and add to conditional probability
if(2.4==2.4){
	
	#Perform logic that counts these lengths to enrich data points
	stri_length(c(triageDf$Subject)) # requires package dependency, but works
	nchar(c(as.character(triageDf$Subject))) # built in library for counting char lengths
	
	#Add this new column to your dataset!
 	triageDf$Subject.Length <- nchar(c(as.character(triageDf$Subject)))

	#view/confirm new column visually     
	head(triageDf[, c("Subject", "Subject.Length")], n=20)
	
	#Done
}


#Transform/enrich data for Subject.Length (Subject.Size), i.e long, short
if(2.5==2.5){

	#note: subset not good for this solution as it will pick and choose depending on logic
	# its better to use For loop with condition and feed in your binary true false table and ...
	# use if condition to make the my custom categorical value name...
		#commute = c(27, 26, 30, 34, 32, 25, 31, 25, 27, 32)
		#gte30 = commute >= 30
		#gte30
		## [1] FALSE FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE  TRUE
		#commute[gte30]
		## [1] 30 34 32 31 32
		#result = length(commute[gte30])
		#result
		## [1] 5	

	#make Subject.Length into numerical 
	triageDf$Subject.Length <- as.numeric(triageDf$Subject.Length)
	
	#Try to visibly determine the proper number to cut off for short and long categories
	hist(triageDf$Subject.Length) #majority of sub lengths are between 0 to 100
	hist(triageDf$Subject.Length, breaks=50) #break help to see distrubution of data better

	mean(triageDf$Subject.Length)
	#[1] 38.17053
	#mean(triageDf$Suject.Length[complete.cases(triageDf$Subject.Length),])
	median(triageDf$Subject.Length)
	#[1] 34
	max(triageDf$Subject.Length)
	#[1] 521
	min(triageDf$Subject.Length)
	#[1] 1


	#run mode on Subject.Length 
	getmode(triageDf$Subject.Length)
	#[1] 23

	str(triageDf) #good
	#Done 

}	
	
#Transform/enrich data for attribute From, extract the email domains
if(2.6==2.6){


#Extract Email domains only  with separate from Tidyr package  
#Isolate from as a dataframe to prep for surgery 
fromDf = as.data.frame(triageDf$From)
class(fromDf)
#perform surgery now 
newFromDf <- separate(data = fromDf, col = 1, into = c("From.Name", "From.Domain"), sep = "@", remove = FALSE)
head(newFromDf, n=12) 
head(newFromDf$From.Domain, n=10) 


#spotted extra quotes at the end, deleting them 
newFromDf$From.Domain <- gsub("\\\"", "" , newFromDf$From.Domain)
head(newFromDf$From.Domain, n=12) 
#head(gsub("\\\"", "" , newFromDf$From.Domain))



#check counts due to warnings 
nrow(triageDf)
#[1] 69339
nrow(newFromDf)
#[1] 69339

#Combine the new data set with original now 
triageDf$From.Domain <-  newFromDf$From.Domain


# View it 
#check/verify the new column
	head(triageDf[, c("From", "From.Domain")],n=30) # good it matches



#Change all datatypes to needed data types for feeding to models
triageDf$From <- as.factor(triageDf$From)
#triageDf$Contains.Url <- as.integer(triageDf$Contains.Url) #convert to int
triageDf$Contains.Url <- as.factor(triageDf$Contains.Url)
#triageDf$Contains.Attachment <- as.integer(triageDf$Contains.Attachment) #convert to int
triageDf$Contains.Attachment <- as.factor(triageDf$Contains.Attachment)
triageDf$Subject.Length   <- as.integer(triageDf$Subject.Length)
triageDf$Processed.Time.hh   <- as.integer(triageDf$Processed.Time.hh)
triageDf$Processed.Time.hhmm   <- as.factor(triageDf$Processed.Time.hhmm)
triageDf$Processed.Time <- as.factor(triageDf$Processed.Time)
triageDf$Attachments   <- as.numeric(triageDf$Attachments)
triageDf$URLs <- as.numeric(triageDf$URLs)
triageDf$Reported.TOD <- as.factor(triageDf$Reported.TOD)
triageDf$Reported.Time.hh <- as.integer(triageDf$Reported.Time.hh)
triageDf$Reported.Time.hhmm <- as.factor(triageDf$Reported.Time.hhmm)
triageDf$Reported.Time <- as.factor(triageDf$Reported.Time)
triageDf$Received.TOD <- as.factor(triageDf$Received.TOD)
triageDf$Received.Time.hh <- as.integer(triageDf$Received.Time.hh)
triageDf$Received.Time.hhmm <- as.factor(triageDf$Received.Time.hhmm)
triageDf$Received.Time <- as.factor(triageDf$Received.Time)
triageDf$From <- as.factor(triageDf$From)
triageDf$From.Domain <- as.factor(triageDf$From.Domain)
triageDf$Category <- as.factor(triageDf$Category)
triageDf$VIP <- as.factor(triageDf$VIP)
triageDf$Score <- as.numeric(triageDf$Score)


}


####KEY INSIGHTS ######################################
str(triageDf)

head(summary(triageDf$Recipes.Analyst.Matched))
tail(unique(sort(triageDf$Recipes.Analyst.Match)),5)

#count num of malicious confirmed phishes
nrow(triageDf[which(triageDf$Category=="4"), ]) 
#[1] 18349

#subset of only malicious  ones 
bad_subset = triageDf[which(triageDf$Category=="4"), ]

str(bad_subset)

tail(unique(sort(bad_subset$Recipes.Analyst.Match)),5)
head(unique(sort(bad_subset$From)))

#try to extract all subject lines and perform LdA or text mining on it later




##########################################################



##################################################################################
#SUMMARY STATS ON ALL NUMERIC DATA 
##################################################################################
str(triageDf)

#for summary stats 
reported.tod = c(as.numeric(triageDf$Reported.TOD))
received.tod = c(as.numeric(triageDf$Received.TOD))
processed.tod = c(as.numeric(triageDf$Processed.TOD))
contains.attachment = c(as.numeric(triageDf$Contains.Attachment))
contains.url = c(as.numeric(triageDf$Contains.Url))
vip = c(as.numeric(triageDf$VIP))
category = c(as.numeric(triageDf$Category))

#combine for summary stats functions 
stats_var_vec = cbind(category , vip , contains.url, contains.attachment, processed.tod, received.tod, reported.tod)
stats_var_df = as.data.frame(stats_var_vec)

summary(stats_var_df)


#Summary stats on remaining numerics in original df 
var_names_stats = c("URLs","Attachments","Score","Processed.Time.hh", "Reported.Time.hh","Received.Time.hh", "Subject.Length")
var_names_stats
summary(triageDf[, var_names_stats])

	# Create Mode function.
	getmode <- function(v) {
   	uniqv <- unique(v)
   	uniqv[which.max(tabulate(match(v, uniqv)))]
	}

var_names_stats

	#run mode on all numerics 
	getmode(triageDf$Subject.Length)
	getmode(triageDf$URLs)
	getmode(triageDf$Attachments)
	getmode(triageDf$Score)
	getmode(triageDf$Processed.Time.hh)
	getmode(triageDf$Received.Time.hh)
	getmode(triageDf$Reported.Time.hh)
	getmode(reported.tod)
	getmode(received.tod)
	getmode(processed.tod)
	getmode(contains.attachment)
	getmode(contains.url)
	getmode(vip)
	getmode(category)



#Get sd() for all numerics 
	sd(triageDf$Subject.Length)
	sd(triageDf$URLs)
	sd(triageDf$Attachments)
	sd(triageDf$Score)
	sd(triageDf$Processed.Time.hh)
	sd(triageDf$Received.Time.hh)
	sd(triageDf$Reported.Time.hh)
	sd(reported.tod)
	sd(received.tod)
	sd(processed.tod)
	sd(contains.attachment)
	sd(contains.url)
	sd(vip)
	sd(category)


#correlations on numerics 
cor(triageDf[, var_names_stats])

##################################################################################
# MEASURES OF CENTRAL DENDENCIES - Regardless of category i.e mean, median, mode  
###################################################################################

#Process peak times for RECEIVED,PROCESSED,and REPORTED time/TOD
if(3.0==3.0){


#########################
#Received TOD Peak times - tells me when attacks are coming in most
#########################
#visualize Time of Day data with histogram to understand frequecy of times
hist(as.numeric(triageDf$Received.TOD),
main = "Histogram of Received.TOD Peak Times",
xlab = "Time of Day 
(1-Night,2-Morning,3-Afternoon,4-Evening)",
col = "Green")

#hist(as.numeric(triageDf$Received.Time.hh), breaks = 24) # 24 for 24 hours 
hist(as.numeric(triageDf$Received.Time.hh), breaks = 25,
main = "Histogram of Received Peak Times",
xlab = "Hour-Time",
col = "Green")

sd(triageDf$Received.Time.hh)

# Summary stats 
summary(triageDf)
write.csv(summary(triageDf),"SUMMARY1.csv",row.names = FALSE)


#Done

#########################
#Processed TOD Peak times - tells me when we analyze the most
#########################
#visualize Time of Day data with histogram to understand frequecy of times
hist(as.numeric(triageDf$Processed.TOD),
main = "Histogram of Processed.TOD Peak Times",
xlab = "Time of Day 
(1-Night,2-Morning,3-Afternoon,4-Evening)",
col = "Orange")



#hist(as.numeric(triageDf$Processed.Time.hh), breaks = 24) # 24 for 24 hours 
hist(as.numeric(triageDf$Processed.Time.hh), breaks = 25,
main = "Histogram of Processed Peak Times",
xlab = "Hour-Time",
col = "Orange")

sd(triageDf$Processed.Time.hh)

# Summary stats 
summary(triageDf)

#MORNING and AFTERNOON peak times for processing 
# also confirmed 9:00 - 12:00 and again at 15:00 - 17:00 hours are peak processed
#Done

44073 / 18349


#########################
#Reported TOD Peak times - tells me when Users notify us the most
#########################
#visualize Time of Day data with histogram to understand frequecy of times
hist(as.numeric(triageDf$Reported.TOD),
main = "Histogram of Reported.TOD Peak Times",
xlab = "Time of Day 
(1-Night,2-Morning,3-Afternoon,4-Evening)",
col = "Blue")

hist(as.numeric(triageDf$Reported.Time.hh), breaks = 25,
main = "Histogram of Reported Peak Times",
xlab = "Hour-Time",
col = "Blue") # 24 for 24 hours 

sd(triageDf$Reported.Time.hh)

# Summary stats 
summary(triageDf)

#summarize dataset 
summary(raw_triageDf)
summary(triageDf)



##################################################################################
# MEASURES OF DISPERSION - Regardless of category i.e min, max, range , sd, variance, 
###################################################################################

sd(triageDf$Received.Time.hh)
#[1] 5.082531
sd(triageDf$Reported.Time.hh)
#[1] 3.740927
sd(triageDf$Processed.Time.hh)
#[1] 5.391436

#variance shows how spread out the data distrobution is
var(triageDf$Received.Time.hh)
#[1] 25.83212
var(triageDf$Reported.Time.hh)
#[1] 13.99453
var(triageDf$Processed.Time.hh)
#[1] 29.06758

trash = data.frame(triageDf$Processed.Time.hh,triageDf$Reported.Time.hh,triageDf$Received.Time.hh)
trash


cor(trash)
#No correlations found with time feilds 


cor(as.numeric(triageDf$Contains.Url), as.numeric(triageDf$Contains.Attachment))
summary(triageDf) # provides both Central and dispersion measures
str(triageDf)


}







##################################################################################
# PROCESS DATA - PREDICTION MODEL 
###################################################################################


#Check if data has missing values

#Check class label structure
levels(triageDf$Category)

#distribution of types
summary(as.factor(triageDf$Category)) #needs to be factor like lab


#===================
#SUBSTEP -Build out sample supervised training sample dataset and testing data sets
#===================

#Obtain number of records that make up 80% of total observations
sample_size <- floor(0.70 * nrow(triageDf))
sample_size
#[1] 49937


#randomly draw training data by grabbing random indexes , replace = false so that once a number is picked it cannot be picked again 
training_index <- sample(nrow(triageDf), size = sample_size, replace = FALSE)
training_index


#Supply the randomly drawn training indexes into a indexer "aka list in python" to extract out the values and place them into a variable
train <- triageDf[training_index,]
head(train)
str(train)

#Do the same for testing dataset using the minus syntax
test <- triageDf[-training_index,]
nrow(test)
#[1] 12485


#correlation to prove none are correlated, and thus proceed to NB prediction model 
#check correlation first 
vip = c(as.numeric(triageDf$VIP)) 
 score = c(as.numeric(triageDf$Score)) 
 received.tod = c(as.numeric(triageDf$Received.TOD)) 
  received.time.hh = c(as.numeric(triageDf$Received.Time.hh)) 
  recipes = c(as.numeric(triageDf$Recipes.Analyst.Matched)) 
  subject = c(as.numeric(triageDf$Subject.Length)) 
  contains.attach = c(as.numeric(triageDf$Contains.Attachment)) 
  contains.url = c(as.numeric(triageDf$Contains.Url)) 
  attachments = c(as.numeric(triageDf$Attachments))         
urls = c(as.numeric(triageDf$URLs))

model_cor_set = cbind(vip,score,received.tod,received.time.hh,recipes,subject,urls,contains.attach,contains.url, attachments)
str(as.data.frame(model_cor_set))
cor(model_cor_set)
#cor done 


#names of variables that I want used in Naive Bayes prediction
var_names <- c("VIP", "Score", "Received.TOD","Recipes.Analyst.Matched", "Subject.Length","Contains.Attachment","Contains.Url","Attachments","URLs","From.Domain")

#var_names <- names(triageDf)
var_names # 11 of 29 variables choosen all are not correlated for nb to work  
str(triageDf)
nrow(train[complete.cases(train),])
#[1] [1] 49935 matches sample size total so no missing data


str(train)
str(test)
head(train)







#__________________________________________
#DECISION TREE 
#__________________________________________


#prep data for naive model by coverting data to numerical
dt <- C5.0(x = train[, var_names], y = as.factor(train$Category)) #na.action = na.pass
#dt <- C5.0(x = train[, var_names], y = as.factor(train$Category),na.action = na.pass)

summary(dt)

class(tempNum)
#[1] "C5.0"


plot(dt) # too big 


###### PREDICTION TESTING HERE ##################
dt_pred <- predict(dt, newdata = test)
###   merger dt_prediction value to test dataset
dt_evaluation <- cbind(test, dt_pred)
#dt_evaluation # output of true false table

### compare dt_prediction result to actual value
dt_evaluation$correct <- ifelse(dt_evaluation$Category == dt_evaluation$dt_pred, 1, 0)
#dt_evaluation$correct # output of 0,1 table 

###    accuracy rate
sum(dt_evaluation$correct) / nrow(dt_evaluation)
#[1] 0.9413696

###   confusion matrix
table(dt_evaluation$Category, dt_evaluation$dt_pred)# dnn = c(TRUE, FALSE))



#---------------------------------------------
#        dt_precision and dt_recall            
#---------------------------------------------
###   True Positive Rate (Sensitivity)  TPR = TP / P
###   = count of true positive dt_prediction divided by total positive truth
TPR <- sum(dt_evaluation$dt_pred == 4 & dt_evaluation$Category== 4) / sum(dt_evaluation$Category == 4)
TPR 

###   True Negative Rate (Specificity)  TNR = TN / N
###   = count of true negative dt_prediction divided by total negative truth
TNR <- sum(dt_evaluation$dt_pred == 2 & dt_evaluation$Category== 2) / sum(dt_evaluation$Category == 2)
TNR 

###   False Positive Rate  (1 - Specificity)  FPR = FP / N
###   = count of false positive dt_prediction divided by total negative truth
###   = sum(dt_evaluation$dt_pred == 4& dt_evaluation$Category == 2) / sum(dt_evaluation$Category == 2)
FPR <- 1 - TNR 
FPR 

###   False Negative Rate FNR (1 - Sensitivity)  FNR = FN / P
###   = count of false negative dt_prediction divided by total positive truth
###   = sum(dt_evaluation$dt_pred == 2& dt_evaluation$Category == 4) / sum(dt_evaluation$Category == 4)
FNR <- 1 - TPR
FNR 

###   dt_precision equals 
###   = number of true positive dt_prediction  / total positive dt_prediction

dt_precision <- sum(dt_evaluation$Category == 4 & dt_evaluation$dt_pred == 4) / sum(dt_evaluation$dt_pred == 4)
dt_precision

###   dt_recall equals = TPR
###   = true positive dt_prediction / total true positive

dt_recall <- sum(dt_evaluation$Category == 4 & dt_evaluation$dt_pred == 4) / sum(dt_evaluation$Category == 4)
dt_recall

###  F score
F <- 2 * dt_precision * dt_recall / (dt_precision + dt_recall)
F


}





#__________________________________________
#NAIVE BAYES 
#__________________________________________


#   take all explanatory variables to predict
triageDf.model <- naiveBayes(Category ~ . , data = train)

#details of model explain conditional probability
triageDf.model


class(triageDf.model)
#training is done 

#Convert all Chars to factors for test cases to feed the predictive model, chars done work 


#NEXT STEP IS TO TEST THE TRAINED ALGORITHM 
#predict() method returns a vector of result
#Now run prediction model 
nb_pred <- predict(triageDf.model, newdata =  test)



#-------------------------------
# TRUTH TABLE - Naive Bayes
#-------------------------------

#pick actual value and predicted value together in a dataframe called results
nb_results <- data.frame(actual = test[,'Category'], predicted = nb_pred)
table(nb_results)


#OR 


# Another way of getting the actuals and predictions 
###   merger dt_prediction value to test dataset
nb_evaluation <- cbind(test, nb_pred)
nb_evaluation # output of true false table

### compare dt_prediction result to actual value
nb_evaluation$correct <- ifelse(nb_evaluation$Category == nb_evaluation$nb_pred, 1, 0)
nb_evaluation$correct # output of 0,1 table
class(nb_evaluation)

###   confusion matrix
table(nb_evaluation$Category, nb_evaluation$nb_pred) 


#-------------------------------
#MODEL EVALUAION  - Naive Bayes
#-------------------------------
###    accuracy rate
sum(nb_evaluation$correct) / nrow(nb_evaluation)


###    accuracy rate
sum(nb_evaluation$correct) / nrow(nb_evaluation)

#---------------------------------------------
#        dt_precision and dt_recall            
#---------------------------------------------
###   True Positive Rate (Sensitivity)  TPR = TP / P
###   = count of true positive nb_prediction divided by total positive truth
TPR <- sum(nb_evaluation$nb_pred == 4 & nb_evaluation$Category== 4) / sum(nb_evaluation$Category == 4)
TPR 


###   True Negative Rate (Specificity)  TNR = TN / N
###   = count of true negative nb_prediction divided by total negative truth
TNR <- sum(nb_evaluation$nb_pred == 2 & nb_evaluation$Category== 2) / sum(nb_evaluation$Category == 2)
TNR 


###   False Positive Rate  (1 - Specificity)  FPR = FP / N
###   = count of false positive nb_prediction divided by total negative truth
###   = sum(nb_evaluation$nb_pred == 4& nb_evaluation$Category == 2) / sum(nb_evaluation$Category == 2)
FPR <- 1 - TNR 
FPR 


###   False Negative Rate FNR (1 - Sensitivity)  FNR = FN / P
###   = count of false negative nb_prediction divided by total positive truth
###   = sum(nb_evaluation$nb_pred == 2& nb_evaluation$Category == 4) / sum(nb_evaluation$Category == 4)
FNR <- 1 - TPR
FNR 


###   dt_precision equals 
###   = number of true positive nb_prediction  / total positive nb_prediction

dt_precision <- sum(nb_evaluation$Category == 4 & nb_evaluation$nb_pred == 4) / sum(nb_evaluation$nb_pred == 4)
dt_precision


###   dt_recall equals = TPR
###   = true positive nb_prediction / total true positive

dt_recall <- sum(nb_evaluation$Category == 4 & nb_evaluation$nb_pred == 4) / sum(nb_evaluation$Category == 4)
dt_recall


###  F score
F <- 2 * dt_precision * dt_recall / (dt_precision + dt_recall)
F




#__________________________________________
#Neuro Network
#__________________________________________

#Prep new dataset with all numerics 
head(triageDf$Category)
head(category)
category = as.numeric(triageDf$Category)-1

triage = cbind(category,as.data.frame(model_cor_set))

class(triage)
str(triage)
head(triage)
write.csv(triage,"please let this be complete.csv",row.names = FALSE)


summary(triage)
sum(is.na(triage))




colnames(triage)
# Create Vector of Column Max and Min Values
maxs <- apply(triage[,2:11], 2, max)
mins <- apply(triage[,2:11], 2, min) 

head(maxs)
head(mins)

# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(triage[,2:11], center = mins, scale = maxs - mins))

write.csv(scaled.data,"CHECK SCALE CONVERSION.csv",row.names = FALSE)

class(scaled.data)
# Check out results
print(head(scaled.data,2))


# Convert Private column from Yes/No to 1/0
data = cbind(category,scaled.data)
write.csv(data,"CHECK data final.csv",row.names = FALSE)

nrow(data[1:5000,]) 

set.seed(101)

# Create Split (any column is fine)
split = sample.split(category, SplitRatio = 0.70)

# Split based off of split Boolean Vector
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

head(train, n = 100)


feats <- names(scaled.data)

# Concatenate strings
f <- paste(feats,collapse=' + ')
f

f <- paste('category ~', f)

# Convert to formula
f <- as.formula(f)
f

nn <- neuralnet(f,train[1:5000,],hidden = c(5), stepmax=1e6,  linear.output = TRUE)

class(nn)



# Compute Predictions off Test Set
#predicted.nn.values <- compute(nn,test[2:11])
predicted.nn.values <- neuralnet::compute(nn,test[2:11])

# Check out net.result
print(head(predicted.nn.values$net.result))


#round
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits = 0)

###   confusion matrix
table(test$category,predicted.nn.values$net.result)

plot(nn)



#-------------------------------
#Prep for Evaluation - nn
#-------------------------------
table(test$category,predicted.nn.values$net.result)
nn_evaluation <- cbind(test$category,predicted.nn.values$net.result)
head(nn_evaluation, n = 15)

class(test) # df 
class(predicted.nn.values) # list needs to be df 

### compare dt_prediction result to actual value
nn_evaluation$correct <- ifelse(test$category == as.data.frame(predicted.nn.values$net.result), 1, 0)
nn_evaluation$correct # output of 0,1 table
class(nn_evaluation)




#-------------------------------
#MODEL EVALUAION  - nn
#-------------------------------
###    accuracy rate
sum(nn_evaluation$correct) / nrow(nn_evaluation)



#---------------------------------------------
#        nn_precision and nn_recall            
#---------------------------------------------
###   True Positive Rate (Sensitivity)  TPR = TP / P
###   = count of true positive nn_prediction divided by total positive truth
TPR <- sum(nn_evaluation$nn_pred == 1 & nn_evaluation$category== 1) / sum(nn_evaluation$category == 1)
TPR 


###   True Negative Rate (Specificity)  TNR = TN / N
###   = count of true negative nn_prediction divided by total negative truth
TNR <- sum(nn_evaluation$nn_pred == 0 & nn_evaluation$category== 0) / sum(nn_evaluation$category == 0)
TNR 


###   False Positive Rate  (1 - Specificity)  FPR = FP / N
###   = count of false positive nn_prediction divided by total negative truth
###   = sum(nn_evaluation$nn_pred == 1& nn_evaluation$category == 0) / sum(nn_evaluation$category == 0)
FPR <- 1 - TNR 
FPR 


###   False Negative Rate FNR (1 - Sensitivity)  FNR = FN / P
###   = count of false negative nn_prediction divided by total positive truth
###   = sum(nn_evaluation$nn_pred == 0& nn_evaluation$category == 1) / sum(nn_evaluation$category == 1)
FNR <- 1 - TPR
FNR 


###   nn_precision equals 
###   = number of true positive nn_prediction  / total positive nn_prediction

nn_precision <- sum(nn_evaluation$category == 1 & nn_evaluation$nn_pred == 1) / sum(nn_evaluation$nn_pred == 1)
nn_precision


###   nn_recall equals = TPR
###   = true positive nn_prediction / total true positive

nn_recall <- sum(nn_evaluation$category == 1 & nn_evaluation$nn_pred == 1) / sum(nn_evaluation$category == 1)
nn_recall


###  F score
F <- 0 * nn_precision * nn_recall / (nn_precision + nn_recall)
F



#save this for deeper sections below (subset info) 
	phishing_subset <- head(triageDf[triageDf[, "Category"] == 4,])




