#############################################################################
#############################################################################
# Title: Data analysis for "Cognitive Load and Its Effects on Conformist Social Learning"
# Authors: Lukas von Flüe, Sonja Vogt, and Charles Efferson
# Methods: This is a lab study conducted in collaboration with the Busara Center 
# for Behavioral Economics in Nairobi, Kenya (\url{https://busaracenter.org/about-us/}).

#############################################################################
# Several parts of the following code were adopted from analysis made by 
# Aysha Bellamy in a similar study for the paper "What is the extent of a 
# frequency-dependent social learning strategy space?" by Aysha Bellamy, 
# Ryan McKay, Sonja Vogt, and Charles Efferson (2021). The analysis of that 
# paper is available on: https://osf.io/t5d8h/ 
#############################################################################

###############################################
###############################################
rm(list=ls())
###############################################
###############################################

# Package dependencies:

# Install packages if necessary

if (!requireNamespace("pwr", quietly = TRUE)) {
  install.packages("pwr")
}

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

if (!requireNamespace("gtsummary", quietly = TRUE)) {
  install.packages("gtsummary")
}

if (!requireNamespace("jtools", quietly = TRUE)) {
  install.packages("jtools")
}

if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}

if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}

if (!requireNamespace("sandwich", quietly = TRUE)) {
  install.packages("sandwich")
}

if (!requireNamespace("lmtest", quietly = TRUE)) {
  install.packages("lmtest")
}

if (!requireNamespace("scales", quietly = TRUE)) {
  install.packages("scales")
}

if (!requireNamespace("broom", quietly = TRUE)) {
  install.packages("broom")
}


if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}

# Load packages
library(tidyverse)
library(dplyr)
library(gtsummary)
library(readxl)
library(MASS)
library(jtools)
library(sandwich)
library(lmtest)
library(scales)
library(broom)
library(pwr)
library(car)

###############################################
###############################################
###############################################

# Custom label function to format x-axis labels in the odds ratio plots 
# (handling missing or non-numeric values and rounding values)

custom_decimal_labels <- function(x) {
  sapply(x, function(val) {
    if (is.na(val) || !is.numeric(val)) {
      return(NA)
    } else if (val %% 1 == 0) {
      return(format(val, nsmall = 0))
    } else if (val %% 0.01 == 0) {
      return(format(val, nsmall = 2))
    } else if (val == 0.0001) {
      return(format(val, nsmall = 4))
    } else {
      return(format(round(val, 1), nsmall = 1))
    }
  })
}

###############################################
###############################################
###############################################

###############################################
########## Experiment Parameters
###############################################

total_number_rounds <- 80

number_blocks <- 20

number_rounds_per_block <- 4

number_ind_learn <- 10

number_ind_learn_per_group <- 5

###############################################
# Read in data
###############################################

# In contrast to Zurich, Busara did not register data frames cumulatively.
# Instead, they sent data frame for each session separately.

session1data = as.data.frame(read.csv("busara_session1.csv"))

session2data = as.data.frame(read.csv("busara_session2.csv"))

session3data = as.data.frame(read.csv("busara_session3.csv"))

session4data = as.data.frame(read.csv("busara_session4.csv"))

session5data = as.data.frame(read.csv("busara_session5.csv"))

session6data = as.data.frame(read.csv("busara_session6.csv"))

session7data = as.data.frame(read.csv("busara_session7.csv"))

session8data = as.data.frame(read.csv("busara_session8.csv"))

session9data = as.data.frame(read.csv("busara_session9.csv"))

session10data = as.data.frame(read.csv("busara_session10.csv"))


df_original <- rbind(session1data,session2data,session3data,session4data,session5data,session6data,session7data,session8data,session9data,session10data)

# df_original = read.csv("busara.csv")

options(scipen = 999)

###############################################

###############################################

# rename following columns to avoid issues due to "." in variable name

df_original <- df_original %>%
  rename("session_code" = "session.code")

df_original <- df_original %>%
  rename("participant_code" = "participant.code")

###############################################

###############################################
# add column with block ID:
###############################################

unique_sessions <- as.list(unique(df_original$session_code))

df_original$blockID <- rep(0,nrow(df_original))

blockID_numbers <- 1:20

for (i in 1:length(unique_sessions)) {
  
  temp_df <- subset(df_original, df_original$session_code==unique_sessions[i])
  
  nrow_four_rounds_times_number_participants <- number_rounds_per_block*length(unique(temp_df$participant_code))
  
  df_original[df_original$session_code==unique_sessions[i],"blockID"] <- rep(blockID_numbers, each = nrow_four_rounds_times_number_participants)
  
}

###############################################
# Create a unique trial ID #
###############################################
# Note, trialID identifies individual rounds but across all sessions.

df_original$trialID <- rep(NA,nrow(df_original))
sessionsPresent <- as.list(unique(df_original$session_code))
idx <- 1
for (i in 1:length(sessionsPresent))
{
  periods_per_session <- 1:total_number_rounds
  for (j in 1:length(periods_per_session))
  {
    #Creates a trial ID by the number of unique periods per session.
    df_original$trialID[df_original$session_code == sessionsPresent[i] & df_original$round_number == periods_per_session[j]] <- idx
    idx <- idx + 1
  }
}

#Find the number of periods overall. -> we did 8 sessions and every session had 80 rounds 
#(only individual learners did choices in all 80 rounds, while social learners only chose between urns every fourth round)

noTrial <- length(unique(df_original$trialID))

################ The following predictor will be used for the "Pure Behaviour" Analysis/Regression -> see pre-analysis plan

# Create a predictor: The centered proportion of demonstrators who chose right urn in each block.
# Note, centered prop. of demonstrators choosing RIGHT urn is what we wrote in pre-analysis plan. However, in original data frame, we registered
# number of triangle/square demonstrators choosing LEFT urn. Hence, first create arrays with number of triangle/square choosing RIGHT urn:

number_right_urn_triangle <- 5 - df_original$number_left_urn_triangle

number_right_urn_square <- 5 - df_original$number_left_urn_square

df_original$number_right_urn_triangle <- number_right_urn_triangle

df_original$number_right_urn_square <- number_right_urn_square

df_original$propTriangleRightUrnCentered <- (df_original$number_right_urn_triangle / number_ind_learn_per_group) - 0.5

df_original$propSquareRightUrnCentered <- (df_original$number_right_urn_square / number_ind_learn_per_group) - 0.5

# Fill up all control variables; age, gender, field of subject values for all participants
# Values were only collected in round 80 -> have to copy those values for round 4 because I will later mainly work with 4th round data

# Age
session_code_list <- as.list(unique(df_original$session_code))

for(i in 1:length(session_code_list)){
  
  age_per_session <- df_original$age[df_original$round_number==80 & df_original$session_code==session_code_list[i]]
  
  df_original$age[(df_original$round_number %% number_rounds_per_block == 0) & df_original$session_code==session_code_list[i]] <- age_per_session
  
}

# Gender

session_code_list <- as.list(unique(df_original$session_code))

for(i in 1:length(session_code_list)){
  
  gender_per_session <- df_original$gender[df_original$round_number==80 & df_original$session_code==session_code_list[i]]
  
  df_original$gender[(df_original$round_number %% number_rounds_per_block == 0) & df_original$session_code==session_code_list[i]] <- gender_per_session
  
}

# study_subject

session_code_list <- as.list(unique(df_original$session_code))

for(i in 1:length(session_code_list)){
  
  study_subject_per_session <- df_original$study_subject[df_original$round_number==80 & df_original$session_code==session_code_list[i]]
  
  df_original$study_subject[(df_original$round_number %% number_rounds_per_block == 0) & df_original$session_code==session_code_list[i]] <- study_subject_per_session
  
}

#####################################################################
#####################################################################
################ Create data frame with relevant variables to analyse
#####################################################################
#####################################################################

# New way of creating the data frame df

df <- data.frame(session_code = df_original$session_code)
df$participant_ID <- df_original$participant_code
df$round_number <- df_original$round_number
df$blockID <- df_original$blockID
df$trialID <- df_original$trialID
df$id_in_group <- df_original$id_in_group
df$type_assignment <- df_original$type_assignment
df$group_assignment <- df_original$group_assignment
df$cognitive_load <- df_original$cognitive_load
df$similar_ingroup <- df_original$similar_ingroup
df$observing_ingroup <- df_original$observing_ingroup
df$number_left_urn_triangle <- df_original$number_left_urn_triangle
df$number_left_urn_square <- df_original$number_left_urn_square
df$red_majority <- df_original$red_majority
df$color_drawn_marble <- df_original$color_drawn_marble
df$share_winning_color <- df_original$share_winning_color
df$memorized_correctly <- df_original$memorized_correctly
df$payoff <- df_original$payoff
df$urn_choice <- df_original$urn_choice
df$optimal_urn <- df_original$optimal_urn
df$propTriangleRightUrnCentered <- df_original$propTriangleRightUrnCentered
df$propSquareRightUrnCentered <- df_original$propSquareRightUrnCentered
df <- df %>%
  mutate(chosen_correctly = if_else(urn_choice == optimal_urn, 1, 0))
df$age <- df_original$age
df$gender <- df_original$gender
df$study_subject <- df_original$study_subject
df$treatment_assignment <- df_original$treatment_assignment

# Questionnaire:

df$q1 <- df_original$q1
df$q2 <- df_original$q2
df$q3 <- df_original$q3
df$q4nocl <- df_original$q4nocl
df$q4 <- df_original$q4
df$q5nocl <- df_original$q5nocl
df$q5 <- df_original$q5
df$q6 <- df_original$q6
df$q7 <- df_original$q7
df$q8 <- df_original$q8

#####################################################################
#####################################################################
######### some summary statistics of participants
#####################################################################
#####################################################################

participant_code_list <- as.list(unique(df_original$participant_code))

unique_data_set <- subset(df, df$round_number==4)

unique_data_set <- subset(unique_data_set, unique_data_set$participant_ID==participant_code_list)

sum(unique_data_set$gender=="Man")
sum(unique_data_set$gender=="Woman")
sum(unique_data_set$gender=="Other")

sorted_age <- sort(unique_data_set$age)

# Earnings

earning_data <- subset(df_original, df_original$round_number==80)

earnings <- earning_data$monetary_earnings

min(earnings)
max(earnings)
mean(earnings)

# Summary age and gender

mean(unique_data_set$age)
sd(unique_data_set$age)

#####################################################################
#####################################################################
######### Fill up values
#####################################################################
#####################################################################

# Fill up values such as treatment values, etc. for all rounds in data frame and
# for all participants because certain values were only assigned and registered 
# in first round by the oTree program. We simply retrieve those values 
# registered in the first round and copy paste them for all the rest of rounds

#### Create participant IDs

for(i in 1:length(participant_code_list)){
  
  df$type_assignment[df$participant_ID==participant_code_list[i]][1:length(df$type_assignment[df$participant_ID==participant_code_list[i]])] <- df$type_assignment[df$participant_ID==participant_code_list[i]][1]
  
}

#### Group_assignment

for(i in 1:length(participant_code_list)){
  
  df$group_assignment[df$participant_ID==participant_code_list[i]][1:length(df$group_assignment[df$participant_ID==participant_code_list[i]])] <- df$group_assignment[df$participant_ID==participant_code_list[i]][1]
  
}

#### treatment_assignment

for(i in 1:length(participant_code_list)){
  
  df$treatment_assignment[df$participant_ID==participant_code_list[i]][1:length(df$treatment_assignment[df$participant_ID==participant_code_list[i]])] <- df$treatment_assignment[df$participant_ID==participant_code_list[i]][1]
  
}


# Extract all social learner data

df_soc_learn <- subset(df, df$type_assignment=='social_learner')

# Cognitive load treatment values were assigned only in round 1 -> have to copy those values for round 4

session_code_list <- as.list(unique(df_soc_learn$session_code))

for(i in 1:length(session_code_list)){
  
  cl_per_session <- df_soc_learn$cognitive_load[df_soc_learn$round_number==1 & df_soc_learn$session_code==session_code_list[i]]
  
  df_soc_learn$cognitive_load[(df_soc_learn$round_number %% number_rounds_per_block == 0) & df_soc_learn$session_code==session_code_list[i]] <- cl_per_session
  
}

# Extract round 4 data

df_soc_learn_4round_busara <- subset(df_soc_learn, round_number %% number_rounds_per_block == 0)

nrow(df_soc_learn_4round_busara)

length(unique(df_soc_learn_4round_busara$participant_ID))


###############################################################################
###############################################################################
########################### ANALYSIS INDIVIDUAL LEARNER #######################
###############################################################################
###############################################################################

df_ind_learn <- subset(df, type_assignment == "individual_learner")

###############################################
# Make dummies for each period #
###############################################
#Set up dummies to record the periods within the block, and the final period.

df_ind_learn$finalPeriodDummy <- rep(0,nrow(df_ind_learn))
df_ind_learn$periodDummy <- rep(NA,nrow(df_ind_learn))

#Calculate the final period of the blocks.

firstBlock <- df_ind_learn[df_ind_learn$blockID == 1, ] # create array with only first block data
uniquePeriod <- unique(firstBlock[, "round_number"]) # define all unique periods.
finalPeriod <- max(uniquePeriod) # this is the final period- all final periods should be divisable by this number.

#Uses the mod by final period function to calculate whether each trial is the first, second, third or final period within a block.

for (i in 1:nrow(df_ind_learn)) 
{
  thisRow <- df_ind_learn[i, ]
  if (thisRow$round_number %% finalPeriod == 0){
    df_ind_learn$finalPeriodDummy[i] <- 1 #periods divisible by 4 are the final period of a block.
    df_ind_learn$periodDummy[i] <- 4
  }
  else if (thisRow$round_number %% finalPeriod == 1){
    df_ind_learn$periodDummy[i] <- 1 #those with a 1 remainder would be the first period of a block.
  }
  else if (thisRow$round_number %% finalPeriod == 2){
    df_ind_learn$periodDummy[i] <- 2 #second period of the block.
  }
  else {
    df_ind_learn$periodDummy[i] <- 3 #else third period of the block.
  }
}

###############################################
# Extract round 4 data
###############################################

df_ind_learn_4round <- subset(df_ind_learn, periodDummy == 4)

# Do logit regression for optimal behaviour (choosing optimal urn)
indFinal <- glm(chosen_correctly ~ finalPeriodDummy + optimal_urn, data = df_ind_learn, family = binomial(link = 'logit'))

# summary stats of this regression "indFinal"
summary(indFinal) 

# Results suggest similar bias as Aysha reported in her paper table 2 for coordination game. 
# In particular, it shows a bias for the right urn because "optimal_urn" = 1 means that the right urn was optimal.

# However, I haven't yet used robust standard errors clustered on the demonstrator

indFinalRobust <- coeftest(indFinal,vcov = vcovCL, cluster = df_ind_learn$participant_ID)
indFinalRobust # now, bias doesn't exist anymore
indFinalRobustCapture <- capture.output(indFinalRobust) # save output as txt file.
cat("did IL choose optimally in final periods", indFinalRobustCapture, file = "opt_dem_busara.txt", sep = "n", fill = TRUE, append = TRUE)

# Produce table with confidence interval

indFinalRobustCaptureCI <- tidy(indFinalRobust, conf.int = 0.95)

indFinalRobustCaptureCI$estimate <- sprintf("%.3f", indFinalRobustCaptureCI$estimate)
indFinalRobustCaptureCI$conf.low <- sprintf("%.3f", indFinalRobustCaptureCI$conf.low)
indFinalRobustCaptureCI$conf.high <- sprintf("%.3f", indFinalRobustCaptureCI$conf.high)

indFinalRobustCaptureCI

###############################################
# Optimal behavior of demonstrators
###############################################

# This section calculates the proportion of demonstrators choosing the optimal 
# urn, and performs a t-test to check that their trial-and-error was better than 
# chance performance.

# Counts the number of demonstrators who answer optimally in each block.
uniqueBlockPresent <- unique(df[, "blockID"])
meanOptByBlock <- rep(0,length(uniqueBlockPresent))

#Calculate the average number of demonstrators choosing optimally in each block.
for (i in 1:length(uniqueBlockPresent))
{
  thisBlock <- uniqueBlockPresent[i]
  blockData <- df_ind_learn[df_ind_learn$blockID == thisBlock, ]
  finalPeriodData <- blockData[blockData$round_number %% 4 == 0, ]
  meanOptByBlock[i] <- mean(finalPeriodData$chosen_correctly)
}

#Now that we know the average performance across all blocks, we can find the best performance and the worst performance overall, as well as how many choose optimally on average
worstBlock <- min(meanOptByBlock) #the ppt who did the worst
bestBlock <- max(meanOptByBlock) #the ppt who did the best
meanBlock <- mean(meanOptByBlock) #the average performance of the ppt

#Similarly, we could do the following to calculate the frequency of demonstrators choosing optimally in the average block:

overall_freq_chosen_correctly <- (sum(df_ind_learn_4round$chosen_correctly==1))/nrow(df_ind_learn_4round)

overall_freq_chosen_correctly # = 78 %

# Use a one-sample t-test to calculate whether the average percentage of 
# demonstrators who answer optimally significantly exceeds the 50% of the group 
# who would answer optimally by chance.

# The demonstrators should perform significantly better than chance if they are 
# learning via their own trial-and-error.
tChance <- t.test(meanOptByBlock, mu = 50, alternative = "two.sided") # For coordination game 
tChance

# Whilst checking how the demonstrators learn, check whether left urn 
# (optimal_urn = 0) is more likely to be optimal in the later blocks to explain 
# the significant control predictor which shows up when not using robust 
# standard errors clustered on demonstrator (c.f. "indFinal" glm regression run above).
# This is basically a control check to see whether my randomisation has worked.
# Note, "optimal_urn" = 0 means left urn is optimal, and "optimal_urn" = 1 means 
# right urn is optimal.

optimal_urn_left <- glm(optimal_urn ~ blockID, data = df_ind_learn, family = binomial(link = 'logit'))
summary(optimal_urn_left) # Indeed, there is no correlation between optimal urn being the right urn and block numbers.

#################################################################################
#################################################################################
# Some more ind. learn. analysis, now more focused on whether majority chose opt.
#################################################################################
#################################################################################

"Frequency of rounds, out of 20 because individual learner had 20 times a fourth
round to play, in which a majority chose the optimal urn per group, i.e.
triangle/square:"

"Efferson et al. 2016 write; 'Of the five demonstrators in a group, a majority 
chose the demonstrator optimum in the final period of a block in 95.5% of all 
blocks.'"

"Look at groups, i.e. triangle/square, separately:"

"Triangle"

df_ind_learn_4round_triangle <- df_ind_learn_4round[df_ind_learn_4round$group_assignment=="triangle",]

aggregate_chosen_correctly_triangle <- aggregate(chosen_correctly ~ trialID, data=df_ind_learn_4round_triangle, sum)

freq_chosen_correctly_per_round_triangle <- aggregate_chosen_correctly_triangle[,2]

majority_optimal_triangle <- ifelse(freq_chosen_correctly_per_round_triangle >= 3, 1, 0)

freq_optimal_urn_triangle <- (sum(majority_optimal_triangle))/(number_blocks*length(unique(df_ind_learn_4round_triangle$session_code)))

freq_optimal_urn_triangle # = 0.725

"Square"

df_ind_learn_4round_square <- df_ind_learn_4round[df_ind_learn_4round$group_assignment=="square",]

aggregate_chosen_correctly_square <- aggregate(chosen_correctly ~ trialID, data=df_ind_learn_4round_square, sum)

freq_chosen_correctly_per_round_square <- aggregate_chosen_correctly_square[,2]

majority_optimal_square <- ifelse(freq_chosen_correctly_per_round_square >= 3, 1, 0)

freq_optimal_urn_square <- (sum(majority_optimal_square))/(number_blocks*length(unique(df_ind_learn_4round_triangle$session_code)))

freq_optimal_urn_square # = 0.68

"Overall frequency of a majority choosing optimal urn is %:"

overall_frequency_chosen_optimal <- (freq_optimal_urn_triangle+freq_optimal_urn_square)/2

overall_frequency_chosen_optimal # = 0.7025

###############################################################################
###############################################################################
############################# ANALYSIS SOCIAL LEARNER #########################
###############################################################################
###############################################################################

# Create the predictor for SL optimality regression #
# Calculate the centered proportion of demonstrators who chose correct urn  #
# Note, as there are two different groups of demonstrators, namely 
# triangle and square, and the social learners only observed either one of
# both groups of demonstrators, I have to calculate the proportion of those
# two groups of demonstrators separately. 
# I should then create a column which states the proportion of "observed"
# demonstrators who chose the optimal urn. Together with the treatment
# dummy which states whether a social learner observed the ingroup or outgroup
# it's clear whether a triangle/square social learner observed triangle/square
# demonstrators.

#############################################################################
# Take only the final periods. -> use data frame I created above called
# df_ind_learn_4round

allPpt <- unique(df_ind_learn_4round$participant_ID)
noPpt <- length(allPpt)

#Finds the unique_trials that each demonstrator took part in, to record whether they answered correctly per trial.
unique_trials <- unique(df_ind_learn_4round$trialID)
#This will record the proportion of demonstrators who chose optimally in each trial.
sessionArray <- array(1:length(unique_trials))
sessionMatrix <- matrix(data = NA, nrow = length(unique_trials), ncol = 3) # no of rows is no of unique_trials
sessionMatrix <- as.data.frame(sessionMatrix)

sessionMatrix <- sessionMatrix %>%
  rename("current_trial" = "V1",
         "triangle_dem" = "V2",
         "square_dem" = "V3")

#Calculate the proportion of demonstrators that answer optimally for each trial.
for (i in 1:length(unique_trials))
{
  #Takes data for this session.
  thisSession <- unique_trials[i]
  sessionArray[i] <- thisSession
  sessionMatrix[i,1] <- thisSession
  corrSession <- df_ind_learn_4round[df_ind_learn_4round$trialID == thisSession, ]
  
  corrChoice_triangle <- sum(corrSession[(corrSession$group_assignment=="triangle" & corrSession$chosen_correctly==1),"chosen_correctly"])
  corrChoice_square <- sum(corrSession[(corrSession$group_assignment=="square" & corrSession$chosen_correctly==1),"chosen_correctly"])
  
  sessionMatrix[i,"triangle_dem"] <- corrChoice_triangle  
  sessionMatrix[i,"square_dem"] <- corrChoice_square  
  
}

#order the sessions by the proportion of demonstrators who answered correctly.
#do so for triangle and square demonstrators separately
sessMatrixOrder_triangle <- sessionMatrix[order(sessionMatrix[ ,"triangle_dem"]), ]
sessMatrixOrder_square <- sessionMatrix[order(sessionMatrix[ ,"square_dem"]), ]

#Records the number of individual-learners who chose correctly to the fourth-round data frame of the social learners
#Again, do so for triangle demonstrators and square demonstrators separately

df_soc_learn_4round_busara$ILNoCorrTriangle <- rep(0,nrow(df_soc_learn_4round_busara))
df_soc_learn_4round_busara$ILNoCorrSquare <- rep(0,nrow(df_soc_learn_4round_busara))

for (j in 1:nrow(df_soc_learn_4round_busara))
{
  thisSessRow <- df_soc_learn_4round_busara[j, ]
  thisSess <- thisSessRow$trialID
  useThisSess <- which(sessionArray == thisSess)
  df_soc_learn_4round_busara$ILNoCorrTriangle[j] <- sessionMatrix[useThisSess,"triangle_dem"]
}

for (j in 1:nrow(df_soc_learn_4round_busara))
{
  thisSessRow <- df_soc_learn_4round_busara[j, ]
  thisSess <- thisSessRow$trialID
  useThisSess <- which(sessionArray == thisSess)
  df_soc_learn_4round_busara$ILNoCorrSquare[j] <- sessionMatrix[useThisSess,"square_dem"]
}

# Create a predictor for the second regression (SL optimality).
# The centered proportion of demonstrators who answer optimally in each block.
# Again, do so for triangle demonstrators and square demonstrators separately

df_soc_learn_4round_busara$propIndCorrCenteredTriangle <- (df_soc_learn_4round_busara$ILNoCorrTriangle / number_ind_learn_per_group) - 0.5
df_soc_learn_4round_busara$propIndCorrCenteredSquare <- (df_soc_learn_4round_busara$ILNoCorrSquare / number_ind_learn_per_group) - 0.5

# Social learners did not always observe the same group. Sometimes they observed ingroup and sometimes they observed outgroup demonstrators
# Create a column indicating the proportion of demonstrators choosing correctly that the social learners observed in a given block.

df_soc_learn_4round_busara$propIndCorrCentered <- NA

# Triangle

subset_triangle_soc <- df_soc_learn_4round_busara[df_soc_learn_4round_busara$group_assignment=="triangle",]

subset_triangle_soc$propIndCorrCentered <- ifelse(subset_triangle_soc$observing_ingroup == 1, subset_triangle_soc$propIndCorrCenteredTriangle,subset_triangle_soc$propIndCorrCenteredSquare)

# Square

subset_square_soc <- df_soc_learn_4round_busara[df_soc_learn_4round_busara$group_assignment=="square",]

subset_square_soc$propIndCorrCentered <- ifelse(subset_square_soc$observing_ingroup == 1, subset_square_soc$propIndCorrCenteredSquare,subset_square_soc$propIndCorrCenteredTriangle)

df_soc_learn_4round_busara[df_soc_learn_4round_busara$group_assignment=="triangle",] <- subset_triangle_soc
df_soc_learn_4round_busara[df_soc_learn_4round_busara$group_assignment=="square",] <- subset_square_soc

#############################################################################
# Do the same for individual learners choosing the right urn
# in other words, create a column indicating the centered proportion of 
# demonstrators choosing the right urn. but because social learners either
# observe ingroup or outgroup, I will create the prop. of demonstrators that
# the social learners observed in a given block
#############################################################################

df_soc_learn_4round_busara$propIndRightCentered <- NA

# Triangle

subset_triangle_soc$propIndRightCentered <- ifelse(subset_triangle_soc$observing_ingroup == 1, subset_triangle_soc$propTriangleRightUrnCentered,subset_triangle_soc$propSquareRightUrnCentered)

# Square

subset_square_soc$propIndRightCentered <- ifelse(subset_square_soc$observing_ingroup == 1, subset_square_soc$propSquareRightUrnCentered,subset_square_soc$propTriangleRightUrnCentered)

df_soc_learn_4round_busara[df_soc_learn_4round_busara$group_assignment=="triangle",] <- subset_triangle_soc
df_soc_learn_4round_busara[df_soc_learn_4round_busara$group_assignment=="square",] <- subset_square_soc


####################################################################################
####################################################################################

# Change similarity = 0.9 to = 1, and similarity = 0.1 to 0 for easier data handling

df_soc_learn_4round_busara$similar_ingroup <- ifelse(df_soc_learn_4round_busara$similar_ingroup==0.9, 1,0)

# Adapt control variables for easier data handling

# Age, gender, and field of study have only been registered in last round.
# In the following I will start with age and gender
# Go through all sessions and register those values for the 4th rounds

# Code gender as factor with "Other" being reference category, "Man = 1", "Woman = 2":

df_soc_learn_4round_busara$gender <- factor(df_soc_learn_4round_busara$gender, levels = c("Woman", "Man", "Other"), labels = c(0, 1, 2))

# Code study_subject as factor levels (still have to do this):

all_study_subjects <- as.list(unique(df_soc_learn_4round_busara$study_subject))


# Function to map new study fields to the three main categories
map_study_field_to_category <- function(field) {
  field <- tolower(field)
  if (grepl("engineering|math|science|chemistry|physics|biochemistry|botany|geology|microbiology|geography|research|medicine|wildlife management and conservation|CONSTRUCTION", field)) {
    return("Natural Sciences")
  } else if (grepl("arts|literature|history|music|language|communication|mass communication|anthropology|education arts|bachelor of arts|law|Law|bachelor in broadcast production|diplomacy and international studies|construction|pyschology|psychology", field)) {
    return("Humanities")
  } else if (grepl("economics|sociology|political science|public administration|international relations|education|real estate|finance|accounting|health sciences|social work|public administration|urban and regional planning|agribusiness management|statistics|public administation|bachelor of commerce|politics and adminstration", field)) {
    return("Social Sciences")
  } else {
    return("Other")
  }
}

# Apply the function to create the consolidated column
df_soc_learn_4round_busara$study_subject_category <- sapply(df_soc_learn_4round_busara$study_subject, map_study_field_to_category)

# Convert the new column to a factor
df_soc_learn_4round_busara$study_subject_category <- factor(df_soc_learn_4round_busara$study_subject_category)

# Compare number of observations per subject field:
sum(df_soc_learn_4round_busara$study_subject_category=="Other")
sum(df_soc_learn_4round_busara$study_subject_category=="Humanities")
sum(df_soc_learn_4round_busara$study_subject_category=="Natural Sciences")
sum(df_soc_learn_4round_busara$study_subject_category=="Social Sciences")

# Check which fields fall under "Other"

# List of new study fields
study_fields_new <- c(
  "Electrical engineering", "maths and economy", "Economics and political Science",
  "SOCIOLOGY", "Bachelor of Arts", "political science and sociology", "education arts",
  "arts and social sciences", "SOCIAL WORK", "Arts and social sciences", "psychology",
  "bachelor of science in mathematics", "bachelor of arts in sociology psychology and political science",
  "Economics", "sociology", "ECONOMICS AND STATISTICS", "Mathematics", "law",
  "sciology and poliical science", "bachelor of arts (anthropology)", "Bachelor of arts in Anthropology",
  "economics", "land economics in real estates", "economics and statistics",
  "BSc in Economics and statistics", "Bachelor of Economics", "statistics",
  "Economics and Statistics", " ", "bachelors degree in economics ",
  "arts", "ECONOMICS", "bachelors of arts", "geography",
  "BACHELORS OF ARTS-GEOGRAPHY AND ENVIRONMENTAL STUDIES", "ARTS",
  "Bachelor of economics", "Science and Technology", "BACHELOR OF SCIENCE IN CHEMISTRY",
  "real estate", "bachelor in broadcast production", "science in chemistry",
  "bachelor of science in chemistry", "Bachelor of Science in Statistics", "Economics and ststistics",
  "public Administration", "Education", "BSC (CHEMISTRY)",
  "political science and public administration.", "public administation", "International Relations",
  "barchelor of arts", "Arts", "Economics and statistics",
  "BA in Political Science and Public administration", "Economics and Sociology", "Bachelor of science in Economics",
  "microbiology", "Biochemistry", "research",
  "PSYCHOLOGY", "BACHELOR OF ARTS", "Political Science",
  "civil engineering", "Law", "Barchelor of arts and social sciences",
  "public administration", "Sociology", "Anthropology",
  "PUBLIC ADMINISTRATION", "Bachelor of education science", "Bachelor Of arts",
  "language and communication", "bachelor of arts", "BACHELOR OF EDUCATION SCIENCE",
  "social arts", "BACHELOR OF ARTS IN ANTHROPOLOGY", "diplomacy and international studies",
  "anthropology", "EDUCATION SCIENCE", "bachelor of commerce",
  "construction", "medicine", "Urban and Regional Planning",
  "Accounting", "health sciences", "Electrical Engineering",
  "botany", "engineering", "Wildlife Management and conservation",
  "CONSTRUCTION", "International Relations & Diplomacy", "physics",
  "AGRIBUSINESS MANAGEMENT", "politics and adminstration", "geology",
  "Bachelor of commerce; Finance", "political science", "pyschology",
  "INFORMATION SCIENCE", "finance", "Mass Communication", "STUDENT"
)

# Categorizing the new study fields
categorized_fields_new <- sapply(study_fields_new, map_study_field_to_category)

# Output the fields that are categorized as "Other"
other_fields_new <- study_fields_new[categorized_fields_new == "Other"]
other_fields_new

# Thus, one student has entered "STUDENT", and another one has not entered anything.
# This matches sum(df_soc_learn_4round_busara$study_subject_category=="Other")=40
# because that's the number of observations. There are 20 observations per social learner.

####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
# CREATE DUMMY VARIABLES
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################

# For the analysis of both, optimal behavior and pure behavior, we have fully
# saturated models that we estimate with OLS
# For this, we create dummy variables for each of the 8 treatments (c.f. pre-registration).

# similar to ingroup, observing the ingroup, no cognitive load
df_soc_learn_4round_busara$similar_ingroup_ncl_dummy <- as.factor(ifelse(df_soc_learn_4round_busara$similar_ingroup == 1 & df_soc_learn_4round_busara$observing_ingroup == 1 & df_soc_learn_4round_busara$cognitive_load == 0, 1, 0))

# dissimilar to ingroup, observing the ingroup, no cognitive load
df_soc_learn_4round_busara$dissimilar_ingroup_ncl_dummy <- as.factor(ifelse(df_soc_learn_4round_busara$similar_ingroup == 0 & df_soc_learn_4round_busara$observing_ingroup == 1 & df_soc_learn_4round_busara$cognitive_load == 0, 1, 0))

# similar to ingroup, observing the outgroup, no cognitive load
df_soc_learn_4round_busara$similar_outgroup_ncl_dummy <- as.factor(ifelse(df_soc_learn_4round_busara$similar_ingroup == 1 & df_soc_learn_4round_busara$observing_ingroup == 0 & df_soc_learn_4round_busara$cognitive_load == 0, 1, 0))

# dissimilar to ingroup, observing the outgroup, no cognitive load
df_soc_learn_4round_busara$dissimilar_outgroup_ncl_dummy <- as.factor(ifelse(df_soc_learn_4round_busara$similar_ingroup == 0 & df_soc_learn_4round_busara$observing_ingroup == 0 & df_soc_learn_4round_busara$cognitive_load == 0, 1, 0))

# similar to ingroup, observing the ingroup, cognitive load
df_soc_learn_4round_busara$similar_ingroup_cl_dummy <- as.factor(ifelse(df_soc_learn_4round_busara$similar_ingroup == 1 & df_soc_learn_4round_busara$observing_ingroup == 1 & df_soc_learn_4round_busara$cognitive_load == 1, 1, 0))

# dissimilar to ingroup, observing the ingroup, cognitive load
df_soc_learn_4round_busara$dissimilar_ingroup_cl_dummy <- as.factor(ifelse(df_soc_learn_4round_busara$similar_ingroup == 0 & df_soc_learn_4round_busara$observing_ingroup == 1 & df_soc_learn_4round_busara$cognitive_load == 1, 1, 0))

# similar to ingroup, observing the outgroup, cognitive load
df_soc_learn_4round_busara$similar_outgroup_cl_dummy <- as.factor(ifelse(df_soc_learn_4round_busara$similar_ingroup == 1 & df_soc_learn_4round_busara$observing_ingroup == 0 & df_soc_learn_4round_busara$cognitive_load == 1, 1, 0))

# dissimilar to ingroup, observing the outgroup, cognitive load
df_soc_learn_4round_busara$dissimilar_outgroup_cl_dummy <- as.factor(ifelse(df_soc_learn_4round_busara$similar_ingroup == 0 & df_soc_learn_4round_busara$observing_ingroup == 0 & df_soc_learn_4round_busara$cognitive_load == 1, 1, 0))


#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################

# OPTIMAL BEHAVIOR

#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################

# Average comparison of effect of cognitive load treatment on optimal choice -> unconditional on any other factors

df_soc_learn_no_cl <- subset(df_soc_learn_4round_busara, df_soc_learn_4round_busara$cognitive_load == 0)

freq_chosen_optimally_soc_learn_no_cl <- sum(df_soc_learn_no_cl$chosen_correctly)/nrow(df_soc_learn_no_cl)

freq_chosen_optimally_soc_learn_no_cl

df_soc_learn_cl <- subset(df_soc_learn_4round_busara, df_soc_learn_4round_busara$cognitive_load == 1)

freq_chosen_optimally_soc_learn_cl <- sum(df_soc_learn_cl$chosen_correctly)/nrow(df_soc_learn_cl)

freq_chosen_optimally_soc_learn_cl

t.test(df_soc_learn_cl$chosen_correctly,df_soc_learn_no_cl$chosen_correctly)

#############################################################################
# OLS regression
#############################################################################

ols_optimal_choice <- lm(chosen_correctly ~ propIndCorrCentered + similar_ingroup_ncl_dummy + 
                           dissimilar_ingroup_ncl_dummy + similar_outgroup_ncl_dummy +
                           similar_ingroup_cl_dummy + dissimilar_ingroup_cl_dummy +
                           similar_outgroup_cl_dummy + dissimilar_outgroup_cl_dummy,
                         data = df_soc_learn_4round_busara)

summary(ols_optimal_choice)

# Test for homoscedasticity:

bptest(ols_optimal_choice)

# We use cluster robust standard errors clustered on social learners in any case

ols_optimal_choice_robust <- coeftest(ols_optimal_choice,vcov = vcovCL, cluster = df_soc_learn_4round_busara$participant_ID)

ols_optimal_choice_robust

ols_optimal_choice_robustCI <- tidy(ols_optimal_choice_robust, conf.int = 0.95)

ols_optimal_choice_robustCI

# Test whether residuals are independent of each other:

dwtest(ols_optimal_choice)

# We cannot reject the null that residuals are independent of each other.

# Test for normality of residuals (important for inference. use shapiro-wilk test):

shapiro.test(residuals(ols_optimal_choice))

# We reject the null that there is normality on the 5% level.

#############################################################################
# Logit regression
#############################################################################

# As specified in the preregistration, the reference category corresponds to the treatment combination "outgroup/similarity=0.1/no cognitive load".

logit_optimal_choice <- glm(chosen_correctly ~ propIndCorrCentered + similar_ingroup_ncl_dummy + 
                              dissimilar_ingroup_ncl_dummy + similar_outgroup_ncl_dummy +
                              similar_ingroup_cl_dummy + dissimilar_ingroup_cl_dummy +
                              similar_outgroup_cl_dummy + dissimilar_outgroup_cl_dummy,
                            data = df_soc_learn_4round_busara, family = "binomial")

summary(logit_optimal_choice)

logit_optimal_choice <- capture.output(summary(logit_optimal_choice))
cat("do SL pick their optimal urn", logit_optimal_choice, file = "logit_optimal_choice_busara.txt", sep = "n", fill = TRUE, append = TRUE)

# With cluster-robust SE -> clustered on social learners

logit_optimal_choice <- glm(chosen_correctly ~ propIndCorrCentered + similar_ingroup_ncl_dummy + 
                              dissimilar_ingroup_ncl_dummy + similar_outgroup_ncl_dummy +
                              similar_ingroup_cl_dummy + dissimilar_ingroup_cl_dummy +
                              similar_outgroup_cl_dummy + dissimilar_outgroup_cl_dummy,
                            data = df_soc_learn_4round_busara, family = "binomial")

logit_optimal_choice_robust <- coeftest(logit_optimal_choice,vcov = vcovCL, cluster = df_soc_learn_4round_busara$participant_ID)

logit_optimal_choice_robust

logit_optimal_choice_robustCI <- tidy(logit_optimal_choice_robust, conf.int = 0.95)

logit_optimal_choice_robustCI$estimate <- sprintf("%.3f", logit_optimal_choice_robustCI$estimate)
logit_optimal_choice_robustCI$conf.low <- sprintf("%.3f", logit_optimal_choice_robustCI$conf.low)
logit_optimal_choice_robustCI$conf.high <- sprintf("%.3f", logit_optimal_choice_robustCI$conf.high)

logit_optimal_choice_robustCI

logit_optimal_choice_robust <- capture.output(logit_optimal_choice_robust)
cat("do SL pick their optimal urn, clustered SE", logit_optimal_choice_robust, file = "log_opt_robust_busara.txt", sep = "n", fill = TRUE, append = F)

################################################################################
################################################################################
# Create odds ratio plot with following regression (which is the same as above)
################################################################################
################################################################################

# I seem to have to redo this regression immediately before extracting coefficients, etc. 
# and it doesn't work if I do the confidence interval stuff
# that I did above, before I do the coefficient extraction and rest for odds ratio plot

# Fit the logistic regression model
logit_optimal_choice <- glm(chosen_correctly ~ propIndCorrCentered + similar_ingroup_ncl_dummy + 
                              dissimilar_ingroup_ncl_dummy + similar_outgroup_ncl_dummy +
                              similar_ingroup_cl_dummy + dissimilar_ingroup_cl_dummy +
                              similar_outgroup_cl_dummy + dissimilar_outgroup_cl_dummy,
                            data = df_soc_learn_4round_busara, family = "binomial")

# Extract coefficients and standard errors
coef <- summary(logit_optimal_choice)$coefficients[, "Estimate"]
clustered_se <- sqrt(diag(vcovCL(logit_optimal_choice, cluster = df_soc_learn_4round_busara$participant_ID)))

# Calculate odds ratios and confidence intervals
clustered_odds_ratios <- exp(coef)
clustered_conf_int_lower <- exp(coef - 1.96 * clustered_se)
clustered_conf_int_upper <- exp(coef + 1.96 * clustered_se)

# Create a data frame for plotting with clustered standard errors
clustered_plot_data <- data.frame(Variable = names(coef), OddsRatio = clustered_odds_ratios, 
                                  LowerCI = clustered_conf_int_lower, UpperCI = clustered_conf_int_upper)

# Create a named vector with custom labels
custom_labels <- c("(Intercept)" = "Intercept",
                   "propIndCorrCentered" = "Centered prop. dem. opt.",
                   "similar_ingroup_ncl_dummy1" = "Similar ig, observing ig, ncl",
                   "dissimilar_ingroup_ncl_dummy1" = "Dissimilar ig, observing ig, ncl",
                   "similar_outgroup_ncl_dummy1" = "Similar ig, observing og, ncl",
                   "similar_ingroup_cl_dummy1" = "Similar ig, observing ig, cl",
                   "dissimilar_ingroup_cl_dummy1" = "Dissimilar ig, observing ig, cl",
                   "similar_outgroup_cl_dummy1" = "Similar ig, observing og, cl",
                   "dissimilar_outgroup_cl_dummy1" = "Dissimilar ig, observing og, cl")

# Apply the custom labels
clustered_plot_data$Variable <- custom_labels[clustered_plot_data$Variable]

# Convert to a factor, maintaining the order of coefficients as shown in the regression further above
ordered_levels <- custom_labels[names(coef)]
clustered_plot_data$Variable <- factor(clustered_plot_data$Variable, levels = ordered_levels)

# Plot odds ratios with clustered confidence intervals using ggplot2
odd_ratio_opt <- ggplot(clustered_plot_data, aes(x = Variable, y = OddsRatio, ymin = LowerCI, ymax = UpperCI)) +
  geom_pointrange() +
  scale_y_log10() +
  coord_flip() +
  ylab("Odds ratios (Nairobi)") +
  xlab("") +
  theme_bw() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red")

# Save the plot
ggsave("odd_ratio_opt_busara.png", plot = odd_ratio_opt, width = 8, height = 6, dpi = 300)

#############################################################################
#############################################################################
# Redo regression with interaction terms 
#############################################################################
#############################################################################

# In preregistration we specified a regression without interaction terms for the analysis of optimal behaviour
# But I do it here to see whether that changes something concerning results from the previous regression

logit_optimal_choice_int <- glm(chosen_correctly ~ propIndCorrCentered + similar_ingroup_ncl_dummy + 
                                  dissimilar_ingroup_ncl_dummy + similar_outgroup_ncl_dummy +
                                  similar_ingroup_cl_dummy + dissimilar_ingroup_cl_dummy +
                                  similar_outgroup_cl_dummy + dissimilar_outgroup_cl_dummy +
                                  propIndCorrCentered:similar_ingroup_ncl_dummy +  propIndCorrCentered:dissimilar_ingroup_ncl_dummy +
                                  propIndCorrCentered:similar_outgroup_ncl_dummy + propIndCorrCentered:similar_ingroup_cl_dummy +
                                  propIndCorrCentered:dissimilar_ingroup_cl_dummy + propIndCorrCentered:similar_outgroup_cl_dummy + 
                                  propIndCorrCentered:dissimilar_outgroup_cl_dummy,
                                data = df_soc_learn_4round_busara, family = "binomial")

logit_optimal_choice_int_robust <- coeftest(logit_optimal_choice_int,vcov = vcovCL, cluster = df_soc_learn_4round_busara$participant_ID)

logit_optimal_choice_int_robust

logit_optimal_choice_int_robustCI <- tidy(logit_optimal_choice_int_robust, conf.int = 0.95)

logit_optimal_choice_int_robustCI$estimate <- sprintf("%.3f", logit_optimal_choice_int_robustCI$estimate)
logit_optimal_choice_int_robustCI$conf.low <- sprintf("%.3f", logit_optimal_choice_int_robustCI$conf.low)
logit_optimal_choice_int_robustCI$conf.high <- sprintf("%.3f", logit_optimal_choice_int_robustCI$conf.high)

logit_optimal_choice_int_robustCI

logit_optimal_choice_int_robust <- capture.output(logit_optimal_choice_int_robust)
cat("do SL pick their optimal urn, clustered SE", logit_optimal_choice_int_robust, file = "log_opt_interact_robust_busara.txt", sep = "n", fill = TRUE, append = F)


######################################################################################
# Redo analysis of OPTIMAL behavior with age, gender, and study subject as controls  #
######################################################################################

logit_optimal_choice_controls <- glm(chosen_correctly ~ age + gender + study_subject_category + propIndCorrCentered + similar_ingroup_ncl_dummy + 
                                       dissimilar_ingroup_ncl_dummy + similar_outgroup_ncl_dummy +
                                       similar_ingroup_cl_dummy + dissimilar_ingroup_cl_dummy +
                                       similar_outgroup_cl_dummy + dissimilar_outgroup_cl_dummy,
                                     data = df_soc_learn_4round_busara, family = "binomial")

summary(logit_optimal_choice_controls)

logit_optimal_choice_controls <- capture.output(summary(logit_optimal_choice_controls))
cat("do SL pick their optimal urn", logit_optimal_choice_controls, file = "logit_optimal_choice_controls_busara.txt", sep = "n", fill = TRUE, append = TRUE)

logit_optimal_choice_controls <- glm(chosen_correctly ~ age + gender + study_subject_category + propIndCorrCentered + similar_ingroup_ncl_dummy + 
                                       dissimilar_ingroup_ncl_dummy + similar_outgroup_ncl_dummy +
                                       similar_ingroup_cl_dummy + dissimilar_ingroup_cl_dummy +
                                       similar_outgroup_cl_dummy + dissimilar_outgroup_cl_dummy,
                                     data = df_soc_learn_4round_busara, family = "binomial")

logit_optimal_choice_controls_robust <- coeftest(logit_optimal_choice_controls,vcov = vcovCL, cluster = df_soc_learn_4round_busara$participant_ID)

logit_optimal_choice_controls_robust

logit_optimal_choice_controls_robustCI <- tidy(logit_optimal_choice_controls_robust, conf.int = 0.95)

logit_optimal_choice_controls_robustCI$estimate <- sprintf("%.3f", logit_optimal_choice_controls_robustCI$estimate)
logit_optimal_choice_controls_robustCI$conf.low <- sprintf("%.3f", logit_optimal_choice_controls_robustCI$conf.low)
logit_optimal_choice_controls_robustCI$conf.high <- sprintf("%.3f", logit_optimal_choice_controls_robustCI$conf.high)

logit_optimal_choice_controls_robustCI

logit_optimal_choice_controls_robust <- capture.output(logit_optimal_choice_controls_robust)
cat("do SL pick their optimal urn, clustered SE", logit_optimal_choice_controls_robust, file = "logit_optimal_choice_controls_robust_busara.txt", sep = "n", fill = TRUE, append = F)



#############################################################################
# Logit regression with block ID
#############################################################################

# Test whether social learners performed worse over time by including block ID

# As specified in the preregistration, the reference category corresponds to the treatment combination "outgroup/similarity=0.1/no cognitive load".

logit_optimal_choice_blockID <- glm(chosen_correctly ~ propIndCorrCentered + blockID + similar_ingroup_ncl_dummy + 
                                      dissimilar_ingroup_ncl_dummy + similar_outgroup_ncl_dummy +
                                      similar_ingroup_cl_dummy + dissimilar_ingroup_cl_dummy +
                                      similar_outgroup_cl_dummy + dissimilar_outgroup_cl_dummy,
                                    data = df_soc_learn_4round_busara, family = "binomial")

summary(logit_optimal_choice_blockID)

logit_optimal_choice_blockID <- capture.output(summary(logit_optimal_choice_blockID))
cat("do SL pick their optimal urn", logit_optimal_choice_blockID, file = "logit_optimal_choice_blockID_busara.txt", sep = "n", fill = TRUE, append = TRUE)

# With cluster-robust SE -> clustered on social learners

logit_optimal_choice_blockID <- glm(chosen_correctly ~ propIndCorrCentered + blockID + similar_ingroup_ncl_dummy + 
                                      dissimilar_ingroup_ncl_dummy + similar_outgroup_ncl_dummy +
                                      similar_ingroup_cl_dummy + dissimilar_ingroup_cl_dummy +
                                      similar_outgroup_cl_dummy + dissimilar_outgroup_cl_dummy,
                                    data = df_soc_learn_4round_busara, family = "binomial")

logit_optimal_choice_blockID_robust <- coeftest(logit_optimal_choice_blockID,vcov = vcovCL, cluster = df_soc_learn_4round_busara$participant_ID)

logit_optimal_choice_blockID_robust

logit_optimal_choice_blockID_robustCI <- tidy(logit_optimal_choice_blockID_robust, conf.int = 0.95)

logit_optimal_choice_blockID_robustCI$estimate <- sprintf("%.3f", logit_optimal_choice_blockID_robustCI$estimate)
logit_optimal_choice_blockID_robustCI$conf.low <- sprintf("%.3f", logit_optimal_choice_blockID_robustCI$conf.low)
logit_optimal_choice_blockID_robustCI$conf.high <- sprintf("%.3f", logit_optimal_choice_blockID_robustCI$conf.high)

logit_optimal_choice_blockID_robustCI

logit_optimal_choice_blockID_robust <- capture.output(logit_optimal_choice_blockID_robust)
cat("do SL pick their optimal urn, clustered SE", logit_optimal_choice_blockID_robust, file = "logit_optimal_choice_blockID_robust_busara.txt", sep = "n", fill = TRUE, append = F)

#############################################################################
#############################################################################
#############################################################################
# Pairwise comparisons
#############################################################################
#############################################################################
#############################################################################

source('clx_vcov.R')

# Run the logistic regression model
logit_optimal_choice <- glm(chosen_correctly ~ propIndCorrCentered + similar_ingroup_ncl_dummy + 
                              dissimilar_ingroup_ncl_dummy + similar_outgroup_ncl_dummy +
                              similar_ingroup_cl_dummy + dissimilar_ingroup_cl_dummy +
                              similar_outgroup_cl_dummy + dissimilar_outgroup_cl_dummy,
                            data = df_soc_learn_4round_busara, family = "binomial")

# Calculate vcov matrix, using standard errors clustered on social learners
vcov_coefTestModel1 <- clx_vcov(logit_optimal_choice, 1, df_soc_learn_4round_busara$participant_ID)

# Define treatments, excluding the omitted category
treatments <- c("similar_ingroup_ncl_dummy", "dissimilar_ingroup_ncl_dummy", 
                "similar_outgroup_ncl_dummy", "similar_ingroup_cl_dummy", 
                "dissimilar_ingroup_cl_dummy", "similar_outgroup_cl_dummy",
                "dissimilar_outgroup_cl_dummy")

# Generate all pairwise combinations of treatments
pairwise_combinations <- combn(treatments, 2, simplify = FALSE)

# Iterate over each combination, perform a linear hypothesis test, and print the results
for(pair in pairwise_combinations) {
  # Construct hypothesis string with appended "1"
  hypothesis <- paste0(pair[1], "1 -", pair[2], "1 = 0")
  
  # Perform the linear hypothesis test
  test_result <- linearHypothesis(logit_optimal_choice, hypothesis, test = 'F', vcov. = vcov_coefTestModel1)
  
  # Extract the p-value from the test result
  p_value <- test_result[["Pr(>F)"]][2]
  
  # Calculate the estimated difference between the treatments
  estimated_difference <- coef(logit_optimal_choice)[paste0(pair[1], "1")] - coef(logit_optimal_choice)[paste0(pair[2], "1")]
  
  # Print the comparison, estimated difference, and p-value
  cat("Pairwise comparison between", pair[1], "and", pair[2], "\n")
  cat("Estimated difference (Treatment1 - Treatment2):", estimated_difference, "\n")
  cat("P-value:", p_value, "\n\n")
}

# Use the correct summary model with the appended "1" for coefficients
summary_model <- summary(logit_optimal_choice, vcov. = vcov_coefTestModel1)

# Print p-values of comparisons with the omitted category
for(treatment in treatments) {
  # Append "1" to treatment names to match the naming convention in the model's summary
  p_value_omitted <- summary_model$coefficients[paste0(treatment, "1"), "Pr(>|z|)"]
  
  # The estimated difference from the omitted category is the coefficient itself
  estimated_difference_omitted <- coef(logit_optimal_choice)[paste0(treatment, "1")]
  
  cat("The p-value for the pairwise comparison between", treatment, "and dissimilar_outgroup_ncl_dummy is:", p_value_omitted, "\n")
  cat("Estimated difference from omitted category:", estimated_difference_omitted, "\n\n")
}


######################################################################
# Analyse treatment ordering effects  #
######################################################################

# Participants go through 20 rounds of decision making. the two within-subjects 
# treatments are distributed in different combinations across those 20 rounds as 
# follows: the 20 rounds are split into four groups of 5 rounds. in every group 
# of 5 rounds, one of the four within-subjects treatment combinations is conducted. 
# in other words, participants go through 5 rounds of the same within-subjects 
# treatment combination and then in the next 5 rounds it changes to another 
# within-subjects treatment combination. the within-subjects treatments combinations 
# consist of two treatments that can take on one of two variations. in other words, 
# per treatment there are two variations. since we have two treatments with two 
# treatment variations, this gives us 4 possible combinations of those treatment 
# variations. to illustrate, treatment 1 could be in the versions A and B. 
# treatment 2 could be C and D. hence, the combinations are AxC, AxD, BxC, and BxD. 
# the order in which those four combinations were assigned to the four groups of 
# 5 rounds different between the participants. there are 24 possible permutations 
# of orderings, given we have 4 combinations and 4 groups of 5 rounds. now, in my 
# statistical analysis, in addition to the treatments themselves, I want to test 
# whether the ordering also mattered. in my data frame, participants have numbers 
# going from 0-23, representing the 24 possible permutations (note, the numbers 
# start with 0 because I retrieved the ordering from a python array and there the 
# indexing starts with 0. but this is not relevant for the analysis in R. in R, 
# I could also re-assign the values such that the participants have values ranging 
# between 1-24, so that it would be easier to understand, that those integers 
# represent one of 24 possible orderings of the 2x2 within subjects-treatments).

# Create dummy variables indicating whether a treatment combination
# In otree program I generated an array of all possible permutations (see 
# description above) and generated a list which repeated that array multiple times.
# I did that because it was the only solution I found at that time to allow 
# cross-balancing of treatment orderings between subjects in a way that I could
# manipulate between every session, when configuring the sessions. So, subjects
# do not only have values between 0-23 but actually values up to 100. The list
# is such that after going through all 24 possible permutations, the index 24 in
# the 100-long list simply starts from the first ordering again, i.e. is the same
# ordering as represented with index = 0. In other words, after 23, a participant 
# getting the value 24 is assigned the first ordering, a participant with the value
# 25 has the second ordering, etc. To make it easier to understand, I "reverse-
# engineer" the participants' treatment_assignment values such that everyone has
# a value between 0-23 and I change the values such that it corresponds to the
# indexing of R, i.e. value 0 will be transformed to 1, etc. such that every
# participant has a value between 1-24, representing the 24 different permutations.

# df_soc_learn_4round_busara$uniqueSubjectNum <- rep(NA, length(df_soc_learn_4round_busara[,1]))
# 
# id_integer <- 1
# 
# for (id in unique(df_soc_learn_4round_busara$participant_ID)) {
#   
#   df_soc_learn_4round_busara[which(df_soc_learn_4round_busara$participant_ID==id),"uniqueSubjectNum"] <- id_integer
#   
#   id_integer <- id_integer + 1
# }
# 
# 
# # Change indexing such that it starts from 1 and not from 0:
# 
# df_soc_learn_4round_busara$treatment_assignment <- df_soc_learn_4round_busara$treatment_assignment + 1
# 
# # Now transform values such that everyone has values between 1-24:
# 
# for (i in 1:length(df_soc_learn_4round_busara$treatment_assignment)) {
#   
#   # Use modulo operation and adjust for values that are multiples of 24
#   df_soc_learn_4round_busara$treatment_assignment[i] <- df_soc_learn_4round_busara$treatment_assignment[i] %% 24
#   if (df_soc_learn_4round_busara$treatment_assignment[i] == 0) {
#     df_soc_learn_4round_busara$treatment_assignment[i] <- 24
#   }
#   
# }
# 
# 


#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################

# PURE BEHAVIOR

#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################

# Logit regression to predict social learners' choice of urn based on social information available #

# Runs a logistic regression to predict social-learners' choice, using the three pieces of social information and interactions.
# This is different from optimal choice behavior. Here it is just about being influenced by the observed social information concerning the choice of left/right urn
# This can, but doesn't have to always correlate to optimal behavior because sometimes it is optimal to follow the majority of obvserved demonstrators and sometimes
# it's optimal to follow the minority (depending on similarity to ingroup and whether observing in-/outgroup)

logit_urn_choice <- glm(urn_choice ~ propIndRightCentered + similar_ingroup_ncl_dummy + 
                          dissimilar_ingroup_ncl_dummy + similar_outgroup_ncl_dummy +
                          similar_ingroup_cl_dummy + dissimilar_ingroup_cl_dummy +
                          similar_outgroup_cl_dummy + dissimilar_outgroup_cl_dummy +
                          propIndRightCentered:similar_ingroup_ncl_dummy +  propIndRightCentered:dissimilar_ingroup_ncl_dummy +
                          propIndRightCentered:similar_outgroup_ncl_dummy + propIndRightCentered:similar_ingroup_cl_dummy +
                          propIndRightCentered:dissimilar_ingroup_cl_dummy + propIndRightCentered:similar_outgroup_cl_dummy + 
                          propIndRightCentered:dissimilar_outgroup_cl_dummy,
                        data = df_soc_learn_4round_busara, family = "binomial")

summary(logit_urn_choice)

logit_urn_choice <- capture.output(summary(logit_urn_choice))
cat("predicting SL choice of urn", logit_urn_choice, file = "logit_urn_choice.txt", sep = "n", fill = TRUE, append = TRUE)

# With cluster-robust SE -> clustered on social learners

logit_urn_choice <- glm(urn_choice ~ propIndRightCentered + similar_ingroup_ncl_dummy + 
                          dissimilar_ingroup_ncl_dummy + similar_outgroup_ncl_dummy +
                          similar_ingroup_cl_dummy + dissimilar_ingroup_cl_dummy +
                          similar_outgroup_cl_dummy + dissimilar_outgroup_cl_dummy +
                          propIndRightCentered:similar_ingroup_ncl_dummy +  propIndRightCentered:dissimilar_ingroup_ncl_dummy +
                          propIndRightCentered:similar_outgroup_ncl_dummy + propIndRightCentered:similar_ingroup_cl_dummy +
                          propIndRightCentered:dissimilar_ingroup_cl_dummy + propIndRightCentered:similar_outgroup_cl_dummy + 
                          propIndRightCentered:dissimilar_outgroup_cl_dummy,
                        data = df_soc_learn_4round_busara, family = "binomial")

logit_urn_choice_robust <- coeftest(logit_urn_choice,vcov = vcovCL, cluster = df_soc_learn_4round_busara$participant_ID)

logit_urn_choice_robust

logit_urn_choice_robustCI <- tidy(logit_urn_choice_robust, conf.int = 0.95)

logit_urn_choice_robustCI

logit_urn_choice_robust <- capture.output(logit_urn_choice_robust)
cat("predicting SL choice of urn, clustered SE", logit_urn_choice_robust, file = "logit_urn_choice_robust_robust.txt", sep = "n", fill = TRUE, append = F)


################################################################################
################################################################################
# Create odds ration plot with following regression (which is the same as above)
################################################################################
################################################################################

logit_urn_choice <- glm(urn_choice ~ propIndRightCentered + similar_ingroup_ncl_dummy + 
                          dissimilar_ingroup_ncl_dummy + similar_outgroup_ncl_dummy +
                          similar_ingroup_cl_dummy + dissimilar_ingroup_cl_dummy +
                          similar_outgroup_cl_dummy + dissimilar_outgroup_cl_dummy +
                          propIndRightCentered:similar_ingroup_ncl_dummy +  propIndRightCentered:dissimilar_ingroup_ncl_dummy +
                          propIndRightCentered:similar_outgroup_ncl_dummy + propIndRightCentered:similar_ingroup_cl_dummy +
                          propIndRightCentered:dissimilar_ingroup_cl_dummy + propIndRightCentered:similar_outgroup_cl_dummy + 
                          propIndRightCentered:dissimilar_outgroup_cl_dummy,
                        data = df_soc_learn_4round_busara, family = "binomial")

coef <- summary(logit_urn_choice)$coefficients[, "Estimate"]
std_errors <- summary(logit_urn_choice)$coefficients[, "Std. Error"]
clustered_se <- sqrt(diag(vcovCL(logit_urn_choice, cluster = df_soc_learn_4round_busara$participant_ID)))

clustered_odds_ratios <- exp(coef)
clustered_conf_int_lower <- exp(coef - 1.96 * clustered_se)
clustered_conf_int_upper <- exp(coef + 1.96 * clustered_se)

# Create a data frame for plotting with clustered standard errors
clustered_plot_data <- data.frame(Variable = names(coef), OddsRatio = clustered_odds_ratios, LowerCI = clustered_conf_int_lower, UpperCI = clustered_conf_int_upper)

# Remove the trailing "1" from the variable names in the clustered_plot_data data frame
clustered_plot_data$Variable <- gsub("1$", "", clustered_plot_data$Variable)

# Plot odds ratios with clustered confidence intervals using ggplot2
odd_ratio_pure <- ggplot(clustered_plot_data, aes(x = Variable, y = OddsRatio, ymin = LowerCI, ymax = UpperCI)) +
  geom_pointrange(size = 0.2) +
  scale_y_log10(labels = custom_decimal_labels, breaks = scales::trans_breaks("log10", function(x) 10^x)) +
  coord_flip() +
  ylab("Odds Ratio") +
  xlab("Variable") +
  theme_bw() +
  geom_hline(aes(yintercept = 1), linetype = "dashed", color = "red", linewidth = 0.4)  

ggsave("odd_ratio_pure_busara.png", plot = odd_ratio_pure, width = 8, height = 6, dpi = 300)

###################################################################
# Redo analysis of PURE behavior with age and gender as controls #
###################################################################

logit_urn_choice_controls <- glm(urn_choice ~ age + gender + propIndRightCentered + similar_ingroup_ncl_dummy + 
                                   dissimilar_ingroup_ncl_dummy + similar_outgroup_ncl_dummy +
                                   similar_ingroup_cl_dummy + dissimilar_ingroup_cl_dummy +
                                   similar_outgroup_cl_dummy + dissimilar_outgroup_cl_dummy +
                                   propIndRightCentered:similar_ingroup_ncl_dummy +  propIndRightCentered:dissimilar_ingroup_ncl_dummy +
                                   propIndRightCentered:similar_outgroup_ncl_dummy + propIndRightCentered:similar_ingroup_cl_dummy +
                                   propIndRightCentered:dissimilar_ingroup_cl_dummy + propIndRightCentered:similar_outgroup_cl_dummy + 
                                   propIndRightCentered:dissimilar_outgroup_cl_dummy,
                                 data = df_soc_learn_4round_busara, family = "binomial")

summary(logit_urn_choice_controls)

logit_urn_choice_controls_robust <- coeftest(logit_urn_choice_controls,vcov = vcovCL, cluster = df_soc_learn_4round_busara$participant_ID)

logit_urn_choice_controls_robust

logit_urn_choice_controls_robustCI <- tidy(logit_urn_choice_controls_robust, conf.int = 0.95)

logit_urn_choice_controls_robustCI



######################################################################
######################################################################
######################################################################
############## GRAPHICAL ANALYSIS OF PURE BEHAVIOR ###################
######################################################################
######################################################################
######################################################################

# First create column showing how many observed demonstrators chose right urn
# Currently I have that with respect to LEFT urn for both triangle and square, 
# but social learners sometimes observe one or the other group, and I want to
# use RIGHT urn because of my regression model

df_soc_learn_4round_busara$number_observed_IL_right_urn <- number_ind_learn_per_group -
  ifelse(df_soc_learn_4round_busara$group_assignment=="square" & df_soc_learn_4round_busara$observing_ingroup,df_soc_learn_4round_busara$number_left_urn_square,df_soc_learn_4round_busara$number_left_urn_triangle)

df_soc_learn_4round_busara$number_observed_IL_right_urn <- number_ind_learn_per_group -
  ifelse(df_soc_learn_4round_busara$group_assignment=="triangle" & df_soc_learn_4round_busara$observing_ingroup,df_soc_learn_4round_busara$number_left_urn_triangle,df_soc_learn_4round_busara$number_left_urn_square)

# Create unique subject number based on participant ID but repeat for each block
# and then go up with numbers for subjects of different sessions
df_soc_learn_4round_busara$uniqueSubjectNum <- rep(NA, length(df_soc_learn_4round_busara[,1]))

id_integer <- 1

for (id in unique(df_soc_learn_4round_busara$participant_ID)) {
  
  df_soc_learn_4round_busara[which(df_soc_learn_4round_busara$participant_ID==id),"uniqueSubjectNum"] <- id_integer
  
  id_integer <- id_integer + 1
}

# Save data to working directory so that the "estAndBootImitationFunctions.R" can load this ordered data set.
save(df_soc_learn_4round_busara, file = "df_soc_learn_4round_busara.RData")

# Load function to run estimations and bootstrapped sampling. This allows confidence intervals to be plotted.
source("mybootfunct.R")

# Calls the function and saves output to est_df_soc_learn_4. 
# Uses df_soc_learn_4round_busara dataset, 9999 bootstrapped samples (to give 10,000 together with actual sample) and standard 0.05 alpha.
est_df_soc_learn_4 <- mybootfunct(df_soc_learn_4round_busara, 9999, 0.05)

# Saves the output of this function data frame.
est_df_soc_learn_4_busara <- as.data.frame(est_df_soc_learn_4)
save(est_df_soc_learn_4_busara, file = "est_df_soc_learn_4_busara.RData")



################################################################################
############################# MANIPULATION CHECK ###############################
################################################################################

# Checking whether memorization task actually worked in terms of reducing 
# ability to enter correct numbers

# Average comparison of effect of cognitive load treatment on entering numbers correctly:

freq_memorized_correctly_no_cl <- sum(df_soc_learn_no_cl$memorized_correctly)/nrow(df_soc_learn_no_cl)

freq_memorized_correctly_no_cl

freq_memorized_correctly_cl <- sum(df_soc_learn_cl$memorized_correctly)/nrow(df_soc_learn_cl)

freq_memorized_correctly_cl

t.test(df_soc_learn_no_cl$memorized_correctly,df_soc_learn_cl$memorized_correctly)

# Regression checking same thing:

output <- glm(memorized_correctly ~ cognitive_load,
              data = df_soc_learn_4round_busara, family = "binomial")

output_test <- coeftest(output,vcov = vcovCL, cluster = df_soc_learn_4round_busara$participant_ID)

output_test

output_testCI <- tidy(output_test, conf.int = 0.95)

output_testCI


################################################################################

# How many observations in the 8 treatments?

################################################################################

# How many social learners in cognitive load vs no cognitive load treatment?
# We can check that in first block because cognitive load is a between-subjects
# treatment and we get the distribution of this treatment already in the first block

df_soc_learn_firstblock <- df_soc_learn_4round_busara[df_soc_learn_4round_busara$round_number == 4, ]

sum(df_soc_learn_firstblock$cognitive_load==1)
sum(df_soc_learn_firstblock$cognitive_load==0)

# Distribution of observations over cognitive load vs no cognitive load
# Note, this should just be 20 times the numbers above because we have 20
# observations per social learners

sum(df_soc_learn_4round_busara$cognitive_load==1)
sum(df_soc_learn_4round_busara$cognitive_load==0)

# Observations of 8 different treatments:

nr_cl_sim_in <-  sum(df_soc_learn_4round_busara$cognitive_load==1 & df_soc_learn_4round_busara$similar_ingroup==1 & df_soc_learn_4round_busara$observing_ingroup==1)
nr_cl_sim_out <- sum(df_soc_learn_4round_busara$cognitive_load==1 & df_soc_learn_4round_busara$similar_ingroup==1 & df_soc_learn_4round_busara$observing_ingroup==0)
nr_cl_dissim_in <- sum(df_soc_learn_4round_busara$cognitive_load==1 & df_soc_learn_4round_busara$similar_ingroup==0 & df_soc_learn_4round_busara$observing_ingroup==1)
nr_cl_dissim_out <- sum(df_soc_learn_4round_busara$cognitive_load==1 & df_soc_learn_4round_busara$similar_ingroup==0 & df_soc_learn_4round_busara$observing_ingroup==0)
nr_ncl_sim_in <- sum(df_soc_learn_4round_busara$cognitive_load==0 & df_soc_learn_4round_busara$similar_ingroup==1 & df_soc_learn_4round_busara$observing_ingroup==1)
nr_ncl_sim_out <-  sum(df_soc_learn_4round_busara$cognitive_load==0 & df_soc_learn_4round_busara$similar_ingroup==1 & df_soc_learn_4round_busara$observing_ingroup==0)
nr_ncl_dissim_in <- sum(df_soc_learn_4round_busara$cognitive_load==0 & df_soc_learn_4round_busara$similar_ingroup==0 & df_soc_learn_4round_busara$observing_ingroup==1)
nr_ncl_dissim_out <- sum(df_soc_learn_4round_busara$cognitive_load==0 & df_soc_learn_4round_busara$similar_ingroup==0 & df_soc_learn_4round_busara$observing_ingroup==0)

nr_per_tmt <- as.data.frame(matrix(NA,1,8))
nr_per_tmt[,1] <- nr_cl_sim_in
nr_per_tmt[,2] <- nr_cl_sim_out
nr_per_tmt[,3] <- nr_cl_dissim_in
nr_per_tmt[,4] <- nr_cl_dissim_out
nr_per_tmt[,5] <- nr_ncl_sim_in
nr_per_tmt[,6] <- nr_ncl_sim_out
nr_per_tmt[,7] <- nr_ncl_dissim_in
nr_per_tmt[,8] <- nr_ncl_dissim_out

obs_per_tmt <- nr_per_tmt %>%
  rename("obs_cl_sim_in" = "V1",
         "obs_cl_sim_out" = "V2",
         "obs_cl_dissim_in" = "V3",
         "obs_cl_dissim_out" = "V4",
         "obs_ncl_sim_in" = "V5",
         "obs_ncl_sim_out" = "V6",
         "obs_ncl_dissim_in" = "V7",
         "obs_ncl_dissim_out" = "V8")

obs_per_tmt

save(obs_per_tmt, file = "obs_per_tmt_busara.RData")

################################################################################
################################################################################
################################################################################

# Some checks to see whether the oTree program behaved as it was supposed to

################################################################################
################################################################################
################################################################################

# Social learners were similar or dissimilar in terms of winning color to their ingroup demonstrators
# The probability of similarity was either 0.9 or 0.1. In the otree program I recorded whether
# a social learner did in fact share the winning color with ingroup demonstrators in a given block.
# To see whether the fraction of blocks in which a social learner does in fact share the winning color
# is indeed 0.9 or 0.1 in the corresponding similarity treatments, I calculate the this fraction 
# for each social learner below:

# Similarity 0.9:

average_shared_winning_color_similar <- rep(NA,length(unique(df_soc_learn_4round_busara$participant_ID)))

for (i in 1:length(unique(df_soc_learn_4round_busara$participant_ID))) {
  average_shared_winning_color_similar[i] <- sum(df_soc_learn_4round_busara[which(df_soc_learn_4round_busara$participant_ID==unique(df_soc_learn_4round_busara$participant_ID)[i] & df_soc_learn_4round_busara$similar_ingroup==1),"share_winning_color"]) / length(df_soc_learn_4round_busara[which(df_soc_learn_4round_busara$participant_ID==unique(df_soc_learn_4round_busara$participant_ID)[i] & df_soc_learn_4round_busara$similar_ingroup==1),"share_winning_color"])
}

mean(average_shared_winning_color_similar)


# Similarity 0.1:

average_shared_winning_color_dissimilar <- rep(NA,length(unique(df_soc_learn_4round_busara$participant_ID)))

for (i in 1:length(unique(df_soc_learn_4round_busara$participant_ID))) {
  average_shared_winning_color_dissimilar[i] <- sum(df_soc_learn_4round_busara[which(df_soc_learn_4round_busara$participant_ID==unique(df_soc_learn_4round_busara$participant_ID)[i] & df_soc_learn_4round_busara$similar_ingroup==0),"share_winning_color"]) / length(df_soc_learn_4round_busara[which(df_soc_learn_4round_busara$participant_ID==unique(df_soc_learn_4round_busara$participant_ID)[i] & df_soc_learn_4round_busara$similar_ingroup==0),"share_winning_color"])
}

mean(average_shared_winning_color_dissimilar)

# It worked




################################################################################
################################################################################
################################################################################

# QUESTIONNAIRE 

################################################################################
################################################################################
################################################################################

# Extract round 80 social learner data

df_soc_learn_80round_busara <- subset(df_soc_learn, round_number == 80)

nrow(df_soc_learn_80round_busara)

df_soc_learn_80round_busara_cl <- subset(df_soc_learn_80round_busara, cognitive_load == 1)
df_soc_learn_80round_busara_ncl <- subset(df_soc_learn_80round_busara, cognitive_load == 0)

mean(df_soc_learn_80round_busara$q1)
sd(df_soc_learn_80round_busara$q1)

mean(df_soc_learn_80round_busara$q2)
sd(df_soc_learn_80round_busara$q2)

mean(df_soc_learn_80round_busara$q3)
sd(df_soc_learn_80round_busara$q3)

mean(df_soc_learn_80round_busara_cl$q4)
sd(df_soc_learn_80round_busara_cl$q4)

mean(df_soc_learn_80round_busara_ncl$q4nocl)
sd(df_soc_learn_80round_busara_ncl$q4nocl)

mean(df_soc_learn_80round_busara_cl$q5)
sd(df_soc_learn_80round_busara_cl$q5)

mean(df_soc_learn_80round_busara_ncl$q5nocl)
sd(df_soc_learn_80round_busara_ncl$q5nocl)

hist(df_soc_learn_80round_busara_ncl$q5nocl)

mean(df_soc_learn_80round_busara_cl$q6)
sd(df_soc_learn_80round_busara_cl$q6)

mean(df_soc_learn_80round_busara_cl$q7)
sd(df_soc_learn_80round_busara_cl$q7)

mean(df_soc_learn_80round_busara_cl$q8)
sd(df_soc_learn_80round_busara_cl$q8)









