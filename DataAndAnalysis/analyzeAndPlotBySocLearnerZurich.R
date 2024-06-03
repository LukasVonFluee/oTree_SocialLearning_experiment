#############################################################################
# The majority of the following code was originally created by Charles Efferson
# for the paper "The Evolution of Facultative Conformity Based on Similarity" 
# by Efferson et al. (2016). The code was adapted to the current study.
#############################################################################

rm(list=ls())

load('df_soc_learn_4round_zurich.RData')

###############################################
########## Experiment Parameters ##############
###############################################

total_number_rounds <- 80

number_blocks <- 20

number_rounds_per_block <- 4

number_ind_learn <- 10

number_ind_learn_per_group <- 5

###############################################
############ Plotting Parameters ##############
###############################################

widthWindow <- 14.25
heightWindow <-14
cexNum <- 1

pCrit <- 0.05

###############################################

# Create a dummy indicating if more than half of ind learners choose right
df_soc_learn_4round_zurich$numIndLeftGreaterHalf <- rep(0,nrow(df_soc_learn_4round_zurich))
df_soc_learn_4round_zurich$numIndLeftGreaterHalf[df_soc_learn_4round_zurich$number_observed_IL_right_urn > 5/2] <- 1

# Charles created a normalized independent variable. I already did that in the
# data_analysis_zurich.R script, and the variable is called "propIndRightCentered"

# Charles creates here dummies for the tmt combinations. I already did that in 
# the data_analysis_zurich.R script. In my case, the columns in 
# df_soc_learn_4round_zurich.RData representing the treatments are called:
# similar_ingroup_dummy
# dissimilar_ingroup_dummy
# similar_outgroup_dummy
# disssimilar_outgroup_cl_dummy
# similar_ingroup_cl_dummy
# dissimilar_ingroup_cl_dummy
# similar_outgroup_cl_dummy
# disssimilar_outgroup_cl_dummy

# Charles created a variable to record which choice it is (i.e. 1 to 20) for the social learning => a kind of dynamics
# I already created such a variable in data_analysis_zurich.R, and the variable is called "blockID"

# Charles did the following. Need to change names at some point because "amb" and "unamb" refer to Charles' treatments ambiguous and unambigous
# As far as I can see those are simply boundary values for the beta coefficients and determine the x-axis range in the plots.

ambMinBeta <- -20
ambMaxBeta <- abs(ambMinBeta)

unambMinBeta <- -20
unambMaxBeta <- abs(unambMinBeta)

##################################################
##################################################
############## similar_ingroup_ncl_dummy #########
##################################################
##################################################

# Charles creates the data frame "temp" which includes only fourth period data
# and certain treatments. I already created a data frame with only fourth period
# data in data_analysis_zurich.R. It is the data frame df_soc_learn_4round_zurich.RData
# which I load at the beginning of this script. I can create a "temp" data frame
# which only includes certain treatments. I have different treatments than 
# Efferson et al. (2016). Charles did some analysis with combined treatments
# which he for example calls AMBIGUOUS, ALL CHOICES, NO PRIOR. He also does all 
# of his 8 treatments separately as well though. Not sure whether I need combined
# treatments. I'll do all 8 treatments separately first and I start with 
# similar_ingroup_ncl_dummy"

temp <- df_soc_learn_4round_zurich[(df_soc_learn_4round_zurich$similar_ingroup_ncl_dummy == 1), ]
socLearnersPresent <- as.numeric(levels(as.factor(temp$uniqueSubjectNum)))

perfectMajority <- rep(0,length(socLearnersPresent))
perfectMinority <- rep(0,length(socLearnersPresent))
unconditional <- rep(0,length(socLearnersPresent))
beta <- rep(NA,length(socLearnersPresent))
pValueBeta <- rep(NA,length(socLearnersPresent))

for (i in 1:length(socLearnersPresent))
{
  if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i]]) == 0 | mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i]]) == 1)
  {
    perfectMajority[i] <- 0
    perfectMinority[i] <- 0
    unconditional[i] <- 1
  }
  else if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered < 0]) == 0 & mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered > 0]) == 1)
  {
    perfectMajority[i] <- 1
    perfectMinority[i] <- 0
    unconditional[i] <- 0
  }
  else if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered < 0]) == 1 & mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered > 0]) == 0)
  {
    perfectMajority[i] <- 0
    perfectMinority[i] <- 1
    unconditional[i] <- 0
  }
  else
  {
    res <- glm(urn_choice ~ propIndRightCentered + 0, family = binomial(link = 'logit'), data = temp[temp$uniqueSubjectNum == socLearnersPresent[i], ])
    beta[i] <- coef(summary(res))[ ,1]
    pValueBeta[i] <- coef(summary(res))[ ,4]
  }
}

similar_ingroup_ncl_dummy_pure <- data.frame(socLearnersPresent,perfectMajority,perfectMinority,unconditional,beta,pValueBeta)

##################################################
##################################################
############# dissimilar_ingroup_ncl_dummy #######
##################################################
##################################################

temp <- df_soc_learn_4round_zurich[(df_soc_learn_4round_zurich$dissimilar_ingroup_ncl_dummy == 1), ]
socLearnersPresent <- as.numeric(levels(as.factor(temp$uniqueSubjectNum)))

perfectMajority <- rep(0,length(socLearnersPresent))
perfectMinority <- rep(0,length(socLearnersPresent))
unconditional <- rep(0,length(socLearnersPresent))
beta <- rep(NA,length(socLearnersPresent))
pValueBeta <- rep(NA,length(socLearnersPresent))

for (i in 1:length(socLearnersPresent))
{
  if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i]]) == 0 | mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i]]) == 1)
  {
    perfectMajority[i] <- 0
    perfectMinority[i] <- 0
    unconditional[i] <- 1
  }
  else if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered < 0]) == 0 & mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered > 0]) == 1)
  {
    perfectMajority[i] <- 1
    perfectMinority[i] <- 0
    unconditional[i] <- 0
  }
  else if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered < 0]) == 1 & mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered > 0]) == 0)
  {
    perfectMajority[i] <- 0
    perfectMinority[i] <- 1
    unconditional[i] <- 0
  }
  else
  {
    res <- glm(urn_choice ~ propIndRightCentered + 0, family = binomial(link = 'logit'), data = temp[temp$uniqueSubjectNum == socLearnersPresent[i], ])
    beta[i] <- coef(summary(res))[ ,1]
    pValueBeta[i] <- coef(summary(res))[ ,4]
  }
}

dissimilar_ingroup_ncl_dummy_pure <- data.frame(socLearnersPresent,perfectMajority,perfectMinority,unconditional,beta,pValueBeta)


##################################################
##################################################
############# similar_outgroup_ncl_dummy #########
##################################################
##################################################

temp <- df_soc_learn_4round_zurich[(df_soc_learn_4round_zurich$similar_outgroup_ncl_dummy == 1), ]
socLearnersPresent <- as.numeric(levels(as.factor(temp$uniqueSubjectNum)))

perfectMajority <- rep(0,length(socLearnersPresent))
perfectMinority <- rep(0,length(socLearnersPresent))
unconditional <- rep(0,length(socLearnersPresent))
beta <- rep(NA,length(socLearnersPresent))
pValueBeta <- rep(NA,length(socLearnersPresent))

for (i in 1:length(socLearnersPresent))
{
  if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i]]) == 0 | mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i]]) == 1)
  {
    perfectMajority[i] <- 0
    perfectMinority[i] <- 0
    unconditional[i] <- 1
  }
  else if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered < 0]) == 0 & mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered > 0]) == 1)
  {
    perfectMajority[i] <- 1
    perfectMinority[i] <- 0
    unconditional[i] <- 0
  }
  else if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered < 0]) == 1 & mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered > 0]) == 0)
  {
    perfectMajority[i] <- 0
    perfectMinority[i] <- 1
    unconditional[i] <- 0
  }
  else
  {
    res <- glm(urn_choice ~ propIndRightCentered + 0, family = binomial(link = 'logit'), data = temp[temp$uniqueSubjectNum == socLearnersPresent[i], ])
    beta[i] <- coef(summary(res))[ ,1]
    pValueBeta[i] <- coef(summary(res))[ ,4]
  }
}

similar_outgroup_ncl_dummy_pure <- data.frame(socLearnersPresent,perfectMajority,perfectMinority,unconditional,beta,pValueBeta)

##################################################
##################################################
############# dissimilar_outgroup_ncl_dummy ######
##################################################
##################################################

temp <- df_soc_learn_4round_zurich[(df_soc_learn_4round_zurich$dissimilar_outgroup_ncl_dummy == 1), ]
socLearnersPresent <- as.numeric(levels(as.factor(temp$uniqueSubjectNum)))

perfectMajority <- rep(0,length(socLearnersPresent))
perfectMinority <- rep(0,length(socLearnersPresent))
unconditional <- rep(0,length(socLearnersPresent))
beta <- rep(NA,length(socLearnersPresent))
pValueBeta <- rep(NA,length(socLearnersPresent))

for (i in 1:length(socLearnersPresent))
{
  if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i]]) == 0 | mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i]]) == 1)
  {
    perfectMajority[i] <- 0
    perfectMinority[i] <- 0
    unconditional[i] <- 1
  }
  else if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered < 0]) == 0 & mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered > 0]) == 1)
  {
    perfectMajority[i] <- 1
    perfectMinority[i] <- 0
    unconditional[i] <- 0
  }
  else if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered < 0]) == 1 & mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered > 0]) == 0)
  {
    perfectMajority[i] <- 0
    perfectMinority[i] <- 1
    unconditional[i] <- 0
  }
  else
  {
    res <- glm(urn_choice ~ propIndRightCentered + 0, family = binomial(link = 'logit'), data = temp[temp$uniqueSubjectNum == socLearnersPresent[i], ])
    beta[i] <- coef(summary(res))[ ,1]
    pValueBeta[i] <- coef(summary(res))[ ,4]
  }
}

dissimilar_outgroup_ncl_dummy_pure <- data.frame(socLearnersPresent,perfectMajority,perfectMinority,unconditional,beta,pValueBeta)

################################################################################
################################################################################
################################################################################
# Now with cognitive load
################################################################################
################################################################################
################################################################################

##################################################
##################################################
############ similar_ingroup_cl_dummy ############
##################################################
##################################################

temp <- df_soc_learn_4round_zurich[(df_soc_learn_4round_zurich$similar_ingroup_cl_dummy == 1), ]
socLearnersPresent <- as.numeric(levels(as.factor(temp$uniqueSubjectNum)))

perfectMajority <- rep(0,length(socLearnersPresent))
perfectMinority <- rep(0,length(socLearnersPresent))
unconditional <- rep(0,length(socLearnersPresent))
beta <- rep(NA,length(socLearnersPresent))
pValueBeta <- rep(NA,length(socLearnersPresent))

for (i in 1:length(socLearnersPresent))
{
  if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i]]) == 0 | mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i]]) == 1)
  {
    perfectMajority[i] <- 0
    perfectMinority[i] <- 0
    unconditional[i] <- 1
  }
  else if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered < 0]) == 0 & mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered > 0]) == 1)
  {
    perfectMajority[i] <- 1
    perfectMinority[i] <- 0
    unconditional[i] <- 0
  }
  else if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered < 0]) == 1 & mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered > 0]) == 0)
  {
    perfectMajority[i] <- 0
    perfectMinority[i] <- 1
    unconditional[i] <- 0
  }
  else
  {
    res <- glm(urn_choice ~ propIndRightCentered + 0, family = binomial(link = 'logit'), data = temp[temp$uniqueSubjectNum == socLearnersPresent[i], ])
    beta[i] <- coef(summary(res))[ ,1]
    pValueBeta[i] <- coef(summary(res))[ ,4]
  }
}

similar_ingroup_cl_dummy_pure <- data.frame(socLearnersPresent,perfectMajority,perfectMinority,unconditional,beta,pValueBeta)

##################################################
##################################################
########### dissimilar_ingroup_cl_dummy ##########
##################################################
##################################################

temp <- df_soc_learn_4round_zurich[(df_soc_learn_4round_zurich$dissimilar_ingroup_cl_dummy == 1), ]
socLearnersPresent <- as.numeric(levels(as.factor(temp$uniqueSubjectNum)))

perfectMajority <- rep(0,length(socLearnersPresent))
perfectMinority <- rep(0,length(socLearnersPresent))
unconditional <- rep(0,length(socLearnersPresent))
beta <- rep(NA,length(socLearnersPresent))
pValueBeta <- rep(NA,length(socLearnersPresent))

for (i in 1:length(socLearnersPresent))
{
  if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i]]) == 0 | mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i]]) == 1)
  {
    perfectMajority[i] <- 0
    perfectMinority[i] <- 0
    unconditional[i] <- 1
  }
  else if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered < 0]) == 0 & mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered > 0]) == 1)
  {
    perfectMajority[i] <- 1
    perfectMinority[i] <- 0
    unconditional[i] <- 0
  }
  else if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered < 0]) == 1 & mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered > 0]) == 0)
  {
    perfectMajority[i] <- 0
    perfectMinority[i] <- 1
    unconditional[i] <- 0
  }
  else
  {
    res <- glm(urn_choice ~ propIndRightCentered + 0, family = binomial(link = 'logit'), data = temp[temp$uniqueSubjectNum == socLearnersPresent[i], ])
    beta[i] <- coef(summary(res))[ ,1]
    pValueBeta[i] <- coef(summary(res))[ ,4]
  }
}

dissimilar_ingroup_cl_dummy_pure <- data.frame(socLearnersPresent,perfectMajority,perfectMinority,unconditional,beta,pValueBeta)


##################################################
##################################################
########### similar_outgroup_cl_dummy ############
##################################################
##################################################

temp <- df_soc_learn_4round_zurich[(df_soc_learn_4round_zurich$similar_outgroup_cl_dummy == 1), ]
socLearnersPresent <- as.numeric(levels(as.factor(temp$uniqueSubjectNum)))

perfectMajority <- rep(0,length(socLearnersPresent))
perfectMinority <- rep(0,length(socLearnersPresent))
unconditional <- rep(0,length(socLearnersPresent))
beta <- rep(NA,length(socLearnersPresent))
pValueBeta <- rep(NA,length(socLearnersPresent))

for (i in 1:length(socLearnersPresent))
{
  if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i]]) == 0 | mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i]]) == 1)
  {
    perfectMajority[i] <- 0
    perfectMinority[i] <- 0
    unconditional[i] <- 1
  }
  else if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered < 0]) == 0 & mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered > 0]) == 1)
  {
    perfectMajority[i] <- 1
    perfectMinority[i] <- 0
    unconditional[i] <- 0
  }
  else if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered < 0]) == 1 & mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered > 0]) == 0)
  {
    perfectMajority[i] <- 0
    perfectMinority[i] <- 1
    unconditional[i] <- 0
  }
  else
  {
    res <- glm(urn_choice ~ propIndRightCentered + 0, family = binomial(link = 'logit'), data = temp[temp$uniqueSubjectNum == socLearnersPresent[i], ])
    beta[i] <- coef(summary(res))[ ,1]
    pValueBeta[i] <- coef(summary(res))[ ,4]
  }
}

similar_outgroup_cl_dummy_pure <- data.frame(socLearnersPresent,perfectMajority,perfectMinority,unconditional,beta,pValueBeta)

##################################################
##################################################
########### dissimilar_outgroup_cl_dummy #########
##################################################
##################################################

temp <- df_soc_learn_4round_zurich[(df_soc_learn_4round_zurich$dissimilar_outgroup_cl_dummy == 1), ]
socLearnersPresent <- as.numeric(levels(as.factor(temp$uniqueSubjectNum)))

perfectMajority <- rep(0,length(socLearnersPresent))
perfectMinority <- rep(0,length(socLearnersPresent))
unconditional <- rep(0,length(socLearnersPresent))
beta <- rep(NA,length(socLearnersPresent))
pValueBeta <- rep(NA,length(socLearnersPresent))

for (i in 1:length(socLearnersPresent))
{
  if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i]]) == 0 | mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i]]) == 1)
  {
    perfectMajority[i] <- 0
    perfectMinority[i] <- 0
    unconditional[i] <- 1
  }
  else if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered < 0]) == 0 & mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered > 0]) == 1)
  {
    perfectMajority[i] <- 1
    perfectMinority[i] <- 0
    unconditional[i] <- 0
  }
  else if (mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered < 0]) == 1 & mean(temp$urn_choice[temp$uniqueSubjectNum == socLearnersPresent[i] & temp$propIndRightCentered > 0]) == 0)
  {
    perfectMajority[i] <- 0
    perfectMinority[i] <- 1
    unconditional[i] <- 0
  }
  else
  {
    res <- glm(urn_choice ~ propIndRightCentered + 0, family = binomial(link = 'logit'), data = temp[temp$uniqueSubjectNum == socLearnersPresent[i], ])
    beta[i] <- coef(summary(res))[ ,1]
    pValueBeta[i] <- coef(summary(res))[ ,4]
  }
}

dissimilar_outgroup_cl_dummy_pure <- data.frame(socLearnersPresent,perfectMajority,perfectMinority,unconditional,beta,pValueBeta)

##################################################
##################################################
##################################################
# PLOT HISTOGRAMS
##################################################
##################################################
##################################################

dir_name <- 'socLearningFunctionsDisaggregated'

# Check if directory exists
if (!dir.exists(dir_name)) {
  # Create directory if it does not exist
  dir.create(dir_name)
}

# Set the working directory to the specified directory
setwd(dir_name)

##################################################
##################################################
# ingroup similar and dissimilar, ncl
##################################################
##################################################
betaThresholds_amb <- seq(from = ambMinBeta, to = ambMaxBeta, by = 1)
similar_ingroup_ncl_dummy_betaNotSig <- rep(0,length(betaThresholds_amb))
similar_ingroup_ncl_dummy_betaSig <- rep(0,length(betaThresholds_amb))
dissimilar_ingroup_ncl_dummy_betaNotSig <- rep(0,length(betaThresholds_amb))
dissimilar_ingroup_ncl_dummy_betaSig <- rep(0,length(betaThresholds_amb))

similar_ingroup_ncl_dummy_betaNotSig[1] <- sum(is.na(similar_ingroup_ncl_dummy_pure$beta) == F & similar_ingroup_ncl_dummy_pure$beta <= betaThresholds_amb[2] & similar_ingroup_ncl_dummy_pure$pValueBeta > pCrit)
similar_ingroup_ncl_dummy_betaSig[1] <- sum(is.na(similar_ingroup_ncl_dummy_pure$beta) == F & similar_ingroup_ncl_dummy_pure$beta <= betaThresholds_amb[2] & similar_ingroup_ncl_dummy_pure$pValueBeta <= pCrit)
dissimilar_ingroup_ncl_dummy_betaNotSig[1] <- sum(is.na(dissimilar_ingroup_ncl_dummy_pure$beta) == F & dissimilar_ingroup_ncl_dummy_pure$beta <= betaThresholds_amb[2] & dissimilar_ingroup_ncl_dummy_pure$pValueBeta > pCrit)
dissimilar_ingroup_ncl_dummy_betaSig[1] <- sum(is.na(dissimilar_ingroup_ncl_dummy_pure$beta) == F & dissimilar_ingroup_ncl_dummy_pure$beta <= betaThresholds_amb[2] & dissimilar_ingroup_ncl_dummy_pure$pValueBeta <= pCrit)


for (i in 2:(length(betaThresholds_amb) - 1))
{
  similar_ingroup_ncl_dummy_betaNotSig[i] <- sum(is.na(similar_ingroup_ncl_dummy_pure$beta) == F & similar_ingroup_ncl_dummy_pure$beta > betaThresholds_amb[i] & similar_ingroup_ncl_dummy_pure$beta <= betaThresholds_amb[i+1] & similar_ingroup_ncl_dummy_pure$pValueBeta > pCrit)
  similar_ingroup_ncl_dummy_betaSig[i] <- sum(is.na(similar_ingroup_ncl_dummy_pure$beta) == F & similar_ingroup_ncl_dummy_pure$beta > betaThresholds_amb[i] & similar_ingroup_ncl_dummy_pure$beta <= betaThresholds_amb[i+1] & similar_ingroup_ncl_dummy_pure$pValueBeta <= pCrit)
  dissimilar_ingroup_ncl_dummy_betaNotSig[i] <- sum(is.na(dissimilar_ingroup_ncl_dummy_pure$beta) == F & dissimilar_ingroup_ncl_dummy_pure$beta > betaThresholds_amb[i] & dissimilar_ingroup_ncl_dummy_pure$beta <= betaThresholds_amb[i+1] & dissimilar_ingroup_ncl_dummy_pure$pValueBeta > pCrit)
  dissimilar_ingroup_ncl_dummy_betaSig[i] <- sum(is.na(dissimilar_ingroup_ncl_dummy_pure$beta) == F & dissimilar_ingroup_ncl_dummy_pure$beta > betaThresholds_amb[i] & dissimilar_ingroup_ncl_dummy_pure$beta <= betaThresholds_amb[i+1] & dissimilar_ingroup_ncl_dummy_pure$pValueBeta <= pCrit)
}

yMax <- max(similar_ingroup_ncl_dummy_betaNotSig,similar_ingroup_ncl_dummy_betaSig,dissimilar_ingroup_ncl_dummy_betaNotSig,dissimilar_ingroup_ncl_dummy_betaSig,sum(similar_ingroup_ncl_dummy_pure$perfectMinority == 1),sum(similar_ingroup_ncl_dummy_pure$perfectMajority == 1),sum(similar_ingroup_ncl_dummy_pure$uncoditional == 1),sum(dissimilar_ingroup_ncl_dummy_pure$perfectMinority == 1),sum(dissimilar_ingroup_ncl_dummy_pure$perfectMajority == 1),sum(dissimilar_ingroup_ncl_dummy_pure$uncoditional == 1))

quartz(width = widthWindow, height = heightWindow, type='pdf', file='plotNCLSimDisIngroupBySocialLearner_zurich.pdf')
par(mar=c(5.5,5.5,4,3),mgp=c(3.5,1,0),cex.lab=cexNum,cex.axis=1.2*cexNum,fig=c(0,1,0.5,1))
plot(c(ambMinBeta,ambMaxBeta),c(0,0),xlim=c(ambMinBeta-4,ambMaxBeta+8),ylim=c(0,yMax),type='l',lty='solid',lwd=2,xlab='',ylab='Count',axes=F,cex.lab=1.75*cexNum)
axis(1,c(ambMinBeta-3,seq(from=ambMinBeta,to=ambMaxBeta,by=5),ambMaxBeta+3,ambMaxBeta+7),c('Min',seq(from=ambMinBeta,to=ambMaxBeta,by=5),'Maj','U'),cex.axis=1.75*cexNum)
axis(2,seq(from=0,to=yMax+1,by=2),seq(from=0,to=yMax+1,by=2),cex.axis=1.75*cexNum)
box()
mtext('NCL, Ingroup, Similar',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('A',side=3,line=1.5,at=ambMinBeta - 9,cex=2*cexNum)
mtext(expression(italic(hat(beta)[k])),side=1,line=4.5,at=0,adj=0.5,cex=2*cexNum)

for (i in 1:(length(betaThresholds_amb) - 1))
{
  if (sum(similar_ingroup_ncl_dummy_pure$perfectMinority == 1) > 0)
  {
    rect(ambMinBeta-3.75,0,ambMinBeta-2.25,sum(similar_ingroup_ncl_dummy_pure$perfectMinority == 1),lwd=0.75,angle=45,density=5)
  }
  if (sum(similar_ingroup_ncl_dummy_pure$perfectMajority == 1) > 0)
  {
    rect(ambMaxBeta+2.25,0,ambMaxBeta+3.75,sum(similar_ingroup_ncl_dummy_pure$perfectMajority == 1),lwd=0.75,angle=45,density=5)
  }
  if (sum(similar_ingroup_ncl_dummy_pure$unconditional == 1) > 0)
  {
    rect(ambMaxBeta+6.25,0,ambMaxBeta+7.75,sum(similar_ingroup_ncl_dummy_pure$unconditional == 1),lwd=1,col='black')
  }
  if (similar_ingroup_ncl_dummy_betaSig[i] > 0)
  {
    rect(betaThresholds_amb[i],0,betaThresholds_amb[i+1],similar_ingroup_ncl_dummy_betaSig[i],lwd=1.5,col=gray(0.8))
  }
  if (similar_ingroup_ncl_dummy_betaNotSig[i] > 0)
  {
    rect(betaThresholds_amb[i],similar_ingroup_ncl_dummy_betaSig[i],betaThresholds_amb[i+1],similar_ingroup_ncl_dummy_betaSig[i] + similar_ingroup_ncl_dummy_betaNotSig[i],lwd=1.5)
  }
}

par(fig=c(0,1,0,0.5),new=T)
plot(c(ambMinBeta,ambMaxBeta),c(0,0),xlim=c(ambMinBeta-4,ambMaxBeta+8),ylim=c(0,yMax),type='l',lty='solid',lwd=2,xlab='',ylab='Count',axes=F,cex.lab=1.75*cexNum)
axis(1,c(ambMinBeta-3,seq(from=ambMinBeta,to=ambMaxBeta,by=5),ambMaxBeta+3,ambMaxBeta+7),c('Min',seq(from=ambMinBeta,to=ambMaxBeta,by=5),'Maj','U'),cex.axis=1.75*cexNum)
axis(2,seq(from=0,to=yMax+1,by=2),seq(from=0,to=yMax+1,by=2),cex.axis=1.75*cexNum)
box()
mtext('NCL, Ingroup, Dissimilar',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('B',side=3,line=1.5,at=ambMinBeta - 9,cex=2*cexNum)
mtext(expression(italic(hat(beta)[k])),side=1,line=4.5,at=0,adj=0.5,cex=2*cexNum)

for (i in 1:(length(betaThresholds_amb) - 1))
{
  if (sum(dissimilar_ingroup_ncl_dummy_pure$perfectMinority == 1) > 0)
  {
    rect(ambMinBeta-3.75,0,ambMinBeta-2.25,sum(dissimilar_ingroup_ncl_dummy_pure$perfectMinority == 1),lwd=0.75,angle=45,density=5)
  }
  if (sum(dissimilar_ingroup_ncl_dummy_pure$perfectMajority == 1) > 0)
  {
    rect(ambMaxBeta+2.25,0,ambMaxBeta+3.75,sum(dissimilar_ingroup_ncl_dummy_pure$perfectMajority == 1),lwd=0.75,angle=45,density=5)
  }
  if (sum(dissimilar_ingroup_ncl_dummy_pure$unconditional == 1) > 0)
  {
    rect(ambMaxBeta+6.25,0,ambMaxBeta+7.75,sum(dissimilar_ingroup_ncl_dummy_pure$unconditional == 1),lwd=1,col='black')
  }
  if (dissimilar_ingroup_ncl_dummy_betaSig[i] > 0)
  {
    rect(betaThresholds_amb[i],0,betaThresholds_amb[i+1],dissimilar_ingroup_ncl_dummy_betaSig[i],lwd=1.5,col=gray(0.8))
  }
  if (dissimilar_ingroup_ncl_dummy_betaNotSig[i] > 0)
  {
    rect(betaThresholds_amb[i],dissimilar_ingroup_ncl_dummy_betaSig[i],betaThresholds_amb[i+1],dissimilar_ingroup_ncl_dummy_betaSig[i] + dissimilar_ingroup_ncl_dummy_betaNotSig[i],lwd=1.5)
  }
}

dev.off()



##################################################
##################################################
# outgroup similar and dissimilar, ncl
##################################################
##################################################
betaThresholds_amb <- seq(from = ambMinBeta, to = ambMaxBeta, by = 1)
similar_outgroup_ncl_dummy_betaNotSig <- rep(0,length(betaThresholds_amb))
similar_outgroup_ncl_dummy_betaSig <- rep(0,length(betaThresholds_amb))
dissimilar_outgroup_ncl_dummy_betaNotSig <- rep(0,length(betaThresholds_amb))
dissimilar_outgroup_ncl_dummy_betaSig <- rep(0,length(betaThresholds_amb))

similar_outgroup_ncl_dummy_betaNotSig[1] <- sum(is.na(similar_outgroup_ncl_dummy_pure$beta) == F & similar_outgroup_ncl_dummy_pure$beta <= betaThresholds_amb[2] & similar_outgroup_ncl_dummy_pure$pValueBeta > pCrit)
similar_outgroup_ncl_dummy_betaSig[1] <- sum(is.na(similar_outgroup_ncl_dummy_pure$beta) == F & similar_outgroup_ncl_dummy_pure$beta <= betaThresholds_amb[2] & similar_outgroup_ncl_dummy_pure$pValueBeta <= pCrit)
dissimilar_outgroup_ncl_dummy_betaNotSig[1] <- sum(is.na(dissimilar_outgroup_ncl_dummy_pure$beta) == F & dissimilar_outgroup_ncl_dummy_pure$beta <= betaThresholds_amb[2] & dissimilar_outgroup_ncl_dummy_pure$pValueBeta > pCrit)
dissimilar_outgroup_ncl_dummy_betaSig[1] <- sum(is.na(dissimilar_outgroup_ncl_dummy_pure$beta) == F & dissimilar_outgroup_ncl_dummy_pure$beta <= betaThresholds_amb[2] & dissimilar_outgroup_ncl_dummy_pure$pValueBeta <= pCrit)


for (i in 2:(length(betaThresholds_amb) - 1))
{
  similar_outgroup_ncl_dummy_betaNotSig[i] <- sum(is.na(similar_outgroup_ncl_dummy_pure$beta) == F & similar_outgroup_ncl_dummy_pure$beta > betaThresholds_amb[i] & similar_outgroup_ncl_dummy_pure$beta <= betaThresholds_amb[i+1] & similar_outgroup_ncl_dummy_pure$pValueBeta > pCrit)
  similar_outgroup_ncl_dummy_betaSig[i] <- sum(is.na(similar_outgroup_ncl_dummy_pure$beta) == F & similar_outgroup_ncl_dummy_pure$beta > betaThresholds_amb[i] & similar_outgroup_ncl_dummy_pure$beta <= betaThresholds_amb[i+1] & similar_outgroup_ncl_dummy_pure$pValueBeta <= pCrit)
  dissimilar_outgroup_ncl_dummy_betaNotSig[i] <- sum(is.na(dissimilar_outgroup_ncl_dummy_pure$beta) == F & dissimilar_outgroup_ncl_dummy_pure$beta > betaThresholds_amb[i] & dissimilar_outgroup_ncl_dummy_pure$beta <= betaThresholds_amb[i+1] & dissimilar_outgroup_ncl_dummy_pure$pValueBeta > pCrit)
  dissimilar_outgroup_ncl_dummy_betaSig[i] <- sum(is.na(dissimilar_outgroup_ncl_dummy_pure$beta) == F & dissimilar_outgroup_ncl_dummy_pure$beta > betaThresholds_amb[i] & dissimilar_outgroup_ncl_dummy_pure$beta <= betaThresholds_amb[i+1] & dissimilar_outgroup_ncl_dummy_pure$pValueBeta <= pCrit)
}

yMax <- max(similar_ingroup_ncl_dummy_betaNotSig,similar_ingroup_ncl_dummy_betaSig,dissimilar_ingroup_ncl_dummy_betaNotSig,dissimilar_ingroup_ncl_dummy_betaSig,sum(similar_outgroup_ncl_dummy_pure$perfectMinority == 1),sum(similar_outgroup_ncl_dummy_pure$perfectMajority == 1),sum(similar_outgroup_ncl_dummy_pure$uncoditional == 1),sum(dissimilar_outgroup_ncl_dummy_pure$perfectMinority == 1),sum(dissimilar_outgroup_ncl_dummy_pure$perfectMajority == 1),sum(dissimilar_outgroup_ncl_dummy_pure$uncoditional == 1))

quartz(width = widthWindow, height = heightWindow, type='pdf', file='plotNCLSimDisOutgroupBySocialLearner_zurich.pdf')
par(mar=c(5.5,5.5,4,3),mgp=c(3.5,1,0),cex.lab=cexNum,cex.axis=1.2*cexNum,fig=c(0,1,0.5,1))
plot(c(ambMinBeta,ambMaxBeta),c(0,0),xlim=c(ambMinBeta-4,ambMaxBeta+8),ylim=c(0,yMax),type='l',lty='solid',lwd=2,xlab='',ylab='Count',axes=F,cex.lab=1.75*cexNum)
axis(1,c(ambMinBeta-3,seq(from=ambMinBeta,to=ambMaxBeta,by=5),ambMaxBeta+3,ambMaxBeta+7),c('Min',seq(from=ambMinBeta,to=ambMaxBeta,by=5),'Maj','U'),cex.axis=1.75*cexNum)
axis(2,seq(from=0,to=yMax+1,by=2),seq(from=0,to=yMax+1,by=2),cex.axis=1.75*cexNum)
box()
mtext('NCL, Outgroup, Similar',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('A',side=3,line=1.5,at=ambMinBeta - 9,cex=2*cexNum)
mtext(expression(italic(hat(beta)[k])),side=1,line=4.5,at=0,adj=0.5,cex=2*cexNum)

for (i in 1:(length(betaThresholds_amb) - 1))
{
  if (sum(similar_outgroup_ncl_dummy_pure$perfectMinority == 1) > 0)
  {
    rect(ambMinBeta-3.75,0,ambMinBeta-2.25,sum(similar_outgroup_ncl_dummy_pure$perfectMinority == 1),lwd=0.75,angle=45,density=5)
  }
  if (sum(similar_outgroup_ncl_dummy_pure$perfectMajority == 1) > 0)
  {
    rect(ambMaxBeta+2.25,0,ambMaxBeta+3.75,sum(similar_outgroup_ncl_dummy_pure$perfectMajority == 1),lwd=0.75,angle=45,density=5)
  }
  if (sum(similar_outgroup_ncl_dummy_pure$unconditional == 1) > 0)
  {
    rect(ambMaxBeta+6.25,0,ambMaxBeta+7.75,sum(similar_outgroup_ncl_dummy_pure$unconditional == 1),lwd=1,col='black')
  }
  if (similar_outgroup_ncl_dummy_betaSig[i] > 0)
  {
    rect(betaThresholds_amb[i],0,betaThresholds_amb[i+1],similar_outgroup_ncl_dummy_betaSig[i],lwd=1.5,col=gray(0.8))
  }
  if (similar_outgroup_ncl_dummy_betaNotSig[i] > 0)
  {
    rect(betaThresholds_amb[i],similar_outgroup_ncl_dummy_betaSig[i],betaThresholds_amb[i+1],similar_outgroup_ncl_dummy_betaSig[i] + similar_outgroup_ncl_dummy_betaNotSig[i],lwd=1.5)
  }
}

par(fig=c(0,1,0,0.5),new=T)
plot(c(ambMinBeta,ambMaxBeta),c(0,0),xlim=c(ambMinBeta-4,ambMaxBeta+8),ylim=c(0,yMax),type='l',lty='solid',lwd=2,xlab='',ylab='Count',axes=F,cex.lab=1.75*cexNum)
axis(1,c(ambMinBeta-3,seq(from=ambMinBeta,to=ambMaxBeta,by=5),ambMaxBeta+3,ambMaxBeta+7),c('Min',seq(from=ambMinBeta,to=ambMaxBeta,by=5),'Maj','U'),cex.axis=1.75*cexNum)
axis(2,seq(from=0,to=yMax+1,by=2),seq(from=0,to=yMax+1,by=2),cex.axis=1.75*cexNum)
box()
mtext('NCL, Outgroup, Dissimilar',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('B',side=3,line=1.5,at=ambMinBeta - 9,cex=2*cexNum)
mtext(expression(italic(hat(beta)[k])),side=1,line=4.5,at=0,adj=0.5,cex=2*cexNum)

for (i in 1:(length(betaThresholds_amb) - 1))
{
  if (sum(dissimilar_outgroup_ncl_dummy_pure$perfectMinority == 1) > 0)
  {
    rect(ambMinBeta-3.75,0,ambMinBeta-2.25,sum(dissimilar_outgroup_ncl_dummy_pure$perfectMinority == 1),lwd=0.75,angle=45,density=5)
  }
  if (sum(dissimilar_outgroup_ncl_dummy_pure$perfectMajority == 1) > 0)
  {
    rect(ambMaxBeta+2.25,0,ambMaxBeta+3.75,sum(dissimilar_outgroup_ncl_dummy_pure$perfectMajority == 1),lwd=0.75,angle=45,density=5)
  }
  if (sum(dissimilar_outgroup_ncl_dummy_pure$unconditional == 1) > 0)
  {
    rect(ambMaxBeta+6.25,0,ambMaxBeta+7.75,sum(dissimilar_outgroup_ncl_dummy_pure$unconditional == 1),lwd=1,col='black')
  }
  if (dissimilar_outgroup_ncl_dummy_betaSig[i] > 0)
  {
    rect(betaThresholds_amb[i],0,betaThresholds_amb[i+1],dissimilar_outgroup_ncl_dummy_betaSig[i],lwd=1.5,col=gray(0.8))
  }
  if (dissimilar_outgroup_ncl_dummy_betaNotSig[i] > 0)
  {
    rect(betaThresholds_amb[i],dissimilar_outgroup_ncl_dummy_betaSig[i],betaThresholds_amb[i+1],dissimilar_outgroup_ncl_dummy_betaSig[i] + dissimilar_outgroup_ncl_dummy_betaNotSig[i],lwd=1.5)
  }
}

dev.off()


################################################################################
################################################################################
# Now with cognitive load:
################################################################################
################################################################################

##################################################
##################################################
# ingroup similar and dissimilar, cl
##################################################
##################################################
betaThresholds_amb <- seq(from = ambMinBeta, to = ambMaxBeta, by = 1)
similar_ingroup_cl_dummy_betaNotSig <- rep(0,length(betaThresholds_amb))
similar_ingroup_cl_dummy_betaSig <- rep(0,length(betaThresholds_amb))
dissimilar_ingroup_cl_dummy_betaNotSig <- rep(0,length(betaThresholds_amb))
dissimilar_ingroup_cl_dummy_betaSig <- rep(0,length(betaThresholds_amb))

similar_ingroup_cl_dummy_betaNotSig[1] <- sum(is.na(similar_ingroup_cl_dummy_pure$beta) == F & similar_ingroup_cl_dummy_pure$beta <= betaThresholds_amb[2] & similar_ingroup_cl_dummy_pure$pValueBeta > pCrit)
similar_ingroup_cl_dummy_betaSig[1] <- sum(is.na(similar_ingroup_cl_dummy_pure$beta) == F & similar_ingroup_cl_dummy_pure$beta <= betaThresholds_amb[2] & similar_ingroup_cl_dummy_pure$pValueBeta <= pCrit)
dissimilar_ingroup_cl_dummy_betaNotSig[1] <- sum(is.na(dissimilar_ingroup_cl_dummy_pure$beta) == F & dissimilar_ingroup_cl_dummy_pure$beta <= betaThresholds_amb[2] & dissimilar_ingroup_cl_dummy_pure$pValueBeta > pCrit)
dissimilar_ingroup_cl_dummy_betaSig[1] <- sum(is.na(dissimilar_ingroup_cl_dummy_pure$beta) == F & dissimilar_ingroup_cl_dummy_pure$beta <= betaThresholds_amb[2] & dissimilar_ingroup_cl_dummy_pure$pValueBeta <= pCrit)


for (i in 2:(length(betaThresholds_amb) - 1))
{
  similar_ingroup_cl_dummy_betaNotSig[i] <- sum(is.na(similar_ingroup_cl_dummy_pure$beta) == F & similar_ingroup_cl_dummy_pure$beta > betaThresholds_amb[i] & similar_ingroup_cl_dummy_pure$beta <= betaThresholds_amb[i+1] & similar_ingroup_cl_dummy_pure$pValueBeta > pCrit)
  similar_ingroup_cl_dummy_betaSig[i] <- sum(is.na(similar_ingroup_cl_dummy_pure$beta) == F & similar_ingroup_cl_dummy_pure$beta > betaThresholds_amb[i] & similar_ingroup_cl_dummy_pure$beta <= betaThresholds_amb[i+1] & similar_ingroup_cl_dummy_pure$pValueBeta <= pCrit)
  dissimilar_ingroup_cl_dummy_betaNotSig[i] <- sum(is.na(dissimilar_ingroup_cl_dummy_pure$beta) == F & dissimilar_ingroup_cl_dummy_pure$beta > betaThresholds_amb[i] & dissimilar_ingroup_cl_dummy_pure$beta <= betaThresholds_amb[i+1] & dissimilar_ingroup_cl_dummy_pure$pValueBeta > pCrit)
  dissimilar_ingroup_cl_dummy_betaSig[i] <- sum(is.na(dissimilar_ingroup_cl_dummy_pure$beta) == F & dissimilar_ingroup_cl_dummy_pure$beta > betaThresholds_amb[i] & dissimilar_ingroup_cl_dummy_pure$beta <= betaThresholds_amb[i+1] & dissimilar_ingroup_cl_dummy_pure$pValueBeta <= pCrit)
}

yMax <- max(similar_ingroup_cl_dummy_betaNotSig,similar_ingroup_cl_dummy_betaSig,dissimilar_ingroup_cl_dummy_betaNotSig,dissimilar_ingroup_cl_dummy_betaSig,sum(similar_ingroup_cl_dummy_pure$perfectMinority == 1),sum(similar_ingroup_cl_dummy_pure$perfectMajority == 1),sum(similar_ingroup_cl_dummy_pure$uncoditional == 1),sum(dissimilar_ingroup_cl_dummy_pure$perfectMinority == 1),sum(dissimilar_ingroup_cl_dummy_pure$perfectMajority == 1),sum(dissimilar_ingroup_cl_dummy_pure$uncoditional == 1))

quartz(width = widthWindow, height = heightWindow, type='pdf', file='plotCLSimDisIngroupBySocialLearner_zurich.pdf')
par(mar=c(5.5,5.5,4,3),mgp=c(3.5,1,0),cex.lab=cexNum,cex.axis=1.2*cexNum,fig=c(0,1,0.5,1))
plot(c(ambMinBeta,ambMaxBeta),c(0,0),xlim=c(ambMinBeta-4,ambMaxBeta+8),ylim=c(0,yMax),type='l',lty='solid',lwd=2,xlab='',ylab='Count',axes=F,cex.lab=1.75*cexNum)
axis(1,c(ambMinBeta-3,seq(from=ambMinBeta,to=ambMaxBeta,by=5),ambMaxBeta+3,ambMaxBeta+7),c('Min',seq(from=ambMinBeta,to=ambMaxBeta,by=5),'Maj','U'),cex.axis=1.75*cexNum)
axis(2,seq(from=0,to=yMax+1,by=2),seq(from=0,to=yMax+1,by=2),cex.axis=1.75*cexNum)
box()
mtext('CL, Ingroup, Similar',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('A',side=3,line=1.5,at=ambMinBeta - 9,cex=2*cexNum)
mtext(expression(italic(hat(beta)[k])),side=1,line=4.5,at=0,adj=0.5,cex=2*cexNum)

for (i in 1:(length(betaThresholds_amb) - 1))
{
  if (sum(similar_ingroup_cl_dummy_pure$perfectMinority == 1) > 0)
  {
    rect(ambMinBeta-3.75,0,ambMinBeta-2.25,sum(similar_ingroup_cl_dummy_pure$perfectMinority == 1),lwd=0.75,angle=45,density=5)
  }
  if (sum(similar_ingroup_cl_dummy_pure$perfectMajority == 1) > 0)
  {
    rect(ambMaxBeta+2.25,0,ambMaxBeta+3.75,sum(similar_ingroup_cl_dummy_pure$perfectMajority == 1),lwd=0.75,angle=45,density=5)
  }
  if (sum(similar_ingroup_cl_dummy_pure$unconditional == 1) > 0)
  {
    rect(ambMaxBeta+6.25,0,ambMaxBeta+7.75,sum(similar_ingroup_cl_dummy_pure$unconditional == 1),lwd=1,col='black')
  }
  if (similar_ingroup_cl_dummy_betaSig[i] > 0)
  {
    rect(betaThresholds_amb[i],0,betaThresholds_amb[i+1],similar_ingroup_cl_dummy_betaSig[i],lwd=1.5,col=gray(0.8))
  }
  if (similar_ingroup_cl_dummy_betaNotSig[i] > 0)
  {
    rect(betaThresholds_amb[i],similar_ingroup_cl_dummy_betaSig[i],betaThresholds_amb[i+1],similar_ingroup_cl_dummy_betaSig[i] + similar_ingroup_cl_dummy_betaNotSig[i],lwd=1.5)
  }
}

par(fig=c(0,1,0,0.5),new=T)
plot(c(ambMinBeta,ambMaxBeta),c(0,0),xlim=c(ambMinBeta-4,ambMaxBeta+8),ylim=c(0,yMax),type='l',lty='solid',lwd=2,xlab='',ylab='Count',axes=F,cex.lab=1.75*cexNum)
axis(1,c(ambMinBeta-3,seq(from=ambMinBeta,to=ambMaxBeta,by=5),ambMaxBeta+3,ambMaxBeta+7),c('Min',seq(from=ambMinBeta,to=ambMaxBeta,by=5),'Maj','U'),cex.axis=1.75*cexNum)
axis(2,seq(from=0,to=yMax+1,by=2),seq(from=0,to=yMax+1,by=2),cex.axis=1.75*cexNum)
box()
mtext('CL, Ingroup, Dissimilar',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('B',side=3,line=1.5,at=ambMinBeta - 9,cex=2*cexNum)
mtext(expression(italic(hat(beta)[k])),side=1,line=4.5,at=0,adj=0.5,cex=2*cexNum)

for (i in 1:(length(betaThresholds_amb) - 1))
{
  if (sum(dissimilar_ingroup_cl_dummy_pure$perfectMinority == 1) > 0)
  {
    rect(ambMinBeta-3.75,0,ambMinBeta-2.25,sum(dissimilar_ingroup_cl_dummy_pure$perfectMinority == 1),lwd=0.75,angle=45,density=5)
  }
  if (sum(dissimilar_ingroup_cl_dummy_pure$perfectMajority == 1) > 0)
  {
    rect(ambMaxBeta+2.25,0,ambMaxBeta+3.75,sum(dissimilar_ingroup_cl_dummy_pure$perfectMajority == 1),lwd=0.75,angle=45,density=5)
  }
  if (sum(dissimilar_ingroup_cl_dummy_pure$unconditional == 1) > 0)
  {
    rect(ambMaxBeta+6.25,0,ambMaxBeta+7.75,sum(dissimilar_ingroup_cl_dummy_pure$unconditional == 1),lwd=1,col='black')
  }
  if (dissimilar_ingroup_cl_dummy_betaSig[i] > 0)
  {
    rect(betaThresholds_amb[i],0,betaThresholds_amb[i+1],dissimilar_ingroup_cl_dummy_betaSig[i],lwd=1.5,col=gray(0.8))
  }
  if (dissimilar_ingroup_cl_dummy_betaNotSig[i] > 0)
  {
    rect(betaThresholds_amb[i],dissimilar_ingroup_cl_dummy_betaSig[i],betaThresholds_amb[i+1],dissimilar_ingroup_cl_dummy_betaSig[i] + dissimilar_ingroup_cl_dummy_betaNotSig[i],lwd=1.5)
  }
}

dev.off()


##################################################
##################################################
# outgroup similar and dissimilar, cl
##################################################
##################################################
betaThresholds_amb <- seq(from = ambMinBeta, to = ambMaxBeta, by = 1)
similar_outgroup_cl_dummy_betaNotSig <- rep(0,length(betaThresholds_amb))
similar_outgroup_cl_dummy_betaSig <- rep(0,length(betaThresholds_amb))
dissimilar_outgroup_cl_dummy_betaNotSig <- rep(0,length(betaThresholds_amb))
dissimilar_outgroup_cl_dummy_betaSig <- rep(0,length(betaThresholds_amb))

similar_outgroup_cl_dummy_betaNotSig[1] <- sum(is.na(similar_outgroup_cl_dummy_pure$beta) == F & similar_outgroup_cl_dummy_pure$beta <= betaThresholds_amb[2] & similar_outgroup_cl_dummy_pure$pValueBeta > pCrit)
similar_outgroup_cl_dummy_betaSig[1] <- sum(is.na(similar_outgroup_cl_dummy_pure$beta) == F & similar_outgroup_cl_dummy_pure$beta <= betaThresholds_amb[2] & similar_outgroup_cl_dummy_pure$pValueBeta <= pCrit)
dissimilar_outgroup_cl_dummy_betaNotSig[1] <- sum(is.na(dissimilar_outgroup_cl_dummy_pure$beta) == F & dissimilar_outgroup_cl_dummy_pure$beta <= betaThresholds_amb[2] & dissimilar_outgroup_cl_dummy_pure$pValueBeta > pCrit)
dissimilar_outgroup_cl_dummy_betaSig[1] <- sum(is.na(dissimilar_outgroup_cl_dummy_pure$beta) == F & dissimilar_outgroup_cl_dummy_pure$beta <= betaThresholds_amb[2] & dissimilar_outgroup_cl_dummy_pure$pValueBeta <= pCrit)


for (i in 2:(length(betaThresholds_amb) - 1))
{
  similar_outgroup_cl_dummy_betaNotSig[i] <- sum(is.na(similar_outgroup_cl_dummy_pure$beta) == F & similar_outgroup_cl_dummy_pure$beta > betaThresholds_amb[i] & similar_outgroup_cl_dummy_pure$beta <= betaThresholds_amb[i+1] & similar_outgroup_cl_dummy_pure$pValueBeta > pCrit)
  similar_outgroup_cl_dummy_betaSig[i] <- sum(is.na(similar_outgroup_cl_dummy_pure$beta) == F & similar_outgroup_cl_dummy_pure$beta > betaThresholds_amb[i] & similar_outgroup_cl_dummy_pure$beta <= betaThresholds_amb[i+1] & similar_outgroup_cl_dummy_pure$pValueBeta <= pCrit)
  dissimilar_outgroup_cl_dummy_betaNotSig[i] <- sum(is.na(dissimilar_outgroup_cl_dummy_pure$beta) == F & dissimilar_outgroup_cl_dummy_pure$beta > betaThresholds_amb[i] & dissimilar_outgroup_cl_dummy_pure$beta <= betaThresholds_amb[i+1] & dissimilar_outgroup_cl_dummy_pure$pValueBeta > pCrit)
  dissimilar_outgroup_cl_dummy_betaSig[i] <- sum(is.na(dissimilar_outgroup_cl_dummy_pure$beta) == F & dissimilar_outgroup_cl_dummy_pure$beta > betaThresholds_amb[i] & dissimilar_outgroup_cl_dummy_pure$beta <= betaThresholds_amb[i+1] & dissimilar_outgroup_cl_dummy_pure$pValueBeta <= pCrit)
}

yMax <- max(similar_ingroup_cl_dummy_betaNotSig,similar_ingroup_cl_dummy_betaSig,dissimilar_ingroup_cl_dummy_betaNotSig,dissimilar_ingroup_cl_dummy_betaSig,sum(similar_outgroup_cl_dummy_pure$perfectMinority == 1),sum(similar_outgroup_cl_dummy_pure$perfectMajority == 1),sum(similar_outgroup_cl_dummy_pure$uncoditional == 1),sum(dissimilar_outgroup_cl_dummy_pure$perfectMinority == 1),sum(dissimilar_outgroup_cl_dummy_pure$perfectMajority == 1),sum(dissimilar_outgroup_cl_dummy_pure$uncoditional == 1))

quartz(width = widthWindow, height = heightWindow, type='pdf', file='plotCLSimDisOutgroupBySocialLearner_zurich.pdf')
par(mar=c(5.5,5.5,4,3),mgp=c(3.5,1,0),cex.lab=cexNum,cex.axis=1.2*cexNum,fig=c(0,1,0.5,1))
plot(c(ambMinBeta,ambMaxBeta),c(0,0),xlim=c(ambMinBeta-4,ambMaxBeta+8),ylim=c(0,yMax),type='l',lty='solid',lwd=2,xlab='',ylab='Count',axes=F,cex.lab=1.75*cexNum)
axis(1,c(ambMinBeta-3,seq(from=ambMinBeta,to=ambMaxBeta,by=5),ambMaxBeta+3,ambMaxBeta+7),c('Min',seq(from=ambMinBeta,to=ambMaxBeta,by=5),'Maj','U'),cex.axis=1.75*cexNum)
axis(2,seq(from=0,to=yMax+1,by=2),seq(from=0,to=yMax+1,by=2),cex.axis=1.75*cexNum)
box()
mtext('CL, Outgroup, Similar',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('A',side=3,line=1.5,at=ambMinBeta - 9,cex=2*cexNum)
mtext(expression(italic(hat(beta)[k])),side=1,line=4.5,at=0,adj=0.5,cex=2*cexNum)

for (i in 1:(length(betaThresholds_amb) - 1))
{
  if (sum(similar_outgroup_cl_dummy_pure$perfectMinority == 1) > 0)
  {
    rect(ambMinBeta-3.75,0,ambMinBeta-2.25,sum(similar_outgroup_cl_dummy_pure$perfectMinority == 1),lwd=0.75,angle=45,density=5)
  }
  if (sum(similar_outgroup_cl_dummy_pure$perfectMajority == 1) > 0)
  {
    rect(ambMaxBeta+2.25,0,ambMaxBeta+3.75,sum(similar_outgroup_cl_dummy_pure$perfectMajority == 1),lwd=0.75,angle=45,density=5)
  }
  if (sum(similar_outgroup_cl_dummy_pure$unconditional == 1) > 0)
  {
    rect(ambMaxBeta+6.25,0,ambMaxBeta+7.75,sum(similar_outgroup_cl_dummy_pure$unconditional == 1),lwd=1,col='black')
  }
  if (similar_outgroup_cl_dummy_betaSig[i] > 0)
  {
    rect(betaThresholds_amb[i],0,betaThresholds_amb[i+1],similar_outgroup_cl_dummy_betaSig[i],lwd=1.5,col=gray(0.8))
  }
  if (similar_outgroup_cl_dummy_betaNotSig[i] > 0)
  {
    rect(betaThresholds_amb[i],similar_outgroup_cl_dummy_betaSig[i],betaThresholds_amb[i+1],similar_outgroup_cl_dummy_betaSig[i] + similar_outgroup_cl_dummy_betaNotSig[i],lwd=1.5)
  }
}

par(fig=c(0,1,0,0.5),new=T)
plot(c(ambMinBeta,ambMaxBeta),c(0,0),xlim=c(ambMinBeta-4,ambMaxBeta+8),ylim=c(0,yMax),type='l',lty='solid',lwd=2,xlab='',ylab='Count',axes=F,cex.lab=1.75*cexNum)
axis(1,c(ambMinBeta-3,seq(from=ambMinBeta,to=ambMaxBeta,by=5),ambMaxBeta+3,ambMaxBeta+7),c('Min',seq(from=ambMinBeta,to=ambMaxBeta,by=5),'Maj','U'),cex.axis=1.75*cexNum)
axis(2,seq(from=0,to=yMax+1,by=2),seq(from=0,to=yMax+1,by=2),cex.axis=1.75*cexNum)
box()
mtext('CL, Outgroup, Dissimilar',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('B',side=3,line=1.5,at=ambMinBeta - 9,cex=2*cexNum)
mtext(expression(italic(hat(beta)[k])),side=1,line=4.5,at=0,adj=0.5,cex=2*cexNum)

for (i in 1:(length(betaThresholds_amb) - 1))
{
  if (sum(dissimilar_outgroup_cl_dummy_pure$perfectMinority == 1) > 0)
  {
    rect(ambMinBeta-3.75,0,ambMinBeta-2.25,sum(dissimilar_outgroup_cl_dummy_pure$perfectMinority == 1),lwd=0.75,angle=45,density=5)
  }
  if (sum(dissimilar_outgroup_cl_dummy_pure$perfectMajority == 1) > 0)
  {
    rect(ambMaxBeta+2.25,0,ambMaxBeta+3.75,sum(dissimilar_outgroup_cl_dummy_pure$perfectMajority == 1),lwd=0.75,angle=45,density=5)
  }
  if (sum(dissimilar_outgroup_cl_dummy_pure$unconditional == 1) > 0)
  {
    rect(ambMaxBeta+6.25,0,ambMaxBeta+7.75,sum(dissimilar_outgroup_cl_dummy_pure$unconditional == 1),lwd=1,col='black')
  }
  if (dissimilar_outgroup_cl_dummy_betaSig[i] > 0)
  {
    rect(betaThresholds_amb[i],0,betaThresholds_amb[i+1],dissimilar_outgroup_cl_dummy_betaSig[i],lwd=1.5,col=gray(0.8))
  }
  if (dissimilar_outgroup_cl_dummy_betaNotSig[i] > 0)
  {
    rect(betaThresholds_amb[i],dissimilar_outgroup_cl_dummy_betaSig[i],betaThresholds_amb[i+1],dissimilar_outgroup_cl_dummy_betaSig[i] + dissimilar_outgroup_cl_dummy_betaNotSig[i],lwd=1.5)
  }
}

dev.off()




setwd('..')
# rm(socLearnersPresent,perfectMajority,perfectMinority,unconditional,beta,pValueBeta,i,dataFourTmts,temp,estAndBootImitationFunctions,resEstAndBootImitationFunctions,res)
# rm(ambMinBeta,ambMaxBeta,unambMinBeta,unambMaxBeta)