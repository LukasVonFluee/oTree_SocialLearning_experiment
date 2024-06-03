mybootfunct <- function(df_soc_learn_4round,numBootstrapSamples,alpha)  {
  
  #############################################################################
  # The majority of the following code was originally created by Charles Efferson
  # for the paper "The Evolution of Facultative Conformity Based on Similarity" 
  # by Efferson et al. (2016). The code was adapted to the current study.
  #############################################################################
  
  # This routine calculates the imitation functions for social learners in each of eight treatments.  
  # The eight treatments are {similarIngroup,similarOutgroup,dissimilarIngroup,dissimilarOutgroup} X {cognitive_load,no_cognitive_load}.

  # The routine additionally calculates bootstrapped confidence intervals clustered on social learner. 
  # For each of the eight treatments, there are six possible distributions of choices by individual learners, 
  # i.e. {0/5,1/5,2/5,3/5,4/5,5/5}, and so this routine will calculate 48 CIs altogther.  
  # To do this correctly, the bootstrapping routine will resample social learners
  # with replacement to create each bootstrapped data set.  
  # As a result, each bootstrapped data set will not necessarily produce 48 parameter estimates.  
  # For each of the 48 cases, the CIs will be derived from a vector of parameter 
  # estimates that has some length that emerges endogenously from the resampling process itself.  
  # As a result, the total number of bootstrapped samples should be pretty high, e.g. 9999.
  
  # numBootstrapSamples specifies the number of bootstrap samples, and when paired with the 
  # actual sample the total number of samples should be a nice number like 10,000.
  
  # alpha will typically be 0.05 or something like that.
  
  # Create vectors recording optimality of choices (charles does that here because he didn't 
  # yet create those columns. I already create those in "data_analysis_zurich.R" and "data_analysis_busara.R"):
  # dataFourTmts$numIndLearnersOpt <- rep(0,nrow(dataFourTmts))
  # dataFourTmts$choiceOpt <- rep(-99,nrow(dataFourTmts))
  
  # The following two lines creates a column to indicate when individual learners have chosen optimally, Again, I already have that
  # dataFourTmts$numIndOpt[dataFourTmts$leftOptIndLearners == 1] <- dataFourTmts$numIndLeft[dataFourTmts$leftOptIndLearners == 1]
  # dataFourTmts$numIndOpt[dataFourTmts$leftOptIndLearners == 0] <- dataFourTmts$number_observed_IL_right_urn[dataFourTmts$leftOptIndLearners == 0]
  # dataFourTmts$choiceOpt[dataFourTmts$leftOptSelf == 1 & dataFourTmts$chooseLeft == 1] <- 1
  # dataFourTmts$choiceOpt[dataFourTmts$leftOptSelf == 1 & dataFourTmts$chooseLeft == 0] <- 0
  # dataFourTmts$choiceOpt[dataFourTmts$leftOptSelf == 0 & dataFourTmts$chooseLeft == 1] <- 0
  # dataFourTmts$choiceOpt[dataFourTmts$leftOptSelf == 0 & dataFourTmts$chooseLeft == 0] <- 1
  # dataFourTmts$choiceOpt[dataFourTmts$chooseLeft < 0] <- -99
  
  # Charles had "Left" instead of "Right". Doesn't really matter but I have "Right" in the pre-registration
  numIndLearnersRight <- rep(0:5,8) # 6 possible individual learner distributions x 8 treatments = 48 bootstrapped confidence intervals to sample
  propSocLearnersRight <- rep(NA,length(numIndLearnersRight))
  lowerCI_right <- rep(NA,length(numIndLearnersRight))
  upperCI_right <- rep(NA,length(numIndLearnersRight))
  numEstimatesRight <- rep(0,48)
  # numIndLearnersOpt <- rep(0:5,8)
  # propSocLearnersOpt <- rep(NA,length(numIndLearnersOpt))
  # lowerCI_opt <- rep(NA,length(numIndLearnersOpt))
  # upperCI_opt <- rep(NA,length(numIndLearnersOpt))
  # numEstimatesOpt <- rep(0,48)
  # The following creates all possible combinations of treatments.
  cognitive_load <- c(rep(0,24),rep(1,24))
  similar_ingroup <- c(rep(0,6),rep(0,6),rep(1,6),rep(1,6),rep(0,6),rep(0,6),rep(1,6),rep(1,6))
  observing_ingroup <- c(rep(0,6),rep(1,6),rep(0,6),rep(1,6),rep(0,6),rep(1,6),rep(0,6),rep(1,6))
  
  estimatesBootRight <- rep(NA,48 * (numBootstrapSamples + 1)) # for recording the estimates (i.e. propSocLearnersRight) from the actual data set and each of the boostrapped data sets.
  dim(estimatesBootRight) <- c(48,numBootstrapSamples + 1)
  
  # estimatesBootOpt <- rep(NA,48 * (numBootstrapSamples + 1)) # for recording the estimates (i.e. propSocLearnersOpt) from the actual data set and each of the boostrapped data sets.
  # dim(estimatesBootOpt) <- c(48,numBootstrapSamples + 1)
  
  # Here we pool over (don't yet know whether the pooling that Charles does can really be compared to a pooling over the cognitive_load vs no_cognitive_load treatments) cognitive_load/no_cognitive_load (which is between-subjects) for similar/dissimilar and ingroup/outgroup tmts, all of which are with within-subjects
  # numIndLearnersRight_pooled <- rep(0:5,2)
  # propSocLearnersRight_pooled <- rep(NA,length(numIndLearnersRight_pooled))
  # lowerCI_right_pooled <- rep(NA,length(numIndLearnersRight_pooled))
  # upperCI_right_pooled <- rep(NA,length(numIndLearnersRight_pooled))
  # numEstimatesRight_pooled <- rep(0,length(numIndLearnersRight_pooled))
  # tmts_pooled <- c(rep(0,6),rep(1,6))
  
  # estimatesBootRight_pooled <- rep(NA,12 * (numBootstrapSamples + 1)) # for recording the esimtates (i.e.propSocLearnersRight) from the actual data set and each of the bootstrapped data sets
  # dim(estimatesBootRight_pooled) <- c(12,numBootstrapSamples + 1)
  
  n_cases <- rep(NA,length(numIndLearnersRight)) 
  
  # Not pooling
  for (i in 1:length(numIndLearnersRight))
  {
    # Actual data pure behavior
    if (sum(df_soc_learn_4round$round_number %% 4 == 0 & df_soc_learn_4round$type_assignment == 'social_learner' & df_soc_learn_4round$number_observed_IL_right_urn == numIndLearnersRight[i] & df_soc_learn_4round$cognitive_load == cognitive_load[i] & df_soc_learn_4round$similar_ingroup == similar_ingroup[i] & df_soc_learn_4round$observing_ingroup == observing_ingroup[i]) > 0)
    {
      propSocLearnersRight[i] <- mean(df_soc_learn_4round$urn_choice[df_soc_learn_4round$round_number %% 4 == 0 & df_soc_learn_4round$type_assignment == 'social_learner' & df_soc_learn_4round$number_observed_IL_right_urn == numIndLearnersRight[i] & df_soc_learn_4round$cognitive_load == cognitive_load[i] & df_soc_learn_4round$similar_ingroup == similar_ingroup[i] & df_soc_learn_4round$observing_ingroup == observing_ingroup[i]])
      n_cases[i] <- length(df_soc_learn_4round$urn_choice[df_soc_learn_4round$round_number %% 4 == 0 & df_soc_learn_4round$type_assignment == 'social_learner' & df_soc_learn_4round$number_observed_IL_right_urn == numIndLearnersRight[i] & df_soc_learn_4round$cognitive_load == cognitive_load[i] & df_soc_learn_4round$similar_ingroup == similar_ingroup[i] & df_soc_learn_4round$observing_ingroup == observing_ingroup[i]])
      estimatesBootRight[i,1] <- propSocLearnersRight[i]
      numEstimatesRight[i] <- numEstimatesRight[i] + 1
    }

    # Actual data optimal behavior
    # if (sum(dataFourTmts$Period %% 5 == 0 & dataFourTmts$socLearner == 1 & dataFourTmts$numIndOpt == numIndLearnersOpt[i] & dataFourTmts$concordant == concordant[i] & dataFourTmts$ambiguous == ambiguous[i] & dataFourTmts$between == between[i] & dataFourTmts$prior == prior[i]) > 0)
    # {
    #   propSocLearnersOpt[i] <- mean(dataFourTmts$choiceOpt[dataFourTmts$Period %% 5 == 0 & dataFourTmts$socLearner == 1 & dataFourTmts$numIndOpt == numIndLearnersOpt[i] & dataFourTmts$concordant == concordant[i] & dataFourTmts$ambiguous == ambiguous[i] & dataFourTmts$between == between[i] & dataFourTmts$prior == prior[i]])
    #   estimatesBootOpt[i,1] <- propSocLearnersOpt[i]
    #   numEstimatesOpt[i] <- numEstimatesOpt[i] + 1
    # }
  }
  
  # # Pooling
  # for (i in 1:length(numIndLearnersLeft_pooled))
  # {
  #   # Actual data
  #   if (sum(dataFourTmts$Period %% 5 == 0 & dataFourTmts$socLearner == 1 & dataFourTmts$numIndLeft == numIndLearnersLeft_pooled[i] & dataFourTmts$ambiguous == 1 & dataFourTmts$prior == prior_pooled[i]) > 0)
  #   {
  #     print('test')
  #     propSocLearnersLeft_pooled[i] <- mean(dataFourTmts$chooseLeft[dataFourTmts$Period %% 5 == 0 & dataFourTmts$socLearner == 1 & dataFourTmts$numIndLeft == numIndLearnersLeft_pooled[i] & dataFourTmts$ambiguous == 1 & dataFourTmts$prior == prior_pooled[i]])
  #     estimatesBootLeft_pooled[i,1] <- propSocLearnersLeft_pooled[i]
  #     numEstimatesLeft_pooled[i] <- numEstimatesLeft_pooled[i] + 1
  #   }
  # }
  
  # Bootstrapping (note, Charles had two more columns in there for the optimal behavior of demonstrators. I ignore that for now and only focus on pure behavior)
  dataFourTmts_socLearners <- df_soc_learn_4round[df_soc_learn_4round$round_number %% 4 == 0 & df_soc_learn_4round$type_assignment == 'social_learner',c('cognitive_load','urn_choice','number_observed_IL_right_urn','observing_ingroup','similar_ingroup','uniqueSubjectNum')]
  
  socLearnersPresent <- as.numeric(levels(as.factor(df_soc_learn_4round$uniqueSubjectNum)))
  
  for (j in 3:(numBootstrapSamples + 1))
  {
    socLearnersBoot <- sample(socLearnersPresent,replace=T) # sample soc learners with replacement => we are clustering at the level of the soc learner here
    
    bootSubject <- rep(NA,length(dataFourTmts_socLearners$uniqueSubjectNum))
    bootNumber_observed_IL_right_urn <- rep(NA,length(dataFourTmts_socLearners$uniqueSubjectNum))
    bootUrn_choice <- rep(NA,length(dataFourTmts_socLearners$uniqueSubjectNum))
    # bootNumIndOpt <- rep(NA,length(dataFourTmts_socLearners$uniqueSubjectNum))
    # bootChooseOpt <- rep(NA,length(dataFourTmts_socLearners$uniqueSubjectNum))	
    bootCognitiveLoad <- rep(NA,length(dataFourTmts_socLearners$uniqueSubjectNum))
    bootObserving_ingroup <- rep(NA,length(dataFourTmts_socLearners$uniqueSubjectNum))
    bootSimilar_ingroup <- rep(NA,length(dataFourTmts_socLearners$uniqueSubjectNum))

    count <- 0
    
    for (k in 1:length(socLearnersBoot))
    {
      bootSubject[(count+1):(count+sum(dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k]))] <- dataFourTmts_socLearners$uniqueSubjectNum[dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k]]
      
      bootNumber_observed_IL_right_urn[(count+1):(count+sum(dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k]))] <- dataFourTmts_socLearners$number_observed_IL_right_urn[dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k]]
      
      bootUrn_choice[(count+1):(count+sum(dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k]))] <- dataFourTmts_socLearners$urn_choice[dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k]]
      
      # bootNumIndOpt[(count+1):(count+sum(dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k]))] <- dataFourTmts_socLearners$numIndOpt[dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k]]
      # 
      # bootChooseOpt[(count+1):(count+sum(dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k]))] <- dataFourTmts_socLearners$choiceOpt[dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k]]
      # 
      bootCognitiveLoad[(count+1):(count+sum(dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k]))] <- dataFourTmts_socLearners$cognitive_load[dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k]]
      
      bootObserving_ingroup[(count+1):(count+sum(dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k]))] <- dataFourTmts_socLearners$observing_ingroup[dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k]]
      
      bootSimilar_ingroup[(count+1):(count+sum(dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k]))] <- dataFourTmts_socLearners$similar_ingroup[dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k]]
      
      # bootPrior[(count+1):(count+sum(dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k]))] <- dataFourTmts_socLearners$prior[dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k]]
      
      count <- count + sum(dataFourTmts_socLearners$uniqueSubjectNum == socLearnersBoot[k])
      
      
      if (count %% 20 != 0) {print("Error!")}
      if (k == length(socLearnersBoot) & count != length(dataFourTmts_socLearners$uniqueSubjectNum)) {print("Error!")}
    }
    
    # Not pooling
    for (i in 1:48)
    {
      if (sum(bootNumber_observed_IL_right_urn == numIndLearnersRight[i] & bootCognitiveLoad == cognitive_load[i] & bootObserving_ingroup == observing_ingroup[i] & bootSimilar_ingroup == similar_ingroup[i]) > 0)
      {
        estimatesBootRight[i,j] <- mean(bootUrn_choice[bootNumber_observed_IL_right_urn == numIndLearnersRight[i] & bootCognitiveLoad == cognitive_load[i] & bootObserving_ingroup == observing_ingroup[i] & bootSimilar_ingroup == similar_ingroup[i]])
        numEstimatesRight[i] <- numEstimatesRight[i] + 1
      }
      
      # if (sum(bootNumIndOpt == numIndLearnersOpt[i] & bootConcordant == concordant[i] & bootAmbiguous == ambiguous[i] & bootBetween == between[i] & bootPrior == prior[i]) > 0)
      # {
      #   estimatesBootOpt[i,j] <- mean(bootChooseOpt[bootNumIndOpt == numIndLearnersOpt[i] & bootConcordant == concordant[i] & bootAmbiguous == ambiguous[i] & bootBetween == between[i] & bootPrior == prior[i]])
      #   numEstimatesOpt[i] <- numEstimatesOpt[i] + 1
      # }
    }
    
    # # Pooling
    # for (i in 1:12)
    # {
    #   if (sum(bootNumIndLeft == numIndLearnersLeft_pooled[i] & bootAmbiguous == 1 & bootPrior == prior_pooled[i]) > 0)
    #   {
    #     estimatesBootLeft_pooled[i,j] <- mean(bootChooseLeft[bootNumIndLeft == numIndLearnersLeft_pooled[i] & bootAmbiguous == 1 & bootPrior == prior_pooled[i]])
    #     numEstimatesLeft_pooled[i] <- numEstimatesLeft_pooled[i] + 1 
    #   }
    # }
  }
  
  for (i in 1:48)
  {
    temp <- estimatesBootRight[i, ]
    temp <- sort(temp[is.na(temp) == F])
    if (length(temp) > 0)
    {
      lowerCI_right[i] <- temp[floor((alpha * length(temp))/2)]
      upperCI_right[i] <- temp[ceiling((1 - alpha/2) * length(temp))]
    }
    
    # temp <- estimatesBootOpt[i, ]
    # temp <- sort(temp[is.na(temp) == F])
    # if (length(temp) > 0)
    # {
    #   lowerCI_opt[i] <- temp[floor((alpha * length(temp))/2)]
    #   upperCI_opt[i] <- temp[ceiling((1 - alpha/2) * length(temp))]
    # }
    
  }
  
  # for (i in 1:12)
  # {
  #   temp <- estimatesBootLeft_pooled[i, ]
  #   temp <- sort(temp[is.na(temp) == F])
  #   if (length(temp) > 0)
  #   {
  #     lowerCI_left_pooled[i] <- temp[floor((alpha * length(temp))/2)]
  #     upperCI_left_pooled[i] <- temp[ceiling((1 - alpha/2) * length(temp))]
  #   }
  # }
  
  
  bootResultsAllTmts <- data.frame(numIndLearnersRight,propSocLearnersRight,lowerCI_right,upperCI_right,numEstimatesRight,
                                   # numIndLearnersOpt,propSocLearnersOpt,lowerCI_opt,upperCI_opt,numEstimatesOpt,
                                   cognitive_load,observing_ingroup,similar_ingroup,n_cases)
  
  # bootResultsAmbiguousPooled <- data.frame(numIndLearnersLeft_pooled,propSocLearnersLeft_pooled,lowerCI_left_pooled,upperCI_left_pooled,numEstimatesLeft_pooled,prior_pooled)
  
  out <- list(bootResultsAllTmts)
  names(out) <- c('bootResultsAllTmts')
  
  out
  
}