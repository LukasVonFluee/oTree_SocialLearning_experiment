# Created by Charles Efferson.
# Copyright, 2022, all rights reserved.

rm(list=ls())

load('spilloversByGroup.RData')

source('clx.R')
source('clx_vcov.R')

library(lmtest)
library(car)

spilloversByGroup$tmtPolitical <- 1 - spilloversByGroup$tmtNeutral

resModel1 <- glm(spillover ~ tmtPolitical,data = spilloversByGroup,family = gaussian)
coefTestModel1 <- clx(resModel1,1,spilloversByGroup$groupsPresent)
vcov_coefTestModel1 <- clx_vcov(resModel1,1,spilloversByGroup$groupsPresent)

linearCombo1_coefTestModel1 <- linearHypothesis(resModel1,'(Intercept) + tmtPolitical = 0',test = 'F',vcov. = vcov_coefTestModel1)


logit_optimal_choice <- glm(chosen_correctly ~ propIndCorrCentered + similar_ingroup_ncl_dummy + 
                              dissimilar_ingroup_ncl_dummy + similar_outgroup_ncl_dummy +
                              similar_ingroup_cl_dummy + dissimilar_ingroup_cl_dummy +
                              similar_outgroup_cl_dummy + dissimilar_outgroup_cl_dummy,
                            data = df_soc_learn_4round_zurich, family = "binomial")

coefTestModel1 <- clx(logit_optimal_choice,1,df_soc_learn_4round_zurich$participant_ID)

vcov_coefTestModel1 <- clx_vcov(logit_optimal_choice,1,df_soc_learn_4round_zurich$participant_ID)

linearCombo1_coefTestModel1 <- linearHypothesis(logit_optimal_choice,'similar_ingroup_ncl_dummy1 - dissimilar_ingroup_ncl_dummy1 = 0',test = 'F',vcov. = vcov_coefTestModel1)



