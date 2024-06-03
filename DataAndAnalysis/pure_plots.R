rm(list=ls())
library(dplyr)
################################################################################
################################################################################
################################################################################
# ZURICH
################################################################################
################################################################################
################################################################################

load('est_df_soc_learn_4_zurich.RData')

# This routine plots the imitation functions for the amb treatments with CIs based on boostrapping clustered on soc learner.  All amb tmts are within-subjects

# Somehow in "est_df_soc_learn_4_busara" it put "bootResultsAllTmts." in front of all column names
# will check later how to fix that in "mybootfunct.R"
# for now, I'll rename

est_df_soc_learn_4_zurich <- est_df_soc_learn_4_zurich %>%
  rename("numIndLearnersRight" = "bootResultsAllTmts.numIndLearnersRight",
         "propSocLearnersRight"= "bootResultsAllTmts.propSocLearnersRight",
         "lowerCI_right" = "bootResultsAllTmts.lowerCI_right",
         "upperCI_right" = "bootResultsAllTmts.upperCI_right",
         "numEstimatesRight" = "bootResultsAllTmts.numEstimatesRight",
         "cognitive_load" = "bootResultsAllTmts.cognitive_load",
         "observing_ingroup" = "bootResultsAllTmts.observing_ingroup",
         "similar_ingroup" = "bootResultsAllTmts.similar_ingroup",
         "n_cases" = "bootResultsAllTmts.n_cases")


widthWindow <- 14
heightWindow <- 14
cexNum <- 1

# This routine plots the imitation functions for the amb treatments with CIs based on boostrapping clustered on soc learner.  All amb tmts are within-subjects

setwd('pure_plots_zurich')

quartz(width = widthWindow,height = heightWindow, type='pdf', file='ingroup_zurich.pdf')

# First is similar_ingroup, observing_ingroup, cognitive_load vs no cognitive load

par(mar=c(5,5.5,4,3),mgp=c(3,1,0),cex.lab=cexNum,cex.axis=1.2*cexNum,fig=c(0,0.5,0.5,1))
plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing right',ylab='Proportion social learners choosing right',axes=F,cex.lab=1.6*cexNum)
polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
axis(1,0:5,0:5,cex.axis=1.75*cexNum)
axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
box()
mtext('NCL, similar to ingroup, observing ingroup',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('A',side=3,line=1.5,at=-1,cex=2*cexNum)

temp <- est_df_soc_learn_4_zurich[est_df_soc_learn_4_zurich$cognitive_load == 0 & est_df_soc_learn_4_zurich$similar_ingroup == 1 & est_df_soc_learn_4_zurich$observing_ingroup == 1, ]

# Wasn't able to figure out whether Charles got below numbers manually or not. In any case, I counted cases in "mybootfunct.R". 
# In the data_analysis_zurich.R and data_analysis_busara.R scripts, when executing the mybootfunct.R function, the data sets
# est_df_soc_learn_4_zurich.RData and est_df_soc_learn_4_busara.RData are created. those include the "n_cases" column with the number of cases of 
# social learners choosing the right urn for each case of individual learners 
# choosing the right urn (0,1,2,3,4,5), and this for each of the 8 treatments

n_cases_per_treat <- as.data.frame(matrix(0,6,2)) 
n_cases_per_treat[,1] <- seq(0,5)

for (i in 1:length(n_cases_per_treat[,1])) {
  n_cases_per_treat[i,2] <- est_df_soc_learn_4_zurich[est_df_soc_learn_4_zurich$numIndLearnersRight == n_cases_per_treat[i,1] & est_df_soc_learn_4_zurich$cognitive_load == 0 & est_df_soc_learn_4_zurich$similar_ingroup == 1 & est_df_soc_learn_4_zurich$observing_ingroup == 1, 'n_cases']
}

text(x = 0, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[1, 2])), adj = 0.5, cex = 1.3)
text(x = 1, y = 0.88, labels = bquote(italic(N) == .(n_cases_per_treat[2, 2])), adj = 0.5, cex = 1.3)
text(x = 2, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[3, 2])), adj = 0.5, cex = 1.3)
text(x = 3, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[4, 2])), adj = 0.5, cex = 1.3)
text(x = 4, y = 0.12, labels = bquote(italic(N) == .(n_cases_per_treat[5, 2])), adj = 0.5, cex = 1.3)
text(x = 5, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[6, 2])), adj = 0.5, cex = 1.3)

for (i in 1:6)
{
  points(temp$numIndLearnersRight[i],temp$propSocLearnersRight[i],pch=19,cex=1.5*cexNum)
  arrows(temp$numIndLearnersRight[i],temp$lowerCI_right[i],temp$numIndLearnersRight[i],temp$upperCI_right[i],length=0.1,angle=90,lwd=3,code=3)
}


################################################################################

# Now comes cognitive load, similar_ingroup, observing ingroup

################################################################################

par(fig=c(0.5,1,0.5,1),new=T)
plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing right',ylab='Proportion social learners choosing right',axes=F,cex.lab=1.6*cexNum)
polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
axis(1,0:5,0:5,cex.axis=1.75*cexNum)
axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
box()
mtext('CL, similar to ingroup, observing ingroup',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('B',side=3,line=1.5,at=-1,cex=2*cexNum)

temp <- est_df_soc_learn_4_zurich[est_df_soc_learn_4_zurich$cognitive_load == 1 & est_df_soc_learn_4_zurich$similar_ingroup == 1 & est_df_soc_learn_4_zurich$observing_ingroup == 1, ]

n_cases_per_treat <- as.data.frame(matrix(0,6,2)) 
n_cases_per_treat[,1] <- seq(0,5)

for (i in 1:length(n_cases_per_treat[,1])) {
  n_cases_per_treat[i,2] <- est_df_soc_learn_4_zurich[est_df_soc_learn_4_zurich$numIndLearnersRight == n_cases_per_treat[i,1] & est_df_soc_learn_4_zurich$cognitive_load == 1 & est_df_soc_learn_4_zurich$similar_ingroup == 1 & est_df_soc_learn_4_zurich$observing_ingroup == 1, 'n_cases']
}

text(x = 0, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[1, 2])), adj = 0.5, cex = 1.3)
text(x = 1, y = 0.88, labels = bquote(italic(N) == .(n_cases_per_treat[2, 2])), adj = 0.5, cex = 1.3)
text(x = 2, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[3, 2])), adj = 0.5, cex = 1.3)
text(x = 3, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[4, 2])), adj = 0.5, cex = 1.3)
text(x = 4, y = 0.12, labels = bquote(italic(N) == .(n_cases_per_treat[5, 2])), adj = 0.5, cex = 1.3)
text(x = 5, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[6, 2])), adj = 0.5, cex = 1.3)


for (i in 1:6)
{
  points(temp$numIndLearnersRight[i],temp$propSocLearnersRight[i],pch=19,cex=1.5*cexNum)
  arrows(temp$numIndLearnersRight[i],temp$lowerCI_right[i],temp$numIndLearnersRight[i],temp$upperCI_right[i],length=0.1,angle=90,lwd=3,code=3)
}


################################################################################

# Now comes no cognitive load, not similar_ingroup, observing ingroup

################################################################################

par(fig=c(0,0.5,0,0.5),new=T)
plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing right',ylab='Proportion social learners choosing right',axes=F,cex.lab=1.6*cexNum)
polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
axis(1,0:5,0:5,cex.axis=1.75*cexNum)
axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
box()
mtext('NCL, dissimilar to ingroup, observing ingroup',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('C',side=3,line=1.5,at=-1,cex=2*cexNum)

temp <- est_df_soc_learn_4_zurich[est_df_soc_learn_4_zurich$cognitive_load == 0 & est_df_soc_learn_4_zurich$similar_ingroup == 0 & est_df_soc_learn_4_zurich$observing_ingroup == 1, ]

n_cases_per_treat <- as.data.frame(matrix(0,6,2)) 
n_cases_per_treat[,1] <- seq(0,5)

for (i in 1:length(n_cases_per_treat[,1])) {
  n_cases_per_treat[i,2] <- est_df_soc_learn_4_zurich[est_df_soc_learn_4_zurich$numIndLearnersRight == n_cases_per_treat[i,1] & est_df_soc_learn_4_zurich$cognitive_load == 0 & est_df_soc_learn_4_zurich$similar_ingroup == 0 & est_df_soc_learn_4_zurich$observing_ingroup == 1, 'n_cases']
}

text(x = 0, y = 0.61, labels = bquote(italic(N) == .(n_cases_per_treat[1, 2])), adj = 0.5, cex = 1.3)
text(x = 1, y = 0.54, labels = bquote(italic(N) == .(n_cases_per_treat[2, 2])), adj = 0.5, cex = 1.3)
text(x = 2, y = 0.61, labels = bquote(italic(N) == .(n_cases_per_treat[3, 2])), adj = 0.5, cex = 1.3)
text(x = 3, y = 0.46, labels = bquote(italic(N) == .(n_cases_per_treat[4, 2])), adj = 0.5, cex = 1.3)
text(x = 4, y = 0.39, labels = bquote(italic(N) == .(n_cases_per_treat[5, 2])), adj = 0.5, cex = 1.3)
text(x = 5, y = 0.46, labels = bquote(italic(N) == .(n_cases_per_treat[6, 2])), adj = 0.5, cex = 1.3)


for (i in 1:6)
{
  points(temp$numIndLearnersRight[i],temp$propSocLearnersRight[i],pch=19,cex=1.5*cexNum)
  arrows(temp$numIndLearnersRight[i],temp$lowerCI_right[i],temp$numIndLearnersRight[i],temp$upperCI_right[i],length=0.1,angle=90,lwd=3,code=3)
}


################################################################################

# Now comes cognitive load, not similar_ingroup, observing ingroup

################################################################################

par(fig=c(0.5,1,0,0.5),new=T)
plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing right',ylab='Proportion social learners choosing right',axes=F,cex.lab=1.6*cexNum)
polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
axis(1,0:5,0:5,cex.axis=1.75*cexNum)
axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
box()
mtext('CL, dissimilar to ingroup, observing ingroup',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('D',side=3,line=1.5,at=-1,cex=2*cexNum)

temp <- est_df_soc_learn_4_zurich[est_df_soc_learn_4_zurich$cognitive_load == 1 & est_df_soc_learn_4_zurich$similar_ingroup == 0 & est_df_soc_learn_4_zurich$observing_ingroup == 1, ]

n_cases_per_treat <- as.data.frame(matrix(0,6,2)) 
n_cases_per_treat[,1] <- seq(0,5)

for (i in 1:length(n_cases_per_treat[,1])) {
  n_cases_per_treat[i,2] <- est_df_soc_learn_4_zurich[est_df_soc_learn_4_zurich$numIndLearnersRight == n_cases_per_treat[i,1] & est_df_soc_learn_4_zurich$cognitive_load == 1 & est_df_soc_learn_4_zurich$similar_ingroup == 0 & est_df_soc_learn_4_zurich$observing_ingroup == 1, 'n_cases']
}

text(x = 0, y = 0.61, labels = bquote(italic(N) == .(n_cases_per_treat[1, 2])), adj = 0.5, cex = 1.3)
text(x = 1, y = 0.54, labels = bquote(italic(N) == .(n_cases_per_treat[2, 2])), adj = 0.5, cex = 1.3)
text(x = 2, y = 0.61, labels = bquote(italic(N) == .(n_cases_per_treat[3, 2])), adj = 0.5, cex = 1.3)
text(x = 3, y = 0.46, labels = bquote(italic(N) == .(n_cases_per_treat[4, 2])), adj = 0.5, cex = 1.3)
text(x = 4, y = 0.39, labels = bquote(italic(N) == .(n_cases_per_treat[5, 2])), adj = 0.5, cex = 1.3)
text(x = 5, y = 0.46, labels = bquote(italic(N) == .(n_cases_per_treat[6, 2])), adj = 0.5, cex = 1.3)



for (i in 1:6)
{
  points(temp$numIndLearnersRight[i],temp$propSocLearnersRight[i],pch=19,cex=1.5*cexNum)
  arrows(temp$numIndLearnersRight[i],temp$lowerCI_right[i],temp$numIndLearnersRight[i],temp$upperCI_right[i],length=0.1,angle=90,lwd=3,code=3)
}


dev.off()

################################################################################
################################################################################
# NOW OUTGROUP
################################################################################
################################################################################

widthWindow <- 14
heightWindow <- 14
cexNum <- 1

# First is similar_ingroup, observing_outgroup, no cognitive_load

quartz(width = widthWindow,height = heightWindow, type='pdf', file='outgroup_zurich.pdf')

################################################################################

# Now comes similar_ingroup, observing outgroup, no cognitive load

################################################################################

par(mar=c(5,5.5,4,3),mgp=c(3,1,0),cex.lab=cexNum,cex.axis=1.2*cexNum,fig=c(0,0.5,0.5,1))
plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing right',ylab='Proportion social learners choosing right',axes=F,cex.lab=1.6*cexNum)
polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
axis(1,0:5,0:5,cex.axis=1.75*cexNum)
axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
box()
mtext('NCL, similar to ingroup, observing outgroup',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('A',side=3,line=1.5,at=-1,cex=2*cexNum)

temp <- est_df_soc_learn_4_zurich[est_df_soc_learn_4_zurich$cognitive_load == 0 & est_df_soc_learn_4_zurich$similar_ingroup == 1 & est_df_soc_learn_4_zurich$observing_ingroup == 0, ]

n_cases_per_treat <- as.data.frame(matrix(0,6,2)) 
n_cases_per_treat[,1] <- seq(0,5)

for (i in 1:length(n_cases_per_treat[,1])) {
  n_cases_per_treat[i,2] <- est_df_soc_learn_4_zurich[est_df_soc_learn_4_zurich$numIndLearnersRight == n_cases_per_treat[i,1] & est_df_soc_learn_4_zurich$cognitive_load == 0 & est_df_soc_learn_4_zurich$similar_ingroup == 1 & est_df_soc_learn_4_zurich$observing_ingroup == 0, 'n_cases']
}

text(x = 0, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[1, 2])), adj = 0.5, cex = 1.3)
text(x = 1, y = 0.88, labels = bquote(italic(N) == .(n_cases_per_treat[2, 2])), adj = 0.5, cex = 1.3)
text(x = 2, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[3, 2])), adj = 0.5, cex = 1.3)
text(x = 3, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[4, 2])), adj = 0.5, cex = 1.3)
text(x = 4, y = 0.12, labels = bquote(italic(N) == .(n_cases_per_treat[5, 2])), adj = 0.5, cex = 1.3)
text(x = 5, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[6, 2])), adj = 0.5, cex = 1.3)



for (i in 1:6)
{
  points(temp$numIndLearnersRight[i],temp$propSocLearnersRight[i],pch=19,cex=1.5*cexNum)
  arrows(temp$numIndLearnersRight[i],temp$lowerCI_right[i],temp$numIndLearnersRight[i],temp$upperCI_right[i],length=0.1,angle=90,lwd=3,code=3)
}


################################################################################

# Now comes cognitive load, similar_ingroup, observing outgroup

################################################################################

par(fig=c(0.5,1,0.5,1),new=T)
plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing right',ylab='Proportion social learners choosing right',axes=F,cex.lab=1.6*cexNum)
polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
axis(1,0:5,0:5,cex.axis=1.75*cexNum)
axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
box()
mtext('CL, similar to ingroup, observing outgroup',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('B',side=3,line=1.5,at=-1,cex=2*cexNum)

temp <- est_df_soc_learn_4_zurich[est_df_soc_learn_4_zurich$cognitive_load == 1 & est_df_soc_learn_4_zurich$similar_ingroup == 1 & est_df_soc_learn_4_zurich$observing_ingroup == 0, ]

n_cases_per_treat <- as.data.frame(matrix(0,6,2)) 
n_cases_per_treat[,1] <- seq(0,5)

for (i in 1:length(n_cases_per_treat[,1])) {
  n_cases_per_treat[i,2] <- est_df_soc_learn_4_zurich[est_df_soc_learn_4_zurich$numIndLearnersRight == n_cases_per_treat[i,1] & est_df_soc_learn_4_zurich$cognitive_load == 1 & est_df_soc_learn_4_zurich$similar_ingroup == 1 & est_df_soc_learn_4_zurich$observing_ingroup == 0, 'n_cases']
}

text(x = 0, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[1, 2])), adj = 0.5, cex = 1.3)
text(x = 1, y = 0.88, labels = bquote(italic(N) == .(n_cases_per_treat[2, 2])), adj = 0.5, cex = 1.3)
text(x = 2, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[3, 2])), adj = 0.5, cex = 1.3)
text(x = 3, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[4, 2])), adj = 0.5, cex = 1.3)
text(x = 4, y = 0.12, labels = bquote(italic(N) == .(n_cases_per_treat[5, 2])), adj = 0.5, cex = 1.3)
text(x = 5, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[6, 2])), adj = 0.5, cex = 1.3)



for (i in 1:6)
{
  points(temp$numIndLearnersRight[i],temp$propSocLearnersRight[i],pch=19,cex=1.5*cexNum)
  arrows(temp$numIndLearnersRight[i],temp$lowerCI_right[i],temp$numIndLearnersRight[i],temp$upperCI_right[i],length=0.1,angle=90,lwd=3,code=3)
}



################################################################################

# Now comes not similar_ingroup, observing outgroup, no cognitive load

################################################################################

par(fig=c(0,0.5,0,0.5),new=T)
plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing right',ylab='Proportion social learners choosing right',axes=F,cex.lab=1.6*cexNum)
polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
axis(1,0:5,0:5,cex.axis=1.75*cexNum)
axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
box()
mtext('NCL, dissimilar to ingroup, observing outgroup',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('C',side=3,line=1.5,at=-1,cex=2*cexNum)

temp <- est_df_soc_learn_4_zurich[est_df_soc_learn_4_zurich$cognitive_load == 0 & est_df_soc_learn_4_zurich$similar_ingroup == 0 & est_df_soc_learn_4_zurich$observing_ingroup == 0, ]

n_cases_per_treat <- as.data.frame(matrix(0,6,2)) 
n_cases_per_treat[,1] <- seq(0,5)

for (i in 1:length(n_cases_per_treat[,1])) {
  n_cases_per_treat[i,2] <- est_df_soc_learn_4_zurich[est_df_soc_learn_4_zurich$numIndLearnersRight == n_cases_per_treat[i,1] & est_df_soc_learn_4_zurich$cognitive_load == 0 & est_df_soc_learn_4_zurich$similar_ingroup == 0 & est_df_soc_learn_4_zurich$observing_ingroup == 0, 'n_cases']
}

text(x = 0, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[1, 2])), adj = 0.5, cex = 1.3)
text(x = 1, y = 0.88, labels = bquote(italic(N) == .(n_cases_per_treat[2, 2])), adj = 0.5, cex = 1.3)
text(x = 2, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[3, 2])), adj = 0.5, cex = 1.3)
text(x = 3, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[4, 2])), adj = 0.5, cex = 1.3)
text(x = 4, y = 0.12, labels = bquote(italic(N) == .(n_cases_per_treat[5, 2])), adj = 0.5, cex = 1.3)
text(x = 5, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[6, 2])), adj = 0.5, cex = 1.3)



for (i in 1:6)
{
  points(temp$numIndLearnersRight[i],temp$propSocLearnersRight[i],pch=19,cex=1.5*cexNum)
  arrows(temp$numIndLearnersRight[i],temp$lowerCI_right[i],temp$numIndLearnersRight[i],temp$upperCI_right[i],length=0.1,angle=90,lwd=3,code=3)
}


################################################################################

# Now comes cognitive load, not similar_ingroup, observing outgroup

################################################################################

par(fig=c(0.5,1,0,0.5),new=T)
plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing right',ylab='Proportion social learners choosing right',axes=F,cex.lab=1.6*cexNum)
polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
axis(1,0:5,0:5,cex.axis=1.75*cexNum)
axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
box()
mtext('CL, dissimilar to ingroup, observing outgroup',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('D',side=3,line=1.5,at=-1,cex=2*cexNum)

temp <- est_df_soc_learn_4_zurich[est_df_soc_learn_4_zurich$cognitive_load == 1 & est_df_soc_learn_4_zurich$similar_ingroup == 0 & est_df_soc_learn_4_zurich$observing_ingroup == 0, ]

n_cases_per_treat <- as.data.frame(matrix(0,6,2)) 
n_cases_per_treat[,1] <- seq(0,5)

for (i in 1:length(n_cases_per_treat[,1])) {
  n_cases_per_treat[i,2] <- est_df_soc_learn_4_zurich[est_df_soc_learn_4_zurich$numIndLearnersRight == n_cases_per_treat[i,1] & est_df_soc_learn_4_zurich$cognitive_load == 1 & est_df_soc_learn_4_zurich$similar_ingroup == 0 & est_df_soc_learn_4_zurich$observing_ingroup == 0, 'n_cases']
}

text(x = 0, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[1, 2])), adj = 0.5, cex = 1.3)
text(x = 1, y = 0.88, labels = bquote(italic(N) == .(n_cases_per_treat[2, 2])), adj = 0.5, cex = 1.3)
text(x = 2, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[3, 2])), adj = 0.5, cex = 1.3)
text(x = 3, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[4, 2])), adj = 0.5, cex = 1.3)
text(x = 4, y = 0.12, labels = bquote(italic(N) == .(n_cases_per_treat[5, 2])), adj = 0.5, cex = 1.3)
text(x = 5, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[6, 2])), adj = 0.5, cex = 1.3)


for (i in 1:6)
{
  points(temp$numIndLearnersRight[i],temp$propSocLearnersRight[i],pch=19,cex=1.5*cexNum)
  arrows(temp$numIndLearnersRight[i],temp$lowerCI_right[i],temp$numIndLearnersRight[i],temp$upperCI_right[i],length=0.1,angle=90,lwd=3,code=3)
}

dev.off()



# Note, I haven't done the pooling in the following yet. I commented it out for now

# NOW MOVE TO POOLING DATA OVER DISCORDANT AND CONCORDANT

# widthWindow <- 14.5
# heightWindow <- 7
# cexNum <- 1
# 
# # Ambiguous, within-subject, prior
# 
# quartz(width = widthWindow,height = heightWindow, type='pdf', file='plotAmbiguousLeft_pooled.pdf')
# 
# par(mar=c(5,5.5,4,3),mgp=c(3,1,0),cex.lab=cexNum,cex.axis=1.2*cexNum,fig=c(0,0.5,0,1))
# plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing left',ylab='Proportion social learners choosing left',axes=F,cex.lab=1.6*cexNum)
# polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
# polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
# lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
# lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
# lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
# axis(1,0:5,0:5,cex.axis=1.75*cexNum)
# axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
# box()
# mtext('Opaque, prior',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
# mtext('A',side=3,line=1.5,at=-1,cex=2*cexNum)
# 
# temp <- resEstAndBootImitationFunctions_ambPooled[resEstAndBootImitationFunctions_ambPooled$prior == 1, ]
# 
# text(x = 0,y = 0.85,labels=expression(italic(N) == 155),adj=0.5,cex=1.3)
# text(x = 1,y = 0.78,labels=expression(italic(N) == 213),adj=0.5,cex=1.3)
# text(x = 1.96,y = 0.85,labels=expression(italic(N) == 130),adj=0.5,cex=1.3)
# 
# text(x = 3.03,y = 0.15,labels=expression(italic(N) == 116),adj=0.5,cex=1.3)
# text(x = 4,y = 0.22,labels=expression(italic(N) == 193),adj=0.5,cex=1.3)
# text(x = 5,y = 0.15,labels=expression(italic(N) == 53),adj=0.5,cex=1.3)
# 
# for (i in 1:6)
# {
#   points(temp$numIndLearnersLeft_pooled[i],temp$propSocLearnersLeft_pooled[i],pch=19,cex=1.5*cexNum)
#   arrows(temp$numIndLearnersLeft_pooled[i],temp$lowerCI_left_pooled[i],temp$numIndLearnersLeft_pooled[i],temp$upperCI_left_pooled[i],length=0.1,angle=90,lwd=3,code=3)
# }
# 
# par(fig=c(0.5,1,0,1),new=T)
# plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing left',ylab='Proportion social learners choosing left',axes=F,cex.lab=1.6*cexNum)
# polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
# polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
# lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
# lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
# lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
# axis(1,0:5,0:5,cex.axis=1.75*cexNum)
# axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
# box()
# mtext('Opaque, no prior',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
# mtext('B',side=3,line=1.5,at=-1,cex=2*cexNum)
# 
# temp <- resEstAndBootImitationFunctions_ambPooled[resEstAndBootImitationFunctions_ambPooled$prior == 0, ]
# 
# text(x = 0,y = 0.85,labels=expression(italic(N) == 198),adj=0.5,cex=1.3)
# text(x = 1,y = 0.78,labels=expression(italic(N) == 231),adj=0.5,cex=1.3)
# text(x = 1.96,y = 0.85,labels=expression(italic(N) == 121),adj=0.5,cex=1.3)
# 
# text(x = 3,y = 0.15,labels=expression(italic(N) == 88),adj=0.5,cex=1.3)
# text(x = 4,y = 0.22,labels=expression(italic(N) == 165),adj=0.5,cex=1.3)
# text(x = 5,y = 0.15,labels=expression(italic(N) == 77),adj=0.5,cex=1.3)
# 
# for (i in 1:6)
# {
#   points(temp$numIndLearnersLeft_pooled[i],temp$propSocLearnersLeft_pooled[i],pch=19,cex=1.5*cexNum)
#   arrows(temp$numIndLearnersLeft_pooled[i],temp$lowerCI_left_pooled[i],temp$numIndLearnersLeft_pooled[i],temp$upperCI_left_pooled[i],length=0.1,angle=90,lwd=3,code=3)
# }
# 
# dev.off()

rm(list=ls())

setwd('..')





################################################################################
################################################################################
################################################################################
# BUSARA
################################################################################
################################################################################
################################################################################


rm(list=ls())

load('est_df_soc_learn_4_busara.RData')

# This routine plots the imitation functions for the amb treatments with CIs based on boostrapping clustered on soc learner.  All amb tmts are within-subjects

# Somehow in "est_df_soc_learn_4_busara" it put "bootResultsAllTmts." in front of all column names
# will check later how to fix that in "mybootfunct.R"
# for now, I'll rename


est_df_soc_learn_4_busara <- est_df_soc_learn_4_busara %>%
  rename("numIndLearnersRight" = "bootResultsAllTmts.numIndLearnersRight",
         "propSocLearnersRight"= "bootResultsAllTmts.propSocLearnersRight",
         "lowerCI_right" = "bootResultsAllTmts.lowerCI_right",
         "upperCI_right" = "bootResultsAllTmts.upperCI_right",
         "numEstimatesRight" = "bootResultsAllTmts.numEstimatesRight",
         "cognitive_load" = "bootResultsAllTmts.cognitive_load",
         "observing_ingroup" = "bootResultsAllTmts.observing_ingroup",
         "similar_ingroup" = "bootResultsAllTmts.similar_ingroup",
         "n_cases" = "bootResultsAllTmts.n_cases")


widthWindow <- 14
heightWindow <- 14
cexNum <- 1

# This routine plots the imitation functions for the amb treatments with CIs based on boostrapping clustered on soc learner.  All amb tmts are within-subjects

setwd('pure_plots_busara')

quartz(width = widthWindow,height = heightWindow, type='pdf', file='ingroup_busara.pdf')

# First is similar_ingroup, observing_ingroup, cognitive_load vs no cognitive load

par(mar=c(5,5.5,4,3),mgp=c(3,1,0),cex.lab=cexNum,cex.axis=1.2*cexNum,fig=c(0,0.5,0.5,1))
plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing right',ylab='Proportion social learners choosing right',axes=F,cex.lab=1.6*cexNum)
polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
axis(1,0:5,0:5,cex.axis=1.75*cexNum)
axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
box()
mtext('NCL, similar to ingroup, observing ingroup',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('A',side=3,line=1.5,at=-1,cex=2*cexNum)

temp <- est_df_soc_learn_4_busara[est_df_soc_learn_4_busara$cognitive_load == 0 & est_df_soc_learn_4_busara$similar_ingroup == 1 & est_df_soc_learn_4_busara$observing_ingroup == 1, ]

n_cases_per_treat <- as.data.frame(matrix(0,6,2)) 
n_cases_per_treat[,1] <- seq(0,5)

for (i in 1:length(n_cases_per_treat[,1])) {
  n_cases_per_treat[i,2] <- est_df_soc_learn_4_busara[est_df_soc_learn_4_busara$numIndLearnersRight == n_cases_per_treat[i,1] & est_df_soc_learn_4_busara$cognitive_load == 0 & est_df_soc_learn_4_busara$similar_ingroup == 1 & est_df_soc_learn_4_busara$observing_ingroup == 1, 'n_cases']
}

text(x = 0, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[1, 2])), adj = 0.5, cex = 1.3)
text(x = 1, y = 0.88, labels = bquote(italic(N) == .(n_cases_per_treat[2, 2])), adj = 0.5, cex = 1.3)
text(x = 2, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[3, 2])), adj = 0.5, cex = 1.3)
text(x = 3, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[4, 2])), adj = 0.5, cex = 1.3)
text(x = 4, y = 0.12, labels = bquote(italic(N) == .(n_cases_per_treat[5, 2])), adj = 0.5, cex = 1.3)
text(x = 5, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[6, 2])), adj = 0.5, cex = 1.3)

for (i in 1:6)
{
  points(temp$numIndLearnersRight[i],temp$propSocLearnersRight[i],pch=19,cex=1.5*cexNum)
  arrows(temp$numIndLearnersRight[i],temp$lowerCI_right[i],temp$numIndLearnersRight[i],temp$upperCI_right[i],length=0.1,angle=90,lwd=3,code=3)
}

################################################################################

# Now comes cognitive load, similar_ingroup, observing ingroup

################################################################################

par(fig=c(0.5,1,0.5,1),new=T)
plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing right',ylab='Proportion social learners choosing right',axes=F,cex.lab=1.6*cexNum)
polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
axis(1,0:5,0:5,cex.axis=1.75*cexNum)
axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
box()
mtext('CL, similar to ingroup, observing ingroup',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('B',side=3,line=1.5,at=-1,cex=2*cexNum)

temp <- est_df_soc_learn_4_busara[est_df_soc_learn_4_busara$cognitive_load == 1 & est_df_soc_learn_4_busara$similar_ingroup == 1 & est_df_soc_learn_4_busara$observing_ingroup == 1, ]

n_cases_per_treat <- as.data.frame(matrix(0,6,2)) 
n_cases_per_treat[,1] <- seq(0,5)

for (i in 1:length(n_cases_per_treat[,1])) {
  n_cases_per_treat[i,2] <- est_df_soc_learn_4_busara[est_df_soc_learn_4_busara$numIndLearnersRight == n_cases_per_treat[i,1] & est_df_soc_learn_4_busara$cognitive_load == 1 & est_df_soc_learn_4_busara$similar_ingroup == 1 & est_df_soc_learn_4_busara$observing_ingroup == 1, 'n_cases']
}

text(x = 0, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[1, 2])), adj = 0.5, cex = 1.3)
text(x = 1, y = 0.88, labels = bquote(italic(N) == .(n_cases_per_treat[2, 2])), adj = 0.5, cex = 1.3)
text(x = 2, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[3, 2])), adj = 0.5, cex = 1.3)
text(x = 3, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[4, 2])), adj = 0.5, cex = 1.3)
text(x = 4, y = 0.12, labels = bquote(italic(N) == .(n_cases_per_treat[5, 2])), adj = 0.5, cex = 1.3)
text(x = 5, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[6, 2])), adj = 0.5, cex = 1.3)

for (i in 1:6)
{
  points(temp$numIndLearnersRight[i],temp$propSocLearnersRight[i],pch=19,cex=1.5*cexNum)
  arrows(temp$numIndLearnersRight[i],temp$lowerCI_right[i],temp$numIndLearnersRight[i],temp$upperCI_right[i],length=0.1,angle=90,lwd=3,code=3)
}


################################################################################

# Now comes no cognitive load, not similar_ingroup, observing ingroup

################################################################################

par(fig=c(0,0.5,0,0.5),new=T)
plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing right',ylab='Proportion social learners choosing right',axes=F,cex.lab=1.6*cexNum)
polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
axis(1,0:5,0:5,cex.axis=1.75*cexNum)
axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
box()
mtext('NCL, dissimilar to ingroup, observing ingroup',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('C',side=3,line=1.5,at=-1,cex=2*cexNum)

temp <- est_df_soc_learn_4_busara[est_df_soc_learn_4_busara$cognitive_load == 0 & est_df_soc_learn_4_busara$similar_ingroup == 0 & est_df_soc_learn_4_busara$observing_ingroup == 1, ]

n_cases_per_treat <- as.data.frame(matrix(0,6,2)) 
n_cases_per_treat[,1] <- seq(0,5)

for (i in 1:length(n_cases_per_treat[,1])) {
  n_cases_per_treat[i,2] <- est_df_soc_learn_4_busara[est_df_soc_learn_4_busara$numIndLearnersRight == n_cases_per_treat[i,1] & est_df_soc_learn_4_busara$cognitive_load == 0 & est_df_soc_learn_4_busara$similar_ingroup == 0 & est_df_soc_learn_4_busara$observing_ingroup == 1, 'n_cases']
}

text(x = 0, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[1, 2])), adj = 0.5, cex = 1.3)
text(x = 1, y = 0.88, labels = bquote(italic(N) == .(n_cases_per_treat[2, 2])), adj = 0.5, cex = 1.3)
text(x = 2, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[3, 2])), adj = 0.5, cex = 1.3)
text(x = 3, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[4, 2])), adj = 0.5, cex = 1.3)
text(x = 4, y = 0.12, labels = bquote(italic(N) == .(n_cases_per_treat[5, 2])), adj = 0.5, cex = 1.3)
text(x = 5, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[6, 2])), adj = 0.5, cex = 1.3)


for (i in 1:6)
{
  points(temp$numIndLearnersRight[i],temp$propSocLearnersRight[i],pch=19,cex=1.5*cexNum)
  arrows(temp$numIndLearnersRight[i],temp$lowerCI_right[i],temp$numIndLearnersRight[i],temp$upperCI_right[i],length=0.1,angle=90,lwd=3,code=3)
}


################################################################################

# Now comes cognitive load, not similar_ingroup, observing ingroup

################################################################################

par(fig=c(0.5,1,0,0.5),new=T)
plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing right',ylab='Proportion social learners choosing right',axes=F,cex.lab=1.6*cexNum)
polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
axis(1,0:5,0:5,cex.axis=1.75*cexNum)
axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
box()
mtext('CL, dissimilar to ingroup, observing ingroup',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('D',side=3,line=1.5,at=-1,cex=2*cexNum)

temp <- est_df_soc_learn_4_busara[est_df_soc_learn_4_busara$cognitive_load == 1 & est_df_soc_learn_4_busara$similar_ingroup == 0 & est_df_soc_learn_4_busara$observing_ingroup == 1, ]

n_cases_per_treat <- as.data.frame(matrix(0,6,2)) 
n_cases_per_treat[,1] <- seq(0,5)

for (i in 1:length(n_cases_per_treat[,1])) {
  n_cases_per_treat[i,2] <- est_df_soc_learn_4_busara[est_df_soc_learn_4_busara$numIndLearnersRight == n_cases_per_treat[i,1] & est_df_soc_learn_4_busara$cognitive_load == 1 & est_df_soc_learn_4_busara$similar_ingroup == 0 & est_df_soc_learn_4_busara$observing_ingroup == 1, 'n_cases']
}

text(x = 0, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[1, 2])), adj = 0.5, cex = 1.3)
text(x = 1, y = 0.88, labels = bquote(italic(N) == .(n_cases_per_treat[2, 2])), adj = 0.5, cex = 1.3)
text(x = 2, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[3, 2])), adj = 0.5, cex = 1.3)
text(x = 3, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[4, 2])), adj = 0.5, cex = 1.3)
text(x = 4, y = 0.12, labels = bquote(italic(N) == .(n_cases_per_treat[5, 2])), adj = 0.5, cex = 1.3)
text(x = 5, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[6, 2])), adj = 0.5, cex = 1.3)

for (i in 1:6)
{
  points(temp$numIndLearnersRight[i],temp$propSocLearnersRight[i],pch=19,cex=1.5*cexNum)
  arrows(temp$numIndLearnersRight[i],temp$lowerCI_right[i],temp$numIndLearnersRight[i],temp$upperCI_right[i],length=0.1,angle=90,lwd=3,code=3)
}


dev.off()

################################################################################
################################################################################
# NOW OUTGROUP
################################################################################
################################################################################

widthWindow <- 14
heightWindow <- 14
cexNum <- 1

# First is similar_ingroup, observing_outgroup, no cognitive_load

quartz(width = widthWindow,height = heightWindow, type='pdf', file='outgroup_busara.pdf')

################################################################################

# Now comes similar_ingroup, observing outgroup, no cognitive load

################################################################################

par(mar=c(5,5.5,4,3),mgp=c(3,1,0),cex.lab=cexNum,cex.axis=1.2*cexNum,fig=c(0,0.5,0.5,1))
plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing right',ylab='Proportion social learners choosing right',axes=F,cex.lab=1.6*cexNum)
polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
axis(1,0:5,0:5,cex.axis=1.75*cexNum)
axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
box()
mtext('NCL, similar to ingroup, observing outgroup',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('A',side=3,line=1.5,at=-1,cex=2*cexNum)

temp <- est_df_soc_learn_4_busara[est_df_soc_learn_4_busara$cognitive_load == 0 & est_df_soc_learn_4_busara$similar_ingroup == 1 & est_df_soc_learn_4_busara$observing_ingroup == 0, ]

n_cases_per_treat <- as.data.frame(matrix(0,6,2)) 
n_cases_per_treat[,1] <- seq(0,5)

for (i in 1:length(n_cases_per_treat[,1])) {
  n_cases_per_treat[i,2] <- est_df_soc_learn_4_busara[est_df_soc_learn_4_busara$numIndLearnersRight == n_cases_per_treat[i,1] & est_df_soc_learn_4_busara$cognitive_load == 0 & est_df_soc_learn_4_busara$similar_ingroup == 1 & est_df_soc_learn_4_busara$observing_ingroup == 0, 'n_cases']
}

text(x = 0, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[1, 2])), adj = 0.5, cex = 1.3)
text(x = 1, y = 0.88, labels = bquote(italic(N) == .(n_cases_per_treat[2, 2])), adj = 0.5, cex = 1.3)
text(x = 2, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[3, 2])), adj = 0.5, cex = 1.3)
text(x = 3, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[4, 2])), adj = 0.5, cex = 1.3)
text(x = 4, y = 0.12, labels = bquote(italic(N) == .(n_cases_per_treat[5, 2])), adj = 0.5, cex = 1.3)
text(x = 5, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[6, 2])), adj = 0.5, cex = 1.3)


for (i in 1:6)
{
  points(temp$numIndLearnersRight[i],temp$propSocLearnersRight[i],pch=19,cex=1.5*cexNum)
  arrows(temp$numIndLearnersRight[i],temp$lowerCI_right[i],temp$numIndLearnersRight[i],temp$upperCI_right[i],length=0.1,angle=90,lwd=3,code=3)
}


################################################################################

# Now comes cognitive load, similar_ingroup, observing outgroup

################################################################################

par(fig=c(0.5,1,0.5,1),new=T)
plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing right',ylab='Proportion social learners choosing right',axes=F,cex.lab=1.6*cexNum)
polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
axis(1,0:5,0:5,cex.axis=1.75*cexNum)
axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
box()
mtext('CL, similar to ingroup, observing outgroup',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('B',side=3,line=1.5,at=-1,cex=2*cexNum)

temp <- est_df_soc_learn_4_busara[est_df_soc_learn_4_busara$cognitive_load == 1 & est_df_soc_learn_4_busara$similar_ingroup == 1 & est_df_soc_learn_4_busara$observing_ingroup == 0, ]

n_cases_per_treat <- as.data.frame(matrix(0,6,2)) 
n_cases_per_treat[,1] <- seq(0,5)

for (i in 1:length(n_cases_per_treat[,1])) {
  n_cases_per_treat[i,2] <- est_df_soc_learn_4_busara[est_df_soc_learn_4_busara$numIndLearnersRight == n_cases_per_treat[i,1] & est_df_soc_learn_4_busara$cognitive_load == 1 & est_df_soc_learn_4_busara$similar_ingroup == 1 & est_df_soc_learn_4_busara$observing_ingroup == 0, 'n_cases']
}

text(x = 0, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[1, 2])), adj = 0.5, cex = 1.3)
text(x = 1, y = 0.88, labels = bquote(italic(N) == .(n_cases_per_treat[2, 2])), adj = 0.5, cex = 1.3)
text(x = 2, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[3, 2])), adj = 0.5, cex = 1.3)
text(x = 3, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[4, 2])), adj = 0.5, cex = 1.3)
text(x = 4, y = 0.12, labels = bquote(italic(N) == .(n_cases_per_treat[5, 2])), adj = 0.5, cex = 1.3)
text(x = 5, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[6, 2])), adj = 0.5, cex = 1.3)


for (i in 1:6)
{
  points(temp$numIndLearnersRight[i],temp$propSocLearnersRight[i],pch=19,cex=1.5*cexNum)
  arrows(temp$numIndLearnersRight[i],temp$lowerCI_right[i],temp$numIndLearnersRight[i],temp$upperCI_right[i],length=0.1,angle=90,lwd=3,code=3)
}



################################################################################

# Now comes not similar_ingroup, observing outgroup, no cognitive load

################################################################################

par(fig=c(0,0.5,0,0.5),new=T)
plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing right',ylab='Proportion social learners choosing right',axes=F,cex.lab=1.6*cexNum)
polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
axis(1,0:5,0:5,cex.axis=1.75*cexNum)
axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
box()
mtext('NCL, dissimilar to ingroup, observing outgroup',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('C',side=3,line=1.5,at=-1,cex=2*cexNum)

temp <- est_df_soc_learn_4_busara[est_df_soc_learn_4_busara$cognitive_load == 0 & est_df_soc_learn_4_busara$similar_ingroup == 0 & est_df_soc_learn_4_busara$observing_ingroup == 0, ]

n_cases_per_treat <- as.data.frame(matrix(0,6,2)) 
n_cases_per_treat[,1] <- seq(0,5)

for (i in 1:length(n_cases_per_treat[,1])) {
  n_cases_per_treat[i,2] <- est_df_soc_learn_4_busara[est_df_soc_learn_4_busara$numIndLearnersRight == n_cases_per_treat[i,1] & est_df_soc_learn_4_busara$cognitive_load == 0 & est_df_soc_learn_4_busara$similar_ingroup == 0 & est_df_soc_learn_4_busara$observing_ingroup == 0, 'n_cases']
}

text(x = 0, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[1, 2])), adj = 0.5, cex = 1.3)
text(x = 1, y = 0.88, labels = bquote(italic(N) == .(n_cases_per_treat[2, 2])), adj = 0.5, cex = 1.3)
text(x = 2, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[3, 2])), adj = 0.5, cex = 1.3)
text(x = 3, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[4, 2])), adj = 0.5, cex = 1.3)
text(x = 4, y = 0.12, labels = bquote(italic(N) == .(n_cases_per_treat[5, 2])), adj = 0.5, cex = 1.3)
text(x = 5, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[6, 2])), adj = 0.5, cex = 1.3)



for (i in 1:6)
{
  points(temp$numIndLearnersRight[i],temp$propSocLearnersRight[i],pch=19,cex=1.5*cexNum)
  arrows(temp$numIndLearnersRight[i],temp$lowerCI_right[i],temp$numIndLearnersRight[i],temp$upperCI_right[i],length=0.1,angle=90,lwd=3,code=3)
}


################################################################################

# Now comes cognitive load, not similar_ingroup, observing outgroup

################################################################################

par(fig=c(0.5,1,0,0.5),new=T)
plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing right',ylab='Proportion social learners choosing right',axes=F,cex.lab=1.6*cexNum)
polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
axis(1,0:5,0:5,cex.axis=1.75*cexNum)
axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
box()
mtext('CL, dissimilar to ingroup, observing outgroup',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
mtext('D',side=3,line=1.5,at=-1,cex=2*cexNum)

temp <- est_df_soc_learn_4_busara[est_df_soc_learn_4_busara$cognitive_load == 1 & est_df_soc_learn_4_busara$similar_ingroup == 0 & est_df_soc_learn_4_busara$observing_ingroup == 0, ]

n_cases_per_treat <- as.data.frame(matrix(0,6,2)) 
n_cases_per_treat[,1] <- seq(0,5)

for (i in 1:length(n_cases_per_treat[,1])) {
  n_cases_per_treat[i,2] <- est_df_soc_learn_4_busara[est_df_soc_learn_4_busara$numIndLearnersRight == n_cases_per_treat[i,1] & est_df_soc_learn_4_busara$cognitive_load == 1 & est_df_soc_learn_4_busara$similar_ingroup == 0 & est_df_soc_learn_4_busara$observing_ingroup == 0, 'n_cases']
}

text(x = 0, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[1, 2])), adj = 0.5, cex = 1.3)
text(x = 1, y = 0.88, labels = bquote(italic(N) == .(n_cases_per_treat[2, 2])), adj = 0.5, cex = 1.3)
text(x = 2, y = 0.95, labels = bquote(italic(N) == .(n_cases_per_treat[3, 2])), adj = 0.5, cex = 1.3)
text(x = 3, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[4, 2])), adj = 0.5, cex = 1.3)
text(x = 4, y = 0.12, labels = bquote(italic(N) == .(n_cases_per_treat[5, 2])), adj = 0.5, cex = 1.3)
text(x = 5, y = 0.05, labels = bquote(italic(N) == .(n_cases_per_treat[6, 2])), adj = 0.5, cex = 1.3)



for (i in 1:6)
{
  points(temp$numIndLearnersRight[i],temp$propSocLearnersRight[i],pch=19,cex=1.5*cexNum)
  arrows(temp$numIndLearnersRight[i],temp$lowerCI_right[i],temp$numIndLearnersRight[i],temp$upperCI_right[i],length=0.1,angle=90,lwd=3,code=3)
}

dev.off()



# Note, I haven't done the pooling in the following yet. I commented it out for now

# NOW MOVE TO POOLING DATA OVER DISCORDANT AND CONCORDANT

# widthWindow <- 14.5
# heightWindow <- 7
# cexNum <- 1
# 
# # Ambiguous, within-subject, prior
# 
# quartz(width = widthWindow,height = heightWindow, type='pdf', file='plotAmbiguousLeft_pooled.pdf')
# 
# par(mar=c(5,5.5,4,3),mgp=c(3,1,0),cex.lab=cexNum,cex.axis=1.2*cexNum,fig=c(0,0.5,0,1))
# plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing left',ylab='Proportion social learners choosing left',axes=F,cex.lab=1.6*cexNum)
# polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
# polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
# lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
# lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
# lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
# axis(1,0:5,0:5,cex.axis=1.75*cexNum)
# axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
# box()
# mtext('Opaque, prior',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
# mtext('A',side=3,line=1.5,at=-1,cex=2*cexNum)
# 
# temp <- resEstAndBootImitationFunctions_ambPooled[resEstAndBootImitationFunctions_ambPooled$prior == 1, ]
# 
# text(x = 0,y = 0.85,labels=expression(italic(N) == 155),adj=0.5,cex=1.3)
# text(x = 1,y = 0.78,labels=expression(italic(N) == 213),adj=0.5,cex=1.3)
# text(x = 1.96,y = 0.85,labels=expression(italic(N) == 130),adj=0.5,cex=1.3)
# 
# text(x = 3.03,y = 0.15,labels=expression(italic(N) == 116),adj=0.5,cex=1.3)
# text(x = 4,y = 0.22,labels=expression(italic(N) == 193),adj=0.5,cex=1.3)
# text(x = 5,y = 0.15,labels=expression(italic(N) == 53),adj=0.5,cex=1.3)
# 
# for (i in 1:6)
# {
#   points(temp$numIndLearnersLeft_pooled[i],temp$propSocLearnersLeft_pooled[i],pch=19,cex=1.5*cexNum)
#   arrows(temp$numIndLearnersLeft_pooled[i],temp$lowerCI_left_pooled[i],temp$numIndLearnersLeft_pooled[i],temp$upperCI_left_pooled[i],length=0.1,angle=90,lwd=3,code=3)
# }
# 
# par(fig=c(0.5,1,0,1),new=T)
# plot(c(0,5),c(0,1),xlim=c(-0.5,5.5),type='l',lty='blank',lwd=0.5,xlab='Number demonstrators choosing left',ylab='Proportion social learners choosing left',axes=F,cex.lab=1.6*cexNum)
# polygon(x = c(0,2.5,2.5),y=c(0,0,0.5),col=gray(0.8))
# polygon(x = c(2.5,2.5,5),y=c(0.5,1,1),col=gray(0.8))
# lines(c(0,2.5),c(1,1),lty='dashed',lwd=1)
# lines(c(0,5),c(0.5,0.5),lty='dashed',lwd=1)
# lines(c(2.5,5),c(0,0),lty='dashed',lwd=1)
# axis(1,0:5,0:5,cex.axis=1.75*cexNum)
# axis(2,c(0,1),c(0,1),cex.axis=1.75*cexNum)
# box()
# mtext('Opaque, no prior',side=3,line=1,cex=1.5*cexNum,adj=0.5,outer=F)
# mtext('B',side=3,line=1.5,at=-1,cex=2*cexNum)
# 
# temp <- resEstAndBootImitationFunctions_ambPooled[resEstAndBootImitationFunctions_ambPooled$prior == 0, ]
# 
# text(x = 0,y = 0.85,labels=expression(italic(N) == 198),adj=0.5,cex=1.3)
# text(x = 1,y = 0.78,labels=expression(italic(N) == 231),adj=0.5,cex=1.3)
# text(x = 1.96,y = 0.85,labels=expression(italic(N) == 121),adj=0.5,cex=1.3)
# 
# text(x = 3,y = 0.15,labels=expression(italic(N) == 88),adj=0.5,cex=1.3)
# text(x = 4,y = 0.22,labels=expression(italic(N) == 165),adj=0.5,cex=1.3)
# text(x = 5,y = 0.15,labels=expression(italic(N) == 77),adj=0.5,cex=1.3)
# 
# for (i in 1:6)
# {
#   points(temp$numIndLearnersLeft_pooled[i],temp$propSocLearnersLeft_pooled[i],pch=19,cex=1.5*cexNum)
#   arrows(temp$numIndLearnersLeft_pooled[i],temp$lowerCI_left_pooled[i],temp$numIndLearnersLeft_pooled[i],temp$upperCI_left_pooled[i],length=0.1,angle=90,lwd=3,code=3)
# }
# 
# dev.off()

rm(list=ls())

setwd('..')


