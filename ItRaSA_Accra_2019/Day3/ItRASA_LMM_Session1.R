# INFO ####
# @author: Fraenzi Koerner, Eva Maria Malecore @date: September 2018
# @project: Introduction to R and Statistical Analyses
# @script: Install packages offline
# @filename: ItRaSA_LMM_Session1
# @update: November 2019

# Libraries ####
library(arm)  #also loads library lme4
library(blmeco) #cortbowl data
library(lattice) #bwplot()
library(nmle)
library(lme4)

# Input ####
setwd("D:/Workspace/R/Introduction_to_R_and_Statistical_Analyses_2018/Day3/Data")
dat=read.table("cortbowl.txt",h=TRUE)
str(dat)  

# Reorder levels (reference is "before")
dat$days=factor(dat$days, levels=c("before", "2", "20"))#define the reference levels (before = day of implantation)
str(dat)  

# the data was sampled in 2004,2005, and 2006 by the Swiss Ornithologicla Institute
# Brood = Brood number (brood=family of birds produced at one hatching)
# Ring = individual ring number
# Implant = treatment, C= corticosterone implant, P =  Placebo implant
# Age = age of the nestlings (days)
# days = days when the blood samples were collected (before =  day of implantation, day 2 and day 20 after implantation)
# totCort =  total corticosterone (blood samples taken within 3 minutes of opening the nest box)

# Plot the data ####
par(mfrow=c(1,2), mar=c(4,0,2,0.2), oma=c(0,5,0,0)) # divide the windows in 2 plots and set graphical parameters (mar: margins, oma: outer margins)
for(treat in levels(dat$Implant)){ #loops through the treatments: C and P
  plot(as.numeric(dat$days), dat$totCort, 
       type="n",xlim=c(0.5, 3.5),
       las=1, yaxt="n", xaxt="n", xlab="Days after implantation")
  axis(1, at=1:3, labels=c("before", "2", "20"))
  
  if(treat=="C") { axis(2, las=1)
  	               mtext("Corticosterone",3,line=1) }
    else  mtext("Placebo",3,line=1) 
  
  inds=dat$Ring[dat$Implant==treat]
  for(i in inds){
    lines(dat$days[dat$Ring==i], dat$totCort[dat$Ring==i])
  }
}
mtext("Total corticosterone [ng/ml]", side=2, outer=TRUE, line=3, cex=1.2)

library(lattice)
bwplot(totCort ~ days|Implant, data=dat, xlab="days", cex.lab=1.2)
# Handy but not used as it is intended to be used: 
# Implant is not a grouping factor, but a treatment!

# Linear mixed model using REML ####
mod.REML=lmer(log(totCort) ~ Implant + days +
                Implant:days + (1|Ring), data=dat, REML=TRUE)   # using REML
# REML gives unbiased estimates for the variance components
# but are biased in the variance of the fixed effects
mod.REML
summary(mod.REML)
fixef(mod.REML)  # extracts the fixed effects
ranef(mod.REML)  # extracts the random effects
coef(mod.REML)  # extracts coefficients for each "Ring"

fixef(mod.REML)[1]
ranef(mod.REML)[[1]][1:6,]
coef(mod.REML)[[1]][1:6,1]

ranef(mod.REML)[[1]][1:6,]+fixef(mod.REML)[1]

# Linear mixed model using ML  ####
mod.LM=lmer(log(totCort) ~ Implant + days + Implant:days + (1|Ring), data=dat, REML=FALSE)   # using ML
mod.LM
summary(mod.LM)
fixef(mod.LM)  # extracts the fixed effects
ranef(mod.LM)  # extracts the random effects 
coef(mod.LM)  # extracts coefficients for each "Ring"

# Assessing model assumptions ####
par(mfrow=c(2,2), mar=c(4,4,2,1), mgp=c(2.2,0.8,0))
scatter.smooth(fitted(mod.LM), resid(mod.LM)); abline(h=0, lty=2)
mtext("Tukey-Anscombe Plot", 3, line=0.8, cex=0.8)  # residuals vs. fitted

qqnorm(resid(mod.LM), main="Normal qq-plot, residuals", cex.main=0.8) # qq of residuals
qqline(resid(mod.LM))

scatter.smooth(fitted(mod.LM), sqrt(abs(resid(mod.LM)))) # res. var vs. fitted

qqnorm(ranef(mod.LM)$Ring[,1], main="Normal qq-plot, random effects", cex.main=0.8)
qqline(ranef(mod.LM)$Ring[,1]) # qq of random effects

# Drawing conclusions ####
# Bayesian approach
set.seed(18)# specify the seed (starting value for your random generator)
nsim=2000
bsim=sim(mod.LM, n.sim=nsim)
str(bsim)

round(apply(bsim@fixef, 2, quantile, prob=c(0.025,0.5,0.975)),3) #credible interval
# @: Extract the contents of a slot in a object with a formal (S4) class structure.
#
# A credible interval is the interval in which an (unobserved) parameter
# has a given probability. It's the Bayesian equivalent of the confidence
# interval you've probably encountered before. However, unlike a
# confidence interval, it is dependent on the prior distribution
# (specific to the situation)

newdat=expand.grid(Implant = factor(c('C','P'),levels=levels(dat$Implant)),
                    days= factor(c('before','2','20'),levels=levels(dat$days))) # create a new data frame
Xmat=model.matrix(~ Implant + days + Implant:days, data=newdat)  # use exactly the same formula as for the fixed-effect part in the model specification
fitmat=matrix(ncol=nsim, nrow=nrow(newdat))
for(i in 1:nsim) fitmat[,i]=Xmat %*% bsim@fixef[i,] # fitted values
newdat$lower=apply(fitmat, 1, quantile, prob=0.025)
newdat$upper=apply(fitmat, 1, quantile, prob=0.975)
newdat$fit=Xmat %*% fixef(mod.LM)

# Draw a graph with 95% CrI for each group at each day ####
par(mfrow=c(1,1), mar=c(2,2,1,1), omi= c(0.5,0.5,0,0))# "mar" sets the margins (lower, left, upper, right) around the plot
indexP=newdat$Implant=='P'
indexC=newdat$Implant=='C'
dat$daysNum=ifelse(dat$days=='before', 0, as.character(dat$days)) # nummeric day variable
dat$daysNum=as.numeric(dat$daysNum) # nummeric day variable

a=1
plot(seq(-2,22,1), seq(-0.1,4.8,length=25), yaxt="n", xaxt="n", type="n", xlab="", ylab="", las=2, cex.lab=a,
     main="", cex.main=a, font.main=1)
points(jitter(dat$daysNum[dat$Implant=='P']-0.4), log(dat$totCort[dat$Implant=='P']), lwd=2 ,pch=16, col="blue",   cex=0.5)          # Placebo raw data
points(jitter(dat$daysNum[dat$Implant=='C']+0.2), log(dat$totCort[dat$Implant=='C']), lwd=2 ,pch=16, col="orange", cex=0.5, lty=2)   # Cort raw data

segments(c(0,2,20)-c(0.4,0.4,0.4), newdat$lower[indexP], c(0,2,20)-c(0.4,0.4,0.4), newdat$upper[indexP], lty=1, lwd=2, col="black" ) # CrI 
points(c(0,2,20)-c(0.4,0.4,0.4), newdat$fit[indexP], lwd=2 ,pch=16, col="black", cex=1.2)     # Placebo 

segments(c(0,2,20)+c(0.2,0.2,0.2), newdat$upper[indexC], c(0,2,20)+c(0.2,0.2,0.2), newdat$lower[indexC],lty=1, lwd=2, col="black" )  # CrI 
points(c(0,2,20)+c(0.2,0.2,0.2), newdat$fit[indexC], lwd=1 ,lty=1, pch=21, bg="white", col="black", cex=1.2) # CORT 

axis(side=2, labels=F,at=seq(0,4.8,0.4),line=0,tcl=-0.3,las=1)
axis(side=2, labels=seq(0,4.8,0.4),at=seq(0,4.8,0.4),line=0,tcl=-0.3,las=1, mgp=c(3,0.5,0),cex.axis=a)    
axis(side=1, labels=c(NA, 2,20),at=c(0, 2,20),line=0,tcl=-0.3,las=1)
axis(side=1, labels=c("before"),at=c(-0.5),line=0,tcl=0,las=1)

mtext(side=2,line=3,adj=0.5,cex=a,font=2,"Total corticosterone [log, ng/ml]",las=0,outer=FALSE)
mtext(side=1,line=3,adj=0.5,cex=a,font=2,"Days after implantation",outer=FALSE)

points(c(10,10),c(4.5,4.8),pch=21,bg=c("black","white"))
text(c(11,11),c(4.5,4.8),c("placebo", "corticosterone"),adj=c(0,0.5))
# or use the legend() function
# legend("bottom",legend=c("placebo", "corticosterone"),pch=c(19,21),col=c("black","black"),bty="n")

# Use lme() with p-values ####
library(nlme)
mod=lme(log(totCort)~Implant+days+Implant:days,
         random=~1|Ring, data=dat)
summary(mod)

# Random intercept and random slope ####
# random slope model
dat=read.table("wingbowl.txt",h=TRUE)
str(dat) # the data was sampled in 2004 by the Swiss Ornithologicla Institute
# Brood = Brood number
# Ring = individual ring number
# Implant = treatment, C= corticosterone implant, P =  placebo implant
# Age1 =  age at the day of implantation
# Age = age of the nestlings (days)
# days = days when the wing length measurements were taken (day 1 =  day of implantation, day 3 and day 21)
# Wing = maximum left wing length of barn owl nestlings

# Include a random slope
dat$Age.z=scale(dat$Age)
mod=lmer(Wing ~ Age.z + Implant + Age.z:Implant + (Age.z|Ring), data=dat, REML=FALSE)  # using ML
mod
summary(mod)
fixef(mod)  # extracts the fixed effects
ranef(mod)  # extracts the random effects
coef(mod)  # extracts coefficients for each "Ring"

# Check model assumptions
par(mfrow=c(2,3))
scatter.smooth(fitted(mod),resid(mod)); abline(h=0, lty=2)
title("Tukey-Anscombe Plot")  # residuals vs. fitted
qqnorm(resid(mod), main="normal QQ-plot, residuals") # qq of residuals
qqline(resid(mod))
scatter.smooth(fitted(mod), sqrt(abs(resid(mod)))) # res. var vs. fitted
qqnorm(ranef(mod)$Ring[,1], main="normal QQ-plot, random effects", cex.main=0.8)
qqline(ranef(mod)$Ring[,1]) # qq of random effects
qqnorm(ranef(mod)$Ring[,2], main="normal QQ-plot, random effects", cex.main=0.8)
qqline(ranef(mod)$Ring[,2]) # qq of random effects

# drawing conclusions
set.seed(0470)  # specify the seed (starting value for your random generator)
nsim=2000
bsim=sim(mod, n.sim=nsim)
colnames(bsim@fixef)=names(fixef(mod))
apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))

round(quantile(bsim@fixef[,"Age.z:ImplantP"]/sd(dat$Age), prob=c(0.025,0.975)),3)

newdat=expand.grid(Age=seq(23, 45, length=100), Implant=levels(dat$Implant))
newdat$Age.z=(newdat$Age - mean(dat$Age))/sd(dat$Age)
Xmat=model.matrix(~ Age.z + Implant + Age.z:Implant, data=newdat)
fitmat=matrix(ncol=nsim, nrow=nrow(newdat))
for(i in 1:nsim) fitmat[,i] = Xmat %*% bsim@fixef[i,]
newdat$lower=apply(fitmat, 1, quantile, prob=0.025)
newdat$upper=apply(fitmat, 1, quantile, prob=0.975)

dev.off()
par(mfrow=c(1,2), mar=c(5,1,1,1), oma=c(0,4,0,0))
plot(dat$Age.z, dat$Wing, col=c("orange", "blue")[as.numeric(dat$Implant)],
     pch=1, cex=0.8, las=1, xlab="Age (days)", ylab=NA, xaxt="n")           # plot using Age.z, but label x-Axis manually to represent the original scale
at.x_orig = seq(25,45,by=5)                          # values on the x-axis that we want to be labeled, on the original scale
at.x      = (at.x_orig - mean(dat$Age))/sd(dat$Age)  # corresponding transformed values (only works for a linear transformation such as "scale")
axis(1, at=at.x, labels=at.x_orig)
mtext("Wing length (mm)", side=2, outer=TRUE, line=2, cex=1.2, adj=0.6)
abline(fixef(mod)[1],               fixef(mod)[2],               col="orange", lwd=2)
abline(fixef(mod)[1]+fixef(mod)[3], fixef(mod)[2]+fixef(mod)[4], col="blue", lwd=2)
for(i in 1:2){
  index = newdat$Implant==levels(newdat$Implant)[i]
  polygon(c(newdat$Age.z[index], rev(newdat$Age.z[index])), c(newdat$lower[index], rev(newdat$upper[index])), 
          border=NA, col=c(rgb(1,0.65,0,0.5), rgb(0,0,1,0.5))[i])
}
# plot with individual specific regression lines
plot(dat$Age.z, dat$Wing, col=c("orange", "blue")[as.numeric(dat$Implant)],
     pch=1, cex=0.8, las=1, xlab="Age (days)", ylab=NA, yaxt="n", xaxt="n")
at.x_orig = seq(25,45,by=5)
at.x      = (at.x_orig - mean(dat$Age))/sd(dat$Age)
axis(1, at=at.x, labels=at.x_orig)

indtreat=tapply(dat$Implant, dat$Ring, function(x) as.character(x[1]))
for(i in 1:86){
  if(indtreat[i]=="C") abline(fixef(mod)[1]+              ranef(mod)$Ring[i,1], 
                                 fixef(mod)[2]+              ranef(mod)$Ring[i,2],
                              col="orange") 
  else
                       abline(fixef(mod)[1]+fixef(mod)[3]+ranef(mod)$Ring[i,1],
                                 fixef(mod)[2]+fixef(mod)[4]+ranef(mod)$Ring[i,2],
                              col="blue")
}

# difference in wing length at day 45 (shortly before fleding)
transf.45 <- (45-mean(dat$Age))/sd(dat$Age)   # get the transformed value of Age==45
fixef(mod)[1] + fixef(mod)[2]*transf.45 - (fixef(mod)[1]+fixef(mod)[3] +  (fixef(mod)[2]+fixef(mod)[4])*transf.45)   # 8.7 mm shorter wings for Cort.

#Nested and crossed random effects  ####
dat=read.table("cortbowl.txt",h=TRUE)
str(dat) 
#we include the random intercept brood to our model
data(cortbowl)

mod=lmer(log(totCort) ~ Implant + days + Implant:days + (1|Brood) + (1|Ring),
         data=cortbowl, REML=FALSE)   # using ML
#Important: only when there are  unique level names . Otherwhise use only for crossed structure.
mod

# note that you can also specify directly your nested random effects 
mod =lmer(log(totCort) ~ Implant + days + Implant:days + (1|Brood/Ring),
          data=cortbowl, REML=FALSE)   # using ML
# Here we sepcify the nested structure. Even if we have not unique level names, it would wourk out
mod
summary(mod)
fixef(mod)  # extracts the fixed effects
ranef(mod)  # extracts the random effects
coef(mod)  # extracts coefficients for each "Ring"

# Assessing model assumptions
par(mfrow=c(2,2))
scatter.smooth(fitted(mod),resid(mod)); abline(h=0, lty=2)
title("Tukey-Anscombe Plot")  # residuals vs. fitted
qqnorm(resid(mod), main="normal QQ-plot, residuals") # qq of residuals
qqline(resid(mod))
scatter.smooth(fitted(mod), sqrt(abs(resid(mod)))) # res. var vs. fitted
qqnorm(ranef(mod)$Brood[,1], main="normal QQ-plot, random effects", cex.main=0.8)
qqline(ranef(mod)$Brood[,1]) # qq of random effects

