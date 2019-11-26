# INFO ####
# @author: Noelie Maurel, Eva Maria Malecore @date: September 2018
# @project: Introduction to R and Statistical Analyses
# @script: Day 3 - Generalized Linear Model (GLM)
# @filename: ItRaSA_Day3.2_GLM.R
# @update: November 2019

# Poisson GLM ####

# We will use the dataset about plant species richness and soil nitrogen used for mixed-effects modeling
fields=read.table("D:/Workspace/R/Introduction_to_R_and_Statistical_Analyses_2019/Day3/Data/fields.txt",sep="\t",header=TRUE)
str(fields)

# Run model ####
glmRich<-glm(Species_richness~N_mg_g_soil,data=fields,family="poisson")
summary(glmRich)

# We need to use Pearson residuals to do model validation
par(mfrow=c(1,2))
pi=resid(glmRich,type="pearson")  # Pearson residuals
plot(fitted(glmRich),pi,ylab="Pearson residuals",cex.lab=1.5,cex.axis=1.3)
plot(fields$N_mg_g_soil,pi,xlab="Soil N (mg.g-1)",ylab="Pearson residuals",cex.lab=1.5,cex.axis=1.3)

# We should check for overdispersion (we want a value close to 1)
# we can compare the residual deviance with the residual degrees of freedom directly,
# or we can manually calculate the dispersion parameter rho:

# deviance residuals dispersion parameter #
rdev<-sum(residuals(glmRich)^2) # residual deviance
rdf<-nrow(fields)-length(coef(glmRich)) # residual degrees of freedom
rdev/rdf # dispersion estimate

# Pearson residuals dispersion parameter #
sum(resid(glmRich,type="pearson")^2)/rdf


# Plot model ####

# We make a new data.frame of x values for which we want to plot fitted y values
xval<-data.frame(N_mg_g_soil=seq(from= 0,to= 60,by =0.5)) # new data frame with evenly distributed x values
xval
y<-predict(glmRich,newdata=xval,type="link",se=TRUE) # predict new values based on the x values we have created in xval
# se=TRUE gives the standard errors about the fitted values allowing us to plot confidence interval
y$fit
exp(y$fit)	# the predicted values
hist(y$fit)
hist(exp(y$fit))

par(mfrow=c(1,1))
plot(fields$Species_richness~fields$N_mg_g_soil,xlab="Soil N (mg.g-1)",ylab="Species richness",cex.axis=1.3,cex.lab=1.5) # we plot the data points (observed values)
lines(xval$N_mg_g_soil,exp(y$fit),lty=1,lwd=2) # plots the fitted line
y$se.fit # the 'predict' function gave us the standard errors about the fitted values
yup<-exp(y$fit+(1.96*y$se.fit)) # this allows us to get the 95% confidence interval upper bound 
ylow<-exp(y$fit-(1.96*y$se.fit)) # and the 95% confidence interval lower bound
lines(xval$N_mg_g_soil,yup,lty=2)
lines(xval$N_mg_g_soil,ylow,lty=2)

# Binomial GLM with Bernoulli data ####
# We will use a dataset about naturalization of plants in Tanzania
abg<-read.table("D:/Workspace/R/Introduction_to_R_and_Statistical_Analyses_2019/Day3/Data/naturalizationABG.txt",sep="\t",header=TRUE)
str(abg)

# Run model ####
# We subset the data we need for the analysis
abgOK<-abg[abg$surviving==1 & !is.na(abg$no_plants),]
table(abgOK$nat)

# How is number of plants distributed?
summary(abgOK$no_plants)
plot(abgOK$no_plants) # we have extreme upper outliers, which may be influential in an analysis
# we should try a log transformation
plot(log(abgOK$no_plants))
hist(log(abgOK$no_plants)) # normal distribution not so important, but we have got rid of big outliers
# let's add a column with log-transformed number of plants (to avoid issues with extreme outliers)
abgOK$logplants<-log(abgOK$no_plants)

# We can get an idea of the relationship between number of plants planted and naturalization success
plot(abgOK$logplants,abgOK$nat,xlab="Number of plants (log)",ylab="Probability of naturalization",cex.lab=1.5,cex.axis=1.3)
# Because our response is binary, it is difficult to see all data points.
# We can use 'jitter' to better visualise the data
plot(abgOK$logplants,jitter(abgOK$nat,0.1),xlab="Number of plants (log)",ylab="Probability of naturalization",cex.lab=1.5,cex.axis=1.3)

# Fit GLM
glmNat<-glm(nat~logplants,data=abgOK,family="binomial")
summary(glmNat)

# Plot model ####

# We plot the data points
plot(abgOK$logplants,jitter(abgOK$nat,0.1),xlab="Number of plants (log)",ylab="Probability of naturalization",cex.lab=1.5,cex.axis=1.3)

# we check the range of values for the x-variable (logplants)
min(abgOK$logplants)
max(abgOK$logplants)

# We make a new data.frame of x values for which we want to plot fitted y values
xval<-data.frame(logplants=seq(from= 0,to=9,by =0.01))  # remember, this is on log scale
y<-predict(glmNat,newdata=xval,type="link",se=TRUE) # 'link': the predicted probabilities are given as log odds, on the linear scale
yfit<-exp(y$fit)/(1+exp(y$fit)) # calculating the predicted probability using the log odds
lines(xval$logplants,yfit,lty=1,lwd=2)
yup<-exp(y$fit+1.96*y$se.fit)/(1+exp(y$fit+1.96*y$se.fit)) # the upper 95% confidence interval limit
ylow<-exp(y$fit-1.96*y$se.fit)/(1+exp(y$fit-1.96*y$se.fit)) # the lower 95% confidence interval limit
lines(xval$logplants,yup,lty=2)
lines(xval$logplants,ylow,lty=2)
