# INFO ####
# @author: Eva Maria Malecore @date: October 2017
# @project: An Introduction to R and Statistical Analyses
# @script: Session 3, Introduction to R: data exploration
# @filename: AItRaSA_Session3.R
# @update:

# Data input ####
leaftraits=read.csv("D:/Workspace/R/Introduction_to_R_and_Statistical_Analyses_2019/Day1/Data/leaftraits.txt",sep="\t")

table(leaftraits$GF)

min(leaftraits$LL,na.rm=TRUE)
max(leaftraits$LL,na.rm=TRUE)

# What are the mean values for Leaf Lifespan per Growth Form?
aggregate(LL~GF, data=leaftraits,mean)
?aggregate

# What are the mean values per Leaf Lifespan, for deciduous and evergreen leaf types, within Growtn Form?
aggregate(LL~Decid_Evergreen+GF, data=leaftraits, mean)

# What is the spread of the data like?
plot(leaftraits$LMA,ylab="Leaf Mass per unit area (g/m^2)",cex.lab=1.2)

dotchart(leaftraits$LMA, ylab="Leaf Mass per unit area(g/m^2)", cex.lab=1.4)

# What is the spread of the data like?
boxplot(leaftraits$LMA,ylab="Leaf Mass per unit area (g/m^2)", cex.lab=1.4)

# What is the spread of the data like?
hist(leaftraits$LMA,xlab="Leaf Mass per unit area(g/m^2)",
     ylab="Frequency",cex.lab=1.4,col="green",
     main="Histogram of Leaf Mass per unit area")

# What do relationships between variables look like?
plot(leaftraits$LMA,leaftraits$Nmass, xlab="Leaf Mass per unit area (g/m^2)",
     ylab="Nitrogen per unit mass (%)", pch=16, cex.lab=1.4)


