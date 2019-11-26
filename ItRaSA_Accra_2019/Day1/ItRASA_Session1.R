# INFO ####
# @author: Eva Maria Malecore @date: October 2017
# @project: An Introduction to R and Statistical Analyses
# @script: Session 1, Introduction to R: let's get started
# @filename: ItRaSA_Session1.R
# @update:

# The hashtag-symbol "#" is used for comments. Anything preceded by # is not read as a command
# Script 1 ####
# A comment followed by four "#" (see line 1 or line 12) becomes a section title
# Alternatives: use four "-" (line 13) or four "=" (line 14)
# Sectoin title 1 ####
# Sectoin title 2 ----
# Section title 3 ====
# This allows for easy navigation in your R script

# 1. Directories ####
getwd() # get the current working directory 
setwd("D:/Workspace/R/Introduction_to_R_and_Statistical_Analyses_2019/Day1") # set a new working directory
setwd(paste(getwd(),"/Data",sep=""))
getwd()

# Write data
data=1
write.table(data,paste(getwd(),"/NewData",sep=""))

# 2. Packages ####
install.packages("lme4")
library("lme4")


# 3. Expressions, assignments and functions ####
4+3 # expression
x=4+3 # assignment
mean(1:10) # function

# 4. Object and data classes ####

# Atomic vector types
char1="This is a character string"
num1=3.14
int1=3
c1=2+4i
log1=FALSE
raw1=as.raw(16)

vec0=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)

list1=list(char1,num1,int1,c1,log1,raw1)
list2=list(vec0,c(1:10),seq(1,3,0.5))

# Data classes
vec1=factor(c("A", "B", "A", "C", "B", "B", "A", "C"))
levels(vec1)

mat1=matrix(  c(2, 4, 3, 1, 5, 7), nrow=3, ncol=2)
mat1[2,2]

df=as.data.frame(mat1)
str(df)

names(df)
names(df)=c("Column1","Column2")
names(df)
str(df)
df$Column1

class(df)
list1[[3]][1]

list2[[2]][2]

# 5. Input data ####
data=read.csv("D:/Workspace/R/Introduction_to_R_and_Statistical_Analyses_2019/Day1/Data/species_richness~soil_nitrate.txt",sep="\t")

library("ape")
MyTree=read.tree("MyTree.tre")
plot.phylo(MyTree)

# 6. Managing your workspace ####
ls()
rm(c1)
ls()
rm(list=ls())
ls()

# 7. Handling data in R: useful functions ####
# Arithmetic operators
3+4
4-12
45*2
67/3
3^3
67%%3
67%/%3

# Logical operators
123<4
43>=43
4*4==16
x=F; y=T;
x&y
x|y
isTRUE(x)

# Useful functions
data=read.csv("C:/Users/Administrator/Documents/AItRASA/Data/species_richness~soil_nitrate.txt",sep="\t")

head(data)
tail(data)
names(data)

sub1=subset(data,data$Species_richness==30)
dim(sub1)
table(data$Species_richness)

# 8. Saving your work when closing R ####

# 9. Simple plots ####
x=seq(1:12)
y=seq(4:15)
plot(x,y)
plot(x,y,pch=15,col="green",
     xlab="x label",ylab="y label",main="My plot")
points(6,6,col="blue")
abline(1,2)
abline(h=2)
abline(v=6,col="red")

# 10. Gettin help ####
?plot
?mean
??mean
# ...and of course there is always the internet!



