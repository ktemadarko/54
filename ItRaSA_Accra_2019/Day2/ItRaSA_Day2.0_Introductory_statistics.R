###Basic statistics


#Set working directory
setwd("C:/Users/Ha/Desktop/Konstanz/TReND/Accra_2019")

#1 Load your data
df<-read.csv("leaftraits.csv",header=T)

#Examine your data
str(df)
summary(df)
head(df)

#Chi-square test---------------------------------------------------
#Q -- is there an association between deciduousness vs evergreen leaf type and needle vs broad leaf type?
m0 <- chisq.test(df$Decid_Evergreen, df$Needle_Broadlf)
#Print results
m0

#Correlation tests------------------------------------------------
#Q-- Are there relationships between N & P content, and Leaf-mass-area?
m1_1 <- cor.test(df$Nmass, df$Pmass)
m1_2 <- cor.test(df$Nmass, df$LMA)
m1_3 <- cor.test(df$Pmass, df$LMA)

#Print results
m1_1
m1_2
m1_3


#T-test-----------------------------------------------------
#Q-- Is there a difference in the amount of Nitrogren fixed by N2- vs non-N2-fixing plants?
m2<-t.test(df$Nmass~df$N2_fixer)

#Print results
m2

#ANOVA (one-way)------------------------------------------------------
#Q -- Does leaf mass area differ in plants with different growth forms?
m3<-aov(LMA~GF, data=df)

#Print results
summary(m3)

#Examine differences between groups
TukeyHSD(m3)


#test assumptions: 1) residuals (unexplained variance) are normally distributed
#2) Homogeneity of variance across groups
plot(m3)

bartlett.test(df$LMA,df$GF)
#ANOVA (two-way)-------------------------------------------------------
#Q -- Does leaf length differ among and between N2- vs non-N2-fixing, and deciduous vs evergreen plants?
m4<-aov(LL~N2_fixer*Decid_Evergreen, data=df)

#Print results
summary(m4)

#Examine differences between groups
TukeyHSD(m4)

#Linear regression---------------------------------------------------
#Q -- What is the response of leaf N content to Leaf Mass Area?
m5_1<-lm(Nmass~LMA, data=df)

#Print results
summary(m5_1)

#Test assumptions
plot(m5_1)

#Q -- What is the response of leaf N content to LMA, and to N2-fixing ability?
m5_2<-lm(Nmass~LMA + N2_fixer, data=df)

#Print results
summary(m5_2)

#Q -- What is the response of leaf N content to LMA, and does that relationship vary with N2-fixing ability?
m5_3<-lm(Nmass~LMA*N2_fixer, data=df)

#Print results
summary(m5_3)


#Bonus-------------------------------------------------------------------------

df1<-read.csv('forest_data.csv')

#1
cor.test(df1$Height, df1$DBH)

#2
chisq.test(df1$Profile, df1$Species)

#3
x1<-t.test(Height~Species, data=df1)

#4
x2<-lm(Height~Slope, data=df1)

#5
x3<-lm(Height~Slope + Species, data=df1)

#6
x4<-lm(Height~Slope*Species, data=df1)

#7
x5<-lm(DBH~Profile, data=df1)  #Problem here?