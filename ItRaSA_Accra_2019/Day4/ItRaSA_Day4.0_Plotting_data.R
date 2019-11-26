#load ggplot package
library(ggplot2)

#Set working directory
setwd("C:/Users/Ha/Desktop/Konstanz/TReND/Accra_2019")

#1 Load your data
leaftraits<-read.table("leaftraits.txt",header=T)

#Exercise 1-----------------------------------------------

#make data removing NAs from N2_fixer
newdata<-leaftraits[which(leaftraits$N2_fixer!="NA"),]

#Plot with point geometry
ggplot(data=newdata, aes(x=N2_fixer, y=Nmass)) + 
  geom_point() 

#Plot with boxplot geometry
ggplot(data=newdata, aes(x=N2_fixer, y=Nmass)) + 
  geom_boxplot() 

#Exercise 2: improve the former figure--------------------------
ggplot(data=newdata, aes(x=N2_fixer, y=Nmass)) + 
  geom_boxplot() +
  theme_classic()+
  xlab("Nitrogen fixing ability")+
  ylab("Leaf Nitrogen content")+
  theme(text=element_text(size=15))

#Exercise 3: smoothing lines--------------------------------
ggplot(data=leaftraits, aes(x=Pmass, y=Nmass)) + 
  geom_point() +
  ylab("Nitrogen content")+ 
  xlab("Phosphorous content")+
  theme_classic() +
  geom_smooth()

#Change smoothing method
ggplot(data=leaftraits, aes(x=Pmass, y=Nmass)) + 
  geom_point() +
  ylab("Nitrogen content")+ 
  xlab("Phosphorous content")+
  theme_classic() +
  geom_smooth(method='lm')


#Exercise 4: grouping by color------------------------------------
#Make the dataset without NAs
newdata2<-subset(newdata, Decid_Evergreen!="NA")

#Make the plot
ggplot(data=newdata2, aes(x=N2_fixer, y=Nmass, color=Decid_Evergreen)) + 
  geom_boxplot() +
  ylab("Nitrogen content")+ 
  xlab("Nitrogen-fixing ability")+
  theme_classic()



#Exercise 5: Faceting-----------------------------
#Make the dataset
newdata3<-subset(newdata2, Needle_Broadlf!="NA")

ggplot(data=newdata3, aes(x=N2_fixer, y=Nmass, color=Decid_Evergreen)) + 
  geom_boxplot() +
  facet_grid(.~Needle_Broadlf)+
  ylab("Nitrogen content")+ 
  xlab("Nitrogen-fixing ability")+
  theme_classic()

#Multipanel figures---------------------------------------------
plot1 <- ggplot(data=newdata2, aes(x=N2_fixer, y=Nmass, color=Decid_Evergreen)) + 
  geom_boxplot() +
  ylab("Nitrogen content")+ 
  xlab("Nitrogen-fixing ability")+
  theme_classic()

plot2 <- ggplot(data=leaftraits, aes(x=Pmass, y=Nmass)) + 
  geom_point() +
  ylab("Nitrogen content")+ 
  xlab("Phosphorous content")+
  theme_classic() +
  geom_smooth(method='lm')

library(gridExtra)
grid.arrange(plot1, plot2)


#Extra: multiple geometries--------------------------------------
newdata4<-aggregate(Nmass~N2_fixer+Decid_Evergreen, data=newdata3, mean)
newdata5<-aggregate(Nmass~N2_fixer+Decid_Evergreen, data=newdata3, sd)
newdata4$Nmass_sd=newdata5$Nmass


ggplot(newdata4, aes(x=N2_fixer, y=Nmass, color=Decid_Evergreen, group=Decid_Evergreen)) +
  geom_point(size=3, position = position_dodge(width = 0.5))+
  geom_line(size=1, position = position_dodge(width = 0.5))+
  geom_errorbar(aes(ymin=Nmass-Nmass_sd,
                    ymax=Nmass+Nmass_sd,
                    color=Decid_Evergreen), 
                width=.3,
                size=1,
                position = position_dodge(width = 0.5))+
  theme_classic()#+
  #scale_color_manual(values=c("pink", "green"))
