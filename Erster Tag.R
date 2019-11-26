#Info
#Basics ####
#How to make sections "====" or four hastags "####" or "----"
getwd()

#Section 2- ====

m1<-mean(1:10) #assignment

#LISTS- Section3----
#lists are shown in a column on a separate line
#c= vectors are shown on the same line


#1:10 =(ordered consective numbers for 1 to 20)

#c(= creates a vector)
odd<- function(x){
  
  return (x)
}

x<-c("A","b","c")

view(x)

y<-c(1,"a")
z<-c(1,y)

z1<-list(1,2,"a","b")
#factors ----
#factors has levels - categorical values
#matices====
#multi-dimensional vectors (same data type)rows (first number) and columns (first number)
#dim()= retrieve the dimension

mat1<-matrix(c(1,2,3,4,5,6),nrow=2, ncol=3)

#dataframe and  ====
#like matrices but different data types with headers
#headers accessed with $
df=as.data.frame(mat1)
df1=data.frame(Name=c("Peter A","Joyce B", "Ebenezer S","Mike O"), Yrs=c(4,3,2,4), Age=c(64,65,66,64))
df1$Gender=as.factor(c('m', 'f', "m", "m"))

#plot----
p1<-function(x1,y1){
  p2<-plot(x1,y1)
  return (p2)}

p1(df1$Name,df1$Yrs)

p1(df1$Name,df1$Age)

p1(df1$Gender,df1$Age)

p1(wb1,df3$Freq)

str(df1$Gender)
str(df3$Brood_ID)
str(wb$Brood)
wb1<-as.factor(wb$Brood)
str(wb1)

plot(as.factor(df1$Gender),df1$Age)
#p1+ pch=22 col=rgb(0.2,0.4,0.6)

#subsets====
sub5=df1[df1$Yrs<=4 & df1$Yrs>2,]
#sub3=df1[2<df1$Yrs,] the same sub4=df1[df1$Yrs>2,]


#class====
class(df)
str(df)
data<-read.csv("C:/Users/SunDevil/Desktop/ItRaSA_Accra_2019/Data/fields.txt", sep="\t")
sub1=subset(data,data$Species_richness==30)

?pch

#tree====
library("ape")
MyTree=read.tree("C:/Users/SunDevil/Desktop/ItRaSA_Accra_2019/Data/MyTree.tre")
plot.phylo(MyTree)

#head()

#Exercise 1----
ex1<-read.csv("C:/Users/SunDevil/Desktop/ItRaSA_Accra_2019/Leaftraits.txt", sep="\t")
#there  are 2548 observations in the data set----
str(ex1)

any(is.na(ex1$LMA))
View(ex1$LMA)
summary(ex1$LMA)

#There are 178 NA's----
summary(ex1$GF)
str(ex1$GF)
#there are 7 types of GF----
subGF<-ex1[ex1$GF=="T",]

str(subGF)
str(ex1[ex1$GF=="T",])

#nitrogen====
subT<-ex1[ex1$Nmass>1.5,]

str(subT)
summary(subT)
#443 species have a Nmass > 1.5%
#ex1plot----

explot1<-plot(ex1$Nmass)
#plot ex1$Nmass why I do not need to specific x and y

#explot2----
explot2<-plot(ex1$Nmass,col=ifelse(ex1$Nmass>1.5,"red","blue"))

explot3<-plot(ex1$Nmass,col=ifelse(ex1$GF=="T","red","green"))
#a1<-aggregate(subGF,)

#Ex3----
wb<-read.csv("C:/Users/SunDevil/Desktop/ItRaSA_Accra_2019/Data/wingbowl.txt",sep="\t")
t1<-table(wb$Brood)
t1
str(t1)
df3<-as.data.frame(t1)

colnames(df3)[1]="Brood_ID"

#wing_length====
summary(wb$Wing)


barplot(df3$Freq, ylim=c(0,14), ylab= "Frequency",
         xlab= "Brood_ID", main = "A bargraph showing the frequency of different Brood IDS", 
        names.arg = c("301", "302", '303',' 305',' 306',' 307' ,'308',' 310', '311',' 313', '314', '315', '316', '317', '318 ','319',' 320' ,'321', '322', '323',' 324', '325', '326', '328' ))
box(col=rgb(0.2,0.4,0.6))

summary(wb1)
summary(wb$Brood)
#boxplot----
boxplot(wb$Brood,names = "Brood", main="A boxplot showing the distribution of Brood lengths", col=rgb(0.9,0.7,0.5))
box(col=rgb(0.5,0.3,0.1))
str(df3$Brood_ID)

?boxplot
?barplot

str(wb$Brood)
summary(wb$Brood)
levels(wb$Brood)
length(which(wb$Brood))

length(wb$Brood)

length(wb$Brood==301)
length(wb$Brood==302)


plot(wb$Brood)
summary(wb)

str(wb)
