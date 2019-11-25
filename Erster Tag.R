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

#dataframe ====
#like matrices but different data types with headers
#headers accessed with $
df=as.data.frame(mat1)
df1=data.frame(Name=c("Joyce B", "Ebenezer S"), Yrs=c(3,2))

#class====
class(df)
str(df)
data<-read.csv("C:/Users/SunDevil/Desktop/ItRaSA_Accra_2019/Data/fields.txt", sep="\t")

#tree====
library("ape")
MyTree=read.tree("C:/Users/SunDevil/Desktop/ItRaSA_Accra_2019/Data/MyTree.tre")
plot.phylo(MyTree)

head()

