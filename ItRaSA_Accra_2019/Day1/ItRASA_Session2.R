# INFO ####
# @author: Eva Maria Malecore @date: October 2017
# @project: An Introduction to R and Statistical Analyses
# @script: Session 2, Introduction to R: more advanced R
# @filename: ItRaSA_Session2.R
# @update: November 2019

# for loops ####
for (i in 1:5) {
  print(i^2)
}

x=c(1,7,23)
for (i in x) {
  print(i)
}

# nested for loops 
mat1 = matrix(nrow=10, ncol=10)
mat1
for(i in 1:dim(mat1)[1]) { 
  for(j in 1:dim(mat1)[2]) {
    mat1[i,j] = i*j  
  }
}
mat1

# if else statement ####
x=sample(3:41,1)
if (x <= 27) {
  print("x is less than 27")
} else {
  print("x is greater than 27")
}
x

# Other control flow statements ####

# while
i=-2
while (i<10) {
  print(i)
  i=i+1
}

# repeat break
repeat {
  x=round(runif(1)*1000)
  if (x %% 17 == 0) {
    #x is a multiple of 17
    break
  }
}
x/17

# next
for (i in 1:20) {
  if (i%%2 == 1) {
    next
  } else {
    print(i)
  }
}

# functions ####
my.function= function(a,b,c){
  result=a*b-c+a^2
  return(result)
}
my.function(2,3,4)


# apply family ####
mat2=matrix(nrow=10,ncol=11,data=sample(1:100,110,replace=TRUE))
mat2
apply(mat2,1,mean)

mat2=matrix(nrow=10000,ncol=10000,data=sample(1:100,10000*10000,replace=TRUE))
mean.for=NULL

system.time(
  apply(mat2,1,mean)
)

system.time(
  for (i in 1:ncol(mat2)) {
    mean.for[i]=mean(mat2[,i])
  }
)

