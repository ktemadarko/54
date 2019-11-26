# INFO ####
# @author: Eva Maria Malecore @date: October 2017
# @project: An Introduction to R and Statistical Analyses
# @script: pro Exercise 1, Introduction to R: more advanced R
# @filename: AItRaSA_proExercise1.R
# @update: November 2019

# Chunk 1
vec1=numeric()
for(i in seq_len(100000-1)) {
  calculation1=sqrt(i + 7*(i+1)/5)
  vec1=c(vec1, calculation1)
}


# Chunk 2
iter=100000
vec1=numeric(length=iter)
for(i in seq_len(iter-1)) {
  calculation1=sqrt(i + 7*(i+1)/5)
  vec1[i]=calculation1
}

