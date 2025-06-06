###Open Science in Marketing
###Jan R. Landwehr (Goethe University Frankfurt) & Stefan Mayer (University of Tübingen)
###CC BY 4.0: https://osf.io/t93pf

###Simulation of dataset with 15 random variables with 100 observations each
Cordata <- data.frame(matrix(NA,nrow = 100, ncol = 15))
for (i in 1:15){
  Cordata[,i] <- rnorm(100)
}

###Number of tests
((15*15)-15)/2

###Matrix with correlations and p-values
Cormatrix1 <- data.frame(matrix(NA,nrow = 15, ncol = 15))
for(i in 1:14){
  for(j in (i+1):15){
    Cormatrix1[i,j] <- round(cor(Cordata[,i], Cordata[,j]), digits=2)
    Cormatrix1[j,i] <- round(cor.test(Cordata[,i], Cordata[,j])$p.value, digits=2)
  }
}
Cormatrix1

###Matrix with correlations and significance stars
Cormatrix2 <- data.frame(matrix(NA,nrow = 15, ncol = 15))
for(i in 1:14){
  for(j in (i+1):15){
    Cormatrix2[i,j] <- round(cor(Cordata[,i], Cordata[,j]), digits=2)
    Cormatrix2[j,i] <- ifelse(cor.test(Cordata[,i], Cordata[,j])$p.value<=0.05, "*", "no")
  }
}
Cormatrix2
