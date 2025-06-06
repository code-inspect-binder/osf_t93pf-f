###Open Science in Marketing
###Jan R. Landwehr (Goethe University Frankfurt) & Stefan Mayer (University of Tübingen)
###CC BY 4.0: https://osf.io/t93pf


###Simulation of 100 datasets with sample sizes between 10 and 1000 (increment=10) containing 15 random variables
datlist <- list()
for (i in 1:100){
  Cordata <- data.frame(matrix(NA,nrow = i*10, ncol = 15))
  for (j in 1:15){
    Cordata[,j] <- rnorm(i*10)
  }
  datlist[[i]] <- Cordata
}

###Correlation Matrices
corlist <- list()
for (k in 1:100){
  Cordata <- as.data.frame(datlist[[k]])
  Cormatrix2 <- data.frame(matrix(NA,nrow = 15, ncol = 15))
  for(i in 1:14){
    for(j in (i+1):15){
      Cormatrix2[i,j] <- cor(Cordata[,i], Cordata[,j])
      Cormatrix2[j,i] <- ifelse(cor.test(Cordata[,i], Cordata[,j])$p.value<=0.05, 555, NA)
    }
  }
  corlist[[k]] <- Cormatrix2
}

###Frequency of ranom significance values
n_sig <- NA
for (i in 1:100){
  n_sig[i] <- length(which(corlist[[i]][,]==555))
}

n_sample <- c(seq(10,1000, by=10))

plot(n_sig ~ n_sample, main="# of significant correlations ~ Sample Size")
  abline(h=mean(n_sig), col="red")
mean(n_sig)
