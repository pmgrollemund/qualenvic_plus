n <- nrow(data)
p <- ncol(data)

for(i in 1:n){
 data$CODE_EA_QUALENVIC[i] <- paste(
  sample(c("HPE","MHG"),1),
  "-",
  paste(sample(LETTERS,1),collapse=""),
  paste(sample(0:9,2),collapse=""),
  sep=""
 )
}


data$GEN_CLASSE_ANOVA <- 
 sapply(strsplit(data$CODE_EA_QUALENVIC,split="-"),function(v) v[1])

data$GEN_DATE_DIAG <- sample(c("14/12/2018","01/07/2019"),n,replace=T)

data$GEN_AB <- sample(data$GEN_AB,replace=T)
data$GEN_IRR <- sample(data$GEN_IRR,replace=T)
data$GEN_BL <- sample(data$GEN_BL,replace=T) 

for(j in 7:p){
 data[,j] <- runif(n,min(data[,j],na.rm = T),max(data[,j],na.rm = T)) 
 data[,j] <- jitter(data[,j])
 data[,j] <- round(data[,j],2)
}

