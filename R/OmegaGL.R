OmegaGL <- function(dataset, samp){

select <- (1:samp)/samp
dataset1 <- dataset[order(dataset[,'ipuc']), ]

dataset1$Acum <- cumsum(dataset1$wHX040)
dataset1$Acum.P_i <- dataset1$Acum/dataset1$Acum[length(dataset1$Acum)]

number.individuals <- dataset1$Acum[length(dataset1$Acum)]

p_i <- (1:samp)/samp
np_i <- floor(p_i*number.individuals)

sigma <- mat.or.vec(samp, samp)
vector.gamma.i <- c()

for(i in 1:samp){
  pos.i <- which(dataset1$Acum.P_i>=p_i[i])[1]
  if(pos.i == 1){
    gamma.i <- dataset1$pg[pos.i]*(dataset1$Acum[pos.i]-np_i[pos.i])
    gamma.i <- gamma.i/np_i[i]
    vector.gamma.i[i] <- gamma.i
    lambda.i <- (dataset1$pg[pos.i]-gamma.i)^2*(dataset1$Acum[pos.i]-np_i[i])
    lambda.i <- lambda.i/np_i[i]
  }else{
    gamma.i <- sum(dataset1$pg[1:(pos.i-1)]*dataset1$wHX040[1:(pos.i-1)]) +
      dataset1$pg[pos.i]*(np_i[i]-dataset1$Acum[pos.i-1])
    gamma.i <- gamma.i/np_i[i]
    vector.gamma.i[i] <- gamma.i
    lambda.i <- sum((dataset1$pg[1:(pos.i-1)]-gamma.i)^2*dataset1$wHX040[1:(pos.i-1)])+
      (dataset1$pg[pos.i]-gamma.i)^2*(np_i[i]-dataset1$Acum[pos.i-1]) 
    
    lambda.i <- lambda.i/np_i[i]
  }
  
  for(j in i:samp){
    pos.j <- which(dataset1$Acum.P_i>=p_i[j])[1]
    
    if(pos.j == 1){
      gamma.j <- dataset1$pg[pos.j]*(dataset1$Acum[pos.j]-np_i[pos.j])
      gamma.j <- gamma.j/np_i[j]
    }else{
      gamma.j <- sum(dataset1$pg[1:(pos.j-1)]*dataset1$wHX040[1:(pos.j-1)]) + 
        dataset1$pg[pos.j]*(np_i[j]-dataset1$Acum[pos.j-1])
      gamma.j <- gamma.j/np_i[j]
    }
    sigma[i,j] <- p_i[i]*(lambda.i+(1-p_i[j])*(dataset1$pg[pos.i]-gamma.i)*(dataset1$pg[pos.j]-gamma.j)+(dataset1$pg[pos.i]-gamma.i)*(gamma.j-gamma.i))
  }
}

sigma <- sigma + t(sigma)
diag(sigma) <- diag(sigma)/2

Omega.gl <- sigma/N.individuals
gl.curve <- vector.gamma.i*p_i

return(list(Omega = Omega.gl, gl.curve = gl.curve, p_i = p_i))
}