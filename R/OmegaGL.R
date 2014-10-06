OmegaGL <- function(dataset){

select <- seq(0.1, 1, by = 0.1)
dataset$acum.HX040 <- cumsum(dataset$HX040)
dataset$acum.wHX040 <- cumsum(dataset$wHX040)
dataset$abscisa1 <- dataset$acum.HX040/dataset$acum.HX040[length(dataset$acum.HX040)]
dataset$abscisa2 <- dataset$acum.wHX040/dataset$acum.wHX040[length(dataset$acum.wHX040)]
N.homes <- length(dataset$acum.HX040)
N.individuals <- round(dataset$acum.wHX040[N.homes])
n.selected <- length(select)
#select.aux <- select * N.individuals
#select.aux <- floor(select.aux)
sigma <- matrix(NA, n.selected, n.selected)
vector.gamma.i <- c()
acum.pi <- c()

for (i in 1:n.selected) {
  index.aux1 <- which(dataset$abscisa2 <= select[i])
  pos.i <- length(index.aux1)
  gamma.i <- sum(dataset$ipuc[1:pos.i] * dataset$wHX040[1:pos.i])
  acum.pi[i] <- dataset$abscisa2[pos.i]
  gamma.i <- gamma.i/(acum.pi[i]*N.individuals)
  vector.gamma.i[i] <- gamma.i

  lambda.i <- sum((dataset$ipuc[1:pos.i] - gamma.i)^2 * dataset$wHX040[1:pos.i])
  lambda.i <- lambda.i/(acum.pi[i]*N.individuals)
  
  for (j in i:n.selected) {
    index.aux2 <- which(dataset$abscisa2 <= select[j])
    pos.j <- length(index.aux2)
    gamma.j <- sum(dataset$ipuc[1:pos.j] * dataset$wHX040[1:pos.j])
    gamma.j <- gamma.j/(dataset$abscisa2[pos.j]*N.individuals)
    
    sigma[j, i] <- sigma[i, j] <- dataset$abscisa2[pos.i] * 
      (lambda.i + (1 - dataset$abscisa2[pos.j]) * 
         (dataset$ipuc[pos.i] - gamma.i) * 
         (dataset$ipuc[pos.j] - gamma.j) + (dataset$ipuc[pos.i] - gamma.i) *
         (gamma.j - gamma.i))
  }
}

Omega.gl <- sigma/N.individuals
gl.curve <- vector.gamma.i*acum.pi

return(list(Omega = Omega.gl, gl.curve = gl.curve))
}