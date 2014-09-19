OmegaGL <- function(aux.data){

select <- seq(0.1, 1, by = 0.1)
aux.data$acum.weights1 <- cumsum(aux.data$weights1)
aux.data$acum.weights2 <- cumsum(aux.data$weights2)
aux.data$abscisa1 <- aux.data$acum.weights1/aux.data$acum.weights1[length(aux.data$acum.weights1)]
aux.data$abscisa2 <- aux.data$acum.weights2/aux.data$acum.weights2[length(aux.data$acum.weights2)]
N.hogares <- length(aux.data$acum.weights1)
N.individuos <- round(aux.data$acum.weights2[N.hogares])
n.selected <- length(select)
#select.aux <- select * N.individuos
#select.aux <- floor(select.aux)
sigma <- matrix(NA, n.selected, n.selected)
vector.gamma.i <- c()
acum.pi <- c()

for (i in 1:n.selected) {
  index.aux1 <- which(aux.data$abscisa2 <= select[i])
  pos.i <- length(index.aux1)
  gamma.i <- sum(aux.data$ipuc[1:pos.i] * aux.data$weights2[1:pos.i])
  acum.pi[i] <- aux.data$abscisa2[pos.i]
  gamma.i <- gamma.i/(acum.pi[i]*N.individuos)
  vector.gamma.i[i] <- gamma.i

  lambda.i <- sum((aux.data$ipuc[1:pos.i] - gamma.i)^2 * aux.data$weights2[1:pos.i])
  lambda.i <- lambda.i/(acum.pi[i]*N.individuos)
  
  for (j in i:n.selected) {
    index.aux2 <- which(aux.data$abscisa2 <= select[j])
    pos.j <- length(index.aux2)
    gamma.j <- sum(aux.data$ipuc[1:pos.j] * aux.data$weights2[1:pos.j])
    gamma.j <- gamma.j/(aux.data$abscisa2[pos.j]*N.individuos)
    
    sigma[j, i] <- sigma[i, j] <- aux.data$abscisa2[pos.i] * 
      (lambda.i + (1 - aux.data$abscisa2[pos.j]) * 
         (aux.data$ipuc[pos.i] - gamma.i) * 
         (aux.data$ipuc[pos.j] - gamma.j) + (aux.data$ipuc[pos.i] - gamma.i) *
         (gamma.j - gamma.i))
  }
}

Omega.gl <- sigma/N.individuos
gl.curve <- vector.gamma.i*acum.pi

return(list(Omega = Omega.gl, gl.curve = gl.curve))
}