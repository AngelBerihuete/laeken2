#' @title gl
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is generalized Lorenz curve function
#' @details Todo
#' @export
glc <- function(dataset, samp = 10){
  
select <- seq(0.1, 1, length = samp)  

dataset <- dataset[order(dataset[,"ipuc"]), ]
dataset$acum.HX040 <- cumsum(dataset$HX040)
dataset$abscisa1 <-
  dataset$acum.HX040/dataset$acum.HX040[length(dataset$acum.HX040)]
number.homes <- length(dataset$acum.HX040)
number.individuals <- dataset$acum.HX040[number.homes]

n.selected <- length(select)
select.aux <- select*number.individuals
select.aux <- floor(select.aux)

sigma <- matrix(NA, n.selected, n.selected)
vector.gamma.i <- c()
acum.pi <- c()

for(i in 1:n.selected){
  index.aux1 <- which(dataset$abscisa1<=select[i])
  pos.i <- length(index.aux1)
  gamma.i <- sum(dataset$ipuc[1:pos.i]*dataset$HX040[1:pos.i])
  gamma.i <- gamma.i/select.aux[i]
  vector.gamma.i[i] <- gamma.i
  acum.pi[i] <- dataset$abscisa1[pos.i]
  
  lambda.i <- sum((dataset$ipuc[1:pos.i]-gamma.i)^2*dataset$HX040[1:pos.i])
  lambda.i <- lambda.i/select.aux[i]
  
  for(j in i:n.selected){
    index.aux2 <- which(dataset$abscisa1<=select[j])
    pos.j <- length(index.aux2)
    
    gamma.j <- sum(dataset$ipuc[1:pos.j]*dataset$HX040[1:pos.j])
    gamma.j <- gamma.j/select.aux[j]
    
    sigma[j,i] <- sigma[i,j] <- dataset$abscisa1[pos.i]*(lambda.i+(1-dataset$abscisa1[pos.j])*(dataset$ipuc[pos.i]-gamma.i)*(dataset$ipuc[pos.j]-gamma.j)+(dataset$ipuc[pos.i]-gamma.i)*(gamma.j-gamma.i))
  }
}

Omega.gl <- sigma/number.individuals # Omega, used for dominance test
gl.curve <- acum.pi*vector.gamma.i
max.gl.curve <- gl.curve[length(gl.curve)]
lorenz <- gl.curve/max.gl.curve
results <- list(Omega = Omega.gl, lorenz.curve = lorenz, glorenz.curve = gl.curve)
return(results)
}