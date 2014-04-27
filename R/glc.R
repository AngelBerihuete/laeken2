#' @title gl
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is generalized Lorenz curve function
#' @details Todo
#' @export
glc <- function(aux.data){
  
select <- seq(0.1, 1, by = 0.1)  

aux.data <- aux.data[order(aux.data[,1]), ]
aux.data$acum.weights1 <- cumsum(aux.data$weights1)
aux.data$abscisa1 <-
  aux.data$acum.weights1/aux.data$acum.weights1[length(aux.data$acum.weights1)]
number.homes <- length(aux.data$acum.weights1)
number.individuals <- aux.data$acum.weights1[number.homes]

n.selected <- length(select)
select.aux <- select*number.individuals
select.aux <- floor(select.aux)

sigma <- matrix(NA, n.selected, n.selected)
vector.gamma.i <- c()
acum.pi <- c()

for(i in 1:n.selected){
  index.aux1 <- which(aux.data$abscisa1<=select[i])
  pos.i <- length(index.aux1)
  gamma.i <- sum(aux.data$ipuc[1:pos.i]*aux.data$weights1[1:pos.i])
  gamma.i <- gamma.i/select.aux[i]
  vector.gamma.i[i] <- gamma.i
  acum.pi[i] <- aux.data$abscisa1[pos.i]
  
  lambda.i <- sum((aux.data$ipuc[1:pos.i]-gamma.i)^2*aux.data$weights1[1:pos.i])
  lambda.i <- lambda.i/select.aux[i]
  
  for(j in i:n.selected){
    index.aux2 <- which(aux.data$abscisa1<=select[j])
    pos.j <- length(index.aux2)
    
    gamma.j <- sum(aux.data$ipuc[1:pos.j]*aux.data$weights1[1:pos.j])
    gamma.j <- gamma.j/select.aux[j]
    
    sigma[j,i] <- sigma[i,j] <- aux.data$abscisa1[pos.i]*(lambda.i+(1-aux.data$abscisa1[pos.j])*(aux.data$ipuc[pos.i]-gamma.i)*(aux.data$ipuc[pos.j]-gamma.j)+(aux.data$ipuc[pos.i]-gamma.i)*(gamma.j-gamma.i))
  }
}

Omega.gl <- sigma/number.individuals # Omega, used for dominance test
gl.curve <- acum.pi*vector.gamma.i
max.gl.curve <- gl.curve[length(gl.curve)]
lorenz <- gl.curve/max.gl.curve
results <- list(Omega = Omega.gl, lorenz.curve = lorenz, glorenz.curve = gl.curve)
return(results)
}