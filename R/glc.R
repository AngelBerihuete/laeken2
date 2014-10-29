#' @title gl
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is generalized Lorenz curve function
#' @details Todo
#' @export
glc <- function(dataset, samp = 10){
  res.glc <- OmegaGL(dataset, samp = samp)
  results <- data.frame(x.lg = c(0, res.glc$acum.pi) , 
                        y.lg = c(0, res.glc$gl.curve))
return(results)
}