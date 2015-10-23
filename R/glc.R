#' @title lc
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is generalized Lorenz curve function
#' @details Todo
#' @export
lc <- function(dataset, samp = 10, generalized = FALSE){
  
  res.glc <- OmegaGL(dataset, samp = samp)
  
  if(generalized == FALSE){
    results <- data.frame(x.lg = c(0, res.glc$p_i),
                          y.lg = c(0, res.glc$gl.curve)/miuc(dataset))
    return(results)  
  }else{
    results <- data.frame(x.lg = c(0, res.glc$p_i),
                          y.lg = c(0, res.glc$gl.curve))
    return(results)
  }
}