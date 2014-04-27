flex <- function(lorenz.curve){
  x <- seq(0.1, 1, by = 0.1)
  y <- lorenz.curve
  spl <- smooth.spline(x, y)
  pred <- predict(spl, x, deriv=1)
  res.flex <- (x*pred$y)/y
  return(res.flex)
}
