#' Synthetic dataset generated from real Austrian EU-SILC.
#' 
#' The dataset is the same as in laeken package but
#' with transformed variables in order to perform right calculations
#' using laeken2.
#' @usage data(eusilc2)
#' @format A data frame with 6000 rows of 7 variables:
#' \describe{
#'  \item{DB010}{integer; year of the pull}
#'  \item{DB020}{factor; name of the country}
#'  \item{DB040}{factor; name of the region within the country}}
#' For further details see \url{https://cran.r-project.org/web/packages/laeken/vignettes/laeken-intro.pdf}
"eusilc2"