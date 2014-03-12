\name{filterEUSILC}
\alias{filterEUSILC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
filterEUSILC(year = "2006", varD = c("DB010", "DB020", "DB030", "DB040", "DB090"), varH = c("HB010", "HB020", "HB030", "HY020", "HY025", "HX010", "HX040", "HX050", "HX080", "HX090"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{
%%     ~~Describe \code{year} here~~
}
  \item{varD}{
%%     ~~Describe \code{varD} here~~
}
  \item{varH}{
%%     ~~Describe \code{varH} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (year = "2006", varD = c("DB010", "DB020", "DB030", 
    "DB040", "DB090"), varH = c("HB010", "HB020", "HB030", "HY020", 
    "HY025", "HX010", "HX040", "HX050", "HX080", "HX090")) 
{
    main.directory <- getwd()
    setwd(dir)
    files <- dir(pattern = year)
    dataset1 <- read.table(file = files[1], header = TRUE, sep = ",")
    dataset2 <- read.table(file = files[2], header = TRUE, sep = ",")
    sub.dataset1 <- subset(dataset1, select = varDB)
    sub.dataset2 <- subset(dataset2, select = )
    check1 <- identical(sub.dataset1$DB010, sub.dataset2$HB010)
    check2 <- identical(sub.dataset1$DB030, sub.dataset2$HB030)
    if (!check1) {
        stop("Different years!")
    }
    else if (!check2) {
        stop("You do not have the same identification for homes")
    }
    else {
        dataset <- cbind(sub.dataset1, sub.dataset2)
    }
    setwd(main.directory)
    return(dataset)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
