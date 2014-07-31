#' @title setupDataset
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is Z index
#' @details Todo
#' @export

setupDataset <- function(dataset,
                         s = 'OCDE',
                         country = 'ES' ,
                         region = 'all',
                         deflac = NULL,
                         ppp.rates = FALSE) {
  
  if(!is.null(country)){ # only for one region
    dataset <- subset(dataset, DB020 == country)
  }else{
    stop("The variable country is mandatory")
  }
  
  if(region != 'all'){ # only for one region
    dataset <- subset(dataset, DB040 == region)
  }
  
  remove.data <- which(is.na(dataset$HX090)) # renove NA data 
  
  if(length(remove.data) != 0){
    dataset <- dataset[-remove.data, ]
  }
  
  if(ppp.rates){ # Purchasing power parity
    year1 <- unique(dataset$DB010)
    ppp.rates <- subset(ppp.rates, year == year1)
    country1 <- country
    indx4ppp <- which(ppp.rates$country == country1)
    if(is.na(ppp.rates$ppp[indx4ppp])){
      stop(paste("Country ", country1, " has NA as ppp value", sep = ""))
    }else{
      ppp.rate <- ppp.rates$ppp[indx4ppp]/ppp.rates$rate[indx4ppp]  
    }
    dataset$HX090 <- dataset$HX090/ppp.rate
    rm(ppp.rates)
  }
  
  if(!is.null(deflac)){ # Deflaction 
    dataset$HX090 <- dataset$HX090/deflac
  }
  
  if(s == "OCDE"){
    ipuc <- dataset$HX090 # Income per unit of consumption 
  }else{
    ipuc <- (dataset$HX090*dataset$HX050)/dataset$HX040^s
  }
  
  aux.data <- data.frame(ipuc = ipuc)
  aux.data$region <- factor(as.character(dataset$DB040))
  aux.data$year <- dataset$DB010
  aux.data$weights1 <- dataset$HX040
  aux.data$HX040 <- dataset$HX040
  aux.data$DB090 <- dataset$DB090
  aux.data$weights2 <- dataset$DB090*dataset$HX040
  aux.data$HX050 <- dataset$HX050
  aux.data$HX090 <- dataset$HX090
  return(aux.data)
}