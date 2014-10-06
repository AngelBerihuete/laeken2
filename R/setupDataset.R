#' @title setupDataset
#' @author A. Berihuete, C.D. Ramos and M.A. Sordo
#' @description This is Z index
#' @details Todo
#' @export

setupDataset <- function(dataset,
                         country = 'ES' ,
                         region = 'all',
                         s = 'OCDE',
                         deflac = NULL,
                         ppp.rates = FALSE) {
  
  # SI PPP.RATES = TRUE, entonces da problemas year1
  # ------------------------------------------------
  # Hay que incluirlo en el dataset original
  
  if(!is.null(country)){ # only for one region
    dataset <- subset(dataset, DB020 == country)
  }else{
    stop("The variable country is mandatory")
  }
  
  if(region != 'all'){ # only for one region
    dataset <- subset(dataset, DB040 == region)
  }
  
  ok.cases <- complete.cases(dataset)
  dataset <- dataset[ok.cases,]

  
  #   remove.data <- which(is.na(dataset$HX090)) # renove NA data 
#   
#   if(length(remove.data) != 0){
#     dataset <- dataset[-remove.data, ]
#   }
#   
  if(ppp.rates){ # Purchasing power parity
    year1 <- unique(dataset$DB010)
    data(ppp.rates)
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
  
  # income per unit of consumption
  if(s == "OCDE"){
    dataset$ipuc <- dataset$HX090  
  }else{
    dataset$ipuc <- (dataset$HX090*dataset$HX050)/dataset$HX040^s
  }
  
  dataset$wHX040 <- dataset$DB090*dataset$HX040

# aux.data > dataset
# weights2 > wHX040
# weights1 > HX040
# 
#   aux.data <- data.frame(ipuc = ipuc)
#   aux.data$region <- factor(as.character(dataset$DB040))
#   aux.data$year <- dataset$DB010
#   aux.data$weights1 <- dataset$HX040
#   aux.data$HX040 <- dataset$HX040
#   aux.data$DB090 <- dataset$DB090
#   
#   aux.data$HX050 <- dataset$HX050
#   aux.data$HX090 <- dataset$HX090
  return(dataset)
}

# aux.data <- data.frame(ipuc = eusilc2$eqIncome)
# aux.data$region <- eusilc2$db040
# aux.data$year <- rep(2006, length(eusilc2$db040))
# aux.data$weights1 <- eusilc2$hsize
# aux.data$HX040 <- eusilc2$hsize
# aux.data$DB090 <- eusilc2$db090
# aux.data$weights2 <- aux.data$DB090*aux.data$HX040
# aux.data$HX050 <- eusilc2$eqSS
# aux.data$HX090 <- eusilc2$eqIncome

