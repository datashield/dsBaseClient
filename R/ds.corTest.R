#'
#' @title Tests for correlation between paired samples in the server-side
#' @description This is similar to the R stats function \code{cor.test}.
#' @details Runs a two-sided correlation test between paired samples, using one of
#' Pearson's product moment correlation coefficient, Kendall's tau or Spearman's rho.
#' Server function called: \code{corTestDS}
#' @param x a character string providing the name of a numerical vector. 
#' @param y a character string providing the name of a numerical vector.
#' @param method a character string indicating which correlation coefficient is to be
#' used for the test. One of "pearson", "kendall", or "spearman", can be abbreviated. 
#' Default is set to "pearson".
#' @param exact a logical indicating whether an exact p-value should be computed. Used for
#' Kendall's tau and Spearman's rho. See \emph{Details} of R stats function \code{cor.test} for
#' the meaning of NULL (the default).
#' @param conf.level confidence level for the returned confidence interval. Currently
#' only used for the Pearson product moment correlation coefficient if there are at least
#' 4 complete pairs of observations. Default is set to 0.95.
#' @param type a character string that represents the type of analysis to carry out. 
#' This must be set to \code{'split'} or \code{'combine'}. Default is set to \code{'split'}. If 
#' \code{type} is set to "combine" then an approximated pooled correlation is estimated based on 
#' Fisher's z transformation.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.corTest} returns to the client-side the results of the correlation test. 
#' @author DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#' 
#'  ## Version 6, for version 5 see the Wiki
#'   
#'   # connecting to the Opal servers
#' 
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   # test for correlation
#'   ds.corTest(x = "D$LAB_TSC",
#'              y = "D$LAB_HDL",
#'              datasources = connections[1]) #Only first server is used ("study1")
#'                 
#'   # Clear the Datashield R sessions and logout                 
#'   datashield.logout(connections) 
#'   
#' }   
#'
ds.corTest <- function(x=NULL, y=NULL, method="pearson", exact=NULL, conf.level=0.95, type='split', datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("x=NULL. Please provide the names of the 1st numeric vector!", call.=FALSE)
  }
  if(is.null(y)){
    stop("y=NULL. Please provide the names of the 2nd numeric vector!", call.=FALSE)
  }

  if(!(method %in% c("pearson", "kendall", "spearman"))){
    stop('Function argument "method" has to be either "pearson", "kendall" or "spearman"', call.=FALSE)
  }
  
  # check if the input objects are defined in all the studies
  isDefined(datasources, x)
  isDefined(datasources, y)

  # call the internal function that checks the input objects are of the same class in all studies.
  typ <- checkClass(datasources, x)
  typ <- checkClass(datasources, y)

  # call the server side function
  cally <- call("corTestDS", x, y, method, exact, conf.level)
  out <- DSI::datashield.aggregate(datasources, cally)

  if(type=="split"){
    return(out)
  }else{
    if(type=="combine"){
      ni <- c()
      Zi <- c()
      varZi <- c()
      for(i in 1:length(datasources)){
        ni[i] <- out[[i]][[1]] # sample size
        ri <- out[[i]][[2]]$estimate # the estimated measure of association
        Zi[i] <- 0.5*log((1+ri)/(1-ri)) # Fishers Z transformation
        varZi[i] <- 1/(ni[i]-3) # variance of the correlation
      }
      # pooled correlation and variance
      Zpooled <- 0
      varZpooled <- 0
      for(i in 1:length(datasources)){
        Zpooled <- Zpooled + (ni[i]-3)*Zi[i]
        varZpooled <- varZpooled + (ni[i]-3)*varZi[i]
      }
      Zpooled <- Zpooled/(sum(ni)-3*length(datasources))
      varZpooled <- varZpooled/(sum(ni)-3*length(datasources))
      pval <- 2*(1-stats::pnorm(Zpooled/sqrt(varZpooled)))
      corr <- tanh(Zpooled)
      if(method=="pearson"){
        zlcl = Zpooled - stats::qnorm(1-(1-conf.level)/2)*sqrt(varZpooled)
        zucl = Zpooled + stats::qnorm(1-(1-conf.level)/2)*sqrt(varZpooled)
        lcl= tanh(zlcl) # lower confidence level
        ucl= tanh(zucl) # upper confidence level
        out <- list(corr, c(lcl, ucl), pval)
        names(out) <- c(paste0(method, " correlation estimate"), 
                      paste0(conf.level, " percent confidence interval"),
                      "p-value")
        return(out)
      }else{
        out <- list(corr, pval)
        names(out) <- c(paste0(method, " correlation estimate"),
                        "p-value")
        return(out)
      }  
    }else{
      stop('Function argument "type" has to be either "combine" or "split"', call.=FALSE)
    }
  }

}
