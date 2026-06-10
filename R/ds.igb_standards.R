#' 
#' @title Converts birth measurements to intergrowth z-scores/centiles
#' @description Converts birth measurements to INTERGROWTH z-scores/centiles (generic)
#' @param gagebrth the name of the "gestational age at birth in days" variable.
#' @param z z-score(s) to convert (must be between 0 and 1). Default value is 0.
#' This value is used only if \code{fun} is set to "igb_zscore2value".
#' @param p centile(s) to convert (must be between 0 and 100). Default value is p=50. 
#' This value is used only if \code{fun} is set to "igb_centile2value".
#' @param val the name of the anthropometric variable to convert.
#' @param var the name of the measurement to convert ("lencm", "wtkg", "hcircm", "wlr").
#' @param sex the name of the sex factor variable. The variable should be coded as Male/Female.
#' If it is coded differently (e.g. 0/1), then you can use the ds.recodeValues function to 
#' recode the categories to Male/Female before the use of ds.igb_standards.
#' @param fun the name of the function to be used. This can be one of: "igb_centile2value",
#' "igb_zscore2value", "igb_value2zscore" (default), "igb_value2centile".
#' @param newobj a character string that provides the name for the output variable
#' that is stored on the data servers. Default name is set to \code{igb.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified the default set of connections will be
#' used: see \code{\link[DSI]{datashield.connections_default}}.
#' @note For gestational ages between 24 and 33 weeks, the INTERGROWTH very early preterm 
#' standard is used.
#' @references 
#' \itemize{
#'  \item Villar, J., Ismail, L.C., Victora, C.G., Ohuma, E.O., Bertino, E., 
#'  Altman, D.G., Lambert, A., Papageorghiou, A.T., Carvalho, M., Jaffer, Y.A., 
#'  Gravett, M.G., Purwar, M., Frederick, I.O., Noble, A.J., Pang, R., Barros, 
#'  F.C., Chumlea, C., Bhutta, Z.A., Kennedy, S.H., 2014. International 
#'  standards for newborn weight, length, and head circumference by gestational 
#'  age and sex: the Newborn Cross-Sectional Study of the INTERGROWTH-21st 
#'  Project. The Lancet 384, 857--868. https://doi.org/10.1016/S0140-6736(14)60932-6
#'  \item Villar, J., Giuliani, F., Fenton, T.R., Ohuma, E.O., Ismail, L.C., 
#'  Kennedy, S.H., 2016. INTERGROWTH-21st very preterm size at birth reference 
#'  charts. The Lancet 387, 844--845. https://doi.org/10.1016/S0140-6736(16)00384-6
#'  
#' }
#' @return assigns the converted measurement as a new object on the server-side
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#' 
ds.igb_standards <- function(gagebrth=NULL, z=0, p=50, val=NULL, var=NULL, sex=NULL, 
                             fun='igb_value2zscore', newobj=NULL, datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  # check if user has provided the name of the input variables
  if(is.null(gagebrth)){
    stop("Please provide the name of the gagebrth variable", call.=FALSE)
  }
  if(is.null(sex)){
    stop("Please provide the name of the sex variable", call.=FALSE)
  }
  
  # check if the input objects are defined in all the studies
  isDefined(datasources, gagebrth)
  isDefined(datasources, sex)
  
  # check if the provided fun is valid
  if(!(fun %in% c("igb_centile2value","igb_zscore2value","igb_value2zscore","igb_value2centile"))){
    stop("Please provide a valid fun", call.=FALSE)
  }
  
  # check if val is provided if fun is either igb_value2zscore or igb_value2centile
  if(fun %in% c("igb_value2zscore","igb_value2centile")){
    if(is.null(val)){
      stop("Please provide the name of the val variable", call.=FALSE)
    }else{
      isDefined(datasources, val)
    }
  }
  
  # check if the provided var is valid
  if(!(var %in% c("lencm", "wtkg", "hcircm", "wlr"))){
    stop("Please provide a valid var", call.=FALSE)
  }
  
  # if no output variable specified then provide a default name
  if(is.null(newobj)){
    newobj <- "igb.newobj"
  }
  
  # call the server-side assign function
  cally <-  call('igb_standardsDS', gagebrth, z, p, val, var, sex, fun)
  DSI::datashield.assign(datasources, newobj, cally)

}
