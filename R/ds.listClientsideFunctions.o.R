#' 
#' @title ds.listClientsideFunctions.o calling no server-side functions 
#' @description Lists all current client-side functions
#' @details Depending on choice of arguments can list all client-side functions
#' or any combination of: dsBaseClient, dsGraphicsClient, dsModellingClient,
#' dsStatsClient, or userDefinedClient. Operates by directly interrogating
#' the R objects stored in the input client packages and objects with name
#' "ds. ....." in .GlobalEnv
#' @param all.functions logical, if TRUE will list all client-side functions
#' in the input client-side packages and/or in .GlobalEnv. Default = TRUE
#' @param dsBaseClient logical, if TRUE will selectively list client-side functions
#' in "package:dsBaseClient". Default = FALSE.
#' @param dsGraphicsClient logical, if TRUE will selectively list client-side functions
#' in "package:dsGraphicsClient". Default = FALSE.
#' @param dsModellingClient logical, if TRUE will selectively list client-side functions
#' in "package:dsModellingClient". Default = FALSE.
#' @param dsStatsClient logical, if TRUE will selectively list client-side functions
#' in "package:dsStatsClient". Default = FALSE.
#' @param dsBetaTestClient logical, if TRUE will selectively list client-side functions
#' in "package:dsBetaTestClient". Default = FALSE.
#' @param userDefinedClient logical, if TRUE will selectively list user defined
#' client-side functions held as R objects with name "ds. ..." in .Global.env". Default = FALSE.
#' @return list containing all functions in each or all of these five classes
#' @author Paul Burton for DataSHIELD Development Team
#' @export
ds.listClientsideFunctions.o  <-function(all.functions=TRUE,
						dsBaseClient=FALSE,
					    	dsGraphicsClient=FALSE,
					    	dsModellingClient=FALSE,
					    	dsStatsClient=FALSE,
							dsBetaTestClient=FALSE,
						userDefinedClient=FALSE
						){
#THIS IS UNDERLYING SEARCH FUNCTION IN R IF NEEDED: search()	
	if(all.functions==TRUE&&dsBaseClient==FALSE&&dsGraphicsClient==FALSE&&
	dsModellingClient==FALSE&&dsStatsClient==FALSE&&dsBetaTestClient==FALSE&&
	userDefinedClient==FALSE){
	cat("\n### dsBaseClient functions \n")
	print(ls(pos="package:dsBaseClient"))
	cat("\n\n### dsGraphicsClient functions \n")
	print(ls(pos="package:dsGraphicsClient"))
	cat("\n\n### dsModellingClient functions \n")
	print(ls(pos="package:dsModellingClient"))
	cat("\n\n### dsStatsClient functions \n")
	print(ls(pos="package:dsStatsClient"))
	cat("\n\n### dsBetaTestClient functions \n")
	print(ls(pos="package:dsBetaTestClient"))
	cat("\n\n### userDefinedClient functions \n")
	print(ls(pos=".GlobalEnv",pattern="ds.*"))

	}else{
	if(dsBaseClient==TRUE){
		cat("\n### dsBaseClient functions \n")
		print(ls(pos="package:dsBaseClient"))}
	if(dsGraphicsClient==TRUE){
		cat("\n### dsBaseGraphics functions \n")
		print(ls(pos="package:dsGraphicsClient"))}
	if(dsModellingClient==TRUE){
		cat("\n### dsModellingClient functions \n")
		print(ls(pos="package:dsModellingClient"))}
	if(dsStatsClient==TRUE){
		cat("\n### dsStatsClient functions \n")
		print(ls(pos="package:dsStatsClient"))}
	if(dsBetaTestClient==TRUE){
		cat("\n### dsBetaTestClient functions \n")
		print(ls(pos="package:dsBetaTestClient"))}
	if(userDefinedClient==TRUE){
		cat("\n### userDefinedClient functions \n")
		print(ls(pos=".GlobalEnv",pattern="ds.*"))}

	if(dsBaseClient==FALSE&&dsGraphicsClient==FALSE&&dsModellingClient==FALSE&&
		dsStatsClient==FALSE&&dsStatsClient==FALSE&&dsBetaTestClient==FALSE&&userDefinedClient==FALSE){

	print("ALL OPTIONS DECLARED FALSE SO NO OUTPUT")
	}
   }
}
#ds.listClientsideFunctions.o


