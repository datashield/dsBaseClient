#' 
#' @title Generates a density grid with or without a priori defined limits
#' @description This function generates a grid density object which can then be used to produced 
#' a heatmap or contourplots. In cells with a count > 0 and < 5 are considered invalid and the count 
#' is set to 0. The function prints the number of invalid cells in for participating study.
#' @details In DataSHIELD the user does not have access to the micro-data so and extreme values such as
#' the maximum and the minimum are potentially disclosive so this function does not allow for the user 
#' to set the limits of the density grid and the minimum and maximum values of the x and y vectors. These
#' elements are set by the server side function \code{densitygrid.ds} to 'valid' values (i.e. values that
#' do not lead to leakage of micro-data to the user).
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect a numerical vector
#' @param yvect a numerical vector
#' @param numints a number of intervals for the grid density object, by default is 20
#' @param type a character which represent the type of graph to display. 
#' If \code{type} is set to 'combine', a histogram that merges the single 
#' plot is displayed. Each histogram is plotted separately if If \code{type} 
#' is set to 'split'.
#' @return a global grid density matrix across all studies or a one grid density matrix for each study
#' @author Isaeva, J.; Gaye, A.
#' @export
#' @examples {
#' # load the file that contains the login details
#' data(logindata)
#' 
#' # login and assign the required variables to R
#' myvar <- list("LAB_TSC","LAB_HDL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example1: generate a combined grid density object
#' ds.densitygrid(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="combine")
#' 
#' # Example2: generate a grid density object for each study separately
#' ds.densitygrid(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="split")
#' 
#' # Example3: generate a grid density object where the number of intervals is set to 15, for each study separately
#' ds.densitygrid(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="split", numints=15)
#' }
#' 
ds.densitygrid <- function(datasources=NULL, xvect=NULL, yvect=NULL, numints=20, type="combine"){
  
  if(is.null(datasources)){
    message(" ALERT!")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).")
    stop(" End of process!", call.=FALSE)
  }
  
  if(is.null(xvect)){
    message(" ALERT!")
    message(" Please provide a valid numeric vector for 'xvect'")
    stop(" End of process!", call.=FALSE)
  }
  
  if(is.null(yvect)){
    message(" ALERT!")
    message(" Please provide a valid numeric vector for 'yvec'")
    stop(" End of process!", call.=FALSE)
  }
  
  # call the function that checks the variable is available and not empty
  vars2check <- list(xvect,yvect)
  datasources <- ds.checkvar(datasources, vars2check)
  
  # name of the studies to be used in the plots' titles
  stdnames <- names(datasources)
  
  # number of studies
  num.sources <- length(datasources)
  
  if(type=="combine"){
    # get the range from each study and produce the 'global' range
    cally <- call("rangeDS", xvect) 
    x.ranges <- datashield.aggregate(datasources, cally)
    
    cally <- call("rangeDS", yvect) 
    y.ranges <- datashield.aggregate(datasources, cally)
    
    x.minrs <- c()
    x.maxrs <- c()
    y.minrs <- c()
    y.maxrs <- c()
    for(i in 1:num.sources){
      x.minrs <- append(x.minrs, x.ranges[[i]][1])
      x.maxrs <- append(x.maxrs, x.ranges[[i]][2])
      y.minrs <- append(y.minrs, y.ranges[[i]][1])
      y.maxrs <- append(y.maxrs, y.ranges[[i]][2])
    }
    x.range.arg <- c(min(x.minrs), max(x.maxrs))
    y.range.arg <- c(min(y.minrs), max(y.maxrs))
    
    x.global.min = x.range.arg[1]
    x.global.max = x.range.arg[2]
    y.global.min = y.range.arg[1]
    y.global.max = y.range.arg[2]
    
    # generate the grid density object to plot
    cally <- call("densitygrid.ds", xvect, yvect, limits=T, x.global.min, x.global.max, y.global.min, y.global.max, numints) 
    grid.density.obj <- datashield.aggregate(datasources, cally)
    numcol <- dim(grid.density.obj[[1]])[2]
    
    # print the number of invalid cells in each participating study
    for (i in 1:num.sources) {
      message(stdnames[i],': ', names(dimnames(grid.density.obj[[i]])[2]))
    }
    
    Global.grid.density = matrix(0, dim(grid.density.obj[[1]])[1], numcol-2)
    for (i in 1:num.sources){
      Global.grid.density = Global.grid.density + grid.density.obj[[i]][,1:(numcol-2)]
      names(dimnames(Global.grid.density))[2] <- "Grid Density Matrix of the Pooled Data"
    }
    # newline for some space between the previous messages and the matrix when it is displayed
    message()
    return(Global.grid.density)
  }else{
    if(type=="split"){
      # generate the grid density object
      num_intervals=numints
      cally <- call("densitygrid.ds", xvect, yvect, limits=FALSE, x.min=NULL, x.max=NULL, y.min=NULL, y.max=NULL, numints=num_intervals) 
      grid.density.obj <- datashield.aggregate(datasources, cally)
      numcol <- dim(grid.density.obj[[1]])[2]
      
      # print the number of invalid cells in each participating study
      for (i in 1:num.sources) {
        message(stdnames[i],': ', names(dimnames(grid.density.obj[[i]])[2]))
      }
      return(grid.density.obj)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }

}