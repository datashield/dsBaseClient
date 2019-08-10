#' 
#' @title Generates a density grid with or without a priori defined limits
#' @description This function generates a grid density object which can then be used to produced 
#' heatmap or contour plots. The cells with a count > 0 and < nfilter.tab are considered invalid
#' and the count is set to 0. The function prints the number of invalid cells in for participating
#' study.
#' @details In DataSHIELD the user does not have access to the micro-data so and extreme values
#' such as the maximum and the minimum are potentially disclosive so this function does not allow
#' for the user to set the limits of the density grid and the minimum and maximum values of the x
#' and y vectors. These elements are set by the server side function \code{densityGridDS} to
#' 'valid' values (i.e. values that do not lead to leakage of micro-data to the user).
#' @param x a character the name of numerical vector
#' @param y a character the name of numerical vector
#' @param numints an integer, the number of intervals for the grid density object, by default is 20.
#' @param type a character which represent the type of graph to display. If \code{type} is set to
#' 'combine', a pooled grid density matrix is generated and one grid density matrix is generated
#' for each study if \code{type} is set to 'split'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return a grid density matrix is returned
#' @author Julia Isaeva, Amadou Gaye, Demetris Avraam for DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#' 
#'   # load the file that contains the login details
#'   data(logindata)
#' 
#'   # login and assign the required variables to R
#'   myvar <- list("LAB_TSC","LAB_HDL")
#'   opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#'   # Example1: generate a combined grid density object (the default behaviour)
#'   ds.densityGrid(x='D$LAB_TSC', y='D$LAB_HDL')
#' 
#'   # Example2: generate a grid density object for each study separately
#'   ds.densityGrid(x='D$LAB_TSC', y='D$LAB_HDL', type="split")
#' 
#'   # Example3: generate a grid density object where the number of intervals is set to 15, for 
#'               each study separately
#'   ds.densityGrid(x='D$LAB_TSC', y='D$LAB_HDL', type="split", numints=15)
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.densityGrid <- function(x=NULL, y=NULL, numints=20, type='combine', datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the numeric vector 'x'!", call.=FALSE)
  }
  
  if(is.null(y)){
    stop("Please provide the name of the numeric vector 'y'!", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$object)
  # or just as a vector not attached to a table (i.e. object)
  # we have to make sure the function deals with each case
  objects <- c(x, y)
  xnames <- extract(objects)
  varnames <- xnames$elements
  obj2lookfor <- xnames$holders
  
  # check if the input object(s) is(are) defined in all the studies
  for(i in 1:length(varnames)){
    if(is.na(obj2lookfor[i])){
      defined <- isDefined(datasources, varnames[i])
    }else{
      defined <- isDefined(datasources, obj2lookfor[i])
    }
  }
  
  # call the internal function that checks the input object(s) is(are) of the same class in all studies.
  for(i in 1:length(objects)){
    typ <- checkClass(datasources, objects[i])
  }
  
  # name of the studies to be used in the plots' titles
  stdnames <- names(datasources)
  
  # number of studies
  num.sources <- length(datasources)
  
  if(type=="combine"){
    # get the range from each study and produce the 'global' range
    cally <- paste0('rangeDS(', x, ')')
    x.ranges <- opal::datashield.aggregate(datasources, as.symbol(cally))
    
    cally <- paste0('rangeDS(', y, ')')
    y.ranges <- opal::datashield.aggregate(datasources, as.symbol(cally))
    
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
    
    x.global.min <- x.range.arg[1]
    x.global.max <- x.range.arg[2]
    y.global.min <- y.range.arg[1]
    y.global.max <- y.range.arg[2]
    
    # generate the grid density object to plot
    cally <- paste0("densityGridDS(", x, ",", y, ",", limits=T, ",", x.global.min, ",",
                    x.global.max, ",", y.global.min, ",", y.global.max, ",", numints, ")")
    grid.density.obj <- opal::datashield.aggregate(datasources, as.symbol(cally))
    numcol <- dim(grid.density.obj[[1]])[2]
    
    # print the number of invalid cells in each participating study
    for (i in 1:num.sources){
      message(stdnames[i],': ', names(dimnames(grid.density.obj[[i]])[2]))
    }
    
    Global.grid.density <- matrix(0, dim(grid.density.obj[[1]])[1], numcol-2)
    for (i in 1:num.sources){
      Global.grid.density <- Global.grid.density + grid.density.obj[[i]][,1:(numcol-2)]
      names(dimnames(Global.grid.density))[2] <- "Grid Density Matrix of the Pooled Data"
    }
    # newline for some space between the previous messages and the matrix when it is displayed
    message()
    return(Global.grid.density)
  }else{
    if(type=="split"){
      # generate the grid density object
      num_intervals <- numints
      cally <- paste0("densityGridDS(", x, ",", y, ",", 'limits=FALSE', ",", 'x.min=NULL', ",",
                      'x.max=NULL', ",", 'y.min=NULL', ",", 'y.max=NULL', ",", numints=num_intervals, ")")
      grid.density.obj <- opal::datashield.aggregate(datasources, as.symbol(cally))
      numcol <- dim(grid.density.obj[[1]])[2]
      
      # print the number of invalid cells in each participating study
      for (i in 1:num.sources){
        message(stdnames[i],': ', names(dimnames(grid.density.obj[[i]])[2]))
      }
      return(grid.density.obj)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }

}
