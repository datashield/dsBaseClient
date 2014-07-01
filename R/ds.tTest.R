#' 
#' @title Runs a student's t-test on horizontally partitioned data
#' @description Performs one and two sample t-tests on vectors of data.
#' @details Summary statistics are obtained from each of the data sets that are located on the 
#' distinct computers/servers. And then grand means and variances are calculated. Those are used 
#' for performing t-test.
#' @param x a character, the name of a (non-empty) numeric vector of data values.
#' @param y a character, the name of an optional (non-empty) numeric vector of data values.
#' @param type a character which tells if the test is ran for the pooled data or not. 
#' By default \code{type} is set to 'combine' and a t.test of the pooled data is 
#' carried out. If \code{type} is set to 'split', a t.test is ran for each study separately.
#' @param alternative  a character specifying the alternative hypothesis, must be one of 
#' "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param mu a number indicating the true value of the mean (or difference in means if you are 
#' performing a two sample test).
#' @param paired a logical indicating whether you want a paired t-test.
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal. 
#' If TRUE then the pooled variance is used to estimate the variance otherwise the Welch.
#' (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param conf.level confidence level of the interval.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return a list containing the following elements:
#' \code{statistic} the value of the t-statistic 
#' \code{parameter} the degrees of freedom for the t-statistic 
#' \code{p.value} p.value the p-value for the test 
#' \code{conf.int} a confidence interval for the mean appropriate to the specified alternative hypothesis 
#' \code{estimate} the estimated mean or difference in means depending on whether it was a one-sample test or a two-sample test 
#' \code{null.value} the specified hypothesized value of the mean or mean difference depending on whether it was a one-sample test or a two-sample test 
#' \code{alternative} a character string describing the alternative hypothesis 
#' \code{method} a character string indicating what type of t-test was performed 
#' @author Isaeva, J.; Gaye, A.
#' @export
#' @examples
#' {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("LAB_HDL", "LAB_TSC")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: Run a t.test of the pooled data for the variables 'LAB_HDL' and 'LAB_TSC' - default
#' ds.tTest(x='D$LAB_HDL', y='D$LAB_TSC')
#' 
#' # Example 2: Run a t.test for each study separately for the same variables as above
#' ds.tTest(x='D$LAB_HDL', y='D$LAB_TSC', type='split')
#' 
#' # Example 3: Run a paired t.test of the pooled data
#' ds.tTest(x='D$LAB_HDL', y='D$LAB_TSC', paired=TRUE)
#' 
#' # Example 4: Run a paired t.test for each study separately
#' ds.tTest(x='D$LAB_HDL', y='D$LAB_TSC', paired=TRUE, type='split')
#' 
#' # Example 5: Run a t.test of the pooled data with different alternatives
#' ds.tTest(x='D$LAB_HDL', y='D$LAB_TSC', alternative='greater')
#' ds.tTest(x='D$LAB_HDL', y='D$LAB_TSC', alternative='less')
#' 
#' # Example 6: Run a t.test of the pooled data with mu different from zero
#' ds.tTest(x='D$LAB_HDL', y='D$LAB_TSC', mu=-4)
#' 
#' # Example 7: Run a t.test of the pooled data assuming that variances of variables are equal
#' ds.tTest(x='D$LAB_HDL', y='D$LAB_TSC', var.equal=TRUE)
#' 
#' # Example 8: Run a t.test of the pooled data with 90% confidence interval
#' ds.tTest(x='D$LAB_HDL', y='D$LAB_TSC', conf.level=0.90)
#' 
#' # Example 9: Run a one-sample t.test of the pooled data
#' ds.tTest(x='D$LAB_HDL')
#' # the below example should not work, paired t.test is not possible if the 'y' variable is missing
#' # ds.tTest(x='D$LAB_HDL', paired=TRUE) 
#' 
#' # clear the Datashield R sessions and logout
#' datashield.logout(opals)
#' 
#'}
#'
ds.tTest <- function (x=NULL, y=NULL, type="combine", alternative="two.sided", mu=0, paired=FALSE, var.equal=FALSE, conf.level=0.95, datasources=NULL) {
  
  # if no opal login details were provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    findLogin <- getOpals()
    if(findLogin$flag == 1){
      datasources <- findLogin$opals
    }else{
      if(findLogin$flag == 0){
        stop(" Are yout logged in to any server? Please provide a valid opal login object! ", call.=FALSE)
      }else{
        message(paste0("More than one list of opal login object were found: '", paste(findLogin$opals,collapse="', '"), "'!"))
        userInput <- readline("Please enter the name of the login object you want to use: ")
        datasources <- eval(parse(text=userInput))
        if(class(datasources[[1]]) != 'opal'){
          stop("End of process: you failed to enter a valid login object", call.=FALSE)
        }
      }
    }
  }
  
  if(is.null(x)){
    stop("Please provide the name of the x vector!", call.=FALSE)
  }
    
  # get the names of the variables used for the analysis
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  if(is.null(y)){
    xname <- extract(x)
    dname = xname$elements
    variables <- dname
  }else{
    xname <- extract(x)
    yname <- extract(y)
    dname1 = xname$elements
    dname2 = yname$elements
    variables <- c(dname1, dname2)
    dname = paste(dname1, 'and', dname2)
  }
  
  # call the function that checks theinput variables are defined in all the studies
  if(is.null(y)){
    obj2lookfor <- xname$holders
    if(is.na(obj2lookfor)){
      defined <- isDefined(datasources, variables[1])
    }else{
      defined <- isDefined(datasources, obj2lookfor)
    }
  }else{
    obj2lookfor1 <- xname$holders
    obj2lookfor2 <- yname$holders
    if(is.na(obj2lookfor1)){
      defined <- isDefined(datasources, variables[1])
    }else{
      defined <- isDefined(datasources, obj2lookfor1)
    }
    if(is.na(obj2lookfor2)){
      defined <- isDefined(datasources, variables[2])
    }else{
      defined <- isDefined(datasources, obj2lookfor2)
    }
  }
  
  # call the internal function that checks an input object is of the same class in all studies.
  if(is.null(y)){
    typ1 <- checkClass(datasources, x)
  }else{
    typ1 <- checkClass(datasources, x)
    typ2 <- checkClass(datasources, y)
  }
  
  # number of studies
  num.sources = length(datasources)
  
  if(type == "combine"){
    
    # Performs t-test on merged data sets
    if (!missing(mu) && (length(mu) != 1 || is.na(mu))) 
      stop("'mu' must be a single number")
    if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) || 
                                   conf.level < 0 || conf.level > 1)) 
      stop("'conf.level' must be a single number between 0 and 1")
    if (!is.null(y)) {
      if (paired) {
        cally = paste0("complete.cases(",  x, ",", y, ")")
        datashield.assign(datasources, 'pair.compl.obs', as.symbol(cally))
        cally = paste0("subsetDS('",  x, "', pair.compl.obs)")
        datashield.assign(datasources, 'xok', as.symbol(cally))
        cally = paste0("subsetDS('",  y, "', pair.compl.obs)")
        datashield.assign(datasources, 'yok', as.symbol(cally))
      } else {
        cally = paste0("complete.cases(",  x, ")")
        datashield.assign(datasources, 'not.na.x', as.symbol(cally))
        cally = paste0("subsetDS('",  x, "',not.na.x)")
        datashield.assign(datasources, 'xok', as.symbol(cally))

        cally = paste0("complete.cases(",  y, ")")
        datashield.assign(datasources, 'not.na.y', as.symbol(cally))
        cally = paste0("subsetDS('",  y, "',not.na.y)")
        datashield.assign(datasources, 'yok', as.symbol(cally))
      }
    } else {
      if (paired) 
      stop("'y' is missing for paired test")
      cally = paste0("complete.cases(",  x, ")")
      datashield.assign(datasources, 'not.na.x', as.symbol(cally))
      cally = paste0("subsetDS('",  x, "', not.na.x)")
      datashield.assign(datasources, 'xok', as.symbol(cally))
      cally = paste0("as.null(",  x, ")")
      datashield.assign(datasources, 'yok', as.symbol(cally)) # does not matter that as.null(x) since we just want to make y to be NULL
    }
    
    
    if (paired) {
      cally = paste0("(yok)","*(",-1,")")
      datashield.assign(datasources, 'minus_y', as.symbol(cally))
      # datashield.assign(datasources, 'dummy', quote(cbind(xok, minus_y)))
      datashield.assign(datasources, 'xok', as.symbol("xok+minus_y"))
      datashield.assign(datasources, 'yok', as.symbol("as.null(yok)"))
    }
    
    length.local.x = datashield.aggregate(datasources, as.symbol("NROW(xok)"))
    mean.local.x = datashield.aggregate(datasources, as.symbol("meanDS(xok)"))
    var.local.x = datashield.aggregate(datasources, as.symbol("varDS(xok)"))
    
    length.total.x = 0
    sum.weighted.x = 0
    
    for (i in 1:num.sources) 
      if (!is.null(length.local.x[[i]]))
        length.total.x = length.total.x + length.local.x[[i]]
    
    for (i in 1:num.sources) 
      if (!is.null(length.local.x[[i]]))
        sum.weighted.x = sum.weighted.x + length.local.x[[i]]*mean.local.x[[i]]
    
    if (!is.na(sum.weighted.x))
      mean.global.x = sum.weighted.x/length.total.x else
        stop(paste("Check the data supplied: global ", variables[1], " mean is NA", sep=""))
    estimate = mean.global.x
    
    nrows_var.x = NROW(var.local.x[[1]])
    ncols_var.x = NCOL(var.local.x[[1]])
    dummy.sum.x = matrix(0, nrows_var.x, ncols_var.x)
    
    for (i in 1:num.sources) {
      if (!is.null(var.local.x[[i]]) & !is.null(mean.local.x[[i]]))
        if (!is.na(var.local.x[[i]]) & !is.na(mean.local.x[[i]])) {
          var.weight.x = (length.local.x[[i]]-1)*var.local.x[[i]]
          add.elem.x = length.local.x[[i]]*(mean.local.x[[i]]%x%t(mean.local.x[[i]]))
          dummy.sum.x = dummy.sum.x +var.weight.x+add.elem.x
        }      
    }
    mean.global.products.x = length.total.x*(mean.global.x%x%t(mean.global.x))
    var.global.x = 1/(length.total.x-1)*(dummy.sum.x-mean.global.products.x)
    
    null.y = datashield.aggregate(datasources, as.symbol("is.null(yok)"))
    null.y = unlist(null.y)
    
    if (all(null.y)) {
      if (length.total.x < 2) 
        stop("not enough 'x' observations")
      df <- length.total.x - 1
      stderr <- sqrt(var.global.x/length.total.x)
      if (stderr < 10 * .Machine$double.eps * abs(mean.global.x)) 
        stop("data are essentially constant")
      tstat <- (mean.global.x - mu)/stderr
      method <- ifelse(paired, "Paired t-test", "One Sample t-test")
      names(estimate) <- ifelse(paired, "mean of the differences", paste("mean of", variables[1], sep=""))
    } else {
      length.local.y = datashield.aggregate(datasources, as.symbol("NROW(yok)"))
      
      length.total.y = 0
      sum.weighted.y = 0
      
      for (i in 1:num.sources) 
        if (!is.null(length.local.y[[i]]))
          length.total.y = length.total.y + length.local.y[[i]]
      
      if (length.total.x < 1 || (!var.equal && length.total.x < 2)) 
        stop(paste("not enough ", variables[1], "observations", sep=""))
      if (length.total.y < 1 || (!var.equal && length.total.y < 2)) 
        stop(paste("not enough ", variables[2], "observations", sep=""))
      if (var.equal && length.total.x + length.total.y < 3) 
        stop("not enough observations")
      
      mean.local.y = datashield.aggregate(datasources, as.symbol("meanDS(yok)"))
      var.local.y = datashield.aggregate(datasources, as.symbol("varDS(yok)"))
      method <- paste(if (!var.equal) 
        "Welch", "Two Sample t-test")
      
      length.total.y = 0
      sum.weighted.y = 0
      
      for (i in 1:num.sources) 
        if (!is.null(length.local.y[[i]])) {
          length.total.y = length.total.y + length.local.y[[i]]
          sum.weighted.y = sum.weighted.y + length.local.y[[i]]*mean.local.y[[i]]
        }
      if (!is.na(sum.weighted.y))
        mean.global.y = sum.weighted.y/length.total.y else
          stop(paste("Check the data supplied: global ", variables[2], " mean is NA", sep=""))
      
      estimate <- c(mean.global.x, mean.global.y)
      names(estimate) <- c(paste("mean of ", variables[1], sep=""), paste("mean of ", variables[2], sep=""))
      
      nrows_var.y = NROW(var.local.y[[1]])
      ncols_var.y = NCOL(var.local.y[[1]])
      dummy.sum.y = matrix(0, nrows_var.y, ncols_var.y)
      
      for (i in 1:num.sources) {
        if (!is.null(var.local.y[[i]]) & !is.null(mean.local.y[[i]]))
          if (!is.na(var.local.y[[i]]) & !is.na(mean.local.y[[i]])) {
            var.weight.y = (length.local.y[[i]]-1)*var.local.y[[i]]
            add.elem.y = length.local.y[[i]]*(mean.local.y[[i]]%x%t(mean.local.y[[i]]))
            dummy.sum.y = dummy.sum.y +var.weight.y+add.elem.y
          }      
      }
      mean.global.products.y = length.total.y*(mean.global.y%x%t(mean.global.y))
      var.global.y = 1/(length.total.y-1)*(dummy.sum.y-mean.global.products.y)
      
      if (var.equal) {
        df <- length.total.x + length.total.x - 2
        v <- 0
        if (length.total.x > 1) 
          v <- v + (length.total.x - 1) * var.global.x
        if (length.total.y > 1) 
          v <- v + (length.total.y - 1) * var.global.y
        v <- v/df
        stderr <- sqrt(v * (1/length.total.x + 1/length.total.y))
      } else {
        stderrx <- sqrt(var.global.x/length.total.x)
        stderry <- sqrt(var.global.y/length.total.y)
        stderr <- sqrt(stderrx^2 + stderry^2)
        df <- stderr^4/(stderrx^4/(length.total.x - 1) + stderry^4/(length.total.y - 1))
      }
      if (stderr < 10 * .Machine$double.eps * max(abs(mean.global.x), 
                                                  abs(mean.global.y))) 
        stop("data are essentially constant")
      tstat <- (mean.global.x - mean.global.y - mu)/stderr
    }
    
    
    if (alternative == "less") {
      pval <- pt(tstat, df)
      cint <- c(-Inf, tstat + qt(conf.level, df))
    } else if (alternative == "greater") {
      pval <- pt(tstat, df, lower.tail = FALSE)
      cint <- c(tstat - qt(conf.level, df), Inf)
    } else {
      pval <- 2 * pt(-abs(tstat), df)
      alpha <- 1 - conf.level
      cint <- qt(1 - alpha/2, df)
      cint <- tstat + c(-cint, cint)
    }
    cint <- mu + cint * stderr
    names(tstat) <- "t"
    names(df) <- "df"
    names(mu) <- if (paired || !is.null(y)) 
      "difference in means" else
        "mean"
    attr(cint, "conf.level") <- conf.level
    rval <- list(statistic = tstat, parameter = df, p.value = pval, 
                 conf.int = cint, estimate = estimate, null.value = mu, 
                 alternative = alternative, method = method, data.name=dname)
    class(rval) <- "htest"
    
    # delete files that are no more required
    datashield.rm(datasources, 'pair.compl.obs')
    datashield.rm(datasources, 'xok')
    datashield.rm(datasources, 'yok')
    datashield.rm(datasources, 'not.na.x')
    datashield.rm(datasources, 'minus_y')  
    
    return(rval)
    
  }else{
    if(type == "split"){
      cally <- paste0("t.test(", x, ",", y, ",alternative='",alternative, "',mu=",mu, ",paired=",paired, ",var.equal=",var.equal, ",conf.level=",conf.level,")")
      results <- datashield.aggregate(datasources, as.symbol(cally))
      return(results)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }
}
