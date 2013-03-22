#' Summary.
#'
#' @param opals Opal object or list of opal objects.
#' @param expr Expression or name of a R symbol
#' @export
datashield.summary <- function(opals, expr) {
  symbol <- expr
  if(is.character(expr)) {
    symbol <- as.symbol(expr)
  }
  return(datashield.aggregate(opals, as.call(c(quote(summary), symbol))))
}

#' Length.
#'
#' @param opals Opal object or list of opal objects.
#' @param expr Expression or name of a R symbol
#' @export
datashield.length <- function(opals, expr) {
  symbol <- expr
  if(is.character(expr)) {
    symbol <- as.symbol(expr)
  }
  return(datashield.aggregate(opals, as.call(c(quote(length), symbol))))
}

#' Generalize linear model.
#'
#' @export
#' 
datashield.glm <- function(opals, formula, family, maxit=10) {
  numstudies<-length(opals)
  
  beta.vect.next<-NULL
  
  #Iterations need to be counted. Start off with the count at 0
  #and increment by 1 at each new iteration
  iteration.count<-0
  
  #Provide arbitrary starting value for deviance to enable subsequent calculation of the
  #change in deviance between iterations
  dev.old<-9.99e+99
  
  #Convergence state needs to be monitored.
  converge.state<-FALSE
  
  #Define a convergence criterion. This value of epsilon corresponds to that used
  #by default for GLMs in R (see section S3 for details)
  epsilon<-1.0e-08
  
  f<-NULL
  
  while(!converge.state && iteration.count < maxit) {
    
    iteration.count<-iteration.count+1
    
    cat("--------------------------------------------\n")
    cat("Iteration", iteration.count, "\n")
    
    call<-as.call(list(quote(glm.ds), formula, family, as.vector(beta.vect.next)));
    
    study.summary<-datashield.aggregate(opals, call);
    
    info.matrix.total<-Reduce(f="+", .select(study.summary, 'info.matrix'))
    score.vect.total<-Reduce(f="+", .select(study.summary, 'score.vect'))
    dev.total<-Reduce(f="+", .select(study.summary, 'dev'))
    
    if(iteration.count==1) {
      # Sum participants only during first iteration.
      nsubs.total<-Reduce(f="+", .select(study.summary, 'numsubs'))
      # Save family
      f <- study.summary[[1]]$family
    }
    
    #Create variance covariance matrix as inverse of information matrix
    variance.covariance.matrix.total<-solve(info.matrix.total)
    
    #Create beta vector update terms
    beta.update.vect<-variance.covariance.matrix.total %*% score.vect.total
    
    #Add update terms to current beta vector to obtain new beta vector for next iteration
    if(is.null(beta.vect.next)) {
      beta.vect.next<-beta.update.vect
    } else {
      beta.vect.next<-beta.vect.next+beta.update.vect
    }
    
    #Calculate value of convergence statistic and test whether meets convergence criterion
    converge.value<-abs(dev.total-dev.old)/(abs(dev.total)+0.1)
    if(converge.value<=epsilon)converge.state<-TRUE
    if(converge.value>epsilon)dev.old<-dev.total
    
    #For ALL iterations summarise model state after current iteration
    cat("\nSUMMARY OF MODEL STATE after iteration No",iteration.count,
        "\n\nCurrent deviance",dev.total,"on",
        (nsubs.total-length(beta.vect.next)), "degrees of freedom",
        "\nConvergence criterion    ",converge.state," (", converge.value,")\n\n")
    
    cat("beta\n")
    print(as.vector(beta.vect.next))
    
    cat("Information matrix overall\n")
    print(info.matrix.total)
    
    cat("Score vector overall\n")
    print(score.vect.total)
    
    cat("Current Deviance\n")
    print(dev.total)
    cat("--------------------------------------------\n")    
  }
  
  #If convergence has been obtained, declare final (maximum likelihood) beta vector,
  #and calculate the corresponding standard errors, z scores and p values
  #(the latter two to be consistent with the output of a standard GLM analysis)
  #Then print out final model summary
  if(converge.state)
  {
    beta.vect.final<-beta.vect.next
    
    scale.par <- 1
    if(f$family== 'gaussian') {
      scale.par <- dev.total / (nsubs.total-length(beta.vect.next))
    }
    
    se.vect.final <- sqrt(diag(variance.covariance.matrix.total)) * sqrt(scale.par)
    
    z.vect.final<-beta.vect.final/se.vect.final
    pval.vect.final<-2*pnorm(-abs(z.vect.final))
    parameter.names<-names(score.vect.total[,1])
    model.parameters<-cbind(beta.vect.final,se.vect.final,z.vect.final,pval.vect.final)
    dimnames(model.parameters)<-list(parameter.names,c("Estimate","Std. Error","z-value","p-value"))
    
    glmds <- list(
      formula=formula,
      coefficients=model.parameters,
      dev=dev.total,
      nsubs=nsubs.total,
      df=(nsubs.total-length(beta.vect.next)),
      iter=iteration.count
    )
    
    class(glmds) <- 'glmds'
    
    glmds   
  } else {
    warning(paste("Did not converge after", maxit, "iterations. Increase maxit parameter as necessary."))
    NULL
  }
}

#' Datashield Histogram.
#'
#' @export
#' 
datashield.histogram <- function(opals, a) {
  
  # call the helper function and use its output
  cally <- call("histogram.1", a) 
  output.object <- datashield.aggregate(opals, cally)
  
  
  numsources<-length(output.object)
  
  if(numsources==1)
  {
    numr<-1
    numc<-1
  }
  
  if(numsources==2)
  {
    numr<-1
    numc<-2
  }
  
  if(numsources==3 | numsources==4)
  {
    numr<-2
    numc<-2
  }
  
  if(numsources==5 | numsources==6)
  {
    numr<-2
    numc<-3
  }
  
  if(numsources>6)
  {
    numr<-4
    numc<-4
  }
  
  par(mfrow=c(numr,numc))
  
  for(j in 1:numsources)
  {
    plot(output.object[[j]],main=c("Histogram","Study",j),xlab="Variable")
  }
}