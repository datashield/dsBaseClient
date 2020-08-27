ds.test_env$MEAN <- 1
ds.test_env$VARIANCE <- 2
ds.test_env$SIZE <- 3
ds.test_env$MIN <- 4
ds.test_env$MAX <- 5


#This local function calculates the arithmetical mean, variance and the length of 
#some local values.  The function returns a vector of length 3; the first element 
#contains the mean, the second the variance and the third the number of elements.
.calc.distribution.locally <- function(some.local.values)
{
  value <- c(0.0,0.0,0.0)
  
  value[1] <- mean(some.local.values)
  value[2] <- var(some.local.values)
  value[3] <- length(some.local.values)
  #value[4] <- min(some.local.values)
  #value[5] <- max(some.local.values)
  return(value)
}

#This local function calculates the arithmetical mean, variance and the length of 
#some  values store on a server. The function returns a vector of length 3; the first element 
#contains the mean, the second the variance and the third the number of elements.
.calc.distribution.server <- function(name.variable)
{
  #set an initial value
  value <- c(0.0,0.0,0.0)
  
  # compute dispersion and centrality
  mean.from.servers <- ds.mean(x=name.variable,type='combine', check=TRUE,save.mean.Nvalid=FALSE)
  var.from.servers <- ds.var(x=name.variable,type='combine')
  
  # compute range
  #levels <- ds.asFactor(name.variable, newobj.name = "factors")
  #vector <- (as.numeric(unlist(levels[[1]])))
  #server.min <- min(vector)
  #server.max <- max(vector)
  
  #values to return
  value[1] <- mean.from.servers[[1]][1]
  value[2] <- var.from.servers[[1]][1]
  value[3] <- mean.from.servers[[1]][3]
  #value[4] <- min(vector)
  #value[5] <- max(vector)
  return(value)
  
}

#This local function compares two distributions in term of their measure of 
#centrality and dispersion
.compute.errors.between.distributions <- function(first.dist, second.dist, size)
{
  errors <- c(0.0,0.0)
  
  # compute statistics 
  stats.dist.1 <- .calc.distribution.server(first.dist)
  stats.dist.2 <- .calc.distribution.server(second.dist)
  
  #calculate the error between the mean and variance
  errors[1] <- (stats.dist.1[1]- stats.dist.2[1])/size
  errors[2]  <- (stats.dist.1[2] - stats.dist.2[2])/size
  
  return(errors)
}

#This local function computes the difference between each element of two vectors. Then, it sorts these
#differences in decreasing order. The latter allows to test some remainders in recodeValues, for example. 
#returns a vector of the same length as second.vector.
.calc.differences <- function(first.vector, second.vector)
{
  differences <- c()
  for (i in 1:length(second.vector))
  {
    differences[i] <- first.vector[i] - second.vector[i]
  }
  differences <- sort(differences, decreasing = TRUE)
  return(differences)
}

.mult.vectors <- function(first.vector, second.vector)
{
   return(first.vector * second.vector)
}

.add.vectors <- function(first.vector, second.vector)
{
  return(first.vector + second.vector)
}

.divide.vectors <- function(first.vector, second.vector)
{
   result <- first.vector / second.vector
   return(result[!is.na(result)])
}

.substract.vectors <- function(first.vector, second.vector)
{
  return(first.vector - second.vector)
}


