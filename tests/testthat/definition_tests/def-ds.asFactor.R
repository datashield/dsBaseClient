source("connection_to_datasets/init_testing_datasets.R")
source("definition_tests/def-assign-stats.R")

.find.factors.locally <- function(some.local.values)
{
  levels <- list(levels(as.factor(some.local.values)))
  vector <- as.numeric(unlist(levels))
  return(vector)
  
}

.find.factors.server <- function(variable.name, variable.recoded)
{
   levels <- ds.asFactor(variable.name,variable.recoded)
   vector <- (as.numeric(unlist(levels[[1]])))
   return(vector)
}

.test.find.factor <- function(variable.name, variable.recoded,some.values, column)
{
   factor.local <- .find.factors.locally(some.values[,column])
   factor.server <- .find.factors.server(variable.name, variable.recoded)
   dist.local <- .calc.distribution.locally(factor.local)
   dist.server <- .calc.distribution.server(factor.server)
   expect_equal(dist.local[1],dist.server [1], tolerance = ds.test_env$tolerance)
   expect_equal(dist.local[2],dist.server [2], tolerance = ds.test_env$tolerance)
}

.test.uniqueness <- function(variable.name,variable.recoded)
{
   factor.server <- ds.asFactor(variable.name,variable.recoded)
   factors.vector <- unlist(factor.server[1])
   expect_true(!any(duplicated(factors.vector)))
}
