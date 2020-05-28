source("connection_to_datasets/init_testing_datasets.R")
source("definition_tests/def-assign-stats.R")

.test.reshape <- function()
{
  init.testing.datasets()
  #local data
  local.data <- data.frame("IDENTIFIER" = ds.test_env$local.values[,16], 
                           "CATEGORY" = ds.test_env$local.values[,17],
                           "VALUES" =ds.test_env$local.values[,6])
  local.wide <- reshape(local.data,idvar="IDENTIFIER",timevar="CATEGORY",direction="wide")
 
  #server data
  #ds.dataFrame(x=c('IDENTIFIER','CATEGORY','INTEGER'),newobj = 'dataframe',datasources = ds.test_env$local.values)
  #dsReshape(data.name   , datasources = ds.test_env$local.values)
}
