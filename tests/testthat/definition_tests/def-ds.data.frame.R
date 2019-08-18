
.test.data.frame.creation <- function(list.variables,data.frame.name)
{
     data.frame.server <- ds.dataFrame(x=list.variables, newobj = data.frame.name, datasources = ds.test_env$connection.opal)
     type <- ds.class(data.frame.name)
     exists <- ds.exists(data.frame.name, datasources = ds.test_env$connection.opal)
     cols.name <- ds.colnames(data.frame.name,datasources = ds.test_env$connection.opal)
    
     expect_true(type[[1]][1]=="data.frame")
     
     for(i in 1:length(exists))
     {
       expect_true(exists[[i]])
     }
    
     for (server in length(cols.name))
     {
        for(i in 1:length(list.variables))
        {
          expect_true(cols.name[[server]][i] == list.variables[i])
        }
     }
     
     
}

.test.data.frame.from.objects <- function(variable.name,variable.created,data.frame.name)
{
  list.variables <- c()
  list.variables[1] <- variable.name
  
  ds.make(variable.name,variable.created,datasources = ds.test_env$connection.opal)
 
  data.frame.server <- ds.dataFrame(x=list.variables, newobj = data.frame.name, datasources = ds.test_env$connection.opal)
  
  type <- ds.class(data.frame.name)
  exists <- ds.exists(data.frame.name, datasources = ds.test_env$connection.opal)
  
  for(i in 1:length(exists))
  {
    expect_true(exists[[i]])
  }
  expect_true(type[[1]][1]=="data.frame")
}


.test.data.frame.from.different.objects <- function(data.frame.name)
{
  ds.make('D$NUMERIC','numeric_var')
  ds.make('D$FACTOR','factor_var')
  
  data.frame.server <- ds.dataFrame(x=c('numeric_var','factor_var'), newobj = data.frame.name, datasources = ds.test_env$connection.opal)
  type <- ds.class(data.frame.name)
  exists <- ds.exists(data.frame.name, datasources = ds.test_env$connection.opal)
  
  for(i in 1:length(exists))
  {
    expect_true(exists[[i]])
  }
  expect_true(type[[1]][1]=="data.frame")
  
}
