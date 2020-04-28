
.test.data.frame.creation<-function(initial.df.name,key.name,sort.descending,sort.method,df.created){
    library(dsDangerClient)
    # Create a sort data frame
    sort.key.name<-paste(initial.df.name,key.name,sep="$")
    ds.dataFrameSort(df.name = initial.df.name,
                   sort.key.name = sort.key.name,
                   sort.descending = sort.descending,
                   sort.method = sort.method,
                   newobj =df.created,
                   datasources = ds.test_env$connections)
    type <- ds.class(df.created, datasources = ds.test_env$connections)
    exists <- ds.exists(df.created, datasources = ds.test_env$connections)
    cols.name <- ds.colnames(df.created,datasources = ds.test_env$connections)
    # Initial data frame column names
    initial.df.cols.name<-ds.colnames(initial.df.name, datasources = ds.test_env$connections)
    # Testing - testthat
    expect_true(type[[1]][1]=="data.frame")
  
    for(i in 1:length(exists))
    {
      expect_true(exists[[i]])
    }
  
    for (i in 1:length(cols.name))
    {
        expect_equal(cols.name[[i]],
                     initial.df.cols.name[[i]],
                     ds.test_env$tolerance)
      }
  
  }



.test.data.frame.sorting <-function(initial.df.name,key.name,sort.descending,sort.method,df.created,local.df.list){

  # Sort local dfs
  sort.local<-list()
  for(i in 1:length(local.df.list))
    {
    if (sort.method=="alphabetic")
      {
      order.key<-order(as.character(local.df.list[[i]][,key.name]),
                       decreasing = sort.descending,
                       na.last = TRUE)
      
    }else{
      order.key<-order(as.numeric(local.df.list[[i]][,key.name]),
                       decreasing = sort.descending,
                       na.last = TRUE)
      
    }
    sort.local[[i]]<-local.df.list[[i]][order.key,]
    }
  # Sort server dfs
  sort.key.name<-paste(initial.df.name,key.name,sep="$")
  ds.dataFrameSort(df.name = initial.df.name,
                   sort.key.name = sort.key.name,
                   sort.descending = sort.descending,
                   sort.method = sort.method,
                   newobj =df.created,
                   datasources = ds.test_env$connections)
  
    # Upload server-side testing data frames in the client-side (danger function)
    server.data<-ds.DANGERdfEXTRACT(df.created,
                                    datasources = ds.test_env$connections)
    server.data<-server.data$study.specific.df
    
  #testing- testthat
  
  for ( i in 1:length(server.data)){
      expect_equal(server.data[[i]][,key.name],
                   sort.local[[i]][,key.name],
                   ds.test_env$tolerance) 
    }
  
}


# Clear the Datashield R sessions and logout
datashield.logout(connections) 



