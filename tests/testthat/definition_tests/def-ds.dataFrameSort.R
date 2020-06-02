
.test.function.parameters<-function(initial.df.name,key.name,sort.descending,sort.method,df.created)
  {
  if(class(initial.df.name)!="character" | class(key.name)!="character" | class(sort.method)!="character" | class(sort.descending) != "logical"  | class(df.created)!="character")
    {
    expect_error(ds.dataFrameSort(df.name = initial.df.name,
                                  sort.key.name = key.name,
                                  sort.descending = sort.descending,
                                  sort.method = sort.method,
                                  newobj =df.created, 
                                  datasources = ds.test_env$connections))
  }
  
  if(class(initial.df.name)=="character" & class(key.name)=="character" & class(sort.method)=="character" & class(sort.descending) == "logical"  & class(df.created)=="character")
  {
    if(grepl("[$]",key.name)==FALSE | initial.df.name!="D"){
      expect_error(ds.dataFrameSort(df.name = initial.df.name,
                                    sort.key.name = key.name,
                                    sort.descending = sort.descending,
                                    sort.method = sort.method,
                                    newobj =df.created, 
                                    datasources =ds.test_env$connections))
    }else{
      var.exist<-substr(key.name, 3, nchar(key.name))
      for(j in 1:length(ds.test_env$connections)){
          var.in.df<-var.exist %in% ds.colnames("D", datasources = ds.test_env$connections)[[j]] 
          if(var.in.df==FALSE)
          {
            results<-ds.dataFrameSort(df.name = initial.df.name,
                                      sort.key.name = key.name,
                                      sort.descending = sort.descending,
                                      sort.method = sort.method,
                                      newobj =df.created, 
                                      datasources =test_env$connections)
            not.ok.message<-"NOT ALL OK: there are studysideMessage(s) on this datasource"
            for(j in 1:length(ds.test_env$connections))
            {
              expect_equal(results$studyside.messages[[j]],not.ok.message,ds.test_env$tolerance)
            }
          }
        
      }
    }
  }
  
}


.test.data.frame.creation<-function(initial.df.name,key.name,sort.descending,sort.method,df.created)
  {
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



.test.data.frame.sorting <-function(initial.df.name,key.name,sort.descending,sort.method,df.created,local.df.list)
  {

  # Sort local dfs
  sort.local<-list()
  sort.local.dim<-list()
  sort.local.colnames<-list()
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
    sort.local.dim[[i]]<-dim(local.df.list[[i]])
    }
  # Sort server dfs
  sort.key.name<-paste(initial.df.name,key.name,sep="$")
  ds.dataFrameSort(df.name = initial.df.name,
                   sort.key.name = sort.key.name,
                   sort.descending = sort.descending,
                   sort.method = sort.method,
                   newobj =df.created,
                   datasources = ds.test_env$connections)
  sort.server.dim<-ds.dim(df.created,
                          datasources = ds.test_env$connections)
  sort.key.name.class<-ds.class(sort.key.name, datasources = ds.test_env$connections)[[1]]
  
  for (i in 1:length(ds.test_env$connections))
  {
    expect_equal(sort.local.dim[[i]],
                 sort.server.dim[[i]],
                 ds.test_env$tolerance)
  }
  
  
  
  if(sort.key.name.class == "integer" | sort.key.name.class == "numeric" & sort.method == "numeric" )
    {
     sorted.key.name<-list()
     sort.server.mean<-list()
     sort.server.var<-list()
     len.key.server<-sort.server.dim[[1]][1]
     for(l in 1:3)
       {
     i<-c(1,(len.key.server/2)-2.5,len.key.server-5)
     j<-c(5,(len.key.server/2)+2.5,len.key.server)
     sorted.key.name<-paste(df.created,"$",key.name,"[",i[l],":",j[l],"]",sep="")
    
     sort.server.mean[[l]]<-ds.mean(sorted.key.name, type = "c", datasources = ds.test_env$connections)[[1]][1]
     #sort.server.var[[l]]<-ds.var(sorted.key.name, type = "c", datasources = ds.test_env$connections)[[1]][1]
     }
     
     
       #expect_equal(sort.server.var[[1]],
                    #sort.server.var[[2]],
                    #ds.test_env$tolerance)
       #expect_equal(sort.server.var[[2]],
                    #sort.server.var[[3]],
                    #ds.test_env$tolerance)
     
     if (sort.descending == TRUE & sort.method == "numeric")
     {
       expect_true(sort.server.mean[[1]] > sort.server.mean[[2]])
       expect_true(sort.server.mean[[2]] > sort.server.mean[[3]])
     }
     if (sort.descending == FALSE & sort.method == "numeric")
     {
       expect_true(sort.server.mean[[1]] < sort.server.mean[[2]])
       expect_true(sort.server.mean[[2]] < sort.server.mean[[3]])
     }
     
     
    
   }
  
  
}





