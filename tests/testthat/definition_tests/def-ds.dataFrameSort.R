
## Libraries
require(DSI)
require(DSOpal)
require(dsBaseClient)
require(testthat)
library(dsDangerClient)

## Set directory
setwd("tests/testthat")

## Load local testing files
dataset1<-read.csv("data_files/TESTING/DATASET1.csv")[,-1]
dataset2<-read.csv("data_files/TESTING/DATASET2.csv")[,-1]
dataset3<-read.csv("data_files/TESTING/DATASET3.csv")[,-1]

##Parameters to change!!!!! 
key.name<-"INTEGER" #this parameter needs to change when creating the function
data.frame.name<-"TestData" #this parameter needs to change when creating the function
  #Save all local dataset in the same object (list)
local.df<-list(dataset1,dataset2,dataset3)

## Connecting to the Opal servers
builder <- DSI::newDSLoginBuilder(.silent = TRUE)
builder$append(server = "study1", 
               url = "http://192.168.56.100:8080/", 
               user = "administrator", password = "datashield_test&", 
               table = "TESTING.DATASET1", driver = "OpalDriver")
builder$append(server = "study2", 
               url = "http://192.168.56.100:8080/", 
               user = "administrator", password = "datashield_test&", 
               table = "TESTING.DATASET2", driver = "OpalDriver")
builder$append(server = "study3",
               url = "http://192.168.56.100:8080/", 
               user = "administrator", password = "datashield_test&", 
               table = "TESTING.DATASET3", driver = "OpalDriver")
logindata <- builder$build()
  # Log onto the remote Opal training servers
connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "TestData")



## Testing

  #Test the sorted data frame creation in the server
.test.data.frame.creation<-function(initial.df.name,key.name,df.created){
    # Create a sort data frame
    sort.key.name<-paste(initial.df.name,key.name,sep="$")
    ds.dataFrameSort(df.name = initial.df.name,
                   sort.key.name = sort.key.name,
                   sort.descending = FALSE,
                   sort.alphabetic = FALSE,
                   sort.numeric = TRUE,
                   newobj =df.created,
                   datasources = ds.test_env$connections)
    type <- ds.class(df.created, datasources = ds.test_env$connections)
    exists <- ds.exists(df.created, datasources = ds.test_env$connections)
    cols.name <- ds.colnames(df.created,datasources = ds.test_env$connections)
  
    expect_true(type[[1]][1]=="data.frame")
  
    for(i in 1:length(exists))
    {
      expect_true(exists[[i]])
    }
  
    for (i in 1:length(cols.name))
    {
        expect_equal(cols.name[[i]],ds.colnames(initial.df.name, datasources = ds.test_env$connections)[[1]],ds.test_env$tolerance)
      }
  
  }



.sort.numeric.increasing<-function(initial.df.name,key.name,df.created,local.df.list){
  #Order the column names of the local data frames as in the server data frames
  for(i in 1:length(local.df.list)){
    local.df.list[[i]]<-local.df.list[[i]][,ds.colnames(initial.df.name,datasources = ds.test_env$connections)[[i]]]
  }
  # Sort local dfs
  sort.local<-list()
  for(i in 1:length(local.df.list)){
      sort.local[[i]]<-local.df.list[[i]][order(local.df.list[[i]][,key.name]),]
  }
  # Sort server dfs
  sort.key.name<-paste(initial.df.name,key.name,sep="$")
  ds.dataFrameSort(df.name = initial.df.name,
                   sort.key.name = sort.key.name,
                   sort.descending = FALSE,
                   sort.alphabetic = FALSE,
                   sort.numeric = TRUE,
                   newobj =df.created,
                   datasources = connections)
    # Upload server-side testing data frames in the client-side (danger function)
    server.data<-ds.DANGERdfEXTRACT(df.created,datasources = ds.test_env$connections )
    server.data<-server.data$study.specific.df
    
  #test if the local data and server data is the same 
  
  for ( i in 1:length(server.data)){
      expect_equal(server.data[[i]][,key.name],sort.local[[i]][,key.name]) 
    }
  
}

.sort.numeric.descending<-function(initial.df.name,key.name,df.created,local.df.list){
  #Order the column names of the local data frames as in the server data frames
  for(i in 1:length(local.df.list)){
    local.df.list[[i]]<-local.df.list[[i]][,ds.colnames(initial.df.name,datasources = ds.test_env$connections)[[i]]]
  }
  # Sort local dfs
  sort.local<-list()
  for(i in 1:length( local.df.list)){
    sort.local[[i]]<- local.df.list[[i]][order( local.df.list[[i]][,key.name],decreasing = TRUE),]
  }
  # Sort server dfs
  sort.key.name<-paste(initial.df.name,key.name,sep="$")
  ds.dataFrameSort(df.name = initial.df.name,
                   sort.key.name = sort.key.name,
                   sort.descending = TRUE,
                   sort.alphabetic = FALSE,
                   sort.numeric = TRUE,
                   newobj =df.created,
                   datasources =  ds.test_env$connections)
  # Upload server-side testing data frames in the client-side (danger function)
  server.data<-ds.DANGERdfEXTRACT(df.created,ds.test_env$connections)
  server.data<-server.data$study.specific.df
  
  #test if the local data and server data is the same 
  
  for ( i in 1:length(server.data)){
      expect_equal(server.data[[i]][,key.name],sort.local[[i]][,key.name]) 
    }
}


.sort.lphabetic.increasing<-function(data.frame.name,key.name){
  # Sort local dfs
  sort.local<-list()
  for(i in 1:length(local.df)){
    sort.local[[i]]<-local.df[[i]][order(as.character(local.df[[i]][,key.name])),]
  }
  # Sort server dfs
  sort.key.name<-paste(data.frame.name,key.name,sep="$")
  ds.dataFrameSort(df.name = data.frame.name,
                   sort.key.name = sort.key.name,
                   sort.descending = FALSE,
                   sort.alphabetic = TRUE,
                   sort.numeric = FALSE,
                   newobj ="sort.server.lphabetic.increasing",
                   datasources = connections)
  # Upload server-side testing data frames in the client-side (danger function)
  server.data<-ds.DANGERdfEXTRACT("sort.server.lphabetic.increasing")
  server.data<-server.data$study.specific.df
  
  #test if the local data and server data is the same 
  
  for ( i in 1:length(server.data)){
      expect_equal(server.data[[i]][,key.name],sort.local[[i]][,key.name]) 
  }
}

.sort.alphabetic.descending<-function(data.frame.name,key.name){
  # Sort local dfs
  sort.local<-list()
  for(i in 1:length(local.df)){
    sort.local[[i]]<-local.df[[i]][order(as.character(local.df[[i]][,key.name]),decreasing = TRUE),]
  }
  # Sort server dfs
  sort.key.name<-paste(data.frame.name,key.name,sep="$")
  ds.dataFrameSort(df.name = data.frame.name,
                   sort.key.name = sort.key.name,
                   sort.descending = TRUE,
                   sort.alphabetic = TRUE,
                   sort.numeric = FALSE,
                   newobj ="sort.server.alphabetic.descending",
                   datasources = connections)
  # Upload server-side testing data frames in the client-side (danger function)
  server.data<-ds.DANGERdfEXTRACT("sort.server.alphabetic.descending")
  server.data<-server.data$study.specific.df
  
  #test if the local data and server data is the same 
  
  for ( i in 1:length(server.data)){
      expect_equal(server.data[[i]][,key.name],sort.local[[i]][,key.name]) 
  }
}


# Clear the Datashield R sessions and logout
datashield.logout(connections) 



