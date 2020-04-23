
## Libraries
require(DSI)
require(DSOpal)
require(dsBaseClient)
require(testthat)
library(dsDangerClient)

setwd("tests/testthat")

## Load local files
dataset1<-read.csv("data_files/DATASET1.csv")[,-1]
dataset2<-read.csv("data_files/DATASET2.csv")[,-1]
dataset3<-read.csv("data_files/DATASET3.csv")[,-1]

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
.test.data.frame.creation<-function(data.frame.name,key.name){
    # Create a sort data frame
    sort.key.name<-paste(data.frame.name,key.name,sep="$")
    ds.dataFrameSort(df.name = data.frame.name,
                   sort.key.name = sort.key.name,
                   sort.descending = FALSE,
                   sort.alphabetic = FALSE,
                   sort.numeric = TRUE,
                   newobj ="data.frame.name",
                   datasources = connections)
    type <- ds.class(data.frame.name)
    exists <- ds.exists(data.frame.name)
    cols.name <- ds.colnames(data.frame.name)
  
    expect_true(type[[1]][1]=="data.frame")
  
    for(i in 1:length(exists))
    {
      expect_true(exists[[i]])
    }
  
    for (i in 1:length(cols.name))
    {
        expect_equal(cols.name[[i]],colnames(local.df[[i]]))
      }
  
  }



.sort.numeric.increasing<-function(data.frame.name,key.name){
  # Sort local dfs
  sort.local<-list()
  for(i in 1:length(local.df)){
      sort.local[[i]]<-local.df[[i]][order(local.df[[i]][,key.name]),]
  }
  # Sort server dfs
  sort.key.name<-paste(data.frame.name,key.name,sep="$")
  ds.dataFrameSort(df.name = data.frame.name,
                   sort.key.name = sort.key.name,
                   sort.descending = FALSE,
                   sort.alphabetic = FALSE,
                   sort.numeric = TRUE,
                   newobj ="sort.server.numeric.increasing",
                   datasources = connections)
    # Upload server-side testing data frames in the client-side (danger function)
    server.data<-ds.DANGERdfEXTRACT("sort.server.numeric.increasing")
    server.data<-server.data$study.specific.df
    
  #test if the local data and server data is the same 
  
  for ( i in 1:length(server.data)){
      expect_equal(server.data[[i]][,key.name],sort.local[[i]][,key.name]) 
    }
  
}

.sort.numeric.descending<-function(data.frame.name,key.name){
  # Sort local dfs
  sort.local<-list()
  for(i in 1:length(local.df)){
    sort.local[[i]]<-local.df[[i]][order(local.df[[i]][,key.name],decreasing = TRUE),]
  }
  # Sort server dfs
  sort.key.name<-paste(data.frame.name,key.name,sep="$")
  ds.dataFrameSort(df.name = data.frame.name,
                   sort.key.name = sort.key.name,
                   sort.descending = TRUE,
                   sort.alphabetic = FALSE,
                   sort.numeric = TRUE,
                   newobj ="sort.server.numeric.descending",
                   datasources = connections)
  # Upload server-side testing data frames in the client-side (danger function)
  server.data<-ds.DANGERdfEXTRACT("sort.server.numeric.descending")
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



