setwd("tests/testthat")
source("connection_to_datasets/init_testing_datasets.R")
source("definition_tests/def-assign-stats.R")
##########Then this need to be deleted############
ds.test_env <- new.env()
options(datashield.env=ds.test_env)
ds.test_env$tolerance = 10^-6
ds.test_env$local.values.1 <- read.csv("data_files/TESTING/DATASET1.csv", header = TRUE)
ds.test_env$local.values.2 <- read.csv("data_files/TESTING/DATASET2.csv", header = TRUE)
ds.test_env$local.values.3 <- read.csv("data_files/TESTING/DATASET3.csv", header = TRUE)
ds.test_env$local.values   <- list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1])

## Libraries
require(DSI)
require(DSOpal)
require(dsBaseClient)
require(testthat)

## Connecting to the Opal servers
builder <- DSI::newDSLoginBuilder()
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
connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")

## Set the parameters (This parameters come from the source of expected value test).
  # When creating the testing correctly this parameters need to be deleted
initial.df.name<-"D"
V1.name<-"INTEGER"
V2.name<-"NUMERIC"
boole<- ">"
df.created<-"subset.server"
keep.cols<-1:10
##Order the column names of the local data frames as in the server data frames
for(i in 1:length(ds.test_env$local.values)){
  ds.test_env$local.values[[i]]<-ds.test_env$local.values[[i]][,ds.colnames(initial.df.name,datasources = connections)[[i]]]
}
#####################################################################################
## Testing
V1<-paste(initial.df.name,V1.name,sep="$")
V2<-paste(initial.df.name,V2.name,sep="$")
.test.data.frame.creation<-function(initial.df.name,V1.name,V2.name,boole,df.created){
    ds.dataFrameSubset(df.name = initial.df.name,
                       V1.name = V1,
                       V2.name = V2,
                       Boolean.operator = boole,
                       newobj = df.created,
                       datasources = connections)  
  type <- ds.class(df.created, datasources = connections)
  exists <- ds.exists(df.created, datasources = connections)
  cols.name <- ds.colnames(df.created, datasources = connections)
  
  expect_true(type[[1]][1]=="data.frame")
  
  for(i in 1:length(exists))
  {
    expect_true(exists[[i]])
  }
  
  for (i in 1:length(cols.name))
  {
    expect_equal(cols.name[[i]],ds.colnames(df.created, datasources = connections)[[1]],ds.test_env$tolerance)
  }
}

subset.by.rows.NA<-function(initial.df.name,V1.name,V2.name,boole,df.created){
  #Local subset
  local.df.list<-ds.test_env$local.values
  df.subset.local<-list ()
  for (i in 1:length(ds.test_env$local.values)){
  sub.local.text<-paste0("local.df.list","[[",i,"]]","$",V1.name,boole,"local.df.list","[[",i,"]]","$",V2.name)  
  select.local.vec<-eval(parse(text=sub.local.text))
  select.local.vec[is.na(select.local.vec)==1]<-TRUE
  df.subset.local[[i]]<-local.df.list[[i]][select.local.vec,]
  }
  #Server subset
  V1<-paste(initial.df.name,V1.name,sep="$")
  V2<-paste(initial.df.name,V2.name,sep="$")
  ds.dataFrameSubset(df.name = initial.df.name,
                     V1.name = V1,
                     V2.name = V2,
                     Boolean.operator = boole,
                     keep.NAs = TRUE,
                     newobj = df.created,
                     datasources = connections)  
  #testing
  for (i in 1:length(df.subset.local))
  {
    expect_equal(dim(df.subset.local[[i]]),ds.dim(df.created, datasources = connections)[[i]],ds.test_env$tolerance)
  }
  
}

subset.by.rows.noNA<-function(initial.df.name,V1.name,V2.name,boole,df.created){
  #Local subset
  local.df.list<-ds.test_env$local.values
  df.subset.local<-list ()
  for (i in 1:length(ds.test_env$local.values)){
    sub.local.text<-paste0("local.df.list","[[",i,"]]","$",V1.name,boole,"local.df.list","[[",i,"]]","$",V2.name)  
    select.local.vec<-eval(parse(text=sub.local.text))
    select.local.vec[is.na(select.local.vec)==1]<-FALSE
    df.subset.local[[i]]<-local.df.list[[i]][select.local.vec,]
  }
  #Server subset
  V1<-paste(initial.df.name,V1.name,sep="$")
  V2<-paste(initial.df.name,V2.name,sep="$")
  ds.dataFrameSubset(df.name = initial.df.name,
                     V1.name = V1,
                     V2.name = V2,
                     Boolean.operator = boole,
                     keep.NAs = FALSE,
                     newobj = df.created,
                     datasources = connections)  
  #testing
  for (i in 1:length(df.subset.local))
  {
    expect_equal(dim(df.subset.local[[i]]),ds.dim(df.created, datasources = connections)[[i]],ds.test_env$tolerance)
  }
  
}

subset.by.rows.cols.NA<-function(initial.df.name,V1.name,V2.name,keep.cols,boole,df.created){
  #Local subset
  local.df.list<-ds.test_env$local.values
  df.subset.local<-list ()
  for (i in 1:length(ds.test_env$local.values)){
    sub.local.text<-paste0("local.df.list","[[",i,"]]","$",V1.name,boole,"local.df.list","[[",i,"]]","$",V2.name)  
    select.local.vec<-eval(parse(text=sub.local.text))
    select.local.vec[is.na(select.local.vec)==1]<-TRUE
    df.subset.local[[i]]<-local.df.list[[i]][select.local.vec,keep.cols]
  }
  #Server subset
  V1<-paste(initial.df.name,V1.name,sep="$")
  V2<-paste(initial.df.name,V2.name,sep="$")
  ds.dataFrameSubset(df.name = initial.df.name,
                     V1.name = V1,
                     V2.name = V2,
                     keep.cols = keep.cols,
                     Boolean.operator = boole,
                     keep.NAs = TRUE,
                     newobj = df.created,
                     datasources = connections)  
  #testing
  for (i in 1:length(df.subset.local))
  {
    expect_equal(dim(df.subset.local[[i]]),ds.dim(df.created, datasources = connections)[[i]],ds.test_env$tolerance)
    expect_equal(colnames(df.subset.local[[i]]),ds.colnames(df.created, datasources = connections)[[i]],ds.test_env$tolerance)
  }
  
}

subset.by.rows.cols.nonNA<-function(initial.df.name,V1.name,V2.name,keep.cols,boole,df.created){
  #Local subset
  local.df.list<-ds.test_env$local.values
  df.subset.local<-list ()
  for (i in 1:length(ds.test_env$local.values)){
    sub.local.text<-paste0("local.df.list","[[",i,"]]","$",V1.name,boole,"local.df.list","[[",i,"]]","$",V2.name)  
    select.local.vec<-eval(parse(text=sub.local.text))
    select.local.vec[is.na(select.local.vec)==1]<-FALSE
    df.subset.local[[i]]<-local.df.list[[i]][select.local.vec,keep.cols]
  }
  #Server subset
  V1<-paste(initial.df.name,V1.name,sep="$")
  V2<-paste(initial.df.name,V2.name,sep="$")
  ds.dataFrameSubset(df.name = initial.df.name,
                     V1.name = V1,
                     V2.name = V2,
                     keep.cols = keep.cols,
                     Boolean.operator = boole,
                     keep.NAs = FALSE,
                     newobj = df.created,
                     datasources = connections)  
  #testing
  for (i in 1:length(df.subset.local))
  {
    expect_equal(dim(df.subset.local[[i]]),ds.dim(df.created, datasources = connections)[[i]],ds.test_env$tolerance)
    expect_equal(colnames(df.subset.local[[i]]),ds.colnames(df.created, datasources = connections)[[i]],ds.test_env$tolerance)
  }
  
}


subset.by.cols.NA<-function(initial.df.name,V1.name,V2.name,keep.cols,df.created){
  #Local subset
  local.df.list<-ds.test_env$local.values
  df.subset.local<-list ()
  for (i in 1:length(ds.test_env$local.values)){
    df.subset.local[[i]]<-local.df.list[[i]][,keep.cols]
  }
  #Server subset
  V1<-paste(initial.df.name,V1.name,sep="$")
  context.ones<-paste0(V1,"-",V1,"+","1")
  #Create a vector with all ones
  ds.make(toAssign = context.ones,newobj = "ONES",datasources = connections)
  ds.dataFrameSubset(df.name = initial.df.name,
                     V1.name = "ONES",
                     V2.name = "ONES",
                     keep.cols = keep.cols,
                     Boolean.operator = "==",
                     keep.NAs = FALSE,
                     newobj = df.created,
                     datasources = connections)  
  #testing
  for (i in 1:length(df.subset.local))
  {
    expect_equal(dim(df.subset.local[[i]]),ds.dim(df.created, datasources = connections)[[i]],ds.test_env$tolerance)
    expect_equal(colnames(df.subset.local[[i]]),ds.colnames(df.created, datasources = connections)[[i]],ds.test_env$tolerance)
  }
  
}


# Clear the Datashield R sessions and logout
datashield.logout(connections) 