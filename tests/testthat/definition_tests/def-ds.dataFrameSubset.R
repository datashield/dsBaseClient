
source("definition_tests/def-assign-stats.R")

.test.data.frame.creation<-function(initial.df.name,V1.name,V2.name,boole,df.created){
  #Create the correct form of V1.name and V2.name
  var1<-paste(initial.df.name,V1.name,sep="$")
  var2<-paste(initial.df.name,V2.name,sep="$")
    ds.dataFrameSubset(df.name = initial.df.name,
                       V1.name = var1,
                       V2.name = var2,
                       Boolean.operator = boole,
                       newobj = df.created,
                       datasources = ds.test_env$connections)  
  type <- ds.class(df.created, datasources = ds.test_env$connections)
  exists <- ds.exists(df.created, datasources = ds.test_env$connections)
  exists.length<-length(exists) 
  conns.length<-length(ds.test_env$connections)
  cols.name <- ds.colnames(df.created, datasources = ds.test_env$connections)
  int.df.cols.name<-ds.colnames(initial.df.name, datasources = ds.test_env$connections)
  
  expect_true(type[[1]][1]=="data.frame")
  
  for(i in 1:length(exists))
  {
    expect_true(exists[[i]])
  }
  
  expect_equal(exists.length,
               conns.length,
               ds.test_env$tolerance)
  
  
  for (i in 1:length(cols.name))
  {
    expect_equal(cols.name[[i]],
                 int.df.cols.name[[i]],
                 ds.test_env$tolerance)
  }
}


subset.by.rows<-function(initial.df.name,V1.name,V2.name,boole,keep.NAs,df.created,local.df.list){
  #Order the column names of the local data frames as in the server data frames
  for(i in 1:length(local.df.list))
    {
    local.df.list[[i]]<-local.df.list[[i]][,ds.colnames(x = initial.df.name,datasources = ds.test_env$connections)[[i]]]
    }
  #Local subset
  df.subset.local<-list ()
  for (i in 1:length(local.df.list))
    {
  sub.local.text<-paste0("local.df.list","[[",i,"]]","$",V1.name,boole,"local.df.list","[[",i,"]]","$",V2.name)
  select.local.vec<-eval(parse(text=sub.local.text))
  select.local.vec[is.na(select.local.vec)==1]<-keep.NAs
  df.subset.local[[i]]<-local.df.list[[i]][select.local.vec,]
  }
  #Server subset
  var1<-paste(initial.df.name,V1.name,sep="$")
  var2<-paste(initial.df.name,V2.name,sep="$")
  ds.dataFrameSubset(df.name = initial.df.name,
                     V1.name = var1,
                     V2.name = var2,
                     Boolean.operator = boole,
                     keep.NAs = keep.NAs,
                     newobj = df.created,
                     datasources = ds.test_env$connections)

  #testing
  for (i in 1:length(df.subset.local))
  {
    expect_equal(dim(df.subset.local[[i]]),
                 ds.dim(df.created, 
                        datasources = ds.test_env$connections)[[i]],
                        ds.test_env$tolerance)
  }
  #Calculate the mean, sd,length, min and max of the variable in the parameter 'V1.name'
  var1<-paste(df.created,V1.name,sep="$")
 
  local.rbind.df<-rbind(df.subset.local[[1]],df.subset.local[[2]],df.subset.local[[3]])
  dist.local <- .calc.distribution.locally(local.rbind.df[,V1.name])
  dist.server <- .calc.distribution.server(var1)
  expect_equal(dist.local,dist.server, tolerance = ds.test_env$tolerance)
}


subset.by.rows.cols<-function(initial.df.name,V1.name,V2.name,keep.cols,boole,keep.NAs,df.created,local.df.list){
  #Order the column names of the local data frames as in the server data frames
  for(i in 1:length(local.df.list)){
    local.df.list[[i]]<-local.df.list[[i]][,ds.colnames(initial.df.name,datasources = ds.test_env$connections)[[i]]]
  }
  #Order the column names of the local data frames as in the server data frames
  for(i in 1:length(local.df.list)){
    local.df.list[[i]]<-local.df.list[[i]][,ds.colnames(initial.df.name,datasources = ds.test_env$connections)[[i]]]
  }
  #Local subset
  df.subset.local<-list ()
  for (i in 1:length(local.df.list)){
    sub.local.text<-paste0("local.df.list","[[",i,"]]","$",V1.name,boole,"local.df.list","[[",i,"]]","$",V2.name)
    select.local.vec<-eval(parse(text=sub.local.text))
    select.local.vec[is.na(select.local.vec)==1]<-keep.NAs
    df.subset.local[[i]]<-local.df.list[[i]][select.local.vec,keep.cols]
  }
  #Server subset
  var1<-paste(initial.df.name,V1.name,sep="$")
  var2<-paste(initial.df.name,V2.name,sep="$")
  ds.dataFrameSubset(df.name = initial.df.name,
                     V1.name = var1,
                     V2.name = var2,
                     keep.cols = keep.cols,
                     Boolean.operator = boole,
                     keep.NAs = keep.NAs,
                     newobj = df.created,
                     datasources = ds.test_env$connections)
  #testing
  for (i in 1:length(df.subset.local))
  {
    expect_equal(dim(df.subset.local[[i]]),ds.dim(df.created, datasources = ds.test_env$connections)[[i]],ds.test_env$tolerance)
    expect_equal(colnames(df.subset.local[[i]]),ds.colnames(df.created, datasources = ds.test_env$connections)[[i]],ds.test_env$tolerance)
  }
  
}


subset.by.cols<-function(initial.df.name,V1.name,keep.cols,keep.NAs,df.created,local.df.list){
  #Order the column names of the local data frames as in the server data frames
  for(i in 1:length(local.df.list)){
    local.df.list[[i]]<-local.df.list[[i]][,ds.colnames(initial.df.name,datasources = ds.test_env$connections)[[i]]]
  }
  #Local subset
  df.subset.local<-list ()
  for (i in 1:length(local.df.list)){
    df.subset.local[[i]]<-local.df.list[[i]][,keep.cols]
  }
  #Server subset
  var1<-paste(initial.df.name,V1.name,sep="$")
  context.ones<-paste0(var1,"-",var1,"+","1")
  #Create a vector with all ones
  ds.make(toAssign = context.ones,newobj = "ONES",datasources = ds.test_env$connections)
  ds.dataFrameSubset(df.name = initial.df.name,
                     V1.name = "ONES",
                     V2.name = "ONES",
                     keep.cols = keep.cols,
                     Boolean.operator = "==",
                     keep.NAs = keep.NAs,
                     newobj = df.created,
                     datasources = ds.test_env$connections)
  #testing
  for (i in 1:length(df.subset.local))
  {
    expect_equal(dim(df.subset.local[[i]]),ds.dim(df.created, datasources = ds.test_env$connections)[[i]],ds.test_env$tolerance)
    expect_equal(colnames(df.subset.local[[i]]),ds.colnames(df.created, datasources = ds.test_env$connections)[[i]],ds.test_env$tolerance)
  }
  
}
# Clear the Datashield R sessions and logout
datashield.logout(ds.test_env$connections)