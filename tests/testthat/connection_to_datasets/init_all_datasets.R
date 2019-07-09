

init.all.datasets <- function()
{
  if (ds.test_env$secure_login_details)
  {
    #reading data from local files 
    ds.test_env$local.values.1 <- read.csv("data_files/data_set_1.csv", header = TRUE)
    ds.test_env$local.values.2 <- read.csv("data_files/data_set_2.csv", header = TRUE)
    ds.test_env$local.values.3 <- read.csv("data_files/data_set_3.csv", header = TRUE)
    ds.test_env$local.values   <- rbind(ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3)

    #connecting to the servers
    ds.test_env$server <- c("study1", "study2", "study3")
    ds.test_env$url <- c(ds.test_env$ip_address_1,ds.test_env$ip_address_2,ds.test_env$ip_address_3)
    ds.test_env$user <- c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3)
    ds.test_env$password <- c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3)
    ds.test_env$table <- c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3")
    ds.test_env$login.data <- datashield.build.login.data.frame.o(ds.test_env$server,
                                                              ds.test_env$url,
                                                              ds.test_env$table,
                                                              ds.test_env$user,
                                                              ds.test_env$password)

    ds.test_env$stats.var <- list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
                                  'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
                                  'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC','FACTOR_CHARACTER', 
                                  'FACTOR_INTEGER')


    
  }
}

init.dataset.3 <- function()
{
  if (ds.test_env$secure_login_details)
  {

    ds.test_env$local.values.3 <- read.csv("data_files/data_set_3.csv", header = TRUE)
    
    
    ds.test_env$server <- c("study3")
    ds.test_env$url <- c(ds.test_env$ip_address_3)
    ds.test_env$user <- c(ds.test_env$user_3)
    ds.test_env$password <- c(ds.test_env$password_3)
    ds.test_env$table <- c("TESTING.DATASET3")
    ds.test_env$login.data <- datashield.build.login.data.frame.o(ds.test_env$server,
                                                                  ds.test_env$url,
                                                                  ds.test_env$table,
                                                                  ds.test_env$user,
                                                                  ds.test_env$password)
    
    ds.test_env$stats.var <- list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
                                  'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
                                  'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC','FACTOR_CHARACTER', 
                                  'FACTOR_INTEGER')
  }
}


init.dataset.2 <- function()
{
  if (ds.test_env$secure_login_details)
  {
    ds.test_env$local.values.2 <- read.csv("data_files/data_set_2.csv", header = TRUE)
    
    
    ds.test_env$server <- c("study2")
    ds.test_env$url <- c(ds.test_env$ip_address_2)
    ds.test_env$user <- c(ds.test_env$user_2)
    ds.test_env$password <- c(ds.test_env$password_2)
    ds.test_env$table <- c("TESTING.DATASET2")
    ds.test_env$login.data <- datashield.build.login.data.frame.o(ds.test_env$server,
                                                                  ds.test_env$url,
                                                                  ds.test_env$table,
                                                                  ds.test_env$user,
                                                                  ds.test_env$password)
    
    ds.test_env$stats.var <- list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
                                  'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
                                  'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC','FACTOR_CHARACTER', 
                                  'FACTOR_INTEGER')
  }
}


init.dataset.1 <- function()
{
  if (ds.test_env$secure_login_details)
  {

    ds.test_env$local.values.1 <- read.csv("data_files/data_set_1.csv", header = TRUE)
    
    
    ds.test_env$server <- c("study1")
    ds.test_env$url <- c(ds.test_env$ip_address_1)
    ds.test_env$user <- c(ds.test_env$user_1)
    ds.test_env$password <- c(ds.test_env$password_1)
    ds.test_env$table <- c("TESTING.DATASET1")
    ds.test_env$login.data <- datashield.build.login.data.frame.o(ds.test_env$server,
                                                                  ds.test_env$url,
                                                                  ds.test_env$table,
                                                                  ds.test_env$user,
                                                                  ds.test_env$password)
    
    ds.test_env$stats.var <- list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
                                  'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
                                  'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC','FACTOR_CHARACTER', 
                                  'FACTOR_INTEGER')
  }
}

log.in.data.server <- function()
{
  ds.test_env$connection.opal <- datashield.login(logins=ds.test_env$login.data, assign=TRUE,variables=ds.test_env$stats.var)
}


log.out.data.server <- function()
{
  objs        <- ls(.GlobalEnv)
  test_index  <- length(objs[objs %in% c("ds.test_env")])
  if (test_index >= 1)
  {
    datashield.logout(ds.test_env$connection.opal)
    rm(list = ls())
    gc()
  }
}

connect.all.datasets <- function()
{
   log.out.data.server()
   source("connection_to_datasets/login_details.R")
   init.all.datasets()
   log.in.data.server()
}

connect.dataset.1 <- function()
{
  log.out.data.server()
  source("connection_to_datasets/login_details.R")
  init.dataset.1()
  log.in.data.server()
}

connect.dataset.2 <- function()
{
  log.out.data.server()
  source("connection_to_datasets/login_details.R")
  init.dataset.2()
  log.in.data.server()
}

connect.dataset.3 <- function()
{
  log.out.data.server()
  source("connection_to_datasets/login_details.R")
  init.dataset.3()
  log.in.data.server()
}
