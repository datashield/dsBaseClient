#This script determine all the login information to the servers used for the testing. 
#The first time, you will need to edit the server_ip_address. The latter is required to set 
#access to the data on the virtual machines. 


source("connection_to_datasets/init_local_settings.R")
init.ip.address()
  ds.test_env <- new.env()

  # this option helps DSI to find the connection objects by looking in the right environment
  options(datashield.env=ds.test_env)

  # switch tetween "DSLiteDriver" and "OpalDriver" to test
  # ds.test_env$driver <- "DSLiteDriver"
  ds.test_env$driver <- "OpalDriver"

  ds.test_env$server_ip_address <- init.ip.address()
  
  opal_url <- paste("http://", ds.test_env$server_ip_address, ":8080", sep="")
  # opal_url <- paste("https://", ds.test_env$server_ip_address, ":8443", sep="")

  #This TCP/IP address is required to test a connect to the server. 
  ds.test_env$ping_address <- opal_url
  ds.test_env$ping_config  <- config(timeout=5)
  # ds.test_env$ping_config  <- config(timeout=5, ssl_verifyhost=0, ssl_verifypeer=0)

  ds.test_env$ip_address_1 <- opal_url
  ds.test_env$ip_address_2 <- opal_url
  ds.test_env$ip_address_3 <- opal_url

  ds.test_env$user_1 <- getOption("opal.user", "administrator")
  ds.test_env$user_2 <- getOption("opal.user", "administrator")
  ds.test_env$user_3 <- getOption("opal.user", "administrator")

  ds.test_env$password_1 <- getOption("opal.password", "datashield_test&")
  ds.test_env$password_2 <- getOption("opal.password", "datashield_test&")
  ds.test_env$password_3 <- getOption("opal.password", "datashield_test&")

  ds.test_env$options_1 <- "list(ssl_verifyhost=0, ssl_verifypeer=0)"
  ds.test_env$options_2 <- "list(ssl_verifyhost=0, ssl_verifypeer=0)"
  ds.test_env$options_3 <- "list(ssl_verifyhost=0, ssl_verifypeer=0)"

  ds.test_env$secure_login_details <- TRUE
  ds.test_env$tolerance            <- 10^-6
