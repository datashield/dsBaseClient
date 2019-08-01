#This script determine all the login information to the servers used for the testing. 
#The first time, you will need to edit the server_ip_address. The latter is required to set 
#access to the data on the virtual machines. 


source("connection_to_datasets/init_local_settings.R")
init.ip.address()
ds.test_env <- new.env()
  ds.test_env$tolerance = 10^-6

  ds.test_env$contexts <- c('opal','dsi','dslite','continuous','coverage')  

  ds.test_env$server_ip_address = init.ip.address()
  
  ds.test_env$context = 'opal'

  ds.test_env$ip_address_1 <- paste("https://", ds.test_env$server_ip_address, ":8443", sep="")
  ds.test_env$ip_address_2 <- paste("https://", ds.test_env$server_ip_address, ":8443", sep="")
  ds.test_env$ip_address_3 <- paste("https://", ds.test_env$server_ip_address, ":8443", sep="")

  #This TCP/IP address is required to test a connect to the server. 
  ds.test_env$ping_address <- paste("http://", ds.test_env$server_ip_address, ":8080", sep="" )

  ds.test_env$user_1 <- "administrator"
  ds.test_env$user_2 <- "administrator"
  ds.test_env$user_3 <- "administrator"

  ds.test_env$password_1 <- "datashield_test&"
  ds.test_env$password_2 <- "datashield_test&"
  ds.test_env$password_3 <- "datashield_test&"
  ds.test_env$secure_login_details = TRUE



