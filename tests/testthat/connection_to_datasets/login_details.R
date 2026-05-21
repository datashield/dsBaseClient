#This script determine all the login information to the servers used for the testing. 

source("connection_to_datasets/init_local_settings.R")

init.ip.address()

# create blank environment of test data
ds.test_env <- new.env()

# this option helps DSI to find the connection objects by looking in the right environment
options(datashield.env=ds.test_env)

ds.test_env$server_ip_address <- init.ip.address()

if (! is.null(getOption("default_driver"))) {
    ds.test_env$driver <- getOption("default_driver")
} else {
    # switch between "DSLiteDriver" and "OpalDriver", "ArmadilloDriver" to test
    ds.test_env$driver <- "DSLiteDriver"
    # ds.test_env$driver <- "OpalDriver"
    # ds.test_env$driver <- "ArmadilloDriver"
}

if ((ds.test_env$driver == "DSLiteDriver") || (ds.test_env$driver == "OpalDriver")) {
    ds.test_env$ping_address <- paste("https://", ds.test_env$server_ip_address, ":8443", sep="")
    ds.test_env$ping_config  <- config(timeout=5, ssl_verifyhost=0, ssl_verifypeer=0)

    ds.test_env$ip_address_1 <- paste("https://", ds.test_env$server_ip_address, ":8443", sep="")
    ds.test_env$ip_address_2 <- paste("https://", ds.test_env$server_ip_address, ":8443", sep="")
    ds.test_env$ip_address_3 <- paste("https://", ds.test_env$server_ip_address, ":8443", sep="")

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
} else if (ds.test_env$driver == "ArmadilloDriver") {
    ds.test_env$ping_address <- paste("http://", ds.test_env$server_ip_address, ":8080", sep="")
    ds.test_env$ping_config  <- config(timeout=5)

    ds.test_env$ip_address_1 <- paste("http://", ds.test_env$server_ip_address, ":8080", sep="")
    ds.test_env$ip_address_2 <- paste("http://", ds.test_env$server_ip_address, ":8080", sep="")
    ds.test_env$ip_address_3 <- paste("http://", ds.test_env$server_ip_address, ":8080", sep="")

    ds.test_env$user_1 <- getOption("opal.user", "admin")
    ds.test_env$user_2 <- getOption("opal.user", "admin")
    ds.test_env$user_3 <- getOption("opal.user", "admin")

    ds.test_env$password_1 <- getOption("opal.password", "admin")
    ds.test_env$password_2 <- getOption("opal.password", "admin")
    ds.test_env$password_3 <- getOption("opal.password", "admin")

    ds.test_env$options_1 <- "list()"
    ds.test_env$options_2 <- "list()"
    ds.test_env$options_3 <- "list()"

    ds.test_env$secure_login_details <- TRUE
} else {
    stop("**** Unknown Driver ****", call. = FALSE)
}

ds.test_env$high_tolerance     <- 10^-7
ds.test_env$medium_tolerance   <- 10^-6
ds.test_env$low_tolerance      <- 10^-4
ds.test_env$very_low_tolerance <- 10^-3
ds.test_env$tolerance          <- ds.test_env$medium_tolerance
