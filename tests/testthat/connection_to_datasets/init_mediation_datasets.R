init.mediation.dataset.upb <- function(variables)
{
    if (ds.test_env$secure_login_details)
    {
        if (ds.test_env$driver == "OpalDriver")
        {
            builder <- DSI::newDSLoginBuilder(.silent = TRUE)
            builder$append(server = "study1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "MEDIATION.UPBdata1", options=ds.test_env$options_1)
            builder$append(server = "study2", url = ds.test_env$ip_address_2, user = ds.test_env$user_2, password = ds.test_env$password_2, table = "MEDIATION.UPBdata2", options=ds.test_env$options_2)
            builder$append(server = "study3", url = ds.test_env$ip_address_3, user = ds.test_env$user_3, password = ds.test_env$password_3, table = "MEDIATION.UPBdata3", options=ds.test_env$options_3)
            ds.test_env$login.data <- builder$build()
        }
        else 
        {
            ds.test_env$login.data <- DSLite::setupMediationTest("dsBase", env = ds.test_env)
        }
        ds.test_env$stats.var <- variables
    }
}

init.mediation.dataset.student <- function(variables)
{
    if (ds.test_env$secure_login_details)
    {
        if (ds.test_env$driver == "OpalDriver")
        {
            builder <- DSI::newDSLoginBuilder(.silent = TRUE)
            builder$append(server = "study1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "MEDIATION.student", options=ds.test_env$options_1)
            ds.test_env$login.data <- builder$build()
        }
        else 
        {
            ds.test_env$login.data <- DSLite::setupMediationTest("dsBase", env = ds.test_env)
        }
        ds.test_env$stats.var <- variables
    }
}

init.mediation.dataset.framing <- function(variables)
{
    if (ds.test_env$secure_login_details)
    {
        if (ds.test_env$driver == "OpalDriver")
        {
            builder <- DSI::newDSLoginBuilder(.silent = TRUE)
            builder$append(server = "study1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "MEDIATION.framing", options=ds.test_env$options_1)
            ds.test_env$login.data <- builder$build()
        }
        else 
        {
            ds.test_env$login.data <- DSLite::setupMediationTest("dsBase", env = ds.test_env)
        }
        ds.test_env$stats.var <- variables
    }
}

init.mediation.dataset.vv2015 <- function(variables)
{
    if (ds.test_env$secure_login_details)
    {
        if (ds.test_env$driver == "OpalDriver")
        {
            builder <- DSI::newDSLoginBuilder(.silent = TRUE)
            builder$append(server = "study1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "MEDIATION.vv2015", options=ds.test_env$options_1)
            ds.test_env$login.data <- builder$build()
        }
        else 
        {
            ds.test_env$login.data <- DSLite::setupMediationTest("dsBase", env = ds.test_env)
        }
        ds.test_env$stats.var <- variables
    }
}

connect.mediation.dataset.upb <- function(variables)
{
    log.out.data.server()
    source("connection_to_datasets/login_details.R")
    init.mediation.dataset.upb(variables)
    log.in.data.server()
}

connect.mediation.dataset.student <- function(variables)
{
    log.out.data.server()
    source("connection_to_datasets/login_details.R")
    init.mediation.dataset.student(variables)
    log.in.data.server()
}

connect.mediation.dataset.framing <- function(variables)
{
    log.out.data.server()
    source("connection_to_datasets/login_details.R")
    init.mediation.dataset.framing(variables)
    log.in.data.server()
}

connect.mediation.dataset.vv2015 <- function(variables)
{
    log.out.data.server()
    source("connection_to_datasets/login_details.R")
    init.mediation.dataset.vv2015(variables)
    log.in.data.server()
}

disconnect.mediation.dataset.upb <- function()
{
    log.out.data.server()
}

disconnect.mediation.dataset.student <- function()
{
    log.out.data.server()
}

disconnect.mediation.dataset.framing <- function()
{
    log.out.data.server()
}

disconnect.mediation.dataset.vv2015 <- function()
{
    log.out.data.server()
}
