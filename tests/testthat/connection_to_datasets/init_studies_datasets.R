init.studies.dataset.cnsim <- function(variables)
{
    if (ds.test_env$secure_login_details)
    {
        ds.test_env$server <- c("sim1", "sim2", "sim3")
        ds.test_env$url <- c(ds.test_env$ip_address_1,ds.test_env$ip_address_2,ds.test_env$ip_address_3)
        ds.test_env$user <- c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3)
        ds.test_env$password <- c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3)
        ds.test_env$table <- c("CNSIM.CNSIM1", "CNSIM.CNSIM2", "CNSIM.CNSIM3")
        ds.test_env$login.data <- datashield.build.login.data.frame.o(ds.test_env$server,
                                                                      ds.test_env$url,
                                                                      ds.test_env$table,
                                                                      ds.test_env$user,
                                                                      ds.test_env$password)

        ds.test_env$stats.var <- variables
    }
}

init.studies.dataset.dasim <- function(variables)
{
    if (ds.test_env$secure_login_details)
    {
        ds.test_env$server <- c("sim1", "sim2", "sim3")
        ds.test_env$url <- c(ds.test_env$ip_address_1,ds.test_env$ip_address_2,ds.test_env$ip_address_3)
        ds.test_env$user <- c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3)
        ds.test_env$password <- c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3)
        ds.test_env$table <- c("DASIM.DASIM1", "DASIM.DASIM2", "DASIM.DASIM3")
        ds.test_env$login.data <- datashield.build.login.data.frame.o(ds.test_env$server,
                                                                      ds.test_env$url,
                                                                      ds.test_env$table,
                                                                      ds.test_env$user,
                                                                      ds.test_env$password)

        ds.test_env$stats.var <- variables
    }
}

init.studies.dataset.survival <- function(variables)
{
    if (ds.test_env$secure_login_details)
    {
        ds.test_env$server <- c("survival1", "survival2", "survival3")
        ds.test_env$url <- c(ds.test_env$ip_address_1,ds.test_env$ip_address_2,ds.test_env$ip_address_3)
        ds.test_env$user <- c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3)
        ds.test_env$password <- c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3)
        ds.test_env$table <- c("SURVIVAL.EXPAND_WITH_MISSING1", "SURVIVAL.EXPAND_WITH_MISSING2", "SURVIVAL.EXPAND_WITH_MISSING3")
        ds.test_env$login.data <- datashield.build.login.data.frame.o(ds.test_env$server,
                                                                      ds.test_env$url,
                                                                      ds.test_env$table,
                                                                      ds.test_env$user,
                                                                      ds.test_env$password)

        ds.test_env$stats.var <- variables
    }
}

connect.studies.dataset.cnsim <- function(variables)
{
    log.out.data.server()
    source("connection_to_datasets/login_details.R")
    init.studies.dataset.cnsim(variables)
    log.in.data.server()
}

connect.studies.dataset.dasim <- function(variables)
{
    log.out.data.server()
    source("connection_to_datasets/login_details.R")
    init.studies.dataset.dasim(variables)
    log.in.data.server()
}

connect.studies.dataset.survival <- function(variables)
{
    log.out.data.server()
    source("connection_to_datasets/login_details.R")
    init.studies.dataset.survival(variables)
    log.in.data.server()
}

disconnect.studies.dataset.cnsim <- function()
{
    log.out.data.server()
}

disconnect.studies.dataset.dasim <- function()
{
    log.out.data.server()
}

disconnect.studies.dataset.survival <- function()
{
    log.out.data.server()
}

