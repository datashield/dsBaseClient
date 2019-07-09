init.smk.dataset.sim <- function(variables)
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

init.smk.dataset.survival <- function(variables)
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

connect.smk.dataset.sim <- function(variables)
{
    log.out.data.server()
    source("connection_to_datasets/login_details.R")
    init.smk.dataset.sim(variables)
    log.in.data.server()
}

connect.smk.dataset.survival <- function(variables)
{
    log.out.data.server()
    source("connection_to_datasets/login_details.R")
    init.smk.dataset.survival(variables)
    log.in.data.server()
}
