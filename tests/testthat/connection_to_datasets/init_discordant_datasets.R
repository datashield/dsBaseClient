init.discordant.dataset.simple <- function(variables)
{
    if (ds.test_env$secure_login_details)
    {
        ds.test_env$server <- c("discordant1", "discordant2", "discordant3")
        ds.test_env$url <- c(ds.test_env$ip_address_1,ds.test_env$ip_address_2,ds.test_env$ip_address_3)
        ds.test_env$user <- c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3)
        ds.test_env$password <- c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3)
        ds.test_env$table <- c("DISCORDANT.DISCORDANT_STUDY1", "DISCORDANT.DISCORDANT_STUDY2", "DISCORDANT.DISCORDANT_STUDY3")
        ds.test_env$login.data <- datashield.build.login.data.frame.o(ds.test_env$server,
                                                                      ds.test_env$url,
                                                                      ds.test_env$table,
                                                                      ds.test_env$user,
                                                                      ds.test_env$password)

        ds.test_env$stats.var <- variables
    }
}

connect.discordant.dataset.simple <- function(variables)
{
    log.out.data.server()
    source("connection_to_datasets/login_details.R")
    init.discordant.dataset.simple(variables)
    log.in.data.server()
}
