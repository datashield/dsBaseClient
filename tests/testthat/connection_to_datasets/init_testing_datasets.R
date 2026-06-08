# Purpose: This script provides all the functions to connect to testing data. The latter are stored 
# in data_files folder of test_that and on the virtual machine as TESTING.DATASET1, TESTING.DATASET2, 
# TESTING.DATASET3.
# Author: Patricia Ryser-Welch, DataSHIELD team

# Connect to three servers and the three datasets. Four local variables named ds.test_env$local.values.1,
# ds.test_env$local.values.2, ds.test_env$local.values.3 and ds.test_env$local.values are created.
init.testing.datasets <- function()
{
  log.out.data.server()
  if (ds.test_env$secure_login_details)
  {
    #reading data from local files 
    local.values.1.name        <- load("data_files/TESTING/DATASET1.rda")
    ds.test_env$local.values.1 <- eval(as.symbol(local.values.1.name))
    local.values.2.name        <- load("data_files/TESTING/DATASET2.rda")
    ds.test_env$local.values.2 <- eval(as.symbol(local.values.2.name))
    local.values.3.name        <- load("data_files/TESTING/DATASET3.rda")
    ds.test_env$local.values.3 <- eval(as.symbol(local.values.3.name))
    ds.test_env$local.values   <- rbind(ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3)
    if (ds.test_env$driver == "OpalDriver") 
    {
      builder <- DSI::newDSLoginBuilder(.silent = TRUE)
      builder$append(server = "study1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "TESTING.DATASET1", options=ds.test_env$options_1)
      builder$append(server = "study2", url = ds.test_env$ip_address_2, user = ds.test_env$user_2, password = ds.test_env$password_2, table = "TESTING.DATASET2", options=ds.test_env$options_2)
      builder$append(server = "study3", url = ds.test_env$ip_address_3, user = ds.test_env$user_3, password = ds.test_env$password_3, table = "TESTING.DATASET3", options=ds.test_env$options_3)
      ds.test_env$login.data <- builder$build()
    }
    else if (ds.test_env$driver == "ArmadilloDriver")
    {
      builder <- DSI::newDSLoginBuilder(.silent = TRUE)
      builder$append(server = "study1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "datashield/testing/DATASET1", driver = "ArmadilloDriver")
      builder$append(server = "study2", url = ds.test_env$ip_address_2, user = ds.test_env$user_2, password = ds.test_env$password_2, table = "datashield/testing/DATASET2", driver = "ArmadilloDriver")
      builder$append(server = "study3", url = ds.test_env$ip_address_3, user = ds.test_env$user_3, password = ds.test_env$password_3, table = "datashield/testing/DATASET3", driver = "ArmadilloDriver")
      ds.test_env$login.data <- builder$build()
    }
    else 
    {
      ds.test_env$login.data <- DSLite::setupDATASETTest("dsBase", env = ds.test_env)
    }
    ds.test_env$stats.var <- list('CHARACTER', 'LOGICAL', 'NA_VALUES', 'INTEGER', 'NULL_VALUES',
                                  'NON_NEGATIVE_INTEGER', 'POSITIVE_INTEGER', 'NEGATIVE_INTEGER',
                                  'NUMERIC', 'NON_NEGATIVE_NUMERIC', 'POSITIVE_NUMERIC', 'NEGATIVE_NUMERIC', 'FACTOR_CHARACTER',
                                  'FACTOR_INTEGER', 'IDENTIFIER', 'CATEGORY', 'IDENTIFIER', 'CATEGORY', 'NUMERIC_ONE_CHANGE', 'INTEGER_ONE_CHANGE')
  }
}

# Connect to one server and the three datasets. One local variables named ds.test_env$local.values.3 is created.
init.dataset.3 <- function()
{
  log.out.data.server()
  if (ds.test_env$secure_login_details)
  {
    local.values.3.name        <- load("data_files/TESTING/DATASET3.rda")
    ds.test_env$local.values.3 <- eval(as.symbol(local.values.3.name))
    if (ds.test_env$driver == "OpalDriver")
    {
      builder <- DSI::newDSLoginBuilder(.silent = TRUE)
      builder$append(server = "study3", url = ds.test_env$ip_address_3, user = ds.test_env$user_3, password = ds.test_env$password_3, table = "TESTING.DATASET3", options=ds.test_env$options_3)
      ds.test_env$login.data <- builder$build()
    }
    else if (ds.test_env$driver == "ArmadilloDriver")
    {
      builder <- DSI::newDSLoginBuilder(.silent = TRUE)
      builder$append(server = "study3", url = ds.test_env$ip_address_3, user = ds.test_env$user_3, password = ds.test_env$password_3, table = "datashield/testing/DATASET3", driver = "ArmadilloDriver")
      ds.test_env$login.data <- builder$build()
    }
    else
    {
      login.data <- DSLite::setupDATASETTest("dsBase", env = ds.test_env)
      ds.test_env$login.data <- subset(login.data, server=="study3")
    }
    ds.test_env$stats.var <- list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
                                  'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
                                  'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC','FACTOR_CHARACTER', 
                                  'FACTOR_INTEGER','IDENTIFIER','CATEGORY','IDENTIFIER','CATEGORY', 'NUMERIC_ONE_CHANGE', 'INTEGER_ONE_CHANGE')
  }
}

# Connect to one server and the three datasets. One local variables named ds.test_env$local.values.2 is created.
init.dataset.2 <- function()
{
  log.out.data.server()
  if (ds.test_env$secure_login_details)
  {
    local.values.2.name        <- load("data_files/TESTING/DATASET2.rda")
    ds.test_env$local.values.2 <- eval(as.symbol(local.values.2.name))
    if (ds.test_env$driver == "OpalDriver")
    {
      builder <- DSI::newDSLoginBuilder(.silent = TRUE)
      builder$append(server = "study2", url = ds.test_env$ip_address_2, user = ds.test_env$user_2, password = ds.test_env$password_2, table = "TESTING.DATASET2", options=ds.test_env$options_2)
      ds.test_env$login.data <- builder$build()
    }
    else if (ds.test_env$driver == "ArmadilloDriver")
    {
      builder <- DSI::newDSLoginBuilder(.silent = TRUE)
      builder$append(server = "study2", url = ds.test_env$ip_address_2, user = ds.test_env$user_2, password = ds.test_env$password_2, table = "datashield/testing/DATASET2", driver = "ArmadilloDriver")
      ds.test_env$login.data <- builder$build()
    }
    else
    {
      login.data <- DSLite::setupDATASETTest("dsBase", env = ds.test_env)
      ds.test_env$login.data <- subset(login.data, server=="study2")
    } 
    ds.test_env$stats.var <- list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
                                  'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
                                  'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC','FACTOR_CHARACTER', 
                                  'FACTOR_INTEGER','IDENTIFIER','CATEGORY','IDENTIFIER','CATEGORY', 'NUMERIC_ONE_CHANGE', 'INTEGER_ONE_CHANGE')
  }
}

# Connect to one server and the three datasets. One local variables named ds.test_env$local.values.2is created.
init.dataset.1 <- function()
{
  log.out.data.server()
  if (ds.test_env$secure_login_details)
  {
    local.values.1.name        <- load("data_files/TESTING/DATASET1.rda")
    ds.test_env$local.values.1 <- eval(as.symbol(local.values.1.name))
    if (ds.test_env$driver == "OpalDriver")
    {
      builder <- DSI::newDSLoginBuilder(.silent = TRUE)
      builder$append(server = "study1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "TESTING.DATASET1", options=ds.test_env$options_1)
      ds.test_env$login.data <- builder$build()
    }
    else if (ds.test_env$driver == "ArmadilloDriver")
    {
      builder <- DSI::newDSLoginBuilder(.silent = TRUE)
      builder$append(server = "study1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "datashield/testing/DATASET1", driver = "ArmadilloDriver")
      ds.test_env$login.data <- builder$build()
    }
    else
    {
      login.data <- DSLite::setupDATASETTest("dsBase", env = ds.test_env)
      ds.test_env$login.data <- subset(login.data, server=="study1")
    }  
    ds.test_env$stats.var <- list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
                                  'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
                                  'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC','FACTOR_CHARACTER', 
                                  'FACTOR_INTEGER','IDENTIFIER','CATEGORY','IDENTIFIER','CATEGORY', 'NUMERIC_ONE_CHANGE', 'INTEGER_ONE_CHANGE')
  }
}

#####FACTOR_LEVELS

# Connect to one server and the two studies. One local variables named ds.test_env$local.values.2is created.
init.testing.dataset.factor_levels <- function()
{
  log.out.data.server()
  if (ds.test_env$secure_login_details)
  {
    local.values.1.name        <- load("data_files/FACTOR_LEVELS/FACTOR_LEVELS1.rda")
    ds.test_env$local.values.1 <- eval(as.symbol(local.values.1.name))
    local.values.2.name        <- load("data_files/FACTOR_LEVELS/FACTOR_LEVELS2.rda")
    ds.test_env$local.values.2 <- eval(as.symbol(local.values.2.name))
    local.values.3.name        <- load("data_files/FACTOR_LEVELS/FACTOR_LEVELS3.rda")
    ds.test_env$local.values.3 <- eval(as.symbol(local.values.3.name))
    ds.test_env$local.values   <- rbind(ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3)
    if (ds.test_env$driver == "OpalDriver")
    {
      builder <- DSI::newDSLoginBuilder(.silent = TRUE)
      builder$append(server = "GROUP1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "FACTOR_LEVELS.FACTOR_LEVELS1", options=ds.test_env$options_1)
      builder$append(server = "GROUP2", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "FACTOR_LEVELS.FACTOR_LEVELS2", options=ds.test_env$options_2)
      builder$append(server = "GROUP3", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "FACTOR_LEVELS.FACTOR_LEVELS3", options=ds.test_env$options_3)
      ds.test_env$login.data <- builder$build()
    }
    else if (ds.test_env$driver == "ArmadilloDriver")
    {
      builder <- DSI::newDSLoginBuilder(.silent = TRUE)
      builder$append(server = "GROUP1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "datashield/factor_levels/FACTOR_LEVELS1", driver = "ArmadilloDriver")
      builder$append(server = "GROUP2", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "datashield/factor_levels/FACTOR_LEVELS2", driver = "ArmadilloDriver")
      builder$append(server = "GROUP3", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "datashield/factor_levels/FACTOR_LEVELS3", driver = "ArmadilloDriver")
      ds.test_env$login.data <- builder$build()
    }
    else
    {
      login.data <- DSLite::setupDATASETTest("dsBase", env = ds.test_env)
      ds.test_env$login.data <- subset(login.data, server=="study1")
    }  
    ds.test_env$stats.var <- list('ID', 'COLOURS', 'COLOURS.NUMBERS', 'POSITIVE.NUMBERS', 'NEGATIVE.NUMBERS', 'NUMBERS',
                                  'POSITIVE.DECIMAL', 'NEGATIVE.DECIMAL', 'DECIMAL', 'PLANETS.CHARACTERS')
  }
}

init.testing.dataset.factor_levels.1 <- function()
{
  log.out.data.server()
  if (ds.test_env$secure_login_details)
  {
    local.values.1.name        <- load("data_files/FACTOR_LEVELS/FACTOR_LEVELS1.rda")
    ds.test_env$local.values.1 <- eval(as.symbol(local.values.1.name))
    if (ds.test_env$driver == "OpalDriver")
    {
      builder <- DSI::newDSLoginBuilder(.silent = TRUE)
      builder$append(server = "GROUP1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "FACTOR_LEVELS.FACTOR_LEVELS1", options=ds.test_env$options_1)
      ds.test_env$login.data <- builder$build()
    }
    else if (ds.test_env$driver == "ArmadilloDriver")
    {
      builder <- DSI::newDSLoginBuilder(.silent = TRUE)
      builder$append(server = "GROUP1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "datashield/factor_levels/FACTOR_LEVELS1", driver = "ArmadilloDriver")
      ds.test_env$login.data <- builder$build()
    }
    else
    {
      login.data <- DSLite::setupDATASETTest("dsBase", env = ds.test_env)
      ds.test_env$login.data <- subset(login.data, server=="study1")
    }  
    ds.test_env$stats.var <- list('ID', 'COLOURS', 'COLOURS.NUMBERS', 'POSITIVE.NUMBERS', 'NEGATIVE.NUMBERS', 'NUMBERS',
                                  'POSITIVE.DECIMAL', 'NEGATIVE.DECIMAL', 'DECIMAL', 'PLANETS.CHARACTERS')
  }
}

log.in.data.server <- function()
{
  # ds.test_env$connections <- datashield.login(logins=ds.test_env$login.data, assign=TRUE,variables=ds.test_env$stats.var, opts = getOption("datashield.opts", list(ssl_verifyhost=0, ssl_verifypeer=0)))

  if (isTRUE(ds.test_env$stay_logged_in) && !is.null(ds.test_env$connections))
  {
    # Try to reuse the existing login when the new test file targets the same
    # servers. clear.data.server() doubles as a liveness probe: if the cached
    # connection is stale (e.g. logged out, or restored dead from a saved
    # workspace) the ds.ls() inside it errors, and we drop through to a fresh
    # login. A successful clear means the connection is good, so we re-assign the
    # data table without re-authenticating. The assign itself is NOT guarded, so a
    # genuine assignment error still surfaces.
    same.servers <- setequal(names(ds.test_env$connections), as.character(ds.test_env$login.data$server))
    cleared <- same.servers && tryCatch({ clear.data.server(); TRUE }, error = function(e) FALSE)
    if (cleared)
    {
      assign.current.dataset()
      return(invisible(NULL))
    }
    # Different servers, or a stale connection: close it (best effort) and log in afresh.
    try(datashield.logout(ds.test_env$connections), silent = TRUE)
    ds.test_env$connections <- NULL
  }

  ds.test_env$connections <- datashield.login(logins=ds.test_env$login.data, assign=TRUE,variables=ds.test_env$stats.var, opts = getOption("datashield.opts"))

  if (isTRUE(ds.test_env$stay_logged_in))
  {
    # Arrange for a single logout once the whole test run has finished.
    register.teardown.logout()
  }
}


log.out.data.server <- function()
{
  if (isTRUE(ds.test_env$stay_logged_in))
  {
    # Stay connected for the next test file. Clearing the server-side environment
    # happens in the reuse path of log.in.data.server() (where the connection has
    # just been confirmed live); the real logout happens once at end of the run.
    return(invisible(NULL))
  }

  if (!is.null(ds.test_env) && !is.null(ds.test_env$connections))
  {
    datashield.logout(ds.test_env$connections)
    # Reflect that the handle is now dead, so a later run cannot inherit and try
    # to reuse it (which would fail with an authorization error).
    ds.test_env$connections <- NULL
  }
  rm(list = ls())
  gc()
}


# Remove every object from the server-side environment(s) on the current login.
# ds.ls() returns a per-server list whose $objects.found holds the object names;
# ds.rm() takes a vector of names in one (aggregate) call and ignores ones that
# are already absent, so this is safe across servers that hold different objects.
clear.data.server <- function()
{
  object.names <- unique(unlist(lapply(ds.ls(), function(x) x$objects.found)))
  if (length(object.names) > 0)
  {
    ds.rm(object.names)
  }
}


# (Re)assign the data table 'D' for the current dataset onto the existing
# connection, without re-authenticating. Mirrors datashield.login(assign=TRUE),
# using the table/variables prepared by the init.* function for this test file.
assign.current.dataset <- function()
{
  tables <- stats::setNames(as.character(ds.test_env$login.data$table), as.character(ds.test_env$login.data$server))
  DSI::datashield.assign.table(ds.test_env$connections, "D", tables, variables = ds.test_env$stats.var)
}


# Register a one-off logout to run after all test files complete. Uses the
# testthat teardown environment, so it fires once at the end of the run - and at
# the end of a single-file run too. If we are not inside a testthat run the
# registration is skipped (the session is closed when R exits).
register.teardown.logout <- function()
{
  if (isTRUE(ds.test_env$logout_registered))
  {
    return(invisible(NULL))
  }
  ds.test_env$logout_registered <- tryCatch(
    {
      withr::defer(final.logout.data.server(), testthat::teardown_env())
      TRUE
    },
    error = function(e) FALSE
  )
}


# Unconditional logout, used by the deferred teardown above.
final.logout.data.server <- function()
{
  if (!is.null(ds.test_env) && !is.null(ds.test_env$connections))
  {
    datashield.logout(ds.test_env$connections)
    ds.test_env$connections <- NULL
  }
  ds.test_env$logout_registered <- NULL
}

connect.all.datasets <- function()
{
   log.out.data.server()
   source("connection_to_datasets/login_details.R")
   init.testing.datasets()
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

connect.testing.dataset.factor_levels <- function()
{
  log.out.data.server()
  source("connection_to_datasets/login_details.R")
  init.testing.dataset.factor_levels()
  log.in.data.server()
}

connect.testing.dataset.factor_levels.1 <- function()
{
  log.out.data.server()
  source("connection_to_datasets/login_details.R")
  init.testing.dataset.factor_levels.1()
  log.in.data.server()
}

disconnect.all.datasets <- function()
{
    log.out.data.server()
}

disconnect.dataset.1 <- function()
{
    log.out.data.server()
}

disconnect.dataset.2 <- function()
{
    log.out.data.server()
}

disconnect.dataset.3 <- function()
{
    log.out.data.server()
}

disconnect.testing.dataset.factor_levels <- function()
{
  log.out.data.server()
}

disconnect.testing.dataset.factor_levels.1 <- function()
{
  log.out.data.server()
}
