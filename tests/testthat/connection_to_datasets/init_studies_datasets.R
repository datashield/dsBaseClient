init.studies.dataset.cnsim <- function(variables)
{
    if (ds.test_env$secure_login_details)
    {
      if (ds.test_env$driver == "OpalDriver")
      {
        builder <- DSI::newDSLoginBuilder(.silent = TRUE)
        builder$append(server = "sim1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "CNSIM.CNSIM1", options=ds.test_env$options_1)
        builder$append(server = "sim2", url = ds.test_env$ip_address_2, user = ds.test_env$user_2, password = ds.test_env$password_2, table = "CNSIM.CNSIM2", options=ds.test_env$options_2)
        builder$append(server = "sim3", url = ds.test_env$ip_address_3, user = ds.test_env$user_3, password = ds.test_env$password_3, table = "CNSIM.CNSIM3", options=ds.test_env$options_3)
        ds.test_env$login.data <- builder$build()
      }
      else if (ds.test_env$driver == "ArmadilloDriver")
      {
        builder <- DSI::newDSLoginBuilder(.silent = TRUE)
        builder$append(server = "sim1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "datashield/cnsim/CNSIM1", driver = ds.test_env$driver)
        builder$append(server = "sim2", url = ds.test_env$ip_address_2, user = ds.test_env$user_2, password = ds.test_env$password_2, table = "datashield/cnsim/CNSIM2", driver = ds.test_env$driver)
        builder$append(server = "sim3", url = ds.test_env$ip_address_3, user = ds.test_env$user_3, password = ds.test_env$password_3, table = "datashield/cnsim/CNSIM3", driver = ds.test_env$driver)
        ds.test_env$login.data <- builder$build()
      }
      else 
      {
         ds.test_env$login.data <- DSLite::setupCNSIMTest("dsBase", env = ds.test_env)
      }
      ds.test_env$stats.var <- variables
    }
}

init.studies.dataset.dasim <- function(variables)
{
    if (ds.test_env$secure_login_details)
    {
      if (ds.test_env$driver == "OpalDriver")
      {
        builder <- DSI::newDSLoginBuilder(.silent = TRUE)
        builder$append(server = "sim1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "DASIM.DASIM1", options=ds.test_env$options_1)
        builder$append(server = "sim2", url = ds.test_env$ip_address_2, user = ds.test_env$user_2, password = ds.test_env$password_2, table = "DASIM.DASIM2", options=ds.test_env$options_2)
        builder$append(server = "sim3", url = ds.test_env$ip_address_3, user = ds.test_env$user_3, password = ds.test_env$password_3, table = "DASIM.DASIM3", options=ds.test_env$options_3)
        ds.test_env$login.data <- builder$build()
      }
      else if (ds.test_env$driver == "ArmadilloDriver")
      {
        builder <- DSI::newDSLoginBuilder(.silent = TRUE)
        builder$append(server = "sim1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "datashield/dasim/DASIM1", driver = ds.test_env$driver)
        builder$append(server = "sim2", url = ds.test_env$ip_address_2, user = ds.test_env$user_2, password = ds.test_env$password_2, table = "datashield/dasim/DASIM2", driver = ds.test_env$driver)
        builder$append(server = "sim3", url = ds.test_env$ip_address_3, user = ds.test_env$user_3, password = ds.test_env$password_3, table = "datashield/dasim/DASIM3", driver = ds.test_env$driver)
        ds.test_env$login.data <- builder$build()
      }
      else 
      {
        ds.test_env$login.data <- DSLite::setupDASIMTest("dsBase", env = ds.test_env)
      }
      ds.test_env$stats.var <- variables
    }
}

init.studies.dataset.survival <- function(variables)
{
    if (ds.test_env$secure_login_details)
    {
      if (ds.test_env$driver == "OpalDriver")
      {
        builder <- DSI::newDSLoginBuilder(.silent = TRUE)
        builder$append(server = "survival1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "SURVIVAL.EXPAND_WITH_MISSING1", options=ds.test_env$options_1)
        builder$append(server = "survival2", url = ds.test_env$ip_address_2, user = ds.test_env$user_2, password = ds.test_env$password_2, table = "SURVIVAL.EXPAND_WITH_MISSING2", options=ds.test_env$options_2)
        builder$append(server = "survival3", url = ds.test_env$ip_address_3, user = ds.test_env$user_3, password = ds.test_env$password_3, table = "SURVIVAL.EXPAND_WITH_MISSING3", options=ds.test_env$options_3)
        ds.test_env$login.data <- builder$build()
      }
      else if (ds.test_env$driver == "ArmadilloDriver")
      {
        builder <- DSI::newDSLoginBuilder(.silent = TRUE)
        builder$append(server = "survival1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "datashield/survival/EXPAND_WITH_MISSING1", driver = ds.test_env$driver)
        builder$append(server = "survival2", url = ds.test_env$ip_address_2, user = ds.test_env$user_2, password = ds.test_env$password_2, table = "datashield/survival/EXPAND_WITH_MISSING2", driver = ds.test_env$driver)
        builder$append(server = "survival3", url = ds.test_env$ip_address_3, user = ds.test_env$user_3, password = ds.test_env$password_3, table = "datashield/survival/EXPAND_WITH_MISSING3", driver = ds.test_env$driver)
        ds.test_env$login.data <- builder$build()
      }
      else 
      {
        ds.test_env$login.data <- DSLite::setupSURVIVALTest("dsBase", env = ds.test_env)
      }
      ds.test_env$stats.var <- variables
    }
}

init.studies.dataset.cluster.int <- function(variables)
{
  if (ds.test_env$secure_login_details)
  {
    #reading data from local files
    ds.test_env$local.values.1 <- read.csv("data_files/CLUSTER/CLUSTER_INT1.csv", header = TRUE)
    ds.test_env$local.values.2 <- read.csv("data_files/CLUSTER/CLUSTER_INT2.csv", header = TRUE)
    ds.test_env$local.values.3 <- read.csv("data_files/CLUSTER/CLUSTER_INT3.csv", header = TRUE)
    ds.test_env$local.values   <- rbind(ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3)
    if (ds.test_env$driver == "OpalDriver")
    {
      builder <- DSI::newDSLoginBuilder(.silent = TRUE)
      builder$append(server = "cluster.int1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "CLUSTER.CLUSTER_INT1", options=ds.test_env$options_1)
      builder$append(server = "cluster.int2", url = ds.test_env$ip_address_2, user = ds.test_env$user_2, password = ds.test_env$password_2, table = "CLUSTER.CLUSTER_INT2", options=ds.test_env$options_2)
      builder$append(server = "cluster.int3", url = ds.test_env$ip_address_3, user = ds.test_env$user_3, password = ds.test_env$password_3, table = "CLUSTER.CLUSTER_INT3", options=ds.test_env$options_3)
      ds.test_env$login.data <- builder$build()
    }
    else if (ds.test_env$driver == "ArmadilloDriver")
    {
      builder <- DSI::newDSLoginBuilder(.silent = TRUE)
      builder$append(server = "cluster.int1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "datashield/cluster/CLUSTER_INT1", driver = ds.test_env$driver)
      builder$append(server = "cluster.int2", url = ds.test_env$ip_address_2, user = ds.test_env$user_2, password = ds.test_env$password_2, table = "datashield/cluster/CLUSTER_INT2", driver = ds.test_env$driver)
      builder$append(server = "cluster.int3", url = ds.test_env$ip_address_3, user = ds.test_env$user_3, password = ds.test_env$password_3, table = "datashield/cluster/CLUSTER_INT3", driver = ds.test_env$driver)
      ds.test_env$login.data <- builder$build()
    }
    else 
    {
      #to do
      #ds.test_env$login.data <- DSLite::setupCLUSTERTest("dsBase", env = ds.test_env)
    }
    ds.test_env$stats.var <- variables
  }
}

init.studies.dataset.cluster.slo <- function(variables)
{
  if (ds.test_env$secure_login_details)
  {
    #reading data from local files
    ds.test_env$local.values.1 <- read.csv("data_files/CLUSTER/CLUSTER_SLO1.csv", header = TRUE)
    ds.test_env$local.values.2 <- read.csv("data_files/CLUSTER/CLUSTER_SLO2.csv", header = TRUE)
    ds.test_env$local.values.3 <- read.csv("data_files/CLUSTER/CLUSTER_SLO3.csv", header = TRUE)
    ds.test_env$local.values   <- rbind(ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3)
    if (ds.test_env$driver == "OpalDriver")
    {
      builder <- DSI::newDSLoginBuilder(.silent = TRUE)
      builder$append(server = "cluster.slo1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "CLUSTER.CLUSTER_SLO1", options=ds.test_env$options_1)
      builder$append(server = "cluster.slo2", url = ds.test_env$ip_address_2, user = ds.test_env$user_2, password = ds.test_env$password_2, table = "CLUSTER.CLUSTER_SLO2", options=ds.test_env$options_2)
      builder$append(server = "cluster.slo3", url = ds.test_env$ip_address_3, user = ds.test_env$user_3, password = ds.test_env$password_3, table = "CLUSTER.CLUSTER_SLO3", options=ds.test_env$options_3)
      ds.test_env$login.data <- builder$build()
    }
    else if (ds.test_env$driver == "ArmadilloDriver")
    {
      builder <- DSI::newDSLoginBuilder(.silent = TRUE)
      builder$append(server = "cluster.slo1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "datashield/cluster/CLUSTER_SLO1", driver = ds.test_env$driver)
      builder$append(server = "cluster.slo2", url = ds.test_env$ip_address_2, user = ds.test_env$user_2, password = ds.test_env$password_2, table = "datashield/cluster/CLUSTER_SLO2", driver = ds.test_env$driver)
      builder$append(server = "cluster.slo3", url = ds.test_env$ip_address_3, user = ds.test_env$user_3, password = ds.test_env$password_3, table = "datashield/cluster/CLUSTER_SLO3", driver = ds.test_env$driver)
      ds.test_env$login.data <- builder$build()
    }
    else 
    {
      #to do
      #ds.test_env$login.data <- DSLite::setupCLUSTERTest("dsBase", env = ds.test_env)
    }
    ds.test_env$stats.var <- variables

  }
}

init.studies.dataset.anthro <- function(variables)
{
  if (ds.test_env$secure_login_details)
  {
    #reading data from local files
    ds.test_env$local.values.1 <- read.csv("data_files/ANTHRO/anthro1.csv", header = TRUE)
    ds.test_env$local.values.2 <- read.csv("data_files/ANTHRO/anthro2.csv", header = TRUE)
    ds.test_env$local.values.3 <- read.csv("data_files/ANTHRO/anthro3.csv", header = TRUE)
    ds.test_env$local.values   <- rbind(ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3)
    if (ds.test_env$driver == "OpalDriver")
    {
      builder <- DSI::newDSLoginBuilder(.silent = TRUE)
      builder$append(server = "study1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "ANTHRO.anthro1", options=ds.test_env$options_1)
      builder$append(server = "study2", url = ds.test_env$ip_address_2, user = ds.test_env$user_2, password = ds.test_env$password_2, table = "ANTHRO.anthro2", options=ds.test_env$options_2)
      builder$append(server = "study3", url = ds.test_env$ip_address_3, user = ds.test_env$user_3, password = ds.test_env$password_3, table = "ANTHRO.anthro3", options=ds.test_env$options_3)
      ds.test_env$login.data <- builder$build()
    }
    else if (ds.test_env$driver == "ArmadilloDriver")
    {
      builder <- DSI::newDSLoginBuilder(.silent = TRUE)
      builder$append(server = "study1", url = ds.test_env$ip_address_1, user = ds.test_env$user_1, password = ds.test_env$password_1, table = "datashield/anthro/anthro1", driver = ds.test_env$driver)
      builder$append(server = "study2", url = ds.test_env$ip_address_2, user = ds.test_env$user_2, password = ds.test_env$password_2, table = "datashield/anthro/anthro2", driver = ds.test_env$driver)
      builder$append(server = "study3", url = ds.test_env$ip_address_3, user = ds.test_env$user_3, password = ds.test_env$password_3, table = "datashield/anthro/anthro3", driver = ds.test_env$driver)
      ds.test_env$login.data <- builder$build()
    }
    else 
    {
      #to do
      #ds.test_env$login.data <- DSLite::setupCLUSTERTest("dsBase", env = ds.test_env)
    }
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

connect.studies.dataset.cluster.int <- function(variables)
{
  log.out.data.server()
  source("connection_to_datasets/login_details.R")
  init.studies.dataset.cluster.int(variables)
  log.in.data.server()
}

connect.studies.dataset.cluster.slo <- function(variables)
{
  log.out.data.server()
  source("connection_to_datasets/login_details.R")
  init.studies.dataset.cluster.slo(variables)
  log.in.data.server()
}

connect.studies.dataset.anthro <- function(variables)
{
  log.out.data.server()
  source("connection_to_datasets/login_details.R")
  init.studies.dataset.anthro(variables)
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

disconnect.studies.dataset.cluster.int <- function()
{
  log.out.data.server()
}

disconnect.studies.dataset.cluster.slo <- function()
{
  log.out.data.server()
}

disconnect.studies.dataset.anthro <- function()
{
  log.out.data.server()
}
