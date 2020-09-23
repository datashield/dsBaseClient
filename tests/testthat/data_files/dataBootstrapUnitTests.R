library(remotes)
install.packages("MolgenisArmadillo", repos = "https://registry.molgenis.org/repository/R")
library(MolgenisArmadillo)
library(DSI)

# init_studies_dataset
CNSIM1 <- load("data_files/CNSIM/CNSIM1.rda")
CNSIM2 <- load("data_files/CNSIM/CNSIM2.rda")
CNSIM3 <- load("data_files/CNSIM/CNSIM3.rda")

DASIM1 <- load("data_files/DASIM/DASIM1.rda")
DASIM2 <- load("data_files/DASIM/DASIM2.rda")
DASIM3 <- load("data_files/DASIM/DASIM3.rda")

EXPAND_WITH_MISSING1 <- load("data_files/SURVIVAL/EXPAND_MISSING/expand_missing_study1.rda")
EXPAND_WITH_MISSING2 <- load("data_files/SURVIVAL/EXPAND_MISSING/expand_missing_study2.rda")
EXPAND_WITH_MISSING3 <- load("data_files/SURVIVAL/EXPAND_MISSING/expand_missing_study3.rda")

CLUSTER_INT1 <- load("data_files/CLUSTER/CLUSTER_INT1.rda")
CLUSTER_INT2 <- load("data_files/CLUSTER/CLUSTER_INT2.rda")
CLUSTER_INT3 <- load("data_files/CLUSTER/CLUSTER_INT3.rda")

CLUSTER_SLO1 <- load("data_files/CLUSTER/CLUSTER_SLO1.rda")
CLUSTER_SLO2 <- load("data_files/CLUSTER/CLUSTER_SLO2.rda")
CLUSTER_SLO3 <- load("data_files/CLUSTER/CLUSTER_SLO3.rda")

# init_discordant_datasets
DISCORDANT_STUDY1 <- load("data_files/DISCORDANT/discordant_study1.rda")
DISCORDANT_STUDY2 <- load("data_files/DISCORDANT/discordant_study2.rda")
DISCORDANT_STUDY3 <- load("data_files/DISCORDANT/discordant_study3.rda")

# init_testing_datasets
DATASET1 <- load("data_files/TESTING/DATASET1.rda")
DATASET2 <- load("data_files/TESTING/DATASET2.rda")
DATASET3 <- load("data_files/TESTING/DATASET3.rda")

FACTOR_LEVELS_DATASET1 <- load("data_files/FACTOR_LEVELS/FACTOR_LEVELS_DATASET1.rda")
FACTOR_LEVELS_DATASET2 <- load("data_files/FACTOR_LEVELS/FACTOR_LEVELS_DATASET2.rda")
FACTOR_LEVELS_DATASET3 <- load("data_files/FACTOR_LEVELS/FACTOR_LEVELS_DATASET3.rda")


storage_url <- "http://localhost:9000"
# access_key and secret_key can be found in the minio configuration in the docker-compose.yml
MolgenisArmadillo::armadillo.set_credentials(server = storage_url, access_key = "molgenis", secret_key = "molgenis")
MolgenisArmadillo::armadillo.create_project("testdata")


# init_studies_datasets
MolgenisArmadillo::armadillo.upload_table("testdata", "cnsim", CNSIM1)
MolgenisArmadillo::armadillo.upload_table("testdata", "cnsim", CNSIM2)
MolgenisArmadillo::armadillo.upload_table("testdata", "cnsim", CNSIM3)

MolgenisArmadillo::armadillo.upload_table("testdata", "dasim", DASIM1)
MolgenisArmadillo::armadillo.upload_table("testdata", "dasim", DASIM2)
MolgenisArmadillo::armadillo.upload_table("testdata", "dasim", DASIM3)

MolgenisArmadillo::armadillo.upload_table("testdata", "survival", EXPAND_WITH_MISSING1)
MolgenisArmadillo::armadillo.upload_table("testdata", "survival", EXPAND_WITH_MISSING2)
MolgenisArmadillo::armadillo.upload_table("testdata", "survival", EXPAND_WITH_MISSING3)

MolgenisArmadillo::armadillo.upload_table("testdata", "cluster", CLUSTER_INT1)
MolgenisArmadillo::armadillo.upload_table("testdata", "cluster", CLUSTER_INT2)
MolgenisArmadillo::armadillo.upload_table("testdata", "cluster", CLUSTER_INT3)

MolgenisArmadillo::armadillo.upload_table("testdata", "cluster", CLUSTER_SLO1)
MolgenisArmadillo::armadillo.upload_table("testdata", "cluster", CLUSTER_SLO2)
MolgenisArmadillo::armadillo.upload_table("testdata", "cluster", CLUSTER_SLO3)


# init_discordant_datasets
MolgenisArmadillo::armadillo.upload_table("testdata", "discordant", DISCORDANT_STUDY1)
MolgenisArmadillo::armadillo.upload_table("testdata", "discordant", DISCORDANT_STUDY2)
MolgenisArmadillo::armadillo.upload_table("testdata", "discordant", DISCORDANT_STUDY3)


# remove first column
DATASET1 <- DATASET1[,-1]
DATASET2 <- DATASET2[,-1]
DATASET3 <- DATASET3[,-1]

# init_testing_datasets
MolgenisArmadillo::armadillo.upload_table("testdata", "testing", DATASET1)
MolgenisArmadillo::armadillo.upload_table("testdata", "testing", DATASET2)
MolgenisArmadillo::armadillo.upload_table("testdata", "testing", DATASET3)

MolgenisArmadillo::armadillo.upload_table("testdata", "factor_levels", FACTOR_LEVELS_DATASET1)
MolgenisArmadillo::armadillo.upload_table("testdata", "factor_levels", FACTOR_LEVELS_DATASET2)
MolgenisArmadillo::armadillo.upload_table("testdata", "factor_levels", FACTOR_LEVELS_DATASET3)

class(CNSIM1$PM_BMI_CATEGORICAL)
levels(CNSIM1$PM_BMI_CATEGORICAL)

devtools::test()
devtools::test(filter = "datachk", invert = TRUE)
devtools::test(filter = "dataFrameSort")
  
  # build the login dataframe
builder <- DSI::newDSLoginBuilder()
builder$append(server = "armadillo",
               url = armadillo_url,
               user = "admin",
               password = "admin",
               table = "testdata/testing/DISCORDANT_STUDY1",
               driver = "ArmadilloDriver")

# create loginframe
logindata <- builder$build()
logindata

datashield.logout(conns)

conns <- datashield.login(logins = logindata, assign = TRUE)
ds.ls()
dsGetInfo(conns$armadillo)
ds.colnames("D")
datashield.errors()
dsGetInfo(conns$armadillo)
