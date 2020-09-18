library(remotes)
install.packages("MolgenisArmadillo", repos = "https://registry.molgenis.org/repository/R")
library(MolgenisArmadillo)
library(DSI)

# init_studies_dataset
CNSIM1 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/CNSIM/CNSIM1.csv", col_types = readr::cols("PM_BMI_CATEGORICAL" = readr::col_factor(levels = c("1", "2", "3")), "GENDER" = readr::col_factor(), "DIS_CVA" = readr::col_factor(), "MEDI_LPD" = readr::col_factor(), "DIS_DIAB" = readr::col_factor(), "DIS_AMI" = readr::col_factor())))
CNSIM2 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/CNSIM/CNSIM2.csv", col_types = readr::cols("PM_BMI_CATEGORICAL" = readr::col_factor(levels = c("1", "2", "3")), "GENDER" = readr::col_factor(), "DIS_CVA" = readr::col_factor(), "MEDI_LPD" = readr::col_factor(), "DIS_DIAB" = readr::col_factor(), "DIS_AMI" = readr::col_factor())))
CNSIM3 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/CNSIM/CNSIM3.csv", col_types = readr::cols("PM_BMI_CATEGORICAL" = readr::col_factor(levels = c("1", "2", "3")), "GENDER" = readr::col_factor(), "DIS_CVA" = readr::col_factor(), "MEDI_LPD" = readr::col_factor(), "DIS_DIAB" = readr::col_factor(), "DIS_AMI" = readr::col_factor())))

DASIM1 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/DASIM/DASIM1.csv"))
DASIM2 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/DASIM/DASIM2.csv"))
DASIM3 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/DASIM/DASIM3.csv"))

EXPAND_WITH_MISSING1 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/SURVIVAL/EXPAND_MISSING/expand_missing_study1.csv"))
EXPAND_WITH_MISSING2 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/SURVIVAL/EXPAND_MISSING/expand_missing_study2.csv"))
EXPAND_WITH_MISSING3 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/SURVIVAL/EXPAND_MISSING/expand_missing_study3.csv"))

CLUSTER_INT1 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/CLUSTER/CLUSTER_INT1.csv", col_types = readr::cols("trtGrp" = readr::col_factor(), "nDoctors" = readr::col_integer(), "idDoctor" = readr::col_factor(), "Male" = readr::col_factor(), "private" = readr::col_factor(), "nPatients" = readr::col_integer(), "diabetes" = readr::col_factor(), "incid_rate" = readr::col_integer(), "idSurgery" = readr::col_factor())))
CLUSTER_INT2 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/CLUSTER/CLUSTER_INT2.csv", col_types = readr::cols("trtGrp" = readr::col_factor(), "nDoctors" = readr::col_integer(), "idDoctor" = readr::col_factor(), "Male" = readr::col_factor(), "private" = readr::col_factor(), "nPatients" = readr::col_integer(), "diabetes" = readr::col_factor(), "incid_rate" = readr::col_integer(), "idSurgery" = readr::col_factor())))
CLUSTER_INT3 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/CLUSTER/CLUSTER_INT3.csv", col_types = readr::cols("trtGrp" = readr::col_factor(), "nDoctors" = readr::col_integer(), "idDoctor" = readr::col_factor(), "Male" = readr::col_factor(), "private" = readr::col_factor(), "nPatients" = readr::col_integer(), "diabetes" = readr::col_factor(), "incid_rate" = readr::col_integer(), "idSurgery" = readr::col_factor())))

CLUSTER_SLO1 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/CLUSTER/CLUSTER_SLO1.csv"))
CLUSTER_SLO2 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/CLUSTER/CLUSTER_SLO2.csv"))
CLUSTER_SLO3 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/CLUSTER/CLUSTER_SLO3.csv"))

# init_discordant_datasets
DISCORDANT_STUDY1 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/DISCORDANT/discordant_study1.csv"))
DISCORDANT_STUDY2 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/DISCORDANT/discordant_study2.csv"))
DISCORDANT_STUDY3 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/DISCORDANT/discordant_study3.csv"))

# init_testing_datasets
DATASET1 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/TESTING/DATASET1.csv"), readr::col_types("FACTOR_CHARACTER" = readr::col_factor(), "FACTOR_INTEGER" = readr::col_factor()))
DATASET2 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/TESTING/DATASET2.csv"), readr::col_types("FACTOR_CHARACTER" = readr::col_factor(), "FACTOR_INTEGER" = readr::col_factor()))
DATASET3 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/TESTING/DATASET3.csv"), readr::col_types("FACTOR_CHARACTER" = readr::col_factor(), "FACTOR_INTEGER" = readr::col_factor()))

FACTOR_LEVELS_DATASET1 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/FACTOR_LEVELS/FACTOR_LEVELS_DATASET1.csv"))
FACTOR_LEVELS_DATASET2 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/FACTOR_LEVELS/FACTOR_LEVELS_DATASET2.csv"))
FACTOR_LEVELS_DATASET3 <- base::as.data.frame(readr::read_csv("tests/testthat/data_files/FACTOR_LEVELS/FACTOR_LEVELS_DATASET3.csv"))

armadillo_url <- "http://localhost:8080"
storage_url <- "http://localhost:9000"
token <- MolgenisArmadillo::armadillo.get_token(armadillo_url)
token
MolgenisArmadillo::armadillo.assume_role_with_web_identity(token = token, server = storage_url)
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
