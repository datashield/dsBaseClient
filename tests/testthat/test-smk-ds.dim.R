#
# Set up
#

context("dsClient::ds.dim")

options(datashield.variables=list('LAB_TSC', 'LAB_TRIG','LAB_HDL', 'LAB_GLUC_ADJUSTED', 'PM_BMI_CONTINUOUS', 'DIS_CVA', 'MEDI_LPD', 'DIS_DIAB', 'DIS_AMI', 'GENDER', 'PM_BMI_CATEGORICAL'))
source("setup.R")

#
# Tests
#

context("dsClient::ds.dim()")

test_that("test_dim", {
    expect_equal(ds.dim(x='D')$sim1, c(2163,11))
    expect_equal(ds.dim(x='D')$sim2, c(3088,11))
    expect_equal(ds.dim(x='D')$sim3, c(4128,11))
    expect_equal(ds.dim(x='D', type='combine')$pooled.dimension, c(9379,11))
})


context("dsClient::ds.dim() errors")
test_that("dim_errors", {
    expect_error(ds.dim(), "Please provide a the name of a data.frame or matrix!", fixed=TRUE)
    expect_error(ds.dim(x='D$LAB_TSC'), "The input object must be a table structure!", fixed=TRUE)
    expect_error(ds.dim(x='D', type='datashield'), 'Function argument "type" has to be either "combine" or "split"', fixed=TRUE)
})

#
# Tear down
#

source("teardown.R")