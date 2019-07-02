#
# Set up
#

context("dsClient::ds.assign")

options(datashield.variables=list('LAB_TSC', 'LAB_TRIG','LAB_HDL', 'LAB_GLUC_ADJUSTED', 'PM_BMI_CONTINUOUS', 'DIS_CVA', 'MEDI_LPD', 'DIS_DIAB', 'DIS_AMI', 'GENDER', 'PM_BMI_CATEGORICAL'))
source("setup.R")

#
# Tests
#

context("dsClient::ds.assign()")

test_that("test_assign", {
    res <- ds.assign('D$LAB_TSC', 'assigned_obj')

    print("====")
    print(res)
    print("====")

    expect_equal(res, NULL)
})

#
# Tear down
#

source("teardown.R")
