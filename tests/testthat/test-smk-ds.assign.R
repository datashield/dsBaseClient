#
# Set up
#

context("ds.assign::smk")


source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list('LAB_TSC', 'LAB_TRIG','LAB_HDL', 'LAB_GLUC_ADJUSTED', 'PM_BMI_CONTINUOUS', 'DIS_CVA', 'MEDI_LPD', 'DIS_DIAB', 'DIS_AMI', 'GENDER', 'PM_BMI_CATEGORICAL'))

#
# Tests
#

context("ds.assign::smk")

test_that("test_assign", {
    res <- ds.assign('D$LAB_TSC', 'assigned_obj')

    expect_equal(res, NULL)
})

#
# Tear down
#

