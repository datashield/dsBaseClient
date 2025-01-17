#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

context("DASIM::datachk::setup")

connect.studies.dataset.dasim(list('LAB_TSC', 'LAB_TRIG', 'LAB_HDL', 'LAB_GLUC_FASTING', 'PM_BMI_CONTINUOUS', 'DIS_CVA', 'DIS_DIAB', 'DIS_AMI', 'GENDER', 'PM_BMI_CATEGORICAL'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("DASIM::datachk")
test_that("Check DASIM dataset", {
    res.class <- ds.class(x='D')
    expect_length(res.class, 3)
    expect_gte(length(res.class$sim1), 1)
    expect_true("data.frame" %in% res.class$sim1)
    expect_gte(length(res.class$sim2), 1)
    expect_true("data.frame" %in% res.class$sim2)
    expect_gte(length(res.class$sim3), 1)
    expect_true("data.frame" %in% res.class$sim3)

    res.length <- ds.length(x='D')
    expect_length(res.length, 4)
    expect_length(res.length$`length of D in sim1`, 1)
    expect_equal(res.length$`length of D in sim1`, 10)
    expect_length(res.length$`length of D in sim2`, 1)
    expect_equal(res.length$`length of D in sim2`, 10)
    expect_length(res.length$`length of D in sim3`, 1)
    expect_equal(res.length$`length of D in sim3`, 10)
    expect_equal(res.length$`total length of D in all studies combined`, 30)

    res.colnames <- ds.colnames(x='D')
    expect_length(res.colnames, 3)
    expect_length(res.colnames$sim1, 10)
    expect_equal(res.colnames$sim1, c('LAB_TSC', 'LAB_TRIG', 'LAB_HDL', 'LAB_GLUC_FASTING', 'PM_BMI_CONTINUOUS', 'DIS_CVA', 'DIS_DIAB', 'DIS_AMI', 'GENDER', 'PM_BMI_CATEGORICAL'))
    expect_length(res.colnames$sim2, 10)
    expect_equal(res.colnames$sim2, c('LAB_TSC', 'LAB_TRIG', 'LAB_HDL', 'LAB_GLUC_FASTING', 'PM_BMI_CONTINUOUS', 'DIS_CVA', 'DIS_DIAB', 'DIS_AMI', 'GENDER', 'PM_BMI_CATEGORICAL'))
    expect_length(res.colnames$sim3, 10)
    expect_equal(res.colnames$sim3, c('LAB_TSC', 'LAB_TRIG', 'LAB_HDL', 'LAB_GLUC_FASTING', 'PM_BMI_CONTINUOUS', 'DIS_CVA', 'DIS_DIAB', 'DIS_AMI', 'GENDER', 'PM_BMI_CATEGORICAL'))

    res.class.lab_tsc <- ds.class(x='D$LAB_TSC')
    expect_length(res.class.lab_tsc, 3)
    expect_length(res.class.lab_tsc$sim1, 1)
    expect_equal(res.class.lab_tsc$sim1, "numeric")
    expect_length(res.class.lab_tsc$sim2, 1)
    expect_equal(res.class.lab_tsc$sim2, "numeric")
    expect_length(res.class.lab_tsc$sim3, 1)
    expect_equal(res.class.lab_tsc$sim3, "numeric")

    res.length.lab_tsc <- ds.length(x='D$LAB_TSC')
    expect_length(res.length.lab_tsc, 4)
    expect_length(res.length.lab_tsc$`length of D$LAB_TSC in sim1`, 1)
    expect_equal(res.length.lab_tsc$`length of D$LAB_TSC in sim1`, 10000)
    expect_length(res.length.lab_tsc$`length of D$LAB_TSC in sim2`, 1)
    expect_equal(res.length.lab_tsc$`length of D$LAB_TSC in sim2`, 10000)
    expect_length(res.length.lab_tsc$`length of D$LAB_TSC in sim3`, 1)
    expect_equal(res.length.lab_tsc$`length of D$LAB_TSC in sim3`, 10000)
    expect_length(res.length.lab_tsc$`total length of D$LAB_TSC in all studies combined`, 1)
    expect_equal(res.length.lab_tsc$`total length of D$LAB_TSC in all studies combined`, 30000)

    res.class.lab_trig <- ds.class(x='D$LAB_TRIG')
    expect_length(res.class.lab_trig, 3)
    expect_length(res.class.lab_trig$sim1, 1)
    expect_equal(res.class.lab_trig$sim1, "numeric")
    expect_length(res.class.lab_trig$sim2, 1)
    expect_equal(res.class.lab_trig$sim2, "numeric")
    expect_length(res.class.lab_trig$sim3, 1)
    expect_equal(res.class.lab_trig$sim3, "numeric")

    res.length.lab_trig <- ds.length(x='D$LAB_TRIG')
    expect_length(res.length.lab_trig, 4)
    expect_length(res.length.lab_trig$`length of D$LAB_TRIG in sim1`, 1)
    expect_equal(res.length.lab_trig$`length of D$LAB_TRIG in sim1`, 10000)
    expect_length(res.length.lab_trig$`length of D$LAB_TRIG in sim2`, 1)
    expect_equal(res.length.lab_trig$`length of D$LAB_TRIG in sim2`, 10000)
    expect_length(res.length.lab_trig$`length of D$LAB_TRIG in sim3`, 1)
    expect_equal(res.length.lab_trig$`length of D$LAB_TRIG in sim3`, 10000)
    expect_length(res.length.lab_trig$`total length of D$LAB_TRIG in all studies combined`, 1)
    expect_equal(res.length.lab_trig$`total length of D$LAB_TRIG in all studies combined`, 30000)

    res.class.lab_hdl <- ds.class(x='D$LAB_HDL')
    expect_length(res.class.lab_hdl, 3)
    expect_length(res.class.lab_hdl$sim1, 1)
    expect_equal(res.class.lab_hdl$sim1, "numeric")
    expect_length(res.class.lab_hdl$sim2, 1)
    expect_equal(res.class.lab_hdl$sim2, "numeric")
    expect_length(res.class.lab_hdl$sim3, 1)
    expect_equal(res.class.lab_hdl$sim3, "numeric")

    res.length.lab_hdl <- ds.length(x='D$LAB_HDL')
    expect_length(res.length.lab_hdl, 4)
    expect_length(res.length.lab_hdl$`length of D$LAB_HDL in sim1`, 1)
    expect_equal(res.length.lab_hdl$`length of D$LAB_HDL in sim1`, 10000)
    expect_length(res.length.lab_hdl$`length of D$LAB_HDL in sim2`, 1)
    expect_equal(res.length.lab_hdl$`length of D$LAB_HDL in sim2`, 10000)
    expect_length(res.length.lab_hdl$`length of D$LAB_HDL in sim3`, 1)
    expect_equal(res.length.lab_hdl$`length of D$LAB_HDL in sim3`, 10000)
    expect_length(res.length.lab_hdl$`total length of D$LAB_HDL in all studies combined`, 1)
    expect_equal(res.length.lab_hdl$`total length of D$LAB_HDL in all studies combined`, 30000)

    res.class.lab_gluc_fasting <- ds.class(x='D$LAB_GLUC_FASTING')
    expect_length(res.class.lab_gluc_fasting, 3)
    expect_length(res.class.lab_gluc_fasting$sim1, 1)
    expect_equal(res.class.lab_gluc_fasting$sim1, "numeric")
    expect_length(res.class.lab_gluc_fasting$sim2, 1)
    expect_equal(res.class.lab_gluc_fasting$sim2, "numeric")
    expect_length(res.class.lab_gluc_fasting$sim3, 1)
    expect_equal(res.class.lab_gluc_fasting$sim3, "numeric")

    res.length.lab_gluc_fasting <- ds.length(x='D$LAB_GLUC_FASTING')
    expect_length(res.length.lab_gluc_fasting, 4)
    expect_length(res.length.lab_gluc_fasting$`length of D$LAB_GLUC_FASTING in sim1`, 1)
    expect_equal(res.length.lab_gluc_fasting$`length of D$LAB_GLUC_FASTING in sim1`, 10000)
    expect_length(res.length.lab_gluc_fasting$`length of D$LAB_GLUC_FASTING in sim2`, 1)
    expect_equal(res.length.lab_gluc_fasting$`length of D$LAB_GLUC_FASTING in sim2`, 10000)
    expect_length(res.length.lab_gluc_fasting$`length of D$LAB_GLUC_FASTING in sim3`, 1)
    expect_equal(res.length.lab_gluc_fasting$`length of D$LAB_GLUC_FASTING in sim3`, 10000)
    expect_length(res.length.lab_gluc_fasting$`total length of D$LAB_GLUC_FASTING in all studies combined`, 1)
    expect_equal(res.length.lab_gluc_fasting$`total length of D$LAB_GLUC_FASTING in all studies combined`, 30000)

    res.class.pm_bmi_continuous <- ds.class(x='D$PM_BMI_CONTINUOUS')
    expect_length(res.class.pm_bmi_continuous, 3)
    expect_length(res.class.pm_bmi_continuous$sim1, 1)
    expect_equal(res.class.pm_bmi_continuous$sim1, "numeric")
    expect_length(res.class.pm_bmi_continuous$sim2, 1)
    expect_equal(res.class.pm_bmi_continuous$sim2, "numeric")
    expect_length(res.class.pm_bmi_continuous$sim3, 1)
    expect_equal(res.class.pm_bmi_continuous$sim3, "numeric")

    res.length.pm_bmi_continuous <- ds.length(x='D$PM_BMI_CONTINUOUS')
    expect_length(res.length.pm_bmi_continuous, 4)
    expect_length(res.length.pm_bmi_continuous$`length of D$PM_BMI_CONTINUOUS in sim1`, 1)
    expect_equal(res.length.pm_bmi_continuous$`length of D$PM_BMI_CONTINUOUS in sim1`, 10000)
    expect_length(res.length.pm_bmi_continuous$`length of D$PM_BMI_CONTINUOUS in sim2`, 1)
    expect_equal(res.length.pm_bmi_continuous$`length of D$PM_BMI_CONTINUOUS in sim2`, 10000)
    expect_length(res.length.pm_bmi_continuous$`length of D$PM_BMI_CONTINUOUS in sim3`, 1)
    expect_equal(res.length.pm_bmi_continuous$`length of D$PM_BMI_CONTINUOUS in sim3`, 10000)
    expect_length(res.length.pm_bmi_continuous$`total length of D$PM_BMI_CONTINUOUS in all studies combined`, 1)
    expect_equal(res.length.pm_bmi_continuous$`total length of D$PM_BMI_CONTINUOUS in all studies combined`, 30000)

    res.class.dis_cva <- ds.class(x='D$DIS_CVA')
    expect_length(res.class.dis_cva, 3)
    expect_length(res.class.dis_cva$sim1, 1)
    expect_equal(res.class.dis_cva$sim1, "factor")
    expect_length(res.class.dis_cva$sim2, 1)
    expect_equal(res.class.dis_cva$sim2, "factor")
    expect_length(res.class.dis_cva$sim3, 1)
    expect_equal(res.class.dis_cva$sim3, "factor")

    res.length.dis_cva <- ds.length(x='D$DIS_CVA')
    expect_length(res.length.dis_cva, 4)
    expect_length(res.length.dis_cva$`length of D$DIS_CVA in sim1`, 1)
    expect_equal(res.length.dis_cva$`length of D$DIS_CVA in sim1`, 10000)
    expect_length(res.length.dis_cva$`length of D$DIS_CVA in sim2`, 1)
    expect_equal(res.length.dis_cva$`length of D$DIS_CVA in sim2`, 10000)
    expect_length(res.length.dis_cva$`length of D$DIS_CVA in sim3`, 1)
    expect_equal(res.length.dis_cva$`length of D$DIS_CVA in sim3`, 10000)
    expect_length(res.length.dis_cva$`total length of D$DIS_CVA in all studies combined`, 1)
    expect_equal(res.length.dis_cva$`total length of D$DIS_CVA in all studies combined`, 30000)

    res.class.dis_diab <- ds.class(x='D$DIS_DIAB')
    expect_length(res.class.dis_diab, 3)
    expect_length(res.class.dis_diab$sim1, 1)
    expect_equal(res.class.dis_diab$sim1, "factor")
    expect_length(res.class.dis_diab$sim2, 1)
    expect_equal(res.class.dis_diab$sim2, "factor")
    expect_length(res.class.dis_diab$sim3, 1)
    expect_equal(res.class.dis_diab$sim3, "factor")

    res.length.dis_diab <- ds.length(x='D$DIS_DIAB')
    expect_length(res.length.dis_diab, 4)
    expect_length(res.length.dis_diab$`length of D$DIS_DIAB in sim1`, 1)
    expect_equal(res.length.dis_diab$`length of D$DIS_DIAB in sim1`, 10000)
    expect_length(res.length.dis_diab$`length of D$DIS_DIAB in sim2`, 1)
    expect_equal(res.length.dis_diab$`length of D$DIS_DIAB in sim2`, 10000)
    expect_length(res.length.dis_diab$`length of D$DIS_DIAB in sim3`, 1)
    expect_equal(res.length.dis_diab$`length of D$DIS_DIAB in sim3`, 10000)
    expect_length(res.length.dis_diab$`total length of D$DIS_DIAB in all studies combined`, 1)
    expect_equal(res.length.dis_diab$`total length of D$DIS_DIAB in all studies combined`, 30000)

    res.class.dis_ami <- ds.class(x='D$DIS_AMI')
    expect_length(res.class.dis_ami, 3)
    expect_length(res.class.dis_ami$sim1, 1)
    expect_equal(res.class.dis_ami$sim1, "factor")
    expect_length(res.class.dis_ami$sim2, 1)
    expect_equal(res.class.dis_ami$sim2, "factor")
    expect_length(res.class.dis_ami$sim3, 1)
    expect_equal(res.class.dis_ami$sim3, "factor")

    res.length.dis_ami <- ds.length(x='D$DIS_AMI')
    expect_length(res.length.dis_ami, 4)
    expect_length(res.length.dis_ami$`length of D$DIS_AMI in sim1`, 1)
    expect_equal(res.length.dis_ami$`length of D$DIS_AMI in sim1`, 10000)
    expect_length(res.length.dis_ami$`length of D$DIS_AMI in sim2`, 1)
    expect_equal(res.length.dis_ami$`length of D$DIS_AMI in sim2`, 10000)
    expect_length(res.length.dis_ami$`length of D$DIS_AMI in sim3`, 1)
    expect_equal(res.length.dis_ami$`length of D$DIS_AMI in sim3`, 10000)
    expect_length(res.length.dis_ami$`total length of D$DIS_AMI in all studies combined`, 1)
    expect_equal(res.length.dis_ami$`total length of D$DIS_AMI in all studies combined`, 30000)

    res.class.gender <- ds.class(x='D$GENDER')
    expect_length(res.class.gender, 3)
    expect_length(res.class.gender$sim1, 1)
    expect_equal(res.class.gender$sim1, "factor")
    expect_length(res.class.gender$sim2, 1)
    expect_equal(res.class.gender$sim2, "factor")
    expect_length(res.class.gender$sim3, 1)
    expect_equal(res.class.gender$sim3, "factor")

    res.length.gender <- ds.length(x='D$GENDER')
    expect_length(res.length.gender, 4)
    expect_length(res.length.gender$`length of D$GENDER in sim1`, 1)
    expect_equal(res.length.gender$`length of D$GENDER in sim1`, 10000)
    expect_length(res.length.gender$`length of D$GENDER in sim2`, 1)
    expect_equal(res.length.gender$`length of D$GENDER in sim2`, 10000)
    expect_length(res.length.gender$`length of D$GENDER in sim3`, 1)
    expect_equal(res.length.gender$`length of D$GENDER in sim3`, 10000)
    expect_length(res.length.gender$`total length of D$GENDER in all studies combined`, 1)
    expect_equal(res.length.gender$`total length of D$GENDER in all studies combined`, 30000)

    res.class.pm_bmi_categorical <- ds.class(x='D$PM_BMI_CATEGORICAL')
    expect_length(res.class.pm_bmi_categorical, 3)
    expect_length(res.class.pm_bmi_categorical$sim1, 1)
    expect_equal(res.class.pm_bmi_categorical$sim1, "factor")
    expect_length(res.class.pm_bmi_categorical$sim2, 1)
    expect_equal(res.class.pm_bmi_categorical$sim2, "factor")
    expect_length(res.class.pm_bmi_categorical$sim3, 1)
    expect_equal(res.class.pm_bmi_categorical$sim3, "factor")

    res.length.pm_bmi_categorical <- ds.length(x='D$PM_BMI_CATEGORICAL')
    expect_length(res.length.pm_bmi_categorical, 4)
    expect_length(res.length.pm_bmi_categorical$`length of D$PM_BMI_CATEGORICAL in sim1`, 1)
    expect_equal(res.length.pm_bmi_categorical$`length of D$PM_BMI_CATEGORICAL in sim1`, 10000)
    expect_length(res.length.pm_bmi_categorical$`length of D$PM_BMI_CATEGORICAL in sim2`, 1)
    expect_equal(res.length.pm_bmi_categorical$`length of D$PM_BMI_CATEGORICAL in sim2`, 10000)
    expect_length(res.length.pm_bmi_categorical$`length of D$PM_BMI_CATEGORICAL in sim3`, 1)
    expect_equal(res.length.pm_bmi_categorical$`length of D$PM_BMI_CATEGORICAL in sim3`, 10000)
    expect_length(res.length.pm_bmi_categorical$`total length of D$PM_BMI_CATEGORICAL in all studies combined`, 1)
    expect_equal(res.length.pm_bmi_categorical$`total length of D$PM_BMI_CATEGORICAL in all studies combined`, 30000)
})

#
# Tear down
#

context("DASIM::datachk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.dasim()

context("DASIM::datachk::done")
