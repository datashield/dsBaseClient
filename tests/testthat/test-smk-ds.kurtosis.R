#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

# context("ds.kurtosis::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG", "LAB_HDL", "LAB_GLUC_ADJUSTED", "PM_BMI_CONTINUOUS"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# Method 1

# context("ds.kurtosis::smk::method 1::split")
test_that("simple kurtosis, method 1, split, on LAB_TSC", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_TSC', method = 1, type='split')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("0.171744367103707"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[2]), as.double("0.574419687874713"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[3]), as.double("0.674414218958169"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 3)
    expect_equal(kurtosis.res$Nvalid[1], "1807")
    expect_equal(kurtosis.res$Nvalid[2], "2539")
    expect_equal(kurtosis.res$Nvalid[3], "3479")
    expect_length(kurtosis.res$ValidityMessage, 3)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 1, split, on LAB_TRIG", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_TRIG', method = 1, type='split')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("1.76749454806425"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[2]), as.double("1.04629523942343"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[3]), as.double("1.02128294035219"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 3)
    expect_equal(kurtosis.res$Nvalid[1], "1801")
    expect_equal(kurtosis.res$Nvalid[2], "2526")
    expect_equal(kurtosis.res$Nvalid[3], "3473")
    expect_length(kurtosis.res$ValidityMessage, 3)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 1, split, on LAB_HDL", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_HDL', method = 1, type='split')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("0.290702025953629"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[2]), as.double("0.494573359163136"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[3]), as.double("0.549162800210091"), tolerance = ds.test_env$low_tolerance)
    expect_length(kurtosis.res$Nvalid, 3)
    expect_equal(kurtosis.res$Nvalid[1], "1803")
    expect_equal(kurtosis.res$Nvalid[2], "2533")
    expect_equal(kurtosis.res$Nvalid[3], "3473")
    expect_length(kurtosis.res$ValidityMessage, 3)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 1, split, on LAB_GLUC_ADJUSTED", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_GLUC_ADJUSTED', method = 1, type='split')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("4.32162963839166"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[2]), as.double("4.38468288434594"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[3]), as.double("3.72493030465797"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 3)
    expect_equal(kurtosis.res$Nvalid[1], "1822")
    expect_equal(kurtosis.res$Nvalid[2], "2583")
    expect_equal(kurtosis.res$Nvalid[3], "3519")
    expect_length(kurtosis.res$ValidityMessage, 3)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 1, split, on PM_BMI_CONTINUOUS", {
    kurtosis.res <- ds.kurtosis(x = 'D$PM_BMI_CONTINUOUS', method = 1, type='split')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("0.671416534303503"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[2]), as.double("0.251325359087079"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[3]), as.double("0.187132199973004"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 3)
    expect_equal(kurtosis.res$Nvalid[1], "2066")
    expect_equal(kurtosis.res$Nvalid[2], "2938")
    expect_equal(kurtosis.res$Nvalid[3], "3923")
    expect_length(kurtosis.res$ValidityMessage, 3)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[3], "VALID ANALYSIS")
})

# context("ds.kurtosis::smk::method 1::combine")
test_that("simple kurtosis, combine, on LAB_TSC", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_TSC', method = 1, type='combine')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("0.515598613390042"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 1)
    expect_equal(kurtosis.res$Nvalid[1], "7825")
    expect_length(kurtosis.res$ValidityMessage, 1)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 1, combine, on LAB_TRIG", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_TRIG', method = 1, type='combine')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("1.21679529801477"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 1)
    expect_equal(kurtosis.res$Nvalid[1], "7800")
    expect_length(kurtosis.res$ValidityMessage, 1)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 1, combine, on LAB_HDL", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_HDL', method = 1, type='combine')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("0.472661436116919"), tolerance = ds.test_env$low_tolerance)
    expect_length(kurtosis.res$Nvalid, 1)
    expect_equal(kurtosis.res$Nvalid[1], "7809")
    expect_length(kurtosis.res$ValidityMessage, 1)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 1, combine, on LAB_GLUC_ADJUSTED", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_GLUC_ADJUSTED', method = 1, type='combine')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("4.08935226175995"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 1)
    expect_equal(kurtosis.res$Nvalid[1], "7924")
    expect_length(kurtosis.res$ValidityMessage, 1)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 1, combine, on PM_BMI_CONTINUOUS", {
    kurtosis.res <- ds.kurtosis(x = 'D$PM_BMI_CONTINUOUS', method = 1, type='combine')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("0.335021586102938"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 1)
    expect_equal(kurtosis.res$Nvalid[1], "8927")
    expect_length(kurtosis.res$ValidityMessage, 1)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
})

# context("ds.kurtosis::smk::method 1::both")
test_that("simple kurtosis, both, on LAB_TSC", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_TSC', method = 1, type='both')

    expect_equal(class(kurtosis.res), "list")

    expect_length(kurtosis.res, 2)
    expect_length(kurtosis.res$Kurtosis.by.Study, 3)
    expect_length(kurtosis.res$Kurtosis.by.Study$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[1]), as.double("0.171744367103707"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[2]), as.double("0.574419687874713"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[3]), as.double("0.674414218958169"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Kurtosis.by.Study$Nvalid, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[1], "1807")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[2], "2539")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[3], "3479")
    expect_length(kurtosis.res$Kurtosis.by.Study$ValidityMessage, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(kurtosis.res$Global.Kurtosis, 3)
    expect_length(kurtosis.res$Global.Kurtosis$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Global.Kurtosis$Kurtosis[1]), as.double("0.515598613390042"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Global.Kurtosis$Nvalid, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$Nvalid[1], "7825")
    expect_length(kurtosis.res$Global.Kurtosis$ValidityMessage, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 1, both, on LAB_TRIG", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_TRIG', method = 1, type='both')

    expect_equal(class(kurtosis.res), "list")

    expect_length(kurtosis.res, 2)
    expect_length(kurtosis.res$Kurtosis.by.Study, 3)
    expect_length(kurtosis.res$Kurtosis.by.Study$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[1]), as.double("1.76749454806425"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[2]), as.double("1.04629523942343"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[3]), as.double("1.02128294035219"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Kurtosis.by.Study$Nvalid, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[1], "1801")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[2], "2526")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[3], "3473")
    expect_length(kurtosis.res$Kurtosis.by.Study$ValidityMessage, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(kurtosis.res$Global.Kurtosis, 3)
    expect_length(kurtosis.res$Global.Kurtosis$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Global.Kurtosis$Kurtosis[1]), as.double("1.21679529801477"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Global.Kurtosis$Nvalid, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$Nvalid[1], "7800")
    expect_length(kurtosis.res$Global.Kurtosis$ValidityMessage, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 1, both, on LAB_HDL", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_HDL', method = 1, type='both')

    expect_equal(class(kurtosis.res), "list")

    expect_length(kurtosis.res, 2)
    expect_length(kurtosis.res$Kurtosis.by.Study, 3)
    expect_length(kurtosis.res$Kurtosis.by.Study$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[1]), as.double("0.290702025953629"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[2]), as.double("0.494573359163136"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[3]), as.double("0.549162800210091"), tolerance = ds.test_env$low_tolerance)
    expect_length(kurtosis.res$Kurtosis.by.Study$Nvalid, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[1], "1803")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[2], "2533")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[3], "3473")
    expect_length(kurtosis.res$Kurtosis.by.Study$ValidityMessage, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(kurtosis.res$Global.Kurtosis, 3)
    expect_length(kurtosis.res$Global.Kurtosis$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Global.Kurtosis$Kurtosis[1]), as.double("0.472661436116919"), tolerance = ds.test_env$low_tolerance)
    expect_length(kurtosis.res$Global.Kurtosis$Nvalid, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$Nvalid[1], "7809")
    expect_length(kurtosis.res$Global.Kurtosis$ValidityMessage, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 1, both, on LAB_GLUC_ADJUSTED", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_GLUC_ADJUSTED', method = 1, type='both')

    expect_equal(class(kurtosis.res), "list")

    expect_length(kurtosis.res, 2)
    expect_length(kurtosis.res$Kurtosis.by.Study, 3)
    expect_length(kurtosis.res$Kurtosis.by.Study$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[1]), as.double("4.32162963839166"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[2]), as.double("4.38468288434594"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[3]), as.double("3.72493030465797"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Kurtosis.by.Study$Nvalid, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[1], "1822")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[2], "2583")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[3], "3519")
    expect_length(kurtosis.res$Kurtosis.by.Study$ValidityMessage, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(kurtosis.res$Global.Kurtosis, 3)
    expect_length(kurtosis.res$Global.Kurtosis$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Global.Kurtosis$Kurtosis[1]), as.double("4.08935226175995"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Global.Kurtosis$Nvalid, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$Nvalid[1], "7924")
    expect_length(kurtosis.res$Global.Kurtosis$ValidityMessage, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 1, both, on PM_BMI_CONTINUOUS", {
    kurtosis.res <- ds.kurtosis(x = 'D$PM_BMI_CONTINUOUS', method = 1, type='both')

    expect_equal(class(kurtosis.res), "list")

    expect_length(kurtosis.res, 2)
    expect_length(kurtosis.res$Kurtosis.by.Study, 3)
    expect_length(kurtosis.res$Kurtosis.by.Study$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[1]), as.double("0.671416534303503"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[2]), as.double("0.251325359087079"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[3]), as.double("0.187132199973004"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Kurtosis.by.Study$Nvalid, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[1], "2066")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[2], "2938")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[3], "3923")
    expect_length(kurtosis.res$Kurtosis.by.Study$ValidityMessage, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(kurtosis.res$Global.Kurtosis, 3)
    expect_length(kurtosis.res$Global.Kurtosis$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Global.Kurtosis$Kurtosis[1]), as.double("0.335021586102938"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Global.Kurtosis$Nvalid, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$Nvalid[1], "8927")
    expect_length(kurtosis.res$Global.Kurtosis$ValidityMessage, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$ValidityMessage[1], "VALID ANALYSIS")
})

# Method 2

# context("ds.kurtosis::smk::method 2::split")
test_that("simple kurtosis, method 2, split, on LAB_TSC", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_TSC', method = 2, type='split')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("0.175548320198465"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[2]), as.double("0.577919349817977"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[3]), as.double("0.677111105785997"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 3)
    expect_equal(kurtosis.res$Nvalid[1], "1807")
    expect_equal(kurtosis.res$Nvalid[2], "2539")
    expect_equal(kurtosis.res$Nvalid[3], "3479")
    expect_length(kurtosis.res$ValidityMessage, 3)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 2, split, on LAB_TRIG", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_TRIG', method = 2, type='split')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("1.7757502518397"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[2]), as.double("1.0507483099711"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[3]), as.double("1.02448438876989"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 3)
    expect_equal(kurtosis.res$Nvalid[1], "1801")
    expect_equal(kurtosis.res$Nvalid[2], "2526")
    expect_equal(kurtosis.res$Nvalid[3], "3473")
    expect_length(kurtosis.res$ValidityMessage, 3)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 2, split, on LAB_HDL", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_HDL', method = 2, type='split')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("0.294844984757316"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[2]), as.double("0.497923487075878"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[3]), as.double("0.551683842646188"), tolerance = ds.test_env$low_tolerance)
    expect_length(kurtosis.res$Nvalid, 3)
    expect_equal(kurtosis.res$Nvalid[1], "1803")
    expect_equal(kurtosis.res$Nvalid[2], "2533")
    expect_equal(kurtosis.res$Nvalid[3], "3473")
    expect_length(kurtosis.res$ValidityMessage, 3)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 2, split, on LAB_GLUC_ADJUSTED", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_GLUC_ADJUSTED', method = 2, type='split')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("4.33681301852393"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[2]), as.double("4.39550878961539"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[3]), as.double("3.73193529182726"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 3)
    expect_equal(kurtosis.res$Nvalid[1], "1822")
    expect_equal(kurtosis.res$Nvalid[2], "2583")
    expect_equal(kurtosis.res$Nvalid[3], "3519")
    expect_length(kurtosis.res$ValidityMessage, 3)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 2, split, on PM_BMI_CONTINUOUS", {
    kurtosis.res <- ds.kurtosis(x = 'D$PM_BMI_CONTINUOUS', method = 2, type='split')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("0.675954084252309"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[2]), as.double("0.253798588114679"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[3]), as.double("0.188901928135923"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 3)
    expect_equal(kurtosis.res$Nvalid[1], "2066")
    expect_equal(kurtosis.res$Nvalid[2], "2938")
    expect_equal(kurtosis.res$Nvalid[3], "3923")
    expect_length(kurtosis.res$ValidityMessage, 3)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[3], "VALID ANALYSIS")
})

# context("ds.kurtosis::smk::method 2::combine")
test_that("simple kurtosis, combine, on LAB_TSC", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_TSC', method = 2, type='combine')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("0.516695386307489"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 1)
    expect_equal(kurtosis.res$Nvalid[1], "7825")
    expect_length(kurtosis.res$ValidityMessage, 1)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 2, combine, on LAB_TRIG", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_TRIG', method = 2, type='combine')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("1.21834528057683"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 1)
    expect_equal(kurtosis.res$Nvalid[1], "7800")
    expect_length(kurtosis.res$ValidityMessage, 1)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 2, combine, on LAB_HDL", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_HDL', method = 2, type='combine')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("0.473732952559591"), tolerance = ds.test_env$low_tolerance)
    expect_length(kurtosis.res$Nvalid, 1)
    expect_equal(kurtosis.res$Nvalid[1], "7809")
    expect_length(kurtosis.res$ValidityMessage, 1)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 2, combine, on LAB_GLUC_ADJUSTED", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_GLUC_ADJUSTED', method = 2, type='combine')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("4.09269136885493"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 1)
    expect_equal(kurtosis.res$Nvalid[1], "7924")
    expect_length(kurtosis.res$ValidityMessage, 1)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 2, combine, on PM_BMI_CONTINUOUS", {
    kurtosis.res <- ds.kurtosis(x = 'D$PM_BMI_CONTINUOUS', method = 2, type='combine')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("0.335881726489728"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 1)
    expect_equal(kurtosis.res$Nvalid[1], "8927")
    expect_length(kurtosis.res$ValidityMessage, 1)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
})

# context("ds.kurtosis::smk::method 2::both")
test_that("simple kurtosis, both, on LAB_TSC", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_TSC', method = 2, type='both')

    expect_equal(class(kurtosis.res), "list")

    expect_length(kurtosis.res, 2)
    expect_length(kurtosis.res$Kurtosis.by.Study, 3)
    expect_length(kurtosis.res$Kurtosis.by.Study$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[1]), as.double("0.175548320198465"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[2]), as.double("0.577919349817977"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[3]), as.double("0.677111105785997"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Kurtosis.by.Study$Nvalid, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[1], "1807")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[2], "2539")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[3], "3479")
    expect_length(kurtosis.res$Kurtosis.by.Study$ValidityMessage, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(kurtosis.res$Global.Kurtosis, 3)
    expect_length(kurtosis.res$Global.Kurtosis$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Global.Kurtosis$Kurtosis[1]), as.double("0.516695386307489"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Global.Kurtosis$Nvalid, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$Nvalid[1], "7825")
    expect_length(kurtosis.res$Global.Kurtosis$ValidityMessage, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 2, both, on LAB_TRIG", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_TRIG', method = 2, type='both')

    expect_equal(class(kurtosis.res), "list")

    expect_length(kurtosis.res, 2)
    expect_length(kurtosis.res$Kurtosis.by.Study, 3)
    expect_length(kurtosis.res$Kurtosis.by.Study$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[1]), as.double("1.7757502518397"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[2]), as.double("1.0507483099711"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[3]), as.double("1.02448438876989"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Kurtosis.by.Study$Nvalid, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[1], "1801")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[2], "2526")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[3], "3473")
    expect_length(kurtosis.res$Kurtosis.by.Study$ValidityMessage, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(kurtosis.res$Global.Kurtosis, 3)
    expect_length(kurtosis.res$Global.Kurtosis$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Global.Kurtosis$Kurtosis[1]), as.double("1.21834528057683"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Global.Kurtosis$Nvalid, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$Nvalid[1], "7800")
    expect_length(kurtosis.res$Global.Kurtosis$ValidityMessage, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 2, both, on LAB_HDL", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_HDL', method = 2, type='both')

    expect_equal(class(kurtosis.res), "list")

    expect_length(kurtosis.res, 2)
    expect_length(kurtosis.res$Kurtosis.by.Study, 3)
    expect_length(kurtosis.res$Kurtosis.by.Study$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[1]), as.double("0.294844984757316"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[2]), as.double("0.497923487075878"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[3]), as.double("0.551683842646188"), tolerance = ds.test_env$low_tolerance)
    expect_length(kurtosis.res$Kurtosis.by.Study$Nvalid, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[1], "1803")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[2], "2533")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[3], "3473")
    expect_length(kurtosis.res$Kurtosis.by.Study$ValidityMessage, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(kurtosis.res$Global.Kurtosis, 3)
    expect_length(kurtosis.res$Global.Kurtosis$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Global.Kurtosis$Kurtosis[1]), as.double("0.473732952559591"), tolerance = ds.test_env$low_tolerance)
    expect_length(kurtosis.res$Global.Kurtosis$Nvalid, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$Nvalid[1], "7809")
    expect_length(kurtosis.res$Global.Kurtosis$ValidityMessage, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 2, both, on LAB_GLUC_ADJUSTED", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_GLUC_ADJUSTED', method = 2, type='both')

    expect_equal(class(kurtosis.res), "list")

    expect_length(kurtosis.res, 2)
    expect_length(kurtosis.res$Kurtosis.by.Study, 3)
    expect_length(kurtosis.res$Kurtosis.by.Study$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[1]), as.double("4.33681301852393"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[2]), as.double("4.39550878961539"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[3]), as.double("3.73193529182726"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Kurtosis.by.Study$Nvalid, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[1], "1822")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[2], "2583")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[3], "3519")
    expect_length(kurtosis.res$Kurtosis.by.Study$ValidityMessage, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(kurtosis.res$Global.Kurtosis, 3)
    expect_length(kurtosis.res$Global.Kurtosis$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Global.Kurtosis$Kurtosis[1]), as.double("4.09269136885493"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Global.Kurtosis$Nvalid, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$Nvalid[1], "7924")
    expect_length(kurtosis.res$Global.Kurtosis$ValidityMessage, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 2, both, on PM_BMI_CONTINUOUS", {
    kurtosis.res <- ds.kurtosis(x = 'D$PM_BMI_CONTINUOUS', method = 2, type='both')

    expect_equal(class(kurtosis.res), "list")

    expect_length(kurtosis.res, 2)
    expect_length(kurtosis.res$Kurtosis.by.Study, 3)
    expect_length(kurtosis.res$Kurtosis.by.Study$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[1]), as.double("0.675954084252309"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[2]), as.double("0.253798588114679"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[3]), as.double("0.188901928135923"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Kurtosis.by.Study$Nvalid, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[1], "2066")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[2], "2938")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[3], "3923")
    expect_length(kurtosis.res$Kurtosis.by.Study$ValidityMessage, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(kurtosis.res$Global.Kurtosis, 3)
    expect_length(kurtosis.res$Global.Kurtosis$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Global.Kurtosis$Kurtosis[1]), as.double("0.335881726489728"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Global.Kurtosis$Nvalid, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$Nvalid[1], "8927")
    expect_length(kurtosis.res$Global.Kurtosis$ValidityMessage, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$ValidityMessage[1], "VALID ANALYSIS")
})

# Method 3

# context("ds.kurtosis::smk::method 3::split")
test_that("simple kurtosis, method 3, split, on LAB_TSC", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_TSC', method = 3, type='split')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("0.16823483003675"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[2]), as.double("0.571604630147399"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[3]), as.double("0.672302183238634"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 3)
    expect_equal(kurtosis.res$Nvalid[1], "1807")
    expect_equal(kurtosis.res$Nvalid[2], "2539")
    expect_equal(kurtosis.res$Nvalid[3], "3479")
    expect_length(kurtosis.res$ValidityMessage, 3)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 3, split, on LAB_TRIG", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_TRIG', method = 3, type='split')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("1.76220174297892"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[2]), as.double("1.04309215604256"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[3]), as.double("1.01896753349628"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 3)
    expect_equal(kurtosis.res$Nvalid[1], "1801")
    expect_equal(kurtosis.res$Nvalid[2], "2526")
    expect_equal(kurtosis.res$Nvalid[3], "3473")
    expect_length(kurtosis.res$ValidityMessage, 3)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 3, split, on LAB_HDL", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_HDL', method = 3, type='split')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("0.287052786394011"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[2]), as.double("0.491814667058934"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[3]), as.double("0.54711923451172"), tolerance = ds.test_env$low_tolerance)
    expect_length(kurtosis.res$Nvalid, 3)
    expect_equal(kurtosis.res$Nvalid[1], "1803")
    expect_equal(kurtosis.res$Nvalid[2], "2533")
    expect_equal(kurtosis.res$Nvalid[3], "3473")
    expect_length(kurtosis.res$ValidityMessage, 3)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 3, split, on LAB_GLUC_ADJUSTED", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_GLUC_ADJUSTED', method = 3, type='split')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("4.31359492883116"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[2]), as.double("4.37896607954034"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[3]), as.double("3.72110877877708"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 3)
    expect_equal(kurtosis.res$Nvalid[1], "1822")
    expect_equal(kurtosis.res$Nvalid[2], "2583")
    expect_equal(kurtosis.res$Nvalid[3], "3519")
    expect_length(kurtosis.res$ValidityMessage, 3)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 3, split, on PM_BMI_CONTINUOUS", {
    kurtosis.res <- ds.kurtosis(x = 'D$PM_BMI_CONTINUOUS', method = 3, type='split')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("0.667863264214689"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[2]), as.double("0.249112444154344"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis[3]), as.double("0.185507562711274"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 3)
    expect_equal(kurtosis.res$Nvalid[1], "2066")
    expect_equal(kurtosis.res$Nvalid[2], "2938")
    expect_equal(kurtosis.res$Nvalid[3], "3923")
    expect_length(kurtosis.res$ValidityMessage, 3)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$ValidityMessage[3], "VALID ANALYSIS")
})

# context("ds.kurtosis::smk::method 3::combine")
test_that("simple kurtosis, combine, on LAB_TSC", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_TSC', method = 3, type='combine')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("0.514700115249594"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 1)
    expect_equal(kurtosis.res$Nvalid[1], "7825")
    expect_length(kurtosis.res$ValidityMessage, 1)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 3, combine, on LAB_TRIG", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_TRIG', method = 3, type='combine')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("1.21571413776076"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 1)
    expect_equal(kurtosis.res$Nvalid[1], "7800")
    expect_length(kurtosis.res$ValidityMessage, 1)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 3, combine, on LAB_HDL", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_HDL', method = 3, type='combine')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("0.47177209328527"), tolerance = ds.test_env$low_tolerance)
    expect_length(kurtosis.res$Nvalid, 1)
    expect_equal(kurtosis.res$Nvalid[1], "7809")
    expect_length(kurtosis.res$ValidityMessage, 1)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 3, combine, on LAB_GLUC_ADJUSTED", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_GLUC_ADJUSTED', method = 3, type='combine')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("4.0875630379014"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 1)
    expect_equal(kurtosis.res$Nvalid[1], "7924")
    expect_length(kurtosis.res$ValidityMessage, 1)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 3, combine, on PM_BMI_CONTINUOUS", {
    kurtosis.res <- ds.kurtosis(x = 'D$PM_BMI_CONTINUOUS', method = 3, type='combine')

    expect_equal(class(kurtosis.res), "data.frame")

    expect_length(kurtosis.res, 3)
    expect_length(kurtosis.res$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Kurtosis[1]), as.double("0.334274451613855"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Nvalid, 1)
    expect_equal(kurtosis.res$Nvalid[1], "8927")
    expect_length(kurtosis.res$ValidityMessage, 1)
    expect_equal(kurtosis.res$ValidityMessage[1], "VALID ANALYSIS")
})

# context("ds.kurtosis::smk::method 3::both")
test_that("simple kurtosis, both, on LAB_TSC", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_TSC', method = 3, type='both')

    expect_equal(class(kurtosis.res), "list")

    expect_length(kurtosis.res, 2)
    expect_length(kurtosis.res$Kurtosis.by.Study, 3)
    expect_length(kurtosis.res$Kurtosis.by.Study$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[1]), as.double("0.16823483003675"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[2]), as.double("0.571604630147399"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[3]), as.double("0.672302183238634"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Kurtosis.by.Study$Nvalid, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[1], "1807")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[2], "2539")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[3], "3479")
    expect_length(kurtosis.res$Kurtosis.by.Study$ValidityMessage, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(kurtosis.res$Global.Kurtosis, 3)
    expect_length(kurtosis.res$Global.Kurtosis$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Global.Kurtosis$Kurtosis[1]), as.double("0.514700115249594"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Global.Kurtosis$Nvalid, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$Nvalid[1], "7825")
    expect_length(kurtosis.res$Global.Kurtosis$ValidityMessage, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 3, both, on LAB_TRIG", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_TRIG', method = 3, type='both')

    expect_equal(class(kurtosis.res), "list")

    expect_length(kurtosis.res, 2)
    expect_length(kurtosis.res$Kurtosis.by.Study, 3)
    expect_length(kurtosis.res$Kurtosis.by.Study$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[1]), as.double("1.76220174297892"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[2]), as.double("1.04309215604256"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[3]), as.double("1.01896753349628"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Kurtosis.by.Study$Nvalid, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[1], "1801")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[2], "2526")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[3], "3473")
    expect_length(kurtosis.res$Kurtosis.by.Study$ValidityMessage, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(kurtosis.res$Global.Kurtosis, 3)
    expect_length(kurtosis.res$Global.Kurtosis$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Global.Kurtosis$Kurtosis[1]), as.double("1.21571413776076"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Global.Kurtosis$Nvalid, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$Nvalid[1], "7800")
    expect_length(kurtosis.res$Global.Kurtosis$ValidityMessage, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 3, both, on LAB_HDL", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_HDL', method = 3, type='both')

    expect_equal(class(kurtosis.res), "list")

    expect_length(kurtosis.res, 2)
    expect_length(kurtosis.res$Kurtosis.by.Study, 3)
    expect_length(kurtosis.res$Kurtosis.by.Study$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[1]), as.double("0.287052786394011"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[2]), as.double("0.491814667058934"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[3]), as.double("0.54711923451172"), tolerance = ds.test_env$low_tolerance)
    expect_length(kurtosis.res$Kurtosis.by.Study$Nvalid, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[1], "1803")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[2], "2533")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[3], "3473")
    expect_length(kurtosis.res$Kurtosis.by.Study$ValidityMessage, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(kurtosis.res$Global.Kurtosis, 3)
    expect_length(kurtosis.res$Global.Kurtosis$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Global.Kurtosis$Kurtosis[1]), as.double("0.47177209328527"), tolerance = ds.test_env$low_tolerance)
    expect_length(kurtosis.res$Global.Kurtosis$Nvalid, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$Nvalid[1], "7809")
    expect_length(kurtosis.res$Global.Kurtosis$ValidityMessage, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 3, both, on LAB_GLUC_ADJUSTED", {
    kurtosis.res <- ds.kurtosis(x = 'D$LAB_GLUC_ADJUSTED', method = 3, type='both')

    expect_equal(class(kurtosis.res), "list")

    expect_length(kurtosis.res, 2)
    expect_length(kurtosis.res$Kurtosis.by.Study, 3)
    expect_length(kurtosis.res$Kurtosis.by.Study$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[1]), as.double("4.31359492883116"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[2]), as.double("4.37896607954034"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[3]), as.double("3.72110877877708"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Kurtosis.by.Study$Nvalid, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[1], "1822")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[2], "2583")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[3], "3519")
    expect_length(kurtosis.res$Kurtosis.by.Study$ValidityMessage, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(kurtosis.res$Global.Kurtosis, 3)
    expect_length(kurtosis.res$Global.Kurtosis$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Global.Kurtosis$Kurtosis[1]), as.double("4.0875630379014"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Global.Kurtosis$Nvalid, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$Nvalid[1], "7924")
    expect_length(kurtosis.res$Global.Kurtosis$ValidityMessage, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple kurtosis, method 3, both, on PM_BMI_CONTINUOUS", {
    kurtosis.res <- ds.kurtosis(x = 'D$PM_BMI_CONTINUOUS', method = 3, type='both')

    expect_equal(class(kurtosis.res), "list")

    expect_length(kurtosis.res, 2)
    expect_length(kurtosis.res$Kurtosis.by.Study, 3)
    expect_length(kurtosis.res$Kurtosis.by.Study$Kurtosis, 3)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[1]), as.double("0.667863264214689"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[2]), as.double("0.249112444154344"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(kurtosis.res$Kurtosis.by.Study$Kurtosis[3]), as.double("0.185507562711274"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Kurtosis.by.Study$Nvalid, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[1], "2066")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[2], "2938")
    expect_equal(kurtosis.res$Kurtosis.by.Study$Nvalid[3], "3923")
    expect_length(kurtosis.res$Kurtosis.by.Study$ValidityMessage, 3)
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(kurtosis.res$Kurtosis.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(kurtosis.res$Global.Kurtosis, 3)
    expect_length(kurtosis.res$Global.Kurtosis$Kurtosis, 1)
    expect_equal(as.double(kurtosis.res$Global.Kurtosis$Kurtosis[1]), as.double("0.334274451613855"), tolerance = ds.test_env$tolerance)
    expect_length(kurtosis.res$Global.Kurtosis$Nvalid, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$Nvalid[1], "8927")
    expect_length(kurtosis.res$Global.Kurtosis$ValidityMessage, 1)
    expect_equal(kurtosis.res$Global.Kurtosis$ValidityMessage[1], "VALID ANALYSIS")
})

#
# Done
#

# context("ds.kurtosis::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.kurtosis::smk::done")
