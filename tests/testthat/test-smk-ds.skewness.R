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

# context("ds.skewness::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG", "LAB_HDL", "LAB_GLUC_ADJUSTED", "PM_BMI_CONTINUOUS"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# Method 1

# context("ds.skewness::smk::method 1::split")
test_that("simple skewness, method 1, split, on LAB_TSC", {
    skewness.res <- ds.skewness(x = 'D$LAB_TSC', method = 1, type='split')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("0.188034458112999"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[2]), as.double("0.145513907236103"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[3]), as.double("0.352576848495665"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 3)
    expect_equal(skewness.res$Nvalid[1], "1807")
    expect_equal(skewness.res$Nvalid[2], "2539")
    expect_equal(skewness.res$Nvalid[3], "3479")
    expect_length(skewness.res$ValidityMessage, 3)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple skewness, method 1, split, on LAB_TRIG", {
    skewness.res <- ds.skewness(x = 'D$LAB_TRIG', method = 1, type='split')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("0.32820558565826"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[2]), as.double("0.220887697425414"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[3]), as.double("0.105433229814455"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 3)
    expect_equal(skewness.res$Nvalid[1], "1801")
    expect_equal(skewness.res$Nvalid[2], "2526")
    expect_equal(skewness.res$Nvalid[3], "3473")
    expect_length(skewness.res$ValidityMessage, 3)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple skewness, method 1, split, on LAB_HDL", {
    skewness.res <- ds.skewness(x = 'D$LAB_HDL', method = 1, type='split')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("-0.257771315950979"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(skewness.res$Skewness[2]), as.double("-0.206165733408786"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(skewness.res$Skewness[3]), as.double("-0.322356008145192"), tolerance = ds.test_env$low_tolerance)
    expect_length(skewness.res$Nvalid, 3)
    expect_equal(skewness.res$Nvalid[1], "1803")
    expect_equal(skewness.res$Nvalid[2], "2533")
    expect_equal(skewness.res$Nvalid[3], "3473")
    expect_length(skewness.res$ValidityMessage, 3)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple skewness, method 1, split, on LAB_GLUC_ADJUSTED", {
    skewness.res <- ds.skewness(x = 'D$LAB_GLUC_ADJUSTED', method = 1, type='split')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("1.09805173411495"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[2]), as.double("1.11035058496889"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[3]), as.double("0.979387780725702"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 3)
    expect_equal(skewness.res$Nvalid[1], "1822")
    expect_equal(skewness.res$Nvalid[2], "2583")
    expect_equal(skewness.res$Nvalid[3], "3519")
    expect_length(skewness.res$ValidityMessage, 3)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple skewness, method 1, split, on PM_BMI_CONTINUOUS", {
    skewness.res <- ds.skewness(x = 'D$PM_BMI_CONTINUOUS', method = 1, type='split')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("0.211695333017453"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[2]), as.double("0.0914245797002757"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[3]), as.double("0.031209342768676"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 3)
    expect_equal(skewness.res$Nvalid[1], "2066")
    expect_equal(skewness.res$Nvalid[2], "2938")
    expect_equal(skewness.res$Nvalid[3], "3923")
    expect_length(skewness.res$ValidityMessage, 3)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[3], "VALID ANALYSIS")
})

# context("ds.skewness::smk::method 1::combine")
test_that("simple skewness, combine, on LAB_TSC", {
    skewness.res <- ds.skewness(x = 'D$LAB_TSC', method = 1, type='combine')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 1)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("0.246567206666354"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 1)
    expect_equal(skewness.res$Nvalid[1], "7825")
    expect_length(skewness.res$ValidityMessage, 1)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 1, combine, on LAB_TRIG", {
    skewness.res <- ds.skewness(x = 'D$LAB_TRIG', method = 1, type='combine')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 1)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("0.197724380225568"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 1)
    expect_equal(skewness.res$Nvalid[1], "7800")
    expect_length(skewness.res$ValidityMessage, 1)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 1, combine, on LAB_HDL", {
    skewness.res <- ds.skewness(x = 'D$LAB_HDL', method = 1, type='combine')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 1)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("-0.269960357921624"), tolerance = ds.test_env$low_tolerance)
    expect_length(skewness.res$Nvalid, 1)
    expect_equal(skewness.res$Nvalid[1], "7809")
    expect_length(skewness.res$ValidityMessage, 1)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 1, combine, on LAB_GLUC_ADJUSTED", {
    skewness.res <- ds.skewness(x = 'D$LAB_GLUC_ADJUSTED', method = 1, type='combine')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 1)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("1.05104981283397"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 1)
    expect_equal(skewness.res$Nvalid[1], "7924")
    expect_length(skewness.res$ValidityMessage, 1)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 1, combine, on PM_BMI_CONTINUOUS", {
    skewness.res <- ds.skewness(x = 'D$PM_BMI_CONTINUOUS', method = 1, type='combine')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 1)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("0.0960670927351586"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 1)
    expect_equal(skewness.res$Nvalid[1], "8927")
    expect_length(skewness.res$ValidityMessage, 1)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
})

# context("ds.skewness::smk::method 1::both")
test_that("simple skewness, both, on LAB_TSC", {
    skewness.res <- ds.skewness(x = 'D$LAB_TSC', method = 1, type='both')

    expect_equal(class(skewness.res), "list")

    expect_length(skewness.res, 2)
    expect_length(skewness.res$Skewness.by.Study, 3)
    expect_length(skewness.res$Skewness.by.Study$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[1]), as.double("0.188034458112999"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[2]), as.double("0.145513907236103"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[3]), as.double("0.352576848495665"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Skewness.by.Study$Nvalid, 3)
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[1], "1807")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[2], "2539")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[3], "3479")
    expect_length(skewness.res$Skewness.by.Study$ValidityMessage, 3)
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(skewness.res$Global.Skewness, 3)
    expect_length(skewness.res$Global.Skewness$Skewness, 1)
    expect_equal(as.double(skewness.res$Global.Skewness$Skewness[1]), as.double("0.246567206666354"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Global.Skewness$Nvalid, 1)
    expect_equal(skewness.res$Global.Skewness$Nvalid[1], "7825")
    expect_length(skewness.res$Global.Skewness$ValidityMessage, 1)
    expect_equal(skewness.res$Global.Skewness$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 1, both, on LAB_TRIG", {
    skewness.res <- ds.skewness(x = 'D$LAB_TRIG', method = 1, type='both')

    expect_equal(class(skewness.res), "list")

    expect_length(skewness.res, 2)
    expect_length(skewness.res$Skewness.by.Study, 3)
    expect_length(skewness.res$Skewness.by.Study$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[1]), as.double("0.32820558565826"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[2]), as.double("0.220887697425414"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[3]), as.double("0.105433229814455"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Skewness.by.Study$Nvalid, 3)
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[1], "1801")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[2], "2526")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[3], "3473")
    expect_length(skewness.res$Skewness.by.Study$ValidityMessage, 3)
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(skewness.res$Global.Skewness, 3)
    expect_length(skewness.res$Global.Skewness$Skewness, 1)
    expect_equal(as.double(skewness.res$Global.Skewness$Skewness[1]), as.double("0.197724380225568"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Global.Skewness$Nvalid, 1)
    expect_equal(skewness.res$Global.Skewness$Nvalid[1], "7800")
    expect_length(skewness.res$Global.Skewness$ValidityMessage, 1)
    expect_equal(skewness.res$Global.Skewness$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 1, both, on LAB_HDL", {
    skewness.res <- ds.skewness(x = 'D$LAB_HDL', method = 1, type='both')

    expect_equal(class(skewness.res), "list")

    expect_length(skewness.res, 2)
    expect_length(skewness.res$Skewness.by.Study, 3)
    expect_length(skewness.res$Skewness.by.Study$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[1]), as.double("-0.257771315950979"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[2]), as.double("-0.206165733408786"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[3]), as.double("-0.322356008145192"), tolerance = ds.test_env$low_tolerance)
    expect_length(skewness.res$Skewness.by.Study$Nvalid, 3)
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[1], "1803")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[2], "2533")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[3], "3473")
    expect_length(skewness.res$Skewness.by.Study$ValidityMessage, 3)
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(skewness.res$Global.Skewness, 3)
    expect_length(skewness.res$Global.Skewness$Skewness, 1)
    expect_equal(as.double(skewness.res$Global.Skewness$Skewness[1]), as.double("-0.269960357921624"), tolerance = ds.test_env$low_tolerance)
    expect_length(skewness.res$Global.Skewness$Nvalid, 1)
    expect_equal(skewness.res$Global.Skewness$Nvalid[1], "7809")
    expect_length(skewness.res$Global.Skewness$ValidityMessage, 1)
    expect_equal(skewness.res$Global.Skewness$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 1, both, on LAB_GLUC_ADJUSTED", {
    skewness.res <- ds.skewness(x = 'D$LAB_GLUC_ADJUSTED', method = 1, type='both')

    expect_equal(class(skewness.res), "list")

    expect_length(skewness.res, 2)
    expect_length(skewness.res$Skewness.by.Study, 3)
    expect_length(skewness.res$Skewness.by.Study$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[1]), as.double("1.09805173411495"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[2]), as.double("1.11035058496889"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[3]), as.double("0.979387780725702"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Skewness.by.Study$Nvalid, 3)
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[1], "1822")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[2], "2583")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[3], "3519")
    expect_length(skewness.res$Skewness.by.Study$ValidityMessage, 3)
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(skewness.res$Global.Skewness, 3)
    expect_length(skewness.res$Global.Skewness$Skewness, 1)
    expect_equal(as.double(skewness.res$Global.Skewness$Skewness[1]), as.double("1.05104981283397"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Global.Skewness$Nvalid, 1)
    expect_equal(skewness.res$Global.Skewness$Nvalid[1], "7924")
    expect_length(skewness.res$Global.Skewness$ValidityMessage, 1)
    expect_equal(skewness.res$Global.Skewness$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 1, both, on PM_BMI_CONTINUOUS", {
    skewness.res <- ds.skewness(x = 'D$PM_BMI_CONTINUOUS', method = 1, type='both')

    expect_equal(class(skewness.res), "list")

    expect_length(skewness.res, 2)
    expect_length(skewness.res$Skewness.by.Study, 3)
    expect_length(skewness.res$Skewness.by.Study$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[1]), as.double("0.211695333017453"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[2]), as.double("0.0914245797002757"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[3]), as.double("0.031209342768676"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Skewness.by.Study$Nvalid, 3)
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[1], "2066")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[2], "2938")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[3], "3923")
    expect_length(skewness.res$Skewness.by.Study$ValidityMessage, 3)
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(skewness.res$Global.Skewness, 3)
    expect_length(skewness.res$Global.Skewness$Skewness, 1)
    expect_equal(as.double(skewness.res$Global.Skewness$Skewness[1]), as.double("0.0960670927351586"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Global.Skewness$Nvalid, 1)
    expect_equal(skewness.res$Global.Skewness$Nvalid[1], "8927")
    expect_length(skewness.res$Global.Skewness$ValidityMessage, 1)
    expect_equal(skewness.res$Global.Skewness$ValidityMessage[1], "VALID ANALYSIS")
})

# Method 2

# context("ds.skewness::smk::method 2::split")
test_that("simple skewness, method 2, split, on LAB_TSC", {
    skewness.res <- ds.skewness(x = 'D$LAB_TSC', method = 2, type='split')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("0.188190712227239"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[2]), as.double("0.145599939437722"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[3]), as.double("0.352728948755338"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 3)
    expect_equal(skewness.res$Nvalid[1], "1807")
    expect_equal(skewness.res$Nvalid[2], "2539")
    expect_equal(skewness.res$Nvalid[3], "3479")
    expect_length(skewness.res$ValidityMessage, 3)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple skewness, method 2, split, on LAB_TRIG", {
    skewness.res <- ds.skewness(x = 'D$LAB_TRIG', method = 2, type='split')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("0.328479229678695"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[2]), as.double("0.221018965497232"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[3]), as.double("0.10547879191455"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 3)
    expect_equal(skewness.res$Nvalid[1], "1801")
    expect_equal(skewness.res$Nvalid[2], "2526")
    expect_equal(skewness.res$Nvalid[3], "3473")
    expect_length(skewness.res$ValidityMessage, 3)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple skewness, method 2, split, on LAB_HDL", {
    skewness.res <- ds.skewness(x = 'D$LAB_HDL', method = 2, type='split')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("-0.257985996183054"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(skewness.res$Skewness[2]), as.double("-0.206287913742296"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(skewness.res$Skewness[3]), as.double("-0.322495311633619"), tolerance = ds.test_env$low_tolerance)
    expect_length(skewness.res$Nvalid, 3)
    expect_equal(skewness.res$Nvalid[1], "1803")
    expect_equal(skewness.res$Nvalid[2], "2533")
    expect_equal(skewness.res$Nvalid[3], "3473")
    expect_length(skewness.res$ValidityMessage, 3)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple skewness, method 2, split, on LAB_GLUC_ADJUSTED", {
    skewness.res <- ds.skewness(x = 'D$LAB_GLUC_ADJUSTED', method = 2, type='split')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("1.09895668040486"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[2]), as.double("1.11099586669437"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[3]), as.double("0.979805479581792"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 3)
    expect_equal(skewness.res$Nvalid[1], "1822")
    expect_equal(skewness.res$Nvalid[2], "2583")
    expect_equal(skewness.res$Nvalid[3], "3519")
    expect_length(skewness.res$ValidityMessage, 3)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple skewness, method 2, split, on PM_BMI_CONTINUOUS", {
    skewness.res <- ds.skewness(x = 'D$PM_BMI_CONTINUOUS', method = 2, type='split')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("0.21184917516287"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[2]), as.double("0.0914712871182399"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[3]), as.double("0.0312212818198342"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 3)
    expect_equal(skewness.res$Nvalid[1], "2066")
    expect_equal(skewness.res$Nvalid[2], "2938")
    expect_equal(skewness.res$Nvalid[3], "3923")
    expect_length(skewness.res$ValidityMessage, 3)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[3], "VALID ANALYSIS")
})

# context("ds.skewness::smk::method 2::combine")
test_that("simple skewness, combine, on LAB_TSC", {
    skewness.res <- ds.skewness(x = 'D$LAB_TSC', method = 2, type='combine')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 1)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("0.246614483525739"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 1)
    expect_equal(skewness.res$Nvalid[1], "7825")
    expect_length(skewness.res$ValidityMessage, 1)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 2, combine, on LAB_TRIG", {
    skewness.res <- ds.skewness(x = 'D$LAB_TRIG', method = 2, type='combine')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 1)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("0.197762413490697"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 1)
    expect_equal(skewness.res$Nvalid[1], "7800")
    expect_length(skewness.res$ValidityMessage, 1)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 2, combine, on LAB_HDL", {
    skewness.res <- ds.skewness(x = 'D$LAB_HDL', method = 2, type='combine')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 1)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("-0.270012226272502"), tolerance = ds.test_env$low_tolerance)
    expect_length(skewness.res$Nvalid, 1)
    expect_equal(skewness.res$Nvalid[1], "7809")
    expect_length(skewness.res$ValidityMessage, 1)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 2, combine, on LAB_GLUC_ADJUSTED", {
    skewness.res <- ds.skewness(x = 'D$LAB_GLUC_ADJUSTED', method = 2, type='combine')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 1)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("1.05124882294985"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 1)
    expect_equal(skewness.res$Nvalid[1], "7924")
    expect_length(skewness.res$ValidityMessage, 1)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 2, combine, on PM_BMI_CONTINUOUS", {
    skewness.res <- ds.skewness(x = 'D$PM_BMI_CONTINUOUS', method = 2, type='combine')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 1)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("0.0960832383143017"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 1)
    expect_equal(skewness.res$Nvalid[1], "8927")
    expect_length(skewness.res$ValidityMessage, 1)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
})

# context("ds.skewness::smk::method 2::both")
test_that("simple skewness, both, on LAB_TSC", {
    skewness.res <- ds.skewness(x = 'D$LAB_TSC', method = 2, type='both')

    expect_equal(class(skewness.res), "list")

    expect_length(skewness.res, 2)
    expect_length(skewness.res$Skewness.by.Study, 3)
    expect_length(skewness.res$Skewness.by.Study$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[1]), as.double("0.188190712227239"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[2]), as.double("0.145599939437722"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[3]), as.double("0.352728948755338"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Skewness.by.Study$Nvalid, 3)
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[1], "1807")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[2], "2539")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[3], "3479")
    expect_length(skewness.res$Skewness.by.Study$ValidityMessage, 3)
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(skewness.res$Global.Skewness, 3)
    expect_length(skewness.res$Global.Skewness$Skewness, 1)
    expect_equal(as.double(skewness.res$Global.Skewness$Skewness[1]), as.double("0.246614483525739"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Global.Skewness$Nvalid, 1)
    expect_equal(skewness.res$Global.Skewness$Nvalid[1], "7825")
    expect_length(skewness.res$Global.Skewness$ValidityMessage, 1)
    expect_equal(skewness.res$Global.Skewness$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 2, both, on LAB_TRIG", {
    skewness.res <- ds.skewness(x = 'D$LAB_TRIG', method = 2, type='both')

    expect_equal(class(skewness.res), "list")

    expect_length(skewness.res, 2)
    expect_length(skewness.res$Skewness.by.Study, 3)
    expect_length(skewness.res$Skewness.by.Study$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[1]), as.double("0.328479229678695"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[2]), as.double("0.221018965497232"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[3]), as.double("0.10547879191455"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Skewness.by.Study$Nvalid, 3)
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[1], "1801")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[2], "2526")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[3], "3473")
    expect_length(skewness.res$Skewness.by.Study$ValidityMessage, 3)
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(skewness.res$Global.Skewness, 3)
    expect_length(skewness.res$Global.Skewness$Skewness, 1)
    expect_equal(as.double(skewness.res$Global.Skewness$Skewness[1]), as.double("0.197762413490697"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Global.Skewness$Nvalid, 1)
    expect_equal(skewness.res$Global.Skewness$Nvalid[1], "7800")
    expect_length(skewness.res$Global.Skewness$ValidityMessage, 1)
    expect_equal(skewness.res$Global.Skewness$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 2, both, on LAB_HDL", {
    skewness.res <- ds.skewness(x = 'D$LAB_HDL', method = 2, type='both')

    expect_equal(class(skewness.res), "list")

    expect_length(skewness.res, 2)
    expect_length(skewness.res$Skewness.by.Study, 3)
    expect_length(skewness.res$Skewness.by.Study$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[1]), as.double("-0.257985996183054"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[2]), as.double("-0.206287913742296"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[3]), as.double("-0.322495311633619"), tolerance = ds.test_env$low_tolerance)
    expect_length(skewness.res$Skewness.by.Study$Nvalid, 3)
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[1], "1803")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[2], "2533")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[3], "3473")
    expect_length(skewness.res$Skewness.by.Study$ValidityMessage, 3)
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(skewness.res$Global.Skewness, 3)
    expect_length(skewness.res$Global.Skewness$Skewness, 1)
    expect_equal(as.double(skewness.res$Global.Skewness$Skewness[1]), as.double("-0.270012226272502"), tolerance = ds.test_env$low_tolerance)
    expect_length(skewness.res$Global.Skewness$Nvalid, 1)
    expect_equal(skewness.res$Global.Skewness$Nvalid[1], "7809")
    expect_length(skewness.res$Global.Skewness$ValidityMessage, 1)
    expect_equal(skewness.res$Global.Skewness$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 2, both, on LAB_GLUC_ADJUSTED", {
    skewness.res <- ds.skewness(x = 'D$LAB_GLUC_ADJUSTED', method = 2, type='both')

    expect_equal(class(skewness.res), "list")

    expect_length(skewness.res, 2)
    expect_length(skewness.res$Skewness.by.Study, 3)
    expect_length(skewness.res$Skewness.by.Study$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[1]), as.double("1.09895668040486"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[2]), as.double("1.11099586669437"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[3]), as.double("0.979805479581792"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Skewness.by.Study$Nvalid, 3)
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[1], "1822")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[2], "2583")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[3], "3519")
    expect_length(skewness.res$Skewness.by.Study$ValidityMessage, 3)
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(skewness.res$Global.Skewness, 3)
    expect_length(skewness.res$Global.Skewness$Skewness, 1)
    expect_equal(as.double(skewness.res$Global.Skewness$Skewness[1]), as.double("1.05124882294985"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Global.Skewness$Nvalid, 1)
    expect_equal(skewness.res$Global.Skewness$Nvalid[1], "7924")
    expect_length(skewness.res$Global.Skewness$ValidityMessage, 1)
    expect_equal(skewness.res$Global.Skewness$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 2, both, on PM_BMI_CONTINUOUS", {
    skewness.res <- ds.skewness(x = 'D$PM_BMI_CONTINUOUS', method = 2, type='both')

    expect_equal(class(skewness.res), "list")

    expect_length(skewness.res, 2)
    expect_length(skewness.res$Skewness.by.Study, 3)
    expect_length(skewness.res$Skewness.by.Study$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[1]), as.double("0.21184917516287"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[2]), as.double("0.0914712871182399"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[3]), as.double("0.0312212818198342"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Skewness.by.Study$Nvalid, 3)
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[1], "2066")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[2], "2938")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[3], "3923")
    expect_length(skewness.res$Skewness.by.Study$ValidityMessage, 3)
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(skewness.res$Global.Skewness, 3)
    expect_length(skewness.res$Global.Skewness$Skewness, 1)
    expect_equal(as.double(skewness.res$Global.Skewness$Skewness[1]), as.double("0.0960832383143017"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Global.Skewness$Nvalid, 1)
    expect_equal(skewness.res$Global.Skewness$Nvalid[1], "8927")
    expect_length(skewness.res$Global.Skewness$ValidityMessage, 1)
    expect_equal(skewness.res$Global.Skewness$ValidityMessage[1], "VALID ANALYSIS")
})

# Method 3

# context("ds.skewness::smk::method 3::split")
test_that("simple skewness, method 3, split, on LAB_TSC", {
    skewness.res <- ds.skewness(x = 'D$LAB_TSC', method = 3, type='split')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("0.187878391338523"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[2]), as.double("0.145427948446175"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[3]), as.double("0.352424842957634"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 3)
    expect_equal(skewness.res$Nvalid[1], "1807")
    expect_equal(skewness.res$Nvalid[2], "2539")
    expect_equal(skewness.res$Nvalid[3], "3479")
    expect_length(skewness.res$ValidityMessage, 3)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple skewness, method 3, split, on LAB_TRIG", {
    skewness.res <- ds.skewness(x = 'D$LAB_TRIG', method = 3, type='split')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("0.327932270814304"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[2]), as.double("0.220756541941702"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[3]), as.double("0.105387696137537"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 3)
    expect_equal(skewness.res$Nvalid[1], "1801")
    expect_equal(skewness.res$Nvalid[2], "2526")
    expect_equal(skewness.res$Nvalid[3], "3473")
    expect_length(skewness.res$ValidityMessage, 3)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple skewness, method 3, split, on LAB_HDL", {
    skewness.res <- ds.skewness(x = 'D$LAB_HDL', method = 3, type='split')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("-0.257556893679228"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(skewness.res$Skewness[2]), as.double("-0.206043657579281"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(skewness.res$Skewness[3]), as.double("-0.322216791558985"), tolerance = ds.test_env$low_tolerance)
    expect_length(skewness.res$Nvalid, 3)
    expect_equal(skewness.res$Nvalid[1], "1803")
    expect_equal(skewness.res$Nvalid[2], "2533")
    expect_equal(skewness.res$Nvalid[3], "3473")
    expect_length(skewness.res$ValidityMessage, 3)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple skewness, method 3, split, on LAB_GLUC_ADJUSTED", {
    skewness.res <- ds.skewness(x = 'D$LAB_GLUC_ADJUSTED', method = 3, type='split')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("1.09714786387241"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[2]), as.double("1.10970584448638"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[3]), as.double("0.978970339038204"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 3)
    expect_equal(skewness.res$Nvalid[1], "1822")
    expect_equal(skewness.res$Nvalid[2], "2583")
    expect_equal(skewness.res$Nvalid[3], "3519")
    expect_length(skewness.res$ValidityMessage, 3)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[3], "VALID ANALYSIS")
})

test_that("simple skewness, method 3, split, on PM_BMI_CONTINUOUS", {
    skewness.res <- ds.skewness(x = 'D$PM_BMI_CONTINUOUS', method = 3, type='split')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("0.211541652198686"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[2]), as.double("0.0913779067255815"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness[3]), as.double("0.031197410311189"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 3)
    expect_equal(skewness.res$Nvalid[1], "2066")
    expect_equal(skewness.res$Nvalid[2], "2938")
    expect_equal(skewness.res$Nvalid[3], "3923")
    expect_length(skewness.res$ValidityMessage, 3)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$ValidityMessage[3], "VALID ANALYSIS")
})

# context("ds.skewness::smk::method 3::combine")
test_that("simple skewness, combine, on LAB_TSC", {
    skewness.res <- ds.skewness(x = 'D$LAB_TSC', method = 3, type='combine')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 1)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("0.246519942897225"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 1)
    expect_equal(skewness.res$Nvalid[1], "7825")
    expect_length(skewness.res$ValidityMessage, 1)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 3, combine, on LAB_TRIG", {
    skewness.res <- ds.skewness(x = 'D$LAB_TRIG', method = 3, type='combine')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 1)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("0.197686357525035"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 1)
    expect_equal(skewness.res$Nvalid[1], "7800")
    expect_length(skewness.res$ValidityMessage, 1)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 3, combine, on LAB_HDL", {
    skewness.res <- ds.skewness(x = 'D$LAB_HDL', method = 3, type='combine')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 1)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("-0.269908503961744"), tolerance = ds.test_env$low_tolerance)
    expect_length(skewness.res$Nvalid, 1)
    expect_equal(skewness.res$Nvalid[1], "7809")
    expect_length(skewness.res$ValidityMessage, 1)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 3, combine, on LAB_GLUC_ADJUSTED", {
    skewness.res <- ds.skewness(x = 'D$LAB_GLUC_ADJUSTED', method = 3, type='combine')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 1)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("1.05085085713259"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 1)
    expect_equal(skewness.res$Nvalid[1], "7924")
    expect_length(skewness.res$ValidityMessage, 1)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 3, combine, on PM_BMI_CONTINUOUS", {
    skewness.res <- ds.skewness(x = 'D$PM_BMI_CONTINUOUS', method = 3, type='combine')

    expect_equal(class(skewness.res), "data.frame")

    expect_length(skewness.res, 3)
    expect_length(skewness.res$Skewness, 1)
    expect_equal(as.double(skewness.res$Skewness[1]), as.double("0.0960509510746345"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Nvalid, 1)
    expect_equal(skewness.res$Nvalid[1], "8927")
    expect_length(skewness.res$ValidityMessage, 1)
    expect_equal(skewness.res$ValidityMessage[1], "VALID ANALYSIS")
})

# context("ds.skewness::smk::method 3::both")
test_that("simple skewness, both, on LAB_TSC", {
    skewness.res <- ds.skewness(x = 'D$LAB_TSC', method = 3, type='both')

    expect_equal(class(skewness.res), "list")

    expect_length(skewness.res, 2)
    expect_length(skewness.res$Skewness.by.Study, 3)
    expect_length(skewness.res$Skewness.by.Study$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[1]), as.double("0.187878391338523"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[2]), as.double("0.145427948446175"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[3]), as.double("0.352424842957634"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Skewness.by.Study$Nvalid, 3)
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[1], "1807")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[2], "2539")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[3], "3479")
    expect_length(skewness.res$Skewness.by.Study$ValidityMessage, 3)
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(skewness.res$Global.Skewness, 3)
    expect_length(skewness.res$Global.Skewness$Skewness, 1)
    expect_equal(as.double(skewness.res$Global.Skewness$Skewness[1]), as.double("0.246519942897225"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Global.Skewness$Nvalid, 1)
    expect_equal(skewness.res$Global.Skewness$Nvalid[1], "7825")
    expect_length(skewness.res$Global.Skewness$ValidityMessage, 1)
    expect_equal(skewness.res$Global.Skewness$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 3, both, on LAB_TRIG", {
    skewness.res <- ds.skewness(x = 'D$LAB_TRIG', method = 3, type='both')

    expect_equal(class(skewness.res), "list")

    expect_length(skewness.res, 2)
    expect_length(skewness.res$Skewness.by.Study, 3)
    expect_length(skewness.res$Skewness.by.Study$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[1]), as.double("0.327932270814304"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[2]), as.double("0.220756541941702"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[3]), as.double("0.105387696137537"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Skewness.by.Study$Nvalid, 3)
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[1], "1801")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[2], "2526")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[3], "3473")
    expect_length(skewness.res$Skewness.by.Study$ValidityMessage, 3)
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(skewness.res$Global.Skewness, 3)
    expect_length(skewness.res$Global.Skewness$Skewness, 1)
    expect_equal(as.double(skewness.res$Global.Skewness$Skewness[1]), as.double("0.197686357525035"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Global.Skewness$Nvalid, 1)
    expect_equal(skewness.res$Global.Skewness$Nvalid[1], "7800")
    expect_length(skewness.res$Global.Skewness$ValidityMessage, 1)
    expect_equal(skewness.res$Global.Skewness$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 3, both, on LAB_HDL", {
    skewness.res <- ds.skewness(x = 'D$LAB_HDL', method = 3, type='both')

    expect_equal(class(skewness.res), "list")

    expect_length(skewness.res, 2)
    expect_length(skewness.res$Skewness.by.Study, 3)
    expect_length(skewness.res$Skewness.by.Study$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[1]), as.double("-0.257556893679228"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[2]), as.double("-0.206043657579281"), tolerance = ds.test_env$low_tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[3]), as.double("-0.322216791558985"), tolerance = ds.test_env$low_tolerance)
    expect_length(skewness.res$Skewness.by.Study$Nvalid, 3)
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[1], "1803")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[2], "2533")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[3], "3473")
    expect_length(skewness.res$Skewness.by.Study$ValidityMessage, 3)
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(skewness.res$Global.Skewness, 3)
    expect_length(skewness.res$Global.Skewness$Skewness, 1)
    expect_equal(as.double(skewness.res$Global.Skewness$Skewness[1]), as.double("-0.269908503961744"), tolerance = ds.test_env$low_tolerance)
    expect_length(skewness.res$Global.Skewness$Nvalid, 1)
    expect_equal(skewness.res$Global.Skewness$Nvalid[1], "7809")
    expect_length(skewness.res$Global.Skewness$ValidityMessage, 1)
    expect_equal(skewness.res$Global.Skewness$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 3, both, on LAB_GLUC_ADJUSTED", {
    skewness.res <- ds.skewness(x = 'D$LAB_GLUC_ADJUSTED', method = 3, type='both')

    expect_equal(class(skewness.res), "list")

    expect_length(skewness.res, 2)
    expect_length(skewness.res$Skewness.by.Study, 3)
    expect_length(skewness.res$Skewness.by.Study$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[1]), as.double("1.09714786387241"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[2]), as.double("1.10970584448638"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[3]), as.double("0.978970339038204"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Skewness.by.Study$Nvalid, 3)
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[1], "1822")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[2], "2583")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[3], "3519")
    expect_length(skewness.res$Skewness.by.Study$ValidityMessage, 3)
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(skewness.res$Global.Skewness, 3)
    expect_length(skewness.res$Global.Skewness$Skewness, 1)
    expect_equal(as.double(skewness.res$Global.Skewness$Skewness[1]), as.double("1.05085085713259"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Global.Skewness$Nvalid, 1)
    expect_equal(skewness.res$Global.Skewness$Nvalid[1], "7924")
    expect_length(skewness.res$Global.Skewness$ValidityMessage, 1)
    expect_equal(skewness.res$Global.Skewness$ValidityMessage[1], "VALID ANALYSIS")
})

test_that("simple skewness, method 3, both, on PM_BMI_CONTINUOUS", {
    skewness.res <- ds.skewness(x = 'D$PM_BMI_CONTINUOUS', method = 3, type='both')

    expect_equal(class(skewness.res), "list")

    expect_length(skewness.res, 2)
    expect_length(skewness.res$Skewness.by.Study, 3)
    expect_length(skewness.res$Skewness.by.Study$Skewness, 3)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[1]), as.double("0.211541652198686"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[2]), as.double("0.0913779067255815"), tolerance = ds.test_env$tolerance)
    expect_equal(as.double(skewness.res$Skewness.by.Study$Skewness[3]), as.double("0.031197410311189"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Skewness.by.Study$Nvalid, 3)
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[1], "2066")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[2], "2938")
    expect_equal(skewness.res$Skewness.by.Study$Nvalid[3], "3923")
    expect_length(skewness.res$Skewness.by.Study$ValidityMessage, 3)
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(skewness.res$Skewness.by.Study$ValidityMessage[3], "VALID ANALYSIS")
    expect_length(skewness.res$Global.Skewness, 3)
    expect_length(skewness.res$Global.Skewness$Skewness, 1)
    expect_equal(as.double(skewness.res$Global.Skewness$Skewness[1]), as.double("0.0960509510746345"), tolerance = ds.test_env$tolerance)
    expect_length(skewness.res$Global.Skewness$Nvalid, 1)
    expect_equal(skewness.res$Global.Skewness$Nvalid[1], "8927")
    expect_length(skewness.res$Global.Skewness$ValidityMessage, 1)
    expect_equal(skewness.res$Global.Skewness$ValidityMessage[1], "VALID ANALYSIS")
})

#
# Done
#

# context("ds.skewness::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.skewness::smk::done")
