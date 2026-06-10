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

# context("ds.hetcor::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG", "LAB_HDL", "LAB_GLUC_ADJUSTED", "PM_BMI_CONTINUOUS", "DIS_CVA", "MEDI_LPD", "DIS_DIAB", "DIS_AMI", "GENDER", "PM_BMI_CATEGORICAL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.hetcor::smk::test1")
test_that("hetcor", {
    ds.dataFrame(c("D$LAB_TSC", "D$LAB_TRIG", "D$LAB_HDL", "D$LAB_GLUC_ADJUSTED", "D$PM_BMI_CONTINUOUS", "D$DIS_CVA", "D$MEDI_LPD", "D$DIS_DIAB", "D$DIS_AMI", "D$GENDER", "D$PM_BMI_CATEGORICAL"), newobj="df")

    res <- ds.hetcor(data="df", ML=TRUE, std.err=TRUE, bins=4, pd=TRUE, use="complete.obs", datasources=ds.test_env$connections)

    expect_equal(class(res), "list")
    expect_length(res, 3)
    expect_equal(class(res$sim1), "hetcor")
    expect_equal(class(res$sim2), "hetcor")
    expect_equal(class(res$sim3), "hetcor")

    expect_true("matrix" %in% class(res$sim1$correlations))
    expect_true("array" %in% class(res$sim1$correlations))
    expect_true("matrix" %in% class(res$sim2$correlations))
    expect_true("array" %in% class(res$sim2$correlations))
    expect_true("matrix" %in% class(res$sim3$correlations))
    expect_true("array" %in% class(res$sim3$correlations))

    expect_true("matrix" %in% class(res$sim1$type))
    expect_true("array" %in% class(res$sim1$type))
    expect_true("matrix" %in% class(res$sim2$type))
    expect_true("array" %in% class(res$sim2$type))
    expect_true("matrix" %in% class(res$sim3$type))
    expect_true("array" %in% class(res$sim3$type))

    expect_equal(class(res$sim1$NA.method), "character")
    expect_equal(class(res$sim2$NA.method), "character")
    expect_equal(class(res$sim3$NA.method), "character")

    expect_equal(class(res$sim1$ML), "logical")
    expect_equal(class(res$sim2$ML), "logical")
    expect_equal(class(res$sim3$ML), "logical")

    expect_true("matrix" %in% class(res$sim1$std.errors))
    expect_true("array" %in% class(res$sim1$std.errors))
    expect_true("matrix" %in% class(res$sim2$std.errors))
    expect_true("array" %in% class(res$sim2$std.errors))
    expect_true("matrix" %in% class(res$sim3$std.errors))
    expect_true("array" %in% class(res$sim3$std.errors))

    expect_equal(class(res$sim1$n), "integer")
    expect_equal(class(res$sim2$n), "integer")
    expect_equal(class(res$sim3$n), "integer")

    expect_true("matrix" %in% class(res$sim1$tests))
    expect_true("array" %in% class(res$sim1$tests))
    expect_true("matrix" %in% class(res$sim2$tests))
    expect_true("array" %in% class(res$sim2$tests))
    expect_true("matrix" %in% class(res$sim3$tests))
    expect_true("array" %in% class(res$sim3$tests))

    expect_equal(dim(res$sim1$correlations)[1], 11)
    expect_equal(dim(res$sim1$correlations)[2], 11)
    expect_equal(dim(res$sim2$correlations)[1], 11)
    expect_equal(dim(res$sim2$correlations)[2], 11)
    expect_equal(dim(res$sim3$correlations)[1], 11)
    expect_equal(dim(res$sim3$correlations)[2], 11)

    expect_equal(res$sim1$correlations[1,1], 1.000000000, tolerance=0.00001)
    expect_equal(res$sim1$correlations[2,1], 0.087066121, tolerance=0.00001)
    expect_equal(res$sim1$correlations[3,1], -0.176099780, tolerance=0.00001)
    expect_equal(res$sim1$correlations[4,1], 0.050921402, tolerance=0.00001)
    expect_equal(res$sim1$correlations[5,1], -0.013770365, tolerance=0.00001)
    expect_true(is.na(res$sim1$correlations[6,1]))
    expect_equal(res$sim1$correlations[7,1], 0.049067166, tolerance=0.00001)
    expect_equal(res$sim1$correlations[8,1], 0.041398769, tolerance=0.00001)
    expect_equal(res$sim1$correlations[9,1], 0.366396436, tolerance=0.00001)
    expect_equal(res$sim1$correlations[10,1], -0.025944251, tolerance=0.00001)
    expect_equal(res$sim1$correlations[11,1], -0.004743882, tolerance=0.00001)

    expect_equal(dim(res$sim1$type)[1], 11)
    expect_equal(dim(res$sim1$type)[2], 11)
    expect_equal(dim(res$sim2$type)[1], 11)
    expect_equal(dim(res$sim2$type)[2], 11)
    expect_equal(dim(res$sim3$type)[1], 11)
    expect_equal(dim(res$sim3$type)[2], 11)

    expect_equal(res$sim1$type[1,1], "")
    expect_equal(res$sim1$type[2,1], "Pearson")
    expect_equal(res$sim1$type[3,1], "Pearson")
    expect_equal(res$sim1$type[4,1], "Pearson")
    expect_equal(res$sim1$type[5,1], "Pearson")
    expect_equal(res$sim1$type[6,1], "Polyserial")
    expect_equal(res$sim1$type[7,1], "Polyserial")
    expect_equal(res$sim1$type[8,1], "Polyserial")
    expect_equal(res$sim1$type[9,1], "Polyserial")
    expect_equal(res$sim1$type[10,1], "Polyserial")
    expect_equal(res$sim1$type[11,1], "Polyserial")

    expect_equal(res$sim1$type[1,6], "Polyserial")
    expect_equal(res$sim1$type[2,6], "Polyserial")
    expect_equal(res$sim1$type[3,6], "Polyserial")
    expect_equal(res$sim1$type[4,6], "Polyserial")
    expect_equal(res$sim1$type[5,6], "Polyserial")
    expect_equal(res$sim1$type[6,6], "")
    expect_equal(res$sim1$type[7,6], "Polychoric")
    expect_equal(res$sim1$type[8,6], "Polychoric")
    expect_equal(res$sim1$type[9,6], "Polychoric")
    expect_equal(res$sim1$type[10,6], "Polychoric")
    expect_equal(res$sim1$type[11,6], "Polychoric")

    expect_equal(res$sim1$NA.method, "complete.obs")
    expect_equal(res$sim2$NA.method, "complete.obs")
    expect_equal(res$sim3$NA.method, "complete.obs")

    expect_equal(res$sim1$ML, TRUE)
    expect_equal(res$sim2$ML, TRUE)
    expect_equal(res$sim3$ML, TRUE)

    expect_equal(dim(res$sim1$std.errors)[1], 11)
    expect_equal(dim(res$sim1$std.errors)[2], 11)
    expect_equal(dim(res$sim2$std.errors)[1], 11)
    expect_equal(dim(res$sim2$std.errors)[2], 11)
    expect_equal(dim(res$sim3$std.errors)[1], 11)
    expect_equal(dim(res$sim3$std.errors)[2], 11)

    expect_equal(res$sim1$std.errors[1,1], 0.00000000, tolerance=0.00001)
    expect_equal(res$sim1$std.errors[2,1], 0.02406969, tolerance=0.00001)
    expect_equal(res$sim1$std.errors[3,1], 0.02350198, tolerance=0.00001)
    expect_equal(res$sim1$std.errors[4,1], 0.02419053, tolerance=0.00001)
    expect_equal(res$sim1$std.errors[5,1], 0.02424876, tolerance=0.00001)
    expect_true(is.na(res$sim1$std.errors[6,1]))
    expect_equal(res$sim1$std.errors[7,1], 0.07335154, tolerance=0.00001)
    expect_equal(res$sim1$std.errors[8,1], 0.07970509, tolerance=0.00001)
    expect_equal(res$sim1$std.errors[9,1], 0.19216465, tolerance=0.00001)
    expect_equal(res$sim1$std.errors[10,1], 0.03037314, tolerance=0.00001)
    expect_equal(res$sim1$std.errors[11,1], 0.02699771, tolerance=0.00001)

    expect_equal(dim(res$sim1$tests)[1], 11)
    expect_equal(dim(res$sim1$tests)[2], 11)
    expect_equal(dim(res$sim2$tests)[1], 11)
    expect_equal(dim(res$sim2$tests)[2], 11)
    expect_equal(dim(res$sim3$tests)[1], 11)
    expect_equal(dim(res$sim3$tests)[2], 11)

    expect_equal(res$sim1$tests[1,1], 0.00000000, tolerance=0.00001)
    expect_equal(res$sim1$tests[2,1], 0.76954489, tolerance=0.00001)
    expect_equal(res$sim1$tests[3,1], 0.34894923, tolerance=0.00001)
    expect_equal(res$sim1$tests[4,1], 0.03778798, tolerance=0.00001)
    expect_equal(res$sim1$tests[5,1], 0.97371162, tolerance=0.00001)
    expect_true(is.na(res$sim1$tests[6,1]))
    expect_equal(res$sim1$tests[7,1], 0.88336350, tolerance=0.00001)
    expect_equal(res$sim1$tests[8,1], 0.72244994, tolerance=0.00001)
    expect_equal(res$sim1$tests[9,1], 0.85611378, tolerance=0.00001)
    expect_equal(res$sim1$tests[10,1], 0.14265211, tolerance=0.00001)
    expect_equal(res$sim1$tests[11,1], 0.95287762, tolerance=0.00001)
})

#
# Done
#

# context("ds.hetcor::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "df"))
})

disconnect.studies.dataset.cnsim()

# context("ds.hetcor::smk::done")
