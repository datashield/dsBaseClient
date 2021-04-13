#-------------------------------------------------------------------------------
# Copyright (c) 2019-2021 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.completeCases::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG", "LAB_HDL", "LAB_GLUC_ADJUSTED", "PM_BMI_CONTINUOUS", "DIS_CVA", "MEDI_LPD", "DIS_DIAB", "DIS_AMI", "GENDER", "PM_BMI_CATEGORICAL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.completeCases::smk::data.frame")
test_that("completeCases data.frame", {
    ds.dataFrame(c("D$LAB_TSC", "D$LAB_TRIG", "D$LAB_HDL", "D$LAB_GLUC_ADJUSTED", "D$PM_BMI_CONTINUOUS", "D$DIS_CVA", "D$MEDI_LPD", "D$DIS_DIAB", "D$DIS_AMI", "D$GENDER", "D$PM_BMI_CATEGORICAL"), newobj="df")

    res.completeCases <- ds.completeCases("df", "df_new")

    expect_length(res.completeCases, 2)
    expect_equal(res.completeCases$is.object.created, "A data object <df_new> has been created in all specified data sources")
    expect_equal(res.completeCases$validity.check, "<df_new> appears valid in all sources")

    res.df.class <- ds.class("df")

    expect_length(res.df.class, 3)
    expect_equal(res.df.class$sim1, "data.frame")
    expect_equal(res.df.class$sim2, "data.frame")
    expect_equal(res.df.class$sim3, "data.frame")

    res.df_new.class <- ds.class("df_new")

    expect_length(res.df_new.class, 3)
    expect_equal(res.df_new.class$sim1, "data.frame")
    expect_equal(res.df_new.class$sim2, "data.frame")
    expect_equal(res.df_new.class$sim3, "data.frame")

    res.df.dim <- ds.dim("df")

    expect_length(res.df.dim, 4)
    expect_length(res.df.dim$`dimensions of df in sim1`, 2)
    expect_equal(res.df.dim$`dimensions of df in sim1`[1], 2163)
    expect_equal(res.df.dim$`dimensions of df in sim1`[2], 11)
    expect_length(res.df.dim$`dimensions of df in sim2`, 2)
    expect_equal(res.df.dim$`dimensions of df in sim2`[1], 3088)
    expect_equal(res.df.dim$`dimensions of df in sim2`[2], 11)
    expect_length(res.df.dim$`dimensions of df in sim3`, 2)
    expect_equal(res.df.dim$`dimensions of df in sim3`[1], 4128)
    expect_equal(res.df.dim$`dimensions of df in sim3`[2], 11)
    expect_length(res.df.dim$`dimensions of df in combined studies`, 2)
    expect_equal(res.df.dim$`dimensions of df in combined studies`[1], 9379)
    expect_equal(res.df.dim$`dimensions of df in combined studies`[2], 11)

    res.df_new.dim <- ds.dim("df_new")

    expect_length(res.df_new.dim, 4)
    expect_length(res.df_new.dim$`dimensions of df_new in sim1`, 2)
    expect_equal(res.df_new.dim$`dimensions of df_new in sim1`[1], 1701)
    expect_equal(res.df_new.dim$`dimensions of df_new in sim1`[2], 11)
    expect_length(res.df_new.dim$`dimensions of df_new in sim2`, 2)
    expect_equal(res.df_new.dim$`dimensions of df_new in sim2`[1], 2395)
    expect_equal(res.df_new.dim$`dimensions of df_new in sim2`[2], 11)
    expect_length(res.df_new.dim$`dimensions of df_new in sim3`, 2)
    expect_equal(res.df_new.dim$`dimensions of df_new in sim3`[1], 3275)
    expect_equal(res.df_new.dim$`dimensions of df_new in sim3`[2], 11)
    expect_length(res.df_new.dim$`dimensions of df_new in combined studies`, 2)
    expect_equal(res.df_new.dim$`dimensions of df_new in combined studies`[1], 7371)
    expect_equal(res.df_new.dim$`dimensions of df_new in combined studies`[2], 11)
})

context("ds.completeCases::smk::matrix")
test_that("completeCases matrix", {
    ds.asDataMatrix("D", newobj="mat")

    res.completeCases <- ds.completeCases("mat", "mat_new")

    expect_length(res.completeCases, 2)
    expect_equal(res.completeCases$is.object.created, "A data object <mat_new> has been created in all specified data sources")
    expect_equal(res.completeCases$validity.check, "<mat_new> appears valid in all sources")

    res.mat.class <- ds.class("mat")

    expect_length(res.mat.class, 3)
    expect_true("matrix" %in% res.mat.class$sim1)
    expect_true("matrix" %in% res.mat.class$sim2)
    expect_true("matrix" %in% res.mat.class$sim3)

    res.mat_new.class <- ds.class("mat_new")

    expect_length(res.mat_new.class, 3)
    expect_length(res.mat_new.class$sim1, 2)
    expect_true("array" %in% res.mat_new.class$sim1)
    expect_true("matrix" %in% res.mat_new.class$sim1)
    expect_length(res.mat_new.class$sim2, 2)
    expect_true("array" %in% res.mat_new.class$sim2)
    expect_true("matrix" %in% res.mat_new.class$sim2)
    expect_length(res.mat_new.class$sim3, 2)
    expect_true("array" %in% res.mat_new.class$sim3)
    expect_true("matrix" %in% res.mat_new.class$sim3)

    res.mat.dim <- ds.dim("mat")

    expect_length(res.mat.dim, 4)
    expect_length(res.mat.dim$`dimensions of mat in sim1`, 2)
    expect_equal(res.mat.dim$`dimensions of mat in sim1`[1], 2163)
    expect_equal(res.mat.dim$`dimensions of mat in sim1`[2], 11)
    expect_length(res.mat.dim$`dimensions of mat in sim2`, 2)
    expect_equal(res.mat.dim$`dimensions of mat in sim2`[1], 3088)
    expect_equal(res.mat.dim$`dimensions of mat in sim2`[2], 11)
    expect_length(res.mat.dim$`dimensions of mat in sim3`, 2)
    expect_equal(res.mat.dim$`dimensions of mat in sim3`[1], 4128)
    expect_equal(res.mat.dim$`dimensions of mat in sim3`[2], 11)
    expect_length(res.mat.dim$`dimensions of mat in combined studies`, 2)
    expect_equal(res.mat.dim$`dimensions of mat in combined studies`[1], 9379)
    expect_equal(res.mat.dim$`dimensions of mat in combined studies`[2], 11)

    res.mat_new.dim <- ds.dim("mat_new")

    expect_length(res.mat_new.dim, 4)
    expect_length(res.mat_new.dim$`dimensions of mat_new in sim1`, 2)
    expect_equal(res.mat_new.dim$`dimensions of mat_new in sim1`[1], 1701)
    expect_equal(res.mat_new.dim$`dimensions of mat_new in sim1`[2], 11)
    expect_length(res.mat_new.dim$`dimensions of mat_new in sim2`, 2)
    expect_equal(res.mat_new.dim$`dimensions of mat_new in sim2`[1], 2395)
    expect_equal(res.mat_new.dim$`dimensions of mat_new in sim2`[2], 11)
    expect_length(res.mat_new.dim$`dimensions of mat_new in sim3`, 2)
    expect_equal(res.mat_new.dim$`dimensions of mat_new in sim3`[1], 3275)
    expect_equal(res.mat_new.dim$`dimensions of mat_new in sim3`[2], 11)
    expect_length(res.mat_new.dim$`dimensions of mat_new in combined studies`, 2)
    expect_equal(res.mat_new.dim$`dimensions of mat_new in combined studies`[1], 7371)
    expect_equal(res.mat_new.dim$`dimensions of mat_new in combined studies`[2], 11)
})

#
# Done
#

context("ds.completeCases::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "df", "df_new", "mat", "mat_new"))
})

disconnect.studies.dataset.cnsim()

context("ds.completeCases::smk::done")
