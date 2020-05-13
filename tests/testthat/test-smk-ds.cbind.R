#-------------------------------------------------------------------------------
# Copyright (c) 2018-2020 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.cbind::smk::setup")

connect.studies.dataset.survival(list("survtime", "time.id", "female", "age.60"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.cbind::smk::from dataframe variables")
test_that("simple test, from dataframe variables", {
    res <- ds.cbind(c("D$survtime", "D$time.id", "D$female", "D$age.60"), newobj="cbind_newobj")

    expect_equal(res$is.object.created, "A data object <cbind_newobj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<cbind_newobj> appears valid in all sources")

    res1 <- ds.class("cbind_newobj")
    expect_equal(res1$survival1, "matrix")
    expect_equal(res1$survival2, "matrix")
    expect_equal(res1$survival3, "matrix")

    res.dim <- ds.dim("cbind_newobj")
    expect_length(res.dim, 4)
    expect_length(res.dim$`dimensions of cbind_newobj in survival1`, 2)
    expect_equal(res.dim$`dimensions of cbind_newobj in survival1`[1], 2060)
    expect_equal(res.dim$`dimensions of cbind_newobj in survival1`[2], 4)
    expect_length(res.dim$`dimensions of cbind_newobj in survival2`, 2)
    expect_equal(res.dim$`dimensions of cbind_newobj in survival2`[1], 1640)
    expect_equal(res.dim$`dimensions of cbind_newobj in survival2`[2], 4)
    expect_length(res.dim$`dimensions of cbind_newobj in survival3`, 2)
    expect_equal(res.dim$`dimensions of cbind_newobj in survival3`[1], 2688)
    expect_equal(res.dim$`dimensions of cbind_newobj in survival3`[2], 4)
    expect_length(res.dim$`dimensions of cbind_newobj in combined studies`, 2)
    expect_equal(res.dim$`dimensions of cbind_newobj in combined studies`[1], 6388)
    expect_equal(res.dim$`dimensions of cbind_newobj in combined studies`[2], 4)

    res.colnames <- ds.colnames("cbind_newobj")
    expect_length(res.colnames, 3)
    expect_length(res.colnames$survival1, 4)
    expect_equal(res.colnames$survival1[1], "D$survtime")
    expect_equal(res.colnames$survival1[2], "D$time.id")
    expect_equal(res.colnames$survival1[3], "D$female")
    expect_equal(res.colnames$survival1[4], "D$age.60")
    expect_length(res.colnames$survival2, 4)
    expect_equal(res.colnames$survival2[1], "D$survtime")
    expect_equal(res.colnames$survival2[2], "D$time.id")
    expect_equal(res.colnames$survival2[3], "D$female")
    expect_equal(res.colnames$survival2[4], "D$age.60")
    expect_length(res.colnames$survival3, 4)
    expect_equal(res.colnames$survival3[1], "D$survtime")
    expect_equal(res.colnames$survival3[2], "D$time.id")
    expect_equal(res.colnames$survival3[3], "D$female")
    expect_equal(res.colnames$survival3[4], "D$age.60")

    ds.rm("cbind_newobj")
})

context("ds.cbind::smk::from root variables")
test_that("simple test, from root variables", {
    ds.assign('D$survtime', 'survtime')
    ds.assign('D$time.id', 'time.id')
    ds.assign('D$female', 'female')
    ds.assign('D$age.60', 'age.60')

    res <- ds.cbind(c("survtime", "time.id", "female", "age.60"), newobj="cbind_newobj")

    expect_equal(res$is.object.created, "A data object <cbind_newobj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<cbind_newobj> appears valid in all sources")

    ds.rm("survtime")
    ds.rm("time.id")
    ds.rm("female")
    ds.rm("age.60")

    res1 <- ds.class("cbind_newobj")
    expect_equal(res1$survival1, "matrix")
    expect_equal(res1$survival2, "matrix")
    expect_equal(res1$survival3, "matrix")

    res.dim <- ds.dim("cbind_newobj")
    expect_length(res.dim, 4)
    expect_length(res.dim$`dimensions of cbind_newobj in survival1`, 2)
    expect_equal(res.dim$`dimensions of cbind_newobj in survival1`[1], 2060)
    expect_equal(res.dim$`dimensions of cbind_newobj in survival1`[2], 4)
    expect_length(res.dim$`dimensions of cbind_newobj in survival2`, 2)
    expect_equal(res.dim$`dimensions of cbind_newobj in survival2`[1], 1640)
    expect_equal(res.dim$`dimensions of cbind_newobj in survival2`[2], 4)
    expect_length(res.dim$`dimensions of cbind_newobj in survival3`, 2)
    expect_equal(res.dim$`dimensions of cbind_newobj in survival3`[1], 2688)
    expect_equal(res.dim$`dimensions of cbind_newobj in survival3`[2], 4)
    expect_length(res.dim$`dimensions of cbind_newobj in combined studies`, 2)
    expect_equal(res.dim$`dimensions of cbind_newobj in combined studies`[1], 6388)
    expect_equal(res.dim$`dimensions of cbind_newobj in combined studies`[2], 4)

    res.colnames <- ds.colnames("cbind_newobj")
    expect_length(res.colnames, 3)
    expect_length(res.colnames$survival1, 4)
    expect_equal(res.colnames$survival1[1], "survtime")
    expect_equal(res.colnames$survival1[2], "time.id")
    expect_equal(res.colnames$survival1[3], "female")
    expect_equal(res.colnames$survival1[4], "age.60")
    expect_length(res.colnames$survival2, 4)
    expect_equal(res.colnames$survival2[1], "survtime")
    expect_equal(res.colnames$survival2[2], "time.id")
    expect_equal(res.colnames$survival2[3], "female")
    expect_equal(res.colnames$survival2[4], "age.60")
    expect_length(res.colnames$survival3, 4)
    expect_equal(res.colnames$survival3[1], "survtime")
    expect_equal(res.colnames$survival3[2], "time.id")
    expect_equal(res.colnames$survival3[3], "female")
    expect_equal(res.colnames$survival3[4], "age.60")

    ds.rm("cbind_newobj")
})

#
# Done
#

context("ds.cbind::smk::shutdown")

test_that("setup", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.survival()

context("ds.cbind::smk::done")
