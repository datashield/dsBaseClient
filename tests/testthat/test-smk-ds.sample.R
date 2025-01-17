#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.sample::smk::setup")

connect.studies.dataset.survival(list("survtime", "time.id", "female"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.sample::smk::test")
test_that("simple test", {
    res1 <- ds.sample(x="D", size=30)

    expect_length(res1, 2)
    expect_equal(res1$is.object.created, "A data object <newobj.sample> has been created in all specified data sources", fixed=TRUE)
    expect_equal(res1$validity.check, "<newobj.sample> appears valid in all sources", fixed=TRUE)

    res1_length <- ds.length('newobj.sample')

    expect_length(res1_length, 4)
    expect_equal(res1_length$`length of newobj.sample in survival1`, 6)
    expect_equal(res1_length$`length of newobj.sample in survival2`, 6)
    expect_equal(res1_length$`length of newobj.sample in survival3`, 6)
    expect_equal(res1_length$`total length of newobj.sample in all studies combined`, 18)

    res1_colnames <- ds.colnames('newobj.sample')

    expect_length(res1_colnames, 3)
    expect_length(res1_colnames$survival1, 6)
    expect_true(all(res1_colnames$survival1[1] %in% c("time.id", "survtime")))
    expect_true(all(res1_colnames$survival1[2] %in% c("time.id", "survtime")))
    expect_equal(res1_colnames$survival1[3], "female")
    expect_equal(res1_colnames$survival1[4], "in.sample")
    expect_equal(res1_colnames$survival1[5], "ID.seq")
    expect_equal(res1_colnames$survival1[6], "sampling.order")
    expect_length(res1_colnames$survival2, 6)
    expect_true(all(res1_colnames$survival3[1] %in% c("time.id", "survtime")))
    expect_true(all(res1_colnames$survival3[2] %in% c("time.id", "survtime")))
    expect_equal(res1_colnames$survival2[3], "female")
    expect_equal(res1_colnames$survival2[4], "in.sample")
    expect_equal(res1_colnames$survival2[5], "ID.seq")
    expect_equal(res1_colnames$survival2[6], "sampling.order")
    expect_length(res1_colnames$survival3, 6)
    expect_true(all(res1_colnames$survival3[1] %in% c("time.id", "survtime")))
    expect_true(all(res1_colnames$survival3[2] %in% c("time.id", "survtime")))
    expect_equal(res1_colnames$survival3[3], "female")
    expect_equal(res1_colnames$survival3[4], "in.sample")
    expect_equal(res1_colnames$survival3[5], "ID.seq")
    expect_equal(res1_colnames$survival3[6], "sampling.order")

    res1_survtime_length <- ds.length('newobj.sample$survtime')

    expect_length(res1_survtime_length, 4)
    expect_equal(res1_survtime_length$`length of newobj.sample$survtime in survival1`, 30)
    expect_equal(res1_survtime_length$`length of newobj.sample$survtime in survival2`, 30)
    expect_equal(res1_survtime_length$`length of newobj.sample$survtime in survival3`, 30)
    expect_equal(res1_survtime_length$`total length of newobj.sample$survtime in all studies combined`, 90)

    res2 <- ds.sample(x="D$survtime", size=42, newobj="test.obj")

    expect_length(res2, 2)
    expect_equal(res2$is.object.created, "A data object <test.obj> has been created in all specified data sources", fixed=TRUE)
    expect_equal(res2$validity.check, "<test.obj> appears valid in all sources", fixed=TRUE)

    res2_length <- ds.length('test.obj')

    expect_length(res2_length, 4)
    expect_equal(res2_length$`length of test.obj in survival1`, 4)
    expect_equal(res2_length$`length of test.obj in survival2`, 4)
    expect_equal(res2_length$`length of test.obj in survival3`, 4)
    expect_equal(res2_length$`total length of test.obj in all studies combined`, 12)

    res2_colnames <- ds.colnames('test.obj')

    expect_length(res2_colnames, 3)
    expect_length(res2_colnames$survival1, 4)
    expect_equal(res2_colnames$survival1[1], "survtime")
    expect_equal(res2_colnames$survival1[2], "in.sample")
    expect_equal(res2_colnames$survival1[3], "ID.seq")
    expect_equal(res2_colnames$survival1[4], "sampling.order")
    expect_length(res2_colnames$survival2, 4)
    expect_equal(res2_colnames$survival2[1], "survtime")
    expect_equal(res2_colnames$survival2[2], "in.sample")
    expect_equal(res2_colnames$survival2[3], "ID.seq")
    expect_equal(res2_colnames$survival2[4], "sampling.order")
    expect_length(res2_colnames$survival3, 4)
    expect_equal(res2_colnames$survival3[1], "survtime")
    expect_equal(res2_colnames$survival3[2], "in.sample")
    expect_equal(res2_colnames$survival3[3], "ID.seq")
    expect_equal(res2_colnames$survival3[4], "sampling.order")

    res2_survtime_length <- ds.length('test.obj$in.sample')

    expect_length(res2_survtime_length, 4)
    expect_equal(res2_survtime_length$`length of test.obj$in.sample in survival1`, 42)
    expect_equal(res2_survtime_length$`length of test.obj$in.sample in survival2`, 42)
    expect_equal(res2_survtime_length$`length of test.obj$in.sample in survival3`, 42)
    expect_equal(res2_survtime_length$`total length of test.obj$in.sample in all studies combined`, 126)
})

context("ds.sample::smk::test error")
test_that("simple test, error", {
    expect_error(ds.sample(x="D$survtime", size="30", newobj="no.obj"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)

    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 3)
    expect_match(res.errors$survival1, "* Error : FAILED: if sampling without replacement size must be less than or equal to length\\(x\\)*")
    expect_match(res.errors$survival2, "* Error : FAILED: if sampling without replacement size must be less than or equal to length\\(x\\)*")
    expect_match(res.errors$survival3, "* Error : FAILED: if sampling without replacement size must be less than or equal to length\\(x\\)*")
})

#
# Done
#

context("ds.sample::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "newobj.sample", "test.obj"))
})

disconnect.studies.dataset.survival()

context("ds.sample::smk::done")
