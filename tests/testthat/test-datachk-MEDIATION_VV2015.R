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

context("MEDIATION_VV2015::datachk::setup")

connect.mediation.dataset.vv2015(list('id', 'x', 'm', 'y', 'cens', 'c', 'event'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("MEDIATION_VV2015::datachk")
test_that("Check MEDIATION_VV2015 dataset", {
    res.class <- ds.class(x='D')
    expect_length(res.class, 1)
    expect_length(res.class$study1, 1)
    expect_equal(res.class$study1, "data.frame")

    res.length <- ds.length(x='D')
    expect_length(res.length, 2)
    expect_length(res.length$`length of D in study1`, 1)
    expect_equal(res.length$`length of D in study1`, 7)
    expect_equal(res.length$`total length of D in all studies combined`, 7)

    res.colnames <- ds.colnames(x='D')
    expect_length(res.colnames, 1)
    expect_length(res.colnames$study1, 7)
    expect_equal(res.colnames$study1, c('id', 'x', 'm', 'y', 'cens', 'c', 'event'))

    res.class.a <- ds.class(x='D$id')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "numeric")

    res.length.a <- ds.length(x='D$id')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$id in study1`, 1)
    expect_equal(res.length.a$`length of D$id in study1`, 100)
    expect_length(res.length.a$`total length of D$id in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$id in all studies combined`, 100)

    res.class.a <- ds.class(x='D$x')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "numeric")

    res.length.a <- ds.length(x='D$x')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$x in study1`, 1)
    expect_equal(res.length.a$`length of D$x in study1`, 100)
    expect_length(res.length.a$`total length of D$x in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$x in all studies combined`, 100)

    res.class.a <- ds.class(x='D$m')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "numeric")

    res.length.a <- ds.length(x='D$m')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$m in study1`, 1)
    expect_equal(res.length.a$`length of D$m in study1`, 100)
    expect_length(res.length.a$`total length of D$m in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$m in all studies combined`, 100)

    res.class.a <- ds.class(x='D$y')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "numeric")

    res.length.a <- ds.length(x='D$y')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$y in study1`, 1)
    expect_equal(res.length.a$`length of D$y in study1`, 100)
    expect_length(res.length.a$`total length of D$y in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$y in all studies combined`, 100)

    res.class.a <- ds.class(x='D$cens')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "numeric")

    res.length.a <- ds.length(x='D$cens')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$cens in study1`, 1)
    expect_equal(res.length.a$`length of D$cens in study1`, 100)
    expect_length(res.length.a$`total length of D$cens in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$cens in all studies combined`, 100)

    res.class.a <- ds.class(x='D$c')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "numeric")

    res.length.a <- ds.length(x='D$c')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$c in study1`, 1)
    expect_equal(res.length.a$`length of D$c in study1`, 100)
    expect_length(res.length.a$`total length of D$c in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$c in all studies combined`, 100)

    res.class.a <- ds.class(x='D$event')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")

    res.length.a <- ds.length(x='D$event')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$event in study1`, 1)
    expect_equal(res.length.a$`length of D$event in study1`, 100)
    expect_length(res.length.a$`total length of D$event in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$event in all studies combined`, 100)
})

#
# Tear down
#

context("MEDIATION_VV2015::datachk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.mediation.dataset.vv2015()

#
# Done
#

context("MEDIATION_VV2015::datachk::done")
