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

context("MEDIATION_UPB::datachk::setup")

connect.mediation.dataset.upb(list('att', 'attbin', 'attcat', 'negaff', 'initiator', 'gender', 'educ', 'age', 'UPB'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("MEDIATION_UPB::datachk")
test_that("Check MEDIATION_UPB dataset", {
    res.class <- ds.class(x='D')
    expect_length(res.class, 3)
    expect_length(res.class$study1, 1)
    expect_equal(res.class$study1, "data.frame")
    expect_length(res.class$study2, 1)
    expect_equal(res.class$study2, "data.frame")
    expect_length(res.class$study3, 1)
    expect_equal(res.class$study3, "data.frame")

    res.length <- ds.length(x='D')
    expect_length(res.length, 4)
    expect_length(res.length$`length of D in study1`, 1)
    expect_equal(res.length$`length of D in study1`, 9)
    expect_length(res.length$`length of D in study2`, 1)
    expect_equal(res.length$`length of D in study2`, 9)
    expect_length(res.length$`length of D in study3`, 1)
    expect_equal(res.length$`length of D in study3`, 9)
    expect_equal(res.length$`total length of D in all studies combined`, 27)

    res.colnames <- ds.colnames(x='D')
    expect_length(res.colnames, 3)
    expect_length(res.colnames$study1, 9)
    expect_equal(res.colnames$study1, c('att', 'attbin', 'attcat', 'negaff', 'initiator', 'gender', 'educ', 'age', 'UPB'))
    expect_length(res.colnames$study2, 9)
    expect_equal(res.colnames$study2, c('att', 'attbin', 'attcat', 'negaff', 'initiator', 'gender', 'educ', 'age', 'UPB'))
    expect_length(res.colnames$study3, 9)
    expect_equal(res.colnames$study3, c('att', 'attbin', 'attcat', 'negaff', 'initiator', 'gender', 'educ', 'age', 'UPB'))

    res.class.a <- ds.class(x='D$att')
    expect_length(res.class.a, 3)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "numeric")
    expect_length(res.class.a$study2, 1)
    expect_equal(res.class.a$study2, "numeric")
    expect_length(res.class.a$study3, 1)
    expect_equal(res.class.a$study3, "numeric")

    res.length.a <- ds.length(x='D$att')
    expect_length(res.length.a, 4)
    expect_length(res.length.a$`length of D$att in study1`, 1)
    expect_equal(res.length.a$`length of D$att in study1`, 385)
    expect_length(res.length.a$`length of D$att in study2`, 1)
    expect_equal(res.length.a$`length of D$att in study2`, 359)
    expect_length(res.length.a$`length of D$att in study3`, 1)
    expect_equal(res.length.a$`length of D$att in study3`, 382)
    expect_length(res.length.a$`total length of D$att in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$att in all studies combined`, 1126)

    res.class.a <- ds.class(x='D$attbin')
    expect_length(res.class.a, 3)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "numeric")
    expect_length(res.class.a$study2, 1)
    expect_equal(res.class.a$study2, "numeric")
    expect_length(res.class.a$study3, 1)
    expect_equal(res.class.a$study3, "numeric")

    res.length.a <- ds.length(x='D$attbin')
    expect_length(res.length.a, 4)
    expect_length(res.length.a$`length of D$attbin in study1`, 1)
    expect_equal(res.length.a$`length of D$attbin in study1`, 385)
    expect_length(res.length.a$`length of D$attbin in study2`, 1)
    expect_equal(res.length.a$`length of D$attbin in study2`, 359)
    expect_length(res.length.a$`length of D$attbin in study3`, 1)
    expect_equal(res.length.a$`length of D$attbin in study3`, 382)
    expect_length(res.length.a$`total length of D$attbin in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$attbin in all studies combined`, 1126)

    res.class.a <- ds.class(x='D$attcat')
    expect_length(res.class.a, 3)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")
    expect_length(res.class.a$study2, 1)
    expect_equal(res.class.a$study2, "factor")
    expect_length(res.class.a$study3, 1)
    expect_equal(res.class.a$study3, "factor")

    res.length.a <- ds.length(x='D$attcat')
    expect_length(res.length.a, 4)
    expect_length(res.length.a$`length of D$attcat in study1`, 1)
    expect_equal(res.length.a$`length of D$attcat in study1`, 385)
    expect_length(res.length.a$`length of D$attcat in study2`, 1)
    expect_equal(res.length.a$`length of D$attcat in study2`, 359)
    expect_length(res.length.a$`length of D$attcat in study3`, 1)
    expect_equal(res.length.a$`length of D$attcat in study3`, 382)
    expect_length(res.length.a$`total length of D$attcat in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$attcat in all studies combined`, 1126)

    res.class.a <- ds.class(x='D$negaff')
    expect_length(res.class.a, 3)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "numeric")
    expect_length(res.class.a$study2, 1)
    expect_equal(res.class.a$study2, "numeric")
    expect_length(res.class.a$study3, 1)
    expect_equal(res.class.a$study3, "numeric")

    res.length.a <- ds.length(x='D$negaff')
    expect_length(res.length.a, 4)
    expect_length(res.length.a$`length of D$negaff in study1`, 1)
    expect_equal(res.length.a$`length of D$negaff in study1`, 385)
    expect_length(res.length.a$`length of D$negaff in study2`, 1)
    expect_equal(res.length.a$`length of D$negaff in study2`, 359)
    expect_length(res.length.a$`length of D$negaff in study3`, 1)
    expect_equal(res.length.a$`length of D$negaff in study3`, 382)
    expect_length(res.length.a$`total length of D$negaff in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$negaff in all studies combined`, 1126)

    res.class.a <- ds.class(x='D$initiator')
    expect_length(res.class.a, 3)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")
    expect_length(res.class.a$study2, 1)
    expect_equal(res.class.a$study2, "factor")
    expect_length(res.class.a$study3, 1)
    expect_equal(res.class.a$study3, "factor")

    res.length.a <- ds.length(x='D$initiator')
    expect_length(res.length.a, 4)
    expect_length(res.length.a$`length of D$initiator in study1`, 1)
    expect_equal(res.length.a$`length of D$initiator in study1`, 385)
    expect_length(res.length.a$`length of D$initiator in study2`, 1)
    expect_equal(res.length.a$`length of D$initiator in study2`, 359)
    expect_length(res.length.a$`length of D$initiator in study3`, 1)
    expect_equal(res.length.a$`length of D$initiator in study3`, 382)
    expect_length(res.length.a$`total length of D$initiator in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$initiator in all studies combined`, 1126)

    res.class.a <- ds.class(x='D$gender')
    expect_length(res.class.a, 3)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")
    expect_length(res.class.a$study2, 1)
    expect_equal(res.class.a$study2, "factor")
    expect_length(res.class.a$study3, 1)
    expect_equal(res.class.a$study3, "factor")

    res.length.a <- ds.length(x='D$gender')
    expect_length(res.length.a, 4)
    expect_length(res.length.a$`length of D$gender in study1`, 1)
    expect_equal(res.length.a$`length of D$gender in study1`, 385)
    expect_length(res.length.a$`length of D$gender in study2`, 1)
    expect_equal(res.length.a$`length of D$gender in study2`, 359)
    expect_length(res.length.a$`length of D$gender in study3`, 1)
    expect_equal(res.length.a$`length of D$gender in study3`, 382)
    expect_length(res.length.a$`total length of D$gender in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$gender in all studies combined`, 1126)

    res.class.a <- ds.class(x='D$educ')
    expect_length(res.class.a, 3)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")
    expect_length(res.class.a$study2, 1)
    expect_equal(res.class.a$study2, "factor")
    expect_length(res.class.a$study3, 1)
    expect_equal(res.class.a$study3, "factor")

    res.length.a <- ds.length(x='D$educ')
    expect_length(res.length.a, 4)
    expect_length(res.length.a$`length of D$educ in study1`, 1)
    expect_equal(res.length.a$`length of D$educ in study1`, 385)
    expect_length(res.length.a$`length of D$educ in study2`, 1)
    expect_equal(res.length.a$`length of D$educ in study2`, 359)
    expect_length(res.length.a$`length of D$educ in study3`, 1)
    expect_equal(res.length.a$`length of D$educ in study3`, 382)
    expect_length(res.length.a$`total length of D$educ in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$educ in all studies combined`, 1126)

    res.class.a <- ds.class(x='D$age')
    expect_length(res.class.a, 3)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "integer")
    expect_length(res.class.a$study2, 1)
    expect_equal(res.class.a$study2, "integer")
    expect_length(res.class.a$study3, 1)
    expect_equal(res.class.a$study3, "integer")

    res.length.a <- ds.length(x='D$age')
    expect_length(res.length.a, 4)
    expect_length(res.length.a$`length of D$age in study1`, 1)
    expect_equal(res.length.a$`length of D$age in study1`, 385)
    expect_length(res.length.a$`length of D$age in study2`, 1)
    expect_equal(res.length.a$`length of D$age in study2`, 359)
    expect_length(res.length.a$`length of D$age in study3`, 1)
    expect_equal(res.length.a$`length of D$age in study3`, 382)
    expect_length(res.length.a$`total length of D$age in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$age in all studies combined`, 1126)

    res.class.a <- ds.class(x='D$UPB')
    expect_length(res.class.a, 3)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "integer")
    expect_length(res.class.a$study2, 1)
    expect_equal(res.class.a$study2, "integer")
    expect_length(res.class.a$study3, 1)
    expect_equal(res.class.a$study3, "integer")

    res.length.a <- ds.length(x='D$UPB')
    expect_length(res.length.a, 4)
    expect_length(res.length.a$`length of D$UPB in study1`, 1)
    expect_equal(res.length.a$`length of D$UPB in study1`, 385)
    expect_length(res.length.a$`length of D$UPB in study2`, 1)
    expect_equal(res.length.a$`length of D$UPB in study2`, 359)
    expect_length(res.length.a$`length of D$UPB in study3`, 1)
    expect_equal(res.length.a$`length of D$UPB in study3`, 382)
    expect_length(res.length.a$`total length of D$UPB in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$UPB in all studies combined`, 1126)
})

#
# Tear down
#

context("MEDIATION_UPB::datachk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.mediation.dataset.upb()

#
# Done
#

context("MEDIATION_UPB::datachk::done")
