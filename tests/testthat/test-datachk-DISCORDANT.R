#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
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

context("DISCORDANT::datachk::setup")

connect.discordant.dataset.simple(list('A', 'B', 'C'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("DISCORDANT::datachk")
test_that("Check DISCORDANT dataset", {
    res.class <- ds.class(x='D')
    expect_length(res.class, 3)
    expect_length(res.class$discordant1, 1)
    expect_equal(res.class$discordant1, "data.frame")
    expect_length(res.class$discordant2, 1)
    expect_equal(res.class$discordant2, "data.frame")
    expect_length(res.class$discordant3, 1)
    expect_equal(res.class$discordant3, "data.frame")

    res.length <- ds.length(x='D')
    expect_length(res.length, 4)
    expect_length(res.length$`length of D in discordant1`, 1)
    expect_equal(res.length$`length of D in discordant1`, 2)
    expect_length(res.length$`length of D in discordant2`, 1)
    expect_equal(res.length$`length of D in discordant2`, 2)
    expect_length(res.length$`length of D in discordant3`, 1)
    expect_equal(res.length$`length of D in discordant3`, 2)
    expect_equal(res.length$`total length of D in all studies combined`, 6)

    res.colnames <- ds.colnames(x='D')
    expect_length(res.colnames, 3)
    expect_length(res.colnames$discordant1, 2)
    expect_equal(res.colnames$discordant1, c('A', 'B'))
    expect_length(res.colnames$discordant2, 2)
    expect_equal(res.colnames$discordant2, c('A', 'C'))
    expect_length(res.colnames$discordant3, 2)
    expect_equal(res.colnames$discordant3, c('B', 'C'))

    res.class.a <- ds.class(x='D$A')
    expect_length(res.class.a, 3)
    expect_length(res.class.a$discordant1, 1)
    expect_equal(res.class.a$discordant1, "integer")
    expect_length(res.class.a$discordant2, 1)
    expect_equal(res.class.a$discordant2, "integer")
    expect_length(res.class.a$discordant3, 1)
    expect_equal(res.class.a$discordant3, "NULL")

    res.length.a <- ds.length(x='D$A')
    expect_length(res.length.a, 4)
    expect_length(res.length.a$`length of D$A in discordant1`, 1)
    expect_equal(res.length.a$`length of D$A in discordant1`, 12)
    expect_length(res.length.a$`length of D$A in discordant2`, 1)
    expect_equal(res.length.a$`length of D$A in discordant2`, 12)
    expect_length(res.length.a$`length of D$A in discordant3`, 1)
    expect_equal(res.length.a$`length of D$A in discordant3`, 0)
    expect_length(res.length.a$`total length of D$A in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$A in all studies combined`, 24)

    res.class.b <- ds.class(x='D$B')
    expect_length(res.class.b, 3)
    expect_length(res.class.b$discordant1, 1)
    expect_equal(res.class.b$discordant1, "integer")
    expect_length(res.class.b$discordant2, 1)
    expect_equal(res.class.b$discordant2, "NULL")
    expect_length(res.class.b$discordant3, 1)
    expect_equal(res.class.b$discordant3, "integer")

    res.length.b <- ds.length(x='D$B')
    expect_length(res.length.b, 4)
    expect_length(res.length.b$`length of D$B in discordant1`, 1)
    expect_equal(res.length.b$`length of D$B in discordant1`, 12)
    expect_length(res.length.b$`length of D$B in discordant2`, 1)
    expect_equal(res.length.b$`length of D$B in discordant2`, 0)
    expect_length(res.length.b$`length of D$B in discordant3`, 1)
    expect_equal(res.length.b$`length of D$B in discordant3`, 12)
    expect_length(res.length.b$`total length of D$B in all studies combined`, 1)
    expect_equal(res.length.b$`total length of D$B in all studies combined`, 24)

    res.class.c <- ds.class(x='D$C')
    expect_length(res.class.c, 3)
    expect_length(res.class.c$discordant1, 1)
    expect_equal(res.class.c$discordant1, "NULL")
    expect_length(res.class.c$discordant2, 1)
    expect_equal(res.class.c$discordant2, "integer")
    expect_length(res.class.c$discordant3, 1)
    expect_equal(res.class.c$discordant3, "integer")

    res.length.c <- ds.length(x='D$C')
    expect_length(res.length.c, 4)
    expect_length(res.length.c$`length of D$C in discordant1`, 1)
    expect_equal(res.length.c$`length of D$C in discordant1`, 0)
    expect_length(res.length.c$`length of D$C in discordant2`, 1)
    expect_equal(res.length.c$`length of D$C in discordant2`, 12)
    expect_length(res.length.c$`length of D$C in discordant3`, 1)
    expect_equal(res.length.c$`length of D$C in discordant3`, 12)
    expect_length(res.length.c$`total length of D$C in all studies combined`, 1)
    expect_equal(res.length.c$`total length of D$C in all studies combined`, 24)
})

#
# Tear down
#

context("DISCORDANT::datachk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.discordant.dataset.simple()

#
# Done
#

context("DISCORDANT::datachk::done")
