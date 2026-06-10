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

# context("ANTHRO::datachk::setup")

connect.studies.dataset.anthro(list('age', 'sex', 'weight', 'height', 'muac'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ANTHRO::datachk")
test_that("Check ANTHRO dataset", {
    res.class <- ds.class(x='D')
    expect_length(res.class, 3)
    expect_gte(length(res.class$study1), 1)
    expect_true("data.frame" %in% res.class$study3)
    expect_gte(length(res.class$study2), 1)
    expect_true("data.frame" %in% res.class$study2)
    expect_gte(length(res.class$study3), 1)
    expect_true("data.frame" %in% res.class$study3)

    res.length <- ds.length(x='D')
    expect_length(res.length, 4)
    expect_length(res.length$`length of D in study1`, 1)
    expect_equal(res.length$`length of D in study1`, 5)
    expect_length(res.length$`length of D in study2`, 1)
    expect_equal(res.length$`length of D in study2`, 5)
    expect_length(res.length$`length of D in study3`, 1)
    expect_equal(res.length$`length of D in study3`, 5)
    expect_equal(res.length$`total length of D in all studies combined`, 15)

    res.colnames <- ds.colnames(x='D')
    expect_length(res.colnames, 3)
    expect_length(res.colnames$study1, 5)
    expect_equal(res.colnames$study1, c('age', 'sex', 'weight', 'height', 'muac'))
    expect_length(res.colnames$study2, 5)
    expect_equal(res.colnames$study2, c('age', 'sex', 'weight', 'height', 'muac'))
    expect_length(res.colnames$study3, 5)
    expect_equal(res.colnames$study3, c('age', 'sex', 'weight', 'height', 'muac'))

    res.class.age <- ds.class(x='D$age')
    expect_length(res.class.age, 3)
    expect_length(res.class.age$study1, 1)
    expect_equal(res.class.age$study1, "numeric")
    expect_length(res.class.age$study2, 1)
    expect_equal(res.class.age$study2, "numeric")
    expect_length(res.class.age$study3, 1)
    expect_equal(res.class.age$study3, "numeric")

    res.length.age <- ds.length(x='D$age')
    expect_length(res.length.age, 4)
    expect_length(res.length.age$`length of D$age in study1`, 1)
    expect_equal(res.length.age$`length of D$age in study1`, 873)
    expect_length(res.length.age$`length of D$age in study2`, 1)
    expect_equal(res.length.age$`length of D$age in study2`, 796)
    expect_length(res.length.age$`length of D$age in study3`, 1)
    expect_equal(res.length.age$`length of D$age in study3`, 221)
    expect_length(res.length.age$`total length of D$age in all studies combined`, 1)
    expect_equal(res.length.age$`total length of D$age in all studies combined`, 1890)

    res.class.sex <- ds.class(x='D$sex')
    expect_length(res.class.sex, 3)
    expect_length(res.class.sex$study1, 1)
    expect_equal(res.class.sex$study1, "factor")
    expect_length(res.class.sex$study2, 1)
    expect_equal(res.class.sex$study2, "factor")
    expect_length(res.class.sex$study3, 1)
    expect_equal(res.class.sex$study3, "factor")

    res.length.sex <- ds.length(x='D$sex')
    expect_length(res.length.sex, 4)
    expect_length(res.length.sex$`length of D$sex in study1`, 1)
    expect_equal(res.length.sex$`length of D$sex in study1`, 873)
    expect_length(res.length.sex$`length of D$sex in study2`, 1)
    expect_equal(res.length.sex$`length of D$sex in study2`, 796)
    expect_length(res.length.sex$`length of D$sex in study3`, 1)
    expect_equal(res.length.sex$`length of D$sex in study3`, 221)
    expect_length(res.length.sex$`total length of D$sex in all studies combined`, 1)
    expect_equal(res.length.sex$`total length of D$sex in all studies combined`, 1890)

    res.class.weight <- ds.class(x='D$weight')
    expect_length(res.class.weight, 3)
    expect_length(res.class.weight$study1, 1)
    expect_equal(res.class.weight$study1, "numeric")
    expect_length(res.class.weight$study2, 1)
    expect_equal(res.class.weight$study2, "numeric")
    expect_length(res.class.weight$study3, 1)
    expect_equal(res.class.weight$study3, "numeric")

    res.length.weight <- ds.length(x='D$weight')
    expect_length(res.length.weight, 4)
    expect_length(res.length.weight$`length of D$weight in study1`, 1)
    expect_equal(res.length.weight$`length of D$weight in study1`, 873)
    expect_length(res.length.weight$`length of D$weight in study2`, 1)
    expect_equal(res.length.weight$`length of D$weight in study2`, 796)
    expect_length(res.length.weight$`length of D$weight in study3`, 1)
    expect_equal(res.length.weight$`length of D$weight in study3`, 221)
    expect_length(res.length.weight$`total length of D$weight in all studies combined`, 1)
    expect_equal(res.length.weight$`total length of D$weight in all studies combined`, 1890)

    res.class.height <- ds.class(x='D$height')
    expect_length(res.class.height, 3)
    expect_length(res.class.height$study1, 1)
    expect_equal(res.class.height$study1, "numeric")
    expect_length(res.class.height$study2, 1)
    expect_equal(res.class.height$study2, "numeric")
    expect_length(res.class.height$study3, 1)
    expect_equal(res.class.height$study3, "numeric")

    res.length.height <- ds.length(x='D$height')
    expect_length(res.length.height, 4)
    expect_length(res.length.height$`length of D$height in study1`, 1)
    expect_equal(res.length.height$`length of D$height in study1`, 873)
    expect_length(res.length.height$`length of D$height in study2`, 1)
    expect_equal(res.length.height$`length of D$height in study2`, 796)
    expect_length(res.length.height$`length of D$height in study3`, 1)
    expect_equal(res.length.height$`length of D$height in study3`, 221)
    expect_length(res.length.height$`total length of D$height in all studies combined`, 1)
    expect_equal(res.length.height$`total length of D$height in all studies combined`, 1890)

    res.class.muac <- ds.class(x='D$muac')
    expect_length(res.class.muac, 3)
    expect_length(res.class.muac$study1, 1)
    expect_equal(res.class.muac$study1, "numeric")
    expect_length(res.class.muac$study2, 1)
    expect_equal(res.class.muac$study2, "numeric")
    expect_length(res.class.muac$study3, 1)
    expect_equal(res.class.muac$study3, "numeric")

    res.length.muac <- ds.length(x='D$muac')
    expect_length(res.length.muac, 4)
    expect_length(res.length.muac$`length of D$muac in study1`, 1)
    expect_equal(res.length.muac$`length of D$muac in study1`, 873)
    expect_length(res.length.muac$`length of D$muac in study2`, 1)
    expect_equal(res.length.muac$`length of D$muac in study2`, 796)
    expect_length(res.length.muac$`length of D$muac in study3`, 1)
    expect_equal(res.length.muac$`length of D$muac in study3`, 221)
    expect_length(res.length.muac$`total length of D$muac in all studies combined`, 1)
    expect_equal(res.length.muac$`total length of D$muac in all studies combined`, 1890)
})

#
# Tear down
#

# context("ANTHRO::datachk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.anthro()

#
# Done
#

# context("ANTHRO::datachk::done")
