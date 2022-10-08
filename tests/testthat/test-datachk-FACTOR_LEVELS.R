#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

context("FACTOR_LEVELS::datachk::setup")

connect.testing.dataset.factor_levels()

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("FACTOR_LEVELS::datachk")
test_that("Check FACTOR_LEVELS dataset", {
    res.class <- ds.class(x='D')
    expect_length(res.class, 3)
    expect_gte(length(res.class$GROUP1), 1)
    expect_true("data.frame" %in% res.class$GROUP1)
    expect_gte(length(res.class$GROUP2), 1)
    expect_true("data.frame" %in% res.class$GROUP2)
    expect_gte(length(res.class$GROUP3), 1)
    expect_true("data.frame" %in% res.class$GROUP3)

    res.length <- ds.length(x='D')
    expect_length(res.length, 4)
    expect_length(res.length$`length of D in GROUP1`, 1)
    expect_equal(res.length$`length of D in GROUP1`, 10)
    expect_length(res.length$`length of D in GROUP2`, 1)
    expect_equal(res.length$`length of D in GROUP2`, 10)
    expect_length(res.length$`length of D in GROUP3`, 1)
    expect_equal(res.length$`length of D in GROUP3`, 10)
    expect_equal(res.length$`total length of D in all studies combined`, 30)

    res.colnames <- ds.colnames(x='D')
    expect_length(res.colnames, 3)
    expect_length(res.colnames$GROUP1, 10)
    expect_equal(res.colnames$GROUP1, c("ID", "COLOURS", "COLOURS.NUMBERS", "POSITIVE.NUMBERS", "NEGATIVE.NUMBERS", "NUMBERS", "POSITIVE.DECIMAL", "NEGATIVE.DECIMAL", "DECIMAL", "PLANETS.CHARACTERS"))
    expect_length(res.colnames$GROUP2, 10)
    expect_equal(res.colnames$GROUP2, c("ID", "COLOURS", "COLOURS.NUMBERS", "POSITIVE.NUMBERS", "NEGATIVE.NUMBERS", "NUMBERS", "POSITIVE.DECIMAL", "NEGATIVE.DECIMAL", "DECIMAL", "PLANETS.CHARACTERS"))
    expect_length(res.colnames$GROUP3, 10)
    expect_equal(res.colnames$GROUP3, c("ID", "COLOURS", "COLOURS.NUMBERS", "POSITIVE.NUMBERS", "NEGATIVE.NUMBERS", "NUMBERS", "POSITIVE.DECIMAL", "NEGATIVE.DECIMAL", "DECIMAL", "PLANETS.CHARACTERS"))

    res.class.id <- ds.class(x='D$ID')
    expect_length(res.class.id, 3)
    expect_length(res.class.id$GROUP1, 1)
    expect_equal(res.class.id$GROUP1, "integer")
    expect_length(res.class.id$GROUP2, 1)
    expect_equal(res.class.id$GROUP2, "integer")
    expect_length(res.class.id$GROUP3, 1)
    expect_equal(res.class.id$GROUP3, "integer")

    res.length.id <- ds.length(x='D$ID')
    expect_length(res.length.id, 4)
    expect_length(res.length.id$`length of D$ID in GROUP1`, 1)
    expect_equal(res.length.id$`length of D$ID in GROUP1`, 100)
    expect_length(res.length.id$`length of D$ID in GROUP2`, 1)
    expect_equal(res.length.id$`length of D$ID in GROUP2`, 100)
    expect_length(res.length.id$`length of D$ID in GROUP3`, 1)
    expect_equal(res.length.id$`length of D$ID in GROUP3`, 100)
    expect_length(res.length.id$`total length of D$ID in all studies combined`, 1)
    expect_equal(res.length.id$`total length of D$ID in all studies combined`, 300)

    res.class.colours <- ds.class(x='D$COLOURS')
    expect_length(res.class.colours, 3)
    expect_length(res.class.colours$GROUP1, 1)
    expect_equal(res.class.colours$GROUP1, "factor")
    expect_length(res.class.colours$GROUP2, 1)
    expect_equal(res.class.colours$GROUP2, "factor")
    expect_length(res.class.colours$GROUP3, 1)
    expect_equal(res.class.colours$GROUP3, "factor")

    res.length.colours <- ds.length(x='D$COLOURS')
    expect_length(res.length.colours, 4)
    expect_length(res.length.colours$`length of D$COLOURS in GROUP1`, 1)
    expect_equal(res.length.colours$`length of D$COLOURS in GROUP1`, 100)
    expect_length(res.length.colours$`length of D$COLOURS in GROUP2`, 1)
    expect_equal(res.length.colours$`length of D$COLOURS in GROUP2`, 100)
    expect_length(res.length.colours$`length of D$COLOURS in GROUP3`, 1)
    expect_equal(res.length.colours$`length of D$COLOURS in GROUP3`, 100)
    expect_length(res.length.colours$`total length of D$COLOURS in all studies combined`, 1)
    expect_equal(res.length.colours$`total length of D$COLOURS in all studies combined`, 300)

    res.class.colours.numbers <- ds.class(x='D$COLOURS.NUMBERS')
    expect_length(res.class.colours.numbers, 3)
    expect_length(res.class.colours.numbers$GROUP1, 1)
    expect_equal(res.class.colours.numbers$GROUP1, "factor")
    expect_length(res.class.colours.numbers$GROUP2, 1)
    expect_equal(res.class.colours.numbers$GROUP2, "factor")
    expect_length(res.class.colours.numbers$GROUP3, 1)
    expect_equal(res.class.colours.numbers$GROUP3, "factor")

    res.length.colours.numbers <- ds.length(x='D$COLOURS.NUMBERS')
    expect_length(res.length.colours.numbers, 4)
    expect_length(res.length.colours.numbers$`length of D$COLOURS.NUMBERS in GROUP1`, 1)
    expect_equal(res.length.colours.numbers$`length of D$COLOURS.NUMBERS in GROUP1`, 100)
    expect_length(res.length.colours.numbers$`length of D$COLOURS.NUMBERS in GROUP2`, 1)
    expect_equal(res.length.colours.numbers$`length of D$COLOURS.NUMBERS in GROUP2`, 100)
    expect_length(res.length.colours.numbers$`length of D$COLOURS.NUMBERS in GROUP3`, 1)
    expect_equal(res.length.colours.numbers$`length of D$COLOURS.NUMBERS in GROUP3`, 100)
    expect_length(res.length.colours.numbers$`total length of D$COLOURS.NUMBERS in all studies combined`, 1)
    expect_equal(res.length.colours.numbers$`total length of D$COLOURS.NUMBERS in all studies combined`, 300)

    res.class.positive.numbers <- ds.class(x='D$POSITIVE.NUMBERS')
    expect_length(res.class.positive.numbers, 3)
    expect_length(res.class.positive.numbers$GROUP1, 1)
    expect_equal(res.class.positive.numbers$GROUP1, "factor")
    expect_length(res.class.positive.numbers$GROUP2, 1)
    expect_equal(res.class.positive.numbers$GROUP2, "factor")
    expect_length(res.class.positive.numbers$GROUP3, 1)
    expect_equal(res.class.positive.numbers$GROUP3, "factor")

    res.length.positive.numbers <- ds.length(x='D$POSITIVE.NUMBERS')
    expect_length(res.length.positive.numbers, 4)
    expect_length(res.length.positive.numbers$`length of D$POSITIVE.NUMBERS in GROUP1`, 1)
    expect_equal(res.length.positive.numbers$`length of D$POSITIVE.NUMBERS in GROUP1`, 100)
    expect_length(res.length.positive.numbers$`length of D$POSITIVE.NUMBERS in GROUP2`, 1)
    expect_equal(res.length.positive.numbers$`length of D$POSITIVE.NUMBERS in GROUP2`, 100)
    expect_length(res.length.positive.numbers$`length of D$POSITIVE.NUMBERS in GROUP3`, 1)
    expect_equal(res.length.positive.numbers$`length of D$POSITIVE.NUMBERS in GROUP3`, 100)
    expect_length(res.length.positive.numbers$`total length of D$POSITIVE.NUMBERS in all studies combined`, 1)
    expect_equal(res.length.positive.numbers$`total length of D$POSITIVE.NUMBERS in all studies combined`, 300)

    res.class.negative.numbers <- ds.class(x='D$NEGATIVE.NUMBERS')
    expect_length(res.class.negative.numbers, 3)
    expect_length(res.class.negative.numbers$GROUP1, 1)
    expect_equal(res.class.negative.numbers$GROUP1, "factor")
    expect_length(res.class.negative.numbers$GROUP2, 1)
    expect_equal(res.class.negative.numbers$GROUP2, "factor")
    expect_length(res.class.negative.numbers$GROUP3, 1)
    expect_equal(res.class.negative.numbers$GROUP3, "factor")

    res.length.negative.numbers <- ds.length(x='D$NEGATIVE.NUMBERS')
    expect_length(res.length.negative.numbers, 4)
    expect_length(res.length.negative.numbers$`length of D$NEGATIVE.NUMBERS in GROUP1`, 1)
    expect_equal(res.length.negative.numbers$`length of D$NEGATIVE.NUMBERS in GROUP1`, 100)
    expect_length(res.length.negative.numbers$`length of D$NEGATIVE.NUMBERS in GROUP2`, 1)
    expect_equal(res.length.negative.numbers$`length of D$NEGATIVE.NUMBERS in GROUP2`, 100)
    expect_length(res.length.negative.numbers$`length of D$NEGATIVE.NUMBERS in GROUP3`, 1)
    expect_equal(res.length.negative.numbers$`length of D$NEGATIVE.NUMBERS in GROUP3`, 100)
    expect_length(res.length.negative.numbers$`total length of D$NEGATIVE.NUMBERS in all studies combined`, 1)
    expect_equal(res.length.negative.numbers$`total length of D$NEGATIVE.NUMBERS in all studies combined`, 300)

    res.class.numbers <- ds.class(x='D$NUMBERS')
    expect_length(res.class.numbers, 3)
    expect_length(res.class.numbers$GROUP1, 1)
    expect_equal(res.class.numbers$GROUP1, "factor")
    expect_length(res.class.numbers$GROUP2, 1)
    expect_equal(res.class.numbers$GROUP2, "factor")
    expect_length(res.class.numbers$GROUP3, 1)
    expect_equal(res.class.numbers$GROUP3, "factor")

    res.length.numbers <- ds.length(x='D$NUMBERS')
    expect_length(res.length.numbers, 4)
    expect_length(res.length.numbers$`length of D$NUMBERS in GROUP1`, 1)
    expect_equal(res.length.numbers$`length of D$NUMBERS in GROUP1`, 100)
    expect_length(res.length.numbers$`length of D$NUMBERS in GROUP2`, 1)
    expect_equal(res.length.numbers$`length of D$NUMBERS in GROUP2`, 100)
    expect_length(res.length.numbers$`length of D$NUMBERS in GROUP3`, 1)
    expect_equal(res.length.numbers$`length of D$NUMBERS in GROUP3`, 100)
    expect_length(res.length.numbers$`total length of D$NUMBERS in all studies combined`, 1)
    expect_equal(res.length.numbers$`total length of D$NUMBERS in all studies combined`, 300)

    res.class.positive.decimal <- ds.class(x='D$POSITIVE.DECIMAL')
    expect_length(res.class.positive.decimal, 3)
    expect_length(res.class.positive.decimal$GROUP1, 1)
    expect_equal(res.class.positive.decimal$GROUP1, "factor")
    expect_length(res.class.positive.decimal$GROUP2, 1)
    expect_equal(res.class.positive.decimal$GROUP2, "factor")
    expect_length(res.class.positive.decimal$GROUP3, 1)
    expect_equal(res.class.positive.decimal$GROUP3, "factor")

    res.length.positive.decimal <- ds.length(x='D$POSITIVE.DECIMAL')
    expect_length(res.length.positive.decimal, 4)
    expect_length(res.length.positive.decimal$`length of D$POSITIVE.DECIMAL in GROUP1`, 1)
    expect_equal(res.length.positive.decimal$`length of D$POSITIVE.DECIMAL in GROUP1`, 100)
    expect_length(res.length.positive.decimal$`length of D$POSITIVE.DECIMAL in GROUP2`, 1)
    expect_equal(res.length.positive.decimal$`length of D$POSITIVE.DECIMAL in GROUP2`, 100)
    expect_length(res.length.positive.decimal$`length of D$POSITIVE.DECIMAL in GROUP3`, 1)
    expect_equal(res.length.positive.decimal$`length of D$POSITIVE.DECIMAL in GROUP3`, 100)
    expect_length(res.length.positive.decimal$`total length of D$POSITIVE.DECIMAL in all studies combined`, 1)
    expect_equal(res.length.positive.decimal$`total length of D$POSITIVE.DECIMAL in all studies combined`, 300)

    res.class.negative.decimal <- ds.class(x='D$NEGATIVE.DECIMAL')
    expect_length(res.class.negative.decimal, 3)
    expect_length(res.class.negative.decimal$GROUP1, 1)
    expect_equal(res.class.negative.decimal$GROUP1, "factor")
    expect_length(res.class.negative.decimal$GROUP2, 1)
    expect_equal(res.class.negative.decimal$GROUP2, "factor")
    expect_length(res.class.negative.decimal$GROUP3, 1)
    expect_equal(res.class.negative.decimal$GROUP3, "factor")

    res.length.negative.decimal <- ds.length(x='D$NEGATIVE.DECIMAL')
    expect_length(res.length.negative.decimal, 4)
    expect_length(res.length.negative.decimal$`length of D$NEGATIVE.DECIMAL in GROUP1`, 1)
    expect_equal(res.length.negative.decimal$`length of D$NEGATIVE.DECIMAL in GROUP1`, 100)
    expect_length(res.length.negative.decimal$`length of D$NEGATIVE.DECIMAL in GROUP2`, 1)
    expect_equal(res.length.negative.decimal$`length of D$NEGATIVE.DECIMAL in GROUP2`, 100)
    expect_length(res.length.negative.decimal$`length of D$NEGATIVE.DECIMAL in GROUP3`, 1)
    expect_equal(res.length.negative.decimal$`length of D$NEGATIVE.DECIMAL in GROUP3`, 100)
    expect_length(res.length.negative.decimal$`total length of D$NEGATIVE.DECIMAL in all studies combined`, 1)
    expect_equal(res.length.negative.decimal$`total length of D$NEGATIVE.DECIMAL in all studies combined`, 300)

    res.class.decimal <- ds.class(x='D$DECIMAL')
    expect_length(res.class.decimal, 3)
    expect_length(res.class.decimal$GROUP1, 1)
    expect_equal(res.class.decimal$GROUP1, "factor")
    expect_length(res.class.decimal$GROUP2, 1)
    expect_equal(res.class.decimal$GROUP2, "factor")
    expect_length(res.class.decimal$GROUP3, 1)
    expect_equal(res.class.decimal$GROUP3, "factor")

    res.length.decimal <- ds.length(x='D$DECIMAL')
    expect_length(res.length.decimal, 4)
    expect_length(res.length.decimal$`length of D$DECIMAL in GROUP1`, 1)
    expect_equal(res.length.decimal$`length of D$DECIMAL in GROUP1`, 100)
    expect_length(res.length.decimal$`length of D$DECIMAL in GROUP2`, 1)
    expect_equal(res.length.decimal$`length of D$DECIMAL in GROUP2`, 100)
    expect_length(res.length.decimal$`length of D$DECIMAL in GROUP3`, 1)
    expect_equal(res.length.decimal$`length of D$DECIMAL in GROUP3`, 100)
    expect_length(res.length.decimal$`total length of D$DECIMAL in all studies combined`, 1)
    expect_equal(res.length.decimal$`total length of D$DECIMAL in all studies combined`, 300)

    res.class.planets.characters <- ds.class(x='D$PLANETS.CHARACTERS')
    expect_length(res.class.planets.characters, 3)
    expect_length(res.class.planets.characters$GROUP1, 1)
    expect_equal(res.class.planets.characters$GROUP1, "factor")
    expect_length(res.class.planets.characters$GROUP2, 1)
    expect_equal(res.class.planets.characters$GROUP2, "factor")
    expect_length(res.class.planets.characters$GROUP3, 1)
    expect_equal(res.class.planets.characters$GROUP3, "factor")

    res.length.planets.characters <- ds.length(x='D$PLANETS.CHARACTERS')
    expect_length(res.length.planets.characters, 4)
    expect_length(res.length.planets.characters$`length of D$PLANETS.CHARACTERS in GROUP1`, 1)
    expect_equal(res.length.planets.characters$`length of D$PLANETS.CHARACTERS in GROUP1`, 100)
    expect_length(res.length.planets.characters$`length of D$PLANETS.CHARACTERS in GROUP2`, 1)
    expect_equal(res.length.planets.characters$`length of D$PLANETS.CHARACTERS in GROUP2`, 100)
    expect_length(res.length.planets.characters$`length of D$PLANETS.CHARACTERS in GROUP3`, 1)
    expect_equal(res.length.planets.characters$`length of D$PLANETS.CHARACTERS in GROUP3`, 100)
    expect_length(res.length.planets.characters$`total length of D$PLANETS.CHARACTERS in all studies combined`, 1)
    expect_equal(res.length.planets.characters$`total length of D$PLANETS.CHARACTERS in all studies combined`, 300)
})

#
# Tear down
#

context("FACTOR_LEVELS::datachk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.testing.dataset.factor_levels()

context("FACTOR_LEVELS::datachk::done")
