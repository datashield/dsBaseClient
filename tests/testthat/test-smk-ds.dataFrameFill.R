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

# context("ds.dataFrameFill::smk::setup")

connect.discordant.dataset.simple(list("A", "B", "C"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.dataFrameFill::smk::extend unfilled dataframes")
test_that("dataFrameFill_exists", {
    colnamesD <- ds.colnames('D')

    expect_length(colnamesD, 3)
    expect_length(colnamesD$discordant1, 2)
    expect_equal(colnamesD$discordant1[1], "A")
    expect_equal(colnamesD$discordant1[2], "B")
    expect_length(colnamesD$discordant2, 2)
    expect_equal(colnamesD$discordant2[1], "A")
    expect_equal(colnamesD$discordant2[2], "C")
    expect_length(colnamesD$discordant3, 2)
    expect_equal(colnamesD$discordant3[1], "B")
    expect_equal(colnamesD$discordant3[2], "C")

    res <- ds.dataFrameFill(df.name="D", newobj="filled_df")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <filled_df> has been created in all specified data sources")
    expect_equal(res$validity.check, "<filled_df> appears valid in all sources")

    colnamesFilled <- ds.colnames('filled_df')

    expect_length(colnamesFilled, 3)
    expect_length(colnamesFilled$discordant1, 3)
    expect_equal(colnamesFilled$discordant1[1], "A")
    expect_equal(colnamesFilled$discordant1[2], "B")
    expect_equal(colnamesFilled$discordant1[3], "C")
    expect_length(colnamesFilled$discordant2, 3)
    expect_equal(colnamesFilled$discordant2[1], "A")
    expect_equal(colnamesFilled$discordant2[2], "C")
    expect_equal(colnamesFilled$discordant2[3], "B")
    expect_length(colnamesFilled$discordant3, 3)
    expect_equal(colnamesFilled$discordant3[1], "B")
    expect_equal(colnamesFilled$discordant3[2], "C")
    expect_equal(colnamesFilled$discordant3[3], "A")

    classFilled <- ds.class('filled_df')

    expect_length(classFilled, 3)
    expect_length(classFilled$discordant1, 1)
    expect_equal(classFilled$discordant1, "data.frame")
    expect_length(classFilled$discordant2, 1)
    expect_equal(classFilled$discordant2, "data.frame")
    expect_length(classFilled$discordant3, 1)
    expect_equal(classFilled$discordant3, "data.frame")

    classFilled <- ds.class('filled_df$A')
    
    expect_length(classFilled, 3)
    expect_length(classFilled$discordant1, 1)
    expect_equal(classFilled$discordant1, "integer")
    expect_length(classFilled$discordant2, 1)
    expect_equal(classFilled$discordant2, "integer")
    expect_length(classFilled$discordant3, 1)
    expect_equal(classFilled$discordant3, "integer")
    
    classFilled <- ds.class('filled_df$B')

    expect_length(classFilled, 3)
    expect_length(classFilled$discordant1, 1)
    expect_equal(classFilled$discordant1, "integer")
    expect_length(classFilled$discordant2, 1)
    expect_equal(classFilled$discordant2, "integer")
    expect_length(classFilled$discordant3, 1)
    expect_equal(classFilled$discordant3, "integer")

    classFilled <- ds.class('filled_df$C')

    expect_length(classFilled, 3)
    expect_length(classFilled$discordant1, 1)
    expect_equal(classFilled$discordant1, "integer")
    expect_length(classFilled$discordant2, 1)
    expect_equal(classFilled$discordant2, "integer")
    expect_length(classFilled$discordant3, 1)
    expect_equal(classFilled$discordant3, "integer")
})

#
# Done
#

# context("ds.dataFrameFill::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "filled_df"))
})

disconnect.discordant.dataset.simple()

# context("ds.dataFrameFill::smk::done")
