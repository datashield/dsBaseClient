#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
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

# context("ds.meanSdGp::smk::setup")

connect.studies.dataset.survival(list("age.60", "female"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.meanSdGp::smk::type=split")
test_that("meanSdGp values [split]", {
    stat.meanSdGp <- ds.meanSdGp(x='D$age.60', y='D$female', type='split')

    expect_length(stat.meanSdGp, 7)
    expect_true("matrix" %in% class(stat.meanSdGp$Mean_gp_study))
    expect_true("matrix" %in% class(stat.meanSdGp$StDev_gp_study))
    expect_true("matrix" %in% class(stat.meanSdGp$Nvalid_gp_study))
    expect_true("matrix" %in% class(stat.meanSdGp$SEM_gp_study))
    expect_true("numeric" %in% class(stat.meanSdGp$Total_Nvalid))
    expect_true("numeric" %in% class(stat.meanSdGp$Total_Nmissing))
    expect_true("numeric" %in% class(stat.meanSdGp$Total_Ntotal))
})

# context("ds.meanSdGp::smk::type=combine")
test_that("meanSdGp values [combine]", {
    stat.meanSdGp <- ds.meanSdGp(x='D$age.60', y='D$female', type='combine')

    expect_length(stat.meanSdGp, 7)
    expect_true("matrix" %in% class(stat.meanSdGp$Mean_gp))
    expect_true("matrix" %in% class(stat.meanSdGp$StDev_gp))
    expect_true("matrix" %in% class(stat.meanSdGp$Nvalid_gp))
    expect_true("matrix" %in% class(stat.meanSdGp$SEM_gp))
    expect_true("numeric" %in% class(stat.meanSdGp$Total_Nvalid))
    expect_true("numeric" %in% class(stat.meanSdGp$Total_Nmissing))
    expect_true("numeric" %in% class(stat.meanSdGp$Total_Ntotal))
})

# context("ds.meanSdGp::smk::type=both")
test_that("meanSdGp values [both]", {
    stat.meanSdGp <- ds.meanSdGp(x='D$age.60', y='D$female', type='both')

    expect_length(stat.meanSdGp, 7)
    expect_true("matrix" %in% class(stat.meanSdGp$Mean_gp_study))
    expect_true("matrix" %in% class(stat.meanSdGp$StDev_gp_study))
    expect_true("matrix" %in% class(stat.meanSdGp$Nvalid_gp_study))
    expect_true("matrix" %in% class(stat.meanSdGp$SEM_gp_study))
    expect_true("numeric" %in% class(stat.meanSdGp$Total_Nvalid))
    expect_true("numeric" %in% class(stat.meanSdGp$Total_Nmissing))
    expect_true("numeric" %in% class(stat.meanSdGp$Total_Ntotal))
})

test_that("fails if the object does not exist", {
    expect_error(
        ds.meanSdGp(x = "nonexistent_object", y = "D$female"),
        "There are some DataSHIELD errors, list them with datashield.errors()",
        fixed = TRUE
    )

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "The server-side object 'nonexistent_object' does not exist")
    }
})

test_that("fails if the object is the wrong type", {
    expect_error(
        ds.meanSdGp(x = "D", y = "D$female"),
        "There are some DataSHIELD errors, list them with datashield.errors()",
        fixed = TRUE
    )

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "The server-side object must be of type numeric or integer. 'D' is type data.frame.", fixed = TRUE)
    }
})

#
# Done
#

# context("ds.meanSdGp::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.survival()

# context("ds.meanSdGp::smk::done")
