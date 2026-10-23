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

# context("ds.asCharacter::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.asCharacter::smk::simple test")
test_that("simple test", {
    expect_no_error(ds.asCharacter("D$LAB_TSC"))

    res.class <- ds.class("ascharacter.newobj")
    expect_equal(res.class$sim1, "character")
    expect_equal(res.class$sim2, "character")
    expect_equal(res.class$sim3, "character")
})

test_that("fails if the object does not exist", {
    expect_error(
        ds.asCharacter(x.name = "nonexistent_object"),
        "There are some DataSHIELD errors, list them with datashield.errors()",
        fixed = TRUE
    )

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "The server-side object 'nonexistent_object' does not exist")
    }
})

#
# Done
#

# context("ds.asCharacter::smk::stutdown")

test_that("setup", {
    ds_expect_variables(c("D", "ascharacter.newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.asCharacter::smk::done")
