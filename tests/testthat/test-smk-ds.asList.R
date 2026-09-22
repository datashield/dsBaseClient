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

# context("ds.asList::smk::setup")

connect.studies.dataset.cnsim(list("GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.asList::smk::simple test")
test_that("simple test", {
    expect_no_error(ds.asList(x.name="D$GENDER"))

    res.class <- ds.class("aslist.newobj")
    expect_equal(res.class$sim1, "list")
    expect_equal(res.class$sim2, "list")
    expect_equal(res.class$sim3, "list")
})

test_that("no data is returned to the client", {
    res <- ds.asList(x.name = "D$GENDER", newobj = "gender.list")

    expect_null(res)
})

test_that("a data.frame is written to the server as a named list of its columns", {
    ds.asList(x.name = "D", newobj = "df.list")

    res.class <- ds.class("df.list")
    for (study.class in res.class) {
        expect_equal(study.class, "list")
    }

    res.length <- ds.length("df.list", type = "split")
    for (study.length in res.length) {
        expect_equal(study.length, 1)
    }

    res.names <- ds.names("df.list")
    for (study.names in res.names) {
        expect_equal(study.names, "GENDER")
    }
})

test_that("fails if the object does not exist", {
    expect_error(
        ds.asList(x.name = "nonexistent_object"),
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

# context("ds.asList::smk::shutdown")

test_that("stutdown", {
    ds_expect_variables(c("D", "aslist.newobj", "gender.list", "df.list"))
})

disconnect.studies.dataset.cnsim()

# context("ds.asList::smk::done")
