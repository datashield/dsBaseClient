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

# context("ds.getWGSR::smk::setup")

connect.studies.dataset.anthro(list("age", "sex", "weight", "height", "muac"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.getWGSR::smk")
test_that("simple getWGSR", {
    ds.getWGSR('D$sex', 'D$weight', 'D$height', 'wfh', newobj='newobj.getwgsr')

    res.class <- ds.class('newobj.getwgsr')

    expect_length(res.class, 3)
    expect_length(res.class$study1, 1)
    expect_equal(res.class$study1, 'numeric')
    expect_length(res.class$study2, 1)
    expect_equal(res.class$study2, 'numeric')
    expect_length(res.class$study3, 1)
    expect_equal(res.class$study3, 'numeric')

    res.length <- ds.length('newobj.getwgsr')

    expect_length(res.length, 4)
    expect_length(res.length$`length of newobj.getwgsr in study1`, 1)
    expect_equal(res.length$`length of newobj.getwgsr in study1`, 873)
    expect_length(res.length$`length of newobj.getwgsr in study2`, 1)
    expect_equal(res.length$`length of newobj.getwgsr in study2`, 796)
    expect_length(res.length$`length of newobj.getwgsr in study3`, 1)
    expect_equal(res.length$`length of newobj.getwgsr in study3`, 221)
    expect_length(res.length$`total length of newobj.getwgsr in all studies combined`, 1)
    expect_equal(res.length$`total length of newobj.getwgsr in all studies combined`, 1890)
})

#
# Done
#

test_that("fails if the object does not exist", {
    expect_error(
        ds.getWGSR(sex = "D$sex", firstPart = "nonexistent_object", secondPart = "D$height", index = "hfa"),
        regexp = "DataSHIELD errors"
    )
})

# context("ds.getWGSR::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "newobj.getwgsr"))
})

disconnect.studies.dataset.cnsim()

# context("ds.getWGSR::smk::done")
