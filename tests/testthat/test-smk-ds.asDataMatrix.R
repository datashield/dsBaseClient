#-------------------------------------------------------------------------------
# Copyright (c) 2018-2021 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.asDataMatrix::smk::setup")

connect.studies.dataset.cnsim(list("GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.asDataMatrix::smk::simple test")
test_that("simple test", {
    res <- ds.asDataMatrix(x.name="D$GENDER")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <asdatamatrix.newobj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<asdatamatrix.newobj> appears valid in all sources")

    res.class <- ds.class("asdatamatrix.newobj")
    expect_length(res.class, 3)
    expect_length(res.class$sim1, 2)
    expect_true("array" %in% res.class$sim1)
    expect_true("matrix" %in% res.class$sim1)
    expect_length(res.class$sim2, 2)
    expect_true("array" %in% res.class$sim2)
    expect_true("matrix" %in% res.class$sim2)
    expect_length(res.class$sim3, 2)
    expect_true("array" %in% res.class$sim3)
    expect_true("matrix" %in% res.class$sim3)
})

#
# Done
#

context("ds.asDataMatrix::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "asdatamatrix.newobj"))
})

disconnect.studies.dataset.cnsim()

context("ds.asDataMatrix::smk::done")

