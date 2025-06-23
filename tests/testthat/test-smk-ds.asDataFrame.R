#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.asDataFrame::smk::setup")

connect.studies.dataset.cnsim(list("GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.asDataFrame::smk::simple test")
test_that("simple test", {
    ds.asDataMatrix("D$GENDER")
    ds.asDataFrame(x.name="asdatamatrix.newobj")
    res.class <- ds.class("asdataframe.newobj")
    print(res.class)
    expect_length(res.class, 3)
    expect_length(res.class$sim1, 1)
    expect_true("data.frame" %in% res.class$sim1)
    expect_length(res.class$sim2, 1)
    expect_true("data.frame" %in% res.class$sim2)
    expect_length(res.class$sim3, 1)
    expect_true("data.frame" %in% res.class$sim3)
})

#
# Done
#

context("ds.asDataFrame::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "asdatamatrix.newobj", "asdataframe.newobj"))
})

disconnect.studies.dataset.cnsim()

context("ds.asDataFrame::smk::done")

