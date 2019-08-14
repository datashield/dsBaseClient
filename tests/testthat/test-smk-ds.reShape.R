#-------------------------------------------------------------------------------
# Copyright (c) 2019 University of Newcastle upon Tyne. All rights reserved.
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

connect.studies.dataset.survival(list("id", "study.id", "time.id", "cens", "age.60", "female"))

#
# Tests
#

context("ds.reShape::smk")
test_that("simplest ds.reShape, wide", {
    res <- ds.reShape(data.name="D", v.names="age.60", timevar.name="time.id", idvar.name="id", direction="wide", newobj="reshape1_obj")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <reshape1_obj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<reshape1_obj> appears valid in all sources")
})

# test_that("simplest ds.reShape, long", {
#     res <- ds.reShape(data.name="D", v.names="age.60", timevar.name="time.id", idvar.name="id", direction="long", newobj="reshape2_obj")
#
#     expect_length(res, 2)
#     expect_equal(res$is.object.created, "A data object <reshape2_obj> has been created in all specified data sources")
#     expect_equal(res$validity.check, "<reshape2_obj> appears valid in all sources")
# })

#
# Done
#

disconnect.studies.dataset.survival()
