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

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG", "LAB_HDL"))

#
# Tests
#

context("ds.look::smk")
test_that("simple look", {
    res <- ds.look("lengthDS('D$LAB_TSC')")

    expect_length(res, 1)
    expect_length(res$output, 3)
    expect_equal(res$output$sim1, 1)
    expect_equal(res$output$sim2, 1)
    expect_equal(res$output$sim3, 1)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
