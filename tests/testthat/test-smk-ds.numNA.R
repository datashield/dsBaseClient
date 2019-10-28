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

connect.studies.dataset.cnsim(list("LAB_HDL", "LAB_TRIG", "DIS_CVA"))

#
# Tests
#

context("ds.numNA::smk")
test_that("simple numNA", {
    res1 <- ds.numNA(x='D$LAB_HDL')

    expect_length(res1, 3)
    expect_equal(res1$sim1, 360)
    expect_equal(res1$sim2, 555)
    expect_equal(res1$sim3, 655)

    res2 <- ds.numNA(x='D$LAB_TRIG')

    expect_length(res2, 3)
    expect_equal(res2$sim1, 362)
    expect_equal(res2$sim2, 562)
    expect_equal(res2$sim3, 655)

    res3 <- ds.numNA(x='D$DIS_CVA')

    expect_length(res3, 3)
    expect_equal(res3$sim1, 0)
    expect_equal(res3$sim2, 0)
    expect_equal(res3$sim3, 0)
})

#
# Done
#

disconnect.studies.dataset.cnsim()