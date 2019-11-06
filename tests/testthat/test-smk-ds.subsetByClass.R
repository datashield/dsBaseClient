#-------------------------------------------------------------------------------
# Copyright (c) 2018 University of Newcastle upon Tyne. All rights reserved.
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

connect.studies.dataset.cnsim(list("LAB_TSC", "GENDER"))

#
# Tests
#

context("ds.subsetByClass::smk")
test_that("gender", {
    res <- ds.subsetByClass(x='D', subsets='subclasses')

    expect_true(is.null(res))

    check <- ds.names('subclasses')

    expect_length(check, 3)
    expect_length(check$sim1, 2)
    expect_equal(check$sim1[1], 'GENDER.level_0')
    expect_equal(check$sim1[2], 'GENDER.level_1')
    expect_length(check$sim2, 2)
    expect_equal(check$sim2[1], 'GENDER.level_0')
    expect_equal(check$sim2[2], 'GENDER.level_1')
    expect_length(check$sim3, 2)
    expect_equal(check$sim3[1], 'GENDER.level_0')
    expect_equal(check$sim3[2], 'GENDER.level_1')
})

#
# Tear down
#

disconnect.studies.dataset.cnsim()
