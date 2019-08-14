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

connect.studies.dataset.cnsim(list("LAB_HDL"))

#
# Tests
#

context("ds.isNA::smk")
res <- ds.isNA(x='D$LAB_HDL')
test_that("isNA", {
    expect_false(res$sim1)
    expect_false(res$sim1)
    expect_false(res$sim1)
})

#
# Tear down
#

disconnect.studies.dataset.cnsim()
