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

connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

context("ds.message::smk")
test_that("request message", {
    message.res <- ds.message('Test')

    expect_length(message.res, 3)
    expect_equal(message.res$sim1, "Error: the object <message.object.name> does not exist in this datasource")
    expect_equal(message.res$sim2, "Error: the object <message.object.name> does not exist in this datasource")
    expect_equal(message.res$sim3, "Error: the object <message.object.name> does not exist in this datasource")
})

#
# Done
#

disconnect.studies.dataset.cnsim()
