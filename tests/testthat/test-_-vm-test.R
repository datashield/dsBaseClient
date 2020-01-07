#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
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

init.all.datasets()

#
# Tests
#

#connect to a server
context("VM problems")
test_that("The virtual machine is loaded. ",
{      
    expect_true(url.exists(ds.test_env$ping_address, timeout=5))
})

if (ds.test_env$context == ds.test_env$contexts[1])
{
    log.in.data.server()
}

test_that("The number of servers the same has setup",
{
    expect_true(length(ds.test_env$connection.opal) == length(ds.test_env$server))
})

#
# Done
#

if (ds.test_env$context == ds.test_env$contexts[1])
{
    log.out.data.server()
}
