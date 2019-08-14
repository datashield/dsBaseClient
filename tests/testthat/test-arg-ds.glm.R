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

connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

context("ds.glm::arg::test errors")
test_that("glm_erros", {
    expect_error(ds.glm(), " Please provide a valid regression formula!", fixed=TRUE)
    expect_error(ds.glm("D$LAB_TSC~D$LAB_TSC"), " Please provide a valid 'family' argument!", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
