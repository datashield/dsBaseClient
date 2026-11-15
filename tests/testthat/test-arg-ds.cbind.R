#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

# context("ds.cbind::arg::test errors")
test_that("cbind_erros", {
    ds.asList(x='D$LAB_TSC', newobj="as_list")

    expect_error(ds.cbind(), "must be a vector of the names of the input elements", fixed=TRUE)
    expect_error(ds.cbind(x="as_list", DataSHIELD.checks=TRUE), "Each object must be of class", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
