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

connect.studies.dataset.cnsim(list("DIS_AMI", "LAB_TSC"))

#
# Tests
#

# context("ds.changeRefGroup::arg::test errors")
test_that("changeRefGroup_erros", {
    expect_error(ds.changeRefGroup(), "Please provide the name of a vector of type factor!")
    expect_error(ds.changeRefGroup(x="D$DIS_AMI"), " You must indicate a reference level - set the parameter 'ref'.")
    expect_error(ds.changeRefGroup(x="D$LAB_TSC", ref="LAB_TSC"), "The input vector must be a factor!")
})

#
# Done
#

disconnect.studies.dataset.cnsim()
