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

# context("ds.mean::arg::test errors")
test_that("mean_erros", {
    ds.asCharacter(x='D$LAB_TSC', newobj="not_a_numeric")

    expect_error(ds.mean(), "Please provide the name of the input object!", fixed=TRUE)
    expect_error(ds.mean(x='D$LAB_TSC', type='datashield'), 'Function argument "type" has to be either "both", "combine" or "split"', fixed=TRUE)
    expect_error(ds.mean(x='not_a_numeric', checks=TRUE), "The input object must be an integer or a numeric vector.", fixed=TRUE)
})

#context("ds.mean::arg::discordant errors")
#test_that("mean_discordant", {
#    res <- ds.mean("D$A")
#
#    print("====")
#    print(res)
#    print("====")
#
#    expect_length(res, 1)
#    expect_equal(res, "")
#})

#
# Done
#

disconnect.studies.dataset.cnsim()
