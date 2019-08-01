#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa,
#               2018 University of Newcastle upon Tyne. All rights reserved.
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

# context("dsBetaTestClient::ds.length:args test")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC"))

#
# Tests
#

context("ds.length::arg::test errors")
test_that("length_erros", {
    ds.asMatrix(x='D$LAB_TSC', newobj="not_a_numeric")

    expect_error(ds.length(), "Please provide the name of the input vector!", fixed=TRUE)
    expect_error(ds.length(x='D$LAB_TSC', type='datashield'), 'Function argument "type" has to be either "both", "combine" or "split"', fixed=TRUE)
    expect_error(ds.length(check=TRUE), "Please provide the name of the input vector!", fixed=TRUE)
    expect_error(ds.length(x='D$LAB_TSC', type='datashield', check=TRUE), 'Function argument "type" has to be either "both", "combine" or "split"', fixed=TRUE)
    expect_error(ds.length(x='not_a_numeric', checks=TRUE), "The input object must be a character, factor, integer, logical or numeric vector or a list.", fixed=TRUE)
})

#
# Done
#

# context("dsBetaTestClient::ds.length:arg done")