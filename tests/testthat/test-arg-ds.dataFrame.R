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

# context("dsBetaTestClient::ds.dataFrame:args test")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC"))

#
# Tests
#

context("ds.dataFrame::arg::test errors")
test_that("dataFrame_erros", {
    expect_error(ds.dataFrame(), "Please provide the name of the list that holds the input vectors!", fixed=TRUE)

    ds.asList(x='D$A', newobj="as_list")
    expect_error(ds.dataFrame("as_list", DataSHIELD.checks=TRUE), " Only objects of type 'data.frame', 'matrix', 'numeric', 'integer', 'character', 'factor' and 'logical' are allowed.", fixed=TRUE)
})

#
# Done
#

# context("dsBetaTestClient::ds.dataFrame:arg done")
