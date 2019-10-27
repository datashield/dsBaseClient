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

context("ds.gee::arg::test errors")
test_that("gee_erros", {
    expect_error(ds.gee(), "data=NULL; please provide the name of the data frame that holds the variables! \nTip: Use function 'ds.cbind' to coerce the variables into a data frame", fixed=TRUE)
    expect_error(ds.gee(data="D"), "Please provide the name of the column that holds the cluster IDs!", fixed=TRUE)
    expect_error(ds.gee(data="D", formula="response~1+sex+age.60"), "Please provide the name of the column that holds the cluster IDs!", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
