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

connect.studies.dataset.cnsim(list("DIS_CVA","GENDER"))

#
# Tests
#

# context("ds.table1D::arg::test errors")
test_that("table1D_erros", {
    expect_error(expect_warning(ds.table1D(), "'ds.table1D' is deprecated.", fixed = TRUE), "Please provide the name of the input vector!", fixed=TRUE)
    expect_error(expect_warning(ds.table1D(x='D$GENDER', type="datashield"), "'ds.table1D' is deprecated.", fixed = TRUE), "Function argument 'type' has to be either 'combine' or 'split'", fixed=TRUE)
})

#
# Tear down
#

disconnect.studies.dataset.cnsim()
