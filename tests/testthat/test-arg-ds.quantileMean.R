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

connect.studies.dataset.cnsim(list('LAB_HDL'))

#
# Tests
#

# context("ds.quantileMean::arg::test errors")
ds.asCharacter(x='D$LAB_HDL', newobj="not_a_numeric")
test_that("quantileMean_erros", {
    expect_error(ds.quantileMean(), "Please provide the name of the input vector!", fixed=TRUE)
    expect_error(ds.quantileMean(x='D$LAB_HDL', type='datashield'), 'Function argument "type" has to be either "combine" or "split"', fixed=TRUE)
    expect_error(ds.quantileMean(x='not_a_numeric'), "The input object must be an integer or numeric vector.", fixed=TRUE)
})

#
# Tear down
#

disconnect.studies.dataset.cnsim()
