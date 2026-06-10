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

# context("ds.auc::arg::setup")

#
# Tests
#

# context("ds.auc::arg::test errors")

test_that("ds.auc errors for null pred or y", {
  
    expect_error(ds.auc(pred=NULL, y='y'), "Please provide the name of the vector with predicted values", fixed=TRUE)
    expect_error(ds.auc(pred='pred', y=NULL), "Please provide the name of the outcome variable", fixed=TRUE)
  
})

#
# Done
#

# context("ds.auc::arg::shutdown")
# context("ds.auc::arg::done")
