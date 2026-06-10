#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

# context("ds.message::arg")
test_that("request message for NULL", {
    expect_error(expect_warning(ds.message(NULL), "'ds.message' is deprecated.", fixed = TRUE), "Please provide the name of the studyside list object that holds the message\n in character format ie: 'object.name' in inverted commas", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
