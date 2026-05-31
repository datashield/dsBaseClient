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

# context("ds.var::arg::test errors")
test_that("var_erros", {
    ds.asCharacter(x='D$LAB_TSC', newobj="not_a_numeric")

    expect_error(ds.var(), "Please provide the name of the input object!", fixed=TRUE)
    expect_error(ds.var(x="not_a_numeric", checks=TRUE), "The input object must be an integer or a numeric vector.", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
