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

connect.discordant.dataset.simple(list("A", "B", "C"))

#
# Tests
#

# context("ds.asCharacter::arg::test errors")
test_that("asCharacter_erros", {
    expect_error(ds.asCharacter(), "Please provide the name of the input vector!", fixed=TRUE)
})

# context("ds.asCharacter::arg::discordant errors")
# test_that("asCharacter_discordant", {
#     res <- ds.asCharacter("D$A")
#
#     print("====")
#     print(res)
#     print("====")
#
#     expect_length(res, )
#     expect_equal(res, "")
# })

#
# Done
#

disconnect.discordant.dataset.simple()
