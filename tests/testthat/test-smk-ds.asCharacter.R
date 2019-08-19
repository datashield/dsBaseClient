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

context("ds.asCharacter::smk::simple test")
test_that("simple test", {
    res <- ds.asCharacter("D$LAB_TSC")

    expect_equal(length(res), 2)
    expect_equal(res$is.object.created, "A data object <D$LAB_TSC.char> has been created in all specified data sources")
    expect_equal(res$validity.check, "<D$LAB_TSC.char> appears valid in all sources")
})

#
# Done
#

disconnect.studies.dataset.cnsim()
