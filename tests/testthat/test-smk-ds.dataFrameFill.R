#-------------------------------------------------------------------------------
# Copyright (c) 2019 University of Newcastle upon Tyne. All rights reserved.
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

connect.studies.dataset.discordant(list("A", "B", "C"))

#
# Tests
#

context("ds.dataFrameFill::smk::overright a filled dataframe")
test_that("dataFrameFill_exists", {
    res <- ds.dataFrameFill(df.name="D", newobj="filled_df")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <filled_df> has been created in all specified data sources")
    expect_equal(res$validity.check, "<filled_df> appears valid in all sources")
})

#
# Done
#

disconnect.studies.dataset.discordant()
