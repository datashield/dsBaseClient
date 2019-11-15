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

context("ds.asMatrix::smk::setup")

connect.studies.dataset.cnsim(list("GENDER"))

ds_expect_variables(c("D"))

#
# Tests
#

context("ds.asMatrix::smk::simple test")
test_that("simple test", {
    res <- ds.asMatrix(x.name="D$GENDER")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <D$GENDER.mat> has been created in all specified data sources")
    expect_equal(res$validity.check, "<D$GENDER.mat> appears valid in all sources")
})

#
# Done
#

context("ds.asMatrix::smk::done")

ds_expect_variables(c("D", "D$GENDER.mat"))

disconnect.studies.dataset.cnsim()
