#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa,
#               2018 University of Newcastle upon Tyne. All rights reserved.
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

# context("dsBetaTestClient::ds.asLogical::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("GENDER"))

#
# Tests
#

# context("dsBetaTestClient::ds.asLogical::smk simple test")
context("ds.asLogical::smk::simple test")

test_that("simple test", {
    res <- ds.asLogical("D$GENDER")

    expect_equal(length(res), 2)
    expect_equal(res$is.object.created, "A data object <D$GENDER.logic> has been created in all specified data sources")
    expect_equal(res$validity.check, "<D$GENDER.logic> appears valid in all sources")
})

#
# Done
#

# context("dsBetaTestClient::ds.asLogical 1::smk done")
