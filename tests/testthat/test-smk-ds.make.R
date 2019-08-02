#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa,
#               2019 University of Newcastle upon Tyne. All rights reserved.
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

# context("dsBetaTestClient::ds.make::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC", "LAB_TRIG", "LAB_HDL"))

#
# Tests
#

context("ds.make::smk")
test_that("simple make", {
    res <- ds.make("(D$LAB_TSC*D$LAB_TRIG*D$LAB_HDL)", "maded.rand")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <maded.rand> has been created in all specified data sources")
    expect_equal(res$validity.check, "<maded.rand> appears valid in all sources")
})

#
# Done
#

# context("dsBetaTestClient::ds.make::smk done")
