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

# context("dsBetaTestClient::ds.meanSdGp::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.survival(list("age.60", "female"))

#
# Tests
#

context("ds.meanSdGp::smk::type=split")
test_that("meanSdGp values [split]", {
    stat.meanSdGp <- ds.meanSdGp(x='D$age.60', y='D$female', type='split')

    expect_length(stat.meanSdGp, 7)
    expect_equal(class(stat.meanSdGp$Mean_gp_study), "matrix")
    expect_equal(class(stat.meanSdGp$StDev_gp_study), "matrix")
    expect_equal(class(stat.meanSdGp$Nvalid_gp_study), "matrix")
    expect_equal(class(stat.meanSdGp$SEM_gp_study), "matrix")
    expect_equal(class(stat.meanSdGp$Total_Nvalid), "numeric")
    expect_equal(class(stat.meanSdGp$Total_Nmissing), "numeric")
    expect_equal(class(stat.meanSdGp$Total_Ntotal), "numeric")
})

context("ds.meanSdGp::smk::type=combine")
test_that("meanSdGp values [combine]", {
    stat.meanSdGp <- ds.meanSdGp(x='D$age.60', y='D$female', type='combine')

    expect_length(stat.meanSdGp, 7)
    expect_equal(class(stat.meanSdGp$Mean_gp), "matrix")
    expect_equal(class(stat.meanSdGp$StDev_gp), "matrix")
    expect_equal(class(stat.meanSdGp$Nvalid_gp), "matrix")
    expect_equal(class(stat.meanSdGp$SEM_gp), "matrix")
    expect_equal(class(stat.meanSdGp$Total_Nvalid), "numeric")
    expect_equal(class(stat.meanSdGp$Total_Nmissing), "numeric")
    expect_equal(class(stat.meanSdGp$Total_Ntotal), "numeric")
})

context("ds.meanSdGp::smk::type=both")
test_that("meanSdGp values [both]", {
    stat.meanSdGp <- ds.meanSdGp(x='D$age.60', y='D$female', type='both')

    expect_length(stat.meanSdGp, 7)
    expect_equal(class(stat.meanSdGp$Mean_gp_study), "matrix")
    expect_equal(class(stat.meanSdGp$StDev_gp_study), "matrix")
    expect_equal(class(stat.meanSdGp$Nvalid_gp_study), "matrix")
    expect_equal(class(stat.meanSdGp$SEM_gp_study), "matrix")
    expect_equal(class(stat.meanSdGp$Total_Nvalid), "numeric")
    expect_equal(class(stat.meanSdGp$Total_Nmissing), "numeric")
    expect_equal(class(stat.meanSdGp$Total_Ntotal), "numeric")
})

#
# Done
#

# context("dsBetaTestClient::ds.meanSdGp::smk done")
