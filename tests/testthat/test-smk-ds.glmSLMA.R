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

# context("dsBetaTestClient::ds.glmSLMA::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC", "LAB_TRIG", "GENDER"))

#
# Tests
#

context("ds.glmSLMA::smk::gaussian")
test_that("simple glmSLMA, gaussian", {
    glmSLMA.res <- ds.glmSLMA('D$LAB_TSC~D$LAB_TRIG', family="gaussian")

    expect_length(glmSLMA.res, 6)
    expect_equal(class(glmSLMA.res$input.beta.matrix.for.SLMA), "matrix")
    expect_equal(class(glmSLMA.res$input.se.matrix.for.SLMA), "matrix")
    expect_equal(class(glmSLMA.res$SLMA.pooled.estimates), "matrix")
    expect_length(glmSLMA.res$study1, 29)
    expect_equal(class(glmSLMA.res$study1$family), "family")
    expect_equal(class(glmSLMA.res$study1$coefficients), "matrix")
    expect_equal(glmSLMA.res$study1$rank, 2)
    expect_equal(glmSLMA.res$study1$aic, 5460.549, tolerance = 0.001)
    expect_equal(glmSLMA.res$study1$iter, 2)
    expect_equal(glmSLMA.res$study1$contrasts, NULL)
    expect_equal(glmSLMA.res$study1$dispersion, 1.211496, tolerance = 0.001)
    expect_equal(glmSLMA.res$study1$Ntotal, 2163)
    expect_equal(glmSLMA.res$study1$Nvalid, 1801)
    expect_equal(glmSLMA.res$study1$Nmissing, 362)
    expect_length(glmSLMA.res$study2, 29)
    expect_equal(class(glmSLMA.res$study2$family), "family")
    expect_equal(class(glmSLMA.res$study2$coefficients), "matrix")
    expect_equal(glmSLMA.res$study2$rank, 2)
    expect_equal(glmSLMA.res$study2$aic, 7490.0, tolerance = 0.001)
    expect_equal(glmSLMA.res$study2$iter, 2)
    expect_equal(glmSLMA.res$study2$contrasts, NULL)
    expect_equal(glmSLMA.res$study2$dispersion, 1.13414, tolerance = 0.001)
    expect_equal(glmSLMA.res$study2$Ntotal, 3088)
    expect_equal(glmSLMA.res$study2$Nvalid, 2526)
    expect_equal(glmSLMA.res$study2$Nmissing, 562)
    expect_length(glmSLMA.res$study3, 29)
    expect_equal(class(glmSLMA.res$study3$family), "family")
    expect_equal(class(glmSLMA.res$study3$coefficients), "matrix")
    expect_equal(glmSLMA.res$study3$rank, 2)
    expect_equal(glmSLMA.res$study3$aic, 10256.0, tolerance = 0.001)
    expect_equal(glmSLMA.res$study3$iter, 2)
    expect_equal(glmSLMA.res$study3$contrasts, NULL)
    expect_equal(glmSLMA.res$study3$dispersion, 1.12, tolerance = 0.001)
    expect_equal(glmSLMA.res$study3$Ntotal, 4128)
    expect_equal(glmSLMA.res$study3$Nvalid, 3473)
    expect_equal(glmSLMA.res$study3$Nmissing, 655)
})

#context("ds.glmSLMA::smk::binomial")
#test_that("simple glmSLMA, binomial", {
#    ds.asNumeric('D$GENDER', 'num.gender')
#
#    glmSLMA.res <- ds.glmSLMA('D$LAB_TSC~num.gender', family="binomial")
#
#    expect_length(glmSLMA.res, 4)
#    expect_equal(class(glmSLMA.res$input.beta.matrix.for.SLMA), "matrix")
#    expect_equal(class(glmSLMA.res$input.se.matrix.for.SLMA), "matrix")
#    expect_equal(class(glmSLMA.res$SLMA.pooled.estimates), "matrix")
#    expect_equal(class(glmSLMA.res$study1$family), "family")
#    expect_equal(class(glmSLMA.res$study1$coefficients), "matrix")
#    expect_equal(glmSLMA.res$study1$rank, 2)
#    expect_equal(glmSLMA.res$study1$aic, 5460.549, tolerance = 0.001)
#    expect_equal(glmSLMA.res$study1$iter, 2)
#    expect_equal(glmSLMA.res$study1$contrasts, NULL)
#    expect_equal(glmSLMA.res$study1$dispersion, 1.211496, tolerance = 0.001)
#    expect_equal(glmSLMA.res$study1$Ntotal, 2163)
#    expect_equal(glmSLMA.res$study1$Nvalid, 1801)
#    expect_equal(glmSLMA.res$study1$Nmissing, 362)
#    expect_equal(class(glmSLMA.res$study2$family), "family")
#    expect_equal(class(glmSLMA.res$study2$coefficients), "matrix")
#    expect_equal(glmSLMA.res$study2$rank, 2)
#    expect_equal(glmSLMA.res$study2$aic, 7490.0, tolerance = 0.001)
#    expect_equal(glmSLMA.res$study2$iter, 2)
#    expect_equal(glmSLMA.res$study2$contrasts, NULL)
#    expect_equal(glmSLMA.res$study2$dispersion, 1.13414, tolerance = 0.001)
#    expect_equal(glmSLMA.res$study2$Ntotal, 3088)
#    expect_equal(glmSLMA.res$study2$Nvalid, 2526)
#    expect_equal(glmSLMA.res$study2$Nmissing, 562)
#    expect_equal(class(glmSLMA.res$study3$family), "family")
#    expect_equal(class(glmSLMA.res$study3$coefficients), "matrix")
#    expect_equal(glmSLMA.res$study3$rank, 2)
#    expect_equal(glmSLMA.res$study3$aic, 10256.0, tolerance = 0.001)
#    expect_equal(glmSLMA.res$study3$iter, 2)
#    expect_equal(glmSLMA.res$study3$contrasts, NULL)
#    expect_equal(glmSLMA.res$study3$dispersion, 1.12, tolerance = 0.001)
#    expect_equal(glmSLMA.res$study3$Ntotal, 4128)
#    expect_equal(glmSLMA.res$study3$Nvalid, 3473)
#    expect_equal(glmSLMA.res$study3$Nmissing, 655)
#})

context("ds.glmSLMA::smk::poisson")
test_that("simple glmSLMA, poisson", {
    glmSLMA.res <- ds.glmSLMA('D$LAB_TSC~D$LAB_TRIG', family="poisson")

    expect_equal(class(glmSLMA.res$input.beta.matrix.for.SLMA), "matrix")
    expect_equal(class(glmSLMA.res$input.se.matrix.for.SLMA), "matrix")
    expect_equal(class(glmSLMA.res$SLMA.pooled.estimates), "matrix")
    expect_equal(class(glmSLMA.res$study1$family), "family")
    expect_equal(class(glmSLMA.res$study1$coefficients), "matrix")
    expect_equal(glmSLMA.res$study1$rank, 2)
    expect_equal(glmSLMA.res$study1$aic, Inf, tolerance = 0.001)
    expect_equal(glmSLMA.res$study1$iter, 4)
    expect_equal(glmSLMA.res$study1$contrasts, NULL)
    expect_equal(glmSLMA.res$study1$dispersion, 1.0, tolerance = 0.001)
    expect_equal(glmSLMA.res$study1$Ntotal, 2163)
    expect_equal(glmSLMA.res$study1$Nvalid, 1801)
    expect_equal(glmSLMA.res$study1$Nmissing, 362)
    expect_equal(class(glmSLMA.res$study2$family), "family")
    expect_equal(class(glmSLMA.res$study2$coefficients), "matrix")
    expect_equal(glmSLMA.res$study2$rank, 2)
    expect_equal(glmSLMA.res$study2$aic, Inf, tolerance = 0.001)
    expect_equal(glmSLMA.res$study2$iter, 4)
    expect_equal(glmSLMA.res$study2$contrasts, NULL)
    expect_equal(glmSLMA.res$study2$dispersion, 1.0, tolerance = 0.001)
    expect_equal(glmSLMA.res$study2$Ntotal, 3088)
    expect_equal(glmSLMA.res$study2$Nvalid, 2526)
    expect_equal(glmSLMA.res$study2$Nmissing, 562)
    expect_equal(class(glmSLMA.res$study3$family), "family")
    expect_equal(class(glmSLMA.res$study3$coefficients), "matrix")
    expect_equal(glmSLMA.res$study3$rank, 2)
    expect_equal(glmSLMA.res$study3$aic, Inf, tolerance = 0.001)
    expect_equal(glmSLMA.res$study3$iter, 4)
    expect_equal(glmSLMA.res$study3$contrasts, NULL)
    expect_equal(glmSLMA.res$study3$dispersion, 1.0, tolerance = 0.001)
    expect_equal(glmSLMA.res$study3$Ntotal, 4128)
    expect_equal(glmSLMA.res$study3$Nvalid, 3473)
    expect_equal(glmSLMA.res$study3$Nmissing, 655)
})

#
# Done
#

# context("dsBetaTestClient::ds.glmSLMA::smk done")
