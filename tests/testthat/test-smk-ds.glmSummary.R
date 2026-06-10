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

# context("ds.glmSummary::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG", "DIS_AMI", "DIS_DIAB", "GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.glmSummary::smk::gaussian")
test_that("simple glmSummary, gaussian, without newobj", {
    glmSLMA.res <- ds.glmSLMA('D$LAB_TSC~D$LAB_TRIG', family="gaussian", newobj="gaussian.glmslma.obj")

    expect_length(glmSLMA.res, 9)
    expect_equal(glmSLMA.res$num.valid.studies, 3)
    expect_length(glmSLMA.res$validity.check, 1)
    expect_equal(glmSLMA.res$validity.check, "<gaussian.glmslma.obj> appears valid in all sources")

    res <- ds.glmSummary("gaussian.glmslma.obj")

    expect_length(res, 3)
    expect_length(res$sim1, 2)
    expect_length(res$sim1$glm.obj, 31)
    expect_true('glm' %in% class(res$sim1$glm.obj))
    expect_true('lm' %in% class(res$sim1$glm.obj))
    expect_length(res$sim1$glm.summary.obj, 18)
    expect_equal(class(res$sim1$glm.summary.obj), 'summary.glm')
    expect_length(res$sim2, 2)
    expect_length(res$sim2$glm.obj, 31)
    expect_true('glm' %in% class(res$sim2$glm.obj))
    expect_true('lm' %in% class(res$sim2$glm.obj))
    expect_length(res$sim2$glm.summary.obj, 18)
    expect_equal(class(res$sim2$glm.summary.obj), 'summary.glm')
    expect_length(res$sim3, 2)
    expect_length(res$sim3$glm.obj, 31)
    expect_true('glm' %in% class(res$sim3$glm.obj))
    expect_true('lm' %in% class(res$sim3$glm.obj))
    expect_length(res$sim3$glm.summary.obj, 18)
    expect_equal(class(res$sim3$glm.summary.obj), 'summary.glm')
})

test_that("simple glmSummary, gaussian", {
    glmSLMA.res <- ds.glmSLMA('D$LAB_TSC~D$LAB_TRIG', family="gaussian", newobj="gaussian.glmslma.obj")

    expect_length(glmSLMA.res, 9)
    expect_equal(glmSLMA.res$num.valid.studies, 3)
    expect_length(glmSLMA.res$validity.check, 1)
    expect_equal(glmSLMA.res$validity.check, "<gaussian.glmslma.obj> appears valid in all sources")

    res <- ds.glmSummary("gaussian.glmslma.obj", newobj="gaussian.glmsummary.obj")

    expect_length(res, 3)
    expect_length(res$sim1, 2)
    expect_length(res$sim1$glm.obj, 31)
    expect_true('glm' %in% class(res$sim1$glm.obj))
    expect_true('lm' %in% class(res$sim1$glm.obj))
    expect_length(res$sim1$glm.summary.obj, 18)
    expect_equal(class(res$sim1$glm.summary.obj), 'summary.glm')
    expect_length(res$sim2, 2)
    expect_length(res$sim2$glm.obj, 31)
    expect_true('glm' %in% class(res$sim2$glm.obj))
    expect_true('lm' %in% class(res$sim2$glm.obj))
    expect_length(res$sim2$glm.summary.obj, 18)
    expect_equal(class(res$sim2$glm.summary.obj), 'summary.glm')
    expect_length(res$sim3, 2)
    expect_length(res$sim3$glm.obj, 31)
    expect_true('glm' %in% class(res$sim3$glm.obj))
    expect_true('lm' %in% class(res$sim3$glm.obj))
    expect_length(res$sim3$glm.summary.obj, 18)
    expect_equal(class(res$sim3$glm.summary.obj), 'summary.glm')
})

# context("ds.glmSummary::smk::poisson")
test_that("simple glmSummary, poisson, without newobj", {
    glmSLMA.res <- ds.glmSLMA('D$LAB_TSC~D$LAB_TRIG', family="poisson", newobj="poisson.glmslma.obj")

    expect_length(glmSLMA.res, 9)
    expect_equal(glmSLMA.res$num.valid.studies, 3)
    expect_length(glmSLMA.res$validity.check, 1)
    expect_equal(glmSLMA.res$validity.check, "<poisson.glmslma.obj> appears valid in all sources")

    res <- ds.glmSummary("poisson.glmslma.obj")

    expect_length(res, 3)
    expect_length(res$sim1, 2)
    expect_length(res$sim1$glm.obj, 31)
    expect_true('glm' %in% class(res$sim1$glm.obj))
    expect_true('lm' %in% class(res$sim1$glm.obj))
    expect_length(res$sim1$glm.summary.obj, 18)
    expect_equal(class(res$sim1$glm.summary.obj), 'summary.glm')
    expect_length(res$sim2, 2)
    expect_length(res$sim2$glm.obj, 31)
    expect_true('glm' %in% class(res$sim2$glm.obj))
    expect_true('lm' %in% class(res$sim2$glm.obj))
    expect_length(res$sim2$glm.summary.obj, 18)
    expect_equal(class(res$sim2$glm.summary.obj), 'summary.glm')
    expect_length(res$sim3, 2)
    expect_length(res$sim3$glm.obj, 31)
    expect_true('glm' %in% class(res$sim3$glm.obj))
    expect_true('lm' %in% class(res$sim3$glm.obj))
    expect_length(res$sim3$glm.summary.obj, 18)
    expect_equal(class(res$sim3$glm.summary.obj), 'summary.glm')
})

test_that("simple glmSummary, poisson", {
    glmSLMA.res <- ds.glmSLMA('D$LAB_TSC~D$LAB_TRIG', family="poisson", newobj="poisson.glmslma.obj")

    expect_length(glmSLMA.res, 9)
    expect_equal(glmSLMA.res$num.valid.studies, 3)
    expect_length(glmSLMA.res$validity.check, 1)
    expect_equal(glmSLMA.res$validity.check, "<poisson.glmslma.obj> appears valid in all sources")

    res <- ds.glmSummary("poisson.glmslma.obj", newobj="poisson.glmsummary.obj")

    expect_length(res, 3)
    expect_length(res$sim1, 2)
    expect_length(res$sim1$glm.obj, 31)
    expect_true('glm' %in% class(res$sim1$glm.obj))
    expect_true('lm' %in% class(res$sim1$glm.obj))
    expect_length(res$sim1$glm.summary.obj, 18)
    expect_equal(class(res$sim1$glm.summary.obj), 'summary.glm')
    expect_length(res$sim2, 2)
    expect_length(res$sim2$glm.obj, 31)
    expect_true('glm' %in% class(res$sim2$glm.obj))
    expect_true('lm' %in% class(res$sim2$glm.obj))
    expect_length(res$sim2$glm.summary.obj, 18)
    expect_equal(class(res$sim2$glm.summary.obj), 'summary.glm')
    expect_length(res$sim3, 2)
    expect_length(res$sim3$glm.obj, 31)
    expect_true('glm' %in% class(res$sim3$glm.obj))
    expect_true('lm' %in% class(res$sim3$glm.obj))
    expect_length(res$sim3$glm.summary.obj, 18)
    expect_equal(class(res$sim3$glm.summary.obj), 'summary.glm')
})

#
# Shutdown
#

# context("ds.glmSummary::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "gaussian.glmslma.obj", "gaussian.glmsummary.obj", "poisson.glmslma.obj", "poisson.glmsummary.obj", "summary_glm.newobj"))
})

disconnect.studies.dataset.cnsim()

#
# Done
#

# context("ds.glmSummary::smk::done")
