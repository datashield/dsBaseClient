#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up phase 1
#

context("ds.lmerSLMA::smk::setup phase 1")

connect.studies.dataset.cluster.int(list("incid_rate", "trtGrp", "Male", "idDoctor", "BMI", "idSurgery"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.lmerSLMA::smk::phase 1")
test_that("simple lmerSLMA", {
    res <- ds.lmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor)', dataName = "D")

    expect_length(res, 8)
})



## try some different formulae structures?
test_that("alternative formulae for nested groups", {
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idSurgery/idDoctor)', dataName = "D")
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idSurgery) +(1|idSurgery:idDoctor)', dataName = "D")
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    # different behaviour for normal DS versus DSLite...
    #res = ds.lmerSLMA(formula = 'D$BMI ~ D$trtGrp + D$Male + (1|D$idSurgery)')
    #expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idDoctor)', dataName = "D", combine.with.metafor = FALSE)
    expect_length(res, 5)
})


test_that("server side error", {
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idSurgery)', dataName = 'D', optimizer = 'nloptwrap')
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    res=ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idSurgery)', dataName = 'D', optimizer = 'not_this_one')
    expect_equal(res$errorMessage, "ERROR: the only optimizer currently available for lmer is 'nloptwrap', please respecify")
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idDoctor)', dataName = "D", REML = FALSE)
    expect_equal(res$output.summary$study1$methTitle, "Linear mixed model fit by maximum likelihood ")
})

test_that("test offsets and weights", {
    ds.make('D$BMI/D$BMI', "some.weights")
    ds.make('D$BMI/D$BMI', "some.offsets")
    ds.dataFrame(x=c("D", "some.weights", "some.offsets"), newobj = "D2")
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idDoctor)', weights = "some.weights", dataName = "D")
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idDoctor)', offset = "some.offsets", dataName = "D")
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idDoctor)', weights = "D2$some.weights", dataName = "D")
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idDoctor)', offset = "D2$some.offsets", dataName = "D")
    expect_equal(res$Convergence.error.message[2], "Study2: no convergence error reported")
    
})

#
# Shutdown
#

context("ds.lmerSLMA::smk::shutdown phase 1")

test_that("shutdown", {
    #note the offset and weights objects below are artefacts 

    ds_expect_variables(c("D", "D2", "offset", "some.offsets", "some.weights", "weights"))
})

disconnect.studies.dataset.cluster.int()

#
# Set up
#

context("ds.lmerSLMA::smk::setup phase 2")

connect.studies.dataset.cluster.slo(list("incid_rate", "trtGrp", "Male", "idDoctor", "BMI", "idSurgery"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.lmerSLMA::smk::test phase 2")

test_that("check slope formulae", {
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idDoctor) + (1|idSurgery) + (0+trtGrp|idSurgery)', dataName = 'D', control_type = 'check.conv.grad',control_value = 0.1)

    expect_length(res, 8)
    expect_length(res$output.summary, 5)
    expect_equal(class(res$output.summary), "list")
    expect_length(res$num.valid.studies, 1)
    expect_equal(class(res$num.valid.studies), "numeric")
    expect_length(res$betamatrix.all, 9)
    if (base::getRversion() < '4.0.0')
    {
        expect_length(class(res$betamatrix.all), 1)
        expect_true("matrix" %in% class(res$betamatrix.all))
    }
    else
    {
        expect_length(class(res$betamatrix.all), 2)
        expect_true("matrix" %in% class(res$betamatrix.all))
        expect_true("array" %in% class(res$betamatrix.all))
    }
    expect_length(res$sematrix.all, 9)
    if (base::getRversion() < '4.0.0')
    {
        expect_length(class(res$sematrix.all), 1)
        expect_true("matrix" %in% class(res$sematrix.all))
    }
    else
    {
        expect_length(class(res$sematrix.all), 2)
        expect_true("matrix" %in% class(res$sematrix.all))
        expect_true("array" %in% class(res$sematrix.all))
    }
    expect_length(res$betamatrix.valid, 9)
    if (base::getRversion() < '4.0.0')
    {
        expect_length(class(res$betamatrix.valid), 1)
        expect_true("matrix" %in% class(res$betamatrix.valid))
    }
    else
    {
        expect_length(class(res$betamatrix.valid), 2)
        expect_true("matrix" %in% class(res$betamatrix.valid))
        expect_true("array" %in% class(res$betamatrix.valid))
    }
    expect_length(res$sematrix.valid, 9)
    if (base::getRversion() < '4.0.0')
    {
        expect_length(class(res$sematrix.valid), 1)
        expect_true("matrix" %in% class(res$sematrix.valid))
    }
    else
    {
        expect_length(class(res$sematrix.valid), 2)
        expect_true("matrix" %in% class(res$sematrix.valid))
        expect_true("array" %in% class(res$sematrix.valid))
    }
    expect_length(res$SLMA.pooled.ests.matrix, 18)
    if (base::getRversion() < '4.0.0')
    {
        expect_length(class(res$SLMA.pooled.ests.matrix), 1)
        expect_true("matrix" %in% class(res$SLMA.pooled.ests.matrix))
    }
    else
    {
        expect_length(class(res$SLMA.pooled.ests.matrix), 2)
        expect_true("matrix" %in% class(res$SLMA.pooled.ests.matrix))
        expect_true("array" %in% class(res$SLMA.pooled.ests.matrix))
    }
    expect_length(res$Convergence.error.message, 3)
    expect_equal(class(res$Convergence.error.message), "character")
})

test_that("check slope formulae", {
    res = ds.lmerSLMA(formula = 'BMI ~ trtGrp + Male + (1|idDoctor) + (trtGrp||idSurgery)', dataName = 'D', control_type = 'check.conv.grad',control_value = 0.1)

    expect_length(res, 8)
    expect_length(res$output.summary, 5)
    expect_equal(class(res$output.summary), "list")
    expect_length(res$num.valid.studies, 1)
    expect_equal(class(res$num.valid.studies), "numeric")
    expect_length(res$betamatrix.all, 9)
    if (base::getRversion() < '4.0.0')
    {
        expect_length(class(res$betamatrix.all), 1)
        expect_true("matrix" %in% class(res$betamatrix.all))
    }
    else
    {
        expect_length(class(res$betamatrix.all), 2)
        expect_true("matrix" %in% class(res$betamatrix.all))
        expect_true("array" %in% class(res$betamatrix.all))
    }
    expect_length(res$sematrix.all, 9)
    if (base::getRversion() < '4.0.0')
    {
        expect_length(class(res$sematrix.all), 1)
        expect_true("matrix" %in% class(res$sematrix.all))
    }
    else
    {
        expect_length(class(res$sematrix.all), 2)
        expect_true("matrix" %in% class(res$sematrix.all))
        expect_true("array" %in% class(res$sematrix.all))
    }
    expect_length(res$betamatrix.valid, 9)
    if (base::getRversion() < '4.0.0')
    {
        expect_length(class(res$betamatrix.valid), 1)
        expect_true("matrix" %in% class(res$betamatrix.valid))
    }
    else
    {
        expect_length(class(res$betamatrix.valid), 2)
        expect_true("matrix" %in% class(res$betamatrix.valid))
        expect_true("array" %in% class(res$betamatrix.valid))
    }
    expect_length(res$sematrix.valid, 9)
    if (base::getRversion() < '4.0.0')
    {
        expect_length(class(res$sematrix.valid), 1)
        expect_true("matrix" %in% class(res$sematrix.valid))
    }
    else
    {
        expect_length(class(res$sematrix.valid), 2)
        expect_true("matrix" %in% class(res$sematrix.valid))
        expect_true("array" %in% class(res$sematrix.valid))
    }
    expect_length(res$SLMA.pooled.ests.matrix, 18)
    if (base::getRversion() < '4.0.0')
    {
        expect_length(class(res$SLMA.pooled.ests.matrix), 1)
        expect_true("matrix" %in% class(res$SLMA.pooled.ests.matrix))
    }
    else
    {
        expect_length(class(res$SLMA.pooled.ests.matrix), 2)
        expect_true("matrix" %in% class(res$SLMA.pooled.ests.matrix))
        expect_true("array" %in% class(res$SLMA.pooled.ests.matrix))
    }
    expect_length(res$Convergence.error.message, 3)
    expect_equal(class(res$Convergence.error.message), "character")
})

#
# Shutdown
#

context("ds.lmerSLMA::smk::shutdown phase 2")

test_that("shutdown", {
    ds_expect_variables(c("D", "offset", "weights"))
})

disconnect.studies.dataset.cluster.slo()

#
# Done
#

context("ds.lmerSLMA::smk::done")
