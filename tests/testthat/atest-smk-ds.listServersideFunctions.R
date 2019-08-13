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

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL"))

#
# Tests
#

context("ds.listServersideFunctions::smk::check results")
test_that("check results", {
    assign.functions <- factor(c(
      "BooleDS", "as.character", "as.null", "as.numeric", "asCharacterDS", "asDataMatrixDS",
      "asFactorDS", "asFactorDS2", "asIntegerDS", "asListDS", "asLogicalDS", "asMatrixDS",
      "asMatrixDS", "asNumericDS", "attach", "c", "cDS", "cbind",
      "cbindDS", "changeRefGroupDS", "complete.cases", "dataFrameDS", "dataFrameSortDS", "dataFrameSubsetDS2",
      "dataframeDS", "exp", "lexisDS", "lexisDS2", "lexisDS3", "list",
      "listDS", "log", "matrixDS", "matrixDetDS2", "matrixDiagDS", "matrixDimnamesDS", "matrixInvertDS",
      "matrixMultDS", "matrixTransposeDS", "mergeDS", "rBinomDS", "rNormDS", "rPoisDS",
      "rUnifDS", "rbindDS", "reShapeDS", "recodeLevelsDS", "recodeValuesDS2", "rep",
      "replaceNaDS", "rowColCalcDS", "seedDS", "seqDS", "subsetByClassDS", "subsetDS", "sum",
      "tapplyDS.assign", "unlist"
    ))
    aggregate.functions <- factor(c(
      "NROW", "alphaPhiDS", "asFactorDS1", "asListDS",
      "checkNegValueDS", "class", "colnames", "cor.test",
      "corDS", "covDS", "covDS", "dataFrameSubsetDS1",
      "densityGridDS", "dim", "dimDS", "dimDS",
      "exists", "glmDS1", "glmDS1", "glmDS2",
      "glmDS2", "glmSLMADS2", "histogramDS", "histogramDS", "is.character",
      "is.factor", "is.list", "is.null", "is.numeric",
      "isNaDS", "isValidDS", "length", "lengthDS",
      "levels", "lexisDS1", "listDisclosureSettingsDS", "ls",
      "matrixDetDS1", "meanDS", "meanDS", "meanSdGpDS", "messageDS",
      "namesDS", "numNaDS", "quantileMeanDS", "rangeDS",
      "recodeValuesDS1", "rilmDS.b", "rmDS", "scatterPlotDS",
      "scoreVectDS", "setSeedDS", "t.test", "tTestFDS2",
      "table1dDS", "table2DDS", "table2dDS", "tapplyDS", "testObjExistsDS",
      "unListDS", "varDS", "varDS"
    ))

    res <- ds.listServersideFunctions()

    expect_length(res, 2)
    expect_length(res$serverside.assign.functions, 3)
    expect_length(res$serverside.aggregate.functions, 3)

    sim1.assign.res    <- res$serverside.assign.functions$sim1
    sim1.aggregate.res <- res$serverside.aggregate.functions$sim1
    sim2.assign.res    <- res$serverside.assign.functions$sim2
    sim2.aggregate.res <- res$serverside.aggregate.functions$sim2
    sim3.assign.res    <- res$serverside.assign.functions$sim3
    sim3.aggregate.res <- res$serverside.aggregate.functions$sim3

    expect_length(sim1.assign.res, 58)
    expect_equal(sim1.assign.res, assign.functions)
    expect_length(sim1.aggregate.res, 62)
    expect_equal(sim1.aggregate.res, aggregate.functions)
    expect_length(sim2.assign.res, 58)
    expect_equal(sim2.assign.res, assign.functions)
    expect_length(sim2.aggregate.res, 62)
    expect_equal(sim2.aggregate.res, aggregate.functions)
    expect_length(sim3.assign.res, 58)
    expect_equal(sim3.assign.res, assign.functions)
    expect_length(sim3.aggregate.res, 62)
    expect_equal(sim3.aggregate.res, aggregate.functions)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
