#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
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
      "cbindDS", "changeRefGroupDS", "completeCasesDS", "complete.cases", "dataFrameDS", "dataFrameFillDS", "dataFrameSortDS",
      "dataFrameSubsetDS2", "dataFrameDS", "exp", "glmPredictDS.as", "glmSLMADS.assign", "glmSummaryDS.as",
      "lexisDS2", "lexisDS3", "list", "listDS", "log",
      "matrixDS", "matrixDetDS2", "matrixDiagDS", "matrixDimnamesDS", "matrixInvertDS",
      "matrixMultDS", "matrixTransposeDS", "mergeDS", "rBinomDS", "rNormDS", "rPoisDS",
      "rUnifDS", "rbindDS", "reShapeDS", "recodeLevelsDS", "recodeValuesDS2", "repDS", "rep",
      "replaceNaDS", "rowColCalcDS", "seedDS", "seqDS", "subsetByClassDS", "subsetDS", "sum", "sampleDS",
      "tableDS.assign", "tapplyDS.assign", "unlist", "unListDS"
    ))
    aggregate.functions <- factor(c(
      "NROW", "asFactorDS1", "asListDS",
      "checkNegValueDS", "classDS", "colnamesDS", "corDS", "corTestDS",
      "covDS", "dataFrameSubsetDS1",
      "densityGridDS", "dimDS",
      "exists", "glmDS1", "glmDS1", "glmDS2",
      "glmDS2", "glmerSLMADS2", "glmPredictDS.ag", "glmSLMADS1", "glmSLMADS2", "glmSummaryDS.ag",
      "heatmapPlotDS", "histogramDS1", "histogramDS2", "is.character", "is.factor", "is.list", "is.null", "is.numeric",
      "isNaDS", "isValidDS", "kurtosisDS1", "kurtosisDS2", "lengthDS",
      "levelsDS", "lexisDS1", "listDisclosureSettingsDS", "lmerSLMADS2", "lsDS",
      "matrixDetDS1", "meanDS", "meanSdGpDS", "messageDS",
      "namesDS", "numNaDS", "quantileMeanDS", "rangeDS",
      "recodeValuesDS1", "rmDS", "skewnessDS1", "skewnessDS2", "scatterPlotDS",
      "scoreVectDS", "setSeedDS", "t.test", "tTestFDS2",
      "table1DDS", "table2DDS", "tableDS", "tableDS2", "tapplyDS",
      "testObjExistsDS", "varDS"
    ))

    expect_warning(res <- ds.listServersideFunctions(), "'ds.listServersideFunctions' is deprecated.", fixed=TRUE)

    expect_length(res, 2)
    expect_length(res$serverside.assign.functions, 7)
    expect_length(res$serverside.aggregate.functions, 7)

    sim1.assign.res    <- subset(res$serverside.assign.functions, server == 'sim1', c('name'))
    sim1.aggregate.res <- subset(res$serverside.aggregate.functions, server == 'sim1', c('name'))
    sim2.assign.res    <- subset(res$serverside.assign.functions, server == 'sim2', c('name'))
    sim2.aggregate.res <- subset(res$serverside.aggregate.functions, server == 'sim2', c('name'))
    sim3.assign.res    <- subset(res$serverside.assign.functions, server == 'sim3', c('name'))
    sim3.aggregate.res <- subset(res$serverside.aggregate.functions, server == 'sim3', c('name'))

    for (func.name in assign.functions)
        expect_true(func.name %in% sim1.assign.res$name, info = func.name)
    for (func.name in aggregate.functions)
        expect_true(func.name %in% sim1.aggregate.res$name, info = func.name)
    for (func.name in assign.functions)
        expect_true(func.name %in% sim2.assign.res$name, info = func.name)
    for (func.name in aggregate.functions)
        expect_true(func.name %in% sim2.aggregate.res$name, info = func.name)
    for (func.name in assign.functions)
        expect_true(func.name %in% sim3.assign.res$name, info = func.name)
    for (func.name in aggregate.functions)
        expect_true(func.name %in% sim3.aggregate.res$name, info = func.name)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
