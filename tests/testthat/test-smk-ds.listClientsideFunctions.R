#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

context("ds.listClientsideFunctions::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.listClientsideFunctions::smk::check results")
test_that("check results", {
    output <- list(
        "ds.abs",
        "ds.asCharacter",
        "ds.asDataMatrix",
        "ds.asFactor",
        "ds.asFactorSimple",
        "ds.asInteger",
        "ds.asList",
        "ds.asLogical",
        "ds.asMatrix",
        "ds.asNumeric",
        "ds.assign",
        "ds.Boole",
        "ds.boxPlot",
        "ds.c",
        "ds.cbind",
        "ds.changeRefGroup",
        "ds.class",
        "ds.colnames",
        "ds.completeCases",
        "ds.contourPlot",
        "ds.cor",
        "ds.corTest",
        "ds.cov",
        "ds.dataFrame",
        "ds.dataFrameFill",
        "ds.dataFrameSort",
        "ds.dataFrameSubset",
        "ds.densityGrid",
        "ds.dim",
        "ds.dmtC2S",
        "ds.elspline",
        "ds.exists",
        "ds.exp",
        "ds.forestplot",
        "ds.getWGSR",
        "ds.glm",
        "ds.glmerSLMA",
        "ds.glmPredict",
        "ds.glmSLMA",
        "ds.glmSummary",
        "ds.heatmapPlot",
        "ds.hetcor",
        "ds.histogram",
        "ds.isNA",
        "ds.isValid",
        "ds.kurtosis",
        "ds.length",
        "ds.levels",
        "ds.lexis",
        "ds.list",
        "ds.listClientsideFunctions",
        "ds.listDisclosureSettings",
        "ds.listOpals",
        "ds.listServersideFunctions",
        "ds.lmerSLMA",
        "ds.log",
        "ds.look",
        "ds.ls",
        "ds.lspline",
        "ds.make",
        "ds.matrix",
        "ds.matrixDet",
        "ds.matrixDet.report",
        "ds.matrixDiag",
        "ds.matrixDimnames",
        "ds.matrixInvert",
        "ds.matrixMult",
        "ds.matrixTranspose",
        "ds.mean",
        "ds.meanByClass",
        "ds.meanSdGp",
        "ds.merge",
        "ds.message",
        "ds.metadata",
        "ds.names",
        "ds.numNA",
        "ds.qlspline",
        "ds.quantileMean",
        "ds.ranksSecure",
        "ds.rbind",
        "ds.rBinom",
        "ds.recodeLevels",
        "ds.recodeValues",
        "ds.rep",
        "ds.replaceNA",
        "ds.reShape",
        "ds.rm",
        "ds.rNorm",
        "ds.rowColCalc",
        "ds.rPois",
        "ds.rUnif",
        "ds.sample",
        "ds.scatterPlot",
        "ds.seq",
        "ds.setDefaultOpals",
        "ds.setSeed",
        "ds.skewness",
        "ds.sqrt",
        "ds.subset",
        "ds.subsetByClass",
        "ds.summary",
        "ds.table",
        "ds.table1D",
        "ds.table2D",
        "ds.tapply",
        "ds.tapply.assign",
        "ds.testObjExists",
        "ds.unique",
        "ds.unList",
        "ds.var",
        "ds.vectorCalc"
    )

    expect_output(res <- ds.listClientsideFunctions(), "*")

    for (func.name in output) {
        expect_true(func.name %in% res, info = func.name)
    }
})

#
# Done
#

context("ds.listClientsideFunctions::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

context("ds.listClientsideFunctions::smk::done")
