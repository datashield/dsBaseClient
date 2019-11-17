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
        "ds.test_env",
        "checkClass",
        "colPercent",
        "ds.asCharacter",
        "ds.asDataMatrix",
        "ds.asFactor",
        "ds.asInteger",
        "ds.asList",
        "ds.asLogical",
        "ds.asMatrix",
        "ds.asNumeric",
        "ds.assign",
        "ds.Boole",
        "ds.c",
        "ds.cbind",
        "ds.changeRefGroup",
        "ds.class",
        "ds.colnames",
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
        "ds.exists",
        "ds.exp",
        "ds.glm",
        "ds.glmSLMA",
        "ds.heatmapPlot",
        "ds.histogram",
        "ds.isNA",
        "ds.isValid",
        "ds.length",
        "ds.levels",
        "ds.lexis",
        "ds.list",
        "ds.listClientsideFunctions",
        "ds.listDisclosureSettings",
        "ds.listOpals",
        "ds.listServersideFunctions",
        "ds.log",
        "ds.look",
        "ds.ls",
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
        "ds.names",
        "ds.numNA",
        "ds.quantileMean",
        "ds.rbind",
        "ds.rBinom",
        "ds.recodeLevels",
        "ds.recodeValues",
        "ds.replaceNA",
        "ds.reShape",
        "ds.rm",
        "ds.rNorm",
        "ds.rowColCalc",
        "ds.rPois",
        "ds.rUnif",
        "ds.scatterPlot",
        "ds.seq",
        "ds.setDefaultOpals",
        "ds.setSeed",
        "ds.subset",
        "ds.subsetByClass",
        "ds.summary",
        "ds.table1D",
        "ds.table2D",
        "ds.tapply",
        "ds.tapply.assign",
        "ds.testObjExists",
        "ds.unList",
        "ds.var",
        "ds.vectorCalc",
        "extract",
        "findLoginObjects",
        "getOpals",
        "getPooledMean",
        "getPooledVar",
        "glmChecks",
        "init.object.list.global.environment",
        "init.object.list.testing.environment",
        "init.opal.list",
        "isAssigned",
        "isDefined",
        "library.dynam.unload",
        "logical2int",
        "meanByClassHelper0a",
        "meanByClassHelper0b",
        "meanByClassHelper1",
        "meanByClassHelper2",
        "meanByClassHelper3",
        "meanByClassHelper4",
        "rowPercent",
        "subsetHelper",
        "system.file"
    )

    expect_output(res <- ds.listClientsideFunctions(), "*")

    expect_length(res, 112)
    for (x in c(1:112)) {
        expect_equal(res[[x]], output[[x]])
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
