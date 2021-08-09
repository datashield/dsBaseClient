#-------------------------------------------------------------------------------
# Copyright (c) 2018-2021 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.dataFrameSubset::arg::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.dataFrameSubset::arg::missing 'ds.name' arg errors")
test_that("dataFrameSubset_erros", {
    expect_error(ds.dataFrameSubset(), "Please provide the name of the data.frame to be subsetted as a character string: eg 'xxx'", fixed=TRUE)
})

context("ds.dataFrameSubset::arg::missing 'V1' arg error")
test_that("dataFrameSubset_erros", {
    expect_error(ds.dataFrameSubset(df.name="D"), "Please provide the name of the column or scalar that holds V1 as a character string: eg 'xxx' or '3'", fixed=TRUE)
})

context("ds.dataFrameSubset::arg::missing 'V2' arg error")
test_that("dataFrameSubset_erros", {
    expect_error(ds.dataFrameSubset(df.name="D", V1.name="LAB_TSC"), "Please provide the name of the column or scalar that holds V2 as a character string: eg 'xxx' or '3'", fixed=TRUE)
})

context("ds.dataFrameSubset::arg::missing 'Boolean.operator' arg error")
test_that("dataFrameSubset_erros", {
    expect_error(ds.dataFrameSubset(df.name="D", V1.name="LAB_TSC", V2.name="LAB_TSC"), "Unless you are only subsetting columns, please provide a Boolean operator in character format: eg '==' or '>=' or '<' or '!='. However, if either keep.cols or rm.cols is non-null because you want to subset columns and you specify both V1.name and V2.name as NULL (or 'ONES') and Boolean.operator as NULL,ds.dataFrameSubset will subset out the specified columns but will keep all rows.", fixed=TRUE)
})

context("ds.dataFrameSubset::arg::missing 'df.name' value server-side")
test_that("dataFrameSubset_erros", {
    expect_error(ds.dataFrameSubset(df.name="M", V1.name="A", V2.name="B", Boolean.operator="=/="), "There are some DataSHIELD errors, list them with datashield.errors()", fixed=TRUE)
    
    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 3)
    expect_equal(res.errors$sim1, "Command 'dataFrameSubsetDS1(\"M\", \"A\", \"B\", 0, NULL, NULL, FALSE)' failed on 'sim1': Error while evaluating 'dsBase::dataFrameSubsetDS1(\"M\", \"A\", \"B\", 0, NULL, NULL, FALSE)' -> Error in data.frame(M) : object 'M' not found\n", fixed = TRUE)
    expect_equal(res.errors$sim2, "Command 'dataFrameSubsetDS1(\"M\", \"A\", \"B\", 0, NULL, NULL, FALSE)' failed on 'sim2': Error while evaluating 'dsBase::dataFrameSubsetDS1(\"M\", \"A\", \"B\", 0, NULL, NULL, FALSE)' -> Error in data.frame(M) : object 'M' not found\n", fixed = TRUE)
    expect_equal(res.errors$sim3, "Command 'dataFrameSubsetDS1(\"M\", \"A\", \"B\", 0, NULL, NULL, FALSE)' failed on 'sim3': Error while evaluating 'dsBase::dataFrameSubsetDS1(\"M\", \"A\", \"B\", 0, NULL, NULL, FALSE)' -> Error in data.frame(M) : object 'M' not found\n", fixed = TRUE)
})

context("ds.dataFrameSubset::arg::missing 'V1' value server-side")
test_that("dataFrameSubset_erros", {
    expect_error(ds.dataFrameSubset(df.name="D", V1.name="A", V2.name="B", Boolean.operator="=/="), "There are some DataSHIELD errors, list them with datashield.errors()", fixed=TRUE)
    
    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 3)
    expect_equal(res.errors$sim1, "Command 'dataFrameSubsetDS1(\"D\", \"A\", \"B\", 0, NULL, NULL, FALSE)' failed on 'sim1': Error while evaluating 'dsBase::dataFrameSubsetDS1(\"D\", \"A\", \"B\", 0, NULL, NULL, FALSE)' -> Error in eval(parse(text = V1.name)) : object 'A' not found\n", fixed = TRUE)
    expect_equal(res.errors$sim2, "Command 'dataFrameSubsetDS1(\"D\", \"A\", \"B\", 0, NULL, NULL, FALSE)' failed on 'sim2': Error while evaluating 'dsBase::dataFrameSubsetDS1(\"D\", \"A\", \"B\", 0, NULL, NULL, FALSE)' -> Error in eval(parse(text = V1.name)) : object 'A' not found\n", fixed = TRUE)
    expect_equal(res.errors$sim3, "Command 'dataFrameSubsetDS1(\"D\", \"A\", \"B\", 0, NULL, NULL, FALSE)' failed on 'sim3': Error while evaluating 'dsBase::dataFrameSubsetDS1(\"D\", \"A\", \"B\", 0, NULL, NULL, FALSE)' -> Error in eval(parse(text = V1.name)) : object 'A' not found\n", fixed = TRUE)
})

context("ds.dataFrameSubset::arg::missing 'V2' value server-side")
test_that("dataFrameSubset_erros", {
    expect_error(ds.dataFrameSubset(df.name="D", V1.name="D$LAB_TSC", V2.name="B", Boolean.operator="=/="), "There are some DataSHIELD errors, list them with datashield.errors()", fixed=TRUE)
    
    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 3)
    expect_equal(res.errors$sim1, "Command 'dataFrameSubsetDS1(\"D\", \"D$LAB_TSC\", \"B\", 0, NULL, NULL, FALSE)' failed on 'sim1': Error while evaluating 'dsBase::dataFrameSubsetDS1(\"D\", \"D$LAB_TSC\", \"B\", 0, NULL, NULL, FALSE)' -> Error in eval(parse(text = V2.name)) : object 'B' not found\n", fixed = TRUE)
    expect_equal(res.errors$sim2, "Command 'dataFrameSubsetDS1(\"D\", \"D$LAB_TSC\", \"B\", 0, NULL, NULL, FALSE)' failed on 'sim2': Error while evaluating 'dsBase::dataFrameSubsetDS1(\"D\", \"D$LAB_TSC\", \"B\", 0, NULL, NULL, FALSE)' -> Error in eval(parse(text = V2.name)) : object 'B' not found\n", fixed = TRUE)
    expect_equal(res.errors$sim3, "Command 'dataFrameSubsetDS1(\"D\", \"D$LAB_TSC\", \"B\", 0, NULL, NULL, FALSE)' failed on 'sim3': Error while evaluating 'dsBase::dataFrameSubsetDS1(\"D\", \"D$LAB_TSC\", \"B\", 0, NULL, NULL, FALSE)' -> Error in eval(parse(text = V2.name)) : object 'B' not found\n", fixed = TRUE)
})

context("ds.dataFrameSubset::arg::invalid 'Boolean.operator' value server-side")
test_that("dataFrameSubset_erros", {
    expect_error(ds.dataFrameSubset(df.name="D", V1.name="D$LAB_TSC", V2.name="D$LAB_TSC", Boolean.operator="=/="), "There are some DataSHIELD errors, list them with datashield.errors()", fixed=TRUE)
    
    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 3)
    expect_equal(res.errors$sim1, "Command 'dataFrameSubsetDS1(\"D\", \"D$LAB_TSC\", \"D$LAB_TSC\", 0, NULL, NULL, \n    FALSE)' failed on 'sim1': Error while evaluating 'dsBase::dataFrameSubsetDS1(\"D\", \"D$LAB_TSC\", \"D$LAB_TSC\", 0, NULL, NULL, FALSE)' -> Error : FAILED: Boolean.operator must be: '==', '!=', '<', '<=', '>' or '>='\n", fixed = TRUE)
    expect_equal(res.errors$sim2, "Command 'dataFrameSubsetDS1(\"D\", \"D$LAB_TSC\", \"D$LAB_TSC\", 0, NULL, NULL, \n    FALSE)' failed on 'sim2': Error while evaluating 'dsBase::dataFrameSubsetDS1(\"D\", \"D$LAB_TSC\", \"D$LAB_TSC\", 0, NULL, NULL, FALSE)' -> Error : FAILED: Boolean.operator must be: '==', '!=', '<', '<=', '>' or '>='\n", fixed = TRUE)
    expect_equal(res.errors$sim3, "Command 'dataFrameSubsetDS1(\"D\", \"D$LAB_TSC\", \"D$LAB_TSC\", 0, NULL, NULL, \n    FALSE)' failed on 'sim3': Error while evaluating 'dsBase::dataFrameSubsetDS1(\"D\", \"D$LAB_TSC\", \"D$LAB_TSC\", 0, NULL, NULL, FALSE)' -> Error : FAILED: Boolean.operator must be: '==', '!=', '<', '<=', '>' or '>='\n", fixed = TRUE)
})

#
# Shutdown
#

context("ds.dataFrameSubset::arg::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

#
# Done
#

context("ds.dataFrameSubset::arg::done")
