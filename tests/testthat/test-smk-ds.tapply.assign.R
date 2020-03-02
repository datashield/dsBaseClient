#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa,
#               2019-2020 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.tapply.assign::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

ds.assign('D$LAB_TSC', 'LAB_TSC')
ds.assign('D$GENDER', 'GENDER')

context("ds.tapply.assign::smk::mean")
test_that("simplest 'ds.tapply.assign, mean'", {
    res <- ds.tapply.assign(X.name='LAB_TSC', INDEX.names=c('GENDER'), FUN.name='mean', newobj='temp1.obj')

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <temp1.obj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<temp1.obj> appears valid in all sources")
})

context("ds.tapply.assign::smk::sd")
test_that("simplest 'ds.tapply.assign, sd'", {
    res <- ds.tapply.assign(X.name='LAB_TSC', INDEX.names=c('GENDER'), FUN.name='sd', newobj='temp2.obj')

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <temp2.obj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<temp2.obj> appears valid in all sources")
})

context("ds.tapply.assign::smk::sum")
test_that("simplest 'ds.tapply.assign, sum'", {
    res <- ds.tapply.assign(X.name='LAB_TSC', INDEX.names=c('GENDER'), FUN.name='sum', newobj='temp3.obj')

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <temp3.obj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<temp3.obj> appears valid in all sources")
})

context("ds.tapply.assign::smk::quantile")
test_that("simplest 'ds.tapply.assign, quantile'", {
    res <- ds.tapply.assign(X.name='LAB_TSC', INDEX.names=c('GENDER'), FUN.name='quantile', newobj='temp4.obj')

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <temp4.obj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<temp4.obj> appears valid in all sources")
})

#
# Tear down
#

context("ds.tapply.assign::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "temp1.obj", "temp2.obj", "temp3.obj", "temp4.obj", "current.factor", "GENDER", "LAB_TSC"))
})

disconnect.studies.dataset.cnsim()

context("ds.tapply.assign::smk::done")
