#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa,
#               2019-2022 University of Newcastle upon Tyne. All rights reserved.
#               2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

# context("ds.tapply.assign::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

ds.assign('D$LAB_TSC', 'LAB_TSC')
ds.assign('D$GENDER', 'GENDER')

# context("ds.tapply.assign::smk::fun=mean")
test_that("simplest 'ds.tapply.assign', fun=mean", {
    list <- ds.tapply.assign('LAB_TSC', INDEX.names=c('GENDER'), FUN.name='mean', newobj="fun_mean.newobj")

    expect_length(list, 2)
    expect_equal(list$is.object.created, "A data object <fun_mean.newobj> has been created in all specified data sources", fixed=TRUE)
    expect_equal(list$validity.check, "<fun_mean.newobj> appears valid in all sources", fixed=TRUE)

    res.length <- ds.length("fun_mean.newobj")
    expect_length(res.length, 4)
    expect_equal(res.length$`length of fun_mean.newobj in sim1`, 2)
    expect_equal(res.length$`length of fun_mean.newobj in sim2`, 2)
    expect_equal(res.length$`length of fun_mean.newobj in sim3`, 2)
    expect_equal(res.length$`total length of fun_mean.newobj in all studies combined`, 6)
})

# context("ds.tapply.assign::smk::fun=sd")
test_that("simplest 'ds.tapply.assign', fun=sd", {
    list <- ds.tapply.assign('LAB_TSC', INDEX.names=c('GENDER'), FUN.name='sd', newobj="fun_sd.newobj")

    expect_length(list, 2)
    expect_equal(list$is.object.created, "A data object <fun_sd.newobj> has been created in all specified data sources", fixed=TRUE)
    expect_equal(list$validity.check, "<fun_sd.newobj> appears valid in all sources", fixed=TRUE)

    res.length <- ds.length("fun_sd.newobj")
    expect_length(res.length, 4)
    expect_equal(res.length$`length of fun_sd.newobj in sim1`, 2)
    expect_equal(res.length$`length of fun_sd.newobj in sim2`, 2)
    expect_equal(res.length$`length of fun_sd.newobj in sim3`, 2)
    expect_equal(res.length$`total length of fun_sd.newobj in all studies combined`, 6)
})

# context("ds.tapply.assign::smk::fun=sum")
test_that("simplest 'ds.tapply.assign', fun=sum", {
    list <- ds.tapply.assign('LAB_TSC', INDEX.names=c('GENDER'), FUN.name='sum', newobj="fun_sum.newobj")

    expect_length(list, 2)
    expect_equal(list$is.object.created, "A data object <fun_sum.newobj> has been created in all specified data sources", fixed=TRUE)
    expect_equal(list$validity.check, "<fun_sum.newobj> appears valid in all sources", fixed=TRUE)

    res.length <- ds.length("fun_sum.newobj")
    expect_length(res.length, 4)
    expect_equal(res.length$`length of fun_sum.newobj in sim1`, 2)
    expect_equal(res.length$`length of fun_sum.newobj in sim2`, 2)
    expect_equal(res.length$`length of fun_sum.newobj in sim3`, 2)
    expect_equal(res.length$`total length of fun_sum.newobj in all studies combined`, 6)
})

# context("ds.tapply.assign::smk::fun=quantile")
test_that("simplest 'ds.tapply.assign', fun=quantile", {
    list <- ds.tapply.assign('LAB_TSC', INDEX.names=c('GENDER'), FUN.name='quantile', newobj="fun_quantile.newobj")

    expect_length(list, 2)
    expect_equal(list$is.object.created, "A data object <fun_quantile.newobj> has been created in all specified data sources", fixed=TRUE)
    expect_equal(list$validity.check, "<fun_quantile.newobj> appears valid in all sources", fixed=TRUE)

    res.length <- ds.length("fun_quantile.newobj")
    expect_length(res.length, 4)
    expect_equal(res.length$`length of fun_quantile.newobj in sim1`, 2)
    expect_equal(res.length$`length of fun_quantile.newobj in sim2`, 2)
    expect_equal(res.length$`length of fun_quantile.newobj in sim3`, 2)
    expect_equal(res.length$`total length of fun_quantile.newobj in all studies combined`, 6)
})

#
# Tear down
#

# context("ds.tapply.assign::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "GENDER", "LAB_TSC", "fun_mean.newobj", "fun_sd.newobj", "fun_sum.newobj", "fun_quantile.newobj"))
})

disconnect.studies.dataset.cnsim()

#
# Done
#

# context("ds.tapply.assign::smk::done")
