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

connect.studies.dataset.cnsim(list("LAB_TSC", "GENDER"))

#
# Tests
#

context("ds.tapply::smk::fun=length")
test_that("simplest 'ds.tapply', fun=length", {
    list <- ds.tapply('D$LAB_TSC', INDEX.names=c('D$GENDER'), FUN.name='length')

    expect_true(length(list) == 0)
})

context("ds.tapply::smk::fun=mean")
test_that("simplest 'ds.tapply', fun=mean", {
    list <- ds.tapply('D$LAB_TSC', INDEX.names=c('D$GENDER'), FUN.name='mean')

    expect_true(length(list) == 0)
})

context("ds.tapply::smk::fun=sd")
test_that("simplest 'ds.tapply', fun=sd", {
    list <- ds.tapply('D$LAB_TSC', INDEX.names=c('D$GENDER'), FUN.name='sd')

    expect_true(length(list) == 0)
})

context("ds.tapply::smk::fun=sum")
test_that("simplest 'ds.tapply', fun=sum", {
    list <- ds.tapply('D$LAB_TSC', INDEX.names=c('D$GENDER'), FUN.name='sum')

    expect_true(length(list) == 0)
})

context("ds.tapply::smk::fun=quantile")
test_that("simplest 'ds.tapply', fun=quantile", {
    list <- ds.tapply('D$LAB_TSC', INDEX.names=c('D$GENDER'), FUN.name='quantile')

    expect_true(length(list) == 0)
})

#
# Tear down
#

disconnect.studies.dataset.cnsim()
