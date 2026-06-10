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

# context("ds.tapply::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

ds.assign('D$LAB_TSC', 'LAB_TSC')
ds.assign('D$GENDER', 'GENDER')

# context("ds.tapply::smk::fun=mean")
test_that("simplest 'ds.tapply', fun=mean", {
    list <- ds.tapply('LAB_TSC', INDEX.names=c('GENDER'), FUN.name='mean')

    expect_length(list, 3)
    expect_length(list$sim1, 2)
    expect_length(list$sim1$Mean, 2)
    expect_equal(list$sim1$Mean[[1]], 5.915789, tolerance=0.0001)
    expect_equal(list$sim1$Mean[[2]], 5.827805, tolerance=0.0001)
    expect_length(list$sim1$N, 2)
    expect_equal(list$sim1$N[[1]], 910)
    expect_equal(list$sim1$N[[2]], 897)
    expect_length(list$sim2, 2)
    expect_length(list$sim2$Mean, 2)
    expect_equal(list$sim2$Mean[[1]], 5.935422, tolerance=0.0001)
    expect_equal(list$sim2$Mean[[2]], 5.748556, tolerance=0.0001)
    expect_length(list$sim2$N, 2)
    expect_equal(list$sim2$N[[1]], 1314)
    expect_equal(list$sim2$N[[2]], 1225)
    expect_length(list$sim3, 2)
    expect_length(list$sim3$Mean, 2)
    expect_equal(list$sim3$Mean[[1]], 5.910215, tolerance=0.0001)
    expect_equal(list$sim3$Mean[[2]], 5.779415, tolerance=0.0001)
    expect_length(list$sim3$N, 2)
    expect_equal(list$sim3$N[[1]], 1779)
    expect_equal(list$sim3$N[[2]], 1700)
})

# context("ds.tapply::smk::fun=sd")
test_that("simplest 'ds.tapply', fun=sd", {
    list <- ds.tapply('LAB_TSC', INDEX.names=c('GENDER'), FUN.name='sd')

    expect_length(list, 3)
    expect_length(list$sim1, 2)
    expect_length(list$sim1$SD, 2)
    expect_equal(list$sim1$SD[[1]], 1.161280, tolerance=0.0001)
    expect_equal(list$sim1$SD[[2]], 1.051423, tolerance=0.0001)
    expect_length(list$sim1$N, 2)
    expect_equal(list$sim1$N[[1]], 910)
    expect_equal(list$sim1$N[[2]], 897)
    expect_length(list$sim2, 2)
    expect_length(list$sim2$SD, 2)
    expect_equal(list$sim2$SD[[1]], 1.092681, tolerance=0.0001)
    expect_equal(list$sim2$SD[[2]], 1.032583, tolerance=0.0001)
    expect_length(list$sim2$N, 2)
    expect_equal(list$sim2$N[[1]], 1314)
    expect_equal(list$sim2$N[[2]], 1225)
    expect_length(list$sim3, 2)
    expect_length(list$sim3$SD, 2)
    expect_equal(list$sim3$SD[[1]], 1.107388, tolerance=0.0001)
    expect_equal(list$sim3$SD[[2]], 1.015554, tolerance=0.0001)
    expect_length(list$sim3$N, 2)
    expect_equal(list$sim3$N[[1]], 1779)
    expect_equal(list$sim3$N[[2]], 1700)
})

# context("ds.tapply::smk::fun=sum")
test_that("simplest 'ds.tapply', fun=sum", {
    list <- ds.tapply('LAB_TSC', INDEX.names=c('GENDER'), FUN.name='sum')

    expect_length(list, 3)
    expect_length(list$sim1, 2)
    expect_length(list$sim1$Sum, 2)
    expect_equal(list$sim1$Sum[[1]], 5383.368, tolerance=0.0001)
    expect_equal(list$sim1$Sum[[2]], 5227.541, tolerance=0.0001)
    expect_length(list$sim1$N, 2)
    expect_equal(list$sim1$N[[1]], 910)
    expect_equal(list$sim1$N[[2]], 897)
    expect_length(list$sim2, 2)
    expect_length(list$sim2$Sum, 2)
    expect_equal(list$sim2$Sum[[1]], 7799.144, tolerance=0.0001)
    expect_equal(list$sim2$Sum[[2]], 7041.981, tolerance=0.0001)
    expect_length(list$sim2$N, 2)
    expect_equal(list$sim2$N[[1]], 1314)
    expect_equal(list$sim2$N[[2]], 1225)
    expect_length(list$sim3, 2)
    expect_length(list$sim3$Sum, 2)
    expect_equal(list$sim3$Sum[[1]], 10514.273, tolerance=0.0001)
    expect_equal(list$sim3$Sum[[2]], 9825.005, tolerance=0.0001)
    expect_length(list$sim3$N, 2)
    expect_equal(list$sim3$N[[1]], 1779)
    expect_equal(list$sim3$N[[2]], 1700)
})

# context("ds.tapply::smk::fun=quantile")
test_that("simplest 'ds.tapply', fun=quantile", {
    list <- ds.tapply('LAB_TSC', INDEX.names=c('GENDER'), FUN.name='quantile')

    expect_length(list, 3)
    expect_length(list$sim1, 2)
    expect_length(list$sim1$GENDER.1, 15)
    expect_length(list$sim1$GENDER.2, 15)
    expect_length(list$sim2, 2)
    expect_length(list$sim2$GENDER.1, 15)
    expect_length(list$sim2$GENDER.2, 15)
    expect_length(list$sim3, 2)
    expect_length(list$sim3$GENDER.1, 15)
    expect_length(list$sim3$GENDER.2, 15)
})

#
# Tear down
#

# context("ds.tapply::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "GENDER", "LAB_TSC"))
})

disconnect.studies.dataset.cnsim()

# context("ds.tapply::smk::done")
