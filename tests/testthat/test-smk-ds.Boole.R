#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

# context("ds.Boole::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.Boole::smk::variable")
test_that("simple boole, variable", {
    res <- ds.Boole("D$LAB_TSC", "D$LAB_TRIG", "==")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <boole.newobj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<boole.newobj> appears valid in all sources")

    res.boole <- ds.summary("boole.newobj")

    expect_length(res.boole, 3)

    expect_equal(res.boole$sim1$class, "numeric")
    expect_equal(res.boole$sim1$length, 2163)
    expect_length(res.boole$sim1$`quantiles & mean`, 8)
    expect_equal(res.boole$sim1$`quantiles & mean`[[1]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[2]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[3]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[4]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[5]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[6]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[7]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[8]], 0.0)
    
    expect_equal(res.boole$sim2$class, "numeric")
    expect_equal(res.boole$sim2$length, 3088)
    expect_length(res.boole$sim2$`quantiles & mean`, 8)
    expect_equal(res.boole$sim2$`quantiles & mean`[[1]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[2]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[3]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[4]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[5]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[6]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[7]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[8]], 0.0)
    
    expect_equal(res.boole$sim3$class, "numeric")
    expect_equal(res.boole$sim3$length, 4128)
    expect_length(res.boole$sim3$`quantiles & mean`, 8)
    expect_equal(res.boole$sim3$`quantiles & mean`[[1]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[2]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[3]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[4]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[5]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[6]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[7]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[8]], 0.0)
})

# context("ds.Boole::smk::small neg constant V2")
test_that("simple boole, small neg constant V2", {
    res <- ds.Boole("D$LAB_TRIG", "-1", "<", numeric.output = TRUE, newobj = "boole01.obj")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <boole01.obj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<boole01.obj> appears valid in all sources")

    res.boole <- ds.summary("boole01.obj")

    expect_length(res.boole, 3)
    expect_equal(res.boole$sim1$class, "numeric")
    expect_equal(res.boole$sim1$length, 2163)
    expect_length(res.boole$sim1$`quantiles & mean`, 8)
    expect_equal(res.boole$sim1$`quantiles & mean`[[1]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[2]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[3]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[4]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[5]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[6]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[7]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[8]], 0.02220988)
    
    expect_equal(res.boole$sim2$class, "numeric")
    expect_equal(res.boole$sim2$length, 3088)
    expect_length(res.boole$sim2$`quantiles & mean`, 8)
    expect_equal(res.boole$sim2$`quantiles & mean`[[1]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[2]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[3]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[4]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[5]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[6]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[7]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[8]], 0.02454473)

    expect_equal(res.boole$sim3$class, "numeric")
    expect_equal(res.boole$sim3$length, 4128)
    expect_length(res.boole$sim3$`quantiles & mean`, 8)
    expect_equal(res.boole$sim3$`quantiles & mean`[[1]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[2]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[3]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[4]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[5]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[6]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[7]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[8]], 0.023322781)
})

# context("ds.Boole::smk::big neg constant V2")
test_that("simple boole, big neg constant V2", {
    res <- ds.Boole("D$LAB_TRIG", "-10", "<", numeric.output = TRUE, newobj = "boole02.obj")
  
    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <boole02.obj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<boole02.obj> appears valid in all sources")

    res.boole <- ds.summary("boole02.obj")

    expect_length(res.boole, 3)
    expect_equal(res.boole$sim1$class, "numeric")
    expect_equal(res.boole$sim1$length, 2163)
    expect_length(res.boole$sim1$`quantiles & mean`, 8)
    expect_equal(res.boole$sim1$`quantiles & mean`[[1]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[2]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[3]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[4]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[5]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[6]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[7]], 0.0)
    expect_equal(res.boole$sim1$`quantiles & mean`[[8]], 0.0)

    expect_equal(res.boole$sim2$class, "numeric")
    expect_equal(res.boole$sim2$length, 3088)
    expect_length(res.boole$sim2$`quantiles & mean`, 8)
    expect_equal(res.boole$sim2$`quantiles & mean`[[1]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[2]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[3]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[4]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[5]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[6]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[7]], 0.0)
    expect_equal(res.boole$sim2$`quantiles & mean`[[8]], 0.0)

    expect_equal(res.boole$sim3$class, "numeric")
    expect_equal(res.boole$sim3$length, 4128)
    expect_length(res.boole$sim3$`quantiles & mean`, 8)
    expect_equal(res.boole$sim3$`quantiles & mean`[[1]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[2]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[3]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[4]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[5]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[6]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[7]], 0.0)
    expect_equal(res.boole$sim3$`quantiles & mean`[[8]], 0.0)
})

#
# Done
#

# context("ds.Boole::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "boole.newobj", "boole01.obj", "boole02.obj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.Boole::smk::done")
