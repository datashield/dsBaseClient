#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
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

# context("ds.recodeValues::smk_dgr::setup")

connect.studies.dataset.survival(list("time.id"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.recodeValues::smk_dgr::simple numeric")
test_that("simple test", {
    res <- ds.recodeValues("D$time.id", values2replace.vector=c(1,2), new.values.vector=c(10,20), newobj="time.id")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <time.id> has been created in all specified data sources")
    expect_equal(res$validity.check, "<time.id> appears valid in all sources")

    ds.dataFrame(c('D$time.id'), newobj='odf')
    odf <- ds.DANGERdfEXTRACT('odf')

    odf.survival1 <- odf$study.specific.df$survival1
    odf.survival2 <- odf$study.specific.df$survival2
    odf.survival3 <- odf$study.specific.df$survival3
    
    expect_length(odf.survival1$time.id, 2060)
    expect_length(odf.survival2$time.id, 1640)
    expect_length(odf.survival3$time.id, 2688)
    
    expect_length((subset(odf.survival1, time.id == 1))$time.id, 886)
    expect_length((subset(odf.survival1, time.id == 2))$time.id, 385)
    expect_length((subset(odf.survival1, time.id == 10))$time.id, 0)
    expect_length((subset(odf.survival1, time.id == 20))$time.id, 0)

    expect_length((subset(odf.survival2, time.id == 1))$time.id, 659)
    expect_length((subset(odf.survival2, time.id == 2))$time.id, 309)
    expect_length((subset(odf.survival2, time.id == 10))$time.id, 0)
    expect_length((subset(odf.survival2, time.id == 20))$time.id, 0)
    
    expect_length((subset(odf.survival3, time.id == 1))$time.id, 1167)
    expect_length((subset(odf.survival3, time.id == 2))$time.id, 486)
    expect_length((subset(odf.survival3, time.id == 10))$time.id, 0)
    expect_length((subset(odf.survival3, time.id == 20))$time.id, 0)

    ds.dataFrame(c('time.id'), newobj='ndf')
    ndf <- ds.DANGERdfEXTRACT('ndf')
    
    ndf.survival1 <- ndf$study.specific.df$survival1
    ndf.survival2 <- ndf$study.specific.df$survival2
    ndf.survival3 <- ndf$study.specific.df$survival3

    expect_length(ndf.survival1$time.id, 2060)
    expect_length(ndf.survival2$time.id, 1640)
    expect_length(ndf.survival3$time.id, 2688)

    expect_length((subset(ndf.survival1, time.id == 1))$time.id, 0)
    expect_length((subset(ndf.survival1, time.id == 2))$time.id, 0)
    expect_length((subset(ndf.survival1, time.id == 10))$time.id, 886)
    expect_length((subset(ndf.survival1, time.id == 20))$time.id, 385)
    
    expect_length((subset(ndf.survival2, time.id == 1))$time.id, 0)
    expect_length((subset(ndf.survival2, time.id == 2))$time.id, 0)
    expect_length((subset(ndf.survival2, time.id == 10))$time.id, 659)
    expect_length((subset(ndf.survival2, time.id == 20))$time.id, 309)
    
    expect_length((subset(ndf.survival3, time.id == 1))$time.id, 0)
    expect_length((subset(ndf.survival3, time.id == 2))$time.id, 0)
    expect_length((subset(ndf.survival3, time.id == 10))$time.id, 1167)
    expect_length((subset(ndf.survival3, time.id == 20))$time.id, 486)
})

#
# Done
#
# context("ds.recodeValues::smk_dgr::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "time.id", "odf", "ndf"))
})

disconnect.studies.dataset.survival()

# context("ds.recodeValues::smk_dgr::done")
