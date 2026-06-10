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

# context("ds.recodeValues::smk_dgr::factor::setup")

connect.studies.dataset.cnsim(list("GENDER", "PM_BMI_CATEGORICAL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.recodeValues::smk_dgr::factor::simple factor 1")
test_that("simple factor 1", {
    res <- ds.recodeValues("D$GENDER", values2replace.vector=c('0'), new.values.vector=c('2'), newobj="GENDER")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <GENDER> has been created in all specified data sources")
    expect_equal(res$validity.check, "<GENDER> appears valid in all sources")

    ds.dataFrame(c('D$GENDER'), newobj='odf_1')
    odf <- ds.DANGERdfEXTRACT('odf_1')

    odf.sim1 <- odf$study.specific.df$sim1
    odf.sim2 <- odf$study.specific.df$sim2
    odf.sim3 <- odf$study.specific.df$sim3
    
    expect_length(odf.sim1$GENDER, 2163)
    expect_length(odf.sim2$GENDER, 3088)
    expect_length(odf.sim3$GENDER, 4128)
    
    expect_length((subset(odf.sim1, GENDER == 0))$GENDER, 1092)
    expect_length((subset(odf.sim1, GENDER == 1))$GENDER, 1071)
    expect_length((subset(odf.sim1, GENDER == 2))$GENDER, 0)

    expect_length((subset(odf.sim2, GENDER == 0))$GENDER, 1585)
    expect_length((subset(odf.sim2, GENDER == 1))$GENDER, 1503)
    expect_length((subset(odf.sim2, GENDER == 2))$GENDER, 0)

    expect_length((subset(odf.sim3, GENDER == 0))$GENDER, 2091)
    expect_length((subset(odf.sim3, GENDER == 1))$GENDER, 2037)
    expect_length((subset(odf.sim3, GENDER == 2))$GENDER, 0)

    ds.dataFrame(c('GENDER'), newobj='ndf_1')
    ndf <- ds.DANGERdfEXTRACT('ndf_1')
    
    ndf.sim1 <- ndf$study.specific.df$sim1
    ndf.sim2 <- ndf$study.specific.df$sim2
    ndf.sim3 <- ndf$study.specific.df$sim3

    expect_length(ndf.sim1$GENDER, 2163)
    expect_length(ndf.sim2$GENDER, 3088)
    expect_length(ndf.sim3$GENDER, 4128)

    expect_length((subset(ndf.sim1, GENDER == 0))$GENDER, 0)
    expect_length((subset(ndf.sim1, GENDER == 1))$GENDER, 1071)
    expect_length((subset(ndf.sim1, GENDER == 2))$GENDER, 1092)
    
    expect_length((subset(ndf.sim2, GENDER == 0))$GENDER, 0)
    expect_length((subset(ndf.sim2, GENDER == 1))$GENDER, 1503)
    expect_length((subset(ndf.sim2, GENDER == 2))$GENDER, 1585)
    
    expect_length((subset(ndf.sim3, GENDER == 0))$GENDER, 0)
    expect_length((subset(ndf.sim3, GENDER == 1))$GENDER, 2037)
    expect_length((subset(ndf.sim3, GENDER == 2))$GENDER, 2091)
})

# context("ds.recodeValues::smk_dgr::factor::simple factor 2")
test_that("simple factor 2", {
    res <- ds.recodeValues("D$GENDER", values2replace.vector=c(0), new.values.vector=c(2), newobj="GENDER")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <GENDER> has been created in all specified data sources")
    expect_equal(res$validity.check, "<GENDER> appears valid in all sources")

    ds.dataFrame(c('D$GENDER'), newobj='odf_2')
    odf <- ds.DANGERdfEXTRACT('odf_2')

    odf.sim1 <- odf$study.specific.df$sim1
    odf.sim2 <- odf$study.specific.df$sim2
    odf.sim3 <- odf$study.specific.df$sim3
    
    expect_length(odf.sim1$GENDER, 2163)
    expect_length(odf.sim2$GENDER, 3088)
    expect_length(odf.sim3$GENDER, 4128)
    
    expect_length((subset(odf.sim1, GENDER == 0))$GENDER, 1092)
    expect_length((subset(odf.sim1, GENDER == 1))$GENDER, 1071)
    expect_length((subset(odf.sim1, GENDER == 2))$GENDER, 0)

    expect_length((subset(odf.sim2, GENDER == 0))$GENDER, 1585)
    expect_length((subset(odf.sim2, GENDER == 1))$GENDER, 1503)
    expect_length((subset(odf.sim2, GENDER == 2))$GENDER, 0)

    expect_length((subset(odf.sim3, GENDER == 0))$GENDER, 2091)
    expect_length((subset(odf.sim3, GENDER == 1))$GENDER, 2037)
    expect_length((subset(odf.sim3, GENDER == 2))$GENDER, 0)

    ds.dataFrame(c('GENDER'), newobj='ndf_2')
    ndf <- ds.DANGERdfEXTRACT('ndf_2')
    
    ndf.sim1 <- ndf$study.specific.df$sim1
    ndf.sim2 <- ndf$study.specific.df$sim2
    ndf.sim3 <- ndf$study.specific.df$sim3

    expect_length(ndf.sim1$GENDER, 2163)
    expect_length(ndf.sim2$GENDER, 3088)
    expect_length(ndf.sim3$GENDER, 4128)

    expect_length((subset(ndf.sim1, GENDER == 0))$GENDER, 0)
    expect_length((subset(ndf.sim1, GENDER == 1))$GENDER, 1071)
    expect_length((subset(ndf.sim1, GENDER == 2))$GENDER, 1092)
    
    expect_length((subset(ndf.sim2, GENDER == 0))$GENDER, 0)
    expect_length((subset(ndf.sim2, GENDER == 1))$GENDER, 1503)
    expect_length((subset(ndf.sim2, GENDER == 2))$GENDER, 1585)
    
    expect_length((subset(ndf.sim3, GENDER == 0))$GENDER, 0)
    expect_length((subset(ndf.sim3, GENDER == 1))$GENDER, 2037)
    expect_length((subset(ndf.sim3, GENDER == 2))$GENDER, 2091)
})

# context("ds.recodeValues::smk_dgr::factor::simple factor 3")
test_that("simple factor 3", {
    res <- ds.recodeValues("D$GENDER", values2replace.vector=c(0), new.values.vector=c(2), missing='3', newobj="GENDER")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <GENDER> has been created in all specified data sources")
    expect_equal(res$validity.check, "<GENDER> appears valid in all sources")

    ds.dataFrame(c('D$GENDER'), newobj='odf_3')
    odf <- ds.DANGERdfEXTRACT('odf_3')

    odf.sim1 <- odf$study.specific.df$sim1
    odf.sim2 <- odf$study.specific.df$sim2
    odf.sim3 <- odf$study.specific.df$sim3
    
    expect_length(odf.sim1$GENDER, 2163)
    expect_length(odf.sim2$GENDER, 3088)
    expect_length(odf.sim3$GENDER, 4128)
    
    expect_length((subset(odf.sim1, GENDER == 0))$GENDER, 1092)
    expect_length((subset(odf.sim1, GENDER == 1))$GENDER, 1071)
    expect_length((subset(odf.sim1, GENDER == 2))$GENDER, 0)
    expect_length((subset(odf.sim1, GENDER == 3))$GENDER, 0)

    expect_length((subset(odf.sim2, GENDER == 0))$GENDER, 1585)
    expect_length((subset(odf.sim2, GENDER == 1))$GENDER, 1503)
    expect_length((subset(odf.sim2, GENDER == 2))$GENDER, 0)
    expect_length((subset(odf.sim2, GENDER == 3))$GENDER, 0)

    expect_length((subset(odf.sim3, GENDER == 0))$GENDER, 2091)
    expect_length((subset(odf.sim3, GENDER == 1))$GENDER, 2037)
    expect_length((subset(odf.sim3, GENDER == 2))$GENDER, 0)
    expect_length((subset(odf.sim3, GENDER == 3))$GENDER, 0)

    ds.dataFrame(c('GENDER'), newobj='ndf_3')
    ndf <- ds.DANGERdfEXTRACT('ndf_3')
    
    ndf.sim1 <- ndf$study.specific.df$sim1
    ndf.sim2 <- ndf$study.specific.df$sim2
    ndf.sim3 <- ndf$study.specific.df$sim3

    expect_length(ndf.sim1$GENDER, 2163)
    expect_length(ndf.sim2$GENDER, 3088)
    expect_length(ndf.sim3$GENDER, 4128)

    expect_length((subset(ndf.sim1, GENDER == 0))$GENDER, 0)
    expect_length((subset(ndf.sim1, GENDER == 1))$GENDER, 1071)
    expect_length((subset(ndf.sim1, GENDER == 2))$GENDER, 1092)
    expect_length((subset(ndf.sim1, GENDER == 3))$GENDER, 0)
    
    expect_length((subset(ndf.sim2, GENDER == 0))$GENDER, 0)
    expect_length((subset(ndf.sim2, GENDER == 1))$GENDER, 1503)
    expect_length((subset(ndf.sim2, GENDER == 2))$GENDER, 1585)
    expect_length((subset(ndf.sim2, GENDER == 3))$GENDER, 0)
    
    expect_length((subset(ndf.sim3, GENDER == 0))$GENDER, 0)
    expect_length((subset(ndf.sim3, GENDER == 1))$GENDER, 2037)
    expect_length((subset(ndf.sim3, GENDER == 2))$GENDER, 2091)
    expect_length((subset(ndf.sim3, GENDER == 3))$GENDER, 0)
})

# context("ds.recodeValues::smk_dgr::factor::simple factor 4")
test_that("simple factor 4", {
    res <- ds.recodeValues("D$GENDER", values2replace.vector=c(0,1), new.values.vector=c(10,20), newobj="GENDER")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <GENDER> has been created in all specified data sources")
    expect_equal(res$validity.check, "<GENDER> appears valid in all sources")

    ds.dataFrame(c('D$GENDER'), newobj='odf_4')
    odf <- ds.DANGERdfEXTRACT('odf_4')

    odf.sim1 <- odf$study.specific.df$sim1
    odf.sim2 <- odf$study.specific.df$sim2
    odf.sim3 <- odf$study.specific.df$sim3
    
    expect_length(odf.sim1$GENDER, 2163)
    expect_length(odf.sim2$GENDER, 3088)
    expect_length(odf.sim3$GENDER, 4128)
    
    expect_length((subset(odf.sim1, GENDER == 0))$GENDER, 1092)
    expect_length((subset(odf.sim1, GENDER == 1))$GENDER, 1071)
    expect_length((subset(odf.sim1, GENDER == 10))$GENDER, 0)
    expect_length((subset(odf.sim1, GENDER == 20))$GENDER, 0)

    expect_length((subset(odf.sim2, GENDER == 0))$GENDER, 1585)
    expect_length((subset(odf.sim2, GENDER == 1))$GENDER, 1503)
    expect_length((subset(odf.sim2, GENDER == 10))$GENDER, 0)
    expect_length((subset(odf.sim2, GENDER == 20))$GENDER, 0)

    expect_length((subset(odf.sim3, GENDER == 0))$GENDER, 2091)
    expect_length((subset(odf.sim3, GENDER == 1))$GENDER, 2037)
    expect_length((subset(odf.sim3, GENDER == 10))$GENDER, 0)
    expect_length((subset(odf.sim3, GENDER == 20))$GENDER, 0)

    ds.dataFrame(c('GENDER'), newobj='ndf_4')
    ndf <- ds.DANGERdfEXTRACT('ndf_4')
    
    ndf.sim1 <- ndf$study.specific.df$sim1
    ndf.sim2 <- ndf$study.specific.df$sim2
    ndf.sim3 <- ndf$study.specific.df$sim3

    expect_length(ndf.sim1$GENDER, 2163)
    expect_length(ndf.sim2$GENDER, 3088)
    expect_length(ndf.sim3$GENDER, 4128)

    expect_length((subset(ndf.sim1, GENDER == 0))$GENDER, 0)
    expect_length((subset(ndf.sim1, GENDER == 1))$GENDER, 0)
    expect_length((subset(ndf.sim1, GENDER == 10))$GENDER, 1092)
    expect_length((subset(ndf.sim1, GENDER == 20))$GENDER, 1071)
    
    expect_length((subset(ndf.sim2, GENDER == 0))$GENDER, 0)
    expect_length((subset(ndf.sim2, GENDER == 1))$GENDER, 0)
    expect_length((subset(ndf.sim2, GENDER == 10))$GENDER, 1585)
    expect_length((subset(ndf.sim2, GENDER == 20))$GENDER, 1503)
    
    expect_length((subset(ndf.sim3, GENDER == 0))$GENDER, 0)
    expect_length((subset(ndf.sim3, GENDER == 1))$GENDER, 0)
    expect_length((subset(ndf.sim3, GENDER == 10))$GENDER, 2091)
    expect_length((subset(ndf.sim3, GENDER == 20))$GENDER, 2037)
})

#
# Done
#
# context("ds.recodeValues::smk_dgr::factor::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "GENDER", "odf_1", "ndf_1", "odf_2", "ndf_2", "odf_3", "ndf_3", "odf_4", "ndf_4"))
})

disconnect.studies.dataset.cnsim()

# context("ds.recodeValues::smk_dgr::factor::done")
