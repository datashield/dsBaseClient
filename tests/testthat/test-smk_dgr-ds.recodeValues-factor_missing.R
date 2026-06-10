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

# context("ds.recodeValues::smk_dgr::factor_missing::setup")

connect.studies.dataset.cnsim(list("GENDER", "PM_BMI_CATEGORICAL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.recodeValues::smk_dgr::factor_missing::simple missing factor 1")
test_that("simple missing factor 1", {
    res <- ds.recodeValues("D$PM_BMI_CATEGORICAL", values2replace.vector=c(1,2,3), new.values.vector=c(10,20,30), missing='999', newobj="PM_BMI_CATEGORICAL")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <PM_BMI_CATEGORICAL> has been created in all specified data sources")
    expect_equal(res$validity.check, "<PM_BMI_CATEGORICAL> appears valid in all sources")

    ds.dataFrame(c('D$PM_BMI_CATEGORICAL'), newobj='odf_1')
    odf <- ds.DANGERdfEXTRACT('odf_1')

    odf.sim1 <- odf$study.specific.df$sim1
    odf.sim2 <- odf$study.specific.df$sim2
    odf.sim3 <- odf$study.specific.df$sim3
    
    expect_length(odf.sim1$PM_BMI_CATEGORICAL, 2163)
    expect_length(odf.sim2$PM_BMI_CATEGORICAL, 3088)
    expect_length(odf.sim3$PM_BMI_CATEGORICAL, 4128)
    
    expect_length((subset(odf.sim1, is.na(PM_BMI_CATEGORICAL)))$PM_BMI_CATEGORICAL, 97)
    expect_length((subset(odf.sim1, PM_BMI_CATEGORICAL == 1))$PM_BMI_CATEGORICAL, 641)
    expect_length((subset(odf.sim1, PM_BMI_CATEGORICAL == 2))$PM_BMI_CATEGORICAL, 816)
    expect_length((subset(odf.sim1, PM_BMI_CATEGORICAL == 3))$PM_BMI_CATEGORICAL, 609)
    expect_length((subset(odf.sim1, PM_BMI_CATEGORICAL == 10))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(odf.sim1, PM_BMI_CATEGORICAL == 20))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(odf.sim1, PM_BMI_CATEGORICAL == 30))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(odf.sim1, PM_BMI_CATEGORICAL == 999))$PM_BMI_CATEGORICAL, 0)

    expect_length((subset(odf.sim2, is.na(PM_BMI_CATEGORICAL)))$PM_BMI_CATEGORICAL, 150)
    expect_length((subset(odf.sim2, PM_BMI_CATEGORICAL == 1))$PM_BMI_CATEGORICAL, 899)
    expect_length((subset(odf.sim2, PM_BMI_CATEGORICAL == 2))$PM_BMI_CATEGORICAL, 1173)
    expect_length((subset(odf.sim2, PM_BMI_CATEGORICAL == 3))$PM_BMI_CATEGORICAL, 866)
    expect_length((subset(odf.sim2, PM_BMI_CATEGORICAL == 10))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(odf.sim2, PM_BMI_CATEGORICAL == 20))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(odf.sim2, PM_BMI_CATEGORICAL == 30))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(odf.sim2, PM_BMI_CATEGORICAL == 999))$PM_BMI_CATEGORICAL, 0)

    expect_length((subset(odf.sim3, is.na(PM_BMI_CATEGORICAL)))$PM_BMI_CATEGORICAL, 205)
    expect_length((subset(odf.sim3, PM_BMI_CATEGORICAL == 1))$PM_BMI_CATEGORICAL, 1213)
    expect_length((subset(odf.sim3, PM_BMI_CATEGORICAL == 2))$PM_BMI_CATEGORICAL, 1556)
    expect_length((subset(odf.sim3, PM_BMI_CATEGORICAL == 3))$PM_BMI_CATEGORICAL, 1154)
    expect_length((subset(odf.sim3, PM_BMI_CATEGORICAL == 10))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(odf.sim3, PM_BMI_CATEGORICAL == 20))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(odf.sim3, PM_BMI_CATEGORICAL == 30))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(odf.sim3, PM_BMI_CATEGORICAL == 999))$PM_BMI_CATEGORICAL, 0)

    ds.dataFrame(c('PM_BMI_CATEGORICAL'), newobj='ndf_1')
    ndf <- ds.DANGERdfEXTRACT('ndf_1')
    
    ndf.sim1 <- ndf$study.specific.df$sim1
    ndf.sim2 <- ndf$study.specific.df$sim2
    ndf.sim3 <- ndf$study.specific.df$sim3

    expect_length(ndf.sim1$PM_BMI_CATEGORICAL, 2163)
    expect_length(ndf.sim2$PM_BMI_CATEGORICAL, 3088)
    expect_length(ndf.sim3$PM_BMI_CATEGORICAL, 4128)

    expect_length((subset(ndf.sim1, is.na(PM_BMI_CATEGORICAL)))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim1, PM_BMI_CATEGORICAL == 1))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim1, PM_BMI_CATEGORICAL == 2))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim1, PM_BMI_CATEGORICAL == 3))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim1, PM_BMI_CATEGORICAL == 10))$PM_BMI_CATEGORICAL, 641)
    expect_length((subset(ndf.sim1, PM_BMI_CATEGORICAL == 20))$PM_BMI_CATEGORICAL, 816)
    expect_length((subset(ndf.sim1, PM_BMI_CATEGORICAL == 30))$PM_BMI_CATEGORICAL, 609)
    expect_length((subset(ndf.sim1, PM_BMI_CATEGORICAL == 999))$PM_BMI_CATEGORICAL, 97)

    expect_length((subset(ndf.sim2, is.na(PM_BMI_CATEGORICAL)))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim2, PM_BMI_CATEGORICAL == 1))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim2, PM_BMI_CATEGORICAL == 2))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim2, PM_BMI_CATEGORICAL == 3))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim2, PM_BMI_CATEGORICAL == 10))$PM_BMI_CATEGORICAL, 899)
    expect_length((subset(ndf.sim2, PM_BMI_CATEGORICAL == 20))$PM_BMI_CATEGORICAL, 1173)
    expect_length((subset(ndf.sim2, PM_BMI_CATEGORICAL == 30))$PM_BMI_CATEGORICAL, 866)
    expect_length((subset(ndf.sim2, PM_BMI_CATEGORICAL == 999))$PM_BMI_CATEGORICAL, 150)

    expect_length((subset(ndf.sim3, is.na(PM_BMI_CATEGORICAL)))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim3, PM_BMI_CATEGORICAL == 1))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim3, PM_BMI_CATEGORICAL == 2))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim3, PM_BMI_CATEGORICAL == 3))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim3, PM_BMI_CATEGORICAL == 10))$PM_BMI_CATEGORICAL, 1213)
    expect_length((subset(ndf.sim3, PM_BMI_CATEGORICAL == 20))$PM_BMI_CATEGORICAL, 1556)
    expect_length((subset(ndf.sim3, PM_BMI_CATEGORICAL == 30))$PM_BMI_CATEGORICAL, 1154)
    expect_length((subset(ndf.sim3, PM_BMI_CATEGORICAL == 999))$PM_BMI_CATEGORICAL, 205)
})

# context("ds.recodeValues::smk_dgr::factor_missing::simple missing factor 2")
test_that("simple missing factor 2", {
    res <- ds.recodeValues("D$PM_BMI_CATEGORICAL", values2replace.vector=c(1,2), new.values.vector=c(1,2), missing=99, newobj="PM_BMI_CATEGORICAL")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <PM_BMI_CATEGORICAL> has been created in all specified data sources")
    expect_equal(res$validity.check, "<PM_BMI_CATEGORICAL> appears valid in all sources")

    ds.dataFrame(c('D$PM_BMI_CATEGORICAL'), newobj='odf_2')
    odf <- ds.DANGERdfEXTRACT('odf_2')

    odf.sim1 <- odf$study.specific.df$sim1
    odf.sim2 <- odf$study.specific.df$sim2
    odf.sim3 <- odf$study.specific.df$sim3
    
    expect_length(odf.sim1$PM_BMI_CATEGORICAL, 2163)
    expect_length(odf.sim2$PM_BMI_CATEGORICAL, 3088)
    expect_length(odf.sim3$PM_BMI_CATEGORICAL, 4128)
    
    expect_length((subset(odf.sim1, is.na(PM_BMI_CATEGORICAL)))$PM_BMI_CATEGORICAL, 97)
    expect_length((subset(odf.sim1, PM_BMI_CATEGORICAL == 1))$PM_BMI_CATEGORICAL, 641)
    expect_length((subset(odf.sim1, PM_BMI_CATEGORICAL == 2))$PM_BMI_CATEGORICAL, 816)
    expect_length((subset(odf.sim1, PM_BMI_CATEGORICAL == 3))$PM_BMI_CATEGORICAL, 609)
    expect_length((subset(odf.sim1, PM_BMI_CATEGORICAL == 99))$PM_BMI_CATEGORICAL, 0)

    expect_length((subset(odf.sim2, is.na(PM_BMI_CATEGORICAL)))$PM_BMI_CATEGORICAL, 150)
    expect_length((subset(odf.sim2, PM_BMI_CATEGORICAL == 1))$PM_BMI_CATEGORICAL, 899)
    expect_length((subset(odf.sim2, PM_BMI_CATEGORICAL == 2))$PM_BMI_CATEGORICAL, 1173)
    expect_length((subset(odf.sim2, PM_BMI_CATEGORICAL == 3))$PM_BMI_CATEGORICAL, 866)
    expect_length((subset(odf.sim2, PM_BMI_CATEGORICAL == 99))$PM_BMI_CATEGORICAL, 0)

    expect_length((subset(odf.sim3, is.na(PM_BMI_CATEGORICAL)))$PM_BMI_CATEGORICAL, 205)
    expect_length((subset(odf.sim3, PM_BMI_CATEGORICAL == 1))$PM_BMI_CATEGORICAL, 1213)
    expect_length((subset(odf.sim3, PM_BMI_CATEGORICAL == 2))$PM_BMI_CATEGORICAL, 1556)
    expect_length((subset(odf.sim3, PM_BMI_CATEGORICAL == 3))$PM_BMI_CATEGORICAL, 1154)
    expect_length((subset(odf.sim3, PM_BMI_CATEGORICAL == 99))$PM_BMI_CATEGORICAL, 0)

    ds.dataFrame(c('PM_BMI_CATEGORICAL'), newobj='ndf_2')
    ndf <- ds.DANGERdfEXTRACT('ndf_2')
    
    ndf.sim1 <- ndf$study.specific.df$sim1
    ndf.sim2 <- ndf$study.specific.df$sim2
    ndf.sim3 <- ndf$study.specific.df$sim3

    expect_length(ndf.sim1$PM_BMI_CATEGORICAL, 2163)
    expect_length(ndf.sim2$PM_BMI_CATEGORICAL, 3088)
    expect_length(ndf.sim3$PM_BMI_CATEGORICAL, 4128)

    expect_length((subset(ndf.sim1, is.na(PM_BMI_CATEGORICAL)))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim1, PM_BMI_CATEGORICAL == 1))$PM_BMI_CATEGORICAL, 641)
    expect_length((subset(ndf.sim1, PM_BMI_CATEGORICAL == 2))$PM_BMI_CATEGORICAL, 816)
    expect_length((subset(ndf.sim1, PM_BMI_CATEGORICAL == 3))$PM_BMI_CATEGORICAL, 609)
    expect_length((subset(ndf.sim1, PM_BMI_CATEGORICAL == 99))$PM_BMI_CATEGORICAL, 97)

    expect_length((subset(ndf.sim2, is.na(PM_BMI_CATEGORICAL)))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim2, PM_BMI_CATEGORICAL == 1))$PM_BMI_CATEGORICAL, 899)
    expect_length((subset(ndf.sim2, PM_BMI_CATEGORICAL == 2))$PM_BMI_CATEGORICAL, 1173)
    expect_length((subset(ndf.sim2, PM_BMI_CATEGORICAL == 3))$PM_BMI_CATEGORICAL, 866)
    expect_length((subset(ndf.sim2, PM_BMI_CATEGORICAL == 99))$PM_BMI_CATEGORICAL, 150)

    expect_length((subset(ndf.sim3, is.na(PM_BMI_CATEGORICAL)))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim3, PM_BMI_CATEGORICAL == 1))$PM_BMI_CATEGORICAL, 1213)
    expect_length((subset(ndf.sim3, PM_BMI_CATEGORICAL == 2))$PM_BMI_CATEGORICAL, 1556)
    expect_length((subset(ndf.sim3, PM_BMI_CATEGORICAL == 3))$PM_BMI_CATEGORICAL, 1154)
    expect_length((subset(ndf.sim3, PM_BMI_CATEGORICAL == 99))$PM_BMI_CATEGORICAL, 205)
})

# context("ds.recodeValues::smk_dgr::factor_missing::simple missing factor 3")
test_that("simple missing factor 3", {
    res <- ds.recodeValues("D$PM_BMI_CATEGORICAL", values2replace.vector=c(1,2,3), new.values.vector=c('low','medium','high'), newobj="PM_BMI_CATEGORICAL")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <PM_BMI_CATEGORICAL> has been created in all specified data sources")
    expect_equal(res$validity.check, "<PM_BMI_CATEGORICAL> appears valid in all sources")

    ds.dataFrame(c('D$PM_BMI_CATEGORICAL'), newobj='odf_3')
    odf <- ds.DANGERdfEXTRACT('odf_3')

    odf.sim1 <- odf$study.specific.df$sim1
    odf.sim2 <- odf$study.specific.df$sim2
    odf.sim3 <- odf$study.specific.df$sim3
    
    expect_length(odf.sim1$PM_BMI_CATEGORICAL, 2163)
    expect_length(odf.sim2$PM_BMI_CATEGORICAL, 3088)
    expect_length(odf.sim3$PM_BMI_CATEGORICAL, 4128)
    
    expect_length((subset(odf.sim1, PM_BMI_CATEGORICAL == 1))$PM_BMI_CATEGORICAL, 641)
    expect_length((subset(odf.sim1, PM_BMI_CATEGORICAL == 2))$PM_BMI_CATEGORICAL, 816)
    expect_length((subset(odf.sim1, PM_BMI_CATEGORICAL == 3))$PM_BMI_CATEGORICAL, 609)
    expect_length((subset(odf.sim1, PM_BMI_CATEGORICAL == 'low'))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(odf.sim1, PM_BMI_CATEGORICAL == 'medium'))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(odf.sim1, PM_BMI_CATEGORICAL == 'high'))$PM_BMI_CATEGORICAL, 0)

    expect_length((subset(odf.sim2, PM_BMI_CATEGORICAL == 1))$PM_BMI_CATEGORICAL, 899)
    expect_length((subset(odf.sim2, PM_BMI_CATEGORICAL == 2))$PM_BMI_CATEGORICAL, 1173)
    expect_length((subset(odf.sim2, PM_BMI_CATEGORICAL == 3))$PM_BMI_CATEGORICAL, 866)
    expect_length((subset(odf.sim2, PM_BMI_CATEGORICAL == 'low'))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(odf.sim2, PM_BMI_CATEGORICAL == 'medium'))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(odf.sim2, PM_BMI_CATEGORICAL == 'high'))$PM_BMI_CATEGORICAL, 0)

    expect_length((subset(odf.sim3, PM_BMI_CATEGORICAL == 1))$PM_BMI_CATEGORICAL, 1213)
    expect_length((subset(odf.sim3, PM_BMI_CATEGORICAL == 2))$PM_BMI_CATEGORICAL, 1556)
    expect_length((subset(odf.sim3, PM_BMI_CATEGORICAL == 3))$PM_BMI_CATEGORICAL, 1154)
    expect_length((subset(odf.sim3, PM_BMI_CATEGORICAL == 'low'))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(odf.sim3, PM_BMI_CATEGORICAL == 'medium'))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(odf.sim3, PM_BMI_CATEGORICAL == 'high'))$PM_BMI_CATEGORICAL, 0)

    ds.dataFrame(c('PM_BMI_CATEGORICAL'), newobj='ndf_3')
    ndf <- ds.DANGERdfEXTRACT('ndf_3')
    
    ndf.sim1 <- ndf$study.specific.df$sim1
    ndf.sim2 <- ndf$study.specific.df$sim2
    ndf.sim3 <- ndf$study.specific.df$sim3

    expect_length(ndf.sim1$PM_BMI_CATEGORICAL, 2163)
    expect_length(ndf.sim2$PM_BMI_CATEGORICAL, 3088)
    expect_length(ndf.sim3$PM_BMI_CATEGORICAL, 4128)

    expect_length((subset(ndf.sim1, PM_BMI_CATEGORICAL == 1))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim1, PM_BMI_CATEGORICAL == 2))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim1, PM_BMI_CATEGORICAL == 3))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim1, PM_BMI_CATEGORICAL == 'low'))$PM_BMI_CATEGORICAL, 641)
    expect_length((subset(ndf.sim1, PM_BMI_CATEGORICAL == 'medium'))$PM_BMI_CATEGORICAL, 816)
    expect_length((subset(ndf.sim1, PM_BMI_CATEGORICAL == 'high'))$PM_BMI_CATEGORICAL, 609)

    expect_length((subset(ndf.sim2, PM_BMI_CATEGORICAL == 1))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim2, PM_BMI_CATEGORICAL == 2))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim2, PM_BMI_CATEGORICAL == 3))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim2, PM_BMI_CATEGORICAL == 'low'))$PM_BMI_CATEGORICAL, 899)
    expect_length((subset(ndf.sim2, PM_BMI_CATEGORICAL == 'medium'))$PM_BMI_CATEGORICAL, 1173)
    expect_length((subset(ndf.sim2, PM_BMI_CATEGORICAL == 'high'))$PM_BMI_CATEGORICAL, 866)

    expect_length((subset(ndf.sim3, PM_BMI_CATEGORICAL == 1))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim3, PM_BMI_CATEGORICAL == 2))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim3, PM_BMI_CATEGORICAL == 3))$PM_BMI_CATEGORICAL, 0)
    expect_length((subset(ndf.sim3, PM_BMI_CATEGORICAL == 'low'))$PM_BMI_CATEGORICAL, 1213)
    expect_length((subset(ndf.sim3, PM_BMI_CATEGORICAL == 'medium'))$PM_BMI_CATEGORICAL, 1556)
    expect_length((subset(ndf.sim3, PM_BMI_CATEGORICAL == 'high'))$PM_BMI_CATEGORICAL, 1154)
})

#
# Done
#
# context("ds.recodeValues::smk_dgr::factor_missing::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "PM_BMI_CATEGORICAL", "odf_1", "ndf_1", "odf_2", "ndf_2", "odf_3", "ndf_3"))
})

disconnect.studies.dataset.cnsim()

# context("ds.recodeValues::smk_dgr::factor_missing::done")
