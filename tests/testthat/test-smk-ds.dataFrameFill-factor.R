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

# context("ds.dataFrameFill::smk::factor setup")

connect.studies.dataset.cnsim(list('LAB_TSC', 'DIS_CVA', 'LAB_HDL', 'DIS_DIAB'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.dataFrameFill::smk::factor setup dataframes")
test_that("setup", {
    ds_expect_variables(c("D"))
  
    vectors1 <- c('D$LAB_TSC', 'D$DIS_CVA', 'D$LAB_HDL')
    ds.dataFrame(x=vectors1, newobj = "DD", datasources=ds.test_env$connections[1])

    vectors2 <- c('D$LAB_TSC', 'D$DIS_CVA', 'D$DIS_DIAB')
    ds.dataFrame(x=vectors2, newobj = "DD", datasources=ds.test_env$connections[2])

    vectors3 <- c('D$LAB_TSC', 'D$LAB_HDL', 'D$DIS_DIAB')
    ds.dataFrame(x=vectors3, newobj = "DD", datasources=ds.test_env$connections[3])

    colnamesDD <- ds.colnames('DD')
    
    expect_length(colnamesDD, 3)
    expect_length(colnamesDD$sim1, 3)
    expect_equal(colnamesDD$sim1[1], "LAB_TSC")
    expect_equal(colnamesDD$sim1[2], "DIS_CVA")
    expect_equal(colnamesDD$sim1[3], "LAB_HDL")
    expect_length(colnamesDD$sim2, 3)
    expect_equal(colnamesDD$sim2[1], "LAB_TSC")
    expect_equal(colnamesDD$sim2[2], "DIS_CVA")
    expect_equal(colnamesDD$sim2[3], "DIS_DIAB")
    expect_length(colnamesDD$sim3, 3)
    expect_equal(colnamesDD$sim3[1], "LAB_TSC")
    expect_equal(colnamesDD$sim3[2], "LAB_HDL")
    expect_equal(colnamesDD$sim3[3], "DIS_DIAB")
    
    classesDD <- ds.class('DD')
    
    expect_length(classesDD, 3)
    expect_length(classesDD$sim1, 1)
    expect_equal(classesDD$sim1, "data.frame")
    expect_length(classesDD$sim2, 1)
    expect_equal(classesDD$sim2, "data.frame")
    expect_length(classesDD$sim3, 1)
    expect_equal(classesDD$sim3, "data.frame")
})


# context("ds.dataFrameFill::smk::factor extend unfilled dataframes")
test_that("dataFrameFill_exists", {
    res <- ds.dataFrameFill(df.name="DD", newobj="filled_df")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <filled_df> has been created in all specified data sources")
    expect_equal(res$validity.check, "<filled_df> appears valid in all sources")

    colnamesFilled <- ds.colnames('filled_df')

    expect_length(colnamesFilled, 3)
    expect_length(colnamesFilled$sim1, 4)
    expect_equal(colnamesFilled$sim1[1], "LAB_TSC")
    expect_equal(colnamesFilled$sim1[2], "DIS_CVA")
    expect_equal(colnamesFilled$sim1[3], "LAB_HDL")
    expect_equal(colnamesFilled$sim1[4], "DIS_DIAB")
    expect_length(colnamesFilled$sim2, 4)
    expect_equal(colnamesFilled$sim2[1], "LAB_TSC")
    expect_equal(colnamesFilled$sim2[2], "DIS_CVA")
    expect_equal(colnamesFilled$sim2[3], "DIS_DIAB")
    expect_equal(colnamesFilled$sim2[4], "LAB_HDL")
    expect_length(colnamesFilled$sim3, 4)
    expect_equal(colnamesFilled$sim3[1], "LAB_TSC")
    expect_equal(colnamesFilled$sim3[2], "LAB_HDL")
    expect_equal(colnamesFilled$sim3[3], "DIS_DIAB")
    expect_equal(colnamesFilled$sim3[4], "DIS_CVA")
    
    classFilled <- ds.class('filled_df')

    expect_length(classFilled, 3)
    expect_length(classFilled$sim1, 1)
    expect_equal(classFilled$sim1, "data.frame")
    expect_length(classFilled$sim2, 1)
    expect_equal(classFilled$sim2, "data.frame")
    expect_length(classFilled$sim3, 1)
    expect_equal(classFilled$sim3, "data.frame")

    lab_tsc_classFilled <- ds.class('filled_df$LAB_TSC')
    
    expect_length(lab_tsc_classFilled, 3)
    expect_length(lab_tsc_classFilled$sim1, 1)
    expect_equal(lab_tsc_classFilled$sim1, "numeric")
    expect_length(lab_tsc_classFilled$sim2, 1)
    expect_equal(lab_tsc_classFilled$sim2, "numeric")
    expect_length(lab_tsc_classFilled$sim3, 1)
    expect_equal(lab_tsc_classFilled$sim3, "numeric")
    
    dis_cva_classFilled <- ds.class('filled_df$DIS_CVA')

    expect_length(dis_cva_classFilled, 3)
    expect_length(dis_cva_classFilled$sim1, 1)
    expect_equal(dis_cva_classFilled$sim1, "factor")
    expect_length(dis_cva_classFilled$sim2, 1)
    expect_equal(dis_cva_classFilled$sim2, "factor")
    expect_length(dis_cva_classFilled$sim3, 1)
    expect_equal(dis_cva_classFilled$sim3, "factor")

    dis_cva_levelsFilled <- ds.levels('filled_df$DIS_CVA')

    expect_length(dis_cva_levelsFilled, 3)
    expect_length(dis_cva_levelsFilled$sim1, 2)
    expect_length(dis_cva_levelsFilled$sim1$Levels, 2)
    expect_true(all(dis_cva_levelsFilled$sim1$Levels %in% c("0", "1")))
    expect_length(dis_cva_levelsFilled$sim2, 2)
    expect_length(dis_cva_levelsFilled$sim2$Levels, 2)
    expect_true(all(dis_cva_levelsFilled$sim2$Levels %in% c("0", "1")))
    expect_length(dis_cva_levelsFilled$sim3, 2)
    expect_length(dis_cva_levelsFilled$sim3$Levels, 2)
    expect_true(all(dis_cva_levelsFilled$sim3$Levels %in% c("0", "1")))

    lab_hdl_classFilled <- ds.class('filled_df$LAB_HDL')
    
    expect_length(lab_hdl_classFilled, 3)
    expect_length(lab_hdl_classFilled$sim1, 1)
    expect_equal(lab_hdl_classFilled$sim1, "numeric")
    expect_length(lab_hdl_classFilled$sim2, 1)
    expect_equal(lab_hdl_classFilled$sim2, "numeric")
    expect_length(lab_hdl_classFilled$sim3, 1)
    expect_equal(lab_hdl_classFilled$sim3, "numeric")
    
    dis_diab_classFilled <- ds.class('filled_df$DIS_DIAB')
    
    expect_length(dis_diab_classFilled, 3)
    expect_length(dis_diab_classFilled$sim1, 1)
    expect_equal(dis_diab_classFilled$sim1, "factor")
    expect_length(dis_diab_classFilled$sim2, 1)
    expect_equal(dis_diab_classFilled$sim2, "factor")
    expect_length(dis_diab_classFilled$sim3, 1)
    expect_equal(dis_diab_classFilled$sim3, "factor")

    dis_diab_levelsFilled <- ds.levels('filled_df$DIS_DIAB')

    expect_length(dis_diab_levelsFilled, 3)
    expect_length(dis_diab_levelsFilled$sim1, 2)
    expect_length(dis_diab_levelsFilled$sim1$Levels, 2)
    expect_true(all(dis_diab_levelsFilled$sim1$Levels %in% c("0", "1")))
    expect_length(dis_diab_levelsFilled$sim2, 2)
    expect_length(dis_diab_levelsFilled$sim2$Levels, 2)
    expect_true(all(dis_diab_levelsFilled$sim2$Levels %in% c("0", "1")))
    expect_length(dis_diab_levelsFilled$sim3, 2)
    expect_length(dis_diab_levelsFilled$sim3$Levels, 2)
    expect_true(all(dis_diab_levelsFilled$sim3$Levels %in% c("0", "1")))
})

#
# Done
#

# context("ds.dataFrameFill::smk::factor shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "DD", "filled_df"))
})

disconnect.studies.dataset.cnsim()

# context("ds.dataFrameFill::smk::factor done")
