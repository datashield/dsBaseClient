#-------------------------------------------------------------------------------
# Copyright (c) 2019-2021 University of Newcastle upon Tyne. All rights reserved.
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

context("MEDIATION_FRAMING::datachk::setup")

connect.mediation.dataset.framing(list('cond', 'anx', 'age', 'educ', 'gender', 'income', 'emo', 'p_harm', 'tone', 'eth', 'treat', 'english', 'immigr', 'anti_info', 'cong_mesg'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("MEDIATION_FRAMING::datachk")
test_that("Check MEDIATION_FRAMING dataset", {
    res.class <- ds.class(x='D')
    expect_length(res.class, 1)
    expect_length(res.class$study1, 1)
    expect_equal(res.class$study1, "data.frame")

    res.length <- ds.length(x='D')
    expect_length(res.length, 2)
    expect_length(res.length$`length of D in study1`, 1)
    expect_equal(res.length$`length of D in study1`, 15)
    expect_equal(res.length$`total length of D in all studies combined`, 15)

    res.colnames <- ds.colnames(x='D')
    expect_length(res.colnames, 1)
    expect_length(res.colnames$study1, 15)
    expect_equal(res.colnames$study1, c('cond', 'anx', 'age', 'educ', 'gender', 'income', 'emo', 'p_harm', 'tone', 'eth', 'treat', 'english', 'immigr', 'anti_info', 'cong_mesg'))

    res.class.a <- ds.class(x='D$cond')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")

    res.length.a <- ds.length(x='D$cond')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$cond in study1`, 1)
    expect_equal(res.length.a$`length of D$cond in study1`, 265)
    expect_length(res.length.a$`total length of D$cond in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$cond in all studies combined`, 265)

    res.class.a <- ds.class(x='D$anx')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")

    res.length.a <- ds.length(x='D$anx')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$anx in study1`, 1)
    expect_equal(res.length.a$`length of D$anx in study1`, 265)
    expect_length(res.length.a$`total length of D$anx in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$anx in all studies combined`, 265)

    res.class.a <- ds.class(x='D$age')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "integer")

    res.length.a <- ds.length(x='D$age')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$age in study1`, 1)
    expect_equal(res.length.a$`length of D$age in study1`, 265)
    expect_length(res.length.a$`total length of D$age in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$age in all studies combined`, 265)

    res.class.a <- ds.class(x='D$educ')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")

    res.length.a <- ds.length(x='D$educ')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$educ in study1`, 1)
    expect_equal(res.length.a$`length of D$educ in study1`, 265)
    expect_length(res.length.a$`total length of D$educ in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$educ in all studies combined`, 265)

    res.class.a <- ds.class(x='D$gender')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")

    res.length.a <- ds.length(x='D$gender')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$gender in study1`, 1)
    expect_equal(res.length.a$`length of D$gender in study1`, 265)
    expect_length(res.length.a$`total length of D$gender in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$gender in all studies combined`, 265)

    res.class.a <- ds.class(x='D$income')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "integer")

    res.length.a <- ds.length(x='D$income')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$income in study1`, 1)
    expect_equal(res.length.a$`length of D$income in study1`, 265)
    expect_length(res.length.a$`total length of D$income in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$income in all studies combined`, 265)

    res.class.a <- ds.class(x='D$emo')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "numeric")

    res.length.a <- ds.length(x='D$emo')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$emo in study1`, 1)
    expect_equal(res.length.a$`length of D$emo in study1`, 265)
    expect_length(res.length.a$`total length of D$emo in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$emo in all studies combined`, 265)

    res.class.a <- ds.class(x='D$p_harm')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "numeric")

    res.length.a <- ds.length(x='D$p_harm')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$p_harm in study1`, 1)
    expect_equal(res.length.a$`length of D$p_harm in study1`, 265)
    expect_length(res.length.a$`total length of D$p_harm in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$p_harm in all studies combined`, 265)
    
    res.class.a <- ds.class(x='D$tone')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "integer")
    
    res.length.a <- ds.length(x='D$tone')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$tone in study1`, 1)
    expect_equal(res.length.a$`length of D$tone in study1`, 265)
    expect_length(res.length.a$`total length of D$tone in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$tone in all studies combined`, 265)
    
    res.class.a <- ds.class(x='D$eth')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "integer")
    
    res.length.a <- ds.length(x='D$eth')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$eth in study1`, 1)
    expect_equal(res.length.a$`length of D$eth in study1`, 265)
    expect_length(res.length.a$`total length of D$eth in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$eth in all studies combined`, 265)

    res.class.a <- ds.class(x='D$treat')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "numeric")
    
    res.length.a <- ds.length(x='D$treat')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$treat in study1`, 1)
    expect_equal(res.length.a$`length of D$treat in study1`, 265)
    expect_length(res.length.a$`total length of D$treat in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$treat in all studies combined`, 265)
    
    res.class.a <- ds.class(x='D$english')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")
    
    res.length.a <- ds.length(x='D$english')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$english in study1`, 1)
    expect_equal(res.length.a$`length of D$english in study1`, 265)
    expect_length(res.length.a$`total length of D$english in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$english in all studies combined`, 265)
    
    res.class.a <- ds.class(x='D$immigr')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "integer")
    
    res.length.a <- ds.length(x='D$immigr')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$immigr in study1`, 1)
    expect_equal(res.length.a$`length of D$immigr in study1`, 265)
    expect_length(res.length.a$`total length of D$immigr in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$immigr in all studies combined`, 265)
    
    res.class.a <- ds.class(x='D$anti_info')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "integer")
    
    res.length.a <- ds.length(x='D$anti_info')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$anti_info in study1`, 1)
    expect_equal(res.length.a$`length of D$anti_info in study1`, 265)
    expect_length(res.length.a$`total length of D$anti_info in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$anti_info in all studies combined`, 265)
    
    res.class.a <- ds.class(x='D$cong_mesg')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "integer")
    
    res.length.a <- ds.length(x='D$cong_mesg')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$cong_mesg in study1`, 1)
    expect_equal(res.length.a$`length of D$cong_mesg in study1`, 265)
    expect_length(res.length.a$`total length of D$cong_mesg in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$cong_mesg in all studies combined`, 265)
})

#
# Tear down
#

context("MEDIATION_FRAMING::datachk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.mediation.dataset.framing()

#
# Done
#

context("MEDIATION_FRAMING::datachk::done")
