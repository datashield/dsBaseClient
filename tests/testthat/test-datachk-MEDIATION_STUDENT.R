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

context("MEDIATION_STUDENT::datachk::setup")

connect.mediation.dataset.student(list('fight', 'attachment', 'work', 'score', 'late', 'coed', 'smorale', 'gender', 'income', 'free', 'pared', 'catholic', 'SCH_ID'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("MEDIATION_STUDENT::datachk")
test_that("Check MEDIATION_STUDENT dataset", {
    res.class <- ds.class(x='D')
    expect_length(res.class, 1)
    expect_length(res.class$study1, 1)
    expect_equal(res.class$study1, "data.frame")

    res.length <- ds.length(x='D')
    expect_length(res.length, 2)
    expect_length(res.length$`length of D in study1`, 1)
    expect_equal(res.length$`length of D in study1`, 13)
    expect_equal(res.length$`total length of D in all studies combined`, 13)

    res.colnames <- ds.colnames(x='D')
    expect_length(res.colnames, 1)
    expect_length(res.colnames$study1, 13)
    expect_equal(res.colnames$study1, c('fight', 'attachment', 'work', 'score', 'late', 'coed', 'smorale', 'gender', 'income', 'free', 'pared', 'catholic', 'SCH_ID'))

    res.class.a <- ds.class(x='D$fight')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")

    res.length.a <- ds.length(x='D$fight')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$fight in study1`, 1)
    expect_equal(res.length.a$`length of D$fight in study1`, 9679)
    expect_length(res.length.a$`total length of D$fight in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$fight in all studies combined`, 9679)

    res.class.a <- ds.class(x='D$attachment')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "integer")

    res.length.a <- ds.length(x='D$attachment')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$attachment in study1`, 1)
    expect_equal(res.length.a$`length of D$attachment in study1`, 9679)
    expect_length(res.length.a$`total length of D$attachment in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$attachment in all studies combined`, 9679)

    res.class.a <- ds.class(x='D$work')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")

    res.length.a <- ds.length(x='D$work')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$work in study1`, 1)
    expect_equal(res.length.a$`length of D$work in study1`, 9679)
    expect_length(res.length.a$`total length of D$work in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$work in all studies combined`, 9679)

    res.class.a <- ds.class(x='D$score')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "integer")

    res.length.a <- ds.length(x='D$score')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$score in study1`, 1)
    expect_equal(res.length.a$`length of D$score in study1`, 9679)
    expect_length(res.length.a$`total length of D$score in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$score in all studies combined`, 9679)

    res.class.a <- ds.class(x='D$late')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")

    res.length.a <- ds.length(x='D$late')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$late in study1`, 1)
    expect_equal(res.length.a$`length of D$late in study1`, 9679)
    expect_length(res.length.a$`total length of D$late in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$late in all studies combined`, 9679)

    res.class.a <- ds.class(x='D$coed')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")

    res.length.a <- ds.length(x='D$coed')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$coed in study1`, 1)
    expect_equal(res.length.a$`length of D$coed in study1`, 9679)
    expect_length(res.length.a$`total length of D$coed in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$coed in all studies combined`, 9679)

    res.class.a <- ds.class(x='D$smorale')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")

    res.length.a <- ds.length(x='D$smorale')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$smorale in study1`, 1)
    expect_equal(res.length.a$`length of D$smorale in study1`, 9679)
    expect_length(res.length.a$`total length of D$smorale in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$smorale in all studies combined`, 9679)

    res.class.a <- ds.class(x='D$gender')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")

    res.length.a <- ds.length(x='D$gender')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$gender in study1`, 1)
    expect_equal(res.length.a$`length of D$gender in study1`, 9679)
    expect_length(res.length.a$`total length of D$gender in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$gender in all studies combined`, 9679)

    res.class.a <- ds.class(x='D$income')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")

    res.length.a <- ds.length(x='D$income')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$income in study1`, 1)
    expect_equal(res.length.a$`length of D$income in study1`, 9679)
    expect_length(res.length.a$`total length of D$income in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$income in all studies combined`, 9679)
    
    res.class.a <- ds.class(x='D$free')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")
    
    res.length.a <- ds.length(x='D$free')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$free in study1`, 1)
    expect_equal(res.length.a$`length of D$free in study1`, 9679)
    expect_length(res.length.a$`total length of D$free in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$free in all studies combined`, 9679)
    
    res.class.a <- ds.class(x='D$pared')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")
    
    res.length.a <- ds.length(x='D$pared')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$pared in study1`, 1)
    expect_equal(res.length.a$`length of D$pared in study1`, 9679)
    expect_length(res.length.a$`total length of D$pared in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$pared in all studies combined`, 9679)

    res.class.a <- ds.class(x='D$catholic')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "factor")
    
    res.length.a <- ds.length(x='D$catholic')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$catholic in study1`, 1)
    expect_equal(res.length.a$`length of D$catholic in study1`, 9679)
    expect_length(res.length.a$`total length of D$catholic in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$catholic in all studies combined`, 9679)
    
    res.class.a <- ds.class(x='D$SCH_ID')
    expect_length(res.class.a, 1)
    expect_length(res.class.a$study1, 1)
    expect_equal(res.class.a$study1, "integer")
    
    res.length.a <- ds.length(x='D$SCH_ID')
    expect_length(res.length.a, 2)
    expect_length(res.length.a$`length of D$SCH_ID in study1`, 1)
    expect_equal(res.length.a$`length of D$SCH_ID in study1`, 9679)
    expect_length(res.length.a$`total length of D$SCH_ID in all studies combined`, 1)
    expect_equal(res.length.a$`total length of D$SCH_ID in all studies combined`, 9679)
})

#
# Tear down
#

context("MEDIATION_STUDENT::datachk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.mediation.dataset.student()

#
# Done
#

context("MEDIATION_STUDENT::datachk::done")
