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

context("ds.mediationTest::smk::setup")

connect.mediation.dataset.upb(list('att', 'attbin', 'negaff'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.mediationTest::smk")
test_that("simple mediationTest", {
    res <- ds.mediationTest("D$att", "D$attbin", "D$negaff")

    expect_length(res, 3)
    expect_equal(class(res), "list")
    expect_true(all(c('study1', 'study2', 'study3') %in% names(res)))

    expect_length(res$study1, 3)
    expect_equal(class(res$study1), "data.frame")
    expect_true(all(c('Sobel', 'Aroian', 'Goodman') %in% colnames(res$study1)))
    expect_true(all(c('z.value', 'p.value') %in% rownames(res$study1)))
    expect_length(res$study1$Sobel, 2)
    expect_equal(class(res$study1$Sobel[1]), "numeric")
    expect_equal(res$study1$Sobel[1], 4.321360678, tolerance = 1e-6)
    expect_equal(class(res$study1$Sobel[2]), "numeric")
    expect_equal(res$study1$Sobel[2], 0.000015507, tolerance = 1e-6)
    expect_length(res$study1$Aroian, 2)
    expect_equal(class(res$study1$Aroian[1]), "numeric")
    expect_equal(res$study1$Aroian[1], 4.319228e+00, tolerance = 1e-6)
    expect_equal(class(res$study1$Aroian[2]), "numeric")
    expect_equal(res$study1$Aroian[2], 1.565762e-05, tolerance = 1e-6)
    expect_length(res$study1$Goodman, 2)
    expect_equal(class(res$study1$Goodman[1]), "numeric")
    expect_equal(res$study1$Goodman[1], 4.323497e+00, tolerance = 1e-6)
    expect_equal(class(res$study1$Goodman[2]), "numeric")
    expect_equal(res$study1$Goodman[2], 1.535753e-05, tolerance = 1e-6)

    expect_length(res$study2, 3)
    expect_equal(class(res$study2), "data.frame")
    expect_true(all(c('Sobel', 'Aroian', 'Goodman') %in% colnames(res$study2)))
    expect_true(all(c('z.value', 'p.value') %in% rownames(res$study2)))
    expect_length(res$study2$Sobel, 2)
    expect_equal(class(res$study2$Sobel[1]), "numeric")
    expect_equal(res$study2$Sobel[1], -0.3498626, tolerance = 1e-6)
    expect_equal(class(res$study2$Sobel[2]), "numeric")
    expect_equal(res$study2$Sobel[2], 0.7264418, tolerance = 1e-6)
    expect_length(res$study2$Aroian, 2)
    expect_equal(class(res$study2$Aroian[1]), "numeric")
    expect_equal(res$study2$Aroian[1], -0.3061303, tolerance = 1e-6)
    expect_equal(class(res$study2$Aroian[2]), "numeric")
    expect_equal(res$study2$Aroian[2], 0.7595054, tolerance = 1e-6)
    expect_length(res$study2$Goodman, 2)
    expect_equal(class(res$study2$Goodman[1]), "numeric")
    expect_equal(res$study2$Goodman[1], -0.4200051, tolerance = 1e-6)
    expect_equal(class(res$study2$Goodman[2]), "numeric")
    expect_equal(res$study2$Goodman[2], 0.6744817, tolerance = 1e-6)

    expect_length(res$study3, 3)
    expect_equal(class(res$study3), "data.frame")
    expect_true(all(c('Sobel', 'Aroian', 'Goodman') %in% colnames(res$study3)))
    expect_true(all(c('z.value', 'p.value') %in% rownames(res$study3)))
    expect_length(res$study3$Sobel, 2)
    expect_equal(class(res$study3$Sobel[1]), "numeric")
    expect_equal(res$study3$Sobel[1], 0.2442042, tolerance = 1e-6)
    expect_equal(class(res$study3$Sobel[2]), "numeric")
    expect_equal(res$study3$Sobel[2], 0.8070727, tolerance = 1e-6)
    expect_length(res$study3$Aroian, 2)
    expect_equal(class(res$study3$Aroian[1]), "numeric")
    expect_equal(res$study3$Aroian[1], 0.1345419, tolerance = 1e-6)
    expect_equal(class(res$study3$Aroian[2]), "numeric")
    expect_equal(res$study3$Aroian[2], 0.8929741, tolerance = 1e-6)
    expect_length(res$study3$Goodman, 2)
    expect_equal(class(res$study3$Goodman[1]), "numeric")
    expect_true(is.na(res$study3$Goodman[1]))
    expect_equal(class(res$study3$Goodman[2]), "numeric")
    expect_true(is.na(res$study3$Goodman[2]))
})

#
# Done
#

context("ds.mediationTest::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.mediation.dataset.upb()

context("ds.mediationTest::smk::done")
