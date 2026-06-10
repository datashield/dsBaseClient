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

# context("ds.dmtC2S::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.dmtC2S::smk::data.frame")
test_that("simple dmtC2S - data.frame", {
    a <- data.frame(c(1, 2, 3, 4))
    b <- data.frame(c(2, 3, 4, 1))
    c <- data.frame(c(3, 4, 1, 2))
    cdf <- data.frame(a, b, c)

    res <- ds.dmtC2S(dfdata = cdf, newobj = "dataframe.newobj")

    expect_length(res, 0)

    res.class <- ds.class("dataframe.newobj")

    expect_length(res.class, 3)
    expect_true(all(c("data.frame") %in% res.class$sim1))
    expect_true(all(c("data.frame") %in% res.class$sim2))
    expect_true(all(c("data.frame") %in% res.class$sim3))
})

# context("ds.dmtC2S::smk::matrix")
test_that("simple dmtC2S - matrix", {
    a <- c(1, 2, 3, 4, 2, 3, 4, 1, 3)
    cm <- matrix(a, nrow = 3, ncol = 3, byrow = TRUE)

    res <- ds.dmtC2S(dfdata = cm, newobj = "matrix.newobj")

    expect_length(res, 0)

    res.class <- ds.class("matrix.newobj")

    expect_length(res.class, 3)
    expect_true(all(c("matrix", "array") %in% res.class$sim1))
    expect_true(all(c("matrix", "array") %in% res.class$sim2))
    expect_true(all(c("matrix", "array") %in% res.class$sim3))
})

# context("ds.dmtC2S::smk::tibble")
test_that("simple dmtC2S - tibble", {
    a <- data.frame(c(1, 2, 3, 4))
    b <- data.frame(c(2, 3, 4, 1))
    c <- data.frame(c(3, 4, 1, 2))
    ct <- tibble::tibble(a, b, c)

    res <- ds.dmtC2S(dfdata = ct, newobj = "tibble.newobj")

    expect_length(res, 0)

    res.class <- ds.class("tibble.newobj")

    expect_length(res.class, 3)
    expect_true(all(c("tbl_df", "tbl", "data.frame") %in% res.class$sim1))
    expect_true(all(c("tbl_df", "tbl", "data.frame") %in% res.class$sim2))
    expect_true(all(c("tbl_df", "tbl", "data.frame") %in% res.class$sim3))
})

#
# Done
#

# context("ds.dmtC2S::smk::shutdown")

test_that("setup", {
    ds_expect_variables(c("D", "dataframe.newobj", "matrix.newobj", "tibble.newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.dmtC2S::smk::done")
