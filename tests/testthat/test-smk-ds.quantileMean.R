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

# context("ds.quantileMean::smk::setup")

connect.studies.dataset.cnsim(list('LAB_HDL'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.quantileMean::smk::standard")
test_that("quantileMean", {
    res <- ds.quantileMean(x='D$LAB_HDL')

    expect_length(res, 8)
    expect_equal(res[[1]], 0.8678198, tolerance = .000001)
    expect_equal(res[[2]], 1.0388227, tolerance = .000001)
    expect_equal(res[[3]], 1.2998328, tolerance = .000001)
    expect_equal(res[[4]], 1.5787193, tolerance = .000001)
    expect_equal(res[[5]], 1.8481549, tolerance = .000001)
    expect_equal(res[[6]], 2.0896969, tolerance = .000001)
    expect_equal(res[[7]], 2.2302836, tolerance = .000001)
    expect_equal(res[[8]], 1.5676188, tolerance = .000001)
})

# context("ds.quantileMean::smk::split")
test_that("quantileMean_split", {
    ds.assign("D$LAB_HDL", "hdl")
    res <- ds.quantileMean(x='hdl', type='split')

    expect_length(res, 3)
    expect_length(res$sim1, 8)
    expect_equal(res$sim1[[1]], 0.875240, tolerance = .000001)
    expect_equal(res$sim1[[2]], 1.047400, tolerance = .000001)
    expect_equal(res$sim1[[3]], 1.300000, tolerance = .000001)
    expect_equal(res$sim1[[4]], 1.581000, tolerance = .000001)
    expect_equal(res$sim1[[5]], 1.844500, tolerance = .000001)
    expect_equal(res$sim1[[6]], 2.090000, tolerance = .000001)
    expect_equal(res$sim1[[7]], 2.210900, tolerance = .000001)
    expect_equal(res$sim1[[8]], 1.569416, tolerance = .000001)
    expect_length(res$sim2, 8)
    expect_equal(res$sim2[[1]], 0.850280, tolerance = .000001)
    expect_equal(res$sim2[[2]], 1.032200, tolerance = .000001)
    expect_equal(res$sim2[[3]], 1.294000, tolerance = .000001)
    expect_equal(res$sim2[[4]], 1.563000, tolerance = .000001)
    expect_equal(res$sim2[[5]], 1.840000, tolerance = .000001)
    expect_equal(res$sim2[[6]], 2.077000, tolerance = .000001)
    expect_equal(res$sim2[[7]], 2.225000, tolerance = .000001)
    expect_equal(res$sim2[[8]], 1.556648, tolerance = .000001)
    expect_length(res$sim3, 8)
    expect_equal(res$sim3[[1]], 0.876760, tolerance = .000001)
    expect_equal(res$sim3[[2]], 1.039200, tolerance = .000001)
    expect_equal(res$sim3[[3]], 1.304000, tolerance = .000001)
    expect_equal(res$sim3[[4]], 1.589000, tolerance = .000001)
    expect_equal(res$sim3[[5]], 1.856000, tolerance = .000001)
    expect_equal(res$sim3[[6]], 2.098800, tolerance = .000001)
    expect_equal(res$sim3[[7]], 2.244200, tolerance = .000001)
    expect_equal(res$sim3[[8]], 1.574687, tolerance = .000001)
})

#
# Tear down
#

# context("ds.quantileMean::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "hdl"))
})

disconnect.studies.dataset.cnsim()

# context("ds.quantileMean::smk::done")
