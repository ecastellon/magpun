## -*- encoding: utf-8 -*-
## test_file cambia dir de trabajo
context("crud shape")

test_that("lista-shp",
{
    expect_null(shp_list())
    expect_true(shp_list() > 0L)
})

test_that("shape's name",
{
    skip("ua")
    expect_warning(shp_name())
    expect_length(shp_name(), 0)
    expect_length(shp_name("fake.shp"), 0)
    expect_setequal(shp_name("fake", "."), "fake")
})

test_that("read shape",
{
    skip("uu")
    sh <- shp_read(system.file("shape/nc.shp", package="sf"))
    expect_true(inherits(sh, "sf"))
    expect_null(shp_read("no.shp"))
})
