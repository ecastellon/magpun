## -*- encoding: utf-8 -*-
## test_file cambia dir de trabajo
context("crud shape")

test_that("lista-shp",
{
    expect_length(shp_list(rut = RD$find_file("r")), 0)
    expect_length(shp_list(rut = RD$find_file("tests/testthat")), 1)
})

test_that("shape's name",
{
    skip("no vale")
    expect_warning(shp_name())
    expect_length(shp_name(), 0)
    expect_length(shp_name("fake.shp"), 0)
    expect_setequal(shp_name("fake", "."), "fake")
})

test_that("read shape",
{
    sh <- shp_read(system.file("shape/nc.shp", package="sf"))
    expect_s3_class(sh, "sf")
    expect_null(shp_read("no.shp"))
})

test_that("bloq dpt",
{
    ss <- file.path("c:/encuestas/marco/bloquespuntos",
                    "bloques2016/bloques2016.shp")
    expect_s3_class(shp_blo_dpt(50, ss), "sf")
    expect_warning(shp_blo_dpt(90, ss))
    expect_warning(shp_blo_dpt(90, ss, dpt = "bb"))
})
