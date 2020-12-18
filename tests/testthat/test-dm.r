## -*- encoding: utf-8 -*-
context("test dpto municipio")

test_that("conn-base-datos",
{
    expect_error(con_dm(1))
    expect_null(con_dm("c:/aa.sqlite"))
    expect_warning(con_dm("c:/aa.sqlite"))
    
    cn <- con_dm()
    expect_true(inherits(cn, "SQLiteConnection"))
    DBI::dbDisconnect(cn)

    cn <- con_dm(RD$find_file("depmun.sqlite"))
    expect_true(inherits(cn, "SQLiteConnection"))
    DBI::dbDisconnect(cn)
})

test_that("query-data",
{
    expect_error(qry_dm(1))
    expect_error(qry_dm(dbf = 1))
    expect_error(qry_dm(qry = c("aa", "bb")))

    aa <- "select * from departamento"
    bb <- qry_dm(aa)
    expect_true(is.data.frame(bb))
    expect_true(nrow(bb) == 17)
})

test_that("data.frame municipio departamento",
{
    skip("")
    dd <- df_muni()
    expect_true(inherits(dd, "data.frame"))
    expect_null(df_muni("fake.shp"))
    expect_null(df_muni("fake.rda"))
})

test_that("municipio-dpt",
{
    expect_null(municipios_dpt(0))
    expect_error(municipios_dpt(0, 1))
    expect_true(is.data.frame(municipios_dpt(5)))
})
