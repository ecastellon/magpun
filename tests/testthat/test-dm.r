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
    bb <- qry_dm(aa, RD$find_file("data/depmun.sqlite"))
    expect_true(is.data.frame(bb))
    expect_true(nrow(bb) == 17)
})

test_that("municipio-dpt",
{
    dd <- departamentos(RD$find_file("data/depmun.sqlite"))
    expect_error(departamentos(1))
    expect_warning(departamentos("c:/aa.sqlite"))
    #expect_null(departamentos("c:/aa.sqlite"))
    expect_true(is.data.frame(dd))
    expect_true(nrow(dd) == 17)

    dd <- municipios(dbf = RD$find_file("data/depmun.sqlite"))
    expect_error(municipios(0))
    expect_true(is.data.frame(dd))
    dd <- municipios(c(5, 20),
                     dbf = RD$find_file("data/depmun.sqlite"))
    expect_true(is.data.frame(dd))
})
