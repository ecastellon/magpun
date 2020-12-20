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

test_that("validar dpt-mun",
{
    aa <- departamentos(RD$find_file("data/depmun.sqlite"))
    expect_true(es_dm(5, aa, "dpt", "departamento"))
    expect_true(es_dm("nueva segovia", aa, ccod = "dpt",
                      cnom = "departamento"))
    expect_false(es_dm("jalapa", aa, ccod = "dpt",
                       cnom = "departamento"))
    expect_false(es_dm("", aa, ccod = "dpt",
                       cnom = "departamento"))
    expect_error(es_dm("nueva segovia", aa, ccod = "dpt"))
    expect_error(es_dm("nueva segovia", aa, cnom = "departamento"))
})

test_that("código-municipio",{
    expect_equal(codex_mun(5, 5), 505)
    expect_equal(codex_mun(c(5, 5), c(5, 50)), c(505, 550))
    expect_equal(codex_mun(c(5, 5), c(5, 90)), c(505, NA))
    expect_equal(codex_mun(5, c(5, 10)), c(505, 510))
    expect_error(codex_mun(0, c(5, 10)))
    expect_warning(codex_mun(82, c(5, 10)))
    aa <- codex_mun(82, c(5, 10))
    expect_true(all(is.na(aa)))
})
