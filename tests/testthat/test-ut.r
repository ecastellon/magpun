# -*- coding: utf-8 -*-

context("func utilidad general")

test_that("letras string",
{
    expect_error(solo_letras_ascii(1))
    expect_equal(solo_letras_ascii("  a éf o Ú"), "aefou")
})

test_that("num entre",
{
    expect_error(num_entre("2", 1, 2))
    expect_error(num_entre("2", 1))
    expect_true(num_entre(1.1, 0, 2))
    expect_true(num_entre(2, 0, 2, T))
    expect_false(num_entre(2, 0, 2))
    expect_false(num_entre(-1, 0, 2))
    expect_false(num_entre(2.01, 0, 2, T))
})

test_that("concat int",
{
    expect_error(concatenar_int(2))
    expect_equal(concatenar_int(2, 3), 203)
    expect_equal(concatenar_int(2, 3, 3), 2003)
    expect_false(concatenar_int(2, 3, 3) == 203)
})

## -- archivos --
test_that("list_ar",
{
    expect_error(list_ar(NULL))
    expect_equal(list_ar("d", "r", RD$find_file("r/")),
                 "dm.r")
    aa <- list_ar(rut = RD$find_file("r"))
    expect_true(length(aa) > 0)
    
    aa <- list_ar("12", rut = RD$find_file("r"))
    expect_true(length(aa) == 0)
})
