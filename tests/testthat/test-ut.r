# -*- encoding: utf-8 -*-

context("func utilidad general")

test_that("letras string",
{
    expect_error(sec_letras(1))
    expect_equal(sec_letras("  a éf o Ú"), "aefou")
})
