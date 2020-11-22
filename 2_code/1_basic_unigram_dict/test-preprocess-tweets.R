library(testthat)
library(checkmate)

context("pre-process tweets")

test_that("symbols are removed from text without distortion", {
  expect_identical(remove_umlauts("ärgerlich"), "aergerlich")
  expect_identical(remove_umlauts("Österreich"), "Oesterreich")
  expect_identical(remove_umlauts("Straße"), "Strasse")
  expect_identical(remove_symbols("Wort\nWort"), "Wort Wort")
  expect_identical(remove_symbols("&amp; Co."), "und Co.")
  expect_identical(remove_symbols(
    "Text https://www.google.de Text"), 
    "Text Text")
  expect_identical(remove_symbols("99%"), "99 Prozent")
  expect_identical(remove_symbols("5$"), "5 Dollar")
})

test_that("Python lists are converted fine", {
  expect_list(convert_array_to_list("['hashtag1', 'hashtag2']"))
  expect_length(convert_array_to_list("['hashtag1', 'hashtag2']"), 1)
  expect_identical(convert_array_to_list("[]"), list(""))
})
