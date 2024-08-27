test_that("str_remove_scale_from_formula works", {
  expect_equal(str_remove_scale_from_formula("y ~ scale(x)", "x"), 'y ~ x')
  expect_equal(str_remove_scale_from_formula("y ~ scale(x, scale = F)", "x"), 'y ~ x')
  expect_equal(str_remove_scale_from_formula("y ~ I(scale(x)^2)", "x"), 'y ~ I(x^2)')
  expect_equal(str_remove_scale_from_formula("y ~ I(scale(x, scale = F)^2)", "x"), 'y ~ I(x^2)')
  expect_equal(str_remove_scale_from_formula("y ~ scale(x) + I(scale(x)^2)", "x"), 'y ~ x + I(x^2)')
  expect_equal(str_remove_scale_from_formula("y ~ scale(x, scale = F) + I(scale(x, scale = F)^2)", "x"), 'y ~ x + I(x^2)')
  expect_equal(str_remove_scale_from_formula("y ~ scale(x, scale = F) + I(scale(x, scale = F)^2) + otherx", "x"), 'y ~ x + I(x^2) + otherx')
})
