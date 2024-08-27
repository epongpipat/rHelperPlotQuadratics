test_that("str_remove_fun_from_term works", {
  expect_equal(str_remove_fun_from_term("scale(x)"), "x")
  expect_equal(str_remove_fun_from_term("scale(x, scale = F)"), "x")
  expect_equal(str_remove_fun_from_term("I(scale(x, scale = F)^2)"), "x")
  expect_equal(str_remove_fun_from_term("poly(x, 2)"), "x")
})
