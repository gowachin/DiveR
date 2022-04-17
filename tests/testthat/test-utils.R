
#### Test ceiling_dec ####

test_that("ceiling_dec", {
  expect_identical(ceiling_dec(1.259, 2), 1.26)
  expect_identical(ceiling_dec(1.9, 0), 2)
  expect_identical(ceiling_dec(29, -1), 30)
  expect_identical(ceiling_dec(1.251, 2), 1.26)
  expect_identical(ceiling_dec(1.2, 0), 2)
  expect_identical(ceiling_dec(23, -1), 30)
})
