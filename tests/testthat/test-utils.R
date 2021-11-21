
#### Test ceiling_dec ####

test_that("ceiling_dec", {
  expect_identical(ceiling_dec(1.259, 2), 1.26)
  expect_identical(ceiling_dec(1.9, 0), 2)
  expect_identical(ceiling_dec(29, -1), 30)
  expect_identical(ceiling_dec(1.251, 2), 1.26)
  expect_identical(ceiling_dec(1.2, 0), 2)
  expect_identical(ceiling_dec(23, -1), 30)
})


test_that("insertRow", {
  existingDF <- as.data.frame(matrix(as.numeric(1:12),nrow=3,ncol=3))
  expect_identical(insertRow(existingDF, newrow = seq(3), r = 3), 
                   as.data.frame(matrix(c(1,2,1,3:5,2,6:8,3,9), nrow = 4, ncol = 3)))
})
