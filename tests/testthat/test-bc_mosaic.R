test_df = data.frame(Xgrp = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
                     Ygrp = c("X", "Y", "Z", "X", "Y", "Z", "X", "Y", "Z"),
                     Val = c(1, 3, 2, 5, 3, 4, 2, 6, 1))

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
