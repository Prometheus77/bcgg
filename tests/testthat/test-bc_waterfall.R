test_df <- data.frame(Label = c('2018', 'Mix', 'App lift', 'Click rate', '2019'),
                      Incr.Value = c(500, -25, 35, 20, 0),
                      Abs.Value = c(500, 475, 510, 530, 530),
                      Anchors = c(TRUE, FALSE, TRUE, FALSE, TRUE))

test_that("make_incremental works", {
  expect_equal({
    make_incremental(test_df, Abs.Value) %>%
      dplyr::pull(.incr.values)
    },
    test_df$Incr.Value
    )
})

test_that("make_absolute works", {
  expect_equal({
    make_absolute(test_df, Incr.Value) %>%
      dplyr::pull(.abs.values)
  },
  test_df$Abs.Value
  )
})
