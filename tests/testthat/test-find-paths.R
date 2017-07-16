context("find-paths")
library(dplyr)
library(tibble)
d <- tribble(
 ~vertex0, ~vertex1,
 "a", "b",
 "l", "m",
 "d", "e",
 "w", "x",
 "n", "o",
 "b", "c",
 "v", "w",
 "c", "d",
 "u", "v",
 "m", "n",
 "f", "g",
 "x", "z",
 "g", "a",
 "e", "f"
)

order_paths(d$vertex0, d$vertex1)
d_order <- c(1L, 6L, 8L, 3L, 14L, 11L, 13L, 2L, 10L, 5L, 9L, 7L, 4L, 12L)
group <- c(0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 4, 4, 4, 4)
d3 <- d[d_order, ] %>%
  rename(.v0 = vertex0, .v1 = vertex1) %>%
  mutate(path = group)

test_that("path finder works", {
  expect_equal(order_paths(d$vertex0, d$vertex1), d_order)
  d2 <- find_paths(d$vertex0, d$vertex1)
  d2 %>% expect_s3_class("tbl_df") %>% expect_length(3L)
  expect_identical(d2, d[d_order, ] %>%
                       rename(.v0 = vertex0, .v1 = vertex1) %>%
                       mutate(path = group)

)
  })
