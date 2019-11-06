test_that("get_random_number_from_truncated_normal_dist", {
  expect_length(get_random_number_from_truncated_normal_dist(length_of_vector = 10, mean = 0.5, max = 1, min = 0), 10)
  expect_equal(max(get_random_number_from_truncated_normal_dist(length_of_vector = 10, mean = 0.5, max = 1, min = 0)) <= 1, T)
})
