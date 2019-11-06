test_that("define_rownames_lui_or_components functions produces expected results", {
  expect_equal(define_rownames_lui_or_components("lui"), 
               c("LUI", "deltaLUI", "soil", "isolation", "geo"))
  expect_equal(define_rownames_lui_or_components("components"), 
               c(paste(c("G", "M", "F"), "std", sep = ""), paste("delta", c("Gstd", "Mstd", "Fstd"), sep = ""),
                 "soil", "isolation", "geo"))
})
