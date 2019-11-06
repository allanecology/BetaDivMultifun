test_that("define_rownames_lui_or_components functions produces expected results", {
  expect_equal(define_rownames_lui_or_components("lui"), 
               c("LUI", "deltaLUI", "soil", "isolation", "geo"))
  expect_equal(define_rownames_lui_or_components("components"), 
               c(paste(c("G", "M", "F"), "std", sep = ""), paste("delta", c("Gstd", "Mstd", "Fstd"), sep = ""),
                 "soil", "isolation", "geo"))
  expect_error(define_rownames_lui_or_components("luo"), "Error: input is not either lui or components.", fixed = T)
})

test_that("paste_gdm_input_path_together", {
  expect_equal(paste_gdm_input_path_together(pathtoout = "Documents/BEF/ANALYSIS_RESULT", name="sample_gdmin"),
               "Documents/BEF/ANALYSIS_RESULT/cluster/sample_gdmin_input.Rds")
})

