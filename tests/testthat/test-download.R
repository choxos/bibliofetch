test_that("check_and_download_scihub handles invalid input", {
  expect_error(check_and_download_scihub(NULL))
  expect_error(check_and_download_scihub(numeric(0)))
  expect_error(check_and_download_scihub(""))
})

test_that("check_and_download_scihub creates output directory", {
  dir <- tempfile("test_download")
  expect_false(dir.exists(dir))
  check_and_download_scihub("10.1038/nature09492", output_dir = dir)
  expect_true(dir.exists(dir))
  unlink(dir, recursive = TRUE)
})