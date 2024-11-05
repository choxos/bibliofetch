test_that("download_scihub handles invalid input", {
  expect_error(download_scihub(NULL))
  expect_error(download_scihub(numeric(0)))
  expect_error(download_scihub(""))
})

test_that("download_scihub creates output directory", {
  dir <- tempfile("test_download")
  expect_false(dir.exists(dir))
  download_scihub("10.1038/nature09492", output_dir = dir)
  expect_true(dir.exists(dir))
  unlink(dir, recursive = TRUE)
})