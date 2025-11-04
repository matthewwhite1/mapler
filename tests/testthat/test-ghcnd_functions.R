test_that("download_ghcnd() fails gracefully", {
  expect_error(download_ghcnd(1))
  suppressWarnings(expect_warning(download_ghcnd("hehe")))
  suppressWarnings(expect_warning(download_ghcnd(c("US", "hehe"))))
  expect_error(download_ghcnd("US", "hehe"))
  expect_error(download_ghcnd("US", 1))
})

test_that("get_ghcnd_country_ids() works", {
  suppressWarnings(expect_warning(get_ghcnd_country_ids("hehe")))
  expect_true(is.character(get_ghcnd_country_ids("US")))
  expect_true(length(get_ghcnd_country_ids("US")) > 1)
  suppressWarnings(expect_true(length(get_ghcnd_country_ids(c("US", "hehe"))) > 1))
  suppressWarnings(expect_true(rlang::is_empty(get_ghcnd_country_ids("hehe"))))
  expect_error(get_ghcnd_country_ids(1))
})
