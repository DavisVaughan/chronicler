test_that("Test writing chronicle as csv", {

  r_select <- record(dplyr::select)
  r_filter <- record(dplyr::filter)
  r_group_by <- record(dplyr::group_by)
  r_summarise <- record(dplyr::summarise)

  result_pipe <- mtcars |>
    as_chronicle() %>=%
    r_select(am, starts_with("c")) %>=%
    r_filter(am == 1) %>=%
    r_group_by(carb) %>=%
    r_summarise(mean_cyl = mean(cyl)) %>%
    pick("value") %>%
    as.data.frame()

  result_read <- read.csv("~/Documents/chronicler/tests/testthat/csv_test_write_chronicler.csv",
                          skip = 9)

  expect_equal(result_pipe, result_read)

})
