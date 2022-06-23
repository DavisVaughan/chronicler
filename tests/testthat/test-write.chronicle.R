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
    r_summarise(mean_cyl = mean(cyl))

  tmp_path <- tempfile(fileext = ".csv")

  write_chronicle(result_pipe, tmp_path)

  result_read <- read.csv(tmp_path,
                          skip = 9)

  expect_equal(as.data.frame(pick(result_pipe, "value")), # convert explicitely to data.frame so test passes
               result_read)

})

test_that("Test writing chronicle as xlsx", {

  r_select <- record(dplyr::select)
  r_filter <- record(dplyr::filter)
  r_group_by <- record(dplyr::group_by)
  r_summarise <- record(dplyr::summarise)

  result_pipe <- mtcars |>
    as_chronicle() %>=%
    r_select(am, starts_with("c")) %>=%
    r_filter(am == 1) %>=%
    r_group_by(carb) %>=%
    r_summarise(mean_cyl = mean(cyl))

  tmp_path <- tempfile(fileext = ".xlsx")

  write_chronicle(result_pipe, tmp_path)

  result_value <- readxl::read_excel(tmp_path,
                                     sheet = "value")

  result_log <- pull(readxl::read_excel(tmp_path,
                                        sheet = "log"))

  expect_equal(pick(result_pipe, "value"), result_value)
  expect_equal(read.log(result_pipe), result_log)


})
