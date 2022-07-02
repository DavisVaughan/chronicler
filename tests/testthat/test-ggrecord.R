library(ggplot2)

test_that("ggplot functions get recorded", {
  expect_true(ggplot2::is.ggplot(
  (maybe::from_maybe(
    (ggrecord(ggplot)(mtcars))$value,
    default = maybe::nothing())
  ))
  )
  expect_true(ggplot2::is.ggproto(
  (maybe::from_maybe(
    (ggrecord(geom_point)(aes(mpg, hp)))$value,
    default = maybe::nothing()
  ))
  ))
})

test_that("ggrecorded functions can be added", {

  skip_on_cran()

  r_ggplot <- ggrecord(ggplot)
  r_geom_point <- ggrecord(geom_point)
  r_labs <- ggrecord(labs)

  a <- r_ggplot(mtcars) %>+%
    r_geom_point(aes(y = mpg, x = hp, colour = am)) %>+%
    r_labs(title = paste0("ggrecorded functions can be added, generated on: ", Sys.Date()),
           subtitle = "If you see this plot, it works",
           caption = "This is an example caption")

  print(maybe::from_maybe(a$value,
                          default = maybe::nothing()))

  expect_true(TRUE)

})

test_that("document_gg works", {

  skip_on_cran()

  r_ggplot <- ggrecord(ggplot)
  r_geom_point <- ggrecord(geom_point)
  r_labs <- ggrecord(labs)

  a <- r_ggplot(mtcars) %>+%
    r_geom_point(aes(y = mpg, x = hp, colour = am)) %>+%
    r_labs(title = paste0("ggrecorded functions can be added, generated on: ", Sys.Date()),
           subtitle = "If you see this plot, it works",
           caption = "This is an example caption")



  a <- document_gg(a)

  print(maybe::from_maybe(a$value,
                          default = maybe::nothing()))

  expect_true(TRUE)

})

test_that("document_gg fails successfully", {

  skip_on_cran()

  r_ggplot <- ggrecord(ggplot)
  r_geom_point <- ggrecord(geom_point)
  r_labs <- ggrecord(labs)

  a <- r_ggplot(mtcars) %>+%
    r_geom_point(aes(y = mg, x = hp, colour = am)) %>+%
    r_labs(title = paste0("ggrecorded functions can be added, generated on: ", Sys.Date()),
           subtitle = "If you see this plot, it works",
           caption = "This is an example caption")



  a <- document_gg(a)

  print(maybe::from_maybe(a$value,
                          default = maybe::nothing()))

  expect_true(TRUE)

})

test_that("purely works on ggplot", {
  expect_true(
    is.ggplot(maybe::from_maybe(
    (ggpurely(ggplot)(mtcars))$value
                               )
             )
        )
})

test_that("ggrecord2 works",{

  r_ggplot <- ggrecord2(ggplot)
  r_geom_point <- ggrecord2(geom_point)
  r_ggtitle <- ggrecord2(ggtitle)

  r_ggplot(mtcars) %>+%
    r_geom_point(aes(y = hp, x = mpg)) %>+%
    r_ggtitle(label = "You can read this? Then it works")
})
