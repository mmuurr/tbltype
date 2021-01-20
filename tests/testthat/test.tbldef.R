library(testthat)
library(checkmate)


test_that("tbldef.col ident", {
  coldef <- tbldef.col()
  expect_identical(coldef$cast(), NULL)
  expect_identical(coldef$cast(NULL), NULL)
  expect_identical(coldef$cast(1:3), 1:3)
  expect_identical(coldef$cast(4L), 4L)
})


test_that("tbldef.col", {
  cd <- tbldef.col()
  expect_identical(cd$cast(4L), 4L)
  expect_identical(cd$cast(4L, "foo", "bar", list(1:3, 3:1)), 4L)

  expect_error(tbldef.col(letters))

  cd <- tbldef.col(as.character)
  expect_identical(cd$cast(1:3), c("1","2","3"))

  cd <- tbldef.col(function(x) lubridate::as_datetime(x, tz = "America/Denver"))
  now <- lubridate::now("America/Denver") %>% lubridate::floor_date("seconds")
  expect_equal(cd$cast(now), now)
  expect_equal(cd$cast(as.character(now)), now)
  expect_equal(cd$cast(now, "foo", "bar"), now)
  expect_equal(cd$cast(as.character(now), "foo", "bar"), now)

  cd <- tbldef.col(function(x) rlang::as_double(x)) ## as_double doesn't take dots ... tests wrapper that does take dots ...
  expect_identical(cd$cast(1:3), as.double(1:3))
  expect_identical(cd$cast(1:3, "foo", "bar"), as.double(1:3))

  cd <- tbldef.col(function(x) vctrs::vec_cast(x, double()))
  expect_identical(cd$cast(1:3), as.double(1:3))
  expect_identical(cd$cast(1:3, "foo", "bar"), as.double(1:3))
})


test_that("no coldefs", {
  defs <- list(
    tbldef(),
    tbldef(list()),
    tbldef(NULL)
  )
  for(def in defs) {
    expect_setequal(names(def), c("cast", "ptype", "klass"))
    expect_function(def$cast)
    expect_tibble(def$ptype)
    ## don't retain cols:
    expect_equal(ncol(def$cast(iris)), 0)
    expect_equal(nrow(def$cast(iris)), nrow(iris))
    ## retain cols:
    expect_setequal(names(def$cast(iris, .keep_all = TRUE)), names(iris))
    expect_equal(nrow(def$cast(iris, .keep_all = TRUE)), nrow(iris))
  }
})


test_that("non-NULL ptype", {
  def <- tbldef(list(
    a = tbldef.col(identity, as.integer(1:10))
  ))
  
  x <- def$cast()
  expect_identical(x$a, integer(0))
  expect_identical(x, def$ptype)

  ## this breaks the process, because def$ptype$a is integer, and character can't be combined (in bind_rows) with integer.
  expect_error(def$cast(data.frame(a = letters)))
})


test_that("bad coldefs", {
  expect_error(tbldef(as.integer, as.character))
  expect_error(tbldef(list(as.integer, as.character)))
  expect_error(tbldef(list(col1 = as.integer, col2 = as.character)))
  expect_error(tbldef(list(col1 = list(as.integer), col2 = list(as.character))))  ## must be coldef objects
  expect_error(tbldef(list(tbldef.col(as.integer), tbldef.col(as.character))))  ## unnamed cols
  expect_error(tbldef(col1 = tbldef.col(as.integer), col2 = tbldef.col(as.character)))  ## coldefs should be in a list
  expect_class(tbldef(list(col1 = tbldef.col(as.integer), col2 = tbldef.col(as.character))), "tbltype.tbldef")
})


test_that("null & empty castings", {
  def <- tbldef(list(
    a = tbldef.col(as.integer),
    b = tbldef.col(as.character)
  ))
  expect_equal(def$ptype, tibble::tibble(a = integer(0), b = character(0)))
  expect_equal(def$cast(NULL), def$ptype)
  expect_equal(def$cast(list()), def$ptype)
  expect_equal(def$cast(data.frame()), def$ptype)
  expect_equal(def$cast(tibble::tibble()), def$ptype)
  expect_equal(def$cast(dplyr::slice(iris, 0)), def$ptype)
  expect_equal(def$cast(data.frame(foo = logical(0))), def$ptype)
  expect_equal(def$cast(data.frame(foo = logical(10))), tibble::tibble(a = as.integer(rep(NA, 10)), b = as.character(rep(NA, 10))))
})


test_that("tbldef cannot contain listdef", {
  expect_error(
    tbldef(list(
      d = listdef(list(
        d.a = listdef.el(as.integer)
      ))
    ))
  )
})


test_that("recursion and nesting", {
  def <- tbldef(list(
    a = tbldef.col(as.integer),
    b = tbldef.col(as.character),
    c = tbldef(list(
      a = tbldef.col(as.integer),
      b = tbldef.col(as.character),
      c = tbldef(list(
        a = tbldef.col(as.integer),
        b = tbldef.col(as.character)
      ), klass = "l3")
    ), klass = "l2")
  ), klass = "l1")

  x <- def$cast()
  expect_identical(x, def$ptype)
  expect_data_frame(x)
  expect_s3_class(x, "l1")
  expect_data_frame(x$c)
  expect_s3_class(x$c, "l2")
  expect_data_frame(x$c$c)
  expect_s3_class(x$c$c, "l3")
  expect_equal(def$ptype, tibble::tibble(
    a = integer(0),
    b = character(0),
    c = tibble::tibble(
      a = integer(0),
      b = character(0),
      c = tibble::tibble(
        a = integer(0),
        b = character(0)
      ) %>% prepend_class("l3")
    ) %>% prepend_class("l2")
  ) %>% prepend_class("l1"))

  x <- tibble::tibble(
    a = as.double(1:3),
    b = as.integer(3:1),
    c = tibble::tibble(
      a = as.double(1:3),
      b = as.integer(3:1),
      c = tibble::tibble(
        a = as.double(1:3),
        b = as.integer(3:1)
      )
    )
  )
  x <- def$cast(x)
  expect_data_frame(x, nrows = 3, ncols = 3)
  expect_s3_class(x, "l1")
  expect_identical(x$a, as.integer(1:3))
  expect_identical(x$b, as.character(3:1))
  expect_data_frame(x$c, nrows = 3, ncols = 3)
  expect_s3_class(x$c, "l2")
  expect_identical(x$c$a, as.integer(1:3))
  expect_identical(x$c$b, as.character(3:1))
  expect_data_frame(x$c$c, nrows = 3, ncols = 2)
  expect_s3_class(x$c$c, "l3")
  expect_identical(x$c$c$a, as.integer(1:3))
  expect_identical(x$c$c$b, as.character(3:1))
})


test_that("basic correct behavior", {
  def <- tbldef(list(
    a = tbldef.col(as.character),
    c = tbldef.col(as.character),
    e = tbldef.col(as.logical)
  ))

  x <- data.frame(
    a = as.integer(1:3),
    b = as.double(1:3),
    c = as.character(1:3)
  )

  ## .keep_all = TRUE
  y <- def$cast(x, .keep_all = TRUE)
  expect_tibble(y, nrows = 3, ncols = 4)
  expect_identical(y$a, as.character(x$a))
  expect_identical(y$c, as.character(x$c))
  expect_identical(y$e, rep(NA, 3))
  expect_identical(y$b, x$b)
  expect_equal(names(y), c("a","c","e","b"))  ## pytpe order takes precedence

  ## .keep_all = FALSE (default; implicit)
  y <- def$cast(x)
  expect_tibble(y, nrows = 3, ncols = 3)
  expect_identical(y$a, as.character(x$a))
  expect_identical(y$c, as.character(x$c))
  expect_identical(y$e, rep(NA, 3))
  expect_equal(names(y), c("a","c","e"))  ## pytpe order takes precedence
  
  ## .keep_all = FALSE (explicit)
  y <- def$cast(x, .keep_all = FALSE)
  expect_tibble(y, nrows = 3, ncols = 3)
  expect_identical(y$a, as.character(x$a))
  expect_identical(y$c, as.character(x$c))
  expect_identical(y$e, rep(NA, 3))
  expect_equal(names(y), c("a","c","e"))  ## pytpe order takes precedence
})


test_that("listcol with n = 0 rows", {
  def <- tbldef(list(
    a = tbldef.col(as.list)
  ))

  expect_identical(def$ptype, tibble::tibble(a = list()))
  expect_identical(def$cast(), def$ptype)
  expect_identical(def$cast(data.frame()), def$ptype)
  expect_identical(def$cast(tibble::tibble()), def$ptype)
})


test_that("listcol with n > 0 rows", {
  def <- tbldef(list(
    a = tbldef.col(as.integer),
    b = tbldef.col(as.list)
  ))

  expect_identical(
    def$cast(data.frame(a = as.double(1:3))),
    tibble::tibble(
      a = as.integer(1:3),
      b = lapply(1:3, function(x) NULL)
    )
  )
})


test_that("what if df arg to f$as is unnamed?", {
  def <- tbldef(list(
    a = tbldef.col(as.integer),
    b = tbldef.col(as.character)
  ))

  x <- data.frame(as.integer(1:3), as.character(3:1))
  names(x) <- NULL

  expect_error(def$cast(x))

  y <- tibble::as_tibble(x, .name_repair = "unique")

  z <- def$cast(y, .keep_all = TRUE)
  expect_tibble(z, nrow = 3, ncol = 4)
  expect_identical(names(z), c("a","b","...1","...2"))
  expect_identical(z$a, rep(NA_integer_, 3))
  expect_identical(z$b, rep(NA_character_, 3))
  expect_identical(z$...1, as.integer(1:3))
  expect_identical(z$...2, as.character(3:1))
  expect_type(z$a, "integer")
  expect_type(z$b, "character")
  expect_type(z$...1, "integer")
  expect_type(z$...2, "character")

  z <- def$cast(y)
  expect_tibble(z, nrow = 3, ncol = 2)
  expect_identical(names(z), c("a","b"))
  expect_identical(z$a, rep(NA_integer_, 3))
  expect_identical(z$b, rep(NA_character_, 3))
  expect_type(z$a, "integer")
  expect_type(z$b, "character")
})



test_that("anonymous cast function", {
  def <- tbldef(list(
    a = tbldef.col(function(x) lubridate::as_datetime(x, tz = "America/Denver"))
  ))
  
  x.dttm <- data.frame(a = c(lubridate::now("America/Denver"), lubridate::now("America/Denver") + lubridate::period(1, "minutes")))
  x.dttm$a <- lubridate::floor_date(x.dttm$a, "seconds")  ## floor to seconds to not lose precision for the x.chr test
  x.chr <- data.frame(a = as.character(x.dttm$a))
  x.num <- data.frame(a = as.numeric(x.dttm$a))
  
  for(x in list(x.dttm, x.chr, x.num)) {
    expect_equal(as.data.frame(def$cast(x)), x.dttm)
    expect_s3_class(def$cast(x)$a, "POSIXt")
    expect_s3_class(def$cast(x)$a, "POSIXct")
  }
})


test_that("existing list elements are preserved", {
  def <- tbldef(list(
    vec = tbldef.col(as.integer),
    lst = tbldef.col(as.list)
  ))

  x <- tibble::tibble(
    vec = 1:3,
    lst = list(iris, NULL, mtcars)
  )

  y <- def$cast(x)
  expect_type(y$lst, "list")
  expect_equal(nrow(y), nrow(x))
  expect_equal(x$vec, y$vec)
  expect_equal(x$lst, y$lst)
  for(i in nrow(x)) {
    expect_equal(x$vec[[i]], y$vec[[i]])
    expect_equal(x$lst[[i]], y$lst[[i]])
  }
})
