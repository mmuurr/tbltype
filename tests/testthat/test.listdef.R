library(testthat)
library(checkmate)


## function signatures:
## * listdef.el(cast=identity, missing=NULL, nullas=missing, castnull=FALSE)
## * listdef(eldefs=list(), klass=NULL)

test_that("ident listdef.el", {
  ## ident just returns itself:
  eldef <- listdef.el()  ## default cast funtion == ident()
  x <- list(
    NULL,
    list(),
    1:3,
    4L,
    "foo",
    c("foo","bar"),
    NA,
    rep(NA_real_, 3)
  )
  purrr::walk(x, function(x) {
    expect_identical(eldef$cast(x), x)
    expect_identical(eldef$cast(x, "foo", "bar"), x)  ## ignored additional args to cast(...)
  })
  expect_identical(eldef$cast(), NULL)

  ## castnull is a flag:
  expect_error(listdef.el(castnull = "foo"))
  expect_error(listdef.el(castnull = NULL))

  ## cast must be a function
  expect_error(listdef.el("foo"))
})


test_that("bad eldefs", {
  expect_error(listdef(as.integer, as.character))
  expect_error(listdef(list(as.integer, as.character)))
  expect_error(listdef(list(el1 = as.integer, el2 = as.character)))
  expect_error(listdef(list(listdef.el(as.integer), listdef.el(as.character))))
  expect_error(listdef(el1 = listdef.el(as.integer), el2 = listdef.el(as.character)))
  expect_class(
    listdef(list(el1 = listdef.el(as.integer), el2 = listdef.el(as.character))),
    "tbltype.listdef"
  )
})


test_that("empty listdef", {
  defs <- list(
    listdef(),
    listdef(list()),
    listdef(NULL)
  )
  for(def in defs) {
    expect_setequal(names(def), c("cast", "ptype", "klass"))
    expect_function(def$cast)
    expect_identical(def$ptype, list())
    expect_null(def$klass)
    
    ## .keep_all = FALSE (default implicit)
    expect_identical(def$cast(as.list(iris)), list())  ## .keep_all = FALSE, default implicit
    expect_identical(def$cast(as.list(iris), .keep_all = FALSE), list())  ## .keep_all = FALSE, explicit
    expect_identical(def$cast(as.list(iris), .keep_all = TRUE), as.list(iris))
  }
})


test_that("nesting/recursion", {
  def <- listdef(list(
    int_vec = listdef.el(as.integer),
    chr_vec = listdef.el(as.character),
    level_2 = listdef(list(
      int_vec = listdef.el(as.integer, castnull = TRUE),
      chr_vec = listdef.el(as.character),
      level_3 = listdef(list(
        int_vec = listdef.el(as.integer, castnull = TRUE),
        chr_vec = listdef.el(as.character, castnull = TRUE),
        tbl = tbldef(list(
          int_col = tbldef.col(as.integer),
          chr_col = tbldef.col(as.character),
          dbl_col = tbldef.col(as.double)
        ), klass = "a_tbl")
      ), klass = "level_3_list")
    ), klass = "level_2_list")
  ), klass = "level_1_list")

  expect_identical(
    def$ptype,
    list(
      int_vec = NULL,
      chr_vec = NULL,
      level_2 = list(
        int_vec = integer(0),
        chr_vec = NULL,
        level_3 = list(
          int_vec = integer(0),
          chr_vec = character(0),
          tbl = tibble::tibble(
            int_col = integer(0),
            chr_col = character(0),
            dbl_col = double(0)
          ) %>% prepend_class("a_tbl")
        ) %>% prepend_class("level_3_list")
      ) %>% prepend_class("level_2_list")
    ) %>% prepend_class("level_1_list")
  )

  expect_identical(def$cast(), def$ptype)

  input_list <- list(
    int_vec = as.double(1),
    chr_vec = as.integer(1),
    level_2 = list(
      int_vec = as.character(1:2),
      dbl_vec = as.double(1:2),  ## not part of the listdef
      level_3 = list(
        tbl = data.frame(
          int_col = as.double(1:3),
          chr_col = as.integer(1:3),
          lgl_col = c(FALSE, TRUE, FALSE)  ## not part of tbldef
        ),
        level_4 = list(  ## not part of the listdef
          foo = "bar",
          bar = "baz"
        )
      ) ## closes level_3
    ) ## closes level_2
  ) ## closes root list

  output_list <- def$cast(input_list)
  expect_identical(
    output_list,
    list(
      int_vec = as.integer(input_list$int_vec),
      chr_vec = as.character(input_list$chr_vec),
      level_2 = list(
        int_vec = as.integer(input_list$level_2$int_vec),
        chr_vec = NULL,
        level_3 = list(
          int_vec = integer(0),
          chr_vec = character(0),
          tbl = tibble::tibble(
            int_col = as.integer(input_list$level_2$level_3$tbl$int_col),
            chr_col = as.character(input_list$level_2$level_3$tbl$chr_col),
            dbl_col = NA_real_  ## recycled to tibble nrow
          ) %>% prepend_class("a_tbl")
        ) %>% prepend_class("level_3_list")
      ) %>% prepend_class("level_2_list")
    ) %>% prepend_class("level_1_list")
  )

  ## check to make sure .keep_all = TRUE is recursively applied:
  output_list <- def$cast(input_list, .keep_all = TRUE)
  expect_identical(
    output_list,
    list(
      int_vec = as.integer(input_list$int_vec),
      chr_vec = as.character(input_list$chr_vec),
      level_2 = list(
        int_vec = as.integer(input_list$level_2$int_vec),
        chr_vec = NULL,
        level_3 = list(
          int_vec = integer(0),
          chr_vec = character(0),
          tbl = tibble::tibble(
            int_col = as.integer(input_list$level_2$level_3$tbl$int_col),
            chr_col = as.character(input_list$level_2$level_3$tbl$chr_col),
            dbl_col = NA_real_,  ## recycled to tibble nrow
            lgl_col = input_list$level_2$level_3$tbl$lgl_col
          ) %>% prepend_class("a_tbl"),
          level_4 = input_list$level_2$level_3$level_4
        ) %>% prepend_class("level_3_list"),
        dbl_vec = input_list$level_2$dbl_vec
      ) %>% prepend_class("level_2_list")
    ) %>% prepend_class("level_1_list")
  )
})


test_that("missing and NULL cases", {
  def <- listdef(list(
    el1 = listdef.el(as.integer, missing = NULL, castnull = TRUE)
  ))
  expect_identical(def$cast(list()), list(el1 = integer(0)))

  def <- listdef(list(
    el1 = listdef.el(as.integer, missing = 42)
  ))
  expect_identical(def$cast(list()), list(el1 = 42L))

  def <- listdef(list(
    el1 = listdef.el(as.integer, missing = "foo")
  )) %>% expect_warning()
  expect_warning(expect_identical(def$cast(list()), list(el1 = NA_integer_)))

  def <- listdef(list(
    el1 = listdef.el(as.integer, missing = NULL, castnull = FALSE)
  ))
  expect_identical(def$cast(list()), list(el1 = NULL))

  def <- listdef(list(
    el1 = listdef.el(as.integer, nullas = 42)
  ))
  expect_identical(def$cast(list()), list(el1 = 42L))

  def <- listdef(list(
    el1 = listdef.el(as.integer, castnull = TRUE)
  ))
  expect_identical(def$cast(list(el1 = NULL)), list(el1 = integer(0)))

  def <- listdef(list(
    el1 = listdef.el(as.integer, castnull = FALSE),
    el2 = listdef.el(as.integer)
  ))
  expect_identical(
    def$cast(list(el1 = NULL, el2 = NULL)),
    list(el1 = NULL, el2 = NULL)
  )

  def <- listdef(list(
    el0 = listdef.el(as.integer, nullas = 42),
    el1 = listdef.el(as.integer, nullas = 42, castnull = FALSE),
    el2 = listdef.el(as.integer, nullas = 42, castnull = TRUE)
  ))
  expect_identical(
    def$cast(list(el0 = NULL, el1 = NULL, el2 = NULL)),
    list(el0 = 42L, el1 = 42L, el2 = 42L)
  )
})
