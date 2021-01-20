listdef.el <- function(cast = ident,
                       missing = NULL,
                       nullas = missing,
                       castnull = FALSE) {
  checkmate::assert_function(cast, nargs = 1)
  checkmate::assert_flag(castnull, null.ok = FALSE)
  force(cast)
  list(
    cast = function(x, ...) cast(x),
    missing = missing,
    nullas = nullas,
    castnull = castnull
  ) %>% prepend_class("listdef.el")
}


listdef <- function(eldefs = list(), klass = NULL) {
  eldefs <- eldefs %||% list()
  checkmate::assert_list(eldefs, names = "named")
  checkmate::assert_character(klass, null.ok = TRUE)
  
  elnames <- names(eldefs)

  purrr::walk(eldefs, function(eldef) {
    checkmate::assert(
      checkmate::check_class(eldef, "listdef.el"),
      checkmate::check_class(eldef, "listdef"),
      checkmate::check_class(eldef, "castdef"),
      combine = "or"
    )
  })
  
  cast <- function(x = list(), .keep_all = FALSE, ...) {
    x <- x %||% list()
    checkmate::assert_flag(.keep_all)
    checkmate::assert_list(x, names = "named")
    checkmate::assert(rlang::is_dictionaryish(x))

    ## Which elements are already present in desired output, which need to be added, and which are extraneous (to optionally keep or discard)?
    present_names <- intersect(elnames, names(x))  ## elements in eldefs that are present in x
    missing_names <- setdiff(elnames, names(x))  ## elements in eldefs that are missing in x
    extra_names <- setdiff(names(x), elnames)  ## elements in x that aren't in eldefs

    out <- purrr::map(elnames, function(elname) {

      ## init value to x element or missing.
      outel <-
        if (!exists(elname, x)) {
          if (inherits(eldefs[[elname]], "castdef")) {
            NULL
          } else if (inherits(eldefs[[elname]], "listdef")) {
            list()
          } else if (inherits(eldefs[[elname]], "tbldef")) {
            data.frame()
          } else {
            eldefs[[elname]]$missing
          }
        } else {
          x[[elname]]
        }

      ## handle NULLs:
      if (is.null(outel)) outel <- eldefs[[elname]]$nullas

      ## cast (unless NULL and !castnull):
      if (inherits(eldefs[[elname]], "castdef")) {
        outel <- eldefs[[elname]]$cast(outel, .keep_all = .keep_all, ...)
      } else if (is.null(outel) && !isTRUE(eldefs[[elname]]$castnull)) {
        ## NOP
      } else {
        outel <- eldefs[[elname]]$cast(outel)
      }

      outel
    }) %>% purrr::set_names(elnames)

    if (isTRUE(.keep_all)) {
      out <- c(out, x[extra_names])
    }

    out <- prepend_class(out, klass)
    return(out)
  }

  return(list(cast = cast, klass = klass, ptype = cast(list())) %>% prepend_class(c("listdef", "castdef")))
}  
