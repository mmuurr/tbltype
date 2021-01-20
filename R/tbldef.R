#' @title Table definition/specification.
#'
#' @description
#' Constructs a table specification.
#'
#' @details ## `tbldef.col`
#' Each table's definition is made up of a _named_ list of `tbldef.col` definitions, specifying each column's type.
#' `tbldef.col` takes a casting function `cast` and optionally a `ptype` (the latter of which is described below in "Missing columns").
#' The default `cast` argument is `ident`, which is the identify function and performs no casting on the column at all.
#' 
#' @details ## `[0]`-ability
#' The table's `cast` procedure uses row-binding approach and pre-computes a zero-row table (the definition's `ptype`) to which a (possibly modified) input table will row-combined.
#' To compute the zero-row table, the results of each column's `cast` function must be indexable such that it can be truncated to a length-zero vector or list (or a zero-row dataframe, in the case of a folded dataframe column).
#' Generally-speaking, that means each column's `cast` and `ptype` argument's should be callable like so: `cast(ptype)[0]` or (`cast(ptype)[0,]` in the folded dataframe case).
#'
#' @details ## Missing columns
#' When an input table is missing columns that are part of the table definition, those missing columns are added to the output.
#' The column type in that case is the result of calling the column's `cast` function on the `ptype` argument.
#' By default `ptype` is `NULL`, so `cast` should be tolerant of `NULL` and return a non-`NULL` result, as `NULL` values cannot be in a dataframe column (apart from elements in a listcol).
#' If the `cast` function is not tolerant of `NULL`, specify the `ptype` such that the column's `cast` returns an appropriately-typed value.
#'
#' @details ## 'Folded' dataframes
#' Tibbles support the notion of a column-collapsed 'folded' dataframe.
#' This is distinct from, but conceptually related-to, 'nested' dataframes and listcols.
#' In the folded dataframe case, there is no listcol: the type of the column itself is `tbl`.
#' Folded dataframes have the same number of rows as the surrounding dataframe of which they are a part, but simply allow the columns to be nested.
#' For example:
#' ```r
#' tibble::tibble(col1 = 1:3, col2 = tibble::tibble(col1 = 3:1, col2 = 1:3))
#' ```
#' In that example, the 'outer' table has two columns (`col1` and `col2`), while the inner table (`col2`) has two columns (also called `col1` and `col2`, but distinct from the 'parent' columns of the same name).
#' There are only three 'vector' columns in the full table, however: `col1`, `col2$col1`, and `col2$col2`.
#'
#' `tbldef` supports this notion by permitting additional `tbldef`s as values within the `coldefs` argument.
#' 
#' @details ## Return values
#' `tbldef.col` returns a structure that should only be used within the context of a `tbldef` (hence the naming scheme).
#' `tbldef` returns a list with three components:
#' * `cast` the casting function with this signature: `cast(x = data.frame(), .keep_all = TRUE, ...)`.
#'   * `x` is the table on which to perform the casting. `NULL` is treated identically to an empty (0 x 0) table.
#'   * `.keep_all` is a logical flag telling the casting function whether or not _additional_ columns in `x` should be retained.
#'     These are columns that are not part of the table definition, but perhaps one wants to keep attached to the output table.
#'     In such cases, these columns are moved to the end (right side) and are otherwise not touched.
#' * `ptype` a zero-row 'prototype' table with typed columns.
#' * `klass` simply a pass-back of the `klass` argument in the `tbldef` function (for reference when managing table definitions).
#' 
#' @param cast A casting function that casts its first argument into the appropriate type.
#'             Ideally, this function should handle NULL and convert it into the appropriate type, but if not, use `ptype` for more precise control.
#'             See details for more information.
#' @param ptype The 'prototype' value to use when a column is missing as part of a cast procedure. See details.
#' @param coldefs A named list of `tbldef.col` values.
#' @param klass A classname (or ordered classnames, if more than one) to prepend during the table-casting process.
#'
#' @examples
#' def <- tbldef(list(
#'   col1 = tbldef.col(as.integer),
#'   col2 = tbldef.col(as.character)
#' ))
#'
#' ## casting an empty table (or NULL) results in the zero-row ptype:
#' def$cast()
#' def$ptype
#'
#' ## cast a table with a missing column and drop an extraneous column:
#' def$cast(data.frame(col1 = c("1", "2", "3"), col3 = c(TRUE, FALSE, TRUE)))
#'
#' ## now let's do the same but keep the extraneous column and demonstrate its repositioning:
#' def$cast(data.frame(col3 = c(TRUE, FALSE, TRUE), col1 = c("1", "2", "3")), .keep_all = TRUE)
#' 
#' @name tbldef
NULL


#' @rdname tbldef
#' @export
tbldef.col <- function(cast = ident,
                       ptype = NULL) {
  checkmate::assert_function(cast, nargs = 1)
  force(cast)
  list(
    cast = function(...) cast(..1),
    ptype = ptype
  ) %>% prepend_class("tbltype.tbldef.col")
}


#' @rdname tbldef
#' @export
tbldef <- function(coldefs = list(), klass = NULL) {
  coldefs <- coldefs %||% list()
  checkmate::assert_list(coldefs, names = "named")
  checkmate::assert_character(klass, null.ok = TRUE)

  purrr::walk(coldefs, function(coldef) {
    checkmate::assert(
      checkmate::check_class(coldef, "tbltype.tbldef.col"),
      checkmate::check_class(coldef, "tbltype.tbldef"),
      combine = "or"
    )
  })

  tbl_ptype <-
    coldefs %>%
    purrr::map(function(coldef) .zerorow(coldef$cast(coldef$ptype))) %>%
    tibble::as_tibble()

  cast <- function(x = data.frame(), .keep_all = FALSE, ...) {
    x <- x %||% data.frame()
    checkmate::assert_flag(.keep_all)
    checkmate::assert(if (rlang::is_dictionaryish(x)) TRUE else "x must be dictionaryish (i.e. uniquely named)")

    x <- tibble::as_tibble(x, ...)
    out <- x

    present_names <- intersect(names(x), names(coldefs))
    
    ## trim cols.
    ## this is useful as opposed to _building_ the new tibble from scratch to preserve nrow even if all cols disappear.
    if (!.keep_all) out <- dplyr::select(out, dplyr::all_of(present_names))

    ## cast any of the present cols (possibly recusively):
    for(colname in present_names) {
      out[[colname]] <- coldefs[[colname]]$cast(out[[colname]], .keep_all = .keep_all, ...)
    }

    ## bind_rows, which will attach any cols in ptype not present in output.
    ## setting tbl_ptype as the first arg forces col-ordering, with extra cols (if .keep_all = TRUE) as later cols in the table.
    out <- dplyr::bind_rows(tbl_ptype, out)
    out <- prepend_class(out, klass)
    return(out)
  }

  return(list(cast = cast, ptype = prepend_class(tbl_ptype, klass), klass = klass) %>% prepend_class("tbltype.tbldef"))
}

