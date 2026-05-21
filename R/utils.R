#' Null coalescing operator
#'
#' Returns `y` when `x` is `NULL`, otherwise returns `x`.
#'
#' @param x Value to test.
#' @param y Fallback value.
#'
#' @return `x` if non-NULL, else `y`.
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Repair empty or NA column names in an extracted table
#'
#' Replaces any empty or `NA` column names with `col_<position>` and ensures
#' all names are unique.
#'
#' @param tbl A tibble to repair.
#' @param page_nm Page name for warning messages.
#' @param table_idx Table index for warning messages.
#' @param warn Whether to emit a warning when repairs are made.
#'
#' @return The tibble with repaired column names.
#' @keywords internal
repair_tbl_names <- function(tbl, page_nm = NA_character_, table_idx = NA_integer_, warn = TRUE) {
	nms <- names(tbl)

	if (is.null(nms)) {
		return(tbl)
	}

	nms <- as.character(nms)
	bad <- is.na(nms) | nms == ""

	if (any(bad)) {
		if (isTRUE(warn)) {
			bad_positions <- which(bad)
			warning(
				paste0(
					"Repaired ",
					length(bad_positions),
					" empty/NA column name(s) in ",
					ifelse(is.na(page_nm), "unknown page", page_nm),
					", table ",
					ifelse(is.na(table_idx), "unknown", as.character(table_idx)),
					" at column position(s): ",
					paste(bad_positions, collapse = ", "),
					"."
				),
				call. = FALSE
			)
		}

		nms[bad] <- paste0("col_", which(bad))
	}

	names(tbl) <- make.unique(nms)
	tbl
}
