#' Normalize page-level OCR word confidence output
#'
#' Converts `word_confidence_scores` from the API response into a tibble with
#' absolute start and end character offsets and numeric confidence values.
#'
#' @param page_confidence_scores A page-level `confidence_scores` object.
#'
#' @return A tibble with `word_start`, `word_end`, and `confidence`.
#'
#' @examples
#' mistralOCR:::extract_word_confidence(list(
#'   word_confidence_scores = list(
#'     list(text = "YEAR", confidence = 0.99, start_index = 0)
#'   )
#' ))
#' @keywords internal
extract_word_confidence <- function(page_confidence_scores) {
	words <- page_confidence_scores$word_confidence_scores %||% list()

	if (length(words) == 0) {
		return(
			tibble::tibble(
				word_start = integer(),
				word_end = integer(),
				confidence = numeric()
			)
		)
	}

	purrr::map_dfr(words, \(w) {
		start_1 <- as.integer((w$start_index %||% NA_integer_) + 1L)
		text <- as.character(w$text %||% "")
		width <- nchar(text)
		if (is.na(start_1)) {
			return(
				tibble::tibble(
					word_start = integer(),
					word_end = integer(),
					confidence = numeric()
				)
			)
		}

		tibble::tibble(
			word_start = start_1,
			word_end = start_1 + max(width, 1L) - 1L,
			confidence = as.numeric(w$confidence %||% NA_real_)
		)
	})
}

#' Normalize alternate raw table-confidence schemas
#'
#' Handles multiple API table confidence layouts, including nested `rows/cells`
#' and flat `entries`-style responses.
#'
#' @param raw_table A raw table object from the OCR API.
#'
#' @return A tibble with `row_index`, `col_index`, and `confidence`.
#' @keywords internal
extract_raw_table_confidence <- function(raw_table) {
	empty <- tibble::tibble(
		row_index = integer(),
		col_index = integer(),
		confidence = numeric()
	)

	if (is.null(raw_table)) {
		return(empty)
	}

	rows <- raw_table$rows %||% list()
	if (length(rows) > 0) {
		out <- purrr::imap_dfr(rows, \(rw, r_i) {
			cells <- rw$cells %||% rw$columns %||% list()

			if (length(cells) == 0) {
				return(empty)
			}

			purrr::imap_dfr(cells, \(cell, c_i) {
				tibble::tibble(
					row_index = as.integer(cell$row_index %||% cell$row %||% (as.integer(r_i) - 1L)),
					col_index = as.integer(cell$col_index %||% cell$column %||% (as.integer(c_i) - 1L)),
					confidence = as.numeric(cell$confidence %||% cell$confidence_score %||% cell$score %||% NA_real_)
				)
			})
		})

		return(dplyr::distinct(out, .data$row_index, .data$col_index, .keep_all = TRUE))
	}

	flat_cells <- raw_table$cells %||% raw_table$entries %||% list()
	if (length(flat_cells) > 0) {
		out <- purrr::imap_dfr(flat_cells, \(cell, k) {
			tibble::tibble(
				row_index = as.integer(cell$row_index %||% cell$row %||% NA_integer_),
				col_index = as.integer(cell$col_index %||% cell$column %||% NA_integer_),
				confidence = as.numeric(cell$confidence %||% cell$confidence_score %||% cell$score %||% NA_real_)
			)
		})

		return(
			out |>
				dplyr::filter(!is.na(.data$row_index), !is.na(.data$col_index)) |>
				dplyr::distinct(.data$row_index, .data$col_index, .keep_all = TRUE)
		)
	}

	empty
}

#' Merge confidence columns back into extracted tables
#'
#' Converts extracted tables to long format, joins per-cell confidence, and
#' pivots the result back to wide format with paired `.confidence` columns.
#'
#' @param table_dfs_flat A list of extracted table tibbles.
#' @param table_confidence_flat A list of per-cell confidence tibbles.
#'
#' @return A list of tibbles with appended `.confidence` columns.
#'
#' @examples
#' values <- list(tibble::tibble(
#'   .page = "page_1",
#'   .table_index = 1L,
#'   .table_title = "Example",
#'   YEAR = c("1999", "2000"),
#'   VALUE = c("12", "15")
#' ))
#' conf <- list(tibble::tibble(
#'   .page = "page_1",
#'   .table_index = 1L,
#'   row_index = c(0L, 1L, 0L, 1L),
#'   col_index = c(0L, 0L, 1L, 1L),
#'   col_name = c("YEAR", "YEAR", "VALUE", "VALUE"),
#'   value = c("1999", "2000", "12", "15"),
#'   confidence = c(0.99, 0.98, 0.95, 0.96)
#' ))
#' mistralOCR:::merge_tables_with_confidence(values, conf)
#' @keywords internal
merge_tables_with_confidence <- function(table_dfs_flat, table_confidence_flat) {
	if (length(table_dfs_flat) == 0) {
		return(table_dfs_flat)
	}

	if (length(table_confidence_flat) == 0) {
		return(table_dfs_flat)
	}

	tables_long <- purrr::map(table_dfs_flat, \(tbl) {
		page <- tbl$.page[[1]] %||% NA_character_
		table_idx <- tbl$.table_index[[1]] %||% NA_integer_
		table_title <- tbl$.table_title[[1]] %||% NA_character_

		value_tbl <- tbl |>
			dplyr::select(-dplyr::any_of(c(".page", ".table_index", ".table_title")))

		table_to_cells(value_tbl) |>
			dplyr::mutate(
				.page = page,
				.table_index = table_idx,
				.table_title = table_title,
				.before = 1
			)
	})

	confidence_long <- dplyr::bind_rows(table_confidence_flat) |>
		dplyr::select(.page, .table_index, row_index, col_index, col_name, confidence)

	combined_long <- dplyr::bind_rows(tables_long) |>
		dplyr::left_join(
			confidence_long,
			by = c(".page", ".table_index", "row_index", "col_index", "col_name")
		)

	combined_long |>
		dplyr::group_by(.page, .table_index, .table_title) |>
		tidyr::nest() |>
		dplyr::mutate(
			wide_table = purrr::pmap(
				list(.data$data, .data$.page, .data$.table_index, .data$.table_title),
				\(tbl, page, table_idx, table_title) {

					values_wide <- tbl |>
						dplyr::select(row_index, col_name, value) |>
						tidyr::pivot_wider(
							names_from = col_name,
							values_from = value,
							values_fill = NA_character_
						)

					conf_cols <- tbl |>
						dplyr::select(row_index, col_name, confidence) |>
						dplyr::distinct(row_index, col_name, .keep_all = TRUE) |>
						tidyr::pivot_wider(
							names_from = col_name,
							values_from = confidence,
							values_fill = NA_real_
						)

					conf_names <- setdiff(names(conf_cols), "row_index")
					names(conf_cols)[names(conf_cols) %in% conf_names] <-
						paste0(conf_names, ".confidence")

					dplyr::bind_cols(
						values_wide,
						conf_cols |> dplyr::select(-row_index)
					) |>
						dplyr::mutate(
							.page = page,
							.table_index = table_idx,
							.table_title = table_title,
							.before = 1
						)
				}
			)
		) |>
		dplyr::pull(wide_table)
}

#' Build per-cell table confidence by page
#'
#' Aligns OCR word-level confidence with markdown table cell spans and returns a
#' list of confidence tibbles parallel to `table_dfs_by_page`.
#'
#' @param parsed_pages List of page objects from the OCR API.
#' @param table_dfs_by_page List of extracted tables grouped by page.
#'
#' @return A named list of per-table confidence tibbles grouped by page.
#' @keywords internal
build_table_confidence_by_page <- function(parsed_pages, table_dfs_by_page) {
	page_names <- parsed_pages |>
		purrr::map_chr(\(pg) {
			idx <- pg$index %||% NA_integer_
			paste0("page_", idx)
		})

	out <- purrr::imap(parsed_pages, \(pg, i) {
		tbl_list <- table_dfs_by_page[[i]] %||% list()
		blocks <- extract_markdown_table_blocks(pg$markdown %||% "")
		word_conf <- extract_word_confidence(pg$confidence_scores %||% list())

		purrr::imap(tbl_list, \(tbl, j) {
			value_cells <- table_to_cells(tbl)

			if (j <= length(blocks) && nrow(word_conf) > 0) {
				block <- blocks[[j]]
				cell_spans <- table_block_cell_spans(block$lines, block$line_starts)

				cell_conf <- purrr::pmap_dfr(
					cell_spans,
					\(row_index, col_index, col_name, cell_start, cell_end) {
						hits <- word_conf |>
							dplyr::filter(.data$word_start <= cell_end, .data$word_end >= cell_start)

						tibble::tibble(
							row_index = as.integer(row_index),
							col_index = as.integer(col_index),
							confidence = if (nrow(hits) > 0) mean(hits$confidence, na.rm = TRUE) else NA_real_
						)
					}
				) |>
					dplyr::distinct(row_index, col_index, .keep_all = TRUE)
			} else {
				cell_conf <- tibble::tibble(
					row_index = integer(),
					col_index = integer(),
					confidence = numeric()
				)
			}

			value_cells |>
				dplyr::left_join(cell_conf, dplyr::join_by(row_index, col_index)) |>
				dplyr::mutate(
					.page = page_names[[i]],
					.table_index = j,
					.before = 1
				)
		})
	})

	names(out) <- page_names
	out
}

#' Add extracted titles to flattened tables
#'
#' Copies page-level extracted table titles into each flattened table tibble as
#' `.table_title` metadata.
#'
#' @param table_dfs_flat A flattened list of table tibbles.
#' @param table_titles_by_page A named list of titles grouped by page.
#'
#' @return A list of table tibbles with `.table_title` added.
#' @keywords internal
add_table_titles_to_flat <- function(table_dfs_flat, table_titles_by_page) {
	if (length(table_dfs_flat) == 0) {
		return(table_dfs_flat)
	}

	purrr::map(table_dfs_flat, \(tbl) {
		page_nm <- tbl$.page[[1]] %||% NA_character_
		table_idx <- tbl$.table_index[[1]] %||% NA_integer_
		titles <- table_titles_by_page[[page_nm]] %||% character()
		table_title <- if (!is.na(table_idx) && table_idx <= length(titles)) titles[[table_idx]] else NA_character_

		tbl |>
			dplyr::mutate(
				.table_title = table_title,
				.after = .table_index
			)
	})
}
