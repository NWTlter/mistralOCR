#' Detect a markdown table delimiter row
#'
#' Returns `TRUE` when a line looks like a markdown delimiter row such as
#' `| --- | :---: | ---: |`.
#'
#' @param x A single character string.
#'
#' @return A single logical value.
#'
#' @examples
#' mistralOCR:::is_md_delim_row("| --- | :---: | ---: |")
#' mistralOCR:::is_md_delim_row("| YEAR | VALUE |")
#' @keywords internal
is_md_delim_row <- function(x) {
	row <- stringr::str_trim(x)
	row <- stringr::str_remove(row, "^\\|")
	row <- stringr::str_remove(row, "\\|$")
	parts <- stringr::str_split(row, "\\|", simplify = FALSE)[[1]] |>
		stringr::str_trim()
	parts <- parts[parts != ""]

	if (length(parts) == 0) {
		return(FALSE)
	}

	all(stringr::str_detect(parts, "^:?-{3,}:?$"))
}

#' Split one markdown table row into cells
#'
#' Trims a single markdown row and returns the cell values between pipes.
#'
#' @param x A single markdown table row.
#'
#' @return A character vector of cell values.
#'
#' @examples
#' mistralOCR:::split_md_row("| YEAR | OIL TYPE | NON-OIL TYPE |")
#' @keywords internal
split_md_row <- function(x) {
	row <- stringr::str_trim(x)
	row <- stringr::str_remove(row, "^\\|")
	row <- stringr::str_remove(row, "\\|$")

	stringr::str_split(row, "\\|", simplify = FALSE)[[1]] |>
		stringr::str_trim()
}

#' @keywords internal
collapse_wrapped_header <- function(lines) {
	header <- lines |>
		stringr::str_trim() |>
		stringr::str_c(collapse = " ") |>
		stringr::str_squish()

	if (!stringr::str_detect(header, "^\\|")) {
		header <- paste0("| ", header)
	}

	if (!stringr::str_detect(header, "\\|\\s*$")) {
		header <- paste0(header, " |")
	}

	header
}

#' Parse one markdown table block into a tibble
#'
#' Converts a vector of markdown table lines into a tibble with unique column
#' names and row-aligned values.
#'
#' @param block_lines Character vector containing a full markdown table block.
#'
#' @return A tibble.
#'
#' @examples
#' block <- c(
#'   "| YEAR | VALUE |",
#'   "| --- | --- |",
#'   "| 1999 | 12 |",
#'   "| 2000 | 15 |"
#' )
#' mistralOCR:::parse_md_table_block(block)
#' @keywords internal
parse_md_table_block <- function(block_lines) {
	delim_idx <- which(purrr::map_lgl(block_lines, is_md_delim_row))

	if (length(delim_idx) == 0) {
		return(tibble::tibble())
	}

	delim_idx <- delim_idx[[1]]

	if (delim_idx <= 1L) {
		return(tibble::tibble())
	}

	header_line <- collapse_wrapped_header(block_lines[seq_len(delim_idx - 1L)])
	data_lines <- if (delim_idx < length(block_lines)) block_lines[(delim_idx + 1L):length(block_lines)] else character()
	body_lines <- c(header_line, data_lines)
	rows <- purrr::map(body_lines, split_md_row)

	if (length(rows) == 0) {
		return(tibble::tibble())
	}

	n_cols <- max(lengths(rows))

	rows <- purrr::map(
		rows,
		\(r) {
			if (length(r) < n_cols) {
				c(r, rep(NA_character_, n_cols - length(r)))
			} else {
				r
			}
		}
	)

	header <- rows[[1]]
	header <- dplyr::if_else(is.na(header) | header == "", paste0("col_", seq_along(header)), header)
	header <- as.character(header)
	header <- make.unique(header)

	data_rows <- rows[-1]

	if (length(data_rows) == 0) {
		empty_list <- rep(list(character()), n_cols)
		names(empty_list) <- header
		return(tibble::as_tibble(empty_list))
	}

	mat <- do.call(rbind, data_rows)
	tbl <- tibble::as_tibble(as.data.frame(mat, stringsAsFactors = FALSE), .name_repair = "minimal")
	names(tbl) <- header
	tbl
}

#' Extract markdown tables from OCR text
#'
#' Finds markdown table blocks in OCR markdown and returns a list of parsed
#' tibbles, one tibble per table.
#'
#' @param markdown_text OCR markdown text for a page.
#'
#' @return A named or unnamed list of tibbles.
#'
#' @examples
#' md <- paste(
#'   "Some text",
#'   "| YEAR | VALUE |",
#'   "| --- | --- |",
#'   "| 1999 | 12 |",
#'   sep = "\n"
#' )
#' mistralOCR:::markdown_tables_to_dfs(md)
#' @keywords internal
markdown_tables_to_dfs <- function(markdown_text) {
	if (is.null(markdown_text) || !nzchar(markdown_text)) {
		return(list())
	}

	extract_markdown_table_blocks(markdown_text) |>
		purrr::map(\(b) parse_md_table_block(b$lines))
}

#' Locate markdown table blocks and line offsets
#'
#' Returns metadata for each markdown table block, including the original lines,
#' line start offsets, and start/end line numbers.
#'
#' @param markdown_text OCR markdown text for a page.
#'
#' @return A list of block descriptors.
#' @keywords internal
extract_markdown_table_blocks <- function(markdown_text) {
	if (is.null(markdown_text) || !nzchar(markdown_text)) {
		return(list())
	}

	lines <- stringr::str_split(markdown_text, "\\n", simplify = FALSE)[[1]]
	line_starts <- cumsum(c(1L, utils::head(nchar(lines) + 1L, -1L)))

	delim_lines <- which(purrr::map_lgl(lines, is_md_delim_row))

	if (length(delim_lines) == 0) {
		return(list())
	}

	blocks <- purrr::map(delim_lines, \(d) {
		start_line <- d - 1L
		while (start_line >= 1L && nzchar(stringr::str_trim(lines[[start_line]]))) {
			start_line <- start_line - 1L
		}
		start_line <- start_line + 1L

		end_line <- d + 1L
		while (end_line <= length(lines) && stringr::str_detect(lines[[end_line]], "^\\s*\\|")) {
			end_line <- end_line + 1L
		}
		end_line <- end_line - 1L

		if (start_line > d - 1L || end_line <= d) {
			return(NULL)
		}

		list(
			lines = lines[start_line:end_line],
			line_starts = line_starts[start_line:end_line],
			start_line = start_line,
			end_line = end_line,
			delim_idx = d - start_line + 1L
		)
	})

	blocks |>
		purrr::compact() |>
		purrr::keep(\(b) !is.na(b$delim_idx) && b$delim_idx > 1 && b$delim_idx < length(b$lines))
}

#' Extract table titles from markdown text
#'
#' Uses the nearest non-empty line above each detected markdown table as the
#' table title, removing markdown heading prefixes if present.
#'
#' @param markdown_text OCR markdown text for a page.
#'
#' @return A character vector of table titles, one per detected table.
#'
#' @examples
#' md <- paste(
#'   "Table 1. Example title",
#'   "",
#'   "| YEAR | VALUE |",
#'   "| --- | --- |",
#'   "| 1999 | 12 |",
#'   sep = "\n"
#' )
#' mistralOCR:::extract_table_titles_from_markdown(md)
#' @keywords internal
extract_table_titles_from_markdown <- function(markdown_text) {
	blocks <- extract_markdown_table_blocks(markdown_text)

	if (length(blocks) == 0) {
		return(character())
	}

	lines <- stringr::str_split(markdown_text %||% "", "\\n", simplify = FALSE)[[1]]

	purrr::map_chr(blocks, \(block) {
		if (block$start_line <= 1) {
			return(NA_character_)
		}

		idx <- block$start_line - 1L

		while (idx >= 1L && !nzchar(stringr::str_trim(lines[[idx]]))) {
			idx <- idx - 1L
		}

		if (idx < 1L) {
			return(NA_character_)
		}

		title <- stringr::str_trim(lines[[idx]])
		title <- stringr::str_remove(title, "^#{1,6}\\s*")
		title <- dplyr::na_if(title, "")

		title
	})
}

#' Parse cell text and character positions from a markdown row
#'
#' Computes 1-based character spans for each cell within a single markdown row.
#'
#' @param line A single markdown table row.
#'
#' @return A tibble with `col_index`, `text`, `cell_start`, and `cell_end`.
#' @keywords internal
parse_line_cells_with_pos <- function(line) {
	bar_pos <- gregexpr("\\|", line)[[1]]
	if (length(bar_pos) < 2 || bar_pos[[1]] < 0) {
		return(
			tibble::tibble(
				col_index = integer(),
				text = character(),
				cell_start = integer(),
				cell_end = integer()
			)
		)
	}

	purrr::map_dfr(seq_len(length(bar_pos) - 1L), \(j) {
		left <- bar_pos[[j]] + 1L
		right <- bar_pos[[j + 1L]] - 1L

		if (right < left) {
			return(
				tibble::tibble(
					col_index = j - 1L,
					text = "",
					cell_start = left,
					cell_end = left - 1L
				)
			)
		}

		raw <- substr(line, left, right)
		trim_left <- nchar(raw) - nchar(sub("^\\s+", "", raw))
		trim_right <- nchar(raw) - nchar(sub("\\s+$", "", raw))
		start <- left + trim_left
		end <- right - trim_right

		if (start > end) {
			start <- left
			end <- left - 1L
		}

		tibble::tibble(
			col_index = j - 1L,
			text = trimws(raw),
			cell_start = start,
			cell_end = end
		)
	})
}

#' Compute absolute character spans for cells in a markdown table block
#'
#' Converts relative row positions to absolute character offsets so OCR word
#' offsets can be aligned back to table cells.
#'
#' @param block_lines Character vector of table block lines.
#' @param block_line_starts Integer vector of absolute 1-based line starts.
#'
#' @return A tibble with row, column, column name, and cell spans.
#' @keywords internal
table_block_cell_spans <- function(block_lines, block_line_starts) {
	delim_idx <- which(purrr::map_lgl(block_lines, is_md_delim_row))

	if (length(delim_idx) == 0) {
		return(
			tibble::tibble(
				row_index = integer(),
				col_index = integer(),
				col_name = character(),
				cell_start = integer(),
				cell_end = integer()
			)
		)
	}

	delim_idx <- delim_idx[[1]]

	if (delim_idx <= 1L || delim_idx >= length(block_lines)) {
		return(
			tibble::tibble(
				row_index = integer(),
				col_index = integer(),
				col_name = character(),
				cell_start = integer(),
				cell_end = integer()
			)
		)
	}

	header_line <- collapse_wrapped_header(block_lines[seq_len(delim_idx - 1L)])
	data_lines <- block_lines[(delim_idx + 1L):length(block_lines)]
	data_starts <- block_line_starts[(delim_idx + 1L):length(block_line_starts)]

	if (length(data_lines) == 0) {
		return(
			tibble::tibble(
				row_index = integer(),
				col_index = integer(),
				col_name = character(),
				cell_start = integer(),
				cell_end = integer()
			)
		)
	}

	header_cells <- parse_line_cells_with_pos(header_line) |>
		dplyr::transmute(col_index, col_name = dplyr::if_else(text == "", paste0("col_", col_index + 1L), text))

	out <- purrr::imap_dfr(data_lines, \(ln, r_i) {
		line_cells <- parse_line_cells_with_pos(ln)
		if (nrow(line_cells) == 0) {
			return(
				tibble::tibble(
					row_index = integer(),
					col_index = integer(),
					col_name = character(),
					cell_start = integer(),
					cell_end = integer()
				)
			)
		}

		line_cells |>
			dplyr::left_join(header_cells, dplyr::join_by(col_index)) |>
			dplyr::mutate(
				row_index = as.integer(r_i - 1L),
				cell_start = as.integer(data_starts[[r_i]] + .data$cell_start - 1L),
				cell_end = as.integer(data_starts[[r_i]] + .data$cell_end - 1L)
			) |>
			dplyr::select(row_index, col_index, col_name, cell_start, cell_end)
	})

	dplyr::arrange(out, row_index, col_index)
}

#' Convert a wide table to long cell format
#'
#' Produces one row per cell with row index, column index, column name, and
#' value. This helper is used when joining confidence back to extracted tables.
#'
#' @param tbl A tibble representing one extracted table.
#'
#' @return A tibble with one row per cell.
#'
#' @examples
#' tbl <- tibble::tibble(YEAR = c("1999", "2000"), VALUE = c("12", "15"))
#' mistralOCR:::table_to_cells(tbl)
#' @keywords internal
table_to_cells <- function(tbl) {
	if (nrow(tbl) == 0 || ncol(tbl) == 0) {
		return(
			tibble::tibble(
				row_index = integer(),
				col_index = integer(),
				col_name = character(),
				value = character()
			)
		)
	}

	col_names <- names(tbl)

	purrr::imap_dfr(tbl, \(col_values, col_nm) {
		col_pos <- match(col_nm, col_names) - 1L

		tibble::tibble(
			row_index = seq_len(length(col_values)) - 1L,
			col_index = col_pos,
			col_name = col_nm,
			value = as.character(col_values)
		)
	})
}

#' Extract header and footer text from a page's markdown
#'
#' Returns text occurring before the first table and after the last table on a
#' page. Both values are `NA` when no tables are present.
#'
#' @param markdown_text OCR markdown text for a single page.
#'
#' @return A named list with elements `header` and `footer` (character scalars).
#' @keywords internal
extract_page_header_footer <- function(markdown_text) {
	if (is.null(markdown_text) || !nzchar(markdown_text)) {
		return(list(header = NA_character_, footer = NA_character_))
	}

	lines <- stringr::str_split(markdown_text, "\\n", simplify = FALSE)[[1]]
	blocks <- extract_markdown_table_blocks(markdown_text)

	if (length(blocks) == 0L) {
		return(list(header = NA_character_, footer = NA_character_))
	}

	first_start <- blocks[[1L]]$start_line
	last_end   <- blocks[[length(blocks)]]$end_line

	header_text <- if (first_start > 1L) {
		stringr::str_trim(paste(lines[seq_len(first_start - 1L)], collapse = "\n"))
	} else {
		""
	}

	footer_text <- if (last_end < length(lines)) {
		stringr::str_trim(paste(lines[(last_end + 1L):length(lines)], collapse = "\n"))
	} else {
		""
	}

	list(
		header = if (nzchar(header_text)) header_text else NA_character_,
		footer = if (nzchar(footer_text)) footer_text else NA_character_
	)
}
