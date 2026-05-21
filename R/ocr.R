#' Run Mistral OCR on a PDF and return parsed tables with confidence
#'
#' Submits a local PDF or remote PDF URL to the Mistral OCR API, optionally
#' falls back to pagewise requests when multi-page word confidence is missing,
#' parses markdown tables, extracts titles, and merges per-cell confidence back
#' into each extracted table.
#'
#' @param pdf Path to a local PDF file or a `http(s)` PDF URL.
#' @param api_key Mistral API key. Defaults to `Sys.getenv("MISTRAL_API_KEY")`.
#' @param model OCR model name.
#' @param confidence_scores_granularity Confidence granularity requested from the
#'   API. Use `"word"` for per-word confidence.
#' @param pagewise_confidence_fallback Whether to retry page-by-page when a
#'   multi-page OCR call returns missing page confidence.
#' @param timeout_sec Request timeout in seconds.
#' @param pages Optional integer vector of 1-based page numbers to process.
#'   When `NULL` (default) all pages are processed. For example, `pages = 16:19`
#'   processes pages 16 through 19.
#' @param extract_header Logical. When `TRUE`, a `header` column is added to
#'   the returned `pages` tibble containing any text that appears before the
#'   first table on each page. `NA` for pages with no tables.
#' @param extract_footer Logical. When `TRUE`, a `footer` column is added to
#'   the returned `pages` tibble containing any text that appears after the
#'   last table on each page. `NA` for pages with no tables.
#' @param prompt_json Optional JSON-like guidance (as an R list or JSON string)
#'   describing expected table contents. When provided, this value is
#'   serialized into `document_annotation_prompt`, and a minimal
#'   `document_annotation_format` JSON schema is added so the OCR endpoint
#'   accepts the request.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{pages}{Tibble of page-level metadata.}
#'     \item{table_titles}{Tibble summarising extracted table titles.}
#'     \item{table_dfs_by_page}{Extracted tables grouped by page (list of lists).}
#'     \item{table_dfs}{Flattened list of extracted tables in wide format with
#'       paired `.confidence` columns.}
#'     \item{model}{Model name returned by the API.}
#'     \item{usage_info}{Usage info returned by the API.}
#'     \item{raw}{Raw parsed API response.}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' res <- mistral_ocr_pdf(
#'   "path/to/document.pdf",
#'   confidence_scores_granularity = "word",
#'   pagewise_confidence_fallback = TRUE
#' )
#'
#' res$pages
#' res$table_titles
#' res$table_dfs[[1]]
#' }
mistral_ocr_pdf <- function(pdf,
														api_key = Sys.getenv("MISTRAL_API_KEY"),
														model = "mistral-ocr-latest",
														confidence_scores_granularity = "word",
														pagewise_confidence_fallback = TRUE,
														timeout_sec = 300,
														pages = NULL,
														extract_header = FALSE,
														extract_footer = FALSE,
														prompt_json = NULL) {
	if (!nzchar(api_key)) {
		stop("MISTRAL_API_KEY is not set. Run Sys.setenv(MISTRAL_API_KEY = '...') first.")
	}

	is_url <- grepl("^https?://", pdf)

	if (is_url) {
		document_url <- pdf
	} else {
		if (!file.exists(pdf)) {
			stop("File does not exist: ", pdf)
		}

		b64 <- base64enc::base64encode(pdf, linewidth = NA)
		document_url <- paste0("data:application/pdf;base64,", b64)
	}

	body <- list(
		model = model,
		confidence_scores_granularity = confidence_scores_granularity,
		document = list(
			type = "document_url",
			document_url = document_url
		)
	)

	if (!is.null(pages)) {
		body$pages <- as.list(as.integer(pages) - 1L)
	}

	if (!is.null(prompt_json)) {
		prompt_text <- NULL

		if (is.character(prompt_json) && length(prompt_json) == 1L) {
			parsed_prompt <- tryCatch(jsonlite::fromJSON(prompt_json), error = function(e) NULL)

			if (is.null(parsed_prompt)) {
				stop("`prompt_json` is a character string but not valid JSON")
			}

			prompt_text <- prompt_json
		} else if (is.list(prompt_json)) {
			prompt_text <- jsonlite::toJSON(prompt_json, auto_unbox = TRUE)
		} else {
			stop("`prompt_json` must be either a JSON string or an R list")
		}

		body$document_annotation_prompt <- paste(
			"Return JSON. Use this guidance exactly:",
			prompt_text
		)
		body$document_annotation_format <- list(
			type = "json_schema",
			json_schema = list(
				name = "document_annotation",
				schema = list(
					type = "object",
					additionalProperties = TRUE
				)
			)
		)
	}

	ocr_request <- function(req_body) {
		httr2::request("https://api.mistral.ai/v1/ocr") |>
			httr2::req_headers(
				Authorization = paste("Bearer", api_key),
				`Content-Type` = "application/json"
			) |>
			httr2::req_body_json(req_body, auto_unbox = TRUE) |>
			httr2::req_timeout(timeout_sec) |>
			httr2::req_perform() |>
			httr2::resp_body_json()
	}

	parsed <- ocr_request(body)

	all_conf_null <- all(purrr::map_lgl(parsed$pages %||% list(), \(pg) is.null(pg$confidence_scores)))

	if (
		identical(confidence_scores_granularity, "word") &&
		isTRUE(pagewise_confidence_fallback) &&
		all_conf_null &&
		length(parsed$pages %||% list()) > 1
	) {
		page_idx <- purrr::map_int(parsed$pages, \(pg) as.integer(pg$index %||% NA_integer_))
		page_idx <- page_idx[!is.na(page_idx)]

		parsed_pages <- purrr::map(page_idx, \(pidx) {
			body_i <- body
			body_i$pages <- list(as.integer(pidx))
			resp_i <- ocr_request(body_i)
			resp_i$pages[[1]]
		})

		parsed$pages <- parsed_pages
	}

	if (!is.null(parsed$error)) {
		stop("Mistral OCR API error: ", parsed$error$message)
	}

	pages_tbl <- purrr::map_dfr(
		parsed$pages,
		\(pg) tibble::tibble(
			page_index = if (is.null(pg$index)) NA_integer_ else as.integer(pg$index),
			markdown   = if (is.null(pg$markdown)) NA_character_ else pg$markdown,
			tables     = list(if (is.null(pg$tables)) list() else pg$tables),
			images     = list(if (is.null(pg$images)) list() else pg$images),
			dimensions = list(if (is.null(pg$dimensions)) list() else pg$dimensions)
		)
	)

	table_dfs_by_page <- parsed$pages |>
		purrr::imap(\(pg, i) markdown_tables_to_dfs(pg$markdown %||% ""))

	table_titles_by_page <- parsed$pages |>
		purrr::map(\(pg) extract_table_titles_from_markdown(pg$markdown %||% ""))

	names(table_dfs_by_page) <- parsed$pages |>
		purrr::map_chr(\(pg) {
			idx <- pg$index %||% NA_integer_
			paste0("page_", idx)
		})

	names(table_titles_by_page) <- names(table_dfs_by_page)

	pages_tbl <- pages_tbl |>
		dplyr::mutate(
			table_titles = unname(table_titles_by_page),
			n_tables_extracted = purrr::map_int(.data$table_titles, length)
		)

	hf_by_page <- if (isTRUE(extract_header) || isTRUE(extract_footer)) {
		hf_raw <- purrr::map(parsed$pages, \(pg) extract_page_header_footer(pg$markdown %||% ""))
		names(hf_raw) <- names(table_titles_by_page)
		hf_raw
	} else {
		NULL
	}

	table_dfs_flat <- table_dfs_by_page |>
		purrr::imap(\(page_tables, page_nm) {
			purrr::imap(page_tables, \(tbl, j) {
				tbl |>
					repair_tbl_names(page_nm = page_nm, table_idx = j) |>
					dplyr::mutate(.page = page_nm, .table_index = j, .before = 1)
			})
		}) |>
		purrr::flatten()

	table_confidence_by_page <- build_table_confidence_by_page(parsed$pages, table_dfs_by_page)

	table_dfs_flat <- add_table_titles_to_flat(table_dfs_flat, table_titles_by_page)
	table_dfs_flat <- merge_tables_with_confidence(table_dfs_flat, purrr::flatten(table_confidence_by_page))

	table_titles <- purrr::imap_dfr(table_titles_by_page, \(titles, page_nm) {
		if (length(titles) == 0) {
			return(
				tibble::tibble(
					.page = character(),
					page_index = integer(),
					.table_index = integer(),
					.table_title = character()
				)
			)
		}

		tbl <- tibble::tibble(
			.page = page_nm,
			page_index = as.integer(stringr::str_remove(page_nm, "^page_")),
			.table_index = seq_along(titles),
			.table_title = titles
		)

		if (!is.null(hf_by_page)) {
			hf <- hf_by_page[[page_nm]] %||% list(header = NA_character_, footer = NA_character_)
			if (isTRUE(extract_header)) tbl <- dplyr::mutate(tbl, header = hf$header)
			if (isTRUE(extract_footer)) tbl <- dplyr::mutate(tbl, footer = hf$footer)
		}

		tbl
	}) |>
		dplyr::mutate(table_df_index = dplyr::row_number(), .after = .table_index)

	list(
		pages = dplyr::select(pages_tbl, -markdown),
		table_titles = table_titles,
		table_dfs_by_page = table_dfs_by_page,
		table_dfs = table_dfs_flat,
		model = parsed$model %||% model,
		usage_info = parsed$usage_info %||% list(),
		raw = parsed
	)
}
