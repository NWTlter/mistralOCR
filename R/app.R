#' @keywords internal
build_prompt_columns <- function(x) {
	parts <- stringr::str_split(x %||% "", ",", simplify = FALSE)[[1]] |>
		stringr::str_trim()

	parts <- parts[parts != ""]

	if (length(parts) == 0) {
		return(list())
	}

	purrr::map(parts, \(val) {
		if (toupper(val) %in% c("NA", "NULL")) {
			NULL
		} else {
			val
		}
	})
}

#' Launch the Mistral OCR Shiny app
#'
#' Opens an interactive Shiny application for uploading a PDF, running Mistral
#' OCR, previewing and downloading the extracted tables.
#'
#' @return A `shiny.appobj` (invisibly). Typically passed to
#'   [shiny::runApp()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' app <- launch_mistral_ocr_app()
#' shiny::runApp(app)
#' }
launch_mistral_ocr_app <- function() {
	default_save_dir <- path.expand("~")

	ui <- shiny::fluidPage(
		shiny::tags$head(
			shiny::tags$style(shiny::HTML(
				"@keyframes spin { from { transform: rotate(0deg); } to { transform: rotate(360deg); } }"
			))
		),
		shiny::titlePanel("Mistral OCR PDF Extractor"),
		shiny::sidebarLayout(
			shiny::sidebarPanel(
				shiny::textInput(
					"pdf_path",
					"PDF file path",
					value = "",
					placeholder = "input file path"
				),
				shinyFiles::shinyFilesButton("pdf_path_browse", "Browse file", "Select PDF file", multiple = FALSE),
				shiny::actionButton("clear_file", "Clear file path"),
				shiny::passwordInput("api_key", "Mistral API Key", value = Sys.getenv("MISTRAL_API_KEY")),
				shiny::textInput("model", "Model", value = "mistral-ocr-latest"),
				shiny::selectInput(
					"confidence_scores_granularity",
					"Confidence granularity",
					choices = c("word", "none"),
					selected = "word"
				),
				shiny::textInput("pages", "Pages (1-based, comma-separated, optional)", value = ""),
				shiny::numericInput("timeout_sec", "Timeout (seconds)", value = 300, min = 10, step = 10),
				shiny::checkboxInput("pagewise_confidence_fallback", "Pagewise confidence fallback", value = TRUE),
				shiny::checkboxInput("extract_header", "Extract header", value = FALSE),
				shiny::checkboxInput("extract_footer", "Extract footer", value = FALSE),
				shiny::tags$hr(),
				shiny::checkboxInput("use_prompt", "Use prompt_json guidance", value = FALSE),
				shiny::conditionalPanel(
					condition = "input.use_prompt",
					shiny::tags$p(
						style = "color: #6c757d; margin-bottom: 4px;",
						"Example pattern A: NA, 5, 15, 25, 35, 45, NA"
					),
					shiny::textInput(
						"prompt_cols_a",
						"Prompt columns pattern A",
						value = ""
					),
					shiny::tags$p(
						style = "color: #6c757d; margin-bottom: 4px;",
						"Example pattern B: NA, 55, 65, 75, 85, 95, NA"
					),
					shiny::textInput(
						"prompt_cols_b",
						"Prompt columns pattern B",
						value = ""
					),
					shiny::tags$p(
						style = "color: #6c757d; margin-bottom: 4px;",
						"Example guidance: Column headers should match one of the two allowed patterns. First and last columns are row labels."
					),
					shiny::textAreaInput(
						"guidance",
						"Guidance",
						value = "",
						rows = 3
					)
				),
				shiny::actionButton("run", "Run OCR", class = "btn-primary")
			),
			shiny::mainPanel(
				shiny::verbatimTextOutput("status"),
				shiny::fluidRow(
					shiny::column(6, shiny::downloadButton("download_csv", "Download table_titles CSV")),
					shiny::column(6, shiny::downloadButton("download_rds", "Download full output (.rds)"))
				),
				shiny::fluidRow(
					shiny::column(
						8,
						shiny::textInput("save_dir", "Save-all folder path", value = default_save_dir),
						shinyFiles::shinyDirButton("save_dir_browse", "Browse folder", "Select save folder")
					),
					shiny::column(
						4,
						shiny::tags$div(style = "margin-top: 25px;", shiny::actionButton("save_all", "Save all tables"))
					)
				),
				shiny::h4("Table Titles"),
				shiny::uiOutput("table_titles_ui"),
				shiny::h4("First Extracted Table"),
				shiny::tableOutput("first_table")
			)
		)
	)

	server <- function(input, output, session) {
		result <- shiny::reactiveVal(NULL)
		status_txt <- shiny::reactiveVal("Waiting for input.")
		roots <- c(home = path.expand("~"))

		shinyFiles::shinyFileChoose(
			input,
			"pdf_path_browse",
			roots = roots,
			session = session,
			filetypes = c("pdf")
		)

		shinyFiles::shinyDirChoose(input, "save_dir_browse", roots = roots, session = session)

		shiny::observeEvent(input$pdf_path_browse, {
			file_selected <- shinyFiles::parseFilePaths(roots, input$pdf_path_browse)
			if (nrow(file_selected) > 0 && nzchar(file_selected$datapath[[1]])) {
				shiny::updateTextInput(session, "pdf_path", value = file_selected$datapath[[1]])
			}
		})

		shiny::observeEvent(input$save_dir_browse, {
			dir_selected <- shinyFiles::parseDirPath(roots, input$save_dir_browse)
			if (length(dir_selected) > 0 && nzchar(dir_selected[[1]])) {
				shiny::updateTextInput(session, "save_dir", value = dir_selected[[1]])
			}
		})

		shiny::observeEvent(input$clear_file, {
			result(NULL)
			shiny::updateTextInput(session, "pdf_path", value = "")
			status_txt("File path cleared. Paste or browse a PDF path and click Run OCR.")
		})

		parse_pages <- function(x) {
			x <- stringr::str_trim(x %||% "")
			if (!nzchar(x)) {
				return(NULL)
			}

			vals <- stringr::str_split(x, ",", simplify = FALSE)[[1]] |>
				stringr::str_trim() |>
				as.integer()

			vals[!is.na(vals)]
		}

		build_prompt <- function() {
			if (!isTRUE(input$use_prompt)) {
				return(NULL)
			}

			cols_a <- build_prompt_columns(input$prompt_cols_a)
			cols_b <- build_prompt_columns(input$prompt_cols_b)

			expected <- list()
			if (length(cols_a) > 0) expected <- c(expected, list(list(columns = cols_a)))
			if (length(cols_b) > 0) expected <- c(expected, list(list(columns = cols_b)))

			if (length(expected) == 0) {
				return(NULL)
			}

			list(
				expected_tables = expected,
				guidance = input$guidance
			)
		}

		show_running_modal <- function() {
			shiny::showModal(
				shiny::modalDialog(
					title = "Running OCR",
					shiny::tags$div(
						style = "display: flex; align-items: center; gap: 10px;",
						shiny::tags$div(
							style = paste(
								"width: 2rem;",
								"height: 2rem;",
								"border: 0.25em solid #dee2e6;",
								"border-top-color: #007bff;",
								"border-radius: 50%;",
								"animation: spin 0.8s linear infinite;"
							)
						),
						shiny::tags$span("Sending file and waiting for OCR response...")
					),
					footer = NULL,
					easyClose = FALSE
				)
			)
		}

		shiny::observeEvent(input$run, {
			pdf_value <- stringr::str_trim(input$pdf_path %||% "")
			if (!nzchar(pdf_value)) {
				status_txt("Please provide a PDF file path.")
				return(NULL)
			}

			if (!file.exists(pdf_value)) {
				status_txt("PDF file path does not exist. Check the path or use Browse file.")
				return(NULL)
			}

			status_txt("Running OCR...")
			show_running_modal()
			on.exit(shiny::removeModal(), add = TRUE)

			res <- tryCatch(
				mistral_ocr_pdf(
					pdf = pdf_value,
					api_key = input$api_key,
					model = input$model,
					confidence_scores_granularity = input$confidence_scores_granularity,
					pagewise_confidence_fallback = isTRUE(input$pagewise_confidence_fallback),
					timeout_sec = as.numeric(input$timeout_sec),
					pages = parse_pages(input$pages),
					extract_header = isTRUE(input$extract_header),
					extract_footer = isTRUE(input$extract_footer),
					prompt_json = build_prompt()
				),
				error = function(e) e
			)

			if (inherits(res, "error")) {
				status_txt(paste("Error:", conditionMessage(res)))
				result(NULL)
			} else {
				result(res)
				status_txt(
					paste(
						"Done.",
						"Pages:", nrow(res$pages),
						"Tables:", length(res$table_dfs)
					)
				)
			}
		})

		shiny::observeEvent(input$save_all, {
			res <- result()
			if (is.null(res)) {
				status_txt("No OCR result available yet. Run OCR first.")
				return(NULL)
			}

			target_root <- stringr::str_trim(input$save_dir %||% "")
			if (!nzchar(target_root)) {
				status_txt("Error: provide a folder path in 'Save-all folder path' before saving.")
				shiny::showNotification("Save-all folder path is required.", type = "error")
				return(NULL)
			}

			if (!dir.exists(target_root)) {
				dir.create(target_root, recursive = TRUE, showWarnings = FALSE)
			}

			if (!dir.exists(target_root)) {
				status_txt("Could not create/access the folder path.")
				return(NULL)
			}

			out_dir <- file.path(
				target_root,
				paste0("mistral_export_", format(Sys.time(), "%Y%m%d_%H%M%S"))
			)

			dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
			if (!dir.exists(out_dir)) {
				status_txt("Could not create export subfolder.")
				return(NULL)
			}

			utils::write.csv(res$table_titles, file = file.path(out_dir, "table_titles.csv"), row.names = FALSE, na = "")

			purrr::iwalk(res$table_dfs, \(tbl, idx) {
				nm <- paste0("table_", idx, ".csv")
				utils::write.csv(tbl, file = file.path(out_dir, nm), row.names = FALSE, na = "")
			})

			status_txt(
				paste0(
					"Saved all outputs to: ", out_dir,
					" (", length(res$table_dfs), " tables + table_titles.csv)"
				)
			)
		})

		output$status <- shiny::renderText({
			status_txt()
		})

		output$table_titles_ui <- shiny::renderUI({
			res <- result()
			if (is.null(res)) {
				return(NULL)
			}

			tt <- res$table_titles
			if (is.null(tt) || nrow(tt) == 0) {
				return(shiny::tags$em("No tables extracted."))
			}

			tt_display <- tt
			tt_display$Actions <- ""

			for (i in seq_len(nrow(tt_display))) {
				idx <- tt_display$table_df_index[[i]]
				view_btn_id <- paste0("view_table_", idx)
				download_btn_id <- paste0("download_table_", idx)
				page_nm <- tt_display$.page[[i]] %||% "page"

				tt_display$Actions[[i]] <- as.character(
					shiny::tags$span(
						shiny::actionButton(view_btn_id, "View", class = "btn-sm"),
						" ",
						shiny::downloadButton(download_btn_id, "CSV", class = "btn-sm")
					)
				)

				local({
					idx_local <- idx
					view_id <- view_btn_id

					shiny::observeEvent(input[[view_id]], {
						res_now <- result()
						if (is.null(res_now) || length(res_now$table_dfs) < idx_local) {
							shiny::showNotification("Table is not available.", type = "error")
							return(NULL)
						}

						tbl_data <- res_now$table_dfs[[idx_local]]
						shiny::showModal(
							shiny::modalDialog(
								title = paste("Table", idx_local),
								shiny::tableOutput(paste0("modal_table_", idx_local)),
								easyClose = TRUE,
								size = "l"
							)
						)

						output[[paste0("modal_table_", idx_local)]] <- shiny::renderTable({
							tbl_data
						}, striped = TRUE, bordered = TRUE)
					}, ignoreInit = TRUE)
				})

				local({
					idx_download <- idx
					page_nm_download <- page_nm

					output[[download_btn_id]] <- shiny::downloadHandler(
						filename = function() {
							paste0(
								"table_",
								idx_download,
								"_",
								gsub("[^A-Za-z0-9_-]", "_", page_nm_download),
								".csv"
							)
						},
						content = function(file) {
							res_now <- result()
							if (is.null(res_now) || length(res_now$table_dfs) < idx_download) {
								stop("Requested table is not available.")
							}

							utils::write.csv(res_now$table_dfs[[idx_download]], file = file, row.names = FALSE, na = "")
						}
					)
				})
			}

			shiny::tags$table(
				class = "table table-striped table-bordered",
				shiny::tags$thead(
					shiny::tags$tr(
						lapply(names(tt_display), function(nm) shiny::tags$th(nm))
					)
				),
				shiny::tags$tbody(
					lapply(seq_len(nrow(tt_display)), function(i) {
						shiny::tags$tr(
							lapply(names(tt_display), function(nm) {
								val <- tt_display[[nm]][[i]]
								if (nm == "Actions") {
									shiny::tags$td(shiny::HTML(val))
								} else {
									shiny::tags$td(as.character(val))
								}
							})
						)
					})
				)
			)
		})

		output$first_table <- shiny::renderTable({
			res <- result()
			if (is.null(res) || length(res$table_dfs) == 0) {
				return(NULL)
			}

			res$table_dfs[[1]]
		}, striped = TRUE, bordered = TRUE)

		output$download_csv <- shiny::downloadHandler(
			filename = function() {
				paste0("mistral_table_titles_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
			},
			content = function(file) {
				res <- result()
				if (is.null(res)) {
					stop("No OCR result available yet. Run OCR first.")
				}

				utils::write.csv(res$table_titles, file = file, row.names = FALSE, na = "")
			}
		)

		output$download_rds <- shiny::downloadHandler(
			filename = function() {
				paste0("mistral_ocr_result_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
			},
			content = function(file) {
				res <- result()
				if (is.null(res)) {
					stop("No OCR result available yet. Run OCR first.")
				}

				saveRDS(res, file = file)
			}
		)
	}

	shiny::shinyApp(ui = ui, server = server)
}
