test_that("is_md_delim_row identifies markdown delimiters", {
  expect_true(mistralOCR:::is_md_delim_row("| --- | :---: | ---: |"))
  expect_false(mistralOCR:::is_md_delim_row("| YEAR | VALUE |"))
})

test_that("parse_md_table_block parses a basic markdown table", {
  block <- c(
    "| YEAR | VALUE |",
    "| --- | --- |",
    "| 1999 | 12 |",
    "| 2000 | 15 |"
  )

  out <- mistralOCR:::parse_md_table_block(block)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2)
  expect_named(out, c("YEAR", "VALUE"))
  expect_equal(out$YEAR, c("1999", "2000"))
  expect_equal(out$VALUE, c("12", "15"))
})

test_that("extract_table_titles_from_markdown returns nearest title line", {
  md <- paste(
    "Table 1. Yield Results",
    "",
    "| YEAR | VALUE |",
    "| --- | --- |",
    "| 1999 | 12 |",
    sep = "\n"
  )

  titles <- mistralOCR:::extract_table_titles_from_markdown(md)

  expect_length(titles, 1)
  expect_equal(titles[[1]], "Table 1. Yield Results")
})
