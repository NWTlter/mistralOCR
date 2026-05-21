test_that("mistral_ocr_pdf rejects invalid JSON string prompt_json", {
  fake_pdf <- tempfile(fileext = ".pdf")
  file.create(fake_pdf)
  on.exit(unlink(fake_pdf), add = TRUE)

  expect_error(
    mistral_ocr_pdf(
      pdf = fake_pdf,
      api_key = "fake-key",
      prompt_json = "{invalid_json}"
    ),
    "`prompt_json` is a character string but not valid JSON"
  )
})

test_that("mistral_ocr_pdf rejects unsupported prompt_json type", {
  fake_pdf <- tempfile(fileext = ".pdf")
  file.create(fake_pdf)
  on.exit(unlink(fake_pdf), add = TRUE)

  expect_error(
    mistral_ocr_pdf(
      pdf = fake_pdf,
      api_key = "fake-key",
      prompt_json = 123
    ),
    "`prompt_json` must be either a JSON string or an R list"
  )
})
