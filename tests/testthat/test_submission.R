context("Test submission preparation functions")

# build_submission --------------------------------------------------------


test_that("build_submission makes list of data frames", {
  sample_attrs <- setup_sra_table()
  template <- "MIGS.ba.human-associated.4.0"
  submission <- build_submission(package_name = template,
                                 biosample_attrs = sample_attrs)
  # test basic structure of submission object
  expect_equal(names(submission), c("biosamples","metadata"))
  # check individual data frames
  check_biosamples(submission[["biosamples"]], template, sample_attrs)
  check_metadata(submission[["metadata"]])
})

test_that("build_submission works with package display name", {
  sample_attrs <- setup_sra_table()
  template <- "MIGS.ba.human-associated.4.0"
  template_display <- paste("MIGS: cultured bacteria/archaea,",
                            "human-associated; version 4.0")

  submission <- build_submission(package_name = template_display,
                                 biosample_attrs = sample_attrs)
  expect_equal(names(submission), c("biosamples","metadata"))
  check_biosamples(submission[["biosamples"]], template, sample_attrs)
  check_metadata(submission[["metadata"]])
})

test_that("build_submission handles submission accession", {
  sample_attrs <- setup_sra_table()
  template <- "MIGS.ba.human-associated.4.0"
  sub_acc <- "SUB"
  submission <- build_submission(package_name = template,
                                 biosample_attrs = sample_attrs,
                                 submission_accession = sub_acc)
  # test basic structure of submission object
  expect_equal(names(submission), c("biosamples","metadata"))
  # check individual data frames
  check_biosamples(submission[["biosamples"]], template, sample_attrs, sub_acc)
  check_metadata(submission[["metadata"]], sub_acc)
})

test_that("build_submission handles col_pairs for both", {
  sample_attrs <- setup_sra_table()
  template <- "MIGS.ba.human-associated.4.0"
  biosample_col_pairs <- c(sample_title = "sample_name",
                           sample_thing1 = "sample_thing1")
  metadata_col_pairs <- c(title = "sample_name",
                          sample_thing1 = "sample_thing1")
  submission <- build_submission(package_name = template,
                                 biosample_attrs = sample_attrs,
                                 biosample_col_pairs = biosample_col_pairs,
                                 metadata_col_pairs = metadata_col_pairs)
  expect_equal(submission$biosamples$sample_title, sample_attrs$sample_name)
  expect_equal(submission$biosamples$sample_thing1, sample_attrs$sample_thing1)
  expect_equal(submission$metadata$title, sample_attrs$sample_name)
})

test_that("build_submission handles constants for both", {
  sample_attrs <- setup_sra_table()
  template <- "MIGS.ba.human-associated.4.0"
  biosample_constants <- c(sample_const = "X")
  metadata_constants <- c(platform = "ILLUMINA")

  submission <- build_submission(package_name = template,
                                 biosample_attrs = sample_attrs,
                                 biosample_constants = biosample_constants,
                                 metadata_constants = metadata_constants)

  expect_equal(submission$biosamples$sample_const,
               rep(biosample_constants[[1]], nrow(submission$biosamples)))
  expect_equal(as.character(submission$metadata$platform),
               rep(metadata_constants[[1]], nrow(submission$metadata)))
})

test_that("build_submission handles unknown template", {
  sample_attrs <- setup_sra_table()
  package_name <- "non.existent.template"
  expect_error(
    expect_warning(
      build_biosamples_from_template(package_name = package_name,
                                     biosample_attrs = sample_attrs),
      paste("Template name not recognized:", package_name))
  )
})

test_that("build_submission handles one-to-many for biosamples/metadata", {
  # Try using a case with multiple sequencer samples matching each biological
  # sample.
  template <- "MIGS.ba.human-associated.4.0"
  sample_attrs <- setup_sra_table()
  lib_attrs <- setup_multi_md(sample_attrs)
  submission <- build_submission(package_name = template,
                                 biosample_attrs = sample_attrs,
                                 metadata_attrs = lib_attrs)
  check_biosamples(submission[["biosamples"]], template, sample_attrs)
  check_metadata(submission[["metadata"]], rows=40)


})


# validate_submission -----------------------------------------------------


test_that("validate_submission handles biosample and metadata attributes", {
  # This is a basic check; tests for validate_fields go further.
  sample_attrs <- setup_sra_table()
  md_col_pairs <- c(title = "sample_name", library_ID = "sample_thing1")
  md_constants <- c(platform = "ILLUMINA")
  submission <- build_submission("MIGS.ba.human-associated.4.0",
                                 biosample_attrs = sample_attrs,
                                 metadata_col_pairs = md_col_pairs,
                                 metadata_constants = md_constants)
  expect_warning(problems <- validate_submission(submission))
  # This default has blanks for mandatory fields.  There should be a warning
  # raised and an entry in the returned vector for each one.
  biosamples <- submission$biosamples
  mf <- attributes(biosamples)$mandatory_fields
  blnks <- mf[sapply(mf, function(f) any(blank(biosamples[[f]])))]
  problems_expected_biosamples <- c(
    paste("Mandatory field is missing values:", blnks),
    "Multiple entries cannot have identical attributes (see ?check_uniqueness)"
    )
  expect_equal(names(attributes(problems$biosamples)), "fields")
  expect_equal(length(attributes(problems$biosamples)[["fields"]]), 11)
  expect_equivalent(problems$biosamples, problems_expected_biosamples)
  # This default has blanks for mandatory fields.  There should be a warning
  # raised and an entry in the returned vector for each one.
  metadata <- submission$metadata
  mf <- attributes(metadata)$mandatory_fields
  blnks <- mf[sapply(mf, function(f) any(blank(metadata[[f]])))]
  blnks <- blnks[! blnks %in% c("biosample_accession", "bioproject_accession")]
  problems_expected_metadata <- paste("Mandatory field is missing values:",
                                      blnks)
  expect_equivalent(problems$metadata, problems_expected_metadata)
})


# write_submission --------------------------------------------------------


test_that("write_submission writes both spreadsheets", {
  sample_attrs <- setup_sra_table()
  sub_acc <- "SUB"
  submission <- build_submission(package_name = "MIGS.ba.human-associated.4.0",
                                 biosample_attrs = sample_attrs,
                                 submission_accession = sub_acc)
  dp <- tempfile()
  dir.create(dp)
  setwd(dp)
  result <- write_submission(submission)
  result_expected <- list(
    biosamples = file.path(sub_acc, paste0(sub_acc, "_biosamples.tsv")),
    metadata = file.path(sub_acc, paste0(sub_acc, "_metadata.tsv")))
  # We should get the file paths we expect and those files should exist.
  expect_identical(result, result_expected)
  expect_true(all(file.exists(unlist(result))))
})

test_that("write_submission writes both spreadsheets, no submission set", {
  sample_attrs <- setup_sra_table()
  submission <- build_submission(package_name = "MIGS.ba.human-associated.4.0",
                                 biosample_attrs = sample_attrs)
  dp <- tempfile()
  dir.create(dp)
  setwd(dp)
  result <- write_submission(submission)
  result_expected <- list(
    biosamples = "./biosamples.tsv",
    metadata = "./metadata.tsv")
  # We should get the file paths we expect and those files should exist.
  expect_identical(result, result_expected)
  expect_true(all(file.exists(unlist(result))))
})

test_that("write_submission handles overwrite options", {
  sample_attrs <- setup_sra_table()
  submission <- build_submission(package_name = "MIGS.ba.human-associated.4.0",
                                 biosample_attrs = sample_attrs)
  dp <- tempfile()
  dir.create(dp)
  setwd(dp)
  result <- write_submission(submission)
  result_expected <- list(
    biosamples = "biosamples.tsv",
    metadata = "metadata.tsv")
  expect_identical(result, result_expected)
  expect_true(all(file.exists(unlist(result))))
  fp_bs <- result$biosamples
  data2 <- read_sra_table(fp_bs)
  expect_equal(data2$sample_name[1], "sample1")
  expect_error(write_submission(submission),
               paste("Destination file already exists:", fp_bs))
  submission$biosamples$sample_name[1] <- "newname"
  write_submission(submission, overwrite = TRUE)
  data2 <- read_sra_table(fp_bs)
  expect_equal(data2$sample_name[1], "newname")
})

test_that("write_submission splits for too many biosamples", {
  sample_attrs <- data.frame(sample_name = paste0("sample", 1:1020),
             sample_thing1 = (1:1020),
             sample_thing2 = letters[1:5],
             stringsAsFactors = FALSE)
  submission <- build_submission(package_name = "MIGS.ba.human-associated.4.0",
                                 biosample_attrs = sample_attrs)
  dp <- tempfile()
  dir.create(dp)
  setwd(dp)
  result <- write_submission(submission)
  result_expected <- list(
    biosamples = c("./biosamples_1.tsv", "./biosamples_2.tsv"),
    metadata = c("./metadata_1.tsv", "./metadata_2.tsv"))
  # We should get the file paths we expect and those files should exist.
  expect_identical(result, result_expected)
  expect_true(all(file.exists(unlist(result))))
  # Number of rows should match up as expected.
  bs1 <- read_sra_table(result$biosamples[[1]])
  bs2 <- read_sra_table(result$biosamples[[2]])
  md1 <- read_sra_table(result$metadata[[1]])
  md2 <- read_sra_table(result$metadata[[2]])
  expect_equal(nrow(bs1), 1000)
  expect_equal(nrow(bs2), 20)
  expect_equal(nrow(md1), 1000)
  expect_equal(nrow(md2), 20)
})

test_that("write_submission splits for too many metadata entries", {
  sample_attrs <- setup_sra_table()
  lib_attrs <- setup_multi_md(sample_attrs, N=300)
  submission <- build_submission(package_name = "MIGS.ba.human-associated.4.0",
                                 biosample_attrs = sample_attrs,
                                 metadata_attrs = lib_attrs)
  dp <- tempfile()
  dir.create(dp)
  setwd(dp)
  result <- write_submission(submission)
  result_expected <- list(
    biosamples = c("./biosamples_1.tsv", "./biosamples_2.tsv"),
    metadata = c("./metadata_1.tsv", "./metadata_2.tsv"))
  # We should get the file paths we expect and those files should exist.
  expect_identical(result, result_expected)
  expect_true(all(file.exists(unlist(result))))
  # Number of rows should match up as expected.
  # With 300 metadata rows, we should get three samples in one chunk (900 md
  # rows) and two in the other (600 rows).
  bs1 <- read_sra_table(result$biosamples[[1]])
  bs2 <- read_sra_table(result$biosamples[[2]])
  md1 <- read_sra_table(result$metadata[[1]])
  md2 <- read_sra_table(result$metadata[[2]])
  expect_equal(nrow(bs1), 3)
  expect_equal(nrow(bs2), 2)
  expect_equal(nrow(md1), 900)
  expect_equal(nrow(md2), 600)

})
