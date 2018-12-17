context("Test Library Metadata functions")


# build_metadata ----------------------------------------------------------


test_that("build_metadata makes metadata data frame", {
  sample_attrs <- setup_sra_table()
  metadata <- build_metadata(sample_attrs)
  check_metadata(metadata)
})

test_that("build_metadata handles submission", {
  sample_attrs <- setup_sra_table()
  sub <- "SUB"
  metadata <- build_metadata(sample_attrs, sub)
  expect_equal(attr(metadata, "submission"), sub)
})

test_that("build_metadata supports col_pairs", {
  # make sure it pulls explicitly-named columns as expected
  # TODO: what should happen to extra columns, like sample_thing1?  Custom
  # columns only make sense in BioSample attribute tables, right?
  sample_attrs <- setup_sra_table()
  col_pairs <- c(title = "sample_name",
                 sample_thing1 = "sample_thing1")
  metadata <- build_metadata(sample_attrs = sample_attrs,
                             col_pairs = col_pairs)
  expect_equal(metadata$title, sample_attrs$sample_name)
})

test_that("build_metadata supports constants", {
  sample_attrs <- setup_sra_table()
  constants <- c(platform = "ILLUMINA")
  metadata <- build_metadata(sample_attrs = sample_attrs,
                               constants = constants)
  expect_equal(as.character(metadata$platform),
               rep(constants[[1]], nrow(metadata)))
})


# write_metadata ----------------------------------------------------------


test_that("write_metadata writes SRA library metadata", {
  sample_attrs <- setup_sra_table()
  metadata <- build_metadata(sample_attrs, submission = "SUB")
  dp <- tempfile()
  dir.create(dp)
  setwd(dp)
  write_metadata(metadata)
  s <- attributes(metadata)$submission
  fp <- file.path(s, paste0(s, "_metadata.tsv"))
  expect_true(file.exists(fp))
})

test_that("write_metadata writes metadata, no submission set", {
  sample_attrs <- setup_sra_table()
  metadata <- build_metadata(sample_attrs)
  dp <- tempfile()
  dir.create(dp)
  setwd(dp)
  write_metadata(metadata)
  fp <- "metadata.tsv"
  expect_true(file.exists(fp))
})


test_that("write_metadata handles overwrite options", {
  # This functionality comes from write_sra_table and should be usable from here
  # too.
  sample_attrs <- setup_sra_table()
  col_pairs <- c(title = "sample_name")
  metadata <- build_metadata(sample_attrs,
                             submission = "SUB",
                             col_pairs = col_pairs)
  dp <- tempfile()
  dir.create(dp)
  setwd(dp)
  write_metadata(metadata)
  s <- attributes(metadata)$submission
  fp <- file.path(s, paste0(s, "_metadata.tsv"))
  expect_true(file.exists(fp))
  data2 <- read_sra_table(fp)
  expect_equal(data2$title[1], "sample1")
  expect_error(write_metadata(metadata),
               paste("Destination file already exists:", fp))
  data2 <- read_sra_table(fp)
  expect_equal(data2$title[1], "sample1")
  metadata$title[1] <- "newname"
  write_metadata(metadata, overwrite = TRUE)
  data2 <- read_sra_table(fp)
  expect_equal(data2$title[1], "newname")
})

