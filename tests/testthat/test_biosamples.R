context("Test BioSample Attribute functions")


# Helpers -----------------------------------------------------------------


setup_biosamples <- function(submission = "SUB") {
  sample_attrs <- setup_sra_table()
  biosamples <- build_biosamples_from_template("MIGS.ba.human-associated.4.0",
                                               sample_attrs = sample_attrs,
                                               submission = submission)
  biosamples <- tidy_optional_fields(biosamples)
  biosamples
}


# build_biosamples --------------------------------------------------------


test_that("build_biosamples_from_template makes attributes data frame", {
  sample_attrs <- setup_sra_table()
  template <- "MIGS.ba.human-associated.4.0"
  biosamples <- build_biosamples_from_template(template_name = template,
                                               sample_attrs = sample_attrs)
  # test attributes: submission, template_name, mandatory_fields
  expect_equal(attr(biosamples, "submission"), NULL)
  expect_equal(attr(biosamples, "template"), template)
  expect_equal(length(attr(biosamples, "mandatory_fields")), 13)
  expect_equal(length(attr(biosamples, "optional_fields")), 68)
  expect_equal(biosamples$sample_name, sample_attrs$sample_name)
})

test_that("build_biosamples_from_template handles submission", {
  sample_attrs <- setup_sra_table()
  template <- "MIGS.ba.human-associated.4.0"
  sub <- "SUB"
  biosamples <- build_biosamples_from_template(template_name = template,
                                               sample_attrs = sample_attrs,
                                               submission = sub)
  expect_equal(attr(biosamples, "submission"), sub)
})

test_that("build_biosamples_from_template supports col_pairs", {
  # make sure it pulls explicitly-named columns as expected
  # make sure that extra column names are included (so custom columns can be
  # added)
  sample_attrs <- setup_sra_table()
  template <- "MIGS.ba.human-associated.4.0"
  col_pairs <- c(sample_title = "sample_name",
                 sample_thing1 = "sample_thing1")
  biosamples <- build_biosamples_from_template(template_name = template,
                                               sample_attrs = sample_attrs,
                                               col_pairs = col_pairs)
  expect_equal(biosamples$sample_title, sample_attrs$sample_name)
  expect_equal(biosamples$sample_thing1, sample_attrs$sample_thing1)
})

test_that("build_biosamples_from_template supports constants", {
  sample_attrs <- setup_sra_table()
  template <- "MIGS.ba.human-associated.4.0"
  constants <- c(sample_const = "X")
  biosamples <- build_biosamples_from_template(template_name = template,
                                               sample_attrs = sample_attrs,
                                               constants = constants)
  expect_equal(biosamples$sample_const,
               rep(constants[[1]], nrow(biosamples)))
})

test_that("build_biosamples_from_template handles unknown template", {
  sample_attrs <- setup_sra_table()
  template_name <- "non.existent.template"
  expect_error(
    expect_warning(
      build_biosamples_from_template(template_name = template_name,
                                     sample_attrs = sample_attrs),
      paste("Template name not recognized:", template_name))
  )
})


# write_biosamples --------------------------------------------------------


test_that("write_biosamples writes SRA biosample attributes", {
  biosamples <- setup_biosamples()
  dp <- tempfile()
  dir.create(dp)
  setwd(dp)
  write_biosamples(biosamples)
  s <- attributes(biosamples)$submission
  fp <- file.path(s, paste0(s, "_biosamples.tsv"))
  expect_true(file.exists(fp))
})

test_that("write_biosamples writes attributes, no submission set", {
  biosamples <- setup_biosamples(NULL)
  dp <- tempfile()
  dir.create(dp)
  setwd(dp)
  write_biosamples(biosamples)
  fp <- "biosamples.tsv"
  expect_true(file.exists(fp))
})

test_that("write_biosamples handles overwrite options", {
  # This functionality comes from write_sra_table and should be usable from here
  # too.
  biosamples <- setup_biosamples()
  dp <- tempfile()
  dir.create(dp)
  setwd(dp)
  write_biosamples(biosamples)
  s <- attributes(biosamples)$submission
  fp <- file.path(s, paste0(s, "_biosamples.tsv"))
  expect_true(file.exists(fp))
  data2 <- read_sra_table(fp)
  expect_equal(data2$sample_name[1], "sample1")
  expect_error(write_biosamples(biosamples),
               paste("Destination file already exists:", fp))
  data2 <- read_sra_table(fp)
  expect_equal(data2$sample_name[1], "sample1")
  biosamples$sample_name[1] <- "newname"
  write_biosamples(biosamples, overwrite = TRUE)
  data2 <- read_sra_table(fp)
  expect_equal(data2$sample_name[1], "newname")
})
