context("Test BioSample Attribute functions")


# build_biosamples --------------------------------------------------------


test_that("build_biosamples_from_template makes attributes data frame", {
  sample_attrs <- setup_sra_table()
  template <- "MIGS.ba.human-associated.4.0"
  biosamples <- build_biosamples_from_template(package_name = template,
                                               sample_attrs = sample_attrs)
  # test attributes: submission, template, mandatory_fields
  check_biosamples(biosamples, template, sample_attrs)
})

test_that("build_biosamples_from_template works with display name", {
  sample_attrs <- setup_sra_table()
  template <- "MIGS.ba.human-associated.4.0"
  template_display <- paste("MIGS: cultured bacteria/archaea,",
                            "human-associated; version 4.0")
  biosamples <- build_biosamples_from_template(package_name = template_display,
                                               sample_attrs = sample_attrs)
  # test attributes: submission, template, mandatory_fields
  check_biosamples(biosamples, template, sample_attrs)
})

test_that("build_biosamples_from_template handles submission", {
  sample_attrs <- setup_sra_table()
  template <- "MIGS.ba.human-associated.4.0"
  sub <- "SUB"
  biosamples <- build_biosamples_from_template(package_name = template,
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
  biosamples <- build_biosamples_from_template(package_name = template,
                                               sample_attrs = sample_attrs,
                                               col_pairs = col_pairs)
  expect_equal(biosamples$sample_title, sample_attrs$sample_name)
  expect_equal(biosamples$sample_thing1, sample_attrs$sample_thing1)
})

test_that("build_biosamples_from_template supports constants", {
  sample_attrs <- setup_sra_table()
  template <- "MIGS.ba.human-associated.4.0"
  constants <- c(sample_const = "X")
  biosamples <- build_biosamples_from_template(package_name = template,
                                               sample_attrs = sample_attrs,
                                               constants = constants)
  expect_equal(biosamples$sample_const,
               rep(constants[[1]], nrow(biosamples)))
})

test_that("build_biosamples_from_template handles unknown template", {
  sample_attrs <- setup_sra_table()
  package_name <- "non.existent.template"
  expect_error(
    expect_warning(
      build_biosamples_from_template(package_name = package_name,
                                     sample_attrs = sample_attrs),
      paste("Template name not recognized:", package_name))
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


# field_descriptions ------------------------------------------------------


test_that("field_descriptions describes BioSample fields", {
  fields <- c("env_biome", "env_feature")
  descs_obs <- as.list(field_descriptions(fields))
  descs_exp <- list(
    env_biome = paste("Add terms that identify the major environment type(s)",
                      "where your sample was collected. Recommend subclasses",
                      "of biome [ENVO:00000428]. Multiple terms can be",
                      "separated by one or more pipes e.g.:  mangrove biome",
                      "[ENVO:01000181]|estuarine biome [ENVO:01000020]"),
    env_feature = paste("Add terms that identify environmental entities having",
                        "causal influences upon the entity at time of sampling,",
                        "multiple terms can be separated by pipes, e.g.: ",
                        "shoreline [ENVO:00000486]|intertidal zone",
                        "[ENVO:00000316]")
    )
  # There are unicode characters in here.  I'll just strip out any non-ASCII or
  # spaces.
  descs_obs[[1]] <- charToRaw(descs_obs[[1]])
  descs_obs[[2]] <- charToRaw(descs_obs[[2]])
  descs_exp <- lapply(descs_exp, charToRaw)
  squash <- function(bytes) bytes[bytes < 0x80 & bytes != 0x20]
  # close enough!
  expect_equal(lapply(descs_obs, squash), lapply(descs_exp, squash))
})

test_that("field_descriptions handles missing entries correctly", {
  # There's no description for the subject_id field
  fields <- "subject_id"
  descs_obs <- field_descriptions(fields)
  descs_exp <- c(subject_id = "")
  expect_equal(descs_obs, descs_exp)
})
