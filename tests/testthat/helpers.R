# Just helper functions used in multiple test contexts

# make a basic input data frame for biosamples and/or metadata preparation.
setup_sra_table <- function() {
  data.frame(sample_name = paste0("sample", 1:5),
             sample_thing1 = (1:5)*5,
             sample_thing2 = letters[1:5],
             stringsAsFactors = FALSE)
}

# make a multi-row-per-sample input data frame for metadata preparation, using
# an existing biosamples input data frame.
setup_multi_md <- function(sample_attrs, N=8) {
  lib_attrs <- data.frame(
    sample_name = rep(sample_attrs$sample_name, each=N),
    replicate = paste0("rep", 1:N),
    stringsAsFactors = FALSE)
  lib_attrs$title <- with(lib_attrs, paste(sample_name,
                                           replicate,
                                           sep = "."))
  lib_attrs$filename <- with(lib_attrs, paste(sample_name,
                                              replicate,
                                              "fasta",
                                              sep = "."))
  lib_attrs
}

setup_biosamples <- function(submission = "SUB") {
  sample_attrs <- setup_sra_table()
  biosamples <- build_biosamples_from_template("MIGS.ba.human-associated.4.0",
                                               sample_attrs = sample_attrs,
                                               submission = submission)
  biosamples <- tidy_optional_fields(biosamples)
  biosamples$host <- 1:nrow(biosamples)
  biosamples
}

check_biosamples <- function(biosamples, template, sample_attrs, sub_acc=NULL) {
  expect_equal(attr(biosamples, "submission"), sub_acc)
  expect_equal(attr(biosamples, "template"), template)
  expect_equal(length(attr(biosamples, "mandatory_fields")), 13)
  expect_equal(length(attr(biosamples, "optional_fields")), 68)
  expect_equal(biosamples$sample_name, sample_attrs$sample_name)
}

check_metadata <- function(metadata, sub_acc=NULL, rows=5) {
  expect_equal(attr(metadata, "submission"), sub_acc)
  expect_equal(length(attr(metadata, "mandatory_fields")), 14)
  expect_equal(nrow(metadata), rows)
}
