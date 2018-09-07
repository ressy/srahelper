# Just helper functions used in multiple test contexts

setup_sra_table <- function() {
  data.frame(sample_name = paste0("sample", 1:5),
             sample_thing1 = (1:5)*5,
             sample_thing2 = letters[1:5],
             stringsAsFactors = FALSE)
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
