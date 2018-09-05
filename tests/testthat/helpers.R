# Just helper functions used in multiple test contexts

setup_sra_table <- function() {
  data.frame(sample_name = paste0("sample", 1:5),
             sample_thing1 = (1:5)*5,
             sample_thing2 = letters[1:5],
             stringsAsFactors = FALSE)
}
