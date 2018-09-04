
# Make a data frame of R1/R2 files paired by filename prefix.
tabulate_paired_files <- function(dp,
                                  pat_r1 = "*_R1.fastq.gz",
                                  pat_r2 = "*_R2.fastq.gz",
                                  pat_erase = "_R.\\.fastq\\.gz$") {
  fps_r1 <- list.files(dp, pattern=pat_r1, full.names = TRUE)
  fps_r2 <- list.files(dp, pattern=pat_r2, full.names = TRUE)
  ids_r1 <- gsub(pat_erase, "", basename(fps_r1))
  ids_r2 <- gsub(pat_erase, "", basename(fps_r2))
  ids <- unique(gsub(pat_erase, "", list.files(dp)))
  data.frame(
    ID = ids,
    R1 = fps_r1[match(ids, ids_r1)],
    R2 = fps_r2[match(ids, ids_r2)],
    stringsAsFactors = FALSE)
}


# Return the number of sequences in the FASTQ at the given path.  If anything
# goes wrong return -1.
read_num_seqs <- function(fp) {
  cat(fp, end = "\n", file = 2)
  tryCatch(length(dnar::read.fastq(fp)$seq), error = function(e) -1)
}

# return the number of sequences for each sample listed in the given metadata.
# Assumes FASTQ.  If paired-end, a 2xN matrix is returned.  If not, a vector.
tabulate_num_seqs <- function(dataset) {
  cidx <- match(c("filename", "filename2"), colnames(dataset))
  cidx <- cidx[! is.na(cidx)]
  if (length(cidx == 2)) {
    t(apply(dataset[, cidx], 1, function(pair) {
      r1 <- read_num_seqs(pair[[1]])
      r2 <- read_num_seqs(pair[[2]])
      c(r1, r2)
    }))
  } else if (length(cidx) == 1) {
    sapply(dataset[[cidx]], read_num_seqs)
  }
}

# for each sample in the given metadata, verify read files are not empty and, if
# two are given, are paired correctly
# check_data_files <- function(dataset) {
#   cidx <- match(c("filename", "filename2"), colnames(dataset))
#   cidx <- cidx[! is.na(cidx)]
#   if (length(cidx == 2)) {
#     unname(apply(dataset[, cidx], 1, function(pair) {
#       r1 <- read_num_seqs(pair[[1]])
#       r2 <- read_num_seqs(pair[[2]])
#       r1 > 0 & r1 == r2
#     }))
#   } else if (length(cidx) == 1) {
#     sapply(dataset[[cidx]], function(fp) read_num_seqs(fp) > 0)
#   }
# }
