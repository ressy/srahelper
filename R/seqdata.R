#' Sequence Data Functions
#'
#' These functions help manage sequence data.  Currently there's just one public
#' function, to check for zero-sequence files.
#'
#' Notable functions:
#'
#' * \code{\link{has_zero_seqs}}: check for empty fasta/fastq/fasta.gz/fastq.gz
#'
#' @md
#'
#' @name seqdata
NULL


# has_zero_seqs -----------------------------------------------------------


#' Report files containing no sequences
#'
#' Given a character vector of file paths, produce a logical vector with TRUE
#' for files that contain no sequences and FALSE otherwise.  Supports FASTA and
#' FASTQ, as plain text, gzip-compressed, or bzip-compressed.
#'
#'
#' @param filepaths character vector of file paths to check
#'
#' @return logical vector, TRUE if file contains no sequences or FALSE otherwise
#'
#' @export
has_zero_seqs <- function(filepaths) {
  file.size(filepaths) == 0 |
    is_empty_gzfile(filepaths) |
    is_empty_bzfile(filepaths)
}

is_empty_gzfile <- function(filepaths) {
  isgz <- has_magic(filepaths, c(0x1f, 0x8b))
  # CRC and size%(2^32), so if the sum is zero, it's empty.
  isgz & sum_footer(filepaths, 8) == 0
}

is_empty_bzfile <- function(filepaths) {
  isbz <- has_magic(filepaths, c(0x42, 0x5A))
  # CRC (no size) so if the sum is zero it's probably empty.
  isbz & sum_footer(filepaths, 4) == 0
}

# sum the last N raw bytes of given files.
sum_footer <- function(filepaths, nbytes) {
  sapply(filepaths, function(fp) {
    f <- file(fp, open = "rb")
    seek(f, origin = "end", where = -nbytes)
    footer <- readBin(f, what = "raw", n = nbytes)
    footer_sum <- sum(as.integer(footer))
    close(f)
    footer_sum
  })
}

has_magic <- function(filepaths, magic) {
  sapply(filepaths, function(fp) {
    prefix <- readChar(fp, nchars = 2, useBytes = TRUE)
    # do we have exactly two bytes?
    if (! identical(nchar(prefix, type = "bytes"), 2L)) {
      return(NA)
    }
    # if so, do they match the magic bytes?
    identical(charToRaw(prefix), as.raw(magic))
  })
}


# Misc extras -------------------------------------------------------------


# Make a data frame of R1/R2 files paired by filename prefix.
tabulate_paired_files <- function(dp,
                                  pat_r1 = "*_R1.fastq.gz",
                                  pat_r2 = "*_R2.fastq.gz",
                                  pat_erase = "_R.\\.fastq\\.gz$") {
  fps_r1 <- list.files(dp, pattern = pat_r1, full.names = TRUE)
  fps_r2 <- list.files(dp, pattern = pat_r2, full.names = TRUE)
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
