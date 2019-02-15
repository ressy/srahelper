context("Test sequence data functions")


# has_zero_seqs -----------------------------------------------------------


test_that("has_zero_seqs marks empty plaintext files", {
  paths <- tempfile(letters[1:2])
  cat("", file = paths[1])
  cat(">seq\nACTG\n", file = paths[2])
  bool_obs <- has_zero_seqs(paths)
  bool_exp <- c(TRUE, FALSE)
  names(bool_exp) <- paths
  expect_equal(bool_obs, bool_exp)
})

test_that("has_zero_seqs marks empty gzipped files", {
  paths <- tempfile(paste("seqdata", letters[1:2], sep = "_"))
  files <- lapply(paths, gzfile, open = "w")
  cat("", file = files[[1]])
  cat(">seq\nACTG\n", file = files[[2]])
  lapply(files, close)
  # here it should still know, even though the empty file is not zero bytes
  # anymore.
  bool_obs <- has_zero_seqs(paths)
  bool_exp <- c(TRUE, FALSE)
  names(bool_exp) <- paths
  expect_equal(bool_obs, bool_exp)
})

test_that("has_zero_seqs marks empty bzipped files", {
  paths <- tempfile(paste("seqdata", letters[1:2], sep = "_"))
  files <- lapply(paths, bzfile, open = "w")
  cat("", file = files[[1]])
  cat(">seq\nACTG\n", file = files[[2]])
  lapply(files, close)
  # here it should still know, even though the empty file is not zero bytes
  # anymore.
  bool_obs <- has_zero_seqs(paths)
  bool_exp <- c(TRUE, FALSE)
  names(bool_exp) <- paths
  expect_equal(bool_obs, bool_exp)
})

test_that("has_zero_seqs marks mix of empty files", {
  paths <- tempfile(paste("seqdata", letters[1:6], sep = "_"))
  # Two plaintext files, two gzip, two bzip.  Each pair will have one empty, one
  # not.
  files <- c(paths[1:2],
             lapply(paths[3:4], gzfile, open = "w"),
             lapply(paths[5:6], bzfile, open = "w"))
  for (obj in files[c(TRUE, FALSE)]) {
    cat("", file = obj)
  }
  for (obj in files[c(FALSE, TRUE)]) {
    cat(">seq\nACTG\n", file = obj)
  }
  lapply(files[3:6], close)
  bool_obs <- has_zero_seqs(paths)
  bool_exp <- rep(c(TRUE, FALSE), 3)
  names(bool_exp) <- paths
  expect_equal(bool_obs, bool_exp)
})
