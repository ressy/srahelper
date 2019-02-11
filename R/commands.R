# Some helpers for SRA-related executables like fastq-dump or ascp.

# download fastq files for the SRR's in the given run info table.
fastq_dump <- function(run_info,
                       sample_name_col = "Library_Name",
                       fastq_dump = "fastq-dump",
                       outdir = ".",
                       dump_args=NULL) {
  if (! dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }
  for (run in run_info$Run) {
    args <- c(run, "--outdir", outdir, dump_args)
    system2(fastq_dump, args)
  }
}

# Generate a shell command for an ASCP file transfer in a list format usable
# with do.call(system2, cmd).  If you're feeling adventurous, call it that way.
# Note that evidently ascp will follow symlinks for the transfer rather than
# transfer symlinks, so you can set up a contrived directory of symlinks ahead
# of time.
# md: data frame with any number of filename, filename1, ... columns
# email: email address matching NCBI account
# suffix: SRA-specific suffix used on the upload directory, after the email
# address and underscore.  "Aspera command line upload instructions" will give
# this.
# command: path to the ascp command.
# fp_key: path to the Aspera transfer SSH private key.
# dp_dest: upload sub-directory.  by default uses current date in YYYYMMDD
# format.
ascp_make_command <- function(md,
                              email,
                              suffix,
                              command = "ascp",
                              fp_key = "~/.ssh/id_rsa-ncbi",
                              dp_dest = format(Sys.time(), "%Y%m%d")) {
  colidx <- grep("^filename", colnames(md))
  src <- unname(unlist(md[, colidx]))
  dest <- paste0("subasp@upload.ncbi.nlm.nih.gov:",
                 "uploads/", email, "_", suffix, "/", dp_dest)
  list(command = command,
       args = c("-i",
                path.expand(fp_key),
                "-QT",
                "-l100m",
                "-k1",
                "-d",
                src, dest))
}

# list files in a subdirectory on the NCBI upload server.
# Note that -k1 above means ascp will try to resume where possible when
# uploading, so you don't need to manually track progress.
ascp_list_files <- function(email, suffix,
                            command = "ssh",
                            fp_key = "~/.ssh/id_rsa-ncbi",
                            dp_dest = format(Sys.time(), "%Y%m%d")) {
  dest <- file.path("uploads",
                            paste(email, suffix, sep = "_"),
                            dp_dest)
  cmd_ls <- paste("ls", dest)
  cmd <- list(command = command,
              input = cmd_ls,
              stdout = TRUE,
              args = c("-T",
                       "-i",
                       path.expand(fp_key),
                       "subasp@upload.ncbi.nlm.nih.gov"))
  files <- do.call(system2, cmd)
  # There must be a better way to keep the command prompt out of this.
  files <- sub("^aspsh> ", "", files)
  files <- files[files != ""]
  files
}
