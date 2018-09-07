#' SRA empty field values
#'
#' These strings are listed in the SRA docs as options for data not present even
#' for required fields.  See \code{\link{fill_blanks}}.
#'
#' @export
BLANK_TYPES <- c('not collected',
                 'not applicable',
                 'missing')

#' Metadata fields with restricted values
#'
#' These are metadata field names that can only take on the values listed for
#' each name.
#' @export
FIXED_VOCABULARY <- list(
  platform = c(
    "LS454",
    "OXFORD_NANOPORE",
    "HELICOS",
    "ABI_SOLID",
    "COMPLETE_GENOMICS",
    "PACBIO_SMRT",
    "ION_TORRENT",
    "CAPILLARY",
    "ILLUMINA"
  ),
  instrument_model = list(
    "LS454" = c(
      "454 GS",
      "454 GS 20",
      "454 GS FLX",
      "454 GS FLX+",
      "454 GS FLX Titanium",
      "454 GS Junior"),
    "OXFORD_NANOPORE" = c(
      "GridION",
      "MinION"
    ),
    "HELICOS" = c(
      "Helicos HeliScope"
    ),
    "ABI_SOLID" = c(
      "AB SOLiD System",
      "AB SOLiD System 2.0",
      "AB SOLiD System 3.0",
      "AB SOLiD 4 System",
      "AB SOLiD 4hq System",
      "AB SOLiD PI System",
      "AB 5500 Genetic Analyzer",
      "AB 5500xl Genetic Analyzer",
      "AB 5500x-Wl Genetic Analyzer",
      "AB SOLiD 3 Plus System"
    ),
    "COMPLETE_GENOMICS" = c(
      "Complete Genomics"
    ),
    "PACBIO_SMRT" = c(
      "PacBio RS",
      "PacBio RS II"
    ),
    "ION_TORRENT" = c(
      "Ion Torrent PGM",
      "Ion Torrent Proton"
    ),
    "CAPILLARY" = c(
      "AB 3730xL Genetic Analyzer",
      "AB 3730 Genetic Analyzer",
      "AB 3500xL Genetic Analyzer",
      "AB 3500 Genetic Analyzer",
      "AB 3130xL Genetic Analyzer",
      "AB 3130 Genetic Analyzer",
      "AB 310 Genetic Analyzer"
    ),
    "ILLUMINA" = c(
      "Illumina Genome Analyzer",
      "Illumina Genome Analyzer II",
      "Illumina Genome Analyzer IIx",
      "Illumina HiSeq 2500",
      "Illumina HiSeq 2000",
      "Illumina HiSeq 1000",
      "Illumina MiSeq",
      "Illumina HiScanSQ",
      "NextSeq 500",
      "HiSeq X Ten",
      "HiSeq X Five",
      "Illumina HiSeq 1500",
      "Illumina HiSeq 3000",
      "Illumina HiSeq 4000",
      "NextSeq 550"
    )
  ),
  library_strategy = c(
    "WGA",
    "WGS",
    "WXS",
    "RNA-Seq",
    "miRNA-Seq",
    "WCS",
    "Synthetic-Long-Read",
    "CLONE",
    "POOLCLONE",
    "AMPLICON",
    "CLONEEND",
    "FINISHING",
    "ChIP-Seq",
    "MNase-Seq",
    "DNase-Hypersensitivity",
    "Bisulfite-Seq",
    "Tn-Seq",
    "EST",
    "FL-cDNA",
    "CTS",
    "MRE-Seq",
    "MeDIP-Seq",
    "MBD-Seq",
    "OTHER"
  ),
  library_source = c(
    "GENOMIC",
    "TRANSCRIPTOMIC",
    "METAGENOMIC",
    "METATRANSCRIPTOMIC",
    "SYNTHETIC",
    "VIRAL RNA",
    "OTHER"
  ),
  library_selection = c(
    "RANDOM",
    "PCR",
    "RANDOM PCR",
    "RT-PCR",
    "HMPR",
    "MF",
    "CF-S",
    "CF-M",
    "CF-H",
    "CF-T",
    "MDA",
    "MSLL",
    "cDNA",
    "ChIP",
    "MNase",
    "DNAse",
    "Hybrid Selection",
    "Reduced Representation",
    "Restriction Digest",
    "5-methylcytidine antibody",
    "MBD2 protein methyl-CpG binding domain",
    "CAGE",
    "RACE",
    "size fractionation",
    "Padlock probes capture method",
    "other",
    "unspecified"
  ),
  library_layout = c(
    "Single",
    "Paired"
  )
)

# https://submit.ncbi.nlm.nih.gov/api/2.0/docs/ ?

TEMPLATES = c(
  # Pathogen affecting public health
  "Pathogen.cl.1.0", # Clinical or host-associated pathogen
  "Pathogen.env.1.0", # Environmental, food or other pathogen
  "Pathogen.combined.1.0", # Combined pathogen submission

  "Microbe.1.0", # Microbe
  "Model.organism.animal.1.0", # Model organism or animal sample
  "Metagenome.environmental.1.0", # Metagenome or environmental sample
  "Invertebrate.1.0", # Invertebrate
  "Human.1.0", # Human sample
  "Plant.1.0", # Plant sample
  "Virus.1.0", # Virus sample
  # TODO Genome, metagenome or marker sequences (MIxS compliant)
  "Beta-lactamase.1.0" # Beta-lactamase
)
