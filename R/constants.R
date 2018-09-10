# https://submit.ncbi.nlm.nih.gov/api/2.0/docs/ ?

#' SRA empty field values
#'
#' These strings are listed in the SRA docs as options for data not present even
#' for required fields.  See \code{\link{fill_blanks}}.
#'
#' @export
BLANK_TYPES <- c('not collected',
                 'not applicable',
                 'missing')

HTTP_SRV = c(
    SUBMIT = "submit.ncbi.nlm.nih.gov",
    WWW    =  "www.ncbi.nlm.nih.gov"
  )


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

# TODO subsume all TEMPLATE stuff into BIOSAMPLE_PACKAGES instead.
TEMPLATES = c(
  "Pathogen: clinical or host-associated; version 1.0" = "Pathogen.cl.1.0",
  "Pathogen: environmental/food/other; version 1.0" = "Pathogen.env.1.0",
  "Pathogen: combined; version 1.0" = "Pathogen.combined.1.0",
  "Microbe; version 1.0" = "Microbe.1.0",
  "Model organism or animal; version 1.0" = "Model.organism.animal.1.0",
  "Metagenome or environmental; version 1.0" = "Metagenome.environmental.1.0",
  "Invertebrate; version 1.0" = "Invertebrate.1.0",
  "Human; version 1.0" = "Human.1.0",
  "Plant; version 1.0" = "Plant.1.0",
  "Virus; version 1.0" = "Virus.1.0",
  # TODO Genome, metagenome or marker sequences (MIxS compliant)
  "MIMS: metagenome/environmental, air; version 4.0" = "MIMS.me.air.4.0",
  "MIMS: metagenome/environmental, built; version 4.0" = "MIMS.me.built.4.0",
  "MIMS: metagenome/environmental, host-associated; version 4.0" = "MIMS.me.host-associated.4.0",
  "MIMS: metagenome/environmental, human-associated; version 4.0" = "MIMS.me.human-associated.4.0",
  "MIMS: metagenome/environmental, human-gut; version 4.0" = "MIMS.me.human-gut.4.0",
  "MIMS: metagenome/environmental, human-oral; version 4.0" = "MIMS.me.human-oral.4.0",
  "MIMS: metagenome/environmental, human-skin; version 4.0" = "MIMS.me.human-skin.4.0",
  "MIMS: metagenome/environmental, human-vaginal; version 4.0" = "MIMS.me.human-vaginal.4.0",
  "MIMS: metagenome/environmental, microbial; version 4.0" = "MIMS.me.microbial.4.0",
  "MIMS: metagenome/environmental, miscellaneous; version 4.0" = "MIMS.me.miscellaneous.4.0",
  "MIMS: metagenome/environmental, plant-associated; version 4.0" = "MIMS.me.plant-associated.4.0",
  "MIMS: metagenome/environmental, sediment; version 4.0" = "MIMS.me.sediment.4.0",
  "MIMS: metagenome/environmental, soil; version 4.0" = "MIMS.me.soil.4.0",
  "MIMS: metagenome/environmental, wastewater; version 4.0" = "MIMS.me.wastewater.4.0",
  "MIMS: metagenome/environmental, water; version 4.0" = "MIMS.me.water.4.0",

  "Beta-lactamase; version 1.0" = "Beta-lactamase.1.0"
)


#' Download BioSample Pakage Information
#'
#' Download an XML file of BioSample Package information (including template
#' names) and return a data frame.
#'
#' @return data frame of information for all available BioSample types.
#'
#' @export
download_biosample_packages <- function() {
  url_xml <- paste0("https://",
                    HTTP_SRV["WWW"],
                    "/biosample/docs/packages/?format=xml")
  x <- xml2::xml_children(xml2::read_xml(url_xml))
  columns <- c("Name", "DisplayName", "ShortName", "EnvPackage",
               "EnvPackageDisplay", "Description", "Example")
  names(columns) <- columns
  data <- lapply(columns, function(column) xml2::xml_text(xml2::xml_child(x, column)))
  data <- do.call(cbind.data.frame, c(data,list(stringsAsFactors = FALSE)))
  data
}

#' An included table of BioSample Package information.
#'
#' See \code{\link{download_biosample_packages}} to get a fresh copy.
#'
#' @export
BIOSAMPLE_PACKAGES <- download_biosample_packages()
# TODO keep local unless explicitly updating
