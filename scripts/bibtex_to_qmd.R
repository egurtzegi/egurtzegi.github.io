# bibtex_to_qmd.R
# Script to generate publications/index.qmd from references.bib
# Usage: Run in R with working directory set to project root or scripts dir

library(RefManageR)
library(stringr)

# Paths
# Check if we're in the project root or scripts directory
if (file.exists('publications/references.bib')) {
  # We're in the project root
  bibfile <- 'publications/references.bib'
  qmdfile <- 'publications/index.qmd'
} else if (file.exists('../publications/references.bib')) {
  # We're in the scripts directory
  bibfile <- '../publications/references.bib'
  qmdfile <- '../publications/index.qmd'
} else {
  # Try the original fallback path for compatibility
  bibfile <- file.path('egurtzegi-quarto-website', 'publications', 'references.bib')
  qmdfile <- file.path('egurtzegi-quarto-website', 'publications', 'index.qmd')
}

# Read BibTeX
bib <- ReadBib(bibfile)

# Helper: Convert external egurtzegi.github.io URLs to internal paths
convert_url <- function(url) {
  if (!is.null(url) && url != "") {
    # Convert egurtzegi.github.io URLs to internal /static/ paths
<<<<<<< HEAD
    url <- gsub("https://egurtzegi.github.io/papers/", "/static/papers/", url, fixed = TRUE)
=======
    url <- gsub("https://egurtzegi.github.io/papers/", "/static/files/", url, fixed = TRUE)
>>>>>>> 8baf5a6 (refactor: Update publication links in index.qmd to use internal paths)
    return(url)
  }
  return(url)
}

# Convert URLs in the BibTeX entries themselves
for (i in seq_along(bib)) {
  entry <- bib[[i]]
  # Convert URL field
  if (!is.null(entry$url)) {
    bib[[i]]$url <- convert_url(entry$url)
  }
  # Convert supplementary field
  if (!is.null(entry$supplementary)) {
    bib[[i]]$supplementary <- convert_url(entry$supplementary)
  }
  # Convert preprint field
  if (!is.null(entry$preprint)) {
    bib[[i]]$preprint <- convert_url(entry$preprint)
  }
}

# Helper: icon link if field exists
doi_icon <- function(doi) {
  if (!is.null(doi) && doi != "") paste0('[<i class="ai ai-doi ai-lg"></i>](https://doi.org/', doi, ')') else ''
}
opena_icon <- function(url) {
  if (!is.null(url) && url != "") {
    # Convert URLs before creating the icon link
    url <- convert_url(url)
    paste0('[<i class="ai ai-open-access ai-lg"></i>](', url, ')')
  } else ''
}
arxiv_icon <- function(arxiv) {
  if (!is.null(arxiv) && arxiv != "") paste0('[<i class="ai ai-arxiv ai-lg"></i>](https://arxiv.org/abs/', arxiv, ')') else ''
}
github_icon <- function(github) {
  if (!is.null(github) && github != "") paste0('[<i class="fab fa-github"></i>](', github, ')') else ''
}
osf_icon <- function(osf) {
  if (!is.null(osf) && osf != "") paste0('[<i class="ai ai-osf ai-lg"></i>](', osf, ')') else ''
}

# Group mapping (edit as needed)
type_map <- list(
  article = 'Journal Publications',
  inbook = 'Book Chapters',
  incollection = 'Book Chapters',
  inproceedings = 'Refereed Conference Publications',
  outreach = 'Outreach publications',
  misc = 'Other publications'
)

# Prepare output
header <- c(
  '---',
  'title: "Publications"',
  '---',
  '',
  'In all papers, the open-access icon <i class="ai ai-open-access ai-lg"></i> links to a freely accessible pdf. The DOI icon <i class="ai ai-doi ai-lg"></i> links to the DOI or paper webpage (whenever available) and the arXiv icon <i class="ai ai-arxiv ai-lg"></i> links to arXiv. <i class="fab fa-github"></i> links to the Github repo and <i class="ai ai-osf ai-lg"></i> to the OSF repo (code + extra materials).',
  '',
  '<hr>',
  ''
)

# Group entries with remapping for special cases
groups <- list()
for (i in seq_along(bib)) {
  entry <- bib[[i]]
  type <- tolower(entry$bibtype)
  # Special case: treat phdthesis as misc
  if (type == 'phdthesis') type <- 'misc'
  # Special case: Berichte goes to outreach
  if (!is.null(entry$journal) && grepl('Berichte', entry$journal, ignore.case=TRUE)) type <- 'outreach'
  if (is.null(groups[[type]])) groups[[type]] <- list()
  groups[[type]][[length(groups[[type]])+1]] <- entry
}

# Section order: mapped types first (in old order), then unmapped types alphabetically
all_types <- names(groups)
mapped_types <- intersect(c('article', 'inbook', 'incollection', 'inproceedings', 'outreach', 'misc'), all_types)
unmapped_types <- setdiff(all_types, mapped_types)
section_order <- c(mapped_types, sort(unmapped_types))

# Helper to prettify unmapped type names
type_label <- function(type) {
  if (!is.null(type_map[[type]])) {
    label <- type_map[[type]]
  } else {
    # Capitalize, replace underscores, add plural if appropriate
    label <- gsub('_', ' ', type)
    label <- gsub('([a-z])([A-Z])', '\1 \2', label)
    label <- tools::toTitleCase(label)
    # Special cases
    if (type == 'phdthesis') label <- 'PhD Theses'
    else if (type == 'book') label <- 'Books'
    else if (type == 'unpublished') label <- 'Unpublished Manuscripts'
    else if (type == 'mastersthesis') label <- "Master's Theses"
    else if (!grepl('s$', label)) label <- paste0(label, 's')
  }
  # Map 'Featured Publications' to 'Journal Publications'
  if (label == 'Featured Publications') label <- 'Journal Publications'
  label
}

# Build QMD content
qmd <- header
for (type in section_order) {
  entries <- groups[[type]]
  section <- type_label(type)
  if (!is.null(entries) && length(entries) > 0) {
    qmd <- c(qmd, paste0('## ', section), '')
    for (i in seq_along(entries)) {
      entry <- entries[[i]]
      year <- entry$year

      # --- AUTHOR FORMATTING (Oxford comma, ampersand) ---
      authors <- ""
      if (!is.null(entry$author)) {
        author_vec <- as.character(entry$author)
        author_vec <- trimws(author_vec)
        ander_idx <- which(tolower(author_vec) == 'ander egurtzegi')
        if (length(ander_idx) > 0) {
          # Ander is an author (any position)
          coauthors <- author_vec[-ander_idx]
          if (length(coauthors) == 0) {
            authors <- ""  # Sole author, show nothing
          } else if (length(coauthors) == 1) {
            authors <- paste0("[with ", coauthors, "] ")
          } else if (length(coauthors) == 2) {
            authors <- paste0("[with ", coauthors[1], " & ", coauthors[2], "] ")
          } else {
            authors <- paste0("[with ",
              paste(coauthors[1:(length(coauthors)-1)], collapse = ", "),
              " & ", coauthors[length(coauthors)], "] ")
          }
        } else {
          # Ander not present, use all authors
          if (length(author_vec) == 1) {
            authors <- paste0("[", author_vec, "] ")
          } else if (length(author_vec) == 2) {
            authors <- paste0("[", author_vec[1], " & ", author_vec[2], "] ")
          } else {
            authors <- paste0("[",
              paste(author_vec[1:(length(author_vec)-1)], collapse = ", "),
              " & ", author_vec[length(author_vec)], "] ")
          }
        }
      }

      # --- TITLE (optionally in quotes) ---
      title <- entry$title
      # title <- paste0("\"", title, "\"") # Uncomment for quotes

      # --- VOLUME/ISSUE ---
      volume <- entry$volume
      number <- entry$number
      pages <- entry$pages
      vol_issue <- ""
      if (!is.null(volume) && !is.null(number) && number != "") {
        vol_issue <- paste0(volume, ".", number)
      } else if (!is.null(volume)) {
        vol_issue <- volume
      }

      # --- BOOK CHAPTERS/PROCEEDINGS: Editors, Publisher, Address ---
      editors <- NULL
      if (!is.null(entry$editor)) {
        editors <- as.character(entry$editor)
        if (length(editors) == 1) {
          editors <- paste0(editors, " (ed.)")
        } else if (length(editors) == 2) {
          editors <- paste0(editors[1], " & ", editors[2], " (eds.)")
        } else if (length(editors) > 2) {
          editors <- paste0(
            paste(editors[1:(length(editors)-1)], collapse = ", "),
            " & ", editors[length(editors)], " (eds.)"
          )
        }
      }
      publisher <- entry$publisher
      address <- entry$address

      # --- Compose citation string ---
      cite <- paste0(
        if (!is.null(year)) paste0(year, " ") else "",
        if (!is.null(authors) && authors != "") paste0(authors, " ") else "",
        if (!is.null(title)) paste0(title, ". ") else "",
        if (!is.null(entry$journal)) paste0("*", entry$journal, "* ") else "",
        if (!is.null(entry$booktitle)) paste0("*", entry$booktitle, "* ") else "",
        if (!is.null(editors)) paste0("In ", editors, ". ") else "",
        if (vol_issue != "") paste0(vol_issue, ", ") else "",
        if (!is.null(pages)) paste0(pages, ". ") else "",
        if (!is.null(publisher)) paste0(publisher, ". ") else "",
        if (!is.null(address)) paste0(address, ". ") else ""
      )

      # Icons
      icons <- paste(
        doi_icon(entry$doi),
        opena_icon(entry$url),
        arxiv_icon(entry$arxiv),
        github_icon(entry$github),
        osf_icon(entry$osf)
      )
      # Clean up trailing commas/spaces
      cite <- str_replace(cite, ", \\.", ".")
      cite <- str_replace(cite, ", $", ". ")
      qmd <- c(qmd, paste0("- ", cite, icons))
    }
    qmd <- c(qmd, '')
  }
}

# Write to file
writeLines(qmd, qmdfile)
cat('Wrote', qmdfile, '\n')
