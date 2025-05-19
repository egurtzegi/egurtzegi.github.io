# check_html_pdf_links.R
# Scans ALL HTML files for <a> tags linking to .pdf files and checks if referenced PDF files exist

library(xml2)
library(stringr)
library(fs)

# Set the root directory for link checking
public_dir <- "public"

# Find all HTML files in public_dir and subdirectories
html_files <- dir_ls(public_dir, recurse = TRUE, regexp = "\\.html$")

missing_pdfs <- list()
all_pdf_links_info <- list() # Renamed for clarity

for (html_file_path_from_cwd in html_files) {
  doc <- read_html(html_file_path_from_cwd)
  
  html_file_rel_to_public <- str_remove(html_file_path_from_cwd, paste0("^", public_dir, "/?"))
  html_file_dir_in_public <- path_dir(html_file_rel_to_public)
  if (html_file_dir_in_public == ".") {
    html_file_dir_in_public <- ""
  }
  
  # Find <a> tags with href attributes containing .pdf (case-insensitive)
  # and not explicitly linking to an external site (avoids common analytics/sharing links if they contain .pdf by mistake)
  nodes_a_href <- xml_find_all(doc, '//a[@href[contains(translate(., "PDF", "pdf"), ".pdf")] and not(starts-with(@href, "http:") or starts-with(@href, "https:"))]')
  
  if (length(nodes_a_href) > 0) {
    # cat(sprintf("HTML file: %s - Found %d potential PDF links\\n", html_file_path_from_cwd, length(nodes_a_href)))
  
    for (node in nodes_a_href) {
      original_link <- xml_attr(node, 'href')
      original_link <- str_trim(original_link)
      
      if (original_link == "") next
      
      # Further skip for mailto or other schemes that might slip through
      if (str_detect(original_link, "^[a-zA-Z]+:")) {
         if (!str_starts(original_link, "http:") && !str_starts(original_link, "https:")) {
            # This catches mailto:, file:, etc. but allows http/https (already handled by XPath)
            # However, since XPath already filters http/https, this mainly catches other schemes
            # We are interested in local file paths.
            if(!grepl("^\\.?\\.?/", original_link) && !grepl("^[a-zA-Z]:", original_link)) next
         }
      }

      path_to_check_in_public <- ""
        
      if (str_starts(original_link, "/")) {
        path_to_check_in_public <- str_remove(original_link, "^/")
        } else {
        path_to_check_in_public <- path(html_file_dir_in_public, original_link)
        path_to_check_in_public <- path_tidy(path_to_check_in_public)
        }
        
      full_path_from_cwd <- path(public_dir, path_to_check_in_public)
      file_actually_exists <- file_exists(full_path_from_cwd)
      
      if (!file_actually_exists) {
          missing_pdfs[[length(missing_pdfs) + 1]] <- list(
          html_file = html_file_path_from_cwd,
          pdf_path_original = original_link,
          resolved_path_attempted = full_path_from_cwd,
            type = "a-href-pdf"
          )
        }
      
      all_pdf_links_info[[length(all_pdf_links_info) + 1]] <- list(
        html_file = html_file_path_from_cwd,
        pdf_path_original = original_link,
        resolved_path = full_path_from_cwd, # Store the path used for checking
        exists = file_actually_exists
      )
      
      # Optional: Detailed per-link reporting during processing (can be noisy)
      # status_report <- if (file_actually_exists) "FOUND" else "MISSING"
      # cat(sprintf("  %s: %s -> %s\\n", status_report, original_link, full_path_from_cwd))
    }
  }
}

# Print all PDF links detected and their status
cat("\n\nALL PDF LINKS DETECTED AND THEIR STATUS:\n")
cat(sprintf("Total unique PDF links checked: %d\n", length(all_pdf_links_info)))
if (length(all_pdf_links_info) > 0) {
  for (link_info in all_pdf_links_info) {
    status <- if (link_info$exists) "EXISTS" else "MISSING"
    cat(sprintf("[%s] In HTML: %s\n  Link: %s\n  Checked Path: %s\n\n", 
                status, link_info$html_file, link_info$pdf_path_original, link_info$resolved_path))
  }
}

# Print summary of missing PDF links
if (length(missing_pdfs) == 0) {
  cat("\nAll linked PDF files in HTML files exist!\n")
} else {
  cat("\nMissing PDF files found in HTML files (<a> href links):\n")
  for (miss in missing_pdfs) {
    cat(sprintf("HTML: %s\n  Type: %s\n  PDF Link: %s\n  Resolved Path Attempted: %s\n\n",
                miss$html_file, miss$type, miss$pdf_path_original, miss$resolved_path_attempted))
  }
  cat(sprintf("Total missing PDF files: %d\n", length(missing_pdfs)))
  # quit(status = 1)
} 