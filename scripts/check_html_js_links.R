# check_html_js_links.R
# Scans ALL HTML files for <script src=...> tags and checks if referenced JS files exist

library(xml2)
library(stringr)
library(fs)

# Set the root directory for link checking
public_dir <- "public"

# Find all HTML files in public_dir and subdirectories
html_files <- dir_ls(public_dir, recurse = TRUE, regexp = "\\.html$")

missing_js_files <- list()
all_js_links_info <- list()

for (html_file_path_from_cwd in html_files) {
  doc <- read_html(html_file_path_from_cwd)
  
  html_file_rel_to_public <- str_remove(html_file_path_from_cwd, paste0("^", public_dir, "/?"))
  html_file_dir_in_public <- path_dir(html_file_rel_to_public)
  if (html_file_dir_in_public == ".") {
    html_file_dir_in_public <- "" 
  }
  
  # Check <script src="..."> tags
  nodes_script <- xml_find_all(doc, '//script[@src]')
  for (node in nodes_script) {
    original_link <- xml_attr(node, 'src')
    original_link <- str_trim(original_link)
    if (is.na(original_link) || original_link == "") next # Handle cases where src might be empty or NA
    
    # Skip external links (http, https, //) and data URIs (less common for JS, but good practice)
    if (str_starts(original_link, "http:") || 
        str_starts(original_link, "https:") || 
        str_starts(original_link, "//") || 
        str_starts(original_link, "data:")) next
        
    path_to_check_in_public <- ""
    
    if (str_starts(original_link, "/")) {
      # Absolute path within the site: remove leading slash to make it relative to public_dir
      path_to_check_in_public <- str_remove(original_link, "^/")
    } else {
      # Relative path: resolve relative to the HTML file's directory within public_dir
      path_to_check_in_public <- path(html_file_dir_in_public, original_link)
      # Normalize the path (e.g., remove ../)
      path_to_check_in_public <- path_tidy(path_to_check_in_public)
    }

    full_path_from_cwd <- path(public_dir, path_to_check_in_public)
    file_actually_exists <- file_exists(full_path_from_cwd)
    
    if (!file_actually_exists) {
        missing_js_files[[length(missing_js_files) + 1]] <- list(
          html_file = html_file_path_from_cwd,
          js_link = original_link,
          resolved_path_attempted = full_path_from_cwd,
          type = "script-src"
        )
    }
      
    all_js_links_info[[length(all_js_links_info) + 1]] <- list(
      html_file = html_file_path_from_cwd,
      js_link_original = original_link,
      resolved_path = full_path_from_cwd,
      exists = file_actually_exists
    )
  }
}

# Print all JS links detected and their status
cat("\n\nALL JAVASCRIPT LINKS DETECTED AND THEIR STATUS:\n")
cat(sprintf("Total unique JS links checked: %d\n", length(all_js_links_info)))
if (length(all_js_links_info) > 0) {
  for (link_info in all_js_links_info) {
    status <- if (link_info$exists) "EXISTS" else "MISSING"
    cat(sprintf("[%s] In HTML: %s\n  Link: %s\n  Checked Path: %s\n\n", 
                status, link_info$html_file, link_info$js_link_original, link_info$resolved_path))
  }
}

# Print summary of missing JS files
if (length(missing_js_files) == 0) {
  cat("\nAll linked JavaScript files (<script src>) in HTML files exist!\n")
} else {
  cat("\nMissing JavaScript files found in HTML files (<script src>):\n")
  for (miss in missing_js_files) {
    cat(sprintf("HTML: %s\n  Type: %s\n  JS Link: %s\n  Resolved Path Attempted: %s\n\n",
                miss$html_file, miss$type, miss$js_link, miss$resolved_path_attempted))
  }
  cat(sprintf("Total missing JS files: %d\n", length(missing_js_files)))
  # Consider exiting with an error code if in CI environment
  # quit(status = 1)
} 