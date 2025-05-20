# check_html_image_links.R
# Scans ALL HTML files for <img src=...> tags and checks if referenced files exist

library(xml2)
library(stringr)
library(fs)

# Set the root directory for link checking
public_dir <- "public"

# Find all HTML files in public_dir and subdirectories
html_files <- dir_ls(public_dir, recurse = TRUE, regexp = "\\.html$")

missing_images <- list()

for (html_file_path_from_cwd in html_files) {
  doc <- read_html(html_file_path_from_cwd)
  
  html_file_rel_to_public <- str_remove(html_file_path_from_cwd, paste0("^", public_dir, "/?"))
  html_file_dir_in_public <- path_dir(html_file_rel_to_public)
  if (html_file_dir_in_public == ".") {
    html_file_dir_in_public <- "" 
  }
  
  # Check <img src="..."> tags
  nodes_img <- xml_find_all(doc, '//img[@src]')
  for (node in nodes_img) {
    original_link <- xml_attr(node, 'src')
    original_link <- str_trim(original_link)
    if (original_link == "") next
    
    if (str_starts(original_link, "http:") || 
        str_starts(original_link, "https:") || 
        str_starts(original_link, "//") || 
        str_starts(original_link, "data:")) next
        
    path_to_check_in_public <- ""
    
    if (str_starts(original_link, "/")) {
      path_to_check_in_public <- str_remove(original_link, "^/")
    } else {
      path_to_check_in_public <- path(html_file_dir_in_public, original_link)
    }

    path_to_check_in_public <- path_tidy(path_to_check_in_public)
    full_path_from_cwd <- path(public_dir, path_to_check_in_public)
    
    if (!file_exists(full_path_from_cwd)) {
        missing_images[[length(missing_images) + 1]] <- list(
        html_file = html_file_path_from_cwd,
        image_link = original_link,
        resolved_path_attempted = full_path_from_cwd,
          type = "img-src"
        )
    }
  }
}

if (length(missing_images) == 0) {
  cat("All linked image files (<img src>) in HTML files exist!\n")
} else {
  cat("Missing image files found in HTML files (<img src>):\n")
  for (miss in missing_images) {
    cat(sprintf("HTML: %s\n  Type: %s\n  Image Link: %s\n  Resolved Path Attempted: %s\n\n",
                miss$html_file, miss$type, miss$image_link, miss$resolved_path_attempted))
  }
  cat(sprintf("Total missing images: %d\n", length(missing_images)))
  # quit(status = 1)
} 
