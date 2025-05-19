# check_data_background_image_links.R
# Scans ALL HTML files for data-background-image attributes and checks if referenced files exist

library(xml2)
library(stringr)
library(fs)

# Set the root directory for link checking (where HTML files are and where links are resolved from)
# The script is expected to be run from the root of the Quarto project (egurtzegi-quarto-website)
public_dir <- "public"

# Find all HTML files in public_dir and subdirectories
# html_files paths will be relative to the script's CWD, e.g., "public/index.html"
html_files <- dir_ls(public_dir, recurse = TRUE, regexp = "\\.html$")

missing_files <- list()

for (html_file_path_from_cwd in html_files) {
  # Read the HTML file using its path from the current working directory
  doc <- read_html(html_file_path_from_cwd)
  
  # Determine the directory of the current HTML file, relative to public_dir
  # e.g., if html_file_path_from_cwd is "public/foo/bar.html", html_file_dir_in_public is "foo"
  # if html_file_path_from_cwd is "public/index.html", html_file_dir_in_public is ""
  html_file_rel_to_public <- str_remove(html_file_path_from_cwd, paste0("^", public_dir, "/?"))
  html_file_dir_in_public <- path_dir(html_file_rel_to_public)
  if (html_file_dir_in_public == ".") {
    html_file_dir_in_public <- "" # Handles files directly in public_dir
  }

  process_links <- function(nodes, attr_name, link_type) {
    for (node in nodes) {
      attr_val <- xml_attr(node, attr_name)
      # Handle cases where data-background-image might have multiple comma-separated URLs
      links_in_attr <- str_split(attr_val, ",")[[1]]
      
      for (link_val in links_in_attr) {
        original_link <- str_trim(link_val)
        if (original_link == "") next
      
        # Skip external links (http, https, //) and data URIs
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
        
        # Construct the full path from CWD to check for file existence
        full_path_from_cwd <- path(public_dir, path_to_check_in_public)
        
        if (!file_exists(full_path_from_cwd)) {
          # Use <<- to assign to missing_files in the parent environment
          missing_files[[length(missing_files) + 1]] <<- list(
            html_file = html_file_path_from_cwd, # Path from CWD
            link = original_link,
            resolved_path_attempted = full_path_from_cwd, # Full path from CWD
            type = link_type
          )
        }
      }
    }
  }
  
  # 1. Check data-background-image attributes
  nodes_bg <- xml_find_all(doc, '//*[@data-background-image]')
  process_links(nodes_bg, 'data-background-image', "data-background-image")
  
  # 2. Check <img src="..."> tags
  nodes_img_src <- xml_find_all(doc, '//img[@src]')
  process_links(nodes_img_src, 'src', "img-src")

  # 3. Check <img data-src="..."> tags
  nodes_img_data_src <- xml_find_all(doc, '//img[@data-src]')
  process_links(nodes_img_data_src, 'data-src', "img-data-src")
  
  # 4. Check <source src="..."> tags (e.g., inside <picture> or <video>)
  nodes_source_src <- xml_find_all(doc, '//source[@src]')
  process_links(nodes_source_src, 'src', "source-src")

  # 5. Check <video poster="..."> tags
  nodes_video_poster <- xml_find_all(doc, '//video[@poster]')
  process_links(nodes_video_poster, 'poster', "video-poster")
}

if (length(missing_files) == 0) {
  cat("All checked image/asset files (data-background-image, img-src, img-data-src, source-src, video-poster) exist!\n")
} else {
  cat("Missing image/asset files found:\n")
  for (miss in missing_files) {
    cat(sprintf("HTML: %s\n  Type: %s\n  Link: %s\n  Resolved Path Attempted: %s\n\n",
                miss$html_file, miss$type, miss$link, miss$resolved_path_attempted))
  }
  cat(sprintf("Total missing files: %d\n", length(missing_files)))
  # Consider exiting with an error code if in CI environment
  # quit(status = 1)
} 