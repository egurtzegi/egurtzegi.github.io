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
html_files <- list.files(public_dir, recursive = TRUE, pattern = "\\.html$", full.names = TRUE)

missing_files <- list()
cat(sprintf("Starting link check. Found %d HTML files to process in '%s'.\\n", length(html_files), public_dir))

for (html_file_path_from_cwd in html_files) {
  cat(sprintf("\\nProcessing HTML file: %s\\n", html_file_path_from_cwd))
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

  # Helper function to process individual links and add to missing_files list
  check_and_report <- function(original_link, link_type) {
    if (str_trim(original_link) == "") return()
    
    cat(sprintf("  Found %s link: '%s'\\n", link_type, original_link))
    
    # Skip external links (http, https, //) and data URIs
    if (str_starts(original_link, "http:") || 
        str_starts(original_link, "https:") || 
        str_starts(original_link, "//") || 
        str_starts(original_link, "data:")) return()
    
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
    cat(sprintf("    Resolved to check path: '%s'\\n", full_path_from_cwd))
    
    file_actually_exists <- file_exists(full_path_from_cwd)
    cat(sprintf("    File exists? %s\\n", file_actually_exists))

    if (!file_actually_exists) {
      cat(sprintf("    MISSING: '%s'\\n", full_path_from_cwd))
      missing_files[[length(missing_files) + 1]] <<- list(
        html_file = html_file_path_from_cwd, # Path from CWD
        link = original_link,
        resolved_path_attempted = full_path_from_cwd, # Full path from CWD
        type = link_type
      )
    }
  }

  # Process links from HTML attributes
  process_html_attributes <- function(nodes, attr_name, link_type) {
    for (node in nodes) {
      attr_val <- xml_attr(node, attr_name)
      # Handle cases where attributes might have multiple comma-separated URLs (like data-background-image)
      links_in_attr <- str_split(attr_val, ",")[[1]]
      
      for (link_val in links_in_attr) {
        check_and_report(str_trim(link_val), link_type)
      }
    }
  }
  
  # 1. Check data-background-image attributes
  nodes_bg <- xml_find_all(doc, '//*[@data-background-image]')
  process_html_attributes(nodes_bg, 'data-background-image', "data-background-image")
  
  # 2. Check <img src="..."> tags
  nodes_img_src <- xml_find_all(doc, '//img[@src]')
  process_html_attributes(nodes_img_src, 'src', "img-src")

  # 3. Check <img data-src="..."> tags
  nodes_img_data_src <- xml_find_all(doc, '//img[@data-src]')
  process_html_attributes(nodes_img_data_src, 'data-src', "img-data-src")
  
  # 4. Check <source src="..."> tags (e.g., inside <picture> or <video>)
  nodes_source_src <- xml_find_all(doc, '//source[@src]')
  process_html_attributes(nodes_source_src, 'src', "source-src")

  # 5. Check <video poster="..."> tags
  nodes_video_poster <- xml_find_all(doc, '//video[@poster]')
  process_html_attributes(nodes_video_poster, 'poster', "video-poster")

  # 6. Check Markdown image links within <textarea id="source">
  nodes_source_textarea <- xml_find_all(doc, '//textarea[@id="source"]')
  if (length(nodes_source_textarea) > 0) {
    source_content <- xml_text(nodes_source_textarea[[1]])
    
    # Regex to find Markdown image links: ![](...)
    # Captures the path inside the parentheses
    # Corrected regex and extraction to handle optional quotes around the URL
    markdown_image_matches <- str_match_all(source_content, "!\\[.*\\]\\(([^)]+)\\)")
    
    if (length(markdown_image_matches[[1]]) > 0) {
      for (i in 1:nrow(markdown_image_matches[[1]])) {
        # Extract the path, then strip potential leading/trailing quotes
        original_link_quoted <- markdown_image_matches[[1]][i, 2]
        original_link <- str_replace_all(original_link_quoted, "^\"|\"$", "") # Strip leading/trailing quotes
        original_link <- str_replace_all(original_link, "^\'|\'$", "") # Strip leading/trailing single quotes

        check_and_report(original_link, "markdown-image")
      }
    }
  }

}

if (length(missing_files) == 0) {
  cat("\nAll checked image/asset files (data-background-image, img-src, img-data-src, source-src, video-poster) exist!\n")
} else {
  cat("\n----- MISSING FILES SUMMARY -----\n")
  cat("Missing image/asset files found:\n")
  for (miss in missing_files) {
    cat(sprintf("HTML: %s\n  Type: %s\n  Link: %s\n  Resolved Path Attempted: %s\n\n",
                miss$html_file, miss$type, miss$link, miss$resolved_path_attempted))
  }
  cat(sprintf("Total missing files: %d\n", length(missing_files)))
  # Consider exiting with an error code if in CI environment
  # quit(status = 1)
} 