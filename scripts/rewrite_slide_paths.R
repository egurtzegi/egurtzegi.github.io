# rewrite_slide_paths.R
# Rewrites asset paths in pre-rendered HTML slide files.

library(fs)
library(stringr)

slide_html_dir <- "static/slides/larraine-osoa/"

# List all HTML files in the target directory
html_files <- dir_ls(slide_html_dir, recurse = FALSE, regexp = "\\.html$")

for (html_file_path in html_files) {
  cat(sprintf("Processing: %s\n", html_file_path))
  
  # Read the HTML file content
  html_content <- readLines(html_file_path, warn = FALSE)
  
  # --- Perform String Replacements ---
  # Use stringr::str_replace_all for multiple replacements
  
  # Replace assets/figs/logos/ with /static/img/slides/larraine-osoa/logos/
  html_content <- str_replace_all(html_content, "assets/figs/logos/", "/static/img/slides/larraine-osoa/logos/")

  # Replace assets/figs/ with /static/img/slides/larraine-osoa/
  # Make sure this runs after the logo replacement to avoid double replacement on logo paths
  html_content <- str_replace_all(html_content, "assets/figs/", "/static/img/slides/larraine-osoa/")

  # Replace *_files/figure-revealjs/ with /static/img/slides/larraine-osoa/
  # This needs to be more sophisticated to handle different _files directory names
  # Pattern: match any character sequence followed by _files/figure-revealjs/
  html_content <- str_replace_all(html_content, "[^/]+_files/figure-revealjs/", "/static/img/slides/larraine-osoa/")

  # Replace assets/audios/ with /static/audios/slides/larraine-osoa/
  html_content <- str_replace_all(html_content, "assets/audios/", "/static/audios/slides/larraine-osoa/")

  # Replace *_files/libs/ with /static/files/libs/
  # This needs to be more sophisticated to handle different _files directory names and retain the rest of the path
  # Pattern: match any character sequence followed by _files/libs/ and capture the rest of the path
  # Replacement: /static/files/libs/ followed by the captured path
  # Need to be careful not to replace things that aren't paths
  
  # Example: 2023-larraine-nasals-bcbl_files/libs/clipboard/clipboard.min.js
  # Should become: /static/files/libs/clipboard/clipboard.min.js
  # Using regex with capturing groups
  html_content <- str_replace_all(html_content, "[^/]+_files/libs/(.+)", "/static/files/libs/\\1")

  # Write the modified content back to the file
  writeLines(html_content, html_file_path)
  cat("  Paths rewritten.\n")
}

cat("Path rewriting complete.\n") 