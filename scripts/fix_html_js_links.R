# fix_html_js_links.R
# Corrects <script src> and <link href> paths in specific HTML files
# to point from an old "_files/libs" structure to a new "/static/libs" structure.

library(stringr)
library(fs)

# Define the target directory containing the HTML files to fix
# This script is expected to be run from the root of the Quarto project (egurtzegi-quarto-website)
target_html_dir <- "static/slides/"

# Define the pattern for old links and the new base path
# Old: e.g., "2024-larraine-h-fod_files/libs/revealjs/plugin/zoom/zoom.js"
#          "some-other-prefix_files/libs/some-library/script.js"
# New: e.g., "/static/libs/revealjs/plugin/zoom/zoom.js"
#          "/static/libs/some-library/script.js"

# This regex captures the part after "_files/libs/"
# old_libs_pattern <- regex("([^\""]*?_files)/libs/(.*?[\""])") # Corrected regex syntax for R string, but variable is unused
# replacement_libs_path <- "/static/libs/\2" # Unused variable

# Also handle cases where the path might be relative like "../../_files/libs/..."
# The key is to find "_files/libs/" and replace the prefix up to that point.
# This more general regex finds any path segment ending in "_files/libs/"
# and replaces it and everything before it in that segment with "/static/libs/"
# It looks for a quote, then any characters not a quote, then the specific pattern, then the rest up to the closing quote.
# Simpler approach: directly replace the known problematic part of the string if present.

process_html_file <- function(html_file_path) {
  cat(sprintf("Processing file: %s\n", html_file_path))
  # Read the entire file content into a single string
  original_content <- paste(readLines(html_file_path, warn = FALSE), collapse = "\n")
  modified_content <- original_content
  
  # Fix <script src="..._files/libs/...">
  # Pattern: (src=")                 : captures 'src="' as \1
  #          ([^/"]*?_files/libs/) : matches 'anything_files/libs/'
  #          ([^"]*?")              : captures 'rest_of_path.js"' as \2
  # Replacement: \1/static/libs/\2
  modified_content <- str_replace_all(
    modified_content,
    pattern = '(src=")([^/"]*?_files/libs/)([^"].*?")'
    , replacement = '\\1/static/libs/\\3' 
  )
  
  # Fix <link href="..._files/libs/...">
  # Pattern: (href=")                : captures 'href="' as \1
  #          ([^/"]*?_files/libs/) : matches 'anything_files/libs/'
  #          ([^"]*?")              : captures 'rest_of_path.css"' as \2
  # Replacement: \1/static/libs/\2
  modified_content <- str_replace_all(
    modified_content,
    pattern = '(href=")([^/"]*?_files/libs/)([^"].*?")'
    , replacement = '\\1/static/libs/\\3'
  )

  # Fix paths for assets originally in "..._files/assets/figs/..."
  # Pattern: (src=")                         : captures 'src="' as \1
  #          ([^/"]*?_files/assets/figs/)   : matches 'anything_files/assets/figs/'
  #          ([^"]*?")                      : captures 'image.png"' as \2
  # Replacement: \1/static/img/slides/figs/\2
  modified_content <- str_replace_all(
    modified_content,
    pattern = '(src=")([^/"]*?_files/assets/figs/)([^"].*?")'
    , replacement = '\\1/static/img/slides/figs/\\3'
  )
  modified_content <- str_replace_all(
    modified_content,
    pattern = '(href=")([^/"]*?_files/assets/figs/)([^"].*?")' # e.g. for favicons
    , replacement = '\\1/static/img/slides/figs/\\3'
  )
  
  # Fix paths for assets originally in "assets/figs/..." (direct, no _files prefix)
  # Pattern: (src=")                 : captures 'src="' as \1
  #          (assets/figs/)          : matches 'assets/figs/'
  #          ([^"]*?")              : captures 'image.png"' as \2
  # Replacement: \1/static/img/slides/figs/\2
  modified_content <- str_replace_all(
    modified_content,
    pattern = '(src=")(assets/figs/)([^"].*?")' 
    , replacement = '\\1/static/img/slides/figs/\\3'
  )
   modified_content <- str_replace_all(
    modified_content,
    pattern = '(data-background-image=")(assets/figs/)([^"].*?")' # For revealjs backgrounds
    , replacement = '\\1/static/img/slides/figs/\\3'
  )

  # Fix paths for audio files: "assets/audios/..." to "/static/audios/slides/larraine-osoa/..."
  # Pattern: (src=")                 : captures 'src="' as \1
  #          (assets/audios/)        : matches 'assets/audios/'
  #          ([^"]*?")              : captures 'audio.mp3"' as \2
  # Replacement: \1/static/audios/slides/larraine-osoa/\2
  modified_content <- str_replace_all(
    modified_content,
    pattern = '(src=")(assets/audios/)([^"].*?")' 
    , replacement = '\\1/static/audios/slides/larraine-osoa/\\3'
  )

  # Fix paths for assets using the pattern ../../img/slides/h/... for src attributes
  # This is specific to the 'h' slides and their relative paths
  modified_content <- str_replace_all(
    modified_content,
    pattern = '(src=")\\.\\../\\.\\../img/slides/h/([^"]*?")'
    , replacement = '\\1/static/img/slides/h/\\2'
  )

  # Fix paths for assets using the pattern ../../img/slides/h/... for data-background-image attributes
  # This is specific to the 'h' slides and their relative paths
  modified_content <- str_replace_all(
    modified_content,
    pattern = '(data-background-image=")\\.\\../\\.\\../img/slides/h/([^"]*?")'
    , replacement = '\\1/static/img/slides/h/\\2'
  )

  if (original_content != modified_content) {
    cat(sprintf("  Modifications made. Writing changes to %s\n", html_file_path))
    # Write the modified content back to the file
    writeLines(modified_content, html_file_path)
  } else {
    cat("  No changes needed.\n")
  }
}

# Find all HTML files in the target directory
# Using \.html$ for regex to correctly match .html files
html_files_to_fix <- dir_ls(target_html_dir, recurse = TRUE, regexp = "\\.html$")

if (length(html_files_to_fix) == 0) {
  cat(sprintf("No HTML files found in %s\n", target_html_dir))
} else {
  # Use a standard for loop instead of purrr::walk to avoid dependency issues
  for (html_file in html_files_to_fix) {
    process_html_file(html_file)
  }
  cat("\nLink fixing process complete.\n")
  cat("Please review the changes and test the slides.\n")
  cat("Remember to commit changes to version control if satisfied.\n")
} 