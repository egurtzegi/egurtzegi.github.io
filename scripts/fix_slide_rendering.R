#!/usr/bin/env Rscript

# Script to fix slide HTML files by ensuring proper remark.js loading
# This adds the necessary script tags to render xaringan/remark.js slides

# Get all HTML slide files
get_slide_paths <- function(base_dir = "static/slides") {
  slides <- list.files(
    path = base_dir,
    pattern = "*.html$",
    recursive = TRUE,
    full.names = TRUE
  )
  return(slides)
}

# Function to fix a single slide file
fix_slide_file <- function(file_path) {
  cat("Processing: ", file_path, "\n")
  
  # Read the content
  content <- readLines(file_path, warn = FALSE)
  
  # Check if remark.js is already loaded from CDN or local path
  has_remark_cdn <- any(grepl("remarkjs\\.com/downloads/remark-latest\\.min\\.js", content))
  has_remark_local <- any(grepl("/static/libs/remark/remark-latest\\.min\\.js", content))
  has_init <- any(grepl("slideshow\\s*=\\s*remark\\.create", content))
  
  if (has_remark_cdn && has_init) {
    cat("  Already has CDN remark.js and initialization. No changes needed.\n")
    return(FALSE)
  }
  
  if (has_remark_local && has_init) {
    cat("  Already has local remark.js and initialization. No changes needed.\n")
    return(FALSE)
  }
  
  # Find the textarea closing tag and body closing tag
  textarea_close_idx <- grep("</textarea>", content)
  body_close_idx <- grep("</body>", content)
  
  if (length(textarea_close_idx) == 0 || length(body_close_idx) == 0) {
    cat("  Could not find required HTML tags. Skipping.\n")
    return(FALSE)
  }
  
  # Decide whether to use CDN (always safer)
  use_cdn <- TRUE
  
  # We'll always use the CDN for now for simplicity and reliability
  script_tags <- c(
    '<script src="https://remarkjs.com/downloads/remark-latest.min.js"></script>',
    '<script>',
    '  var slideshow = remark.create();',
    '</script>'
  )
  
  # Insert the script tags before the body close tag
  new_content <- c(
    content[1:(body_close_idx - 1)],
    script_tags,
    content[body_close_idx:length(content)]
  )
  
  # Write the updated content
  writeLines(new_content, file_path)
  cat("  Added CDN remark.js script before </body>\n")
  
  return(TRUE)
}

# Run the fix on all slide files
slide_paths <- get_slide_paths()
cat("Found", length(slide_paths), "slide HTML files to process.\n")

results <- logical(length(slide_paths))

for (i in seq_along(slide_paths)) {
  results[i] <- fix_slide_file(slide_paths[i])
}

# Summary
cat("\nFix summary:\n")
cat("Total files processed:", length(results), "\n")
cat("Files modified:", sum(results), "\n")
cat("Files already correct or skipped:", length(results) - sum(results), "\n") 