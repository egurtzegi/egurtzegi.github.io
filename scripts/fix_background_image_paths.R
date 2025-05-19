# fix_background_image_paths.R
# Safely fixes data-background-image paths in HTML slides
# Edits files in place (no backup, as version control is used)

library(xml2)
library(stringr)
library(fs)

# Set the directories for slides, both static source and public output
slides_dir_static <- "static/slides/"
slides_dir_public <- "public/static/slides/"

# Process function that can be applied to any directory of HTML files
process_html_files <- function(html_files, is_public = FALSE) {
  for (html_file in html_files) {
    message("Processing: ", html_file)
    
    # Read the file content as text
    content <- readLines(html_file, warn = FALSE, encoding = "UTF-8")
    content_original <- content
    
    # Find lines with data-background-image attribute
    bg_image_lines <- which(str_detect(content, "data-background-image"))
    
    for (line_num in bg_image_lines) {
      line <- content[line_num]
      
      # 1. Fix /stat/static/img/ to /static/img/
      content[line_num] <- str_replace_all(line, "/stat/static/img/", "/static/img/")
      
      # 2. Fix incorrect relative paths with ../img/ in them
      content[line_num] <- str_replace_all(content[line_num], 
                                           "/static/img/slides/larraine-osoa/\\.\\./img/slides/larraine-osoa/", 
                                           "/static/img/slides/larraine-osoa/")
      
      # 3. Fix doubled image segments
      content[line_num] <- str_replace_all(content[line_num],
                                           "/static/slides/img/slides/img/slides/larraine-osoa/", 
                                           "/static/img/slides/larraine-osoa/")
      
      # 4. Fix /static/slides/img/slides/ pattern
      content[line_num] <- str_replace_all(content[line_num],
                                           "/static/slides/img/slides/larraine-osoa/", 
                                           "/static/img/slides/larraine-osoa/")
      
      # 5. Fix statassets typo
      content[line_num] <- str_replace_all(content[line_num], 
                                           "/statassets/", 
                                           "/static/")
    }
    
    # Fix doubled directory segments in <img data-src=...> attributes by editing the entire attribute
    img_data_src_lines <- which(str_detect(content, "<img[^>]*data-src="))
    
    if (length(img_data_src_lines) > 0) {
      changes_made <- FALSE
      
      for (line_num in img_data_src_lines) {
        line <- content[line_num]
        original_line <- line
        
        # Check for the problematic doubled path pattern
        if (str_detect(line, "_files/figure-revealjs/\\S+_files/figure-revealjs/")) {
          # Extract the entire img tag
          img_tag_match <- str_match(line, "(<img[^>]*data-src=\"[^\"]+\"[^>]*>)")
          if (!is.na(img_tag_match[1])) {
            img_tag <- img_tag_match[1]
            
            # Extract the data-src attribute value
            src_match <- str_match(img_tag, "data-src=\"([^\"]+)\"")
            if (!is.na(src_match[2])) {
              src_value <- src_match[2]
              
              # Fix the doubled directory pattern
              fixed_src <- str_replace(src_value, 
                                      "(_files/figure-revealjs/)[^/]+_files/figure-revealjs/", 
                                      "\\1")
              
              # Replace the old src with the fixed one
              fixed_img_tag <- str_replace(img_tag, 
                                          "data-src=\"[^\"]+\"", 
                                          paste0("data-src=\"", fixed_src, "\""))
              
              # Replace the entire img tag in the line
              content[line_num] <- str_replace(line, 
                                              "<img[^>]*data-src=\"[^\"]+\"[^>]*>", 
                                              fixed_img_tag)
              
              if (content[line_num] != original_line) {
                changes_made <- TRUE
                message("Fixed doubled path in line ", line_num, ":")
                message("  Old: ", src_value)
                message("  New: ", fixed_src)
              }
            }
          }
        }
      }
      
      if (!changes_made) {
        # If the specific pattern match didn't work, try the regex approach as fallback
        for (line_num in img_data_src_lines) {
          line <- content[line_num]
          
          # Fix specific doubled figure-revealjs paths
          fixed_line <- gsub(
            "(/[^\"]+_files/figure-revealjs/)\\1",
            "\\1",
            line,
            perl=TRUE
          )
          
          # Also fix paths without the leading slash
          fixed_line <- gsub(
            "(\"[^/\"]+_files/figure-revealjs/)\\1",
            "\\1",
            fixed_line,
            perl=TRUE
          )
          
          if (fixed_line != line) {
            content[line_num] <- fixed_line
            changes_made <- TRUE
          }
        }
      }
    }
    
    # Save modified content if changes were made
    if (!identical(content, content_original)) {
      writeLines(content, html_file, useBytes = TRUE)
      message("Fixed paths in: ", html_file)
      
      # Show what was changed (for first few changes only)
      changes <- which(content != content_original)
      if (length(changes) > 0) {
        max_to_show <- min(3, length(changes))
        message("Sample of changes made:")
        for (i in 1:max_to_show) {
          message("  Old: ", content_original[changes[i]])
          message("  New: ", content[changes[i]])
        }
        if (length(changes) > max_to_show) {
          message("  ... and ", length(changes) - max_to_show, " more changes")
        }
      }
    } else {
      message("No changes needed in: ", html_file)
    }
  }
}

# Get all HTML files in static slides directory and subdirectories
html_files_static <- dir_ls(slides_dir_static, recurse = TRUE, regexp = "\\.html$")
message("Processing static slides directory...")
process_html_files(html_files_static)

# Also process HTML files in public slides directory
if (dir_exists(slides_dir_public)) {
  message("\nProcessing public slides directory...")
  html_files_public <- dir_ls(slides_dir_public, recurse = TRUE, regexp = "\\.html$")
  process_html_files(html_files_public, is_public = TRUE)
} else {
  message("\nPublic slides directory not found: ", slides_dir_public)
  message("Skipping public directory processing.")
}

message("\nPath fixing complete.")
message("To check if all issues were fixed, run the check script again:") 