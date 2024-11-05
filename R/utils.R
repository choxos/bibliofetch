#' Download a Single Paper from Sci-Hub
#'
#' @param identifier DOI or URL of the paper
#' @param output_dir Directory to save the PDF
#' @return Logical indicating success or failure
#' @keywords internal
download_single_paper <- function(identifier, output_dir) {
  tryCatch({
    # Construct Sci-Hub URL
    scihub_base <- "https://sci-hub.se/"
    scihub_url <- paste0(scihub_base, trimws(identifier))
    
    # Add delay to avoid overwhelming the server
    Sys.sleep(1)
    
    # Send request
    response <- httr::GET(
      url = scihub_url,
      config = httr::timeout(10),
      httr::add_headers(
        `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
        `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
      )
    )
    
    if (httr::status_code(response) != 200) {
      return(FALSE)
    }
    
    # Parse the HTML content
    page <- rvest::read_html(httr::content(response, "text"))
    
    # Try to find PDF link
    pdf_link <- page %>%
      rvest::html_nodes("embed") %>%
      rvest::html_attr("src")
    
    # If no embed found, try iframe
    if (length(pdf_link) == 0) {
      pdf_link <- page %>%
        rvest::html_nodes("iframe") %>%
        rvest::html_attr("src")
    }
    
    if (length(pdf_link) == 0) {
      return(FALSE)
    }
    
    # Take first PDF link if multiple found
    pdf_link <- pdf_link[1]
    
    # Ensure PDF link is complete
    if (startsWith(pdf_link, "//")) {
      pdf_link <- paste0("https:", pdf_link)
    }
    
    # Generate output filename
    safe_identifier <- gsub("[^a-zA-Z0-9]", "_", identifier)
    filename <- paste0("paper_", safe_identifier, ".pdf")
    output_path <- file.path(output_dir, filename)
    
    # Download the PDF
    pdf_response <- httr::GET(
      url = pdf_link,
      config = httr::timeout(30),
      httr::add_headers(
        `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
      ),
      httr::write_disk(output_path, overwrite = TRUE)
    )
    
    return(httr::status_code(pdf_response) == 200)
    
  }, error = function(e) {
    return(FALSE)
  })
}

#' Check Required Packages
#'
#' @keywords internal
check_packages <- function() {
  packages <- c("httr", "rvest", "progress", "crayon")
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required. Please install it."))
    }
  }
}

#' Print Summary of Downloads
#'
#' @param total Total number of papers
#' @param successful Number of successful downloads
#' @param failed Number of failed downloads
#' @param output_dir Output directory
#' @keywords internal
print_summary <- function(total, successful, failed, output_dir) {
  cat("\n")
  cat(paste0(
    "Download Summary:\n",
    "Total papers: ", total, "\n",
    crayon::green(paste0("Successfully downloaded: ", successful, "\n")),
    crayon::red(paste0("Failed: ", failed, "\n")),
    "Success rate: ", round(successful/total * 100, 1), "%\n",
    "Files saved in: ", normalizePath(output_dir), "\n"
  ))
}