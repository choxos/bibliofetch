#' Sci-Hub Paper Downloader
#' A comprehensive tool for downloading academic papers from Sci-Hub
#' 
#' @author Ahmad Sofi-Mahmudi
#' @version 1.0.0

# Required packages
#' @import httr
#' @import rvest
#' @import progress
#' @import crayon
#' @import tools
#' @import stringr

# Function to check and install required packages
check_packages <- function() {
  required_packages <- c("httr", "rvest", "progress", "crayon", "tools", "stringr")
  missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  
  if (length(missing_packages) > 0) {
    message("Installing required packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages)
  }
  
  # Load all required packages
  for (pkg in required_packages) {
    library(pkg, character.only = TRUE)
  }
}

#' Get default Sci-Hub mirrors
#' @return Character vector of Sci-Hub URLs
get_scihub_mirrors <- function() {
  return(c(
    "https://sci-hub.se",
    "https://sci-hub.st",
    "https://sci-hub.ru"
    # Add more mirrors as needed
  ))
}

#' Construct URL for paper download
#' @param identifier DOI or URL of the paper
#' @param scihub_url Optional custom Sci-Hub URL
#' @return Valid Sci-Hub URL for the paper
construct_paper_url <- function(identifier, scihub_url = NULL) {
  # Clean the identifier
  identifier <- trimws(identifier)
  identifier <- gsub("^https?://doi.org/", "", identifier)
  
  # If custom URL provided, use it
  if (!is.null(scihub_url)) {
    return(paste0(scihub_url, "/", identifier))
  }
  
  # Try each mirror until one works
  mirrors <- get_scihub_mirrors()
  for (mirror in mirrors) {
    url <- paste0(mirror, "/", identifier)
    tryCatch({
      resp <- httr::GET(url, httr::timeout(10))
      if (httr::status_code(resp) == 200) {
        return(url)
      }
    }, error = function(e) {
      # Continue to next mirror
    })
  }
  
  stop("No working Sci-Hub mirror found")
}

#' Extract PDF URL from Sci-Hub page
#' @param html_content HTML content of Sci-Hub page
#' @return URL of the PDF
extract_pdf_url <- function(html_content) {
  # Try to find PDF iframe
  pdf_url <- html_content %>%
    rvest::html_nodes("iframe#pdf") %>%
    rvest::html_attr("src")
  
  # If not found, try alternative elements
  if (length(pdf_url) == 0) {
    pdf_url <- html_content %>%
      rvest::html_nodes("embed#pdf") %>%
      rvest::html_attr("src")
  }
  
  # Clean up URL
  if (length(pdf_url) > 0) {
    pdf_url <- pdf_url[1]
    if (!grepl("^https?://", pdf_url)) {
      pdf_url <- paste0("https:", pdf_url)
    }
    return(pdf_url)
  }
  
  stop("PDF URL not found in Sci-Hub page")
}

#' Download a single paper from Sci-Hub
#' @param identifier DOI or URL of the paper
#' @param output_dir Directory to save the PDF
#' @param scihub_url Optional custom Sci-Hub URL
#' @return Logical indicating success
download_single_paper <- function(identifier, output_dir, scihub_url = NULL) {
  tryCatch({
    # Construct paper URL
    paper_url <- construct_paper_url(identifier, scihub_url)
    
    # Get Sci-Hub page
    response <- httr::GET(paper_url, httr::timeout(30))
    if (httr::status_code(response) != 200) {
      stop("Failed to access Sci-Hub page")
    }
    
    # Extract PDF URL
    html_content <- rvest::read_html(response)
    pdf_url <- extract_pdf_url(html_content)
    
    # Generate output filename
    filename <- paste0(
      gsub("[^a-zA-Z0-9]", "_", identifier),
      ".pdf"
    )
    output_path <- file.path(output_dir, filename)
    
    # Download PDF
    pdf_response <- httr::GET(pdf_url, 
                             httr::timeout(60),
                             httr::write_disk(output_path, overwrite = TRUE))
    
    # Verify download
    if (!file.exists(output_path) || file.size(output_path) < 1000) {
      stop("Downloaded file appears to be invalid")
    }
    
    return(TRUE)
    
  }, error = function(e) {
    warning(sprintf("Failed to download %s: %s", identifier, e$message))
    return(FALSE)
  })
}

#' Download Academic Papers from Sci-Hub
#'
#' This function downloads academic papers from Sci-Hub using DOIs or URLs.
#' It provides real-time progress tracking and supports batch downloading.
#'
#' @param identifiers Character vector of DOIs or URLs
#' @param output_dir Directory to save downloaded PDFs (default: "downloads")
#' @param parallel Logical; whether to use parallel processing (default: FALSE)
#' @param scihub_url Character; custom Sci-Hub URL (default: NULL, will use internal mirror list)
#' @param wait_time Numeric; waiting time in seconds between downloads (default: 10)
#' @param random_wait Logical; add random variation to waiting time (default: TRUE)
#'
#' @return Invisible list of download results
#' @export
#'
#' @examples
#' \dontrun{
#' dois <- c("10.1038/nature09492", "10.1126/science.1157784")
#' download_scihub(dois)
#' 
#' # Using custom Sci-Hub URL and longer wait time
#' download_scihub(dois, 
#'                scihub_url = "https://sci-hub.example.com",
#'                wait_time = 15)
#' }
download_scihub <- function(identifiers, 
                          output_dir = "downloads", 
                          parallel = FALSE, 
                          scihub_url = NULL,
                          wait_time = 10,
                          random_wait = TRUE) {
  # Load required packages
  check_packages()
  
  # Validate wait time
  if (!is.numeric(wait_time) || wait_time < 0) {
    stop("wait_time must be a non-negative number")
  }
  
  # Validate Sci-Hub URL if provided
  if (!is.null(scihub_url)) {
    if (!grepl("^https?://", scihub_url)) {
      stop("Invalid Sci-Hub URL. Must start with 'http://' or 'https://'")
    }
    # Remove trailing slash if present
    scihub_url <- sub("/$", "", scihub_url)
    message(sprintf("Using custom Sci-Hub URL: %s", scihub_url))
  }
  
  # Ensure identifiers is a character vector and remove any duplicates
  identifiers <- unique(as.character(identifiers))
  total_papers <- length(identifiers)
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Initialize counters
  successful <- 0
  failed <- 0
  
  # Create progress bar
  pb <- progress::progress_bar$new(
    format = paste0(
      "Downloading [:bar] :percent | ",
      crayon::green("Success: :success"), " | ",
      crayon::red("Failed: :failed"), " | ",
      "Paper :current/:total | ",
      "Wait time: :wait sec | ",
      "Est. time remaining: :eta"
    ),
    total = total_papers,
    width = 80,
    clear = FALSE
  )
  
  # Process each identifier
  results <- list()
  for (i in seq_along(identifiers)) {
    current_id <- identifiers[i]
    
    # Calculate actual wait time for this iteration
    current_wait <- if (random_wait) {
      # Add random variation of ±30% to wait time
      wait_time * runif(1, 0.7, 1.3)
    } else {
      wait_time
    }
    
    # Download paper
    success <- download_single_paper(current_id, output_dir, scihub_url)
    
    # Update counters
    if (success) {
      successful <- successful + 1
    } else {
      failed <- failed + 1
    }
    
    # Store result
    results[[i]] <- list(
      identifier = current_id,
      success = success,
      scihub_url = scihub_url,
      wait_time = current_wait
    )
    
    # Update progress bar
    pb$tick(tokens = list(
      success = successful,
      failed = failed,
      current = i,
      total = total_papers,
      wait = round(current_wait, 1)
    ))
    
    # Wait before next download if not the last item
    if (i < total_papers) {
      Sys.sleep(current_wait)
      message(sprintf("Waiting %.1f seconds before next download...", current_wait))
    }
  }
  
  # Print final summary
  print_summary(total_papers, successful, failed, output_dir)
  
  # Return invisible results list
  invisible(results)
}

# Helper function for printing summary
print_summary <- function(total, successful, failed, output_dir) {
  cat("\nDownload Summary:\n")
  cat("----------------\n")
  cat(sprintf("Total papers: %d\n", total))
  cat(sprintf("Successfully downloaded: %d\n", successful))
  cat(sprintf("Failed downloads: %d\n", failed))
  cat(sprintf("Success rate: %.1f%%\n", (successful/total) * 100))
  cat(sprintf("Downloads saved to: %s\n", normalizePath(output_dir)))
}