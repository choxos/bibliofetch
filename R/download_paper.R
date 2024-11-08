#' Sci-Hub Paper Downloader
#' A comprehensive tool for downloading academic papers from Sci-Hub
#' using either Selenium or direct HTTP requests
#' 
#' @author Ahmad Sofi-Mahmudi
#' @version 2.0.0

#' @import httr
#' @import rvest
#' @import RSelenium
#' @import wdman
#' @import progress
#' @import crayon
#' @import tools
#' @import stringr

# Utility Functions ------------------------------------------------------------

#' @importFrom utils install.packages installed.packages
NULL

#' Check and install required packages
#' @param method Character string indicating the download method ("rvest" or "selenium")
#' @keywords internal
.check_packages <- function(method = c("rvest", "selenium")) {
  # Match method argument
  method <- match.arg(method)
  
  # Define required packages based on method
  base_packages <- c("progress", "crayon", "tools", "stringr")
  method_packages <- switch(method,
    "selenium" = c("RSelenium", "wdman"),
    "rvest" = c("httr", "rvest")
  )
  
  required_packages <- c(base_packages, method_packages)
  missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  
  if (length(missing_packages) > 0) {
    message("Installing required packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages)
  }
  
  # Load all required packages
  for (pkg in required_packages) {
    suppressPackageStartupMessages({
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(paste("Package", pkg, "is required but not available."))
      }
    })
  }
  
  invisible(required_packages)
}

#' Get default Sci-Hub mirrors
#' @return Character vector of Sci-Hub URLs
#' @keywords internal
get_scihub_mirrors <- function() {
  return(c(
    "https://sci-hub.se",
    "https://sci-hub.st",
    "https://sci-hub.ru"
    # Add more mirrors as needed
  ))
}

#' Print download summary
#' @keywords internal
print_summary <- function(total, successful, failed, output_dir) {
  cat("\nDownload Summary:\n")
  cat("----------------\n")
  cat(sprintf("Total papers: %d\n", total))
  cat(sprintf("Successfully downloaded: %d\n", successful))
  cat(sprintf("Failed downloads: %d\n", failed))
  cat(sprintf("Success rate: %.1f%%\n", (successful/total) * 100))
  cat(sprintf("Downloads saved to: %s\n", normalizePath(output_dir)))
}

# Selenium-specific Functions -------------------------------------------------

#' Initialize Selenium WebDriver
#' @param browser Character: browser to use ("firefox" or "chrome")
#' @return Selenium WebDriver remote driver object
#' @keywords internal
initialize_selenium <- function(browser = "firefox") {
  tryCatch({
    message("Initializing Selenium WebDriver...")
    
    if (browser == "firefox") {
      driver <- wdman::selenium(browser = "firefox")
      remote_driver <- RSelenium::remoteDriver(
        browserName = "firefox",
        port = 4444L
      )
    } else {
      driver <- wdman::selenium(browser = "chrome")
      remote_driver <- RSelenium::remoteDriver(
        browserName = "chrome",
        port = 4444L,
        extraCapabilities = list(
          chromeOptions = list(
            args = c('--headless', '--disable-gpu', '--no-sandbox',
                    '--disable-dev-shm-usage')
          )
        )
      )
    }
    
    remote_driver$open(silent = TRUE)
    return(list(driver = driver, remote_driver = remote_driver))
    
  }, error = function(e) {
    stop("Failed to initialize Selenium: ", e$message)
  })
}

#' Download a single paper using Selenium
#' @keywords internal
download_single_paper_selenium <- function(remote_driver, identifier, output_dir, scihub_url = NULL) {
  tryCatch({
    # Construct paper URL
    paper_url <- if (!is.null(scihub_url)) {
      paste0(scihub_url, "/", identifier)
    } else {
      mirrors <- get_scihub_mirrors()
      for (mirror in mirrors) {
        url <- paste0(mirror, "/", identifier)
        remote_driver$navigate(url)
        Sys.sleep(2)
        if (!grepl("404|error", tolower(remote_driver$getTitle()))) {
          return(url)
        }
      }
      stop("No working Sci-Hub mirror found")
    }
    
    remote_driver$navigate(paper_url)
    Sys.sleep(3)
    
    pdf_elements <- remote_driver$findElements(
      using = "css selector", 
      value = c("iframe#pdf", "embed#pdf", "a[href$='.pdf']")
    )
    
    if (length(pdf_elements) == 0) {
      stop("PDF element not found")
    }
    
    pdf_url <- pdf_elements[[1]]$getElementAttribute("src")[[1]] %||%
               pdf_elements[[1]]$getElementAttribute("href")[[1]]
    
    if (is.null(pdf_url)) {
      stop("Could not extract PDF URL")
    }
    
    if (!grepl("^https?://", pdf_url)) {
      pdf_url <- paste0("https:", pdf_url)
    }
    
    filename <- paste0(gsub("[^a-zA-Z0-9]", "_", identifier), ".pdf")
    output_path <- file.path(output_dir, filename)
    
    remote_driver$navigate(pdf_url)
    Sys.sleep(5)
    
    if (!file.exists(output_path) || file.size(output_path) < 1000) {
      stop("Downloaded file appears to be invalid")
    }
    
    return(TRUE)
  }, error = function(e) {
    warning(sprintf("Failed to download %s: %s", identifier, e$message))
    return(FALSE)
  })
}

# Rvest-specific Functions --------------------------------------------------

#' Download a single paper using rvest/httr
#' @keywords internal
download_single_paper_rvest <- function(identifier, output_dir, scihub_url = NULL) {
  tryCatch({
    # Construct paper URL
    paper_url <- if (!is.null(scihub_url)) {
      paste0(scihub_url, "/", identifier)
    } else {
      mirrors <- get_scihub_mirrors()
      for (mirror in mirrors) {
        url <- paste0(mirror, "/", identifier)
        resp <- httr::GET(url, httr::timeout(10))
        if (httr::status_code(resp) == 200) {
          return(url)
        }
      }
      stop("No working Sci-Hub mirror found")
    }
    
    response <- httr::GET(paper_url, httr::timeout(30))
    if (httr::status_code(response) != 200) {
      stop("Failed to access Sci-Hub page")
    }
    
    html_content <- rvest::read_html(response)
    
    pdf_url <- html_content %>%
      rvest::html_nodes("iframe#pdf, embed#pdf") %>%
      rvest::html_attr("src") %>%
      .[1]
    
    if (is.null(pdf_url)) {
      stop("PDF URL not found")
    }
    
    if (!grepl("^https?://", pdf_url)) {
      pdf_url <- paste0("https:", pdf_url)
    }
    
    filename <- paste0(gsub("[^a-zA-Z0-9]", "_", identifier), ".pdf")
    output_path <- file.path(output_dir, filename)
    
    pdf_response <- httr::GET(
      pdf_url,
      httr::timeout(60),
      httr::write_disk(output_path, overwrite = TRUE)
    )
    
    if (!file.exists(output_path) || file.size(output_path) < 1000) {
      stop("Downloaded file appears to be invalid")
    }
    
    return(TRUE)
  }, error = function(e) {
    warning(sprintf("Failed to download %s: %s", identifier, e$message))
    return(FALSE)
  })
}

#' Check if PDF exists and is valid
#' @param filepath Path to PDF file
#' @param min_size Minimum file size in bytes (default: 1000)
#' @return Logical indicating if file exists and is valid
#' @keywords internal
.check_pdf_exists <- function(filepath, min_size = 1000) {
  file.exists(filepath) && file.size(filepath) >= min_size
}

# Main Function -------------------------------------------------------------

#' Download Academic Papers from Sci-Hub
#'
#' @param identifiers Character vector of DOIs or URLs
#' @param output_dir Directory to save downloaded PDFs (default: "downloads")
#' @param method Download method to use: "rvest" or "selenium" (default: "rvest")
#' @param scihub_url Character; custom Sci-Hub URL (default: NULL, will use internal mirror list)
#' @param wait_time Numeric; waiting time in seconds between downloads (default: 10)
#' @param random_wait Logical; add random variation to waiting time (default: TRUE)
#' @param skip_existing Logical; skip downloading if PDF already exists (default: TRUE)
#'
#' @return Invisible list of download results
#' @export
#'
download_scihub <- function(identifiers, 
                          output_dir = "downloads", 
                          method = c("rvest", "selenium"),
                          scihub_url = NULL,
                          wait_time = 10,
                          random_wait = TRUE,
                          skip_existing = TRUE) {
  
  # Match method argument
  method <- match.arg(method)
  
  # Check packages without explicit method argument
  .check_packages(method)
  
  # Validate parameters
  if (!is.numeric(wait_time) || wait_time < 0) {
    stop("wait_time must be a non-negative number")
  }
  
  if (!is.null(scihub_url) && !grepl("^https?://", scihub_url)) {
    stop("Invalid Sci-Hub URL. Must start with 'http://' or 'https://'")
  }
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Process identifiers
  identifiers <- unique(as.character(identifiers))
  total_papers <- length(identifiers)
  
  # Initialize counters
  successful <- 0
  failed <- 0
  skipped <- 0
  
  # Create progress bar with skipped count
  pb <- progress::progress_bar$new(
    format = paste0(
      "Downloading [:bar] :percent | ",
      crayon::green("Success: :success"), " | ",
      crayon::yellow("Skipped: :skipped"), " | ",
      crayon::red("Failed: :failed"), " | ",
      "Paper :current/:total"
    ),
    total = total_papers,
    width = 80,
    clear = FALSE
  )
  
  # Process each identifier
  results <- list()
  for (i in seq_along(identifiers)) {
    current_id <- identifiers[i]
    
    # Generate filename
    filename <- paste0(gsub("[^a-zA-Z0-9]", "_", current_id), ".pdf")
    output_path <- file.path(output_dir, filename)
    
    # Check if file already exists
    if (skip_existing && .check_pdf_exists(output_path)) {
      skipped <- skipped + 1
      results[[i]] <- list(
        identifier = current_id,
        success = TRUE,
        skipped = TRUE,
        method = method,
        file_path = output_path
      )
    } else {
      # Calculate wait time (but don't show message)
      current_wait <- if (random_wait) {
        wait_time * runif(1, 0.7, 1.3)
      } else {
        wait_time
      }
      
      # Download paper using selected method
      success <- if (method == "selenium") {
        download_single_paper_selenium(selenium$remote_driver, current_id, output_dir, scihub_url)
      } else {
        download_single_paper_rvest(current_id, output_dir, scihub_url)
      }
      
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
        skipped = FALSE,
        method = method,
        file_path = if(success) output_path else NA
      )
      
      # Wait before next download (silently)
      if (i < total_papers) {
        Sys.sleep(current_wait)
      }
    }
    
    # Update progress bar
    pb$tick(tokens = list(
      success = successful,
      skipped = skipped,
      failed = failed,
      current = i,
      total = total_papers
    ))
  }
  
  # Print final summary
  cat("\nDownload Summary:\n")
  cat("----------------\n")
  cat(sprintf("Total papers: %d\n", total_papers))
  cat(sprintf("Successfully downloaded: %d\n", successful))
  cat(sprintf("Skipped existing: %d\n", skipped))
  cat(sprintf("Failed downloads: %d\n", failed))
  cat(sprintf("Downloads saved to: %s\n", normalizePath(output_dir)))
  
  # Return results
  invisible(results)
}