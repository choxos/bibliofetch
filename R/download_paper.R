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

#' Check and install required packages
#' @param method Character string indicating the download method ("rvest" or "selenium")
#' @keywords internal
check_packages <- function(method = c("rvest", "selenium")) {
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
      library(pkg, character.only = TRUE)
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

# Main Function -------------------------------------------------------------

#' Download Academic Papers from Sci-Hub
#'
#' This function downloads academic papers from Sci-Hub using either Selenium
#' WebDriver or direct HTTP requests via rvest.
#'
#' @param identifiers Character vector of DOIs or URLs
#' @param output_dir Directory to save downloaded PDFs (default: "downloads")
#' @param method Download method to use: "rvest" or "selenium" (default: "rvest")
#' @param browser Browser to use for selenium method: "firefox" or "chrome" (default: "firefox")
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
#' 
#' # Using rvest method (default)
#' download_scihub(dois)
#' 
#' # Using selenium method
#' download_scihub(dois, method = "selenium", browser = "firefox")
#' }
download_scihub <- function(identifiers, 
                          output_dir = "downloads", 
                          method = "rvest",
                          browser = "firefox",
                          scihub_url = NULL,
                          wait_time = 10,
                          random_wait = TRUE) {
  
  # Validate method
  method <- match.arg(method, c("rvest", "selenium"))
  
  # Load required packages
  check_packages(method)
  
  # Validate parameters
  if (!is.numeric(wait_time) || wait_time < 0) {
    stop("wait_time must be a non-negative number")
  }
  
  if (!is.null(scihub_url) && !grepl("^https?://", scihub_url)) {
    stop("Invalid Sci-Hub URL. Must start with 'http://' or 'https://'")
  }
  
  if (method == "selenium" && !browser %in% c("firefox", "chrome")) {
    stop("Browser must be either 'firefox' or 'chrome' for selenium method")
  }
  
  # Initialize Selenium if needed
  if (method == "selenium") {
    selenium <- initialize_selenium(browser)
    on.exit({
      try(selenium$remote_driver$close())
      try(selenium$driver$stop())
    })
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
    
    # Calculate wait time
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
      method = method,
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
    
    # Wait before next download
    if (i < total_papers) {
      Sys.sleep(current_wait)
      message(sprintf("Waiting %.1f seconds before next download...", current_wait))
    }
  }
  
  # Print final summary
  print_summary(total_papers, successful, failed, output_dir)
  
  # Return results
  invisible(results)
}