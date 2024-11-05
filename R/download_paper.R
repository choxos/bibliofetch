#' Download Academic Papers from Sci-Hub
#'
#' This function downloads academic papers from Sci-Hub using DOIs or URLs.
#' It provides real-time progress tracking and supports batch downloading.
#'
#' @param identifiers Character vector of DOIs or URLs
#' @param output_dir Directory to save downloaded PDFs (default: "downloads")
#' @param parallel Logical; whether to use parallel processing (default: FALSE)
#'
#' @return Invisible list of download results
#' @export
#'
#' @examples
#' \dontrun{
#' dois <- c("10.1038/nature09492", "10.1126/science.1157784")
#' download_scihub(dois)
#' }
download_scihub <- function(identifiers, output_dir = "downloads", parallel = FALSE) {
  # Load required packages
  check_packages()
  
  # Ensure identifiers is a character vector and remove any duplicates
  identifiers <- unique(as.character(identifiers))
  total_papers <- length(identifiers)
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
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
    
    # Download paper
    success <- download_single_paper(current_id, output_dir)
    
    # Update counters
    if (success) {
      successful <- successful + 1
    } else {
      failed <- failed + 1
    }
    
    # Store result
    results[[i]] <- list(
      identifier = current_id,
      success = success
    )
    
    # Update progress bar
    pb$tick(tokens = list(
      success = successful,
      failed = failed,
      current = i,
      total = total_papers
    ))
  }
  
  # Print final summary
  print_summary(total_papers, successful, failed, output_dir)
  
  # Return invisible results list
  invisible(results)
}