# bibliofetch

Download academic papers from Sci-Hub using DOIs or URLs.

## Installation

You can install the development version of scihubr from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("choxos/bibliofetch")
```

## Usage

```r
library(bibliofetch)

# Download a single paper
download_scihub("10.1038/nature09492")

# Download multiple papers
dois <- c(
  "10.1038/nature09492",
  "10.1126/science.1157784"
)
download_scihub(dois)
```

## Features

- Download papers using DOIs or URLs
- Real-time progress tracking
- Color-coded success/failure indication
- Batch downloading support
- Automatic PDF naming and organization

## License

This project is licensed under the GNU GPL 3 - see the LICENSE file for details.

## Note

Please use this package responsibly and in accordance with your institution's policies regarding academic paper access.