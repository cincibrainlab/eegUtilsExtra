# eegUtilsExtra

[![R-CMD-check](https://github.com/cincibrainlab/eegUtilsExtra/actions/workflows/main.yml/badge.svg)](https://github.com/cincibrainlab/eegUtilsExtra/actions/workflows/main.yml)


`eegUtilsExtra` is an R package that provides additional utility functions for processing and analyzing EEG data, building on the `eegUtils` package. It includes functions for data cleaning, filtering, feature extraction, and visualization.

## Installation

You can install `eegUtilsExtra` from GitHub using the `devtools` package:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install eegUtilsExtra from GitHub
devtools::install_github("cincibrainlab/eegUtilsExtra")
```

## Usage

Here's an example of how to use the `clean_data` function from `eegUtilsExtra` to remove noisy data from an EEG dataset:

```r
library(eegUtilsExtra)

# Find all EEG SET files in subdirectories of a directory that contain "VEP"
results_df <- list_SET_files_within_subdirs(dir_path = "/path/to/directory",
                                             keyword = "VEP")

# View the resulting tidy data frame
head(results_df)
```

## Contributing

If you find a bug or have an idea for a new feature, please open an issue on the `eegUtilsExtra` GitHub repository. We also welcome pull requests with bug fixes, feature enhancements, or new functions.

## License

`eegUtilsExtra` is released under the MIT license. See the LICENSE file for details.
