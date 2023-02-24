# eegUtilsExtra


`eegUtilsExtra` is an R package that provides additional utility functions for processing and analyzing EEG data, building on the `eegUtils` package. It includes functions for data cleaning, filtering, feature extraction, and visualization.

## Installation

You can install `eegUtilsExtra` from GitHub using the `devtools` package:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install eegUtilsExtra from GitHub
devtools::install_github("username/eegUtilsExtra")
```

Replace "username" with the GitHub username of the package author.

## Usage

Here's an example of how to use the `clean_data` function from `eegUtilsExtra` to remove noisy data from an EEG dataset:

```r
library(eegUtilsExtra)

# Load an example EEG dataset from the eegUtils package
data(eeg_data)

# Clean the data using the clean_data function
cleaned_data <- clean_data(eeg_data, threshold = 3, window_size = 100)

# View the cleaned data
plot(cleaned_data)
```

## Contributing

If you find a bug or have an idea for a new feature, please open an issue on the `eegUtilsExtra` GitHub repository. We also welcome pull requests with bug fixes, feature enhancements, or new functions.

## License

`eegUtilsExtra` is released under the MIT license. See the LICENSE file for details.
