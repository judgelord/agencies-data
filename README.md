# Agency Regex Builder #

This package relies on a dataframe (crosswalk) of government agencies and versions of their names, acronyms, and any other commonly used aliases. 
It constructs a regular expression search pattern for each agency using this information, along with conditions under which the pattern should yield a match. 
Pattern formats differ across agencies because some names or acronyms are shared by multiple agencies or change over time. 


## Example regex table ##
```r
#>   agency                         envirodatagov_url             pattern
#>   <chr>                          <chr>                         <chr>
#> 1 Environmental Protection Agency https://www.epa.gov            Environmental Protection|EPA|US EPA
#> 2 Department of Energy            https://www.energy.gov         Department of Energy|DOE
#> 5 Department of Agriculture       https://www.usda.gov          USDA|Department of Agriculture
#> 6 Department of Transportation    https://www.transportation… DOT|Department of Transportation
```

Before searching the text, the function also flags duplicate aliases to help prevent incorrect or unclear matches. Users can review these duplicates in the duplicate object.

```r
# View aliases shared by multiple agencies by running
result$duplicates
```

## Usage ##

The main function, `build_regextable`, returns a list with two objects:
+ `regextable` - a dataframe with each agency, its primary URL, and its regex pattern.
+ `duplicates` - a vector of aliases that appear in multiple agencies and do not return a clear match,

```r
library(dplyr)
library(readxl)
library(googlesheets4)

sheet_url <- "https://docs.google.com/spreadsheets/d/YOUR_SHEET_ID/edit#gid=0"

result <- build_regextable(sheet_url)

# Access the regex table
regex_table <- result$regextable

# Access flagged duplicate aliases
duplicates <- result$duplicates
```

The regular expression table is designed to be used for text extraction and matching when parsing agency names from websites, government documents, or publically available datasets.

