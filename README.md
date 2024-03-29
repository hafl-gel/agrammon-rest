R Package `agrammon`
================

  - [1 Introduction](#1-introduction)
  - [2 Installation](#2-installation)
  - [3 Access Token](#3-access-token)
  - [4 Input Data Set](#4-input-data-set)
      - [4.1 Valid Input Data](#41-valid-input-data)
      - [4.2 Template - no categories
        specified](#42-template---no-categories-specified)
      - [4.3 Template - user defined
        categories](#43-template---user-defined-categories)
      - [4.4 Example Input Data](#44-example-input-data)
  - [5 Run Agrammon](#5-run-agrammon)
      - [5.1 Setting user specific
        defaults](#51-setting-user-specific-defaults)
      - [5.2 Specifying different
        reports](#52-specifying-different-reports)
      - [5.3 Extracting reports from “full” set of
        results](#53-extracting-reports-from-full-set-of-results)
      - [5.4 Results filtered by animal
        category](#54-results-filtered-by-animal-category)

# 1 Introduction

The package `agrammon` facilitates using the Nitrogen flow model
Agrammon (www.agrammon.ch) via its REST API interface with the help of
R. The communication between the R session and the Agrammon model is
possible via Agrammon’s REST API ([Wikipedia: REST
interface](https://en.wikipedia.org/wiki/Representational_state_transfer)
and [Agrammon REST
API](https://redocly.github.io/redoc/?url=https://model.agrammon.ch/single/api/v1/openapi.yaml).)

The package `agrammon` provides 6 functions that let the user

  - `register_token()`: store and re-use the personal access token
    (section [Access Token](#3-access-token))
  - `save_template()`: generate a valid data set (a CSV file) containing
    all necessary input data (section [Input Data
    Set](#4-input-data-set))
  - `run_agrammon()`: run Agrammon via its REST interface (section [Run
    Agrammon](#5-run-agrammon))
  - `agrammon_defaults()`: save some package specific defaults for the
    current R session (section [Run Agrammon](#5-run-agrammon))
  - `report()`: extract report from a “full” set of model results
    (section [Run Agrammon](#5-run-agrammon))
  - `save_excel()`: save results as an excel file

The main function of this package `run_agrammon()` needs the path to a
CSV file with input data to run Agrammon. To help the user prepare a
valid input data file (i.e. a CSV file with all valid and necessary
inputs), the function `save_template()` will save a CSV template file
with all necessary input variables including their default values (if
the variable has a default) as well as a remark on the accepted input
values (if there are any restrictions). See section [Input Data
Set](#4-input-data-set) for further explanation and examples on the
input data file.

`run_agrammon()` will act as follows:

1.  read in the input data set from the provided file path
2.  check the input data set for missing data and validate its values
3.  print a summary of the input data set in the console
4.  upload the input data set to the Agrammon server and run Agrammon on
    the server
5.  receive and check the model results
6.  return the results as either a `data.frame` or a `data.table`
    (default)

The user can process the results in the current R session or save them
as an EXCEL file using `save_excel()`.

# 2 Installation

The package can be installed from GitHub. Use `build_vignettes = TRUE`
if you want this vignette available locally:

``` r
devtools::install_github('hafl-gel/agrammon-rest', build_vignettes = TRUE)
```

To load & attach the package in the current R session, run:

``` r
library(agrammon)
```

# 3 Access Token

A personal access token (i.e., a string representing the user API key)
is required to communicate with the Agrammon REST interface. This access
token can be requested at support@agrammon.ch.

The access token can be registered either for the current R session or
permanently (in the `.Renviron` file in the user’s home directory
(`Sys.getenv("HOME")`)) using the function `register_token()`. If the
file `.Renviron` does not exist in the user home directory a new file
will be created, otherwise a line containing the token will be added to
the existing `.Renviron` file.

# 4 Input Data Set

## 4.1 Valid Input Data

Most of the Agrammon input variables are mandatory and must contain
valid entries, only few input variables are optional and will fall back
on a predefined default. The function `save_template()` writes an input
template file that can be used as guidance. **The template file column
separator is a semicolon (not a comma)\!**

Optional input variables contain an entry in the column ‘default’ in the
template. Remarks on valid inputs (numeric bounds, list of valid
entries, etc.) are indicated in column ‘remarks’ in the template.

## 4.2 Template - no categories specified

A template file with *all* possible input variables (i.e. not limited to
specific animal categories) can be written by setting the argument
`livestock` to `TRUE`:

``` r
# all categories
save_template('template_all.csv', TRUE)

# template on all categories with german help text
save_template('template_all_de.csv', TRUE, language = 'de')
```

## 4.3 Template - user defined categories

The user can provide specific animal categories or their parent classes
to restrict the template content to these categories:

``` r
# specific categories/classes
save_template('template_user.csv', list(RoughageConsuming = c('bisons', 'alpaca'), 
        wapiti = 'wapiti', dairy_cows = 'DC'), storage = 'tank 1')
```

## 4.4 Example Input Data

An example data file is shipped with the package.

``` r
# example input data set
path_ex <- system.file('extdata', 'example_data_set_3farms.csv', package = 'agrammon')
fread(path_ex)
#>             V1 V2                                                  V3                        V4        V5
#>   1: ex_3farms  1      Livestock::OtherCattle[Beef Cattle]::Excretion         dimensioning_barn        78
#>   2: ex_3farms  1                          Application::Slurry::Csoft              appl_hotdays sometimes
#>   3: ex_3farms  1                     Application::Slurry::Cfermented          fermented_slurry         0
#>   4: ex_3farms  1                          Application::Slurry::Ctech      share_deep_injection         0
#>   5: ex_3farms  1 Livestock::OtherCattle[Beef Cattle]::Housing::Floor  mitigation_housing_floor      none
#>  ---                                                                                                     
#> 303: ex_3farms  3                        Application::Slurry::Cseason appl_autumn_winter_spring        50
#> 304: ex_3farms  3                        Application::Slurry::Cseason               appl_summer        50
#> 305: ex_3farms  3                   Application::SolidManure::Cseason appl_autumn_winter_spring        50
#> 306: ex_3farms  3                   Application::SolidManure::Cseason               appl_summer        50
#> 307: ex_3farms  3                          Application::Slurry::Csoft              appl_evening         0
```

The first column is optional and contains the simulation label. The
second column is the farm label/id and is mandatory if the user
specifies input for more than one farm.

The order of the columns is irrelevant since the package tries to find
and assign the correct columns to the correct inputs. However, if you
encounter issues that might be related to false column assignment,
please report this back as an issue on GitHub
(<https://github.com/hafl-gel/agrammon-rest/issues>).

# 5 Run Agrammon

With the path to the input data set file, the model can be run with the
`run_agrammon()` function.

## 5.1 Setting user specific defaults

Defaults to the function `run_agrammon()` can be saved on a R session
basis by using the function `agrammon_defaults()`.

## 5.2 Specifying different reports

The content of the returned model result can be limited to contain
specific result reports. At the moment, the following reports can be
specified with argument `report`:

| argument `report` | model reporting                                                                                      |
| :---------------- | :--------------------------------------------------------------------------------------------------- |
| `"summary"`       | Short summary on the “top-level” NH3 emissions                                                       |
| `"detailed"`      | Summary on the “top-level” NH3 and other gaseous N emissions, as well as on the total N and TAN flow |
| `"NH3"`           | Slightly more detailed summary on the NH3 emissions                                                  |
| `"TAN"`           | Summary on the TAN flow                                                                              |
| `"N"`             | Summary on the total N flow                                                                          |
| `"HAFL"`          | Report used in teaching classes                                                                      |
| `"full"`          | Full (\!) set of model results                                                                       |

Argument `report` will be partially matched.

``` r
# path to example data set
path_ex <- system.file('inst/extdata', 'example_data_set_3farms.csv', package = 'agrammon')
# run model with default summary report:
run_agrammon(path_ex)
# run model with detailed results (report = 'detailed')
run_agrammon(path_ex, report = 'det')
```

## 5.3 Extracting reports from “full” set of results

Instead of calling the Agrammon model with different reports several
times, it is also possible to call Agrammon once with the “full” report
returning and extract the reports afterwards. This allows to extract
several reports by using only one model call which is faster, though,
this means also an increased data traffic, since the full set of model
results is returned (which might not be necessary if e.g. one is just
interested in one specific report).

``` r
# path to example data set
path_ex <- system.file('inst/extdata', 'example_data_set_3farms.csv', package = 'agrammon')
# run model with 'full' report:
res <- run_agrammon(path_ex, report = 'full')
# extract N report
report(res, 'N')
```

## 5.4 Results filtered by animal category

It is possible to “filter” the model results, i.e. attribute them to the
different animal categories. The following filters can be specified:

| argument `filter`       | animal category filtering                                                                                             |
| :---------------------- | :-------------------------------------------------------------------------------------------------------------------- |
| `"total_only"`          | Only the total amount from all animal categories will be returned                                                     |
| `"existing_categories"` | Individual values incl. the total amount from all animal categories that exist in the input data set will be returned |
| `"all_categories"`      | Individual values incl. the total amount from *all animal categories that can be defined* will be returned            |

Argument `filter` will be partially matched.

``` r
# path to example data set
path_ex <- system.file('inst/extdata', 'example_data_set_3farms.csv', package = 'agrammon')
# default summary report incl. filter by all available categories (even if they don't exist
# in the input)
run_agrammon(path_ex, filter = 'all')
# detailed results incl. filter by existing categories
run_agrammon(path_ex, report = 'det', filter = 'ex')
```
