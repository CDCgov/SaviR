
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SaviR <img src='man/figures/hex-SaviR.png' align = "right" height="200" />

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/CDCgov/SaviR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/CDCgov/SaviR?branch=master)
<!-- badges: end -->

## Overview

The purpose of the SaviR package is to collate data from various
publicly available Coronavirus 2019 (COVID-19) data sources for analysis
and visualization. This package is divided into two main sections: COVID
Analytical Datasets (COVAD) and COVID Data Visualizations (COVIS).

### Data Sources

-   [**World Health Organization (WHO)**](https://covid19.who.int/) -
    COVID-19 Cases and Deaths (China data includes Taiwan, Hong Kong,
    and Macua data)  
-   [**Johns Hopkins University
    (JHU)**](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series) -
    COVID-19 Cases and Deaths (China, Taiwan, Hong Kong, and Macua data
    are disaggregated)  
-   [**Our World In Data
    (OWID)**](https://github.com/owid/covid-19-data/tree/master/public/data) -
    COVID-19 Vaccines, Tests, and Hospitalizations  
-   [**United
    Nations**](https://population.un.org/wpp/Download/Standard/Population/) -
    Population Estimates
-   [**World
    Bank**](http://api.worldbank.org/v2/country?format=json&per_page=300) -
    Country Income Classifications

*Note: Please refer to original data sources to ensure data accuracy as
data models could have changed*

## Installation and Package Load

You can install the released version of SaviR from
[Github](https://github.com/CDCGov/SaviR) with:

``` r
devtools::install_github("CDCgov/SaviR")
```

You can also use the code above to update your SaviR package as needed.
When loading SaviR, be sure to load in this package *last*.

### COVAD - COVID Analytical Datasets

| Metadata Functions      | Get Functions        | Calculation Functions   |
|-------------------------|----------------------|-------------------------|
| get\_onetable           | get\_combined\_table | calc\_add\_risk         |
| get\_country\_coords    | get\_covid\_df       | calc\_vax\_carryforward |
| get\_vax\_dates         | get\_testing         |                         |
| get\_vax\_manufacturers | get\_testing\_long   |                         |
|                         | get\_vax             |                         |
|                         | get\_vax\_by\_type   |                         |
|                         | get\_hospdata        |                         |
|                         | get\_gdeltnews       |                         |

-   Metadata is stored in this package and can be called:

``` r
metadata <- onetable  
country_coordinates <- country_coords  
```

### COVIS - COVID Visualizations

| Plot Functions              | Map Functions     | Table Functions           |
|-----------------------------|-------------------|---------------------------|
| plot\_epicurve              | map\_template     | table\_10mostcases        |
| plot\_epicurve\_ind         | map\_burden       | table\_10incidence        |
| plot\_epicurve\_epidouble   | map\_trend        | table\_10percentchange    |
| plot\_epicurve\_dailydouble | map\_vaccinations | table\_10vaccinations     |
| plot\_riskmatrix            |                   | table\_countriesofconcern |
| plot\_vaxcoverage           |                   |                           |
| plot\_vaxcoverage\_pop      |                   |                           |
| plot\_vaxcurve              |                   |                           |

## Public Domain Standard Notice

This repository constitutes a work of the United States Government and
is not subject to domestic copyright protection under 17 USC § 105. This
repository is in the public domain within the United States, and
copyright and related rights in the work worldwide are waived through
the CC0 1.0 Universal public domain dedication. All contributions to
this repository will be released under the CC0 dedication. By submitting
a pull request you are agreeing to comply with this waiver of copyright
interest.

## License Standard Notice

The repository utilizes code licensed under the terms of the Apache
Software License and therefore is licensed under ASL v2 or later. This
source code in this repository is free: you can redistribute it and/or
modify it under the terms of the Apache Software License version 2, or
(at your option) any later version. This source code in this repository
is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the Apache Software License for
more details. You should have received a copy of the Apache Software
License along with this program. If not, see
<http://www.apache.org/licenses/LICENSE-2.0.html> The source code forked
from other open source projects will inherit its license.

## Privacy Standard Notice

This repository contains only non-sensitive, publicly available data and
information. All material and community participation is covered by the
Disclaimer and Code of Conduct. For more information about CDC’s privacy
policy, please visit <http://www.cdc.gov/other/privacy.html>.

## Contributing Standard Notice

Anyone is encouraged to contribute to the repository by forking and
submitting a pull request. (If you are new to GitHub, you might start
with a basic tutorial.) By contributing to this project, you grant a
world-wide, royalty-free, perpetual, irrevocable, non-exclusive,
transferable license to all users under the terms of the Apache Software
License v2 or later. All comments, messages, pull requests, and other
submissions received through CDC including this GitHub page may be
subject to applicable federal law, including but not limited to the
Federal Records Act, and may be archived. Learn more at
<http://www.cdc.gov/other/privacy.html>.

## Records Management Standard Notice

This repository is not a source of government records, but is a copy to
increase collaboration and collaborative potential. All government
records will be published through the CDC web site.

## Additional Standard Notices

Please refer to CDC’s Template Repository for more information about
contributing to this repository, public domain notices and disclaimers,
and code of conduct.
