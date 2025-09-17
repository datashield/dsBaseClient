## dsBaseClient: 'DataSHIELD' Client Side Base Functions

[![License](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![](https://www.r-pkg.org/badges/version/dsBaseClient?color=black)](https://cran.r-project.org/package=dsBaseClient)
[![R build
status](https://github.com/datashield/dsBaseClient/workflows/R-CMD-check/badge.svg)](https://github.com/datashield/dsBaseClient/actions)
[![Codecov test coverage](https://codecov.io/gh/datashield/dsBaseClient/graph/badge.svg)](https://app.codecov.io/gh/datashield/dsBaseClient)

## Installation

You can install the released version of dsBaseClient from
[CRAN](https://cran.r-project.org/package=dsBaseClient) with:

``` r
install.packages("dsBaseClient")
```

And the development version from
[GitHub](https://github.com/datashield/dsBaseClient/) with:
<!-- You can install the development version from [GitHub](https://github.com/) with: -->

``` r
install.packages("remotes")
remotes::install_github("datashield/dsBaseClient", "<BRANCH>")

# Install v6.3.4 with the following
remotes::install_github("datashield/dsBaseClient", "6.3.4")
```

For a full list of development branches, checkout https://github.com/datashield/dsBaseClient/branches


## About

DataSHIELD is a software package which allows you to do non-disclosive federated analysis on sensitive data. Our website (https://www.datashield.org) has in depth descriptions of what it is, how it works and how to install it. A key point to highlight is that DataSHIELD has a client-server infrastructure, so the dsBase package (https://github.com/datashield/dsBase) needs to be used in conjunction with the dsBaseClient package (https://github.com/datashield/dsBaseClient) - trying to use one without the other makes no sense.

Detailed instructions on how to install DataSHIELD are at https://www.datashield.org/wiki.

Discussion and help with using DataSHIELD can be obtained from The DataSHIELD Forum https://datashield.discourse.group/

The code here is organised as:

| Location                     | What is it? |
| ---------------------------- | ------------| 
| obiba CRAN                   | Where you probably should install DataSHIELD from. |
| releases                     | Stable releases. |
| master branch                | Mostly in sync with the latest release, changes rarely. |

## References

[1] Burton P, Wilson R, Butters O, Ryser-Welch P, Westerberg A, Abarrategui L, Villegas-Diaz R,
  Avraam D, Marcon Y, Bishop T, Gaye A, Escribà Montagut X, Wheater S (2025). 
  _dsBaseClient: 'DataSHIELD' Client Side Base Functions_. R package version 6.3.4.

[2] Gaye A, Marcon Y, Isaeva J, LaFlamme P, Turner A, Jones E, Minion J, Boyd A, Newby C, Nuotio
  M, Wilson R, Butters O, Murtagh B, Demir I, Doiron D, Giepmans L, Wallace S, Budin-Ljøsne I,
  Oliver Schmidt C, Boffetta P, Boniol M, Bota M, Carter K, deKlerk N, Dibben C, Francis R,
  Hiekkalinna T, Hveem K, Kvaløy K, Millar S, Perry I, Peters A, Phillips C, Popham F, Raab G,
  Reischl E, Sheehan N, Waldenberger M, Perola M, van den Heuvel E, Macleod J, Knoppers B,
  Stolk R, Fortier I, Harris J, Woffenbuttel B, Murtagh M, Ferretti V, Burton P (2014).
  “DataSHIELD: taking the analysis to the data, not the data to the analysis.” _International
  Journal of Epidemiology_, *43*(6), 1929-1944. <https://doi.org/10.1093/ije/dyu188>.

[3] Wilson R, W. Butters O, Avraam D, Baker J, Tedds J, Turner A, Murtagh M, R. Burton P (2017).
  “DataSHIELD – New Directions and Dimensions.” _Data Science Journal_, *16*(21), 1-21.
  <https://doi.org/10.5334/dsj-2017-021>.

[4] Avraam D, Wilson R, Aguirre Chan N, Banerjee S, Bishop T, Butters O, Cadman T, Cederkvist L,
  Duijts L, Escribà Montagut X, Garner H, Gonçalves G, González J, Haakma S, Hartlev M,
  Hasenauer J, Huth M, Hyde E, Jaddoe V, Marcon Y, Mayrhofer M, Molnar-Gabor F, Morgan A,
  Murtagh M, Nestor M, Nybo Andersen A, Parker S, Pinot de Moira A, Schwarz F,
  Strandberg-Larsen K, Swertz M, Welten M, Wheater S, Burton P (2024). “DataSHIELD:
  mitigating disclosure risk in a multi-site federated analysis platform.” _Bioinformatics
  Advances_, *5*(1), 1-21. <https://doi.org/10.1093/bioadv/vbaf046>.

> **_Note:_** Apple Mx architecture users, please be aware that there are some numerical limitations on this platform, which leads to unexpected results when using base R packages, like stats​.
>
> x <- c(0, 3, 7)
>
> 1 - cor(x, x)​
>
> The above should result in a value of zero.
>
> _Also See:_ For more details see https://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f and the bug report: https://bugs.r-project.org/show_bug.cgi?id=18941
