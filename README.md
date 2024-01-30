
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scimo

<!-- badges: start -->

[![experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![packageversion](https://img.shields.io/badge/version-0.0.0.9000-orange.svg)](commits/master)
<!-- badges: end -->

**scimo** provides extra recipes steps for dealing with omics data, but
not only.

## Installation

You can install **scimo** from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("abichat/scimo")
```

## Example

``` r
library(scimo)

data("pedcan_expression")
pedcan_expression
#> # A tibble: 108 × 19,197
#>    cell_line sex   event disease  A1BG   A1CF   A2M  A2ML1 A3GALT2 A4GALT  A4GNT
#>    <chr>     <chr> <chr> <chr>   <dbl>  <dbl> <dbl>  <dbl>   <dbl>  <dbl>  <dbl>
#>  1 143B      Fema… Prim… Osteos…  3.02 0.0566 2.78  0       0       2.13  0     
#>  2 A-673     Fema… Prim… Ewing …  4.87 0      2.00  3.19    0.0841  4.62  0.189 
#>  3 BT-12     Fema… Prim… Embryo…  3.52 0.0286 0.111 0       0       2.32  0.0704
#>  4 BT-16     Male  Unkn… Embryo…  3.51 0      0.433 0.0144  0       1.54  0.0144
#>  5 C396      Male  Meta… Osteos…  4.59 0      0.956 0       0       5.10  0     
#>  6 CADO-ES1  Fema… Meta… Ewing …  5.89 0      0.614 0.379   0.0704  6.60  0.151 
#>  7 CAL-72    Male  Prim… Osteos…  4.35 0.0426 0.333 0       0       0.614 0     
#>  8 CBAGPN    Fema… Prim… Ewing …  4.87 0.0976 1.33  0.111   0       0.722 0.0704
#>  9 CHLA-06   Fema… Unkn… Embryo…  5.05 0      0.124 0       0       0.848 0.138 
#> 10 CHLA-10   Fema… Unkn… Ewing …  5.05 0.0144 0.949 1.73    0.0704  0.506 0.0704
#> # ℹ 98 more rows
#> # ℹ 19,186 more variables: AAAS <dbl>, AACS <dbl>, AADAC <dbl>, AADACL2 <dbl>,
#> #   AADACL3 <dbl>, AADACL4 <dbl>, AADAT <dbl>, AAGAB <dbl>, AAK1 <dbl>,
#> #   AAMDC <dbl>, AAMP <dbl>, AANAT <dbl>, AAR2 <dbl>, AARD <dbl>, AARS1 <dbl>,
#> #   AARS2 <dbl>, AARSD1 <dbl>, AASDH <dbl>, AASDHPPT <dbl>, AASS <dbl>,
#> #   AATF <dbl>, AATK <dbl>, ABAT <dbl>, ABCA1 <dbl>, ABCA10 <dbl>,
#> #   ABCA12 <dbl>, ABCA13 <dbl>, ABCA2 <dbl>, ABCA3 <dbl>, ABCA4 <dbl>, …
```

``` r
rec <-
  recipe(pedcan_expression) %>% 
  update_role(disease, new_role = "outcome") %>% 
  update_role(-disease, new_role = "predictor") %>% 
  update_role(cell_line, new_role = "ID") %>% 
  step_select_cv(all_numeric_predictors(), n_kept = 3000) %>% 
  step_select_kruskal(all_numeric_predictors(), cutoff = 0.05,
                      outcome = "disease", correction = "fdr") %>% 
  prep()

rec
#> 
#> ── Recipe ──────────────────────────────────────────────────────────────────────
#> 
#> ── Inputs
#> Number of variables by role
#> outcome:       1
#> predictor: 19195
#> ID:            1
#> 
#> ── Training information
#> Training data contained 108 data points and no incomplete rows.
#> 
#> ── Operations
#> • Top CV filtering on: A1BG, A1CF, A2M, A2ML1, A3GALT2, A4GALT, ... | Trained
#> • Kruskal filtering against disease on: A1CF, AADAC, AADACL2, ... | Trained

juice(rec)
#> # A tibble: 108 × 928
#>    cell_line sex    event      disease AADACL2 AADACL4 ABCB11 AC008770.4    ACAN
#>    <fct>     <fct>  <fct>      <fct>     <dbl>   <dbl>  <dbl>      <dbl>   <dbl>
#>  1 143B      Female Primary    Osteos…  0       0      0          0       0.0286
#>  2 A-673     Female Primary    Ewing …  0.0286  0.856  0          0       0.0566
#>  3 BT-12     Female Primary    Embryo…  0.0144  0      0          0       0     
#>  4 BT-16     Male   Unknown    Embryo…  0       0      0          0       0     
#>  5 C396      Male   Metastatic Osteos…  0       0      0          0      10.1   
#>  6 CADO-ES1  Female Metastatic Ewing …  0.0144  0      0          0       0     
#>  7 CAL-72    Male   Primary    Osteos…  0       0      0          0       0.111 
#>  8 CBAGPN    Female Primary    Ewing …  0       0.0976 0          0       0     
#>  9 CHLA-06   Female Unknown    Embryo…  0       0      0          0       0     
#> 10 CHLA-10   Female Unknown    Ewing …  0       0.239  0.0144     0.0426  0     
#> # ℹ 98 more rows
#> # ℹ 919 more variables: ACCSL <dbl>, ACOT12 <dbl>, ACP7 <dbl>, ACSM4 <dbl>,
#> #   ACSM5 <dbl>, ACTBL2 <dbl>, ACY3 <dbl>, ADAM33 <dbl>, ADAMDEC1 <dbl>,
#> #   ADCY8 <dbl>, ADGRD2 <dbl>, ADGRF5 <dbl>, ADGRG7 <dbl>, ADGRL4 <dbl>,
#> #   AGTR2 <dbl>, AICDA <dbl>, AKAIN1 <dbl>, AKAP4 <dbl>, AL160269.1 <dbl>,
#> #   AL445238.1 <dbl>, ALLC <dbl>, ALOX15B <dbl>, AMTN <dbl>, ANKRD30B <dbl>,
#> #   ANKRD30BL <dbl>, ANKRD35 <dbl>, ANXA10 <dbl>, ANXA13 <dbl>, AOAH <dbl>, …

tidy(rec, 1)
#> # A tibble: 19,193 × 4
#>    terms       cv kept  id             
#>    <chr>    <dbl> <lgl> <chr>          
#>  1 A1BG    0.371  FALSE select_cv_Jr5WU
#>  2 A1CF    4.60   TRUE  select_cv_Jr5WU
#>  3 A2M     1.69   FALSE select_cv_Jr5WU
#>  4 A2ML1   2.45   FALSE select_cv_Jr5WU
#>  5 A3GALT2 2.37   FALSE select_cv_Jr5WU
#>  6 A4GALT  0.979  FALSE select_cv_Jr5WU
#>  7 A4GNT   1.53   FALSE select_cv_Jr5WU
#>  8 AAAS    0.0934 FALSE select_cv_Jr5WU
#>  9 AACS    0.194  FALSE select_cv_Jr5WU
#> 10 AADAC   3.40   TRUE  select_cv_Jr5WU
#> # ℹ 19,183 more rows
tidy(rec, 2)
#> # A tibble: 3,000 × 5
#>    terms         pv            qv kept  id                  
#>    <chr>      <dbl>         <dbl> <lgl> <chr>               
#>  1 A1CF    9.70e- 1 0.975         FALSE select_kruskal_WKayj
#>  2 AADAC   1.84e- 1 0.320         FALSE select_kruskal_WKayj
#>  3 AADACL2 3.45e- 4 0.00255       TRUE  select_kruskal_WKayj
#>  4 AADACL3 7.58e- 1 0.799         FALSE select_kruskal_WKayj
#>  5 AADACL4 5.75e-11 0.00000000821 TRUE  select_kruskal_WKayj
#>  6 ABCB11  1.07e- 5 0.000156      TRUE  select_kruskal_WKayj
#>  7 ABCB5   3.05e- 2 0.0854        FALSE select_kruskal_WKayj
#>  8 ABCC12  8.79e- 2 0.187         FALSE select_kruskal_WKayj
#>  9 ABO     3.58e- 1 0.498         FALSE select_kruskal_WKayj
#> 10 ABRA    5.85e- 2 0.138         FALSE select_kruskal_WKayj
#> # ℹ 2,990 more rows
```

## Notes

### `protection stack overflow` error

If you have a very large dataset, you may encounter this error:

``` r
recipe(disease ~ ., data = pedcan_expression) %>% 
    step_select_cv(all_numeric_predictors(), prop_kept = 0.1) 
#> Error: protect(): protection stack overflow
```

It is linked to [how **R** handles many variables in
formulas](https://github.com/tidymodels/recipes/issues/467). To solve
it, pass only the dataset to `recipe()` and manually update roles with
`update_role()`, like in the example above.

### Steps for variable selection

Like [**colino**](https://github.com/stevenpawley/colino), **scimo**
proposes 3 arguments for variable selection steps based on a statistic:
`n_kept`, `prop_kept` and `cutoff`.

- `n_kept` and `prop_kept` deal with how many variables will be kept in
  the preprocessed dataset, based on an exact count of variables or a
  proportion relative to the original dataset. They are mutually
  exclusive.

- `cutoff` removes variables whose statistic is below (or above,
  depending on the step) it. It could be used alone or in addition to
  the two others.
