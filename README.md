
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scimo

<!-- badges: start -->

[![packageversion](https://img.shields.io/badge/version-0.0.0.9000-orange.svg)](commits/master)
<!-- badges: end -->

**scimo** provides extra recipes steps for dealing with omics data.

## Installation

You can install the **scimo** from GitHub with:

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
  update_role(-disease, new_role = "predictor") %>% 
  update_role(disease, new_role = "outcome") %>% 
  update_role(cell_line, new_role = "ID") %>% 
  step_select_cv(all_numeric_predictors(), prop_kept = 0.1) %>% 
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

juice(rec)
#> # A tibble: 108 × 1,923
#>    cell_line sex    event     disease   A1CF  AADAC AADACL3 ABCB11 ABCC12   ABRA
#>    <fct>     <fct>  <fct>     <fct>    <dbl>  <dbl>   <dbl>  <dbl>  <dbl>  <dbl>
#>  1 143B      Female Primary   Osteos… 0.0566 0.0704       0 0      0      0     
#>  2 A-673     Female Primary   Ewing … 0      0.151        0 0      0      0     
#>  3 BT-12     Female Primary   Embryo… 0.0286 3.49         0 0      0      0     
#>  4 BT-16     Male   Unknown   Embryo… 0      0.0286       0 0      0      0     
#>  5 C396      Male   Metastat… Osteos… 0      0            0 0      0      0.0144
#>  6 CADO-ES1  Female Metastat… Ewing … 0      0            0 0      0.0286 0     
#>  7 CAL-72    Male   Primary   Osteos… 0.0426 0            0 0      0.0426 0.0144
#>  8 CBAGPN    Female Primary   Ewing … 0.0976 0            0 0      0      0     
#>  9 CHLA-06   Female Unknown   Embryo… 0      0            0 0      0      0     
#> 10 CHLA-10   Female Unknown   Ewing … 0.0144 0            0 0.0144 0.0566 0.0144
#> # ℹ 98 more rows
#> # ℹ 1,913 more variables: AC002456.2 <dbl>, AC008397.1 <dbl>, ACOD1 <dbl>,
#> #   ACSM1 <dbl>, ACSM2B <dbl>, ACSM5 <dbl>, ACSM6 <dbl>, ACTBL2 <dbl>,
#> #   ACTL9 <dbl>, ACTRT1 <dbl>, ACTRT2 <dbl>, ACY3 <dbl>, ADAD1 <dbl>,
#> #   ADAD2 <dbl>, ADAM2 <dbl>, ADAM30 <dbl>, ADAMDEC1 <dbl>, ADGRE3 <dbl>,
#> #   ADGRF2 <dbl>, ADGRG4 <dbl>, ADH1B <dbl>, ADH4 <dbl>, ADIG <dbl>, AFM <dbl>,
#> #   AGBL1 <dbl>, AGRP <dbl>, AGTR2 <dbl>, AGXT <dbl>, AGXT2 <dbl>, …

tidy(rec, 1)
#> # A tibble: 19,193 × 4
#>    terms       cv kept  id             
#>    <chr>    <dbl> <lgl> <chr>          
#>  1 A1BG    0.371  FALSE select_cv_H5YSG
#>  2 A1CF    4.60   TRUE  select_cv_H5YSG
#>  3 A2M     1.69   FALSE select_cv_H5YSG
#>  4 A2ML1   2.45   FALSE select_cv_H5YSG
#>  5 A3GALT2 2.37   FALSE select_cv_H5YSG
#>  6 A4GALT  0.979  FALSE select_cv_H5YSG
#>  7 A4GNT   1.53   FALSE select_cv_H5YSG
#>  8 AAAS    0.0934 FALSE select_cv_H5YSG
#>  9 AACS    0.194  FALSE select_cv_H5YSG
#> 10 AADAC   3.40   TRUE  select_cv_H5YSG
#> # ℹ 19,183 more rows
```
