
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scimo <a href="https://abichat.github.io/scimo/"><img src="man/figures/logo.png" align="right" height="138" alt="scimo website" /></a>

<!-- badges: start -->

![packageversion](https://img.shields.io/badge/version-0.0.2-orange.svg)
[![R-CMD-check](https://github.com/abichat/scimo/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/abichat/scimo/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/scimo)](https://CRAN.R-project.org/package=scimo)
<!-- badges: end -->

**scimo** provides extra recipes steps for dealing with omics data,
while also being adaptable to other data types.

## Installation

You can install **scimo** from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("abichat/scimo")
```

## Example

The `cheese_abundance` dataset describes fungal community abundance of
74 Amplicon Sequences Variants (ASVs) sampled from the surface of three
different French cheeses.

``` r
library(scimo)
data("cheese_abundance", "cheese_taxonomy")

cheese_abundance
#> # A tibble: 9 × 77
#>   sample    cheese    rind_type asv_01 asv_02 asv_03 asv_04 asv_05 asv_06 asv_07
#>   <chr>     <chr>     <chr>      <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 sample1-1 Saint-Ne… Natural        1      0     38     40      1      2     31
#> 2 sample1-2 Saint-Ne… Natural        3      4     38     61      4      4     48
#> 3 sample1-3 Saint-Ne… Natural       28     16     33     23     31     29     21
#> 4 sample2-1 Livarot   Washed         0      2      1      0      5      1      0
#> 5 sample2-2 Livarot   Washed         0      0      4      0      1      1      2
#> 6 sample2-3 Livarot   Washed         0      1      2      0      2      1      0
#> 7 sample3-1 Epoisses  Washed         4      2      3      0      2      5      0
#> 8 sample3-2 Epoisses  Washed         0      0      0      0      0      0      0
#> 9 sample3-3 Epoisses  Washed         0      0      1      0      0      0      2
#> # ℹ 67 more variables: asv_08 <dbl>, asv_09 <dbl>, asv_10 <dbl>, asv_11 <dbl>,
#> #   asv_12 <dbl>, asv_13 <dbl>, asv_14 <dbl>, asv_15 <dbl>, asv_16 <dbl>,
#> #   asv_17 <dbl>, asv_18 <dbl>, asv_19 <dbl>, asv_20 <dbl>, asv_21 <dbl>,
#> #   asv_22 <dbl>, asv_23 <dbl>, asv_24 <dbl>, asv_25 <dbl>, asv_26 <dbl>,
#> #   asv_27 <dbl>, asv_28 <dbl>, asv_29 <dbl>, asv_30 <dbl>, asv_31 <dbl>,
#> #   asv_32 <dbl>, asv_33 <dbl>, asv_34 <dbl>, asv_35 <dbl>, asv_36 <dbl>,
#> #   asv_37 <dbl>, asv_38 <dbl>, asv_39 <dbl>, asv_40 <dbl>, asv_41 <dbl>, …

glimpse(cheese_taxonomy)
#> Rows: 74
#> Columns: 9
#> $ asv     <chr> "asv_01", "asv_02", "asv_03", "asv_04", "asv_05", "asv_06", "a…
#> $ lineage <chr> "k__Fungi|p__Ascomycota|c__Dothideomycetes|o__Dothideales|f__D…
#> $ kingdom <chr> "Fungi", "Fungi", "Fungi", "Fungi", "Fungi", "Fungi", "Fungi",…
#> $ phylum  <chr> "Ascomycota", "Ascomycota", "Ascomycota", "Ascomycota", "Ascom…
#> $ class   <chr> "Dothideomycetes", "Eurotiomycetes", "Eurotiomycetes", "Euroti…
#> $ order   <chr> "Dothideales", "Eurotiales", "Eurotiales", "Eurotiales", "Euro…
#> $ family  <chr> "Dothioraceae", "Aspergillaceae", "Aspergillaceae", "Aspergill…
#> $ genus   <chr> "Aureobasidium", "Aspergillus", "Penicillium", "Penicillium", …
#> $ species <chr> "Aureobasidium Group pullulans", "Aspergillus fumigatus", "Pen…
```

``` r
list_family <- split(cheese_taxonomy$asv, cheese_taxonomy$family)
head(list_family, 2)
#> $Aspergillaceae
#> [1] "asv_02" "asv_03" "asv_04" "asv_05" "asv_06" "asv_07" "asv_08" "asv_09"
#> 
#> $Debaryomycetaceae
#>  [1] "asv_10" "asv_11" "asv_12" "asv_13" "asv_14" "asv_15" "asv_16" "asv_17"
#>  [9] "asv_18" "asv_19" "asv_20" "asv_21" "asv_22"
```

The following recipe will

1.  aggregate the ASV variables at the family level, as defined by
    `list_family`;
2.  transform counts into proportions;
3.  discard variables those p-values are above 0.05 with a
    Kruskal-Wallis test against `cheese`.

``` r
rec <-
  recipe(cheese ~ ., data = cheese_abundance) %>% 
  step_aggregate_list(all_numeric_predictors(),
                      list_agg = list_family, fun_agg = sum) %>%
  step_rownormalize_tss(all_numeric_predictors()) %>% 
  step_select_kruskal(all_numeric_predictors(), 
                      outcome = "cheese", cutoff = 0.05) %>%
  prep()

rec
#> 
#> ── Recipe ──────────────────────────────────────────────────────────────────────
#> 
#> ── Inputs
#> Number of variables by role
#> outcome:    1
#> predictor: 76
#> 
#> ── Training information
#> Training data contained 9 data points and no incomplete rows.
#> 
#> ── Operations
#> • Aggregation of: asv_01, asv_02, asv_03, asv_04, asv_05, ... | Trained
#> • TSS normalization on: Aspergillaceae and Debaryomycetaceae, ... | Trained
#> • Kruskal filtering against cheese on: Aspergillaceae, ... | Trained

bake(rec, new_data = NULL)
#> # A tibble: 9 × 8
#>   sample    rind_type cheese  Debaryomycetaceae Dipodascaceae Saccharomycetaceae
#>   <fct>     <fct>     <fct>               <dbl>         <dbl>              <dbl>
#> 1 sample1-1 Natural   Saint-…            0.719         0.0684           0.113   
#> 2 sample1-2 Natural   Saint-…            0.715         0.0725           0.119   
#> 3 sample1-3 Natural   Saint-…            0.547         0.277            0.0938  
#> 4 sample2-1 Washed    Livarot            0.153         0.845            0.000854
#> 5 sample2-2 Washed    Livarot            0.150         0.848            0.00106 
#> 6 sample2-3 Washed    Livarot            0.160         0.837            0.00108 
#> 7 sample3-1 Washed    Epoiss…            0.0513        0.944            0.00327 
#> 8 sample3-2 Washed    Epoiss…            0.0558        0.941            0.00321 
#> 9 sample3-3 Washed    Epoiss…            0.0547        0.942            0.00329 
#> # ℹ 2 more variables: `Saccharomycetales fam Incertae sedis` <dbl>,
#> #   Trichosporonaceae <dbl>
```

To see which variables are kept and the associated p-values, you can use
the `tidy` method on the third step:

``` r
tidy(rec, 3)
#> # A tibble: 13 × 4
#>    terms                                    pv kept  id                  
#>    <chr>                                 <dbl> <lgl> <chr>               
#>  1 Aspergillaceae                       0.0608 FALSE select_kruskal_WKayj
#>  2 Debaryomycetaceae                    0.0273 TRUE  select_kruskal_WKayj
#>  3 Dipodascaceae                        0.0273 TRUE  select_kruskal_WKayj
#>  4 Dothioraceae                         0.101  FALSE select_kruskal_WKayj
#>  5 Lichtheimiaceae                      0.276  FALSE select_kruskal_WKayj
#>  6 Metschnikowiaceae                    0.0509 FALSE select_kruskal_WKayj
#>  7 Mucoraceae                           0.0608 FALSE select_kruskal_WKayj
#>  8 Phaffomycetaceae                     0.0794 FALSE select_kruskal_WKayj
#>  9 Saccharomycetaceae                   0.0273 TRUE  select_kruskal_WKayj
#> 10 Saccharomycetales fam Incertae sedis 0.0221 TRUE  select_kruskal_WKayj
#> 11 Trichomonascaceae                    0.0625 FALSE select_kruskal_WKayj
#> 12 Trichosporonaceae                    0.0273 TRUE  select_kruskal_WKayj
#> 13 Wickerhamomyceteae                   0.177  FALSE select_kruskal_WKayj
```

## Notes

### `protection stack overflow` error

If you have a very large dataset, you may encounter this error:

``` r
data("pedcan_expression")
recipe(disease ~ ., data = pedcan_expression) %>% 
    step_select_cv(all_numeric_predictors(), prop_kept = 0.1) 
#> Error: protect(): protection stack overflow
```

It is linked to [how **R** handles many variables in
formulas](https://github.com/tidymodels/recipes/issues/467). To solve
it, pass only the dataset to `recipe()` and manually update roles with
`update_role()`, like in the example below:

``` r
recipe(pedcan_expression) %>% 
  update_role(disease, new_role = "outcome") %>% 
  update_role(-disease, new_role = "predictor") %>% 
  step_select_cv(all_numeric_predictors(), prop_kept = 0.1) 
#> 
#> ── Recipe ──────────────────────────────────────────────────────────────────────
#> 
#> ── Inputs
#> Number of variables by role
#> outcome:       1
#> predictor: 19196
#> 
#> ── Operations
#> • Top CV filtering on: all_numeric_predictors()
```

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

### Dependencies

**scimo** doesn’t introduce any additional dependencies compared to
**recipes**.
