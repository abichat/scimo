library(devtools)
library(testthat)

# use_build_ignore("dev/")

# use_gpl3_license()

# use_news_md()

# use_readme_rmd()

# use_git()

# use_github_links()

# use_package_doc()
# use_pipe()


# use_r("select_cv")
# use_r("to_keep")

# use_data_raw("pedcan_expression")
# use_r("data")
# use_tibble()

# use_testthat()
# use_test("to_keep")

# use_test("select_cv")

# use_r("select_wilcoxon")
# use_test("select_wilcoxon")

# use_r("checks")
# use_test("checks")

# use_r("select_kruskal")
# use_test("select_kruskal")

# use_r("select_background")
# use_test("select_background")

# use_r("aggregate")
# use_test("aggregate")

# use_r("aggregate_list")

# use_github_action("check-standard")

# use_vignette("pedcan")

# use_pkgdown()

# use_data_raw("cheese_abundance")

# use_r("rownormalize_tss")
# use_test("rownormalize_tss")

# use_test("aggregate_list")

# use_r("aggregate_hclust")
# use_test("aggregate_hclust")

# use_github_action("check-release", save_as = "R-CMD-check-prdev.yaml")
# use_github_action("pkgdown")


####

devtools::load_all()
devtools::document()
attachment::att_amend_desc()

usethis::use_tidy_description()

devtools::run_examples()
devtools::test()

devtools::check()
goodpractice::goodpractice()

####

devtools::install(upgrade = "never")
rmarkdown::render("README.Rmd"); file.remove("README.html")
devtools::install(upgrade = "never")


# pkgdown::template_reference()
pkgdown::check_pkgdown()
pkgdown::build_site()
unlink("docs/", recursive = TRUE)

