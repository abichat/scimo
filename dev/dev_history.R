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

####

devtools::load_all()
devtools::document()
attachment::att_amend_desc()

usethis::use_tidy_description()

devtools::run_examples()

devtools::check()
