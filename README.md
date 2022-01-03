
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/ddsjoberg/gt.doc.images/workflows/R-CMD-check/badge.svg)](https://github.com/ddsjoberg/gt.doc.images/actions)
[![gt.doc.images status
badge](https://ddsjoberg.r-universe.dev/badges/gt.doc.images)](https://ddsjoberg.r-universe.dev)
<!-- badges: end -->

# gt.doc.images

Package exports two functions

-   `save_help_file_images()`: create and save {gtsummary}, {gt}, and
    {flextable} images from package help files.

-   `shrink_help_file_images()`: shrink the help file images using
    `webshot::shrink()`. Use this function if the help files increase
    the build size of your package over the CRAN limit of 5MB

Install package from the R Universe with

``` r
install.packages('gt.doc.images', 
                 repos = c(ddsjoberg = 'https://ddsjoberg.r-universe.dev',
                           CRAN = 'https://cloud.r-project.org'))
```

Example below from the {gtsummary} package saving the example gt table
images.

``` r
> gt.doc.images::save_help_file_images()
#> 
#> -- gtsummary (C:/Users/SjobergD/GitHub/gtsummary) ------------------------------
#> 
#> -- Working on add_ci.Rd 
#> √ Saving add_ci_ex1.png
#> 
#> -- Working on add_difference.Rd 
#> √ Saving add_difference_ex1.png
#> √ Saving add_difference_ex2.png
#> 
#> -- Working on tbl_split.Rd 
#> i No saved example objects in tbl_split.Rd
```

The saved images can be inserted into the help files to exhibit how the
rendered tables appear. Include the following chunk in the {roxygen2}
comments to insert an image.

``` r
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_survfit_ex1.png}{options: width=55\%}}
```
