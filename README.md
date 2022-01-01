# gt.doc.images

Package exports a single function to create and save {gtsummary}, {gt}, and {flextable} images from package help files.

Install package with

```r
remotes::install_github("ddsjoberg/gt.doc.images")
```

Example below from the {gtsummary} package saving the example gt table images.

```r
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

The saved images can be inserted into the help files to exhibit how the rendered tables appear.
Include the following chunk in the {roxygen2} comments to insert an image.

```r
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_survfit_ex1.png}{options: width=55\%}}
```
