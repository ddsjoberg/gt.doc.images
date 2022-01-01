#' Save Rd File Images
#'
#' Only objects of class gtsummary, gt, or flextable will be saved.
#' Object names must end in `_ex` or `_ex[:digit:]`
#'
#' @param path path to package directory. Default is `here::here()`
#' @param pkg Name of package. Default is the folder name in `path=`
#' @param delete_existing_pngs Logical indicating whether to delete all
#' png files in the `~/man/figures/` folder. Default is to ask interactively
#' when `rd_files = NULL`. Otherwise, `FALSE`
#' @param rd_files Character vector of Rd file names to search. Default is all
#' Rd files. Include the `.Rd` extension in the file names.
#' @inheritParams utils::example
#'
#' @export

save_help_file_images <- function(path = here::here(),
                                  pkg = basename(path),
                                  rd_files = NULL,
                                  delete_existing_pngs =  NULL,
                                  run.dontrun = TRUE, run.donttest = TRUE) {
  # check inputs ---------------------------------------------------------------
  if (!rlang::is_empty(ls(envir = rlang::global_env()))) {
    paste("This function writes and deletes objects in the global environment,",
          "and the global environment is not empty!") %>%
      cli::cli_alert_danger()
    cli::cli_ul("Restart R and run {.code save_help_file_images()} in a fresh R session.")
    return(invisible())
  }
  if (!fs::dir_exists(path)) {
    cli::cli_alert_danger("The package path {.file {path}} does not exist.")
    return(invisible())
  }
  cli::cli_h1("{.pkg {pkg}} ({.file {path}})")
  if (!pkg %in% rownames(utils::installed.packages())) {
    cli::cli_alert_danger("Package {.pkg {pkg}} is not installed.")
    cli::cli_ul("Install the package, restart R, and try again.")
    return(invisible())
  }

  path_figures <- file.path(path, "man", "figures")
  if (!fs::dir_exists(path_figures)) {
    cli::cli_alert_danger("The package figures path {.file {path_figures}} does not exist.")
    return(invisible())
  }

  # set `delete_existing_pngs=` argument ---------------------------------------
  if (is.null(delete_existing_pngs)) {
    delete_existing_pngs <-
      ifelse(
        !(is.null(rd_files) && interactive()),
        FALSE,
        usethis::ui_yeah("Delete existing {usethis::ui_code('.png')} files?",
                         n_no = 2, shuffle = FALSE)
      )
  }

  # set list of all rd files ---------------------------------------------------
  all_rd_files <-
    list.files(file.path(path, "man")) %>%
    purrr::keep(~ str_ends(., fixed(".Rd")))
  rd_files <- rd_files %||% all_rd_files
  if (!rlang::is_empty(setdiff(rd_files, all_rd_files))) {
    paste("The following are not {.file .Rd} files in the package.",
          "{.file {setdiff(rd_files, all_rd_files)}}", sep = "\n") %>%
      cli::cli_alert_danger()
    return(invisible())
  }

  # create temp directory (example objects will be saved here)
  path_tempdir <- file.path(tempdir(), "save_help_file_images")
  fs::dir_create(path_tempdir)

  # delete existing png example images
  if (isTRUE(delete_existing_pngs)) {
    list.files(path_figures) %>%
      purrr::keep(~ (str_ends(., "_ex[:digit:]+.png") | str_ends(., "_ex.png")) &
                    !str_starts(., "README-")) %>%
      purrr::walk(~ fs::file_delete(file.path(path_figures, .x)))
  }

  # cycling over each help file, and saving the gt and flextable images
  gtsummary::set_gtsummary_theme(list("pkgwide-lgl:quiet" = TRUE))
  for (f in rd_files) {
    cli::cli_h3("Working on {.file {f}}")

    # run code from example
    suppressWarnings(
      utils::example(topic = stringr::str_remove(f, ".Rd$"),
                     package = pkg,
                     character.only = TRUE,
                     give.lines = FALSE,
                     echo = FALSE,
                     local = FALSE,
                     run.dontrun = run.dontrun,
                     run.donttest = run.donttest)
    )

    # get list of example objects that end in "_ex###"
    example_objs <-
      ls(envir = rlang::global_env())[str_ends(ls(envir = rlang::global_env()), "_ex[:digit:]+") | str_ends(ls(envir = rlang::global_env()), "_ex")]

    if (rlang::is_empty(example_objs))
      cli::cli_alert_info("No saved example objects in {.file {f}}")
    else {
      # saving an image of every gt, gtsummary, or flextable example
      purrr::walk(
        example_objs,
        function(example_chr) {
          # converting string to object
          example_obj <- eval(parse(text = example_chr))
          cli::cli_alert_success("Saving {.file {example_chr}.png}")

          # convert gtsummary object to gt
          if (inherits(example_obj, "gtsummary")) {
            example_obj <- gtsummary::as_gt(example_obj)
          }

          # checking object is now a gt object
          if (inherits(example_obj, "gt_tbl")) {
            # saving image
            gt::gtsave(example_obj,
                       filename = file.path(path_figures, str_glue("{example_chr}.png"))
            )
          }

          # saving flextable image
          if (inherits(example_obj, "flextable")) {
            flextable::save_as_image(example_obj,
                                     webshot = "webshot2",
                                     path = file.path(path_figures, str_glue("{example_chr}.png"))
            )
          }

          # shrink image
          shrink_it <-
            tryCatch(
              utils::capture.output(
                webshot::shrink(file.path(path_figures, str_glue("{example_chr}.png")))
              ),
              error = function(e) {
                return(invisible())
              }
            )
        }
      )
      rm(list = example_objs, envir = rlang::global_env())
    }
  }
  gtsummary::reset_gtsummary_theme()
  return(invisible())
}
