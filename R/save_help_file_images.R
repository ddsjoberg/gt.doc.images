#' Save Rd File Images
#'
#' Only objects of class gtsummary, gt, or flextable will be saved.
#' Object names must end in `_ex` or `_ex[:digit:]`
#'
#' @param delete_existing_pngs Logical
#' @param pkg Name of package
#' @param rd_files Character vector of Rd file names to search. Default is all
#' Rd files.
#' @inheritParams utils::example
#'
#' @export

save_help_file_images <- function(pkg, rd_files = NULL,
                                  delete_existing_pngs =  FALSE,
                                  run.dontrun = TRUE, run.donttest = TRUE) {
  # list of all help files
  if (is.null(rd_files)) {
    rd_files <-
      list.files(here::here("man")) %>%
      purrr::keep(~ stringr::str_ends(., stringr::fixed(".Rd"))) %>%
      stringr::str_remove(".Rd")
  }

  # create temp directory (example objects will be saved here)
  path_tempdir <- file.path(tempdir(), "save_help_file_images")
  fs::dir_create(path_tempdir)

  # delete existing png example images
  if (isTRUE(delete_existing_pngs)) {
    list.files(here::here("man", "figures")) %>%
      purrr::keep(~ (stringr::str_ends(., "_ex[:digit:]+.png") | stringr::str_ends(., "_ex.png")) &
                    !stringr::str_starts(., "README-")) %>%
      purrr::walk(~ fs::file_delete(here::here("man", "figures", .x)))
  }

  # cycling over each help file, and saving gt images
  gtsummary::set_gtsummary_theme(list("pkgwide-lgl:quiet" = TRUE))
  for (f in rd_files) {
    cli::cli_alert_success("Working on {.field {f}}")

    # run code from example
    suppressWarnings(
      utils::example(topic = f, package = pkg, character.only = TRUE,
                     give.lines = FALSE, echo = FALSE, local = FALSE,
                     run.dontrun = run.dontrun, run.donttest = run.donttest)
    )

    # get list of example objects that end in "_ex###"
    example_objs <-
      ls(envir = rlang::global_env())[stringr::str_ends(ls(envir = rlang::global_env()), "_ex[:digit:]+") | stringr::str_ends(ls(envir = rlang::global_env()), "_ex")]

    if (rlang::is_empty(example_objs))
      cli::cli_alert_success("  No Example Objects Present in {.field {f}}")
    else {
      # saving an image of every gt, gtsummary, or flextable example
      purrr::walk(
        example_objs,
        function(example_chr) {
          # converting string to object
          example_obj <- eval(parse(text = example_chr))
          cli::cli_alert_success("  Saving '{example_chr}.png'")

          # convert gtsummary object to gt
          if (inherits(example_obj, "gtsummary")) {
            example_obj <- gtsummary::as_gt(example_obj)
          }

          # checking object is now a gt object
          if (inherits(example_obj, "gt_tbl")) {
            # saving image
            gt::gtsave(example_obj,
                       filename = here::here("man", "figures", stringr::str_glue("{example_chr}.png"))
            )
          }

          # saving flextable image
          if (inherits(example_obj, "flextable")) {
            flextable::save_as_image(example_obj,
                                     webshot = "webshot2",
                                     path = here::here("man", "figures", stringr::str_glue("{example_chr}.png"))
            )
          }

          # shrink image
          shrink_it <-
            tryCatch(
              utils::capture.output(
                webshot::shrink(here::here("man", "figures", stringr::str_glue("{example_chr}.png")))
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
