#' Shrink Rd File Images
#'
#' @inheritParams save_help_file_images
#' @param image_files Character vector of .png file names to shrink. Default is all
#' png files in the `~/man/figures` folder that end with `_ex[digits].png`
#' or `_ex.png` that do not begin with `README-`.
#'
#' @export

shrink_help_file_images <- function(path = here::here(),
                                    pkg = basename(path),
                                    image_files = NULL,
                                    path.output = file.path(path, "man", "figures")) {
  # check inputs ---------------------------------------------------------------
  if (!fs::dir_exists(path)) {
    cli::cli_alert_danger("The package path {.file {path}} does not exist.")
    return(invisible())
  }
  cli::cli_h1("{.pkg {pkg}} ({.file {path}})")

  if (!fs::dir_exists(path.output)) {
    cli::cli_alert_danger("The package figures path {.file {path.output}} does not exist.")
    return(invisible())
  }

  # set list of all image files ------------------------------------------------
  all_image_files <-
    list.files(path.output, pattern = "_ex[[:digit:]]+.png$") %>%
    union(list.files(path.output, pattern = "_ex.png$")) %>%
    purrr::discard(~startsWith(., "README-"))
  image_files <- image_files %||% all_image_files
  if (!rlang::is_empty(setdiff(image_files, all_image_files))) {
    paste("The following are not {.file .png} files in the package.",
          "{.file {setdiff(image_files, all_image_files)}}", sep = "\n") %>%
      cli::cli_alert_danger()
    return(invisible())
  }

  # shrink images
  shrink_size_all <- c()
  original_size_all <- c()
  for (f in image_files) {
    cli::cli_h3("Working on {.file {f}}")

    original_size <- file.size(file.path(path.output, f))
    original_size_all <- c(original_size_all, original_size)
    tryCatch(
      utils::capture.output(
        webshot::shrink(file.path(path.output, f))
      )
      ,
      error = function(e) {
        return(invisible())
      }
    )
    shrink_size <- file.size(file.path(path.output, f))
    shrink_size_all <- c(shrink_size_all, shrink_size)
    paste("Size reduction {pretty_bytes(original_size - shrink_size)}",
          "({pretty_bytes(original_size)} -> {pretty_bytes(shrink_size)})") %>%
    cli::cli_alert_success()

  }

  cli::cli_h1("Summary")
  paste("Total size reduction {pretty_bytes(sum(original_size_all - shrink_size_all))}",
        "({pretty_bytes(sum(original_size_all))} -> {pretty_bytes(sum(shrink_size_all))})") %>%
    cli::cli_alert_success()
}
