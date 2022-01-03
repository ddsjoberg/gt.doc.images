
shrink_help_file_images <- function(path = here::here(),
                                    pkg = basename(path),
                                    image_files = NULL) {
  # check inputs ---------------------------------------------------------------
  if (!fs::dir_exists(path)) {
    cli::cli_alert_danger("The package path {.file {path}} does not exist.")
    return(invisible())
  }
  cli::cli_h1("{.pkg {pkg}} ({.file {path}})")

  path_figures <- file.path(path, "man", "figures")
  if (!fs::dir_exists(path_figures)) {
    cli::cli_alert_danger("The package figures path {.file {path_figures}} does not exist.")
    return(invisible())
  }

  # set list of all image files ------------------------------------------------
  all_image_files <-
    list.files(path_figures, pattern = "_ex[[:digit:]]+.png$") %>%
    union(list.files(path_figures, pattern = "_ex.png$")) %>%
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

    original_size <- file.size(file.path(path_figures, f))
    original_size_all <- c(original_size_all, original_size)
    tryCatch(
      utils::capture.output(
        webshot::shrink(file.path(path_figures, f))
      )
      ,
      error = function(e) {
        return(invisible())
      }
    )
    shrink_size <- file.size(file.path(path_figures, f))
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
