ggsave_and_return_path <- function(path, plt, ...) {
  ggplot2::ggsave(path, plt, ...)
  path
}