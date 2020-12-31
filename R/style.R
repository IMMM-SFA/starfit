#' @importFrom clisymbols symbol
#' @importFrom crayon green
done <- function(...) {
  bullet(paste0(...), bullet = green(symbol$tick))
}

#' @importFrom clisymbols symbol
#' @importFrom crayon blue
info <- function(...) {
  bullet(paste0(...), bullet = blue(symbol$info))
}

#' @importFrom clisymbols symbol
#' @importFrom crayon red
problem <- function(...) {
  bullet(paste0(...), bullet = red(symbol$warning))
}

bullet <- function(lines, bullet) {
  lines <- paste0(bullet, " ", lines)
  cat_line(lines)
}

cat_line <- function(...) {
  cat(..., "\n", sep = "")
}
