#' @import rlang
#' @importFrom utils globalVariables

# is there a forcats for this?
recode_data <- function(obs, prob, threshold) {
  lvl <- levels(obs)
  if (getOption("yardstick.event_first", default = TRUE)) {
    pred <- ifelse(prob >= threshold, lvl[1], lvl[2])
  } else {
    pred <- ifelse(prob >= threshold, lvl[2], lvl[1])
  }
  factor(pred, levels = lvl)
}

quote_collapse <- function(x, quote = "`", collapse = ", ") {
  paste(encodeString(x, quote = quote), collapse = collapse)
}

abort_default <- function(x, fn) {
  cls <- quote_collapse(class(x))
  msg <- paste0("No implementation of `", fn, "()` for object of class ", cls, ".")
  abort(msg)
}

# Check if a class_pred object came from an ordered factor
is_ordered_class_pred <- function(x) {
  attr(x, "ordered")
}

get_equivocal_label <- function(x) {
  attr(x, "equivocal")
}

is_ordered <- function(x) {
  UseMethod("is_ordered")
}

is_ordered.class_pred <- function(x) {
  is_ordered_class_pred(x)
}

is_ordered.default <- function(x) {
  is.ordered(x)
}
