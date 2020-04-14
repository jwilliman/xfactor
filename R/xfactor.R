#' Manipulate factor levels using regex matching.
#'
#' Reorder, add to, drop, and/or collapse factor levels.
#'
#' @param x A character vector or factor. Usually taking a small number of
#'   distinct values.
#' @param levels An optional vector of the unique regular expressions (as
#'   character strings) to be passed to the \code{pattern} argument of
#'   \code{\link{grepl}} or \code{\link{gsub}}. Missing values are allowed.
#' @param labels An optional character vector of labels for the levels (in the
#'   same order as levels) For fixed = FALSE this can include backreferences
#'   \code{"\1"} to \code{"\9"} to parenthesized subexpressions of pattern. For perl =
#'   TRUE only, it can also contain \code{"\U"} or \code{"\L"} to convert the rest of the
#'   replacement to upper or lower case and \code{"\E"} to end case conversion. If a
#'   character vector of length 2 or more is supplied, the first element is used
#'   with a warning. If NA, all elements in the result corresponding to matches
#'   will be set to NA.
#' @param exclude A vector of regex expressions used to exclude values when
#'   forming the set of levels. Setting \code{exclude = TRUE}, causes all values
#'   not listed in the level or label statements to be excluded.
#' @param reorder Sets the final ordering of the factor levels. Can be a
#'   function (or formula), integer, or character variable. Functions (formula)
#'   are passed to the ... argument of `fct_relevel`. Integers are passed to the
#'   `after` argument of `fct_relevel`. Character variables should match one of
#'   the \code{forcats} functions for sorting (`"fct_inorder"`, `fct_infreq`,
#'   `fct_inseq`, `fct_rev`, `fct_shuffle`).
#' @param type Controls whether grepl (type = "match") or gsub (type =
#'   "substitute") is used.
#' @param ... Other arguments passed to \code{\link{grepl}} or
#'   \code{\link{gsub}}, such as ignore.case, perl, fixed.
#'
#' @details The encoding of the vector happens as follows. First any values
#'   matching \code{regex} expressions in \code{exclude} are removed from
#'   \code{x}. If \code{levels} is a named vector, or \code{labels} is provided,
#'   existing values for \code{x} are replaced or altered accordingly.
#'
#'   All \code{labels} are added as factor levels even if no matching values are
#'   found. If \code{levels} is unnamed and \code{labels} is not provided,
#'   character strings listed in \code{levels} are moved and/or added to the
#'   beginning of the final factor levels. If \code{exclude = TRUE} all values
#'   not listed in \code{levels} or \code{labels} are dropped.
#'
#'   By default, factor levels are returned in the order of the \code{levels}
#'   (or \code{labels}) statement. Any values of x that do not match
#'   \code{levels} are added to the end in their previous order (if \code{x} is
#'   a factor) or in alphabetical order (if \code{x} is a character).
#'   
#'   Level ordering can be controlled further using the \code{reorder} argument. 
#'   
#'   
#'
#' @return \code{xfactor} returns an object of class \code{"factor"} which has a
#'   set of integer codes the length of x with a \code{"levels"} attribute of
#'   mode \code{\link{character}} and unique \code{(!\link{anyDuplicated}(.))}
#'   entries.
#'
#' @export
#' @examples
#'
#' x <- c("Man", "Male", "Man", "Lady", "Female")
#' ## Map from 4 different values to only two levels:
#' xfactor(x, levels = c("Male", "Man" , "Lady", "Female"),
#'            labels = c("Male", "Male", "Female", "Female"))
#' xfactor(x, levels = c("^M", "L|F"),
#'            labels = c("Male", "Female"))
#' xfactor(x, levels = c(Male = "^M", Female = "L|F"))
#'
#'
#' 
xfactor <- function(x, levels = NULL, labels = NULL, exclude = FALSE, reorder = NULL, type = "match", ...) {
  
  # Coerce to factor if not already
  x <- check_factor(x)
  # levels_old <- levels(x)
  
  # Check if labels argument is provided, or if not, if levels argument is named
  if (is.null(labels)) {
    if (!is.null(names(levels))) {
      labels <- names(levels)
      labels[labels == ""] <- levels[labels == ""]
    } 
  }

  ## Drop levels matching regex pattern(s) in exclude
  if (is.character(exclude)) {
    levels(x)[grepl(paste(exclude, collapse = "|"), levels(x), ...)] <- NA
  }
  
  # Replace any levels that match patterns in labels or names(levels)
  if (!is.null(levels) & !is.null(labels)) {
    
    # Check if length of labels matches length of levels
    if (length(labels) != length(levels))
      stop(paste("invalid 'labels'; length", length(labels), "should be", length(levels), sep = " "))
    
    
    # Create data.frame recording matches between old levels and patterns
    dat_match <- check_patterns(x, levels, ...)
    if (any(dat_match$duplicates))
      warning(
        "Multiple matches for: ", 
        paste(dat_match$level_old[dat_match$duplicates], collapse = ", ")
        , ". First match taken.", call. = FALSE)
    dat_match <- dat_match[!dat_match$duplicates,]
    
    
    dat_match$replacement <- labels[dat_match$pattern_id]
    
    
    if (grepl(type, "match")) {
      levels(x)[dat_match$level_id] <- labels[dat_match$pattern_id]      
    } else if (grepl(type, "substitute")) {
      levels(x)[dat_match$level_id] <- sapply(seq(nrow(dat_match)), function(i)
        gsub(pattern = dat_match[i, "pattern"], 
             replacement = dat_match[i, "replacement"], 
             x = dat_match[i, "level_old"], ...))
    }

  }

  ## Add any extra levels not in labels 
  if(is.null(labels))
    labels <- levels
  levels(x) <- c(levels(x), setdiff(labels, levels(x)))
  
  # return(list(x, levels(x), levels, labels))}
  
  ## Drop levels not included in levels argument
  if (isTRUE(exclude)) {
    levels(x)[!levels(x) %in% labels] <- NA
  }  
  
  ## Reorder factor levels
  if (is.null(reorder)) {
    if(!is.null(labels)) {
      levels_new <- unique(c(labels, setdiff(levels(x), labels)))
      x <- factor(x, levels = levels_new)
    }
  }
  else if (is.numeric(reorder)) {
    x <- forcats::fct_relevel(x, labels, after = reorder)
  } else if (is.function(reorder)) {
    x <- forcats::fct_relevel(x, reorder)
  } else if (is.character(reorder) & substr(reorder, 1, 3) == "fct") {
      eval(parse(text = paste0("forcats::", reorder, "(x)")))
  }
   
return(x)
  
}



#' Check patterns
#'
#' @param f A facter
#' @param patterns A character vector of regex patterns to be check against the factors levels.
#' @param ... 
#'
#' @return
#' @export
#'
check_patterns <- function(f, patterns, ...) {
  
  levels <- levels(f)
  
  match_matrix <- sapply(patterns, function(pattern)
    sapply(levels, grepl, pattern = pattern, ...))
  
  match_ids  <- which(match_matrix)
  nlevels    <- length(levels)
  match_rows <- match_ids %% nlevels
  match_rows[match_rows == 0] <- nlevels
  match_dups <- duplicated(match_rows)
  match_cols <- ceiling(match_ids/nlevels)
  
  dat <- data.frame(
    level_id    = match_rows, 
    level_old   = levels[match_rows],
    pattern_id  = match_cols,
    pattern     = patterns[match_cols],
    duplicates  = match_dups,
    stringsAsFactors = FALSE)
  
  dat[order(dat$level_id),]
  
}


