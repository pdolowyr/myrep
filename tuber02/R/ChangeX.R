#' Edit inputs inside FILEX
#'
#' Editing a value in a specified section and under a specified header inside a DSSAT FILEX.
#' Not vectorised - can currently edit only one input.
#'
#' @param X FILEX loaded using readLines, char
#' @param section section name, char
#' @param column_name header name, char
#' @param new_value new value, char of length matching number of lines under the header
#'
#' @return Edited X for further edition or to write using writeLines.
#' @export
#'
#' @examples X <- ChangeX_F(X = X, section = "FIELDS", column_name = "WSTA", new_value = "ABCD")
ChangeX_F <- function(X, section, column_name, new_value){

  new_value <- as.character(new_value)

  # define internal functions
  detect_sections_f <- function(X){
    s <- str_starts(X, "\\*")
    sname <- X[s]
    sname[str_detect(sname, "EXP.DETAILS")] <- "EXP.DETAILS"
    sname[str_detect(sname, "TREATMENTS")] <- "TREATMENTS"
    sname <- stringr::str_replace(string = sname, pattern = "\\*", replacement = "")
    snum0 <- which(s)
    snum1 <- 1 + which(s)
    snum2 <- as.integer(c(snum0[-1] - 2, length(X) - 2))
    sections <- data.frame(sname, snum0, snum1, snum2)
  }

  detect_headers_f <- function(X){
    s <- str_detect(X, "@")
  }

  detect_values_f <- function(X){
    s <- !str_detect(X, "@")
  }

  is.even <- function(x) x %% 2 == 0

  dv_replace <- function(v0, replace, new_value){
    b <- str_locate_all(v0, " ")[[1]][,1]
    fs <- (c(1:str_length(v0)) %in% b)
    d <- rle(fs)
    e <- unlist(str_split(str_squish(v0), " "))
    e[replace] <- new_value
    fs <- d$lengths[d$values] # space lengths

    sp <- character(length(fs))
    for(ii in 1:length(sp)){
      sp[ii] <- paste(rep(" ", fs[ii]), collapse = "")
    }
    # put the vectors together - e, sp
    dv <- character(length(sp) + length(e))
    dv[c(T, F)] <- sp
    dv[c(F, T)] <- e
    g <- paste(dv, collapse = "")
    return(g)
  }
  # ---

  # detect sections and save its first and last line
  sections <- detect_sections_f(X)
  s1 <- sections$snum1[sections$sname==section]
  s2 <- sections$snum2[sections$sname==section]

  # extract the section as table (dv)
  Y <- X[s1:s2]
  # detect header rows
  dheaders <- detect_headers_f(Y)
  # if multiple header rows, select the one with the column name to change
  dh <- which(str_detect(Y[dheaders], column_name))
  # detect value rows and arrange as matrix with row indices for values under separate headers in separate columns
  dvalues <- which(detect_values_f(Y))
  dvalues <- matrix(dvalues, ncol = length(which(dheaders)))
  # select the header and value rows from Y
  dv <- Y[dvalues[,dh]]
  dh <- Y[dh]

  # squish and split value rows
  dl <- list()
  for(ii in 1:length(dv)){
    a <- str_squish(dv[ii])
    a <- unlist(str_split(a, " "))
    dl <- c(dl, list(a))
  }
  dv <- matrix(c(unlist(dl)), nrow = length(dl), byrow = TRUE)

  # squish and split header rows
  dl <- list()
  for(ii in 1:length(dh)){
    a <- str_squish(dh[ii])
    a <- unlist(str_split(a, " "))
    dl <- c(dl, list(a))
  }
  #dh <- matrix(c(unlist(dl)), nrow = length(dl), byrow = TRUE) # backup
  dh <- c(unlist(dl))

  # old_value to be found and replaced in the relevant field
  cn <- which(str_detect(dh, column_name))
  if(length(cn)>1){
    stop("ChangeX_F says: X-file column name detected in more than 1 place.")
  }
  old_value <- dv[,cn]

  jj <- as.numeric(s1 + dvalues - 1)
  for(ii in 1:length(new_value)){
    X[jj][ii] <- dv_replace(X[jj][ii], cn, new_value[ii])
  }
  return(X)
}
