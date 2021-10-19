# codec -------------------------------------------------------------------------
#' Estimate conditional dependence coefficient (CODEC) for categorical observation
#'
#' The conditional dependence coefficient (CODEC) is a measure of the amount of conditional dependence between
#' a random variable Y and a random vector Z given a random vector X, based on an i.i.d. sample of (Y, Z, X).
#' The coefficient can take any real value in a finite sample, but is asymptotically guaranteed to be between 0 and 1.
#'
#' @param Y Categorical or discrete value vector (length n)
#' @param Z Matrix (n by q)
#' @param X Matrix (n by p), default is NULL
#' @param na.rm Remove NAs if TRUE
#' @param  type a factor variable indicating whether X and Z are continuous vectors or discrete, the
#' default value is continuous
#' @details The value returned by codec can be positive or negative. Asymptotically, it is guaranteed
#' to be between 0 and 1. A small value indicates low conditional dependence between Y and Z given X, and
#' a high value indicates strong conditional dependence. The codec_categorical function is used by the  \code{\link{foci_categorical}} function
#' for variable selection.
#' @return The conditional dependence coefficient (CODEC) of Y and Z given X. If X == NULL, this is just a
#' measure of the dependence between Y and Z.
#' @importFrom stats complete.cases sd
#' @import data.table
#' @export
#' @author Mona Azadkia, Sourav Chatterjee, and Norman Matloff
#' @references !!!!!!!!! NOT CLEAR WHERE YET :D !!!!!!!!!
#' @seealso \code{\link[FOCI]{codec}}, \code{\link[XICOR]{xicor}}
#' @examples
#' n = 1000
#' p = 2
#' x = matrix(runif(n * p), ncol = p)
#' y = (2 * floor(2 * x[, 1]) - 1) * (2 * floor(2 * x[, 2]) - 1)
#' codec_categorical(y, x)
#' codec_categorical(y, x[, 1])
#' codec_categorical(y, x[, 2])
#' codec_categorical(y, x[, 1], x[, 2])
codec_categorical <- function(Y, Z, X = NULL, na.rm = TRUE, type = "continuous") {

  if(!(type == "continuous" || type == "discrete")) stop("type should be defined as continuous or discrete")

  if(is.null(X)) {
    # if inputs are not in proper matrix format change if possible
    # otherwise send error
    if(!is.vector(Y)) {
      Y = as.vector(Y)
    }
    if(!is.matrix(Z)) {
      Z = as.matrix(Z)
    }
    if((length(Y) != nrow(Z))) stop("Number of rows of Y and X should be equal.")
    if (na.rm == TRUE) {
      # NAs are removed here:
      ok = complete.cases(Y,Z)
      Z = as.matrix(Z[ok,])
      Y = Y[ok]
    }

    n = length(Y)
    if(n < 2) stop("Number of rows with no NAs should be bigger than 1.")

    q = ncol(Z)
    # Convert factor columns into numeric
    if(type == "continuous") {
      return(T_categorical_continuous(Y, Z))
    }
    return(T_categorical_discrete(Y, Z))
  }
  # if inputs are not in proper matrix format change if possible
  # otherwise send error
  if(!is.vector(Y)) {
    Y = as.vector(Y)
  }
  if(!is.matrix(X)) {
    X = as.matrix(X)
  }
  if(!is.matrix(Z)) {
    Z = as.matrix(Z)
  }
  if((length(Y) != nrow(X))) stop("Number of rows of Y and X should be equal.")
  if((length(Y) != nrow(Z))) stop("Number of rows of Y and Z should be equal.")
  if((nrow(Z) != nrow(X))) stop("Number of rows of Z and X should be equal.")
  if (na.rm == TRUE) {
    # NAs are removed here:
    ok = complete.cases(Y,Z,X)
    Z = as.matrix(Z[ok,])
    Y = Y[ok]
    X = as.matrix(X[ok,])
  }

  n = length(Y)
  if(n < 2) stop("Number of rows with no NAs should be bigger than 1.")

  if(type == "continuous") {
    return(T_categorical_continuous_conditional(Y, Z, X))
  }
  return(T_categorical_discrete_conditional(Y, Z, X))
}
