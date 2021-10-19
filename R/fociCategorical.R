########################################################################################################
# MAIN FUNCTIONS:
# foci_categorical:  Performs feature ordering by conditional independence for categorical observation
#########################################################################################################

# foci_categorical_main_continuous -------------------------------------------------------------------------
# Variable selection by the FOCI algorithm
#
# foci_categorical_main is the core function for foci_categorical, implementing the
# variable selection algorithm based on the measure of conditional dependence \code{\link{codec}}.
#
# @param X Matrix of predictors (n by p)
# @param Y Vector of responses (length n)
# @param num_features Number of variables to be selected, cannot be larger than p. The default value is NULL and in that
# case it will be set equal to p. If stop == TRUE (see below), then num_features is irrelevant.
# @param stop Stops at the first instance of negative codec, if TRUE.
# @param numCores number of cores that are going to be used for parallelizing the process.
# default value is continuous
# @details foci is a forward stepwise algorithm that uses the conditional dependence coefficient (\code{\link{codec}})
# defined in Azadkla and Chatterjee (2019) at each step, instead of the multiple correlation coefficient
# as in ordinary forward stepwise. If stop == TRUE, the process is stopped at the first instance of
# nonpositive codec, thereby selecting subset of variables. Otherwise, a set of covariates of size
# num_features, ordered according to predictive power (as measured by codec) is produced.
# @return A vector of selected covariates, in decreasing order of predictive power.
# @author Mona Azadkia, Sourav Chatterjee, and Norman Matloff
# @references
# @seealso
foci_categorical_main_continuous <- function(Y, X, num_features = NULL, stop = TRUE, numCores = parallel::detectCores()){

  namesX <- colnames(X)
  if (is.null(num_features)) num_features = dim(X)[2]
  if(!is.vector(Y)) {
    Y <- as.vector(unlist(Y))
  }
  if(!is.matrix(X)) {
    X <- as.matrix(X)
  }
  n = length(Y)
  p = ncol(X)

  Q = rep(0, num_features)
  index_select = rep(0, num_features)

  # select the first variable
  if (is.null(dim(X))) {
    seq_Q = T_categorical_continuous(Y, X)
  }else{
    estimateQFixedY <- function(id){
      return(T_categorical_continuous(Y, X[, id]))
    }
    seq_Q = parallel::mclapply(seq(1, p), estimateQFixedY, mc.cores = numCores)
    seq_Q = unlist(seq_Q)
  }

  Q[1] = max(seq_Q)
  if (Q[1] <= 0 & stop == TRUE) return(NULL)
  index_max = min(which(seq_Q == Q[1]))
  index_select[1] = index_max
  count = 1

  # select rest of the variables
  while (count < num_features) {
    seq_Q = rep(0, p - count)
    # indices that have not been selected yet
    index_left = setdiff(seq(1, p), index_select[1:count])

    # find the next best feature
    estimateQFixedYandSubX <- function(id){
      return(T_categorical_continuous(Y, cbind(X[, c(index_select[1:count], id)])))
    }

    if (length(index_left) == 1) {
      seq_Q = estimateQFixedYandSubX(index_left[1])
    } else {
      seq_Q = parallel::mclapply(index_left, estimateQFixedYandSubX, mc.cores = numCores)
      seq_Q = unlist(seq_Q)
    }
    Q[count + 1] = max(seq_Q)
    index_max = min(which(seq_Q == Q[count + 1]))
    if (Q[count + 1] <= Q[count] & stop == TRUE) break
    index_select[count + 1] = index_left[index_max]
    count = count + 1
  }

  selectedVar = data.table(index = index_select[1:count], names = namesX[index_select[1:count]])
  stepT = Q
  result = list(selectedVar = selectedVar, stepT = stepT)
  class(result) = "foci_categorical"
  return(result)
}


# foci_categorical_main_discrete -------------------------------------------------------------------------
# Variable selection by the FOCI algorithm
#
# foci_categorical_main is the core function for foci_categorical, implementing the
# variable selection algorithm based on the measure of conditional dependence \code{\link{codec}}.
#
# @param X Matrix of predictors (n by p)
# @param Y Vector of responses (length n)
# @param num_features Number of variables to be selected, cannot be larger than p. The default value is NULL and in that
# case it will be set equal to p. If stop == TRUE (see below), then num_features is irrelevant.
# @param stop Stops at the first instance of negative codec, if TRUE.
# @param numCores number of cores that are going to be used for parallelizing the process.
# default value is continuous
# @details foci is a forward stepwise algorithm that uses the conditional dependence coefficient (\code{\link{codec}})
# defined in Azadkla and Chatterjee (2019) at each step, instead of the multiple correlation coefficient
# as in ordinary forward stepwise. If stop == TRUE, the process is stopped at the first instance of
# nonpositive codec, thereby selecting subset of variables. Otherwise, a set of covariates of size
# num_features, ordered according to predictive power (as measured by codec) is produced.
# @return A vector of selected covariates, in decreasing order of predictive power.
# @author Mona Azadkia, Sourav Chatterjee, and Norman Matloff
# @references
# @seealso
foci_categorical_main_discrete <- function(Y, X, num_features = NULL, stop = TRUE, numCores = parallel::detectCores()){

  namesX <- colnames(X)
  if (is.null(num_features)) num_features = dim(X)[2]
  n = length(Y)
  p = ncol(X)

  Q = rep(0, num_features)
  index_select = rep(0, num_features)

  # select the first variable
  if (is.null(dim(X))) {
    seq_Q = T_categorical_discrete(Y, X)
  }else{
    estimateQFixedY <- function(id){
      return(T_categorical_discrete(Y, X[, id]))
    }
    seq_Q = parallel::mclapply(seq(1, p), estimateQFixedY, mc.cores = numCores)
    seq_Q = unlist(seq_Q)
  }

  Q[1] = max(seq_Q)
  if (Q[1] <= 0 & stop == TRUE) return(NULL)
  index_max = min(which(seq_Q == Q[1]))
  index_select[1] = index_max
  count = 1

  # select rest of the variables
  while (count < num_features) {
    seq_Q = rep(0, p - count)
    # indices that have not been selected yet
    index_left = setdiff(seq(1, p), index_select[1:count])

    # find the next best feature
    estimateQFixedYandSubX <- function(id){
      return(T_categorical_discrete(Y, cbind(X[, c(index_select[1:count], id)])))
    }

    if (length(index_left) == 1) {
      seq_Q = estimateQFixedYandSubX(index_left[1])
    } else {
      seq_Q = parallel::mclapply(index_left, estimateQFixedYandSubX, mc.cores = numCores)
      seq_Q = unlist(seq_Q)
    }
    Q[count + 1] = max(seq_Q)
    index_max = min(which(seq_Q == Q[count + 1]))
    if (Q[count + 1] <= Q[count] & stop == TRUE) break
    index_select[count + 1] = index_left[index_max]
    count = count + 1
  }

  selectedVar = data.table(index = index_select[1:count], names = namesX[index_select[1:count]])
  stepT = Q
  result = list(selectedVar = selectedVar, stepT = stepT[1:length(selectedVar)])
  class(result) = "foci_categorical"
  return(result)
}


# foci_categorical -------------------------------------------------------------------------
#' Variable selection by the FOCI algorithm
#'
#' FOCI is a variable selection algorithm based on the measure of conditional dependence \code{\link{codec_categorical}}.
#'
#' @param X Matrix of predictors (n by p)
#' @param Y Vector of responses (length n)
#' @param num_features Number of variables to be selected, cannot be larger than p. The default value is NULL and in that
#' case it will be set equal to p. If stop == TRUE (see below), then num_features is irrelevant.
#' @param stop Stops at the first instance of negative codec, if TRUE.
#' @param na.rm Removes NAs if TRUE.
#' @param standardize Standardize covariates if set equal to "scale" or "bounded". Otherwise will use the raw inputs.
#' The default value is "scale" and normalizes each column of X to have mean zero and variance 1. If set equal to "bounded"
#' map the values of each column of X to [0, 1].
#' @param type type a factor variable indicating whether X and Z are continuous vectors or discrete, the
#' default value is continuous
#' @param numCores Number of cores that are going to be used for
#' parallelizing the variable selecction process.
#' @param parPlat Specifies the parallel platform to chunk data by rows.
#' It can take three values:
#' 1- The default value is set to 'none', in which case no row chunking
#' is done;
#' 2- the \code{parallel} cluster to be used for row chunking;
#' 3- "locThreads", specifying that row chunking will be done via
#' threads on the host machine.
#' @param printIntermed The default value is TRUE, in which case print intermediate results from the cluster nodes before final processing.
#' @details FOCI is a forward stepwise algorithm that uses the conditional dependence coefficient (\code{\link{codec_categorical}})
#' at each step, instead of the multiple correlation coefficient
#' as in ordinary forward stepwise. If \code{stop} == TRUE, the process is stopped at the first instance of
#' nonpositive codec, thereby selecting a subset of variables. Otherwise, a set of covariates of size
#' \code{num_features}, ordered according to predictive power (as measured by codec) is produced.
#'
#' \emph{Parallel computation:}
#'
#' The computation can be lengthy, so the package offers two kinds of
#' parallel computation.
#'
#' The first, controlled by the argument \code{numCores},
#' specifies the number of cores to be used on the host
#' machine. If at a given step there are k candidate variables
#' under consideration for inclusion, these k tasks are assigned
#' to the various cores.
#'
#' The second approach, controlled by the argument \code{parPlat}
#' ("parallel platform"), involves the user first setting up a cluster via
#' the \pkg{parallel} package. The data are divided into chunks by rows,
#' with each cluster node applying FOCI to its data chunk.  The
#' union of the results is then formed, and fed through FOCI one more
#' time to adjust the discrepancies. The idea is that that last step
#' will not be too lengthy, as the number of candidate variables has
#' already been reduced.  A cluster size of r may actually
#' produce a speedup factor of more than r (Matloff 2016).
#'
#' Potentially the best speedup is achieved by using the two approaches
#' together.
#'
#' The first approach cannot be used on Windows platforms, as
#' \code{parallel::mcapply} has no effect.  Windows users should thus
#' use the second approach only.
#'
#' In addition to speed, the second approach is useful for diagnostics, as
#' the results from the different chunks gives the user an
#' idea of the degree of sampling variability in the
#' FOCI results.
#'
#' In the second approach, a random permutation is applied to the
#' rows of the dataset, as many datasets are sorted by one or more
#' columns.
#'
#' Note that if a certain value of a feature is rare in the
#' full dataset, it may be absent entirely in some chunk.
#' @return An object of class "foci_categorical", with attributes
#' \code{selectedVar}, showing the selected variables in decreasing
#' order of (conditional) predictive power, and \code{stepT}, listing
#' the 'codec' values. Typically the latter will begin to level off at
#' some point, with additional marginal improvements being small.
#' @export
#' @author Mona Azadkia, Sourav Chatterjee, and Norman Matloff
#' @references !!!!!!!!! NOT CLEAR WHERE YET :D !!!!!!!!!
#' @references Matloff, N. (2016). Software Alchemy: Turning Complex
#' Statistical Computations into Embarrassingly-Parallel Ones.
#' \emph{J. of Stat. Software.}
#' @seealso \code{\link[FOCI]{foci}}, \code{\link[FOCI]{codec}}, \code{\link[XICOR]{xicor}}
foci_categorical <- function(Y, X, num_features = NULL, stop = TRUE, na.rm = TRUE,
                             standardize = "scale", type = "continuous", numCores = parallel::detectCores(),
                             parPlat = 'none', printIntermed = TRUE) {

  if(!(type == "continuous" || type == "discrete")) stop("type should be defined continuous or discrete")

  namesX <- colnames(X)

  # if inputs are not in proper format change if possible
  # otherwise send error
  if(!is.vector(Y)) {
    Y <- as.vector(unlist(Y))
  }
  if(!is.matrix(X)) {
    X <- as.matrix(X)
  }

  if (is.null(num_features)) num_features <- dim(X)[2]

  if((length(Y) != nrow(X))) stop("Number of rows of Y and X should be equal.")

  if (na.rm == TRUE) {
    # NAs are removed here:
    ok <- complete.cases(Y,X)
    X <- as.matrix(X[ok,])
    Y <- Y[ok]
  }

  n <- length(Y)

  if(n < 2) stop("Number of rows with no NAs should be bigger than 1.")

  p = ncol(X)

  if (num_features > p) stop("Number of features should not be larger than maximum number of original features.")
  if ((floor(num_features) != num_features) || (num_features <= 0)) stop("Number of features should be a positive integer.")

  if (standardize == "scale") {
    for (i in 1:p) {
      if(length(unique(X[, i])) > 1) {
        X[,i] <- (X[,i] - mean(X[,i])) / sd(X[,i])
      }else{
        stop(stop(paste0("Column ", i, " of X is constant.")))
      }
    }
  }

  if (standardize == "bounded") {
    for (i in 1:p) {
      if(length(unique(X[, i])) > 1) {
        X[,i] <- (X[,i] - min(X[,i])) / (max(X[, i]) - min(X[, i]))
      }else{
        stop(stop(paste0("Column ", i, " of X is constant.")))
      }
    }
  }

  if (parPlat[1] == 'none') {
    if(type == "continuous"){
      return(foci_categorical_main_continuous(Y, X, num_features = num_features, stop = stop, numCores = numCores))
    }else {
      return(foci_categorical_main_discrete(Y, X, num_features = num_features, stop = stop, numCores = numCores))
    }
  }

  # many datasets are ordered by one or more columns; to
  # preserve iid-ness, randomize the row order; if we get here, we will
  # be chunking
  nr <- nrow(X)
  permRowNums <- sample(1:nr,nr,replace=FALSE)
  X <- X[permRowNums,]
  Y <- Y[permRowNums]

  rowNums <- parallel::splitIndices(length(Y), numCores)
  if(type == "continuous"){
    selectFromChunk <- function(nodeNum) {
      myRows <- rowNums[[nodeNum]]
      sel <- foci_categorical_main_continuous(Y[myRows], X[myRows,], stop = stop,
                                   numCores = numCores)$selectedVar$index
    }
  }else{
    selectFromChunk <- function(nodeNum) {
      myRows <- rowNums[[nodeNum]]
      sel <- foci_categorical_main_discrete(Y[myRows], X[myRows,], stop = stop,
                                   numCores = numCores)$selectedVar$index
    }
  }

  if(inherits(parPlat,'cluster')) {
    cls <- parPlat
  }else if(parPlat == 'locThreads') {
    # set up the cluster (in multicore case, it's virtual)
    cls <- parallel::makeCluster(numCores)
  } else stop('invalid parPlat')

  # worker nodes load library
  parallel::clusterEvalQ(cls, library(FOCIcategorical))
  # ship data to workers
  parallel::clusterExport(cls, c('Y', 'X', 'rowNums', 'selectFromChunk'),
                          envir = environment())
  # drop-in replacement for mclapply
  slc <- parallel::parLapply(cls, seq(1, length(cls)), selectFromChunk)

  if (printIntermed) print(slc)

  slc <- Reduce(union, slc)

  if(type == "continuous") {
    res <- foci_categorical_main_continuous(Y, X[, slc], num_features, stop)
  }else{
    res <- foci_categorical_main_discrete(Y, X[, slc], num_features, stop)
  }

  # must translate indices in reduced system to those of original
  newIdxs <- res$selectedVar$index
  origIdxs <- slc[newIdxs]
  res$selectedVar$index <- origIdxs
  res$stepT <- res$stepT[1:length(res$selectedVar)]

  parallel::stopCluster(cls)

  return(res)
}


