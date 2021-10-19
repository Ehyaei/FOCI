#########################################
# HELPER FUNCTIONS
#
# S_categorical_continuous
# Q_categorical_continuous
# T_categorical_continuous
# T_conditional_categorical_continuous
#########################################

# S_categorical_continuous -------------------------------------------------------------------------
#
# @param Y: Vector (length n)
S_categorical_continuous <- function(Y) {
  n = length(Y)
  probY = as.data.table(Y)
  colnames(probY) = "Y"
  probY = probY[, .N, by = "Y"]
  probY$N = probY$N / n
  S = sum((probY$N^2) * (1 - probY$N))
  return(S)
}

# Q_categorical_continuous -------------------------------------------------------------------------
# @param X: Matrix of predictors (n by p)
# @param Y: Vector (length n)
Q_categorical_continuous <- function (Y, X) {

  id <- group <- rnn <- pY <- NULL

  if(!is.matrix(X)) {
    X = as.matrix(X)
  }

  n = length(Y)
  nn_X = RANN::nn2(X, query = X, k = 3)
  # remove the first nearest neighbor for each x which is x itself in case of no repeat data
  # when there is repeated data this is wrong but for repeated data we find the nearest
  # neighbors separately.
  nn_index_X = nn_X$nn.idx[, 2]

  # find all data points that are not unique
  repeat_data = which(nn_X$nn.dists[, 2] == 0)

  # for the repeated data points, choose one of their identicals at random and set its index
  # as the index of the nearest neighbor
  df_X = data.table(id = repeat_data, group = nn_X$nn.idx[repeat_data, 1])
  df_X[, rnn := .randomNN(id), by = "group"]

  nn_index_X[repeat_data] = df_X$rnn

  # nearest neighbors with ties
  ties = which(nn_X$nn.dists[, 2] == nn_X$nn.dists[, 3])
  ties = setdiff(ties, repeat_data)
  if(length(ties) > 0) {
    helper_ties <- function(a) {
      distances <- proxy::dist(matrix(X[a, ], ncol = ncol(X)), matrix(X[-a, ], ncol = ncol(X)))
      ids <- which(distances == min(distances))
      x <- sample(ids, 1)
      return(x + (x >= a))
    }

    nn_index_X[ties] = sapply(ties, helper_ties)
  }
  # distribution Y
  probY = as.data.table(Y)
  colnames(probY) = "Y"
  probY = probY[, pY := .N, by = "Y"]
  probY$pY = probY$pY / n
  # Q estimation
  Q = sum(as.numeric(Y != Y[nn_index_X]) * probY$pY)
  return(Q)
}

# T_categorical_continuous -------------------------------------------------------------------------
# @param X: Matrix of predictors (n by p)
# @param Y: Vector (length n)
T_categorical_continuous <- function(Y, X) {
  n = length(Y)
  return(1 - Q_categorical_continuous(Y, X) / (n * S_categorical_continuous(Y)))
}

# T_categorical_continuous_conditional -------------------------------------------------------------
#
# @param X: Matrix of predictors (n by p)
# @param Z: Matrix of predictors (n by q)
# @param Y: Vector (length n)
T_categorical_continuous_conditional <- function(Y, Z, X) {
  return(1 - (Q_categorical_continuous(Y, cbind(X, Z)) / Q_categorical_continuous(Y, X)))
}

