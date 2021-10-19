#########################################
# HELPER FUNCTIONS
#
# T_categorical_discrete
# Q_categorical_discrete
# T_categorical_discrete_conditional
#########################################

# T_categorical_discrete -------------------------------------------------------------
#
# @param X: Matrix of predictors (n by p)
# @param Y: Vector (length n)
T_categorical_discrete <- function(Y, X) {
  pX <- pY <- N <- NULL
  XY = cbind(X, Y)
  XY = XY[order(Y), ]
  n = nrow(XY)
  d = as.data.table(XY)
  p = ncol(d) - 1
  # distribution conditional
  distJoint = d[, .N, by = c(colnames(d))]
  probYCondX = distJoint[, pX := sum(N), by = c(colnames(d)[1:p])]
  probYCondX = cbind(probYCondX, pYcondX = probYCondX$N / probYCondX$pX)
  # distribution Y
  probYCondX = probYCondX[, pY := sum(N), by = Y]
  probYCondX$pY = probYCondX$pY / n
  # distribution X
  probYCondX$pX = probYCondX$pX / n
  probY = unique(data.table(probY = probYCondX$pY))

  numerator = sum(probYCondX$pY * probYCondX$pX * probYCondX$pYcondX * (1 - probYCondX$pYcondX))
  denominator = sum((probY$probY^2) * (1 - probY$probY))

  return(1 - (numerator / denominator))
}


# Q_categorical_discrete -------------------------------------------------------------
#
# @param X: Matrix of predictors (n by p)
# @param Y: Vector (length n)
Q_categorical_discrete <- function(Y, X) {
  pX <- pY <- N <- NULL
  XY = cbind(X, Y)
  XY = XY[order(Y), ]
  n = nrow(XY)
  d = as.data.table(XY)
  p = ncol(d) - 1
  # distribution conditional
  distJoint = d[, .N, by = c(colnames(d))]
  probYCondX = distJoint[, pX := sum(N), by = c(colnames(d)[1:p])]
  probYCondX = cbind(probYCondX, pYcondX = probYCondX$N / probYCondX$pX)
  # distribution Y
  probYCondX = probYCondX[, pY := sum(N), by = Y]
  probYCondX$pY = probYCondX$pY / n
  # distribution X
  probYCondX$pX = probYCondX$pX / n
  probY = unique(data.table(probY = probYCondX$pY))

  return(sum(probYCondX$pY * probYCondX$pX * probYCondX$pYcondX * (1 - probYCondX$pYcondX)))
}

# T_categorical_discrete_conditional -------------------------------------------------------------
#
# @param X: Matrix of predictors (n by p)
# @param Z: Matrix of predictors (n by q)
# @param Y: Vector (length n)
T_categorical_discrete_conditional <- function(Y, Z, X) {
  return(1 - (Q_categorical_discrete(Y, cbind(X, Z)) / Q_categorical_discrete(Y, X)))
}



