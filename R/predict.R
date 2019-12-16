predict.emmix <- function(object, Y, ...) {
  
  if (missing(Y)) {
   stop("Missing newdata Y.")
  }
  
  if (any(class(object) %in% "mcfa")) {
     tau <- do.call("tau.mcfa", c(list(Y = Y), object))
  }
  if (any(class(object) %in% "mctfa")) {
    tau <- do.call("tau.mctfa", c(list(Y = Y), object))
  }
  if (any((class(object) %in% "mfa"))) {
    tau <- do.call("tau.mfa", c(list(Y = Y), object))
  }
  if (any((class(object) %in% "mtfa"))) {
    tau <- do.call("tau.mtfa", c(list(Y = Y), object))$tau
  }
  if (!(any(class(object) %in% "mcfa") || any(class(object) %in% "mctfa") ||
          any(class(object) %in% "mfa") || any(class(object) %in% "mtfa"))) {

    stop("STOP:object must be of class mcfa, mctfa, mfa or mtfa")
  } else {
    clust <- apply(tau, 1, which.max)
    return(clust)
  }
}

predict.mcfa <- function(object, Y, ...) {
  
  tau <- do.call("tau.mcfa", c(list(Y = Y), object))
  clust <- apply(tau, 1, which.max) 
  clust
}

predict.mctfa <- function(object, Y, ...) {
  
  tau <- do.call("tau.mctfa", c(list(Y = Y), object))
  clust <- apply(tau, 1, which.max) 
  clust
}

predict.mfa <- function(object, Y, ...) {

  tau <- do.call("tau.mfa", c(list(Y = Y), object))
  clust <- apply(tau, 1, which.max)
  clust
}

predict.mtfa <- function(object, Y, ...) {

  tau <- do.call("tau.mtfa", c(list(Y = Y), object))
  clust <- apply(tau, 1, which.max)
  clust
}