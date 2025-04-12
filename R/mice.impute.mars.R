
#' MARS algorithm to be used within \code{mice}
#'
#' Imputes univariate missing data using the MARS algorithm with subsampling.
#'
#' @param y Vector to be imputed.
#' @param ry Logical vector of length \code{length(y)} indicating the the subset \code{y[ry]} of elements in y to which the imputation model is fitted. The \code{ry} generally distinguishes the observed (\code{TRUE}) and missing values (\code{FALSE}) in \code{y}.
#' @param x Numeric design matrix with \code{length(y)} rows with predictors for \code{y}. Matrix \code{x} may have no missing values.
#' @param wy Logical vector of length \code{length(y)}. A \code{TRUE} value indicates locations in \code{y} for which imputations are created.
#' @param degree Maximum degree of interaction. Default is 10.
#' @param sampling Amount of subsampling that is applied. Default is 0 which corresponds to bootstrap resampling.
#' @param ... Other named arguments.
#'
#' @return Vector with imputed data, same type as \code{y}, and of length \code{sum(wy)}
#' @export
#'
#' @examples
#' # We normally call mice.impute.mars() from within mice()


mice.impute.mars <- function (y, ry, x, wy = NULL, degree = 10, sampling = 0, ...){
  mice:::install.on.demand("earth", ...)
  if (is.null(wy))
    wy <- !ry
  nobs <- sum(ry)

  # if sampling==0 then bootstrap otherwise is subsampling
  if(sampling==0){
    s <- sample(1:nobs,size = nobs,replace = TRUE)
  }else{
    s <- sample(1:nobs,size = sampling*nobs,replace = FALSE)
  }

  dotxobs <- x[ry, , drop = FALSE][s, , drop = FALSE]
  dotyobs <- y[ry][s]

  if(is.factor(dotyobs)){
    suppressWarnings({
    earth.mod <- earth::earth(x = dotxobs, y = dotyobs
                              , glm=list(family=binomial)
                              , degree = degree)
    p <- predict(earth.mod, x[wy,,drop = FALSE],type = "response")
    })
    # extend if only two levels
    if(length(earth.mod$levels)==2){
      p <- cbind(p,1-p)
      colnames(p)[2] <- earth.mod$levels[which(!earth.mod$levels==colnames(p))]
    }
    impute <- sapply(1:nrow(p),function(i) sample(x = colnames(p), size = 1, prob = p[i,]))
  }else{
    earth.mod <- earth::earth(x = dotxobs, y = dotyobs
                              , glm=list(family=gaussian)
                              , degree = degree)
    s2hat <- mean((predict(earth.mod, dotxobs) - dotyobs)^2)
    impute <- as.vector(predict(earth.mod, x[wy,,drop = FALSE])) +
      rnorm(sum(wy), 0, sqrt(s2hat))
  }
  return(impute)
}

