##' Function to extracts random coefficients from a list containing TMB::MakeADFun and nlminb objects.
##'
##' @param obj A list that contains the TMB::MakeADFun and nlminb objects.
##' @param params Parameter names to extract. If NULL, all parameters will be extracted.
##' @param reNames A vector of names to rename parameters. If NULL, the original TMB names will be retained.
##' @return A vector of chosen parameters.
##' @author Jemay Salomon
## @examples
##'@export
ExtractRandTmb <- function(obj, params = NULL, reNames=NULL) {
  
  requireNamespace(package="TMB")

  sdreportObj <- TMB::sdreport(obj$f)

  if (is.null(params)) {
    randEffs <- summary(sdreportObj, select = "random")[, "Estimate"]

  } else {
    if (!is.character(params)) stop("The 'random' argument must be a character vector.")

    randEffs <- lapply(params, function(rand) {

      idx <- which(rownames(summary(sdreportObj, select = "random")) == rand)

      if (length(idx) == 0) stop(paste("Random effect '", rand, "' not found in summary."))

      summary(sdreportObj, select = "random")[idx, "Estimate"]

    })

    names(randEffs) <- params
  }

  return(randEffs)
}

##' Function to extracts specified parameters from a list containing TMB::MakeADFun and nlminb objects.
##'
##' @param obj A list that contains the TMB::MakeADFun and nlminb objects.
##' @param params Parameter names to extract. If NULL, all parameters will be extracted.
##' @param reNames A vector of names to rename parameters. If NULL, the original TMB names will be retained.
##' @return A vector of chosen parameters.
##' @author Jemay Salomon
## @examples
##'@export
ExtractParamsTmb <- function(obj, params = NULL, reNames = NULL) {

  if (!is.list(obj)) {
    stop("out must be a list")
  }

  if(is.null(params)){
    parameters <- obj$fit$par
  } else {
    tmbParams <- lapply(params, function(param) {

      if (!any(grepl(paste0("^", param, "$"), names(obj$fit$par)))) {
        stop(paste(param, " not found in out$fit$par"))
      }

      idx <- grepl(paste0("^", param, "$"), names(obj$fit$par))

      return(obj$fit$par[idx])

    })

    parameters <- (unlist(tmbParams))

    if (!is.null(reNames)) {
      stopifnot(length(reNames)==length(parameters))
      names(parameters) <- reNames
    }

  }

  return(parameters)
}

##' Function to extract  variances parameters from a list containing TMB::MakeADFun and nlminb objects
##'
##' @param obj A list that contains the TMB::MakeADFun and nlminb objects.
##' @param params Parameter names to extract. If NULL, all parameters will be extracted.
##' @param reNames A vector of names to rename parameters. If NULL, the original TMB names will be retained.
##' @return A vector of chosen parameters.
##' @author Jemay Salomon
## @examples
##'
##'@export
ExtractVarTmb <- function(obj, params, reNames = NULL) {

  if(is.null(params)){
    stop("Params must be specified")
  }
  idx <- ExtractParamsTmb(obj,params, reNames)

  var <- exp((idx))^2

  return(var)
}

##' Function to extract  correlation parameters from a list containing TMB::MakeADFun and nlminb objects
##'
##' @param obj A list that contains the TMB::MakeADFun and nlminb objects.
##' @param params Parameter names to extract. If NULL, all parameters will be extracted.
##' @param reNames A vector of names to rename parameters. If NULL, the original TMB names will be retained.
##' @return A vector of chosen parameters.
##' @author Jemay Salomon
## @examples
##'
##'@export
ExtractCorTmb <- function(obj, params = NULL, reNames = NULL) {

  if (!is.list(obj)) {
    stop("out must be a list")
  }

  objReport = obj$f$report()

  out <- list()

  if(is.null(params)){
    Names <- list()
    for (param in 1: length(objReport)){
      Names[[param]] <- names(objReport)[[param]]
      out[[param]] <- objReport[[param]][2]
      names(out) <- Names
    }
  } else {
    for(param in params) {
      if (is.null(objReport[[param]])) stop(paste(param, "not found in obj$f$report()"))
      out[[param]] <- objReport[[param]][2]
    }

    if (!is.null(reNames)) {
      stopifnot(length(reNames) == length(out))
      names(out) <- reNames
    }
  }

  return(unlist(out))
}


##' Macro function to extract TMB parameters of specified types.
##'
##' @param obj A list containing TMB::MakeADFun and nlminb objects.
##' @param params Parameter names to extract. If NULL, all parameters will be extracted.
##' @param reNames A vector of names to rename parameters. If NULL, the original TMB names will be retained.
##' @param paramsType Specifies the type of TMB parameters to extract (e.g., "paramsTmb", "random", "variance", "correlation", "std.error").
##' @return A vector of selected parameters.
##' @author Jemay Salomon
## @examples
##' @export
tmbExtract <- function(obj, params = NULL, reNames = NULL, paramsType){

  argsTmb <- list(
    obj = obj,
    params = params,
    reNames = reNames
  )

  if (paramsType == "paramsTmb") {
    out <- do.call(ExtractParamsTmb, argsTmb)
  } else if (paramsType == "random") {
    out <- do.call(ExtractRandTmb, argsTmb)
  } else if (paramsType == "variance") {
    out <- do.call(ExtractVarTmb, argsTmb)
  } else if (paramsType == "correlation") {
    out <- do.call(ExtractCorTmb, argsTmb)
  } else {
    stop("Invalid paramsType")
  }
  return(out)
}
