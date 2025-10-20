
      #' @title ExtractRandTmb
      #'
      #' @description
      #'  Function to extracts random coefficients from a list containing TMB::MakeADFun and nlminb bObjects.
      #'
      #'
      #' @param tmbObj A list that contains the TMB::MakeADFun and nlminb objects.
      #' @param params Parameter names to extract. If NULL, all parameters will be extracted.
      #' @param reNames A vector of names to rename parameters. If NULL, the original TMB names will be retained.
      #' @param path The path trough your dllID-object location. If it is in the current working directory, set it to NULL
      #' @param dllID DLL
      #' @return A vector of chosen parameters.
      #' @author Jemay Salomon
      ## @examples
      #'@export
      ExtractRandTmb <- function(tmbObj,
                                 dllID,
                                 params = NULL,
                                 reNames = NULL,
                                 path = NULL) {
        
        #require tmb packages and make summary
        requireNamespace(package="TMB")
        
        # Load the DLL using dyn.load
        if(!is.null(path)){
          dyn.load(file.path(path, TMB::dynlib(dllID)))
        }else{
          dyn.load(TMB::dynlib(dllID))}
        
        #range check
        if(!is.list(tmbObj)) stop("tmbOj must be a list")
        
        sdreporttmbObj <- TMB::sdreport(tmbObj$f)
        
        if (is.null(params)) {
          randEffs <- summary(sdreporttmbObj, select = "random")[, "Estimate"]
        } else {
          if (!is.character(params)) stop("The 'random' argument must be a character vector.")
          
          randEffs <- lapply(params, function(rand) {
            idx <- which(rownames(summary(sdreporttmbObj, select = "random")) == rand)
            if (length(idx) == 0) stop(paste("Random effect '", rand, "' not found in summary."))
            summary(sdreporttmbObj, select = "random")[idx, "Estimate"] })
          randEffs <- unlist(randEffs, recursive = TRUE, use.names = TRUE)
          randEffs <- as.numeric(randEffs)
          names(randEffs) <- reNames
        }
        
        return(unlist(randEffs))
      }
      
      
      
      #' @title ExtractParamsTmb
      #'
      #'@description
      #' Function to extracts specified parameters from a list containing TMB::MakeADFun and nlminb Objects.
      #'
      #' @param tmbObj A list that contains the TMB::MakeADFun and nlminb tmbObjects.
      #' @param params Parameter names to extract. If NULL, all parameters will be extracted.
      #' @param reNames A vector of names to rename parameters. If NULL, the original TMB names will be retained.
      #' @param path The path trough your dllID-object location. If it is in the current working directory, set it to NULL
      #' @param dllID DLL
      #' @return A vector of chosen parameters.
      #' @author Jemay Salomon
      ## @examples
      #'@export
      ExtractParamsTmb <- function(tmbObj,
                                   params = NULL,
                                   dllID,
                                   reNames = NULL,
                                   path = NULL) {
        #require tmb packages and make summary
        requireNamespace(package="TMB")
        
        # Load the DLL using dyn.load
        if(!is.null(path)) {
          dyn.load(file.path(path, TMB::dynlib(dllID)))
        }else {
          dyn.load(TMB::dynlib(dllID))}
        
        #range check
        if(!is.list(tmbObj)) stop("tmbOj must be a list")
        
        if(is.null(params)){
          parameters <- tmbObj$fit$par
        } else {
          tmbParams <- lapply(params, function(param) {
            if (!any(grepl(paste0("^", param, "$"), names(tmbObj$fit$par)))) {
              stop(paste(param, " not found in out$fit$par"))}
            idx <- grepl(paste0("^", param, "$"), names(tmbObj$fit$par))
            return(tmbObj$fit$par[idx])})
          
          parameters <- (unlist(tmbParams))
          
          if (!is.null(reNames)) {
            stopifnot(length(reNames)==length(parameters))
            names(parameters) <- reNames
          }
        }
        
        return(parameters)
      }
      
      
      
      #' @title ExtractVarTmb
      #'
      #'@description
      #' Function to extract  variances parameters from a list containing TMB::MakeADFun and nlminb Objects
      #'
      #' @param tmbObj A list that contains the TMB::MakeADFun and nlminb tmbObjects.
      #' @param params Parameter names to extract. If NULL, all parameters will be extracted.
      #' @param reNames A vector of names to rename parameters. If NULL, the original TMB names will be retained.
      #' @param path The path trough your dllID-object location. If it is in the current working directory, set it to NULL
      #' @param dllID DLL
      #' @return A vector of chosen parameters.
      #' @author Jemay Salomon
      ## @examples
      #'
      #'@export
      ExtractVarTmb <- function(tmbObj,
                                params,
                                dllID,
                                reNames = NULL,
                                path = NULL) {
        
        #require tmb packages and make summary
        requireNamespace(package="TMB")
        
        # Load the DLL using dyn.load
        if(!is.null(path)) {
          dyn.load(file.path(path, TMB::dynlib(dllID)))
        }else{
          dyn.load(TMB::dynlib(dllID))}
        
        #range check
        if(!is.list(tmbObj)) stop("tmbOj must be a list")
        if(is.null(params)){
          stop("Params must be specified")}
        idx <- ExtractParamsTmb(tmbObj, params, path = path, dllID, reNames)
        var <- exp(idx)^2
        
        return(var)
      }
      
      
      #' @title  ExtractCorTmb
      #'
      #'@description
      #' Function to extract  correlation parameters from a list containing TMB::MakeADFun and nlminb Objects
      #'
      #' @param tmbObj A list that contains the TMB::MakeADFun and nlminb tmbObjects.
      #' @param params Parameter names to extract. If NULL, all parameters will be extracted.
      #' @param reNames A vector of names to rename parameters. If NULL, the original TMB names will be retained.
      #' @param path The path trough your dllID-object location. If it is in the current working directory, set it to NULL
      #' @param dllID DLL
      #' @return A vector of chosen parameters.
      #' @author Jemay Salomon
      ## @examples
      #'
      #'@export
      ExtractCorTmb <- function(tmbObj,
                                params = NULL,
                                dllID,
                                reNames = NULL,
                                path = NULL) {
        
        #require tmb packages and make summary
        requireNamespace(package="TMB")
        
        # Load the DLL using dyn.load
        if(!is.null(path)){
          dyn.load(file.path(path, TMB::dynlib(dllID)))
        }else{
          dyn.load(TMB::dynlib(dllID))}
        
        #range check
        if(!is.list(tmbObj)) stop("tmbOj must be a list")
        
        #Get report object from report(f)
        objReport = tmbObj$f$report()
        
        #set the output list
        out <- list()
        
        #set conditions parameters
        if(is.null(params)){
          Names <- list()
          for (param in 1: length(objReport)){
            Names[[param]] <- names(objReport)[[param]]
            out[[param]] <- objReport[[param]][2]
            names(out) <- Names}
          
        } else {
          for(param in params) {
            if (is.null(objReport[[param]])) stop(paste(param, "not found in obj$f$report()"))
            out[[param]] <- objReport[[param]][2]}
          
          if (!is.null(reNames)) {
            stopifnot(length(reNames) == length(out))
            names(out) <- reNames
          }
        }
        
        return(unlist(out))
      }
      
      
      #' @title  ExtractStdTmb
      #'
      #'@description
      #' Function to extract  standard error parameters from a list containing TMB::MakeADFun and nlminb Objects
      #'
      #' @param tmbObj A list that contains the TMB::MakeADFun and nlminb tmbObjects.
      #' @param params Parameter names to extract. If NULL, all parameters will be extracted.
      #' @param reNames A vector of names to rename parameters. If NULL, the original TMB names will be retained.
      #' @param path The path trough your dllID-object location. If it is in the current working directory, set it to NULL
      #' @param dllID DLL
      #' @return A matrix of chosen parameters with their standard error
      #' @author Jemay Salomon
      ## @examples
      #'
      #'@export
      ExtractStdTmb <- function(tmbObj,
                                dllID,
                                params = NULL, 
                                reNames = NULL,
                                path = NULL){
        
        #require tmb packages and make summary
        requireNamespace(package="TMB")  
        
        # Load the DLL using dyn.load
        if(!is.null(path)){
          dyn.load(file.path(path, TMB::dynlib(dllID)))
        }else{
          dyn.load(TMB::dynlib(dllID))}
      
        #range check
        if(!is.list(tmbObj)) stop("tmbOj must be a list")
        
        sdreporttmbObj <- TMB::sdreport(tmbObj$f)
        
        if (is.null(params)) {
          stdEffs <- summary(sdreporttmbObj)[, "Std. Error"]
        } else {
          if (!is.character(params)) stop("The 'params' argument must be a character vector.")
          
          tmp <- lapply(params, function(std) {
            idx <- which(rownames(summary(sdreporttmbObj)) == std)
            if (length(idx) == 0) stop(paste("std effect '", std, "' not found in summary."))
            summary(sdreporttmbObj)[idx, "Std. Error"] })
        }
        
        out <- unlist(tmp)
        
        if(!is.null(reNames)){
          stopifnot(length(out)==length(reNames))
          names(out) <- reNames
        }
        
        return(cbind("Std. Error" = out))
      }
      
      
      
      #' @title tmbExtract
      #'
      #'@description
      #' Macro function to extract TMB parameters of specified types.
      #'
      #' @param tmbObj A list containing TMB::MakeADFun and nlminb tmbObjects.
      #' @param params Parameter names to extract. If NULL, all parameters will be extracted.
      #' @param reNames A vector of names to rename parameters. If NULL, the original TMB names will be retained.
      #' @param path The path trough your dllID-object location. If it is in the current working directory, set it to NULL
      #' @param paramsType Specifies the type of TMB parameters to extract (e.g., "paramsTmb", "random", "variance", "correlation").
      #' @param dllID DLL
      #' @return A vector of selected parameters.
      #' @author Jemay Salomon
      #' @export
      tmbExtract <- function(tmbObj,
                             params = NULL,
                             reNames = NULL,
                             dllID,
                             path = NULL,
                             paramsType){
        
        #set arguments parameters
        argsTmb <- list(tmbObj = tmbObj,
                        params = params,
                        dllID = dllID,
                        reNames = reNames,
                        path = path)
        
        #set conditions
        if (paramsType == "paramsTmb") {
          out <- do.call(ExtractParamsTmb, argsTmb)
        } else if (paramsType == "random") {
          out <- do.call(ExtractRandTmb, argsTmb)
        } else if (paramsType == "variance") {
          out <- do.call(ExtractVarTmb, argsTmb)
        } else if (paramsType == "correlation") {
          out <- do.call(ExtractCorTmb, argsTmb)
        } else if (paramsType == "std. error") {
          out <- do.call(ExtractStdTmb, argsTmb)
        } else {
          stop("Invalid paramsType")
        }
        
        #return
        return(out)
        
      }
      