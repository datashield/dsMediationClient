#'
#' @title Linear hypotheses for natural effect models
#' @description This function is similar to R function \code{neLht} from the 
#' \code{medflex} package.
#' @details The function \code{ds.neLht} allows to calculate linear combinations
#' of natural effect model parameter estimates.
#' @param model the name of a fitted natural effect model object. This is the
#' object saved on the server-side by the \code{ds.neModel} function.
#' @param linfct a specification of the linear hypothesis to be tested.
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return a summary table of the object of class c("neLht", "glht") (see glht).
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
ds.neLht <- function(model=NULL, linfct=NULL, datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  # verify that name of the fitted model was set
  if(is.null(model)){
    stop(" Please provide the name of a fitted natural effect model object!", call.=FALSE)
  }
  
  # check if the fitted model is defined in all studies
  defined <- dsBaseClient:::isDefined(datasources, model)
  
  if (!is.null(linfct)) {
    linfct <- paste0(as.character(linfct), collapse = ",")
    linfct <- gsub(":", "qqq", linfct, fixed = TRUE)
    linfct <- gsub("=", "rrr", linfct, fixed = TRUE)
    linfct <- gsub(" ", "", linfct, fixed = TRUE)
  }
  
  calltext <- call('neLhtDS', model, linfct)
  out <- DSI::datashield.aggregate(datasources, calltext)
  
  return(out)
  
}
