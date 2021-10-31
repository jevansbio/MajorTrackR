#' MajorTrackR: R interface for MajorTrack
#'
#' R interface for the majortrack python library for carrying out dynamic
#'     community detection method tracking persistent but transiently
#'     discontinuous communities in time window graphs.
#'
#' @section Functions:
#' \code{\link{do_track}},
#' \code{\link{get_dc_membership}},
#' \code{\link{add_dc_membership}},
#' \code{\link{move_events_df}},
#' \code{\link{ind_membership_df}},
#' \code{\link{community_lifespans}},
#' \code{\link{get_similarities}},
#' \code{\link{get_alluvialplot}},
#'
#'
#' @section Data:
#' \code{\link{allnets}},
#'
#' @docType package
#' @name MajorTrackR
NULL
#> NULL

mt=NULL

.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Attempting to find python install with MajorTrack library \n Use reticulate::use_python to specify a different python install")

  #see if a python install with majortrack exists
  reticulate::import("majortrack",delay_load=T)
  mtexists=!is.null(reticulate::py_version())
  if(!mtexists){
    #see if python exists at all
    reticulate::use_python("")
    pyexists=!is.null(reticulate::py_version())
  }else{
    pyexists=T
  }
  #inform user if python and python with majortrack can be found
  if(!pyexists&!mtexists){
    warnings("Python install with majortrack not found. \n Use reticulate::use_python to specify correct python install")
  }else if(pyexists & !mtexists){
    warnings("Python install with majortrack not found. \n Install majortrack using 'pip install --upgrade git+https://github.com/j-i-l/majortrack.git' \n
             Use reticulate::use_python to specify correct python install")
  }
  #import python functions
  mt<<-reticulate::py_run_file(system.file("python","MTprocess.py",package="MajorTrackR"))
}
