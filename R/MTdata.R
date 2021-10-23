#' Generated example network data.
#'
#' A dataset of network data to demonstrate MajorTrack. See vignette for code.
#'
#' @format A list of 6 igraph graphs, each with the following attributes:
#' \describe{
#'   \item{V(name)}{numeric label, vertex ID}
#'   \item{V(group)}{numeric, group assigned while generating data}
#'   \item{V(timestep)}{number from 1-6 indicating the timestep}
#'   \item{V(com)}{number indicating detected commuinity ID}
#'   \item{E(weight)}{association strength}
#'   \item{E(from_name)}{from vertex ID}
#'   \item{E(from_group)}{from vertex initially assigned group}
#'   \item{E(to_name)}{from vertex ID}
#'   \item{E(to_group)}{to vertex initially assigned group}
#'   \item{E(same_group)}{boolean indicating if both vertices are part of the
#'       same initially assigned group}
#'   \item{E(group)}{if E(samegroup) is TRUE, the group both vertices were
#'        initially assigned. If E(samegroup) if FALSE, NA}
#'   \item{E(timestep)}{number from 1-6 indicating the timestep}
#' }
"allnets"

#' Generated example detected communities.
#'
#' Detected communities from the dataset of network data to demonstrate
#'     MajorTrack (\code{\link{allnets}}). See vignette for code.
#'
#' @format A list of 6 numeric vectors, giving the community ID to which that
#'     vertex is assigned:
"allcoms"
