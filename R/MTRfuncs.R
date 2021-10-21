mt=NULL

.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Attempting to find python install with MajorTrack library \n
                        Use reticulate::use_python to specify a different python install")

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


#' Run MajorTrack algorithm
#'
#' \code{do_track} runs the MajorTrack algorithm for detecting dynamic communities
#'
#'
#' @param allnets A list of igraph networks.
#' @param allcoms A list of detected communities, as returned by many igraph
#'    community detection algorithms. The list should be the same length as
#'    allnets.
#'    Each element of the list should be a vector of integers with the
#'    same length as the number of vertices in the corresponding network.
#' @param historypar An integer to set the number of time points (or slices)
#'     the algorithm can maximally go back in time to check for majority matches.
#' @return A MajorTrack object, which R can interpret as a list.  See
#'   \url{https://majortrack.readthedocs.io/en/latest/api/majortrack.tracker.html}
#'   for full details
#'
#'   Dynamic community membership can be extracted using \code{\link{get_dc_membership}}
#'
#'
#' @examples
#' data(allnets)
#' data(allcoms)
#' do_track(allnets, allcoms, history=1)
do_track=function(allnets, allcoms,historypar=2){
  allpyids = get_pyids(allnets) #convert to a list of individual IDs for each timestep in python style
  allpycoms=get_com_pyids(coms)#get community memberships in python style foreach timestep
  cat("Do MajorTrack \n")
	track = mt$R_do_track(allpycoms, allpyids, history=historypar)
	return(track)
}

#' Get individual dynamic community membership from a MajorTrack object
#'
#' \code{get_dc_membership} extracts the per timestep dynamic communities for
#'    each vertex.
#'
#'
#' @param track A MajorTrack object as produced by \code{\link{do_track}}
#' @param allcoms A list of detected communities, as returned by many igraph
#' @return A list, the length of which is the number of timesteps included in
#'    the MajorTrack object. Each element of the list is a numeric vector
#'    indicating which dynamic community a vertex is currently a member of.
#'
#'
#' @examples
#' data(allnets)
#' data(allcoms)
#' track = do_track(allnets, allcoms, history=1)
#' get_dc_membership(track)
get_dc_membership=function(track){
	#get per timestep memberships of dynamic community from MajorTrack return
	dcmembership=lapply(track$individual_membership,function(x){
		unlist(x)
	})
	return(dcmembership)
}

#' Add dynamic community membership to graphs as vertex attribute
#'
#' Utility function for adding dynamic community memberships to a list of igraph
#'     networks
#'
#'
#' @param allnets A list of igraph networks.
#' @param dcmembership A list of dynamic community membership as produced by \code{\link{get_dc_membership}}
#' @return The provided list of igraph networks with the added vertex attribute "DC"
#'     indicating which dynamic community that vertex is currently a member of.
#'
#' @examples
#' data(allnets)
#' data(allcoms)
#' track = do_track(allnets, allcoms, history=1)
#' dcmembership = get_dc_membership(track)
#' allnets = add_dc_membership(allnets, dcmembership) #overwriting original allnets
add_dc_membership=function(allnets,dcmembership){
	#apply membership of dynamic community as node attribute
	allnets=lapply(1:length(allnets),function(x){
		igraph::V(allnets[[x]])$DC=dcmembership[[x]][match(igraph::V(allnets[[x]])$name,names(dcmembership[[x]]))]
		allnets[[x]]
	})
	return(allnets)
}

#' Get dataframe of dynamic community changes
#'
#' Generate data frame of movement, splits, merges and remains between dynamic
#'    communities.
#'
#'
#' @param track A MajorTrack object as produced by \code{\link{do_track}}
#' @param allremains Boolean indicating if remain events from dynamic
#'     communities undergoing no change should be included in the dataframe.
#'     Default is false.
#' @return A dataframe consisting of 6 columns.
#'     The first column "slice" indicates the timestep,
#'     starting from the second timestep.
#'
#'     The second column "parent" indicates which dynamic community in the
#'     previous timestep a "child" dynamic community (third column) emerged
#'     from in the current timestep. This will be NA if the vertex is involved
#'     in an immigration or emmigration event.
#'
#'
#'     The fourth column "type" shows the type of event.
#'     This can be a "merge" - a dynamic community being created due to two
#'     dynamic communities joining together, a "split" - a dynamic community
#'     emerging as the result of a single dynamic community splitting up,
#'     a "move" - some nodes moving from one previously existing dynamic
#'     community to previously existing another and
#'     finally a "remain" - nodes staying in the same dynamic community as the
#'     previous timestep.
#'
#'     By default, remain events only appear in the table if the parent/child
#'     is also involved in some other event.
#'
#'     The fifth colum "moveid" provides a unique identifier for each event,
#'     combining the timestep, parent and child columns.
#'
#'     The sixth column "size" shows how many vertices were involved in a particular event.
#'
#' @examples
#' data(allnets)
#' data(allcoms)
#' track = do_track(allnets, allcoms, history=1)
#' move_events_df(track)
move_events_df=function(track,allremains=F){

	dcmembership=get_dc_membership(track)


	##Build dataframe of movement, splits, merges and remains between DCs.
	#get all split events
	newfromsplits=do.call(rbind,lapply(1:length(track$community_splits),function(slice){
		currslice=track$community_splits[[slice]]
		if(length(currslice)==0){
			return()
		}
		do.call(rbind,lapply(currslice,function(x){
			data.frame(slice=slice,parent=x[[1]],child=x[[2]],type=I("split"))
		}))
	}))

	#get all merge events
	merges=do.call(rbind,lapply(1:length(track$community_merges),function(slice){
		currslice=track$community_merges[[slice]]
		if(length(currslice)==0){
			return()
		}
		do.call(rbind,lapply(currslice,function(x){
			data.frame(slice=slice,parent=x[[1]],child=x[[2]],type=I("merge"))
		}))

	}))


	#combine
	comorigins=rbind(newfromsplits,merges)

	#create unique move id
	comorigins$moveid=paste(comorigins$slice,comorigins$parent,comorigins$child)

	#reorder based on move ID, then slice
	comorigins=comorigins[order(comorigins$moveid),]
	comorigins=comorigins[order(comorigins$slice),]

	#where we have identical splits between and merges between groups set type to move
	comorigins$type[comorigins$moveid%in%comorigins$moveid[duplicated(comorigins$moveid)]]="move"

	#create remain category when parent and child are the same
	comorigins$type[comorigins$parent==comorigins$child]="remain"

	#remove the duplicate moves - also removes duplicate remains
	comorigins=comorigins[!duplicated(comorigins$moveid)|comorigins$type%in%c("split","merge"),]

	#get size of moves
	comorigins$size=sapply(1:nrow(comorigins),function(x){
		cevent=comorigins[x,]
		cslice=dcmembership[[cevent$slice]]
		#individuals in child event
		cmembers=names(cslice[cslice==cevent$child])
		pslice=dcmembership[[cevent$slice-1]]
		#individuals in parent event
		pmembers=names(pslice[pslice==cevent$parent])
		#individuals
		cmembers=cmembers[cmembers%in%pmembers]
		length(cmembers)
	})

	if(allremains){
  	memdf=ind_membership_df(dcmembership)
  	newremains=lapply(unique(comorigins$slice),function(x){
  	  currinds=memdf[[1]][memdf[[1]]$timestep==x,]
  	  allgroups=unique(currinds$group)
  	  previnds=memdf[[1]][memdf[[1]]$timestep==x-1,]
  	  allprevgroups=unique(previnds$group)
  	  #get groups existing in previous timestep but not in comorigins
  	  missinggroups=allgroups[!allgroups%in%comorigins$child[comorigins$slice==x]&allgroups%in%allprevgroups]
  	  if(length(missinggroups)>0){
  	    data.frame(slice=x,parent=missinggroups,child=missinggroups,type="remain",moveid=paste(x,missinggroups,missinggroups),size=sapply(missinggroups,function(y){sum(currinds$group==y)}))
  	  }

   })
  	newremains=do.call(rbind,newremains)
  	comorigins=rbind(comorigins,newremains)

  	#reorder based on move ID, then slice
  	comorigins=comorigins[order(comorigins$moveid),]
  	comorigins=comorigins[order(comorigins$slice),]
	}

	#ADD EMMIGRATION AND IMMIGRATION
	#For each parent check for node IDs that were not in the previous timestep but are in this one


	#For each child check for node IDs that are in this timestep, but not in this one



	return(comorigins)
}

#' @export
ind_membership_df=function(dcmembership){
	##A dataframe of individual DC membership per timestep
	memdf=do.call(rbind,lapply(1:length(dcmembership),function(x){
		data.frame(id=names(dcmembership[[x]]),timestep=x,group=dcmembership[[x]])
	}))

	##A matrix of individual group membership/presence over time
	allids=unique(memdf$id)
	allgroups=unique(memdf$group)
	alltimesteps=unique(memdf$timestep)
	memdf2=do.call(cbind,lapply(alltimesteps,function(x){
		cgroup=memdf$group[memdf$timestep==x][match(allids,memdf$id[memdf$timestep==x])]
		cgroup[!is.na(cgroup)]=cgroup[!is.na(cgroup)]
		cgroup[is.na(cgroup)]=NA
		return(cgroup)
	}))
	row.names(memdf2)=allids
	colnames(memdf2)=alltimesteps
	return(list(memdf1=memdf,memdf2=memdf2))
}

#' @export
community_lifespans=function(track){
  unlist(track$community_lifespans)
}


#' @export
get_similarities=function(track){
  gs=track$group_similarities

  allsim=data.frame()
  for(i in 1:length(gs)){
    currgs=gs[[i]]

    currgsb = currgs$backward
    currgsf = currgs$forward
    names(currgsb)=get_DC_names(track,names(currgsb),i)
    names(currgsf)=get_DC_names(track,names(currgsf),i)

    back=lapply(names(currgsb),function(j){
      currgroup = currgsb[[j]]
      if(is.null(currgroup)){
        return (NULL)
      }
      names(currgroup)=get_DC_names(track,names(currgroup),i-1)
      data.frame(timestep=i,group1=j,direction=I("backward"),timestep2=i-1,group2=names(currgroup),similarity=unlist(currgroup))
    })
    back=do.call(rbind,back)

    forward=lapply(names(currgsf),function(j){
      currgroup = currgsf[[j]]
      if(is.null(currgroup)){
        return (NULL)
      }
      names(currgroup)=get_DC_names(track,names(currgroup),i+1)
      data.frame(timestep=i,group1=j,direction=I("forward"),timestep2=i+1,group2=names(currgroup),similarity=unlist(currgroup))
    })
    forward=do.call(rbind,forward)
    allsim=rbind(allsim,back,forward)
  }
  return (allsim)
}


#' @export
get_alluvialplot=function(track,dcmembership,allcols,fluxbysource=T,fluxsinglecol=T,fluxmovecol="grey",fluxsinglecolremain=T,fluxremaincol="grey",
         fluxalpha=0.4,figwidth=8,figheight=2,rlabels=NULL,rstart=NULL,rstop=NULL,
         rmargins=c(0,0.2,1,1),
         cwidth=0.2,clusterlw=0.5,
         labelsize=0,
         reimport=T,removefile=T,exportfilename="Rplot.png")
  {

  cols2=coldictionary(track,allcols)

  fluxcols1=get_flux_colors(track=track,dcmembership=dcmembership,allcols,cols2,
                            fluxbysource,
                          singlecol=fluxsinglecol,movecol=fluxmovecol,
                          singlecolremain=fluxsinglecolremain,remaincol=fluxremaincol)

    if(is.null(rstop)){
      rstop=length(track$dcs)
    }
    if(is.null(rstart)){
      rstart=0
    }
    if(is.null(rlabels)){
      rlabels=c(1:length(track$dcs))
    }

    mt$R_make_figure(track,cols2,figwidth,figheight,rmargins=rmargins,rstart=rstart,rstop=rstop,cwidth=cwidth,clusterlw=clusterlw,rlabels=rlabels,
                exportfilename=exportfilename,labelsize=labelsize,
                fluxalpha=fluxalpha,fluxfacecolor=fluxcols1$col,fluxfacefrom=fluxcols1$fromlab,fluxfaceto=fluxcols1$tolab,fluxfacets=fluxcols1$time)
    if(reimport){
      opar<-par(no.readonly=TRUE)
      on.exit(par(opar),add=TRUE,after=FALSE)
      alluplot=imager::load.image(exportfilename)
      par(mai=c(0,0,0,0))
      plot(alluplot,axes=F,rescale=T,xaxs="i",yaxs="i",asp="varying")
      rimsize=dev.size("in")

      par(mai=c(rmargins[2]*rimsize[2],
                (rmargins[1]*rimsize[1])+(cwidth*((1+rstart)-1)),
                (1-rmargins[4])*rimsize[2],
                ((1-rmargins[3])*rimsize[1])+(cwidth*((rstop)-length(track$dcs)))
                ),new=T)
      plot(0,type="n",xaxs="i",yaxs="i",xlim=c(rstart+1.5,rstop+1.5)-1,ylim=c(0,1),xlab="",ylab="",axes=F)

    }
    if(removefile&file.exists(exportfilename)){
      invisible(file.remove(exportfilename))
    }
  }

