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
#' #detect each networks communities
#' coms=lapply(allnets,function(x){
#'   igraph::cluster_louvain(x)
#' })
#' track = do_track(allnets, coms, history=1)
#' @export
do_track=function(allnets, allcoms,historypar=2){
  allpyids = get_pyids(allnets) #convert to a list of individual IDs for each timestep in python style
  allpycoms=get_com_pyids(allcoms)#get community memberships in python style foreach timestep
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
#' @return A list, the length of which is the number of timesteps included in
#'    the MajorTrack object. Each element of the list is a numeric vector
#'    indicating which dynamic community a vertex is currently a member of.
#'
#'
#' @examples
#' data(allnets)
#' #detect each networks communities
#' coms=lapply(allnets,function(x){
#'   igraph::cluster_louvain(x)
#' })
#' track = do_track(allnets, coms, history=1)
#' get_dc_membership(track)
#' @export
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
#' @param dcmembership A list of dynamic community membership as produced by
#'     \code{\link{get_dc_membership}}
#' @return The provided list of igraph networks with the added vertex attribute
#'    "DC" indicating which dynamic community that vertex is currently a member
#'    of.
#'
#' @examples
#' data(allnets)
#' #detect each networks communities
#' coms=lapply(allnets,function(x){
#'   igraph::cluster_louvain(x)
#' })
#' track = do_track(allnets, coms, history=1)
#' dcmembership = get_dc_membership(track)
#' #add vertex attribute to allnets, overwriting original allnets
#' allnets = add_dc_membership(allnets, dcmembership)
#' @export
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
#'	 parent2 and child2 give the internally used group ID for a DC. Some DCs can have
#'	 multiple groups assigned to them.
#'
#'     The column "moveid" provides a unique identifier for each event,
#'     combining the timestep, parent and child columns.
#'
#'     The sixth column "size" shows how many vertices were involved in a particular event.
#'
#' @examples
#' data(allnets)
#' #detect each networks communities
#' coms=lapply(allnets,function(x){
#'   igraph::cluster_louvain(x)
#' })
#' track = do_track(allnets, coms, history=1)
#' move_events_df(track)
#' @export
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

	#add extra labels

	grouplabs=lapply(unique(comorigins$slice),function(x){
		parent2=data.frame(type=I("parent"),slice=x,get_cluster_names(track,
			inputnames=unique(comorigins$parent[comorigins$slice==x]),
			x-1))
		child2=data.frame(type=I("child"),slice=x,get_cluster_names(track,
			inputnames=unique(comorigins$child[comorigins$slice==x]),
			x))
		rbind(parent2,child2)
	})
	grouplabs=do.call(rbind,grouplabs)

	#check for missing GROUPS that have been assigned to the same DC

	comorigins=lapply(unique(comorigins$slice),function(x){
		currdat=comorigins[comorigins$slice==x,]
		#multiple parents
		multiparents=lapply(1:nrow(currdat),function(y){
			multilabs=grouplabs[grouplabs$slice==x&grouplabs$type=="parent"&grouplabs$DC==currdat$parent[y],]
			newdf=currdat[y,]
			newdf=data.frame(newdf,parent2=multilabs$cluster,row.names=NULL)
			newdf
		})
		multiparents=do.call(rbind,multiparents)
		multichild=lapply(1:nrow(multiparents),function(y){
			multilabs=grouplabs[grouplabs$slice==x&grouplabs$type=="child"&grouplabs$DC==multiparents$child[y],]
			newdf=multiparents[y,]
			newdf=data.frame(newdf,child2=multilabs$cluster,row.names=NULL)
			newdf
		})
		do.call(rbind,multichild)
	})

	comorigins=do.call(rbind,comorigins)

	#create unique move id
	comorigins$moveid=paste(comorigins$slice,comorigins$parent,comorigins$child,comorigins$parent2,comorigins$child2)

	#reorder based on move ID, then slice
	comorigins=comorigins[order(comorigins$moveid),]
	comorigins=comorigins[order(comorigins$slice),]

	#where we have identical splits between and merges between groups set type to move
	comorigins$type[comorigins$moveid%in%comorigins$moveid[duplicated(comorigins$moveid)]]="move"

	#create remain category when parent and child are the same
	comorigins$type[comorigins$parent2==comorigins$child2]="remain"

	#remove the duplicate moves - also removes duplicate remains
	comorigins=comorigins[!duplicated(comorigins$moveid)|comorigins$type%in%c("split","merge"),]





	if(allremains){
  	memdf=ind_membership_df(track)
  	newremains=lapply(2:max(memdf$memdf1$timestep),function(x){
  	  currinds=memdf[[1]][memdf[[1]]$timestep==x,]
  	  allgroups=unique(currinds$group)
  	  previnds=memdf[[1]][memdf[[1]]$timestep==x-1,]
  	  allprevgroups=unique(previnds$group)
  	  #get groups existing in previous timestep but not in comorigins
  	  missinggroups=allgroups[!allgroups%in%comorigins$child[comorigins$slice==x]&allgroups%in%allprevgroups]
  	  if(length(missinggroups)>0){
  	    	df1=data.frame(slice=x,parent=missinggroups,child=missinggroups,type="remain")

		parent2=data.frame(type=I("parent"),slice=x,get_cluster_names(track,
			inputnames=unique(df1$parent[df1$slice==x]),
			x-1))
		child2=data.frame(type=I("child"),slice=x,get_cluster_names(track,
			inputnames=unique(df1$child[df1$slice==x]),
			x))
		grouplabs=rbind(parent2,child2)

		#check for missing GROUPS that have been assigned to the same DC

		currdat=df1
		#multiple parents
		multiparents=lapply(1:nrow(currdat),function(y){
			multilabs=grouplabs[grouplabs$slice==x&grouplabs$type=="parent"&grouplabs$DC==currdat$parent[y],]
			newdf=currdat[y,]
			newdf=data.frame(newdf,parent2=multilabs$cluster,row.names=NULL)
			newdf
		})
		multiparents=do.call(rbind,multiparents)
		multichild=lapply(1:nrow(multiparents),function(y){
			multilabs=grouplabs[grouplabs$slice==x&grouplabs$type=="child"&grouplabs$DC==multiparents$child[y],]
			newdf=multiparents[y,]
			newdf=data.frame(newdf,child2=multilabs$cluster,row.names=NULL)
			newdf
		})
		multichild=do.call(rbind,multichild)
		multichild$moveid=paste(multichild$slice,multichild$parent,multichild$child,multichild$parent2,multichild$child2)
		return(multichild)
  	  }

   	})
  	newremains=do.call(rbind,newremains)
  	comorigins=rbind(comorigins,newremains)

  	#reorder based on move ID, then slice
  	comorigins=comorigins[order(comorigins$moveid),]
  	comorigins=comorigins[order(comorigins$slice),]
	}

	#get size of moves
	comorigins$size=sapply(1:nrow(comorigins),function(x){
		cevent=comorigins[x,]
		cslice=unlist(track$individual_group_membership[[cevent$slice]])
		#individuals in child event
		cmembers=names(cslice[cslice==cevent$child2])
		pslice=unlist(track$individual_group_membership[[cevent$slice-1]])
		#individuals in parent event
		pmembers=names(pslice[pslice==cevent$parent2])
		#individuals
		cmembers=cmembers[cmembers%in%pmembers]
		length(cmembers)
	})

	#remove moves with zero members
	comorigins=comorigins[comorigins$size>0,]



	#ADD EMMIGRATION AND IMMIGRATION
	#For each parent check for node IDs that were not in the previous timestep but are in this one


	#For each child check for node IDs that are in this timestep, but not in this one



	return(comorigins)
}

#' Get summaries of individual dynamic community membership
#'
#' Generate summaries of individual vertices' dynamic community membership over
#'     time.
#'
#'
#' @param track A MajorTrack object as produced by \code{\link{do_track}}.
#'     Not required if dcmembership is provided.
#' @param dcmembership A list of dynamic community membership as produced by
#'     \code{\link{get_dc_membership}}. Not required if track is provided.
#' @return A list consisting of two objects, each showing individual dynamic
#'   community membership in a different way.
#'
#'    The first object 'memdf1' is a dataframe giving timestep, individual ID,
#'    dynamic community membership and internal group ID as 4 columns.
#'	This is useful for computation.
#'
#'    The second object 'memdf2' is a matrix with a row for
#'    each individual and a column for each timestep.
#'    This is an easily readable way of looking at how an individual's membership
#'    changes over time.
#'
#' @examples
#' data(allnets)
#' #detect each networks communities
#' coms=lapply(allnets,function(x){
#'   igraph::cluster_louvain(x)
#' })
#' track = do_track(allnets, coms, history=1)
#' indmembership=ind_membership_df(track)
#'
#' #use a dummy variable to look at how many IDs are in each combination of timestep and groupsize
#'
#' groupsizes=aggregate(rep(1,nrow(indmembership$memdf1))~
#'     group+timestep,FUN=sum,data=indmembership$memdf1,drop=FALSE)
#' names(groupsizes)[3]="groupsize"
#' groupsizes$groupsize[is.na(groupsizes$groupsize)]=0
#' groupsizes
#'
#' #matrix of individual memberships
#' head(indmembership$memdf2)
#' @export
ind_membership_df=function(track=NULL,dcmembership=NULL){
  if(is.null(dcmembership)){
    if(is.null(track)){
      stop("No MajorTrack object or dynamic community membership")
    }else{
      dcmembership=get_dc_membership(track)
    }
  }
	##A dataframe of individual DC membership per timestep
	memdf=do.call(rbind,lapply(1:length(dcmembership),function(x){
		df1=data.frame(id=names(dcmembership[[x]]),timestep=x,group=dcmembership[[x]])
		cslice=unlist(track$individual_group_membership[[x]])
		df1$group2=cslice[match(df1$id,names(cslice))]
		df1
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


#' Get dynamic community lifespans
#'
#' Summary of how many timesteps each dynamic community in a MajorTrack object
#'     exists for.
#'
#'
#' @param track A MajorTrack object as produced by \code{\link{do_track}}.
#' @return Named vector giving how many timesteps each dynamic community
#'     is present for.
#'
#' @examples
#' data(allnets)
#' #detect each networks communities
#' coms=lapply(allnets,function(x){
#'   igraph::cluster_louvain(x)
#' })
#' track = do_track(allnets, coms, history=1)
#' community_lifespans(track)
#' @export
community_lifespans=function(track){
  unlist(track$community_lifespans)
}


#' Get similarity in membership between communities
#'
#' Summary of the similarity in membership between dynamic communities in
#'     different timesteps, both forwards and backwards.
#'
#'
#' @param track A MajorTrack object as produced by \code{\link{do_track}}.
#' @return Dataframe of 6 columns.
#'
#'     The first column 'timestep' is the current timestep, to which
#'     timesteps forwards and backwards are being compared (except in the case
#'     of the first timestep).
#'
#'     The second column 'group1' is the dynamic community to which
#'     dynamic communities in other timesteps are being compared.
#'
#'     The third column 'direction' shows which direction the comparison is
#'     being made, either 'forwards' or 'backwards'
#'
#'     The fourth column 'timestep2' indicates in which other timestep the
#'     comparison in being made.
#'
#'     The fifth column 'group2' is the dynamic community in 'timestep2' to to
#'     which the dynamic community 'group1' is being compared to in 'timestep'
#'
#'     The sixth column shows the similarity in membership between the two
#'     dynamic communities as a proportion. A value of 0 indicates that the
#'     compared communities contain none of the same members while a value of 1
#'     means the communities are exactly the same.
#'
#' @examples
#' data(allnets)
#' coms=lapply(allnets,function(x){
#'   igraph::cluster_louvain(x)
#' })
#' track = do_track(allnets, coms, history=1)
#' get_similarities(track)
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


#' Plot alluvial plot of dynamic community membership
#'
#' Plots dynamic communities and the movements between them as an alluvial plot
#' \url{https://en.wikipedia.org/wiki/Alluvial_diagram}
#'
#'
#' @param track A MajorTrack object as produced by \code{\link{do_track}}.
#' @param allcols A vector of colours, the same length at the number of unique
#'     dynamic communities in the MajorTrack object. Defaults to rainbow
#'     colours.
#' @param fluxbysource Boolean. If TRUE, the connecting fluxes between timesteps
#'     will be coloured the same as the dynamic community from which they
#'     originate. Defaults to TRUE.
#' @param fluxsinglecol Boolean. If TRUE, the connecting fluxes between
#'     timesteps will be a single colour as defined by \code{fluxmovecol}.
#'     Overrides fluxbysource.
#' @param fluxmovecol Single colour of movements between different dynamic
#'     communities in different timesteps. Defaults to "grey".
#' @param fluxsinglecolremain Boolean. If TRUE fluxes between the same dynamic
#'     community in different timesteps will be a single colour as defined by
#'     \code{fluxremaincol}. Defaults to TRUE.
#' @param fluxremaincol Single colour for remaining in the same dynamic
#'     community in different timesteps. Defaults to "grey".
#' @param fluxalpha Transparency of fluxes, where 1 is opaque and 0 is
#'     invisible. Defaults to 0.4
#' @param figwidth Width of figure in inches. Defaults to 8.
#' @param figheight height of figure in inches. Defaults to 2.
#' @param rlabels Vector of labels that, if supplied, will override the
#'     timestep labels.
#' @param rstart Integer specifying timestep at which to start plotting.
#' @param rstop Integer specifying timestep at which to stop plotting.
#' @param rmargins Vector of numbers between 0 and 1 in the format
#'     \code{c(left,bottom,right,top)}. Specifies a rectangle in which the
#'     alluvial plot (blocks and fluxes) is placed in the overall plotting area.
#'     By default the alluvial plot takes up the entirety of the plotting area,
#'      except for the bottom where a gap is left for the
#'      labels: \code{c(0,0.2,1,1)}.
#' @param cwidth Number between 0 and 1 defining cluster width. 0 will hide
#'     clusters, 1 will leave no room between clusters. Defaults to 0.2
#' @param clusterlw Number between 0 and 1 defining line width of cluster
#'     borders.
#'     Defaults to 0.5. Currently has no effect.
#' @param labelsize Number defining size of per cluster labels. Defaults to 0,
#'     hiding labels.
#' @param reimport Boolean. If TRUE, the python figure will be imported into R.
#'     Defaults to TRUE.
#' @param removefile Boolean. If TRUE, the exported python figure will be
#'     deleted when the function is finished. Defaults to TRUE.
#' @param exportfilename String giving the name and path of the exported file.
#'     Defaults to "Rplot.png"
#'
#' @details The R function for the MT alluvial plots is a wrapper for the
#'     python plotting functions.
#'
#'     If reimport is TRUE it will run the plotting
#'     code, export an image then bring it back into R.
#'     By default the image file will then be removed.
#'
#'     The code attempts to place the R margins and axis in the same positions
#'     as those in the python plot. There is a chance this might fail on some
#'     graphics devices. If succesful the resulting plotting device will have
#'     an x-axis where each tick is a timestep.
#'
#'     Combining \code{fluxbysource}, \code{fluxsinglecol} and
#'     \code{fluxsinglecolremain} will produce different effects.
#'     \code{fluxsinglecol} being TRUE will colour all fluxes the
#'     same. If \code{fluxsinglecolremain} is also TRUE, this will result in
#'     remain fluxes being one colour and all other fluxes (moves, splits,
#'     merges) being another colour. If \code{fluxsinglecol} is FALSE and
#'     \code{fluxbysource} is TRUE then fluxes will be coloured by the community
#'     they originate from.  and all other fluxes (moves, splits,
#'     merges) being another colour. If \code{fluxsinglecol} is FALSE and
#'     \code{fluxbysource} is also FALSE then fluxes will be coloured by
#'     the community they are heading toward. Both these combinations can
#'     further be combined with \code{fluxsinglecolremain} being TRUE to
#'     separate remain fluxes and all other fluxes.
#'
#'
#' @examples
#' data(allnets)
#' data(allnets)
#' #detect each networks communities
#' coms=lapply(allnets,function(x){
#'   igraph::cluster_louvain(x)
#' })
#' track = do_track(allnets, coms, history=1)
#' get_similarities(track)
#' @export
get_alluvialplot=function(track,allcols=NULL,
                          fluxbysource=T,
                          fluxsinglecol=T,
                          fluxmovecol="grey",
                          fluxsinglecolremain=T,
                          fluxremaincol="grey",
                          fluxalpha=0.4,
                          figwidth=8,figheight=2,rlabels=NULL,
                          rstart=NULL,rstop=NULL,
                          rmargins=c(0,0.2,1,1),
                          cwidth=0.2,clusterlw=0.5,
                          labelsize=-1,
                          reimport=T,removefile=T,
                          exportfilename="Rplot.png")
  {
  dcmembership=get_dc_membership(track)

  if(is.null(allcols)){
    allcols=grDevices::rainbow(length(track$comm_all))
  }

  cols2=coldictionary(track,allcols)

  fluxcols1=get_flux_colors(track=track,allcols,cols2,
                            bysource=fluxbysource,
                          singlecol=fluxsinglecol,movecol=fluxmovecol,
                          singlecolremain=fluxsinglecolremain,remaincol=fluxremaincol)

    if(is.null(rstop)){
      rstop=length(track$dcs)
    }
    if(is.null(rstart)){
      rstart=0
    }else{
	    rstart=rstart-1
	  }
    if(is.null(rlabels)){
      rlabels=c(1:length(track$dcs))
    }
    cat(c(rstart,rstop))
    cat("\n")

    mt$R_make_figure(track,cols2,figwidth,figheight,rmargins=rmargins,rstart=rstart,rstop=rstop,cwidth=cwidth,clusterlw=clusterlw,rlabels=rlabels,
                exportfilename=exportfilename,labelsize=labelsize,
                fluxalpha=fluxalpha,fluxfacecolor=fluxcols1$col,fluxfacefrom=fluxcols1$fromlab,fluxfaceto=fluxcols1$tolab,fluxfacets=fluxcols1$time)
    if(reimport){
      #opar<-par(no.readonly=TRUE)
      #on.exit(par(opar),add=TRUE,after=FALSE)
      alluplot=imager::load.image(exportfilename)
      graphics::par(mai=c(0,0,0,0))
      graphics::plot(alluplot,axes=F,rescale=T,xaxs="i",yaxs="i",asp="varying")
      rimsize=grDevices::dev.size("in")

      newmai=c(rmargins[2]*rimsize[2],
               (rmargins[1]*rimsize[1])+(cwidth*((1+rstart)-1)),
               (1-rmargins[4])*rimsize[2],
               ((1-rmargins[3])*rimsize[1])+(cwidth*((rstop)))
      )
      cat(paste(rmargins))
      cat("\n")
      cat(paste(rimsize))
      cat("\n")
      cat(newmai)
      cat("\n")
      graphics::par(mai=newmai,new=T)
      graphics::plot(0,type="n",xaxs="i",yaxs="i",xlim=c(rstart+1.5,rstop+1.5)-1,ylim=c(0,1),xlab="",ylab="",axes=F)

    }
    if(removefile&file.exists(exportfilename)){
      invisible(file.remove(exportfilename))
    }
  }

