
get_pyids=function(allnets){
  #convert IDs into a python style


  allpyids=(lapply(allnets,function(x){
    ids=V(x)$name
    ids2=paste0("'",ids,"'")
    ids3=paste0(ids2,collapse=",")
    cmdstring=paste0("pyids=set({",ids3,"})")
    reticulate::py_run_string(cmdstring)
    reticulate::py$pyids
  }))
  return(allpyids)
}

get_com_pyids=function(coms){
  #convert community membership into python style

  allpycoms=lapply(coms,function(x){
    lapply(1:length(x),function(y){
      ids=x[[y]]
      ids2=paste0("'",ids,"'")
      ids3=paste0(ids2,collapse=",")
      cmdstring=paste0("pycoms=set({",ids3,"})")
      reticulate::py_run_string(cmdstring)
      reticulate::py$pycoms
    })
  })
  return(allpycoms)
}


get_DC_names=function(track,inputnames,timestep){
  track$comm_group_members[[timestep]]
  allnames=lapply(1:length(track$comm_group_members[[timestep]]),function(x){
    data.frame(DC=rep(names(track$comm_group_members[[timestep]])[[x]],length(track$comm_group_members[[timestep]][[x]])),
               cluster=track$comm_group_members[[timestep]][[x]])
  })
  allnames=do.call(rbind,allnames)
  return(allnames$DC[match(inputnames,allnames$cluster)])
}

get_cluster_names=function(track,inputnames,timestep){
  track$comm_group_members[[timestep]]
  allnames=lapply(1:length(track$comm_group_members[[timestep]]),function(x){
    data.frame(DC=rep(names(track$comm_group_members[[timestep]])[[x]],length(track$comm_group_members[[timestep]][[x]])),
               cluster=track$comm_group_members[[timestep]][[x]])
  })
  allnames=do.call(rbind,allnames)
  return(allnames$cluster[match(inputnames,allnames$DC)])
}


get_flux_colors=function(track,allcols,cols2,singlecol=F,movecol="red",bysource=T,singlecolremain=T,remaincol="grey"){
  #get a per slice colour vector set up for python


  allflux=move_events_df(track,T)

  fluxcols1=lapply(unique(allflux$slice),function(x){
    currslice=allflux[allflux$slice==x,]

    currflux=data.frame(time=x-2,source=currslice$parent,target=currslice$child,fromlab=get_cluster_names(track,currslice$parent,x-1),tolab=get_cluster_names(track,currslice$child,x))
    if(singlecol){
      currflux$col=rgb(t(col2rgb(movecol)),maxColorValue = 255)
    }
    if(bysource&!singlecol){
      currflux$col=allcols[match(as.numeric(currslice$parent),track$comm_all)]
    }else if(!singlecol){
      currflux$col=allcols[match(as.numeric(currslice$child),track$comm_all)]
    }
    if(singlecolremain){

      #remains in grey
      currflux$col[currslice$parent==currslice$child]=rgb(t(col2rgb(remaincol)),maxColorValue = 255)
    }
    currflux
  })
  fluxcols1=do.call(rbind,fluxcols1)
  return (fluxcols1)
}

coldictionary=function(track,allcols){
  cols1=lapply(1:length(track$dcs),function(x){
    currdcs=track$dcs[[x]]#
    cols2=allcols[match(currdcs,track$comm_all)]
    reticulate::py_dict(cols2,keys=0:(length(currdcs)-1))
  })

  #nest this dictionary
  cols2=reticulate::py_dict(cols1,keys=0:(length(track$dcs)-1))
  return(cols2)
}
