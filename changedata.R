for(i in 1:nrow(mydf)){
    if(is.na(mydf[[i,1]])){
      tmp <- summarized_by_interval[which(summarized_by_interval$interval == mydf[[i,3]]),2]
      mydf[i,1] <-  tmp
    }
}