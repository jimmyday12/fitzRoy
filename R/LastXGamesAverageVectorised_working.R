LastXRoliing<-function(df,X=10,Variable=c("PTS","PACE"),FUN="mean"){
  
  df<-df[order(df$Date,df$GAME_ID),]
  
  ColsOfInterest<-unlist(lapply(Variable,function(x){which(colnames(df) == x)}))
  
  df2<-df[,c(which(colnames(df) %in% c("GAME_ID","TEAM_ABBREVIATION")),ColsOfInterest)]
  
  df2[,c(3:ncol(df2))]<-NA
  
  for (n in c(1:nrow(df))){
    if(n>1){
      PastGames<-df[c(1:n-1),]
      
      PastGamesThisTeam<-PastGames[which(PastGames$TEAM_ID==df$TEAM_ID[n]),]
      
      if(!nrow(PastGamesThisTeam)==0){
        StartRow<-ifelse(nrow(PastGamesThisTeam)<X,1,nrow(PastGamesThisTeam)-X+1)
        PastXGamesThisTeam<-PastGamesThisTeam[c(StartRow:nrow(PastGamesThisTeam)),]
        
        for (k in c(1:length(Variable))){
          df2[n,2+k]<-eval(parse(text=paste(FUN,"(PastXGamesThisTeam[,ColsOfInterest[k]])",sep="")))
        }
        
      }
      
    }
    #print(n) #Update progress of loop
  }
  
  colnames(df2)[c(3:ncol(df2))]<-paste(Variable,"Last",X,FUN,sep="_")
  
  return(df2)
}
