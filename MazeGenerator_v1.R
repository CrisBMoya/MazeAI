#Basic maze generator
#This script was a test, trying to get some insights about generating a maze
#It really didnt work well

library(ggplot2)
library(reshape)


#Set rules
MazeConstructor=function(Row, Col, DF){
  N=Row-1
  S=Row+1
  W=Col-1
  E=Col+1
  
  if(N<=0 | S<=0 | W<=0 | E<=0 | N>ncol(DF) | E>nrow(DF) | S>ncol(DF) | W>nrow(DF)){
    #print('Negative or out of border')
    return(DF)
  }
  
  Pos=c(DF[N,Col], DF[S,Col], DF[Row,W], DF[Row,E])
  #print(Pos)
  
  if(sum(Pos==0)<=3 & sum(Pos==0)>1){
    Step=sample(x=1:4, size=1)
    
    #Step
    if(Step==1){ #Will edit Rows, moving from North or South
      #print(paste0('Moving North to ', N, ', ', Col ,' from ', Row,', ',Col))
      DF[N,Col]=1
      return(DF)
    }else if(Step==2){
      #print(paste0('Moving East to ', Row, ', ', E ,' from ', Row,', ',Col))
      DF[Row,E]=1
      return(DF)  
    }else if(Step==3){
      #print(paste0('Moving South to ', S, ', ', Col ,' from ', Row,', ',Col))
      DF[S,Col]=1
      return(DF) 
    }else if(Step==4){
      #print(paste0('Moving West to ', Row, ', ', W ,' from ', Row,', ',Col))
      DF[Row,W]=1
      return(DF) 
    }
    
  } else {
    #print('Theres no position to advance')
    return(DF)
  }
}


#Generate a maze
Maze=matrix(data=0, nrow=10, ncol=10)
Maze[5,5]=1
####
for(i in 1:20000){

  Maze=MazeConstructor(Row=abs(floor(rnorm(n=1, mean=nrow(Maze)/2.5, sd=nrow(Maze)/2.5))), 
    Col=abs(floor(rnorm(n=1, mean=nrow(Maze)/2.5, sd=nrow(Maze)/2.5))), 
    DF=Maze)
  #print(Maze)
}
Maze
MazeConstructor(Row=2, Col=1, DF=Maze)

MeltedMaze=melt(Maze)
ggplot(data=MeltedMaze, aes(x=X1, y=X2, fill=value)) + geom_tile()
