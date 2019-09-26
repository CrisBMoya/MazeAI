#This script works well enough.
#It just create the maze based on iterations
#So, it could waste a lot of resources or take a lot of time

library(ggplot2)
library(reshape)

StepSize=function(Row, Col, Step){
  N=Row-Step
  S=Row+Step
  W=Col-Step
  E=Col+Step
  
  return(list('N'=N, 'S'=S, 'W'=W, 'E'=E))
}

RandomStep=function(Row, Col, DF, Direction=NULL){
  NSWE=StepSize(Row=Row, Col=Col, Step=1)
  
  #Prevent falling from table and illegal positions
  #print(NSWE)
  if(NSWE$N<=0 | NSWE$S<=0 | NSWE$W<=0 | NSWE$E<=0 | NSWE$N>ncol(DF) | NSWE$E>nrow(DF) | NSWE$S>ncol(DF) | NSWE$W>nrow(DF)){
    #print('Negative or out of border')
    return(list(DF, c(Row,Col)))
  }
  
  Pos=c(DF[NSWE$N,Col], DF[NSWE$S,Col], DF[Row,NSWE$W], DF[Row,NSWE$E])
  #print(Pos)
  
  #Random move or selected
  if(is.null(Direction)){
    Step=sample(x=c('N','S','W','E'), size=1)
  }else{
    Step=Direction
  }
  #print(paste0("Moving ", Step))
  
  #Step
  StepAndPos=MovingRules(CurrentStep=Step, DF=DF, Row=Row, Col=Col, 
                         N=NSWE$N, S=NSWE$S, W=NSWE$W, E=NSWE$E)
  
  return(list(StepAndPos[[1]], StepAndPos[[2]]))
}


MovingRules=function(CurrentStep, DF, Row, Col, N, S, W, E){
  if(CurrentStep=='S' && S+1 <= nrow(DF) && DF[S+1,Col]==0 && DF[S,Col]==0 && 
     DF[Row+1,W]==0 && DF[Row+1,E]==0){
    DF[S,Col]=1
    return(list(DF, c(S,Col)))
  }else if(CurrentStep=='N' && N-1 > 0 && DF[N,Col]==0 && DF[N-1,Col]==0 && 
           DF[Row-1,W]==0 && DF[Row-1,E]==0){
    DF[N,Col]=1
    return(list(DF, c(N,Col)))
  }else if(CurrentStep=='E' && E+1 <= ncol(DF) && DF[N,Col+1]==0 && DF[S,Col+1]==0 && 
           DF[Row,E]==0 && DF[Row,E+1]==0){
    DF[Row,E]=1
    return(list(DF, c(Row,E))) 
  }else if(CurrentStep=='W' && W-1 > 0 && DF[N,Col-1]==0 && DF[S,Col-1]==0 && 
           DF[Row,W]==0 && DF[Row,W-1]==0){
    DF[Row,W]=1
    return(list(DF, c(Row,W))) 
  }else if(CurrentStep=='S' && DF[S,Col]==1){
    return(list(DF, c(S,Col)))
  }else if(CurrentStep=='N' && DF[N,Col]==1){
    return(list(DF, c(N,Col)))
  }else if(CurrentStep=='E' && DF[Row,E]==1){
    return(list(DF, c(Row,E)))
  }else if(CurrentStep=='W' && DF[Row,W]==1){
    return(list(DF, c(Row,W)))
  }else{
    return(list(DF, c(Row,Col)))
  }
}

MazeSize=20
Runs=100000
for(i in 1:Runs){
  
  if(i==1){
    #Create Maze
    Maze=matrix(data=0, nrow=MazeSize, ncol=MazeSize)
    
    #Create entrance
    Maze[1,ncol(Maze)/2]=1
    Maze[2,ncol(Maze)/2]=1
    LastPos=c(2,ncol(Maze)/2)
  } else if(i==Runs){
    #Create exit
    Maze[nrow(Maze),ncol(Maze)/2]=1
    Maze[nrow(Maze),ncol(Maze)/2]=1
  }
  
  RandomRes=RandomStep(Row=LastPos[1], Col=LastPos[2], DF=Maze)
  Maze=RandomRes[[1]]
  LastPos=RandomRes[[2]]
  
  #Start again
  if(i%%500==0){
    MeltedMaze=melt(Maze)
    LastRandom=MeltedMaze[sample(x=which(MeltedMaze$value==1), size=1),]
    LastPos=c(LastRandom$X1, LastRandom$X2)
  }
}

MeltedMaze=melt(Maze)
MazePlot=ggplot(data=MeltedMaze, aes(x=X2, y=X1, fill=value)) + geom_tile(show.legend=FALSE) +  scale_y_reverse() +
  theme_void() + coord_fixed()

#dev.new(width=5, height=5, noRStudioGD=TRUE)
#png(filename="tempMaze.png", width=5, height=5)
windows()
print(MazePlot)
print(MazePlot)
#image(x=t(Maze), col=c("black","white"), xaxt="n", yaxt="n")
WaitUser=readLines(con="stdin", 1)
dev.off()
print(WaitUser)