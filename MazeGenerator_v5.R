#This script should create a maze flaweslly and quickly.
#It mus perform better than before by avoiding unnvesesary steps in the creation of the maze
#So big mazes are created more quickly and they look better.

#Previous versionas have fundamental problems: they often fail to fill greater than 52%,
#which is actually OK for some sizes, but not for others.

#Also, the maze exit is really bad. It used to be random on the last row, so some mazes
#are created without exit!

#Times of creation arent well either, sometimes takes up too much time


##CHANGES ARE ACTUALLY WORST!

library(ggplot2)
library(reshape)

#Take a random step
RandomStep=function(Row, Col, DF, Direction=NULL){
  N=Row-1
  S=Row+1
  W=Col-1
  E=Col+1
  
  #Prevent falling from table and illegal positions
  if(N<=0 | S<=0 | W<=0 | E<=0 | N>ncol(DF) | E>nrow(DF) | S>ncol(DF) | W>nrow(DF)){
    return(list(DF, c(Row,Col)))
  }
  
  #Create vector for positions  
  Pos=c(DF[N,Col], DF[S,Col], DF[Row,W], DF[Row,E])
  
  #Random move or selected move
  if(is.null(Direction)){
    Step=sample(x=c('N','S','W','E'), size=1)
  }else{
    Step=Direction
  }
  
  #Make a step to a random direction
  StepAndPos=MovingRules(CurrentStep=Step, DF=DF, Row=Row, Col=Col, 
                         N=N, S=S, W=W, E=E)
  
  #Return location of new step
  return(list(StepAndPos[[1]], StepAndPos[[2]]))
}

#Set of rules to move
MovingRules=function(CurrentStep, DF, Row, Col, N, S, W, E){
  
  #Move to south
  if(CurrentStep=='S' && S+1 <= nrow(DF) && DF[S+1,Col]==0 && DF[S,Col]==0 && 
     DF[Row+1,W]==0 && DF[Row+1,E]==0){
    DF[S,Col]=1
    return(list(DF, c(S,Col)))
    
    #Move to north
  }else if(CurrentStep=='N' && N-1 > 0 && DF[N,Col]==0 && DF[N-1,Col]==0 && 
           DF[Row-1,W]==0 && DF[Row-1,E]==0){
    DF[N,Col]=1
    return(list(DF, c(N,Col)))
    
    #Move to east
  }else if(CurrentStep=='E' && E+1 <= ncol(DF) && DF[N,Col+1]==0 && DF[S,Col+1]==0 && 
           DF[Row,E]==0 && DF[Row,E+1]==0){
    DF[Row,E]=1
    return(list(DF, c(Row,E))) 
    
    #Move to west
  }else if(CurrentStep=='W' && W-1 > 0 && DF[N,Col-1]==0 && DF[S,Col-1]==0 && 
           DF[Row,W]==0 && DF[Row,W-1]==0){
    DF[Row,W]=1
    return(list(DF, c(Row,W))) 
    
    #Move to south when cell was already visited
  }else if(CurrentStep=='S' && DF[S,Col]==1){
    return(list(DF, c(S,Col)))
    
    #Move to north when cell was already visited
  }else if(CurrentStep=='N' && DF[N,Col]==1){
    return(list(DF, c(N,Col)))
    
    #Move to east when cell was already visited
  }else if(CurrentStep=='E' && DF[Row,E]==1){
    return(list(DF, c(Row,E)))
    
    #Move to west when cell was already visited
  }else if(CurrentStep=='W' && DF[Row,W]==1){
    return(list(DF, c(Row,W)))
    
    #Dont move in case the random movement doesn't follow the rules
  }else{
    return(list(DF, c(Row,Col)))
  }
}



MazeSize=25
FilledPercent=50
CutNumber=10
{
  OneProportion=0
  i=0
  while(OneProportion<FilledPercent){
    i=i+1
    if(i==1){
      #Create empty Maze matrix
      Maze=matrix(data=0, nrow=MazeSize, ncol=MazeSize)
      
      #Create entrance for maze
      InCol=rbinom(n=1, size=floor(ncol(Maze)/2), prob=0.6)
      Maze[1,InCol]=1
      Maze[2,InCol]=1
      LastPos=c(2,InCol)
    }
    
    #Take a random step
    RandomRes=RandomStep(Row=LastPos[1], Col=LastPos[2], DF=Maze)
    Maze=RandomRes[[1]]
    LastPos=RandomRes[[2]]
    
    #Sthuffle. It choose a random point in the maze and start walking again
    #This is what makes the maze complex, otherwise it would be really simple
    if(i%%100==0){
      ##Check proportions
      MeltedMaze=melt(Maze)
      
      #Check proportion -- it usually never surpasses 50%
      OneProportion=floor(sum(MeltedMaze$value)*100/nrow(MeltedMaze))
      
      #If maze is filled more than a certain percent, then stop generating
      if(OneProportion>=FilledPercent){
        #If stopping early, create exit
        ExitCol=rbinom(n=1, size=floor(ncol(Maze)/2), prob=0.75)
        Maze[nrow(Maze),ExitCol]=1
        Maze[(nrow(Maze)-1),ExitCol]=1
        
        print(paste0(FilledPercent,"% proportion reached at iteration ", i))
      }
      
      #Try New Seed
      SubMaze=Maze[(2:MazeSize-1),(2:MazeSize-1)]
      MeltSubMaze=melt(SubMaze)
      FilterMeltSubMaze=MeltSubMaze[MeltSubMaze$value==1,]
      
      Cond=TRUE
      Count=0
      while(Cond==TRUE){
        #If 4 cells has been chosen and no one has at least 3 0 surrounding,
        #then stop the while loop and choose another random position
        if(Count==1){
          Cond=FALSE
          NewSeed=FALSE
        }
        #Select random cell with value 1
        ChooseRowCol=FilterMeltSubMaze[sample(x=nrow(FilterMeltSubMaze), size=1),]
        
        #Obtain coordinates of the cell with value 1
        LocQ=c(ChooseRowCol$X1, ChooseRowCol$X2)
        
        #Check that LocQ isnt falling out of borders
        if(LocQ[1]+1<=nrow(SubMaze) && LocQ[2]+1 <=ncol(SubMaze) &&
           LocQ[1]-1>=1 && LocQ[2]-1 >= 1){
          
          #Check if it has at least 3 0 surrounding it
          if(sum(c(SubMaze[LocQ[1]+1,LocQ[2]],
                   SubMaze[LocQ[1]-1,LocQ[2]],
                   SubMaze[LocQ[1],LocQ[2]-1],
                   SubMaze[LocQ[1],LocQ[2]+1]))==1){
            
            Cond=FALSE
            NewSeed=LocQ
            #return(TEMPRES)
            #If not, try again with other cell chosen at random
          }else{
            Count=1+Count
          }
        }else{
          Count=Count+1
        }
      }
      ###### End of New Seed
      
      if(length(NewSeed)==1 && NewSeed==FALSE){
        LastRandom=MeltedMaze[which(MeltedMaze$value==1),]
        
        #Try to avoid shuffling in already complex parts
        Balance=as.numeric(as.character(cut(x=rowSums(Maze), breaks=CutNumber, labels=1:CutNumber)))
        R1=sample(x=which(Balance<ceiling(CutNumber/2)), size=1)
        
        #Make an step
        if(any((LastRandom$X1)>=R1)){
          LastRandom=LastRandom[sample(x=which(LastRandom$X1 >= R1), size=1),]
          LastPos=c(LastRandom$X1, LastRandom$X2)
        }else{
          LastRandom=LastRandom[sample(x=nrow(LastRandom), size=1),]
          LastPos=c(LastRandom$X1, LastRandom$X2)
        }
        
      }else{
        #If new seed was succesfull, then use it
        LastPos=NewSeed
      }
    }
  }
}

#Default parameters
# PointsLabel=1
# 
# #Default colors
# NameVect=c('black','white','red')
# names(NameVect)=c(0,1,2)
# 
# MeltedMaze=melt(Maze)
# MazePlot=ggplot(data=MeltedMaze, aes(x=X2, y=X1)) +
#   geom_raster(aes(fill=as.factor(value)), show.legend=FALSE) +
#   scale_fill_manual(values=NameVect)+
#   scale_y_reverse() +
#   theme_void() +
#   coord_fixed() +
#   annotate(geom='text', x=max(MeltedMaze$X2)+2, y=min(MeltedMaze$X1),
#            label=paste0('Points: ', PointsLabel),
#            color='black')
# MazePlot

