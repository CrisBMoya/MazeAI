#This works well enough. It moves and updates a plot
#There are rules for moving, so you can only move within the plot and never above walls

library(keypress)

#Create Maze
setwd(gsub(pattern='Documents', replacement='Google Drive/Github/MazeAI/', x=getwd()))
source(file='MazeGenerator_v6.R')


#Default first user step
Maze=MazeGen(MazeSize=20, FilledPercent=50, ShuffleNum=100, PercentTolerance=1000, verbose=FALSE)
reMaze=Maze
OriginalLocation=c(1,which(reMaze[1,]==1))
reMaze[OriginalLocation[1],OriginalLocation[2]]=2

##############################################
#The next two functions create a circular dependency
#If you call one, then the other is automatically called.
#When the other is called, then it automatically call the first one again.
#They pass the arguments between each other so the plot can be updated.
#

#Default colors
NameVect=c('black','white','red')
names(NameVect)=c(0,1,2)

UpdatePlot=function(MazeDF, Loc, Count, PointsLabel){
  #Count
  Count=1+Count
  #print(Loc)
  
  #Save argument as temporal objetc
  MazeDF=MazeDF
  
  #Melt DF and plot
  reMelt=melt(MazeDF)
  reMazePlot=ggplot(data=reMelt, aes(x=X2, y=X1)) + 
    geom_raster(aes(fill=as.factor(value)), show.legend=FALSE) +  
    scale_fill_manual(values=NameVect)+
    scale_y_reverse() +
    theme_void() + 
    coord_fixed() + 
    annotate(geom='text', x=max(reMelt$X2)+2, y=min(reMelt$X1), 
      label=paste0('Points: ', PointsLabel),
      color='black')
  
  #Show Maze in command line
  if(Count==1){
    windows()
  }
  print(reMazePlot)
  print(reMazePlot)
  
  #Check if the user won the game
  WinGame(MazeDF=MazeDF, Loc=Loc)
  
  #Call the other function, who will wait until a key is pressed, so no eternal loop is created ou of control
  UserInput(MazeDF=MazeDF, Loc=Loc, Count=Count, PointsLabel=PointsLabel)
  
} 

UserInput=function(MazeDF, Loc, Count, PointsLabel){
  #Wait for user input. 
  #This allow us to control the flow of the circular dependency between the functions.
  UserKey=keypress()
  
  #If series and rules who control where to move
  if(UserKey=='down' && Loc[1]+1<=nrow(MazeDF) && MazeDF[Loc[1]+1,Loc[2]]!=0){
    MazeDF[Loc[1],Loc[2]]=1 #Return previous tile to original color
    Loc=c(Loc[1]+1, Loc[2]) #Update Loc object
    MazeDF[Loc[1],Loc[2]]=2 #Change maze color tile
    
    
  }else if(UserKey=='up' && Loc[1]-1>=1 && MazeDF[Loc[1]-1,Loc[2]]!=0){
    MazeDF[Loc[1],Loc[2]]=1
    Loc=c(Loc[1]-1, Loc[2])
    MazeDF[Loc[1],Loc[2]]=2
    
  }else if(UserKey=='left' && Loc[2]-1>=1 && MazeDF[Loc[1],Loc[2]-1]!=0){
    MazeDF[Loc[1],Loc[2]]=1
    Loc=c(Loc[1], Loc[2]-1)
    MazeDF[Loc[1],Loc[2]]=2
    
  }else if(UserKey=='right' && Loc[2]+1<=ncol(MazeDF) && MazeDF[Loc[1],Loc[2]+1]!=0){
    MazeDF[Loc[1],Loc[2]]=1
    Loc=c(Loc[1], Loc[2]+1)
    MazeDF[Loc[1],Loc[2]]=2
    
  }else if(UserKey=='escape'){
    #Close open device to exit safely the game
    dev.off()
    stopQuietly()
  }else{
    PointsLabel=PointsLabel+1
    MazeDF[Loc[1],Loc[2]]=2
  }
  
  #Show plot with new values
  UpdatePlot(MazeDF=MazeDF, Loc=Loc, Count=Count, PointsLabel=PointsLabel)
  
  return(list(UserKey,Loc))
}

#Win game condition
WinTile=c(nrow(Maze), which(Maze[nrow(Maze),]==1))
WinGame=function(MazeDF, Loc){
  if(all(Loc==WinTile)){
    print('Congratulations! You have WON!')
    print('Press any key to exit game')
    keypress()
    dev.off()
    stopQuietly()
  } 
}

#Stolen from internet. Apparently by Henrik Bengtsson
stopQuietly=function(...) {
  blankMsg=sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
  stop(simpleError(blankMsg));
}

#Start
Count=0
PointsLabel=0
UpdatePlot(MazeDF=reMaze, Loc=OriginalLocation, Count=Count, PointsLabel=PointsLabel)

