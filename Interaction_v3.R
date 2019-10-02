#This works well enough. It moves and updates a plot
#There are rules for moving, so you can only move within the plot and never above walls

#Adapting code so it would be able to use it from R directly or through command line.
#Also with the option to show or not the graphical maze.
#There are optional usefull reporting returns.

suppressMessages({
  library(keypress)
  library(tidyverse)
})

#Create Maze
setwd(gsub(pattern='Documents', replacement='Google Drive/Github/MazeAI/', x=getwd()))
source(file='MazeGenerator_v6.R')

#Set Parameters
SizeOfMaze=15
PercentFill=50

#Default first user step
Maze=MazeGen(MazeSize=SizeOfMaze, FilledPercent=PercentFill, ShuffleNum=50, PercentTolerance=1000, verbose=FALSE)
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

UpdatePlot=function(MazeDF, Loc, Count, PointsLabel, Plot, InputType){
  #Count
  Count=1+Count
  
  #Plot or not de maze
  if(Plot==TRUE){
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
  }
  
  #Reporting
  write_lines(x=paste0(PointsLabel,' ', paste0(Loc, collapse=',')), 
    path='C:/Users/Tobal/Desktop/Reporting.txt', append=TRUE)
  
  #Check if the user won the game
  WinGame(MazeDF=MazeDF, Loc=Loc)
  
  #Check if the user lose the game
  LoseGame(PointsLabel=PointsLabel, SizeOfMaze=SizeOfMaze, PercentFill=PercentFill)
  
  #Call the other function, who will wait until a key is pressed, so no eternal loop is created out of control
  UserInput(MazeDF=MazeDF, Loc=Loc, Count=Count, PointsLabel=PointsLabel, Plot=Plot, InputType=InputType)
}

UserInput=function(MazeDF, Loc, Count, PointsLabel, Plot, InputType){
  #Wait for user input. 
  #This allow us to control the flow of the circular dependency between the functions.
  if(InputType=='Classic'){
    UserKey=keypress()
  }else if(InputType=="ReadLines"){
    TempKey=readLines(con="stdin", n=1)
    UserKey=KeyReplace(x=TempKey)
  }
  
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
    
    if(Plot==TRUE){
      #Close open device to exit safely the game
      dev.off()
      stopQuietly()
    }else{
      stopQuietly()
    }
  }else{
    PointsLabel=PointsLabel-1
    MazeDF[Loc[1],Loc[2]]=2
  }
  
  #Show plot with new values
  UpdatePlot(MazeDF=MazeDF, Loc=Loc, Count=Count, PointsLabel=PointsLabel, Plot=Plot, InputType=InputType)
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

#Lose game condition
LoseGame=function(PointsLabel, SizeOfMaze, PercentFill){
  LoseCondition=(SizeOfMaze^2)*(PercentFill/100)
  if(abs(PointsLabel)>=LoseCondition){
    print('You have taken too much steps! You lose!')
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

#Key replacement for non interactive use
KeyReplace=function(x){
  if(x=='w'){
    x='up'
    return(x)
  }else if(x=='s'){
    x='down'
    return(x)
  }else if(x=='a'){
    x='left'  
    return(x)
  }else if(x=='d'){
    x='right'  
    return(x)
  }else if(x=='q'){
    x='escape'
    return(x)
  }
}

#Start
Count=0
PointsLabel=0
PlotOpt=FALSE
InputType='ReadLines'
UpdatePlot(MazeDF=reMaze, Loc=OriginalLocation, Count=Count, PointsLabel=PointsLabel, 
  Plot=PlotOpt, InputType=InputType)
