#This works well enough. It moves and updates a plot
#There arent rules for moving, so it essentially 'eat' some cells

library(keypress)

#Create Maze
source(file='Old Maze Generator Code/MazeGenerator_v4.R')


#Default first user step
reMaze=Maze
OriginalLocation=c(1,which(reMaze[1,]==1))
reMaze[OriginalLocation[1],OriginalLocation[2]]=2

##############################################
#The next two functions create a circular dependency
#If you call one, then the other is automatically called.
#When the other is called, then it automatically call the first one again.
#They pass the arguments between each other so the plot can be updated.
#

UpdatePlot=function(MazeDF, Loc, Count){
  #Count
  Count=1+Count
  print(Count)
  #Save argument as temporal objetc
  MazeDF=MazeDF
  
  #Melt DF and plot
  reMelt=melt(MazeDF)
  reMazePlot=ggplot(data=reMelt, aes(x=X2, y=X1, fill=value)) + geom_tile(show.legend=FALSE) +  
    scale_y_reverse() +
    theme_void() + coord_fixed()
  
  #Show Maze in command line
  if(Count==1){
    windows()
  }
  print(reMazePlot)
  print(reMazePlot)
  
  #Call the other function, who will wait until a key is pressed, so no eternal loop is created ou of control
  UserInput(MazeDF=MazeDF, Loc=Loc, Count=Count)
  
} 

UserInput=function(MazeDF, Loc, Count){
  
  #Wait for user input. 
  #This allow us to control the flow of the circular dependency between the functions.
  UserKey=keypress()
  
  #Close open device, otherwise they would stack
  #dev.off()
  
  #If series and rules who control where to move
  if(UserKey=='down'){
    MazeDF[Loc[1],Loc[2]]=1 #Return previous tile to original color
    Loc=c(Loc[1]+1, Loc[2]) #Update Loc object
    MazeDF[Loc[1],Loc[2]]=2 #Change maze color tile
  }else if(UserKey=='up'){
    MazeDF[Loc[1],Loc[2]]=1
    Loc=c(Loc[1]-1, Loc[2])
    MazeDF[Loc[1],Loc[2]]=2
  }else if(UserKey=='left'){
    MazeDF[Loc[1],Loc[2]]=1
    Loc=c(Loc[1], Loc[2]-1)
    MazeDF[Loc[1],Loc[2]]=2
  }else if(UserKey=='right'){
    MazeDF[Loc[1],Loc[2]]=1
    Loc=c(Loc[1], Loc[2]+1)
    MazeDF[Loc[1],Loc[2]]=2
  }
  
  #Show plot with new values
  UpdatePlot(MazeDF=MazeDF, Loc=Loc, Count=Count)
  
  return(list(UserKey,Loc))
}

Count=0
UpdatePlot(MazeDF=reMaze, Loc=OriginalLocation, Count=Count)

