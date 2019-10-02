rm(list=ls())

suppressMessages({
  library(keypress)
  library(tidyverse)
})

#Create Maze
setwd(gsub(pattern='Documents', replacement='Google Drive/Github/MazeAI/', x=getwd()))
source(file='MazeGenerator_v6.R')

#Set Parameters
SizeOfMaze=25
PercentFill=50

#Default first user step
Maze=MazeGen(MazeSize=SizeOfMaze, FilledPercent=PercentFill, ShuffleNum=50, PercentTolerance=1000, verbose=FALSE)

#Save this particular maze
write_delim(x=as.data.frame(Maze), path='TemporalMaze.txt', delim='\t', col_names=FALSE)


##Visualize the maze
#Default parameters
PointsLabel=1

#Default colors
NameVect=c('black','white','red')
names(NameVect)=c(0,1,2)

MeltedMaze=melt(Maze)
MazePlot=ggplot(data=MeltedMaze, aes(x=X2, y=X1)) +
  geom_raster(aes(fill=as.factor(value)), show.legend=FALSE) +
  scale_fill_manual(values=NameVect)+
  scale_y_reverse() +
  theme_void() +
  coord_fixed() +
  annotate(geom='text', x=max(MeltedMaze$X2)+2, y=min(MeltedMaze$X1),
    label=paste0('Points: ', PointsLabel),
    color='black')
MazePlot

##Preparing reinforcement
Gamma=0.75
Alpha=0.85



#Reward matrix
###################
RewardMatrix=matrix(data=0, nrow=(dim(Maze)[1]*dim(Maze)[2]), ncol=(dim(Maze)[1]*dim(Maze)[2]))
colnames(RewardMatrix)=paste0('L',1:ncol(RewardMatrix))
rownames(RewardMatrix)=paste0('L',1:nrow(RewardMatrix))
LetterMatrix=matrix(data=paste0('L',1:(dim(Maze)[1]*dim(Maze)[2])), nrow=dim(Maze)[1], ncol=dim(Maze)[2])
LetterMatrix=t(LetterMatrix)

PosRow=MeltedMaze[MeltedMaze$value==1,]$X1
PosCol=MeltedMaze[MeltedMaze$value==1,]$X2

for(i in 1:length(PosRow)){
  Pos=c(PosRow[i],PosCol[i])
  if(Pos[1]+1 > nrow(Maze) | Pos[1]-1 == 0 | Pos[2]+1 > ncol(Maze) | Pos[2]-1 == 0){
    print('Fault')
  }else{
    
    if(Maze[Pos[1]+1,Pos[2]]==1){
      RewardMatrix[row.names(RewardMatrix)==LetterMatrix[Pos[1]+1,Pos[2]],
        colnames(RewardMatrix)==LetterMatrix[Pos[1],Pos[2]]]=1
      RewardMatrix[rownames(RewardMatrix)==LetterMatrix[Pos[1],Pos[2]],
        colnames(RewardMatrix)==LetterMatrix[Pos[1]+1,Pos[2]]]=1
    }
    if(Maze[Pos[1]-1,Pos[2]]==1){
      RewardMatrix[row.names(RewardMatrix)==LetterMatrix[Pos[1]-1,Pos[2]],
        colnames(RewardMatrix)==LetterMatrix[Pos[1],Pos[2]]]=1
      RewardMatrix[rownames(RewardMatrix)==LetterMatrix[Pos[1],Pos[2]],
        colnames(RewardMatrix)==LetterMatrix[Pos[1]-1,Pos[2]]]=1
    }
    if(Maze[Pos[1],Pos[2]+1]==1){
      RewardMatrix[row.names(RewardMatrix)==LetterMatrix[Pos[1],Pos[2]+1],
        colnames(RewardMatrix)==LetterMatrix[Pos[1],Pos[2]]]=1
      RewardMatrix[rownames(RewardMatrix)==LetterMatrix[Pos[1],Pos[2]],
        colnames(RewardMatrix)==LetterMatrix[Pos[1],Pos[2]+1]]=1
    }
    if(Maze[Pos[1],Pos[2]-1]==1){
      RewardMatrix[row.names(RewardMatrix)==LetterMatrix[Pos[1],Pos[2]-1],
        colnames(RewardMatrix)==LetterMatrix[Pos[1],Pos[2]]]=1
      RewardMatrix[rownames(RewardMatrix)==LetterMatrix[Pos[1],Pos[2]],
        colnames(RewardMatrix)==LetterMatrix[Pos[1],Pos[2]-2]]=1
    }
  }
  
}
# for(i in 1:length(PosRow)){
#   Pos=c(PosRow[i],PosCol[i])
#   if(Pos[1]+1 > nrow(Maze) | Pos[1]-1 == 0 | Pos[2]+1 > ncol(Maze) | Pos[2]-1 == 0){
#     print('Fault')
#   }else{
#     
#     if(Maze[Pos[1]+1,Pos[2]]==1){
#       RewardMatrix[rownames(RewardMatrix)==LetterMatrix[Pos[1],Pos[2]],
#         colnames(RewardMatrix)==LetterMatrix[Pos[1]+1,Pos[2]]]=1
#     }
#     if(Maze[Pos[1]-1,Pos[2]]==1){
#       RewardMatrix[rownames(RewardMatrix)==LetterMatrix[Pos[1],Pos[2]],
#         colnames(RewardMatrix)==LetterMatrix[Pos[1]-1,Pos[2]]]=1
#     }
#     if(Maze[Pos[1],Pos[2]+1]==1){
#       RewardMatrix[rownames(RewardMatrix)==LetterMatrix[Pos[1],Pos[2]],
#         colnames(RewardMatrix)==LetterMatrix[Pos[1],Pos[2]+1]]=1
#     }
#     if(Maze[Pos[1],Pos[2]-1]==1){
#       RewardMatrix[rownames(RewardMatrix)==LetterMatrix[Pos[1],Pos[2]],
#         colnames(RewardMatrix)==LetterMatrix[Pos[1],Pos[2]-2]]=1
#     }
#   }
#   
# }

#MeltReward=melt(RewardMatrix)
#MeltReward[MeltReward$value==1,]

########################
#States matrix
States=data.frame('State'=paste0('L',1:ncol(RewardMatrix)), 'Value'=1:ncol(RewardMatrix), stringsAsFactors=FALSE)
##Begining

#Copy Reward Matrix
CopyRewardMatrix=RewardMatrix
#Define end state
EndState=LetterMatrix[nrow(Maze), which(Maze[nrow(Maze),]==1)]
CopyRewardMatrix[which(rownames(CopyRewardMatrix)==EndState[1]), which(colnames(CopyRewardMatrix)==EndState[1])]=999
MeltedReward=melt(CopyRewardMatrix)
MeltedReward=MeltedReward[MeltedReward$value!=0,]
##Q-Learning algorithm
#Define Q marix
Q=matrix(data=0, nrow=(dim(Maze)[1]*dim(Maze)[2]), ncol=(dim(Maze)[1]*dim(Maze)[2]))
for(i in 1:100000){
  
  #Define CUrrent state randomly
  Current_State=sample(x=MeltedReward$X1, size=1)
  Current_State=as.character(Current_State)
  Current_State=States[States$State==Current_State,]$Value
  
  ##Based on a random position, find which is the next random playeable position
  
  #Iterate though the colnames to see which parts of the matrix are playeable.
  #As we already choose a row, only the column is left
  PlayPos=which(CopyRewardMatrix[Current_State,]>0)
  Next_State=as.numeric(sample(x=PlayPos, size=1))
  
  #Define temporal difference
  TD=CopyRewardMatrix[Current_State, Next_State] + Gamma * Q[Next_State, which(Q[Next_State,]==max(Q[Next_State,]))[1]] - Q[Current_State, Next_State]
  
  Q[Current_State, Next_State] = Q[Current_State, Next_State] + (Alpha * TD)
}

#
# Route=list()
# Start_Location=LetterMatrix[1,which(Maze[1,]==1)]
# Route[[1]]=Start_Location
# 
# for(i in 2:100000){
#   StartState=States[States$State==Start_Location,]$Value
#   StartState
#   Next_State=which(Q[StartState,]==max(Q[StartState,]))[1]
#   Next_State
#   Q[StartState,]
#   Next_State
#   Start_Location=States[States$Value==Next_State,]$State
#   Start_Location
#   Route[[i]]=Start_Location
# }
# unique(unlist(Route))
# 
# LetterMatrix
# Maze
##################
###################
End_Loc=LetterMatrix[nrow(Maze),which(Maze[nrow(Maze),]==1)]
Route=list()
Start_Location=LetterMatrix[1,which(Maze[1,]==1)]
Route[[1]]=Start_Location
Count=1
while(End_Loc!=Start_Location){
  Count=Count+1
  if(Count%%1000==0){
    print(paste0('Iteration ', Count))
  }
  StartState=States[States$State==Start_Location,]$Value
  Next_State=which(Q[StartState,]==max(Q[StartState,]))[1]
  Start_Location=States[States$Value==Next_State,]$State
  Route[[Count]]=Start_Location
}

#Plot Route
MeltedLetterMatrix=melt(LetterMatrix)
MeltedLetterMatrix=MeltedLetterMatrix[MeltedLetterMatrix$value %in% unlist(Route),]
for(i in 1:nrow(MeltedLetterMatrix)){
  Maze[MeltedLetterMatrix$X1[i], MeltedLetterMatrix$X2[i]]=2
}

MeltedMaze=melt(Maze)
MazePlot=ggplot(data=MeltedMaze, aes(x=X2, y=X1)) +
  geom_raster(aes(fill=as.factor(value)), show.legend=FALSE) +
  scale_fill_manual(values=NameVect)+
  scale_y_reverse() +
  theme_void() +
  coord_fixed() +
  annotate(geom='text', x=max(MeltedMaze$X2)+2, y=min(MeltedMaze$X1),
    label=paste0('Points: ', PointsLabel),
    color='black')
MazePlot
