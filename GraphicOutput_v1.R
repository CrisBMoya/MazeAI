cat("----------------------------------------------------------------------------------------")
WaitUser=readLines(con="stdin", 1)

#Generate an structure
BuildBlocks=c("|","-")

#Aldous-Broder Algorithm
Maze=matrix(data=0, nrow=20, ncol=10)



#Neighboring
StepVariant=c(1,1)
for(i in 1:200){
  
  #Generate random value and regularize between 1 and 2
  Step=rbinom(n=1, size=2, prob=0.5)
  if(Step==0){
    Step=1
  }else if(Step==3){
    Step=2
  }
  
  #Update Step value
  StepVariant[Step]=StepVariant[Step]+sample(x=c(-1,1), size=1)
  
  #Default Sense
  Sense='OK'
  
  while(Sense=='OK'){
    #Check if 1 or 2 where chosen in order to move to that direction and update StepVariant
    if(Step==1){
      #Check if cell has 1 or is limit of maze
      Sense=tryCatch({Maze[StepVariant[Step],StepVariant[2]]},
        error=function(e){
          return('Limit')
        })
      
      #Make the move
      if(Sense!="Limit"){
        Maze[StepVariant[Step],StepVariant[2]]=1
      }else{
        #Return Step Value
        StepVariant[Step]=StepVariant[Step]-1
      }
      
    }else{
      #Check if cell has 1 or is limit of maze
      Sense=tryCatch({Maze[StepVariant[1],StepVariant[Step]]},
        error=function(e){
          return('Limit')
        })
      
      #Make the move
      if(Sense!="Limit"){
        Maze[StepVariant[1],StepVariant[Step]]=1
      }else{
        #Return Step Value
        StepVariant[Step]=StepVariant[Step]-1
      }
      
    }
  }
}
Maze
####################################
StartArray=list()
EndArray=list()

StartArray[[1]]=c(1,1)
EndArray[[1]]=c(0,0)

for(i in 1:(ncol(Maze)*nrow(Maze))){
  #Generate random value and regularize between 1 and 2
  Step=rbinom(n=1, size=2, prob=0.5)
  if(Step==0){
    Step=1
  }else if(Step==3){
    Step=2
  }
  #Populate List
  StartArray[[i+1]]=c(0,0)
  
  EndArray[[i]]=StartArray[[i]]
  #Move
  if(Step==1){
    EndArray[[i]][1]=StartArray[[i]][1]+1
    StartArray[[i+1]]=EndArray[[i]]
  }else{
    EndArray[[i]][2]=StartArray[[i]][2]+1
    StartArray[[i+1]]=EndArray[[i]]
  }
}

#Populate maze
for(i in 1:length(StartArray)){
  Maze[StartArray[[i]][1],StartArray[[i]][2]]=1
  
}
Maze
rbinom(n=1, size=1, prob=0.5)
###########################################
#Neighboring
StepVariant=c(1,1)
Maze[1,1]=1
for(i in 1:500){
  
  #Generate random value and regularize between 1 and 2
  Step=rbinom(n=1, size=2, prob=0.5)
  if(Step==0){
    Step=1
  }else if(Step==3){
    Step=2
  }
  
  #Update Step value
  StepVariant[Step]=abs(StepVariant[Step]+sample(x=c(-1,1), size=1))

  #Default Sense
  Sense='OK'
  Count=0
  while(Sense=='OK'){
    Count=Count+1
    #print(paste0("While count: ", Count))
    #Check if 1 or 2 where chosen in order to move to that direction and update StepVariant
    if(Step==1){
      
      #Check if cell has 1 or is limit of maze
      Sense=tryCatch({Maze[StepVariant[Step],StepVariant[2]]},
        error=function(e){
          return('Limit')
        })
      
      #Check if any its 0
      if(any(c(StepVariant[Step],StepVariant[2])<=0)){
        Sense='Limit'
      }
      
      #Make the move
      if(Sense!="Limit" & Sense!=1){
        Maze[StepVariant[Step],StepVariant[2]]=1
      }else{
        #Return Step Value
        StepVariant[Step]=StepVariant[Step]-1
      }
      #print(c(StepVariant[Step],StepVariant[2]))
      #print(paste0('Sense=',Sense))
    }else{
      #Check if cell has 1 or is limit of maze
      Sense=tryCatch({Maze[StepVariant[1],StepVariant[Step]]},
        error=function(e){
          return('Limit')
        })
      
      #Check if any its 0
      if(any(c(StepVariant[1],StepVariant[Step])<=0)){
        Sense='Limit'
      }
      
      #Make the move
      if(Sense!="Limit" & Sense!=1){
        Maze[StepVariant[1],StepVariant[Step]]=1
      }else{
        #Return Step Value
        StepVariant[Step]=StepVariant[Step]-1
      }
      #print(c(StepVariant[1],StepVariant[Step]))
      #print(paste0('Sense=',Sense))
      
    }
  }
}
warnings()
Maze
tryCatch({Maze[5,3]},
  error=function(e){
    return('Limit')
  })
Sense
Maze[3,0]
StepVariant[Step]
StepVariant[2]
Cond='OK'
Count=0
while(Cond=='OK'){
  Count=Count+1  
  if(Count==10){
    Cond='NO'
    }
  }
Count
