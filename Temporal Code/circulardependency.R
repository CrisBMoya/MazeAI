library(keypress)

One=function(){
  
  print("One")
  keypress()
  Two()
  
}

Two=function(){
  print("TWo")
  keypress()
  One()
  
  }


One()
