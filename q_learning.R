#shortest path
ShortestPath <- function(traffic,N,target_x,target_y){
  
  shortest_path = matrix(N*N*10,N,N)
  shortest_path[target_x,target_y]=0
  identical_count = 100
  
  while(identical_count>0){
    shortest_path_temp = shortest_path
    
    for(x in 1:N){
      for(y in 1:N){
        if(x==target_x & y==target_y){
          shortest_path[x,y] = 0
        }
        else{
          pos_paths = c()
          if(x!=1){
            pos_paths = append(pos_paths,traffic[x-1,y] + shortest_path_temp[x-1,y])
          }
          if(x!=N){
            pos_paths = append(pos_paths,traffic[x+1,y] + shortest_path_temp[x+1,y])
          }
          if(y!=1){
            pos_paths = append(pos_paths,traffic[x,y-1] + shortest_path_temp[x,y-1])
          }
          if(y!=N){
            pos_paths = append(pos_paths,traffic[x,y+1] + shortest_path_temp[x,y+1])
          }
          shortest_path[x,y] = min(pos_paths)
        }
      }
    }
    
    #check if shortest path has not changed for last 100 iterations
    if(identical(shortest_path,shortest_path_temp)){
      identical_count = identical_count - 1
    }
    else identical_count = 100
  }
  return(shortest_path)
}

#potential actions
PotentialMoves <- function(x,y){
  potential_moves = c()
  #check boundaries of the map to determine potential moves
  if(x!=1){
    potential_moves = append(potential_moves,1)
  }
  if(x!=N){
    potential_moves = append(potential_moves,2)
  }
  if(y!=1){
    potential_moves = append(potential_moves,3)
  }
  if(y!=N){
    potential_moves = append(potential_moves,4)
  }
  return(potential_moves)
}

#update position
UpdateLoc <- function(x,y,move){
  #update position based on the given move
  #1: up
  #2: down
  #3: left
  #4: right
  
  if(move==1){
    x = x-1
  }
  else if(move==2){
    x = x+1
  }
  else if(move==3){
    y = y-1
  }
  else if(move==4){
    y=y+1
  }
  return(c(x,y))
}

#maximum potential Q value
MaxPotentialQ <- function(Q,potential_moves,x,y){
  #select maximizing Q value action
  
  max_q = max(Q[x,y,potential_moves])
  max_q_loc = Reduce(intersect,list(which(Q[x,y,] == max_q),potential_moves))[1]
  Q_val_action = c(max_q_loc,max_q)
  
  return(Q_val_action)
}

#if only right turn is allowed, update potential moves
UpdatePotentialMoves <-function(potential_moves,move){
  #if previous move was going up, then going left is forbidden (if 1, forbid 3)
  #if previous move was going down, then going right is forbidden (if 2, forbid 4)
  #if previous move was going left, then going down is forbidden (if 3, forbid 2)
  #if previous move was going right, then going up is forbidden (if 4, forbid 1)
  
  if(move==1){
    return(potential_moves[potential_moves!=3])
  } else if(move==2){
    return(potential_moves[potential_moves!=4])
  } else if(move==3){
    return(potential_moves[potential_moves!=2])
  } else if(move==4){
    return(potential_moves[potential_moves!=1])
  } else return(potential_moves)
}

#Q-learning
QLearning <- function(Q,epsilon,demand,shortest_path,only_right){
  
  length_ratio = c()
  for(i in 1:demand_size){
    #epsilon decay
    eps = epsilon*((1/i)^(0.15))
    
    x=demand[i,1]
    y=demand[i,2]
    old_move = 0
    
    shortest_way = shortest_path[x,y]
    total_traffic = 0
    
    #loop until destination
    while(x!=target_x | y!=target_y){
      Q_temp = Q
      
      #identify potential moves
      potential_moves = PotentialMoves(x,y)
      
      #update if only right
      if(only_right){
        potential_moves  = UpdatePotentialMoves(potential_moves,old_move)
      }
      
      #explore
      if(eps > runif(1)){
        #pick random action
        new_move  = sample(potential_moves,1)
      }
      
      #exploit
      else{
        Q_val_action = MaxPotentialQ(Q_temp,potential_moves,x,y)
        new_move = Q_val_action[1]
      }
      
      new_loc = UpdateLoc(x,y,new_move)
      new_x = new_loc[1]
      new_y = new_loc[2] 
      
      
      potential_moves_next = PotentialMoves(new_x,new_y)
      
      if(only_right){
        potential_moves_next  = UpdatePotentialMoves(potential_moves_next,new_move)
      }
      
      Q_val_action_next = MaxPotentialQ(Q_temp,potential_moves_next,new_x,new_y)
      
      #learnin rate decay
      learning_rate = (1/i)^(0.15)
      
      Q[x,y,new_move] = (1-learning_rate)*Q_temp[x,y,new_move] + learning_rate*(-traffic[new_x,new_y]+Q_val_action_next[2])
      total_traffic = total_traffic + traffic[new_x,new_y]
      
      x = new_x
      y = new_y
      old_move = new_move
    }
    length_ratio = append(length_ratio,total_traffic/shortest_way)
  }
  return(length_ratio)
}

GenerateOptimisticQ <- function(q_opt){
  for(i in 1:N){
    for(j in 1:N){
      if(i>target_x){
        Q[i,j,1]=q_opt
      }
      if(i<target_x){
        Q[i,j,2]=q_opt
      }
      if(j>target_y){
        Q[i,j,3]=q_opt
      }
      if(j<target_y){
        Q[i,j,4]=q_opt
      }
      if(i==target_x & j==target_y){
        Q[i,j,]=q_opt
      }
    }
  }
  return(Q)
}

PickBestMoveHeuristic<-function(x,y,potential_moves,revealed_traffic,prev_move){
  #find actions that takes car to closer
  if(x>=target_x & y>=target_y){
    smart_moves = Reduce(intersect,list(c(1,3)),potential_moves)
  } else if(x>=target_x & y<=target_y){
    smart_moves = Reduce(intersect,list(c(1,4)),potential_moves)
  } else if(x<=target_x & y>=target_y){
    smart_moves = Reduce(intersect,list(c(2,3)),potential_moves)
  } else if(x<=target_x & y<=target_y){
    smart_moves = Reduce(intersect,list(c(2,4)),potential_moves)
  }
  #remove going back actions
  if(prev_move==1){
    smart_moves = smart_moves[smart_moves!=2]
  } else if(prev_move==2){
    smart_moves = smart_moves[smart_moves!=1]
  } else if(prev_move==3){
    smart_moves = smart_moves[smart_moves!=4]
  } else if(prev_move==4){
    smart_moves = smart_moves[smart_moves!=3]
  }
  #find traffics
  trafs = c()
  if(is.element(1,smart_moves)){
    trafs = append(trafs,revealed_traffic[x-1,y])
  }  
  if(is.element(2,smart_moves)){
    trafs = append(trafs,revealed_traffic[x+1,y])
  }  
  if(is.element(3,smart_moves)){
    trafs = append(trafs,revealed_traffic[x,y-1])
  } 
  if(is.element(4,smart_moves)){
    trafs = append(trafs,revealed_traffic[x,y+1])
  }
  #return smallest
  return(smart_moves[which.min(trafs)])
}

Heuristic <- function(demand, shortest_path){
  length_ratio = c()
  revealed_traffic = array(0,dim=c(N,N))
  for(i in 1:demand_size){
    
    x=demand[i,1]
    y=demand[i,2]
    prev_move = 0
    shortest_way = shortest_path[x,y]
    total_traffic = 0
    
    while(x!=target_x | y!=target_y){
      
      potential_moves = PotentialMoves(x,y)
      
      new_move = PickBestMoveHeuristic(x,y,potential_moves,revealed_traffic,prev_move)
      
      new_loc = UpdateLoc(x,y,new_move)
      new_x = new_loc[1]
      new_y = new_loc[2] 
      
      total_traffic = total_traffic + traffic[new_x,new_y]
      
      x = new_x
      y = new_y
      prev_move = new_move
      revealed_traffic[x,y] = traffic[x,y]
    }
    length_ratio = append(length_ratio,total_traffic/shortest_way)
  }
  return(length_ratio)
}

PlotLengths<- function(length_eps_greedy,length_opt_greedy,length_heuristic,length_opt_greedy_right,r){
  roll_mean_eps = c()
  roll_mean_opt = c()
  roll_mean_heur  =c()
  roll_mean_opt_right = c()
  for(i in r:length(length_eps_greedy)){
    roll_mean_eps[i-r] = mean(length_eps_greedy[i-r:i],na.rm=TRUE)
    roll_mean_opt[i-r] = mean(length_opt_greedy[i-r:i],na.rm=TRUE)
    roll_mean_heur[i-r] = mean(length_heuristic[i-r:i],na.rm=TRUE)
    roll_mean_opt_right[i-r] = mean(length_opt_greedy_right[i-r:i],na.rm=TRUE)
  }
  x = 1:length(roll_mean_eps)
  plot(x,log(roll_mean_eps),type = 'l',col='red',ylab='log length',xlab='demand')
  legend('topright',legend=c('eps-greedy'),col=c('red'),lty=1)
  
  x = 1:length(roll_mean_eps)
  plot(x[5000:14990],roll_mean_eps[5000:14990],type = 'l',col='red',ylab='length',xlab='demand')
  legend('topright',legend=c('eps-greedy'),col=c('red'),lty=1)
  
  plot(x,(roll_mean_opt),type = 'l',col='green',ylab='length',xlab='demand')
  lines(x,(roll_mean_heur),col='red')
  lines(x,(roll_mean_opt_right),col='blue')
  legend('topright',legend=c('opt-greedy','heuristic','Right-only'),col=c('green','red','blue'),lty=1:1)
}

#initialize setup
N=100
demand_size = 15000
traffic= matrix(sample(1:10,N*N,replace=TRUE),N,N)
demand = matrix(sample(1:N,demand_size*2,replace=TRUE),demand_size,2)
target_x = 1
target_y = 20

shortest_path = ShortestPath(traffic,N,target_x,target_y)

#e-greedy with 0 initials
Q = array(0,dim=c(N,N,4))
length_eps_greedy = QLearning(Q,epsilon=0.2,demand,shortest_path,FALSE)

#greedy with smart optimistic values
Q = GenerateOptimisticQ(500)
length_opt_greedy = QLearning(Q,epsilon=0,demand,shortest_path,FALSE)

#heuristics
length_heuristic = Heuristic(demand,shortest_path)

#only-right turn with optimistic q values
Q = GenerateOptimisticQ(500)
length_opt_greedy_right = QLearning(Q,epsilon=0,demand,shortest_path,TRUE)

#plot
PlotLengths(length_eps_greedy,length_opt_greedy,length_heuristic,length_opt_greedy_right,r=10)
