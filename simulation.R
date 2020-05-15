setwd("~/Desktop/R/BPM")
N = 1000 # customers per simulation
S = 100 # number of trials


# Simulation Function: 
# 0) Arrival time & Service time follow exponential distribution
sim.time=function(nbuyer=100, 
                  prob.type=c(0.4, 0.6), 
                  pay.rate=c(40),
                  make.rate=c(40, 90), 
                  arrv.rate=c(3600/50), 
                  N.fn=function() 1, 
                  p.ratio=0){
  # make time: (Type1) 40sec in hour (Type2) 90sec in hour
  make.rate = make.rate/3600
  pay.rate = pay.rate/3600
  arrv.rate = arrv.rate/3600
  arr.time = rep(0, nbuyer+1)
  serv.time = rep(0, nbuyer+1)
  type=matrix(0, nrow=2, ncol=nbuyer+1)

  for(i in 2:(nbuyer+1)){
    # Arrival time
    arr.time[i] = arr.time[i-1] + rexp(1, 1/arrv.rate)
    
    # Calculate service time
    # drinks type
    N = N.fn()
    t = sample(c(1, 2), N , T, prob=prob.type)
    type[1, i] = length(which(t==1))
    type[2, i] = length(which(t==2))
    
    # sum up all services
    serv.time[i] = rexp(1, 1/pay.rate) * 
      ifelse(p.ratio>0, sample(c(0, 1), T, prob=c(p.ratio, 1-p.ratio)), 1) +  # Payment skip
      type[1, i] * rexp(1, 1/make.rate[1]) + 
      type[2, i] * rexp(1, 1/make.rate[2])
  }
  out=list(ArrivalTime=arr.time[-1], 
           ServiceTime=serv.time[-1], 
           Type=type[, -1])
}

# 1) Customer balking threshold
Q.fn=function(qseq=c(1:5), nbuyer=100){
  q = sample(c(qseq, Inf), nbuyer, 
             prob=c(rep(1/length(qseq), length(qseq)),0.5), replace=T)
  return(q)
}

# 2) Drink amount simulate (sampling)
N.fn=function(maxN){ # No argument
  other.p = 0.5/(maxN-1)
  n = sample(c(1:maxN), 1, F, prob=c(0.5, rep(other.p, (maxN-1))))
  return(n)
}

# 3) Arrival time & service time are constant time
library(truncnorm)
hist(rtruncnorm(1000, 45, Inf, 90, 10)) # Say making type 1 drink with normal
sim.time.norm=function(nbuyer=100, 
                  prob.type=c(0.4, 0.6), 
                  pay.rate=c(40),
                  make.rate=c(40, 90),
                  make.std=c(10, 10),
                  arrv.rate=c(3600/50),
                  N.fn=function() 1, 
                  p.ratio=0){
  # make time: (Type1) 40sec in hour (Type2) 90sec in hour
  make.rate = make.rate/3600
  make.std = make.std/3600
  pay.rate = pay.rate/3600
  arrv.rate = arrv.rate/3600
  arr.time = rep(0, nbuyer+1)
  serv.time = rep(0, nbuyer+1)
  type=matrix(0, nrow=2, ncol=nbuyer+1)
  
  for(i in 2:(nbuyer+1)){
    # Arrival time
    arr.time[i] = arr.time[i-1] + rexp(1, 1/arrv.rate)
    
    # Calculate service time
    # drinks type
    N = N.fn()
    t = sample(c(1, 2), N , T, prob=prob.type)
    type[1, i] = length(which(t==1))
    type[2, i] = length(which(t==2))
    
    # sum up all services
    serv.time[i] = rexp(1, 1/pay.rate) * 
      ifelse(p.ratio>0, sample(c(0, 1), T, prob=c(p.ratio, 1-p.ratio)), 1) +  # Payment skip
      type[1, i] * rtruncnorm(1, make.rate[1]/2, Inf, make.rate[1], make.std[1]) + 
      type[2, i] * rtruncnorm(1, make.rate[2]/2, Inf, make.rate[2], make.std[2])
    # Assume the min making time equal to half of the making time
  }
  out=list(ArrivalTime=arr.time[-1], 
           ServiceTime=serv.time[-1], 
           Type=type[, -1])
}


### Simulation 0: Original setting
set.seed(8787)
sim.original=function(sim=sim.time(100), nserver=2){
  nbuyer = length(sim$ArrivalTime)
  arr.time = sim$ArrivalTime
  serv.time = sim$ServiceTime
  type = sim$Type
  end.time = c()
  start.time = c()
  start.nserver=c()
  Qsofar=rep(0, nbuyer)
  Ssofar=rep(0, nbuyer)
  wait.time.Q=c()
  wait.time.S=c()
  
  # caculate the Non-waiting debuted customer from 1 to N
  for(i in 1:nserver){
    start.time[i] = arr.time[i]
    end.time[i] = start.time[i] + serv.time[i]
  }
  
  # Gothrough all customers
  for(i in (nserver+1):nbuyer){ # Both servers are busy
    end.time.sofar = sort(end.time[1:(i-1)], T)[1:nserver]
    end.time.sofar[is.na(end.time.sofar)] = 0 # so must go to second condition
    if(all(arr.time[i] < end.time.sofar)){
      start.time[i] = min(end.time.sofar)
      end.time[i] = start.time[i] + serv.time[i]
    } else {
      start.time[i] = arr.time[i]
      end.time[i] = start.time[i] + serv.time[i]
    }
  }
  
  # Count the wating time
  for (i in 1:nbuyer){
    wait.time.Q[i] = start.time[i] - arr.time[i]
    wait.time.S[i] = end.time[i] - arr.time[i]
  }
  
  # Count the quening size in System
  for (i in 1:nbuyer){
    processing = which(end.time[1:(i-1)] > arr.time[i])
    Ssofar[i]=length(processing)
  }
  
  # Count the quening size in Queue
  for (i in 1:nbuyer){
    if (start.time[i] > arr.time[i]){
      waiting = which(start.time[1:(i-1)] > arr.time[i])
      Qsofar[i]=length(waiting)
    }
  }
  
  sim.table = cbind(sim$ArrivalTime, 
                    sim$Type[1, ],
                    sim$Type[2, ],
                    matrix(start.time, ncol=1), 
                    sim$ServiceTime, 
                    matrix(end.time, ncol=1), 
                    matrix(wait.time.Q, ncol=1), 
                    matrix(wait.time.S, ncol=1), 
                    matrix(Qsofar, ncol=1), 
                    matrix(Ssofar, ncol=1))
  colnames(sim.table) = c('ArrivalTime', '#Type1', '#Type2', 'StartTime', 'ServiceTime', 'EndTime', 
                          'QWaitTime', 'SWaitTime','#InQueue', '#InSystem')
  return(list(sim.table))
}
result = replicate(S, sim.original(sim.time(N)))
means = do.call(cbind, lapply(result, function(x) colMeans(x[, -4:-1])))
plot.ts(t(means))
cat('=============Simulation Results=============\n',
    '# Avg.Wq: ', mean(means['QWaitTime', ]), '\n', 
    '# Avg.Ws: ', mean(means['SWaitTime', ]), '\n',
    '# Avg.Lq: ', mean(means['#InQueue', ]), '\n',
    '# Avg.Ls: ', mean(means['#InSystem', ]), '\n',
    '===========================================\n')
write.csv(sim.original(sim.time(N)), file='Simulation-Original.csv')

### Simulation 1: Q balking: 
# Scenario: Q balking threshold for each individual
sim.1=function(sim=sim.time(100), nserver=2, balk.fn=Q.fn){
  nbuyer = length(sim$ArrivalTime)
  arr.time = sim$ArrivalTime
  serv.time = sim$ServiceTime
  type = sim$Type
  end.time = c()
  start.time = c()
  start.nserver=c()
  Qsofar=rep(0, nbuyer)
  Ssofar=rep(0, nbuyer)
  wait.time.Q=c()
  wait.time.S=c()
  Q.result=rep(1, nbuyer) # 1 means remaining
  
  # Pre-requisites: 
  ## 1)calculate the Non-waiting debuted customer from 1 to N
  for(i in 1:nserver){
    start.time[i] = arr.time[i]
    end.time[i] = start.time[i] + serv.time[i]
  }
  ## 2)calculate the giveup thresholds
  Q = balk.fn(nbuyer=nbuyer)
  
  # Gothrough all customers
  for(i in (nserver+1):nbuyer){ # Both servers are busy
    # Check waiting length
    queueingNow = length(which(start.time[1:(i-1)] > arr.time[i]))
    Qsofar[i] = queueingNow
    
    end.time.sofar = sort(end.time[1:(i-1)], T)[1:nserver]
    end.time.sofar[is.na(end.time.sofar)] = 0 # so must go to second condition
    if(all(arr.time[i] < end.time.sofar)){ # Waiting
      if(queueingNow > Q[i]){ # Balking
        start.time[i] = arr.time[i]
        serv.time[i] = 0
        end.time[i] = arr.time[i]
        Q.result[i] = 0 
      } else { # Accept waiting
        start.time[i] = min(end.time.sofar)
        end.time[i] = start.time[i] + serv.time[i]
      }
    } else { # Servers available
      start.time[i] = arr.time[i]
      end.time[i] = start.time[i] + serv.time[i]
    }
  }
  
  # Count the wating time
  for (i in 1:nbuyer){
    wait.time.Q[i] = start.time[i] - arr.time[i]
    wait.time.S[i] = end.time[i] - arr.time[i]
  }
  
  # Count the quening size in System
  for (i in 1:nbuyer){
    processing = which(end.time[1:(i-1)] > arr.time[i])
    Ssofar[i]=length(processing)
  }
  
  sim.table = cbind(sim$ArrivalTime, 
                    sim$Type[1, ],
                    sim$Type[2, ],
                    matrix(Q.result, ncol=1),
                    matrix(Q, ncol=1),
                    matrix(start.time, ncol=1), 
                    matrix(serv.time, ncol=1),
                    matrix(end.time, ncol=1), 
                    matrix(wait.time.Q, ncol=1), 
                    matrix(wait.time.S, ncol=1), 
                    matrix(Qsofar, ncol=1), 
                    matrix(Ssofar, ncol=1))
  colnames(sim.table) = c('ArrivalTime', '#Type1', '#Type2', 'Balk', 'BalkThres', 'StartTime','ServiceTime','EndTime', 
                          'QWaitTime', 'SWaitTime','#InQueue', '#InSystem')
  return(list(sim.table))
}
result = replicate(S, sim.1(sim.time(N), 2, Q.fn))
View(sim.1(sim.time(N), 2, Q.fn)[[1]])
Nbalk = unlist(lapply(result, function(x) N-sum(x[, 'Balk'])))
means = do.call(cbind, lapply(result, function(x) colMeans(x[, -4:-1])))
plot.ts(t(means))
cat('=============Simulation Results=============\n',
    '# Avg.Wq: ', mean(means['QWaitTime', ]), '\n', 
    '# Avg.Ws: ', mean(means['SWaitTime', ]), '\n',
    '# Avg.Lq: ', mean(means['#InQueue', ]), '\n',
    '# Avg.Ls: ', mean(means['#InSystem', ]), '\n',
    '# Avg.Balk: ', mean(Nbalk), '\n',
    '===========================================\n')
write.csv(sim.1(sim.time(N), 2, Q.fn), file='Simulation-Q5.csv')

### simulation 2:
# Scenario: For limited waiting space K, say K=10
K=10
sim.2=function(sim=sim.time(100), nserver=2, forbid.thres=Inf){
  nbuyer = length(sim$ArrivalTime)
  arr.time = sim$ArrivalTime
  serv.time = sim$ServiceTime
  type = sim$Type
  end.time = c()
  start.time = c()
  start.nserver=c()
  Qsofar=rep(0, nbuyer)
  Ssofar=rep(0, nbuyer)
  wait.time.Q=c()
  wait.time.S=c()
  K.result=rep(1, nbuyer)

  # Pre-requisites: 
  ## 1)calculate the Non-waiting debuted customer from 1 to N
  for(i in 1:nserver){
    start.time[i] = arr.time[i]
    end.time[i] = start.time[i] + serv.time[i]
  }

  # Gothrough all customers
  for(i in (nserver+1):nbuyer){ # Both servers are busy
    # Check waiting length
    queueingNow = length(which(start.time[1:(i-1)] > arr.time[i]))
    Qsofar[i] = queueingNow
    
    end.time.sofar = sort(end.time[1:(i-1)], T)[1:nserver]
    end.time.sofar[is.na(end.time.sofar)] = 0 # so must go to second condition
    if(all(arr.time[i] < end.time.sofar)){ # Waiting
      if(queueingNow > forbid.thres){ # Forbiden, out of waiting space
        start.time[i] = arr.time[i]
        serv.time[i] = 0
        end.time[i] = arr.time[i]
        K.result[i] = 0 
      } else { # Accept waiting
        start.time[i] = min(end.time.sofar)
        end.time[i] = start.time[i] + serv.time[i]
      }
    } else { # Servers available
      start.time[i] = arr.time[i]
      end.time[i] = start.time[i] + serv.time[i]
    }
  }
  
  # Count the wating time
  for (i in 1:nbuyer){
    wait.time.Q[i] = start.time[i] - arr.time[i]
    wait.time.S[i] = end.time[i] - arr.time[i]
  }
  
  # Count the quening size in System
  for (i in 1:nbuyer){
    processing = which(end.time[1:(i-1)] > arr.time[i])
    Ssofar[i]=length(processing)
  }
  
  sim.table = cbind(sim$ArrivalTime, 
                    sim$Type[1, ],
                    sim$Type[2, ],
                    matrix(K.result, ncol=1),
                    matrix(start.time, ncol=1), 
                    matrix(serv.time, ncol=1),
                    matrix(end.time, ncol=1), 
                    matrix(wait.time.Q, ncol=1), 
                    matrix(wait.time.S, ncol=1), 
                    matrix(Qsofar, ncol=1), 
                    matrix(Ssofar, ncol=1))
  colnames(sim.table) = c('ArrivalTime', '#Type1', '#Type2', 'SpaceAvail', 'StartTime','ServiceTime','EndTime', 
                          'QWaitTime', 'SWaitTime','#InQueue', '#InSystem')
  return(list(sim.table))
}
result = replicate(S, sim.2(sim.time(N), 2, K))
Nforbid = unlist(lapply(result, function(x) N-sum(x[, 'SpaceAvail'])))
means = do.call(cbind, lapply(result, function(x) colMeans(x[, -4:-1])))
plot.ts(t(means))
cat('=============Simulation Results=============\n',
    '# Avg.Wq: ', mean(means['QWaitTime', ]), '\n', 
    '# Avg.Ws: ', mean(means['SWaitTime', ]), '\n',
    '# Avg.Lq: ', mean(means['#InQueue', ]), '\n',
    '# Avg.Ls: ', mean(means['#InSystem', ]), '\n',
    '# Avg.Forbid: ', mean(Nforbid), '\n',
    '===========================================\n')
write.csv(sim.2(sim.time(N), 2, K), file='Simulation-K10.csv')



### Simulation 3: Q balking: 
# Scenario: Q balking threshold & K limited space
K=10
sim.3=function(sim=sim.time(100), nserver=2, balk.fn=Q.fn, forbid.thres=Inf){
  nbuyer = length(sim$ArrivalTime)
  arr.time = sim$ArrivalTime
  serv.time = sim$ServiceTime
  type = sim$Type
  end.time = c()
  start.time = c()
  start.nserver=c()
  Qsofar=rep(0, nbuyer)
  Ssofar=rep(0, nbuyer)
  wait.time.Q=c()
  wait.time.S=c()
  QK.result=rep(1, nbuyer)
  # 1 for (Available Space & Accept Waiting)
  # 0 for (Reject Waiting)
  # -1 for (No avaiable Space)...Not sure the waiting decision
  
  # Pre-requisites: 
  ## 1)calculate the Non-waiting debuted customer from 1 to N
  for(i in 1:nserver){
    start.time[i] = arr.time[i]
    end.time[i] = start.time[i] + serv.time[i]
  }
  ## 2)calculate the giveup thresholds
  Q = balk.fn(nbuyer=nbuyer)
  
  # Gothrough all customers
  for(i in (nserver+1):nbuyer){ # Both servers are busy
    # Check waiting length
    queueingNow = length(which(start.time[1:(i-1)] > arr.time[i]))
    Qsofar[i] = queueingNow
    
    end.time.sofar = sort(end.time[1:(i-1)], T)[1:nserver]
    end.time.sofar[is.na(end.time.sofar)] = 0 # so must go to second condition
    if(all(arr.time[i] < end.time.sofar)){ # Waiting
      if(queueingNow > forbid.thres){ # Forbidding
        start.time[i] = arr.time[i]
        serv.time[i] = 0
        end.time[i] = arr.time[i]
        QK.result[i] = -1 
      } else if(queueingNow > Q[i]){ # Balking
        start.time[i] = arr.time[i]
        serv.time[i] = 0
        end.time[i] = arr.time[i]
        QK.result[i] = 0 
      } else { # Accept waiting
        start.time[i] = min(end.time.sofar)
        end.time[i] = start.time[i] + serv.time[i]
      }
    } else { # Servers available
      start.time[i] = arr.time[i]
      end.time[i] = start.time[i] + serv.time[i]
    }
  }
  
  # Count the wating time
  for (i in 1:nbuyer){
    wait.time.Q[i] = start.time[i] - arr.time[i]
    wait.time.S[i] = end.time[i] - arr.time[i]
  }
  
  # Count the quening size in System
  for (i in 1:nbuyer){
    processing = which(end.time[1:(i-1)] > arr.time[i])
    Ssofar[i]=length(processing)
  }
  
  sim.table = cbind(sim$ArrivalTime, 
                    sim$Type[1, ],
                    sim$Type[2, ],
                    matrix(QK.result, ncol=1),
                    matrix(Q, ncol=1),
                    matrix(start.time, ncol=1), 
                    matrix(serv.time, ncol=1),
                    matrix(end.time, ncol=1), 
                    matrix(wait.time.Q, ncol=1), 
                    matrix(wait.time.S, ncol=1), 
                    matrix(Qsofar, ncol=1), 
                    matrix(Ssofar, ncol=1))
  colnames(sim.table) = c('ArrivalTime', '#Type1', '#Type2', 'Entering', 'BalkThres', 'StartTime','ServiceTime','EndTime', 
                          'QWaitTime', 'SWaitTime','#InQueue', '#InSystem')
  return(list(sim.table))
}
result = replicate(S, sim.3(sim.time(N), 2, Q.fn, K))
Nleft = unlist(lapply(result, function(x) N-sum(x[, 'Entering']==1)))
means = do.call(cbind, lapply(result, function(x) colMeans(x[, -4:-1])))
cat('=============Simulation Results=============\n',
    '# Avg.Wq: ', mean(means['QWaitTime', ]), '\n', 
    '# Avg.Ws: ', mean(means['SWaitTime', ]), '\n',
    '# Avg.Lq: ', mean(means['#InQueue', ]), '\n',
    '# Avg.Ls: ', mean(means['#InSystem', ]), '\n',
    '# Avg.Left(KwithQ): ', mean(Nleft), '\n',
    '===========================================\n')
write.csv(sim.3(sim.time(N), 2, Q.fn, K), file='Simulation-Q5-K10.csv')

### Simulation 4: N drinks per customer by sampling
MAXN = 2
cat('@Sampling in max 3:', replicate(5, N.fn(3)), 
    '\n@Sampling in max 5:', replicate(5, N.fn(5))) 
# Demo
hist(replicate(100, N.fn(8)))

sim.4=function(sim=sim.time(100, N.fn=function() N.fn(MAXN)), nserver=2){
  nbuyer = length(sim$ArrivalTime)
  arr.time = sim$ArrivalTime
  serv.time = sim$ServiceTime
  type = sim$Type
  end.time = c()
  start.time = c()
  start.nserver=c()
  Qsofar=rep(0, nbuyer)
  Ssofar=rep(0, nbuyer)
  wait.time.Q=c()
  wait.time.S=c()
  
  # caculate the Non-waiting debuted customer from 1 to N
  for(i in 1:nserver){
    start.time[i] = arr.time[i]
    end.time[i] = start.time[i] + serv.time[i]
  }
  
  # Gothrough all customers
  for(i in (nserver+1):nbuyer){ # Both servers are busy
    end.time.sofar = sort(end.time[1:(i-1)], T)[1:nserver]
    end.time.sofar[is.na(end.time.sofar)] = 0 # so must go to second condition
    if(all(arr.time[i] < end.time.sofar)){
      start.time[i] = min(end.time.sofar)
      end.time[i] = start.time[i] + serv.time[i]
    } else {
      start.time[i] = arr.time[i]
      end.time[i] = start.time[i] + serv.time[i]
    }
  }
  
  # Count the wating time
  for (i in 1:nbuyer){
    wait.time.Q[i] = start.time[i] - arr.time[i]
    wait.time.S[i] = end.time[i] - arr.time[i]
  }
  
  # Count the quening size in System
  for (i in 1:nbuyer){
    processing = which(end.time[1:(i-1)] > arr.time[i])
    Ssofar[i]=length(processing)
  }
  
  # Count the quening size in Queue
  for (i in 1:nbuyer){
    if (start.time[i] > arr.time[i]){
      waiting = which(start.time[1:(i-1)] > arr.time[i])
      Qsofar[i]=length(waiting)
    }
  }
  
  sim.table = cbind(sim$ArrivalTime, 
                    sim$Type[1, ],
                    sim$Type[2, ],
                    matrix(start.time, ncol=1), 
                    sim$ServiceTime, 
                    matrix(end.time, ncol=1), 
                    matrix(wait.time.Q, ncol=1), 
                    matrix(wait.time.S, ncol=1), 
                    matrix(Qsofar, ncol=1), 
                    matrix(Ssofar, ncol=1))
  colnames(sim.table) = c('ArrivalTime', '#Type1', '#Type2', 'StartTime', 'ServiceTime', 'EndTime', 
                          'QWaitTime', 'SWaitTime','#InQueue', '#InSystem')
  return(list(sim.table))
}
result = replicate(S, sim.4(sim.time(N, N.fn=function() N.fn(MAXN))))
means = do.call(cbind, lapply(result, function(x) colMeans(x[, -4:-1])))
cat('=============Simulation Results=============\n',
    '# Avg.Wq: ', mean(means['QWaitTime', ]), '\n', 
    '# Avg.Ws: ', mean(means['SWaitTime', ]), '\n',
    '# Avg.Lq: ', mean(means['#InQueue', ]), '\n',
    '# Avg.Ls: ', mean(means['#InSystem', ]), '\n',
    '===========================================\n')
write.csv(sim.4(sim.time(N, N.fn=function() N.fn(MAXN))), file='Simulation-N2.csv')


### Improvement 1: Process acceleration...@ payment time, NO check skip the payment
### (by online ordering & payment)
improv.1=function(sim=sim.time(100, p.ratio=0.1), nserver=2){
    nbuyer = length(sim$ArrivalTime)
    arr.time = sim$ArrivalTime
    serv.time = sim$ServiceTime
    type = sim$Type
    end.time = c()
    start.time = c()
    start.nserver=c()
    Qsofar=rep(0, nbuyer)
    Ssofar=rep(0, nbuyer)
    wait.time.Q=c()
    wait.time.S=c()
    
    # caculate the Non-waiting debuted customer from 1 to N
    for(i in 1:nserver){
      start.time[i] = arr.time[i]
      end.time[i] = start.time[i] + serv.time[i]
    }
    
    # Gothrough all customers
    for(i in (nserver+1):nbuyer){ # Both servers are busy
      end.time.sofar = sort(end.time[1:(i-1)], T)[1:nserver]
      end.time.sofar[is.na(end.time.sofar)] = 0 # so must go to second condition
      if(all(arr.time[i] < end.time.sofar)){
        start.time[i] = min(end.time.sofar)
        end.time[i] = start.time[i] + serv.time[i]
      } else {
        start.time[i] = arr.time[i]
        end.time[i] = start.time[i] + serv.time[i]
      }
    }
    
    # Count the wating time
    for (i in 1:nbuyer){
      wait.time.Q[i] = start.time[i] - arr.time[i]
      wait.time.S[i] = end.time[i] - arr.time[i]
    }
    
    # Count the quening size in System
    for (i in 1:nbuyer){
      processing = which(end.time[1:(i-1)] > arr.time[i])
      Ssofar[i]=length(processing)
    }
    
    # Count the quening size in Queue
    for (i in 1:nbuyer){
      if (start.time[i] > arr.time[i]){
        waiting = which(start.time[1:(i-1)] > arr.time[i])
        Qsofar[i]=length(waiting)
      }
    }
    
    sim.table = cbind(sim$ArrivalTime, 
                      sim$Type[1, ],
                      sim$Type[2, ],
                      matrix(start.time, ncol=1), 
                      sim$ServiceTime, 
                      matrix(end.time, ncol=1), 
                      matrix(wait.time.Q, ncol=1), 
                      matrix(wait.time.S, ncol=1), 
                      matrix(Qsofar, ncol=1), 
                      matrix(Ssofar, ncol=1))
    colnames(sim.table) = c('ArrivalTime', '#Type1', '#Type2', 'StartTime', 'ServiceTime', 'EndTime', 
                            'QWaitTime', 'SWaitTime','#InQueue', '#InSystem')
    return(list(sim.table))
  }
result = replicate(S, improv.1(sim.time(N, p.ratio=0.1)))
means = do.call(cbind, lapply(result, function(x) colMeans(x[, -4:-1])))
cat('=============Simulation Results=============\n',
    '# Avg.Wq: ', mean(means['QWaitTime', ]), '\n', 
    '# Avg.Ws: ', mean(means['SWaitTime', ]), '\n',
    '# Avg.Lq: ', mean(means['#InQueue', ]), '\n',
    '# Avg.Ls: ', mean(means['#InSystem', ]), '\n',
    '===========================================\n')
write.csv(improv.1(sim.time(N, p.ratio=0.1)), file='Improvement-SKIPpay.csv')

### Improvement 2: Process acceleration...@ making time, Fixed(constant)/Low-std(norm)/Low-mean(exp)

### Case1 - Low-std: little variation on making drinks.
improv.2=function(sim=sim.time.norm(100, make.std=c(10, 10)), nserver=2){
  nbuyer = length(sim$ArrivalTime)
  arr.time = sim$ArrivalTime
  serv.time = sim$ServiceTime
  type = sim$Type
  end.time = c()
  start.time = c()
  start.nserver=c()
  Qsofar=rep(0, nbuyer)
  Ssofar=rep(0, nbuyer)
  wait.time.Q=c()
  wait.time.S=c()
  
  # caculate the Non-waiting debuted customer from 1 to N
  for(i in 1:nserver){
    start.time[i] = arr.time[i]
    end.time[i] = start.time[i] + serv.time[i]
  }
  
  # Gothrough all customers
  for(i in (nserver+1):nbuyer){ # Both servers are busy
    end.time.sofar = sort(end.time[1:(i-1)], T)[1:nserver]
    end.time.sofar[is.na(end.time.sofar)] = 0 # so must go to second condition
    if(all(arr.time[i] < end.time.sofar)){
      start.time[i] = min(end.time.sofar)
      end.time[i] = start.time[i] + serv.time[i]
    } else {
      start.time[i] = arr.time[i]
      end.time[i] = start.time[i] + serv.time[i]
    }
  }
  
  # Count the wating time
  for (i in 1:nbuyer){
    wait.time.Q[i] = start.time[i] - arr.time[i]
    wait.time.S[i] = end.time[i] - arr.time[i]
  }
  
  # Count the quening size in System
  for (i in 1:nbuyer){
    processing = which(end.time[1:(i-1)] > arr.time[i])
    Ssofar[i]=length(processing)
  }
  
  # Count the quening size in Queue
  for (i in 1:nbuyer){
    if (start.time[i] > arr.time[i]){
      waiting = which(start.time[1:(i-1)] > arr.time[i])
      Qsofar[i]=length(waiting)
    }
  }
  
  sim.table = cbind(sim$ArrivalTime, 
                    sim$Type[1, ],
                    sim$Type[2, ],
                    matrix(start.time, ncol=1), 
                    sim$ServiceTime, 
                    matrix(end.time, ncol=1), 
                    matrix(wait.time.Q, ncol=1), 
                    matrix(wait.time.S, ncol=1), 
                    matrix(Qsofar, ncol=1), 
                    matrix(Ssofar, ncol=1))
  colnames(sim.table) = c('ArrivalTime', '#Type1', '#Type2', 'StartTime', 'ServiceTime', 'EndTime', 
                          'QWaitTime', 'SWaitTime','#InQueue', '#InSystem')
  return(list(sim.table))
}
result = replicate(S, improv.2(sim.time.norm(N, make.std=c(10, 10))))
means = do.call(cbind, lapply(result, function(x) colMeans(x[, -4:-1])))
cat('=============Simulation Results=============\n',
    '# Avg.Wq: ', mean(means['QWaitTime', ]), '\n', 
    '# Avg.Ws: ', mean(means['SWaitTime', ]), '\n',
    '# Avg.Lq: ', mean(means['#InQueue', ]), '\n',
    '# Avg.Ls: ', mean(means['#InSystem', ]), '\n',
    '===========================================\n')
write.csv(improv.2(sim.time.norm(N, make.std=c(10, 10))), file='Improvement-SmallStdMake.csv')

### Case2 - Fixed: NO standard deviation
result = replicate(S, improv.2(sim.time.norm(N, make.std=c(0, 0))))
means = do.call(cbind, lapply(result, function(x) colMeans(x[, -4:-1])))
cat('=============Simulation Results=============\n',
    '# Avg.Wq: ', mean(means['QWaitTime', ]), '\n', 
    '# Avg.Ws: ', mean(means['SWaitTime', ]), '\n',
    '# Avg.Lq: ', mean(means['#InQueue', ]), '\n',
    '# Avg.Ls: ', mean(means['#InSystem', ]), '\n',
    '===========================================\n')
write.csv(improv.2(sim.time.norm(N, make.std=c(0, 0))), file='Improvement-ZeroStdMake.csv')

### Case3 - Low-mean: Proficient employee accelerates.
result = replicate(S, improv.2(sim.time(N, make.rate=c(30, 80))))
means = do.call(cbind, lapply(result, function(x) colMeans(x[, -4:-1])))
cat('=============Simulation Results=============\n',
    '# Avg.Wq: ', mean(means['QWaitTime', ]), '\n', 
    '# Avg.Ws: ', mean(means['SWaitTime', ]), '\n',
    '# Avg.Lq: ', mean(means['#InQueue', ]), '\n',
    '# Avg.Ls: ', mean(means['#InSystem', ]), '\n',
    '===========================================\n')
write.csv(improv.2(sim.time(N, make.rate=c(30, 80))), file='Improvement-SmallMeanMake.csv')

### Improvement 3: New employess to 3
improv.3=function(sim=sim.time(100), nserver=3){
  nbuyer = length(sim$ArrivalTime)
  arr.time = sim$ArrivalTime
  serv.time = sim$ServiceTime
  type = sim$Type
  end.time = c()
  start.time = c()
  start.nserver=c()
  Qsofar=rep(0, nbuyer)
  Ssofar=rep(0, nbuyer)
  wait.time.Q=c()
  wait.time.S=c()
  
  # caculate the Non-waiting debuted customer from 1 to N
  for(i in 1:nserver){
    start.time[i] = arr.time[i]
    end.time[i] = start.time[i] + serv.time[i]
  }
  
  # Gothrough all customers
  for(i in (nserver+1):nbuyer){ # Both servers are busy
    end.time.sofar = sort(end.time[1:(i-1)], T)[1:nserver]
    end.time.sofar[is.na(end.time.sofar)] = 0 # so must go to second condition
    if(all(arr.time[i] < end.time.sofar)){
      start.time[i] = min(end.time.sofar)
      end.time[i] = start.time[i] + serv.time[i]
    } else {
      start.time[i] = arr.time[i]
      end.time[i] = start.time[i] + serv.time[i]
    }
  }
  
  # Count the wating time
  for (i in 1:nbuyer){
    wait.time.Q[i] = start.time[i] - arr.time[i]
    wait.time.S[i] = end.time[i] - arr.time[i]
  }
  
  # Count the quening size in System
  for (i in 1:nbuyer){
    processing = which(end.time[1:(i-1)] > arr.time[i])
    Ssofar[i]=length(processing)
  }
  
  # Count the quening size in Queue
  for (i in 1:nbuyer){
    if (start.time[i] > arr.time[i]){
      waiting = which(start.time[1:(i-1)] > arr.time[i])
      Qsofar[i]=length(waiting)
    }
  }
  
  sim.table = cbind(sim$ArrivalTime, 
                    sim$Type[1, ],
                    sim$Type[2, ],
                    matrix(start.time, ncol=1), 
                    sim$ServiceTime, 
                    matrix(end.time, ncol=1), 
                    matrix(wait.time.Q, ncol=1), 
                    matrix(wait.time.S, ncol=1), 
                    matrix(Qsofar, ncol=1), 
                    matrix(Ssofar, ncol=1))
  colnames(sim.table) = c('ArrivalTime', '#Type1', '#Type2', 'StartTime', 'ServiceTime', 'EndTime', 
                          'QWaitTime', 'SWaitTime','#InQueue', '#InSystem')
  return(list(sim.table))
}
result = replicate(S, improv.3(sim.time(N)))
means = do.call(cbind, lapply(result, function(x) colMeans(x[, -4:-1])))
cat('=============Simulation Results=============\n',
    '# Avg.Wq: ', mean(means['QWaitTime', ]), '\n', 
    '# Avg.Ws: ', mean(means['SWaitTime', ]), '\n',
    '# Avg.Lq: ', mean(means['#InQueue', ]), '\n',
    '# Avg.Ls: ', mean(means['#InSystem', ]), '\n',
    '===========================================\n')
write.csv(improv.3(sim.time(N), nserver=3), file='Improvement-3Staff.csv')




