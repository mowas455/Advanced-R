name <- "Mowniesh Asokan"
liuid <- "mowas455"

#library(markmyassignment)
#lab_path <-
  "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab2.yml"
#set_assignment(lab_path)

#1.1 conditional statement
##1.1.1
sheldon_game<-function(player1,player2)
{
  sg_tags<-c("rock","lizard","spock","scissors","paper")
  tolower(player1)
  tolower(player2)
  stopifnot(player1 %in% sg_tags, player2 %in% sg_tags)
  x=which(sg_tags %in% player1)
  y=which(sg_tags %in% player2)
  if(player1==player2)
  {
    return("Draw!")
  }
  if(any((x+c(1,3))%% 5==y)){
    return("Player 1 wins!")
  }else{
    
    return("Player 2 wins!")
  }
}



#1.2 For loop
#1.2.1
my_moving_median<-function(x,n, ...)
{
  if(!is.numeric(x) || !is.numeric(n))
  {
    stop()
  }
   else{
     v<-vector()
     q<-length(x)-n
     for(i in 1:q)
     {
      v[i]<-median(x[i:(i+n)],...)
     }
     }
  return(v)
}


#1.2.2
for_mult_table<-function(from,to)
{
  if((from%%1==0)&(to%%1==0)){
    y=as.vector(from:to)
    r=length(y)
    u=vector()
    for(i in from:to){
      for(j in from:to){
        mult=i*j
        u<-c(u,mult)
      }
    }
    z=matrix(data=u,nrow=r,ncol=r)
    rownames(z)=c(from:to)
    colnames(z)=c(from:to)
    return(z)
  }
  
}

  

#1.3
##1.3.1
find_cumsum<-function(x,find_sum)
{
  if(!is.numeric(x) || !is.numeric(find_sum))
    {
    stop()}
   else{
     c=0
     n=1
     f=length(x)
     while((c<=find_sum)&n<=f){
       c=c+x[n]
       n=n+1
     }
         return(c)}
  }

##1.3.2
while_mult_table<-function(from,to)
{
  if(!is.numeric(from)||!is.numeric(to))
  {
    stop()
  }else{
    b=c(from:to)
    z=length(b)
    v=vector()
    i=from
    while(i<=to){
      j=from
      while(j<=to){
        m=i*j
        v=c(v,m)
        j=j+1
      }
      i=i+1
    }
    z=matrix(data=v,nrow=z,ncol=z)
    rownames(z)=c(from:to)
    colnames(z)=c(from:to)
    return(z)
  }
}


##1.4
#1.4.1
repeat_find_cumsum<-function(x,find_sum)
{
  if(!is.numeric(x) || !is.numeric(find_sum))
{
  stop()}
  else
  {
    i=1
    f=length(x)
    j=0
    repeat{
      l=x[i]
      j=j+l
      i=i+1
      if((j>find_sum)|(i>f)){
        break
      }
    }
    return(j)
  }
  
}


##1.4.2
repeat_my_moving_median<-function(x,n,...)
{
  if(!is.numeric(x)||!is.numeric(n)){
    stop()
  }
  else{
    q<-length(x)-n
    r=vector()
    i=1
    repeat{
      r[i]<-median(x[i:(i+n)],...)
      i=i+1
      if(i>q){
        break
      }
    }
   return(r) 
  }
}




#1.5
##1.5.1
in_environment<-function(env){
  #return the content of the env in the function environment
{
  n<-ls(env)
}
return(n)
}



#1.6
##1.6.1
cov<-function(X)
{
  std_v=lapply(X, function(x){sd(x)})
  m=lapply(X, function(x){mean(x)})
  if(is.data.frame(X)=="TRUE")
  {
    
    
    return(mapply("/",std_v,m, SIMPLIFY = "FALSE"))
  }else
  {
    stop("X is not a data frame")
  }
}

#1.7
###1.7.1
moment<-function(i){
  if(!is.numeric(i)){
    stop("if x argument is numeric")}
  else{
    function(array){
      return(mean((array-mean(array))**i))
    }
  }
  
}


#mark_my_assignment()  

