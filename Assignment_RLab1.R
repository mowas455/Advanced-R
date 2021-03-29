

name <- "Mowniesh Asokan"
liuid <- "mowas455"

# library(markmyassignment)
#lab_path <-
# set_assignment(lab_path)

##1.1
#1.1.1

my_num_vector<-function()
{
 x<-log10(11)
 y<-cos(pi/5)
 z<-exp(pi/3)
 u<-(1173%%7)/19
 return(c(x,y,z,u))
 }
 

#1.1.2
filter_my_vector<-function(x,leq)
{
  for(i in 1:length(x))
    if(x[i]>=leq)
    {
      x[i]<-NA
    }
  return(x)
}

#1.1.3
dot_prod<- function(a,b){
  return(as.numeric(a%*%b))
}

# 1.1.4 approx_e(N)
approx_e <- function(N){
  sume <- 0
  for(i in 0:N){
    sume = sume + (1/factorial(i))
  }
  return(sume)
}


##1.2
#1.2.1
my_magic_matrix=function()
{
 a<-c(4,9,2)
 b<-c(3,5,7)
 c<-c(8,1,6)
 z<-matrix(c(a,b,c),nrow = 3, byrow = TRUE)
 return(z)
}


#1.2.2
calculate_elements <- function(A){
  col_num <- ncol(A)
  row_num <- nrow(A)
  return(as.numeric(col_num*row_num))
}

#1.2.3
row_to_zero <- function(A,i)
{
  A[i,] <- 0
  return(A)
}


#1.2.4
add_elements_to_matrix <- function(A,x,i,j){
  A[i,j] <- A[i,j] + x
  return(A)
}

##1.3
#1.3.1
my_magic_list<-function()
{
  y<-list(info="my own list",my_num_vector(),my_magic_matrix())
return(y)
}

#1.3.2
change_info<-function(x,text)
{
  x$info<-text
  return(x)
}

#1.3.3
add_note <- function(x,note){
  x$note <- note
  return(x)
}


#1.3.4
sum_numeric_parts<-function(x)  
{
  z <- length(x)
  r <- 0
  for(i in x){
    if(!is.character(i)){
      r = r + sum(i)
    }
  }
  return(r)
}

#1.4
#1.4.1
my_data.frame <- function()
{
  id <- c(1,2,3)
  name <- c('John','Lisa','Azra')
  income <- c(7.30,0.00,15.21)
  rich <- c(FALSE,FALSE,TRUE)
  df <- data.frame(id,name,income,rich)
  return(df)
}



#1.4.2
sort_head<-function(df, var.name, n)
{
  x <- head(df[order(df[,var.name],decreasing = TRUE),],n)
  return(x)
}




#1.4.3
add_median_variable<-function(df,j)
{
x <- median(df[,j])
z = length(df[,j])
for(i in 1:z){
  if(df[i,j]>x){
    df[i,'compared_to_median'] <- 'Greater'
  } else if(df[i,j]<x){
    df[i,'compared_to_median'] <- 'Smaller'
  } else{
    df[i,'compared_to_median'] <- 'Median'
  }
}
return(df)
}

#1.4.4
analyze_columns<-function(df, j)
{
  Firstcolumn <- c(mean = mean(df[,j[1]]),median = median(df[,j[1]]),sd = sd(df[,j[1]]))
  Secondcolumn <- c(mean = mean(df[,j[2]]),median = median(df[,j[2]]),sd = sd(df[,j[2]]))
  correlation_matrix <- cor(df[j])
  list1 <- list(Firstcolumn,Secondcolumn,correlation_matrix)
  names(list1) <- c(colnames(df[j[1]]),colnames(df[j[2]]),'correlation_matrix')
  return(list1)
}
#mark_my_assignment()

