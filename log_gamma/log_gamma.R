#GTID: ywan43
GTID='ywan43'
print(GTID)



#Problem 1
#There is some pre_loaded data in the R environment, for example iris
iris
#To see the statistical metrics of the data, we can use summary() function
summary(iris)


#Problem 2
#Log Gamma Loop Function Definition
log_gamma_loop<-function(n){
  
  result=0
  
  if(n<=2)
    result=0
  else
    
    for(i in c(2:n-1))
    {result=result+log(i)}
  
  return(result)
  
}

#Problem 3
#Log Gamma Recursive Function Definition
log_gamma_recursive<-function(n){
  
  result=0
  
  if(n<=2)
    result=0
  else
    result=log(n-1)+log_gamma_recursive(n-1)
  return(result)
  
}


#Problem 4
#Sum Log Gamma Loop Function
sum_log_gamma_loop<-function(n)
{
  result=0
  for(i in c(1:n))
  {
    result=result+log_gamma_loop(i)
  }
  return(result)
  
}
#Sum Log Gamma Recursive Function
sum_log_gamma_recursive<-function(n)
{
  if(n<=2)
    result=0
  else
    result=log_gamma_recursive(n)+sum_log_gamma_recursive(n-1)
  
  return(result)
  
}

#Problem 5
#Compare Results to Built-In R Function
options("expressions"=500000)
#time comparison before stack overflow
time_comparison_1<-function(n){
  loop_time<-c()
  recursive_time<-c()
  lgamma_time<-c()
  values<-c()
  
  for(i in c(1:n))
  {
    values[i]<-2^i
    loop_time[i]=system.time(sum_log_gamma_loop(2^i))[1]
    recursive_time[i]=system.time(sum_log_gamma_recursive(2^i))[1]
    lgamma_time[i]=system.time(sum_lgamma_loop(2^i))[1]
  }
  
  result_table<-data.frame(values,loop_time,recursive_time,lgamma_time)
  return(result_table)
}

#time comparison after stack overflow
time_comparison_2<-function(n){
  loop_time<-c()
  recursive_time<-c()
  lgamma_time<-c()
  values<-c()
  
  for(i in c(1:n))
  {
    values[i]<-2^i
    loop_time[i]=system.time(sum_log_gamma_loop(2^i))[1]
    #recursive_time[i]=system.time(sum_log_gamma_recursive(2^i))[1]
    lgamma_time[i]=system.time(sum_lgamma_loop(2^i))[1]
  }
  
  result_table<-data.frame(values,loop_time,lgamma_time)
  return(result_table)
}

first_table<-time_comparison_1(11)
second_table<-time_comparison_2(20)

total <- merge(first_table,second_table,by="values",all.y = TRUE)
#final table for the comparison result
result_table<-total[c(1,3,5,6)]
