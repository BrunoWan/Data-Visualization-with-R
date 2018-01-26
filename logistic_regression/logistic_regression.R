mnist_train=read.csv("mnist_train.csv", header=FALSE)
mnist_test=read.csv("mnist_test.csv", header=FALSE)

test_0_1=mnist_test[,mnist_test[nrow(mnist_test),]==0|mnist_test[nrow(mnist_test),]==1]
test_3_5=mnist_test[,mnist_test[nrow(mnist_test),]==3|mnist_test[nrow(mnist_test),]==5]
train_0_1=mnist_train[,mnist_train[nrow(mnist_train),]==0|mnist_train[nrow(mnist_train),]==1]
train_3_5=mnist_train[,mnist_train[nrow(mnist_train),]==3|mnist_train[nrow(mnist_train),]==5]

true_label_test_0_1=test_0_1[nrow(test_0_1),]
true_label_test_3_5=test_3_5[nrow(test_3_5),]
true_label_train_0_1=train_0_1[nrow(train_0_1),]
true_label_train_3_5=train_3_5[nrow(train_3_5),]

test_0_1=test_0_1[-nrow(test_0_1),]
test_3_5=test_3_5[-nrow(test_3_5),]
train_0_1=train_0_1[-nrow(train_0_1),]
train_3_5=train_3_5[-nrow(train_3_5),]

image((1:28), (1:28), matrix(data=as.numeric(train_0_1[,true_label_train_0_1[1,]==0][,1]), nrow=sqrt(nrow(train_0_1)), ncol=sqrt(nrow(train_0_1))), col=gray.colors(256))
image((1:28), (1:28), matrix(data=as.numeric(train_0_1[,true_label_train_0_1[1,]==1][,1]), nrow=sqrt(nrow(train_0_1)), ncol=sqrt(nrow(train_0_1))), col=gray.colors(256))
image((1:28), (1:28), matrix(data=as.numeric(train_3_5[,true_label_train_3_5[1,]==3][,1]), nrow=sqrt(nrow(train_3_5)), ncol=sqrt(nrow(train_3_5))), col=gray.colors(256))
image((1:28), (1:28), matrix(data=as.numeric(train_3_5[,true_label_train_3_5[1,]==5][,1]), nrow=sqrt(nrow(train_3_5)), ncol=sqrt(nrow(train_3_5))), col=gray.colors(256))

sigmoid=function(z){
  g=1/(1+exp(-z))
  return(g)
}


logit_model=function(input_data, class_data, theta_initial, alpha,epsilon ){
  
  if(length(unique(unlist(class_data)))==2){
    
    init=0
    for(i in unique(unlist(class_data)) ){
      if(!(i%in%c(0,1))) { 
        class_data[class_data==3]=0
        class_data[class_data==5]=1
        
      }}  
  }
  
  
  x=t(as.matrix(input_data))
  y=t(as.matrix(class_data))
  theta=matrix(data=rep(theta_initial, ncol(x)), nrow=ncol(x), ncol=1)
  J=epsilon+1
  while(J>epsilon)
  {
    
    theta=theta-alpha*(t(x)%*%(1/(1+exp(-x%*%theta))-y)/(nrow(x)))
    J=(t(-y)%*%log(sigmoid(x%*%theta))-t(1-y)%*%log(1-sigmoid(x%*%theta)))
    print(J)
  }
  return(theta)
}

model_test=function(theta, training_input_data, training_class_data, test_input_data, test_class_data){
  
  if(length(unique(unlist(test_class_data)))==2){
    
    init=0
    for(i in unique(unlist(test_class_data)) ){
      if(!(i%in%c(0,1))) { 
        test_class_data[test_class_data==3]=0
        test_class_data[test_class_data==5]=1
        
      }}  
  }
  if(length(unique(unlist(training_class_data)))==2){
    
    init=0
    for(i in unique(unlist(training_class_data)) ){
      if(!(i%in%c(0,1))) { 
        training_class_data[training_class_data==3]=0
        training_class_data[training_class_data==5]=1
        
      }}  
  }
  
  prediction_train=round(sigmoid(t(as.matrix(training_input_data))%*%theta))
  error_train=sum(abs(prediction_train-t(as.matrix(training_class_data))))/nrow(t(as.matrix(training_class_data)))
  prediction_test=round(sigmoid(t(as.matrix(test_input_data))%*%theta))
  error_test=sum(abs(prediction_test-t(as.matrix(test_class_data))))/nrow(t(as.matrix(test_class_data)))
  
  J_training=(t(-t(as.matrix(training_class_data)))%*%log(sigmoid(t(as.matrix(training_input_data))%*%theta))-t(1-t(as.matrix(training_class_data)))%*%log(1-sigmoid(t(as.matrix(training_input_data))%*%theta)))
  J_test=(t(-t(as.matrix(test_class_data)))%*%log(sigmoid(t(as.matrix(test_input_data))%*%theta))-t(1-t(as.matrix(test_class_data)))%*%log(1-sigmoid(t(as.matrix(test_input_data))%*%theta)))
  
  
  print(cat("Accuracy of Training Set: ", 1-error_train))
  print(cat("Accuracy of Test Set: ", 1-error_test))
  return(c(1-error_train, 1-error_test,J_training,J_test))
  
}


sample_and_shuffle=function(data, sample_perc){
  new_data=sample(data, sample_perc*(ncol(data)), replace=FALSE)
  return(new_data)
}


repeat_modeling=function(training_data, test_data, class,repeat_time, size,size_step, initial_theta, alpha, cost){
  
  result=c()  
  for(i in c(1:repeat_time)){
    spsf_train=sample_and_shuffle(training_data,size)
    if(identical(unlist(class),unlist(c(3,5)))){
      spsf_train=spsf_train[,spsf_train[nrow(spsf_train),]==3|spsf_train[nrow(spsf_train),]==5]
      true_label_spsf_train=spsf_train[nrow(spsf_train),]
      spsf_train=spsf_train[-nrow(spsf_train),]
      
      spsf_test=test_data
      spsf_test=spsf_test[,spsf_test[nrow(spsf_test),]==3|spsf_test[nrow(spsf_test),]==5]
      true_label_spsf_test=spsf_test[nrow(spsf_test),]
      spsf_test=spsf_test[-nrow(spsf_test),]
    }
    else if(identical(unlist(class),unlist(c(0,1)))){
      spsf_train=spsf_train[,spsf_train[nrow(spsf_train),]==0|spsf_train[nrow(spsf_train),]==1]
      true_label_spsf_train=spsf_train[nrow(spsf_train),]
      spsf_train=spsf_train[-nrow(spsf_train),]
      
      spsf_test=test_data
      spsf_test=spsf_test[,spsf_test[nrow(spsf_test),]==0|spsf_test[nrow(spsf_test),]==1]
      true_label_spsf_test=spsf_test[nrow(spsf_test),]
      spsf_test=spsf_test[-nrow(spsf_test),]
    } else{ print("Wrong Class")}
    
    print(nrow(t(spsf_train)))
    print(nrow(t(true_label_spsf_train)))
    print(nrow(t(spsf_test)))
    print(nrow(t(true_label_spsf_test)))
    spsf_theta=logit_model(spsf_train,true_label_spsf_train, initial_theta,alpha, cost)
    
    
    
    accuracy=model_test(spsf_theta, spsf_train,true_label_spsf_train, spsf_test, true_label_spsf_test)
    
    result=rbind(result,c(i, size, initial_theta, alpha, cost, accuracy[1],accuracy[2],accuracy[3],accuracy[4]))
    
    size=size-size_step
  }
  
  return(result)  
}


q_1a_result_01=repeat_modeling(mnist_train,mnist_test,c(0,1),1,1,0.1,0.1,0.8,200)
q_1a_result_35=repeat_modeling(mnist_train,mnist_test,c(3,5),1,1,0.1,0.1,0.8,2000)


colnames(q_1a_result_01)=c("i", "size","initial_theta","alpha","cost","train_accuracy", "test_accuracy",  "train_cost", "test_cost")
colnames(q_1a_result_35)=c("i", "size","initial_theta","alpha","cost","train_accuracy", "test_accuracy",  "train_cost", "test_cost")
print(q_1a_result_01)
print(q_1a_result_35)


q_1b_result_01=repeat_modeling(mnist_train,mnist_test,c(0,1),10,1,0.1,0.1,0.8,200)
q_1b_result_35=repeat_modeling(mnist_train,mnist_test,c(3,5),10,1,0.1,0.1,0.8,2000)

df_result_01=as.data.frame(q_1b_result_01)
df_result_35=as.data.frame(q_1b_result_35)
colnames(df_result_01)=c("i", "size","initial_theta","alpha","cost","train_accuracy", "test_accuracy",  "train_cost", "test_cost")
colnames(df_result_35)=c("i", "size","initial_theta","alpha","cost","train_accuracy", "test_accuracy",  "train_cost", "test_cost")
print(q_1b_result_01)
print(q_1b_result_35)



parameter_eval=function(training_data,test_data,class,repeat_time,experiment_time,size,size_step,initial_theta,theta_step,alpha,convergence_cost,convergence_cost_step){
  
  if(theta_step!=0){
    theta=initial_theta
    result=c()
    experiment_result=c()
    for(i in c(1:experiment_time)){
      result=repeat_modeling(training_data,test_data, class,repeat_time,size,size_step,theta,alpha,convergence_cost)
      result=cbind(result,rep(theta,repeat_time))
      experiment_result=rbind(experiment_result,result)
      theta=theta-theta_step
    }
  }
  
  if(convergence_cost_step!=0){
    cost=convergence_cost
    result=c()
    experiment_result=c()
    for(i in c(1:experiment_time)){
      result=repeat_modeling(training_data,test_data, class,repeat_time,size,size_step,initial_theta,alpha,cost)
      result=cbind(result,rep(cost,repeat_time))
      experiment_result=rbind(experiment_result,result)
      cost=cost-convergence_cost_step
    }
  }
  
  return(experiment_result)
}

theta_eval_01=parameter_eval(mnist_train,mnist_test,c(0,1),10,10,1,0.1,0.1,0.01,0.8,200,0)
theta_eval_35=parameter_eval(mnist_train,mnist_test,c(3,5),10,10,1,0.1,0.1,0.01,0.8,2000,0)


df_theta_eval_01=as.data.frame(theta_eval_01)
df_theta_eval_35=as.data.frame(theta_eval_35)
colnames(df_theta_eval_01)=c("i","size","initial_theta","alpha","cost","train_accuracy", "test_accuracy", "train_cost", "test_cost","theta")
colnames(df_theta_eval_35)=c("i","size","initial_theta","alpha","cost","train_accuracy", "test_accuracy",  "train_cost", "test_cost","theta")
agg_theta_eval_01=(aggregate(df_theta_eval_01,list(df_theta_eval_01$theta),mean))[,c("theta","train_accuracy", "test_accuracy")]
agg_theta_eval_35=(aggregate(df_theta_eval_35,list(df_theta_eval_35$theta),mean))[,c("theta","train_accuracy", "test_accuracy")]


print(agg_theta_eval_01)
print(agg_theta_eval_35)

convergence_eval_01=parameter_eval(mnist_train,mnist_test,c(0,1),10,10,1,0.1,0.1,0,0.8,300,10)
convergence_eval_35=parameter_eval(mnist_train,mnist_test,c(3,5),10,10,1,0.1,0.1,0,0.8,2100,10)

df_convergence_eval_01=as.data.frame(convergence_eval_01)
df_convergence_eval_35=as.data.frame(convergence_eval_35)
colnames(df_convergence_eval_01)=c("i","size","initial_theta","alpha","cost","train_accuracy", "test_accuracy", "train_cost", "test_cost","cost")
colnames(df_convergence_eval_35)=c("i","size","initial_theta","alpha","cost","train_accuracy", "test_accuracy",  "train_cost", "test_cost","cost")
agg_convergence_eval_01=(aggregate(df_convergence_eval_01,list(df_convergence_eval_01$cost),mean))[,c("cost","train_accuracy", "test_accuracy")]
agg_convergence_eval_35=(aggregate(df_convergence_eval_35,list(df_convergence_eval_35$cost),mean))[,c("cost","train_accuracy", "test_accuracy")]



print(agg_convergence_eval_01)
print(agg_convergence_eval_35)


learning_curves=function(training_data,test_data,class,repeat_time,experiment_time,size,size_step,initial_theta,alpha,convergence_cost){
  result=c()
  experiment_result=c()
  for(i in c(1:experiment_time)){
    result=repeat_modeling(training_data, test_data,class,repeat_time,size, 0, initial_theta, alpha, convergence_cost)
    result=cbind(result,rep(size,repeat_time))
    experiment_result=rbind(experiment_result,result)
    size=size-size_step
  }
  return(experiment_result)
}

lcurve_01=learning_curves(mnist_train,mnist_test,c(0,1),10,20,1,0.05,0.1,0.8,200)
lcurve_35=learning_curves(mnist_train,mnist_test,c(3,5),10,20,1,0.05,0.1,0.8,2000)

df_lcurve_01=as.data.frame(lcurve_01)
df_lcurve_35=as.data.frame(lcurve_35)
colnames(df_lcurve_01)=c("i","size","initial_theta","alpha","cost","train_accuracy", "test_accuracy", "train_cost", "test_cost","size_learn")
colnames(df_lcurve_35)=c("i","size","initial_theta","alpha","cost","train_accuracy", "test_accuracy", "train_cost", "test_cost","size_learn")
agg_lcurve_01_size=(aggregate(df_lcurve_01,list(df_lcurve_01$size_learn),mean))[,c("size_learn","train_accuracy", "test_accuracy")]
agg_lcurve_35_size=(aggregate(df_lcurve_35,list(df_lcurve_35$size_learn),mean))[,c("size_learn","train_accuracy", "test_accuracy")]

plot(agg_lcurve_01_size$size_learn,agg_lcurve_01_size$train_accuracy,type = "l",col="red")
lines(agg_lcurve_01_size$size_learn,agg_lcurve_01_size$test_accuracy,col="green")
plot(agg_lcurve_35_size$size_learn,agg_lcurve_35_size$train_accuracy,type = "l",col="red")
lines(agg_lcurve_35_size$size_learn,agg_lcurve_35_size$test_accuracy,col="green")

agg_lcurve_01_cost=(aggregate(df_lcurve_01,list(df_lcurve_01$size_learn),mean))[,c("size_learn","train_cost", "test_cost")]
agg_lcurve_35_cost=(aggregate(df_lcurve_35,list(df_lcurve_35$size_learn),mean))[,c("size_learn","train_cost", "test_cost")]
print(agg_lcurve_01_cost)
print(agg_lcurve_35_cost)
plot(agg_lcurve_01_cost$size_learn,agg_lcurve_01_cost$train_cost,type = "l",col="red")
lines(agg_lcurve_01_cost$size_learn,agg_lcurve_01_cost$test_cost,col="green")
plot(agg_lcurve_35_cost$size_learn,agg_lcurve_35_cost$train_cost,type = "l",col="red")
lines(agg_lcurve_35_cost$size_learn,agg_lcurve_35_cost$test_cost,col="green")



