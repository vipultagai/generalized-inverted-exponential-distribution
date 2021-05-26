set.seed(1)
#calculate log likelihood
llf=function(par) {
  alpha=par[1]
  lambda=par[2]
  pdf=alpha*lambda/y^2*exp(-lambda/y)*(1-exp(-lambda/y))^(alpha-1)
  cdf=1-(1-exp(-lambda/y))^alpha
  return(sum(log(pdf))+sum((k*(r+1)-1)*log(1-cdf)))
}


m_list <- c(31,50,100,500,1000,10000)
k_list <- c(3,5,10)
alpha_list  <- c(0.3,0.5,1,1.5,2)
lambda_list <- c(0.3,0.5,1,1.5,2)   

sim <-data.frame(k = NULL, m=NULL,alpha_population=NULL,
                 lambda_population =NULL,alpha_predicted=NULL,
                 lambda_predicted =NULL)

for(m in m_list){
  for(k in k_list){
    for(alpha in alpha_list){
      for(lambda in lambda_list){
        r=rep(5,m)
        n=sum(r)+m
        u=runif(n)
        x=NULL
        y=NULL
        x[1]=rexp(1)/n
        for(i1 in 2:m){
          x[i1]=x[i1-1]+rexp(1)/(n-sum(r[1:i1-1])-i1+1)}
        u=1-exp(-x)
        y=-lambda/log(-exp(log(-u+1)/k/alpha)+1)
        
        temp  <- optim(c(alpha,lambda),llf,control=list(fnscale=-1))$par
        new <- data.frame(k = k, m=m,alpha_population=alpha,
                   lambda_population =lambda,alpha_predicted=temp[1],
                   lambda_predicted =temp[2])
        sim <- rbind(sim,new)
        print(paste0(m,",",k,",",alpha,",",lambda))
      }
    }
  }
}
  
write.csv(sim,"simulation.csv",row.names = FALSE)
