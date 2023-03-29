data {
  int<lower=1> K; //categorias
  int<lower=1> Je; //itens para efetividade
  int<lower=1> Jc; //itens para custo de oportunidade
  int<lower=1> N; //número de indivíduos
  int<lower=1> M; //número de medidas protetivas
  //int<lower=1> zl; //number of individuals variables
  int<lower=0,upper=K> x[N,Je]; //padrão de respostas para efetividade
  int<lower=0,upper=K> w[N,Jc]; //padrão de respostas para custo de opt
  //matrix[N, zl] z; //indivuals features
  //int<lower=1,upper=K> y[N,M];
  real mu;
  real<lower=0,upper=1> I;
  real<lower=0,upper=1> Um0;
}

parameters {
  vector<lower=0>[Je] a_e; //discriminação para efetividade
  vector<lower=0>[Jc] a_c; //discriminação para custo
  real b_e[Je]; //dificuldade para efetividade
  real b_c[Jc]; //dificuldade pra custo
  vector[N] theta; //traço latente para efetividade
  vector[N] lambda; //traço latente para custo de opt
  real<lower = -0.2, upper=1> rho1;
  real<lower = -0.2, upper=1> rho2;
  //ordered[K-1] tau[M];
  //vector[zl] bz[M];
  //vector[M] b_l;
  //vector[M] b_t;
  //vector[N] re;
  //real<lower=0> sig;
  
}

model {
  
  
  //prioris para discriminação
  //for(m in 1:M) {
    a_e ~ gamma(0.01, 0.01);
    a_c ~ gamma(0.01, 0.01);
  //}
  

  //b_l ~ normal(0, 10);
  //b_t ~ normal(0, 10);

  //prioris para dificuldade
  //for(m in 1:M) {
    for(je in 1:Je) {
      b_e[je] ~ normal(0,1);
    }
    
    for(jc in 1:Jc) {
      b_c[jc] ~ normal(0,1);
    }

    //tau[m] ~ normal(0, 10);

    //bz[m] ~ normal(0, 10);
  //}
  
  // prioris para os parametros de corr
  rho1 ~ uniform(-0.2, 1);
  rho2 ~ uniform(-0.2, 1);
  
  //priori para traços latentes
  for(i in 1:N) {
    theta[i] ~ normal(mu, I + rho1*Um0);
    lambda[i] ~ normal(mu, I + rho2*Um0); 
  }

  //sig ~ inv_gamma(10, 10);
  //re ~ normal(0, sig);
  
  
  
  //for(m in 1:M){
    for(i in 1:N) {
      
      for(je in 1:Je) {
        
        x[i,je] ~ bernoulli(Phi(a_e[je]*(theta[i]-b_e[je])));
        
        
      }

      
      for(jc in 1:Jc) {

        w[i,jc] ~ bernoulli(Phi(a_c[jc]*(lambda[i]-b_c[jc])));
        
      }
      
    }
  //}


  //for(m in 1:M) {
    //for(i in 1:N) {
      //y[i,m] ~ ordered_logistic(b_t[m]*theta[i,m] + b_l[m]*lambda[i,m] + z[i,]*bz[m] + re[i], tau[m]);
    //}
  //}
  
}


generated quantities {

  int xpred[N, Je];
  int wpred[N, Jc];
  //int ypred[N,M];
  //real logy[N,M];
  
  //for(m in 1:M){
    for(i in 1:N) {
      for(je in 1:Je) {
      
        xpred[i,je] = bernoulli_rng(Phi(a_e[je]*(theta[i]-b_e[je])));

      }

      for(jc in 1:Jc) {
       
        wpred[i,jc] = bernoulli_rng(Phi(a_c[jc]*(lambda[i]-b_c[jc])));
      }
      
      
      //ypred[i,m] = ordered_logistic_rng(b_t[m]*theta[i,m] + b_l[m]*lambda[i,m] + re[i], tau[m]);
      //logy[i,m] = ordered_logistic_lpmf(y[i,m] | b_t[m]*theta[i,m] + b_l[m]*lambda[i,m] + z[i,]*bz[m] + re[i], tau[m]);
    }
  //}

}
