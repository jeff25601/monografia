data {
  int<lower=1> K; //categorias
  int<lower=1> Je; //itens para efetividade
  int<lower=1> Jc; //itens para custo de oportunidade
  int<lower=1> N; //número de indivíduos
  int<lower=1> M; //número de medidas protetivas
  int<lower=1> zl; //number of individuals variables
  int<lower=1> L; //Número de bairros
  int<lower=0,upper=K> x[N,Je,M]; //padrão de respostas para efetividade
  int<lower=0,upper=K> w[N,Jc,M]; //padrão de respostas para custo de opt
  matrix[N, zl] z; //indivuals features
  int<lower=0,upper=K> y[N,M];
  vector[M] mu;
  matrix<lower=0,upper=1>[M,M] I;
  matrix<lower=0,upper=1>[M,M] Um0;
  vector[L] muviz;
  matrix<lower=0,upper=L>[L,L] viz;
  matrix<lower=0,upper=1>[L,L] vizmat;
  int<lower=1> nk[L];
}

parameters {
  vector<lower=0>[Je] a_e[M]; //discriminação para efetividade
  vector<lower=0>[Jc] a_c[M]; //discriminação para custo
  real b_e[Je, M]; //dificuldade para efetividade
  real b_c[Jc, M]; //dificuldade pra custo
  matrix[N,M] theta; //traço latente para efetividade
  matrix[N,M] lambda; //traço latente para custo de opt
  real<lower = -0.2, upper=1> rho1;
  real<lower = -0.2, upper=1> rho2;
  real<lower = -1, upper=1> rho3;
  //ordered[K-1] tau[M];
  vector[zl] bz[M];
  vector[M] b_l;
  vector[M] b_t;
  vector[N] re;
  real<lower=0> sig;
  vector[L] gamma;
  
}

model {
  int count = 0;
  
  
  //prioris para discriminação
  for(m in 1:M) {
    a_e[m] ~ gamma(0.01, 0.01);
    a_c[m] ~ gamma(0.01, 0.01);
  }
  

  b_l ~ normal(0, 10);
  b_t ~ normal(0, 10);

  //prioris para dificuldade
  for(m in 1:M) {
    for(je in 1:Je) {
      b_e[je,m] ~ normal(0,1);
    }
    
    for(jc in 1:Jc) {
      b_c[jc,m] ~ normal(0,1);
    }

    //tau[m] ~ normal(0, 10);

    bz[m] ~ normal(0, 10);
  }
  
  // prioris para os parametros de corr
  rho1 ~ uniform(-0.2, 1);
  rho2 ~ uniform(-0.2, 1);
  
  //priori para traços latentes
  for(i in 1:N) {
    theta[i, ] ~ multi_normal(mu, I + rho1*Um0);
    lambda[i,] ~ multi_normal(mu, I + rho2*Um0); 
  }

  sig ~ inv_gamma(10, 10);
  re ~ normal(0, sig);
  
  
  
  for(m in 1:M){
    for(i in 1:N) {
      
      for(je in 1:Je) {
        
        x[i,je, m] ~ bernoulli(Phi(a_e[m][je]*(theta[i,m]-b_e[je,m])));
        
        
      }

      
      for(jc in 1:Jc) {

        w[i,jc,m] ~ bernoulli(Phi(a_c[m][jc]*(lambda[i,m]-b_c[jc,m])));
        
      }
      
    }
  }

  for(l in 1:L){
    for(k in 1:nk[l]){
      count+=1;
      for(m in 1:M) {
        y[count,m] ~ bernoulli_logit(b_t[m]*theta[count,m] + b_l[m]*lambda[count,m] + z[count,]*bz[m] + re[count] + gamma[l]);
      }
    }
  }
}


generated quantities {

  int xpred[N,Je,M];
  int wpred[N, Jc, M];
  int ypred[N,M];
  real logy[N,M];
  int count = 0;
  
  for(l in 1:L){
    for(k in 1:nk[l]){
      count += 1;
      for(m in 1:M){
        for(je in 1:Je) {
          
          xpred[count,je,m] = bernoulli_rng(Phi(a_e[m][je]*(theta[count,m]-b_e[je,m])));
          
        }
        
        for(jc in 1:Jc) {
          
          wpred[count,jc,m] = bernoulli_rng(Phi(a_c[m][jc]*(lambda[count,m]-b_c[jc,m])));
        }
        
        
        ypred[count,m] = bernoulli_logit_rng(b_t[m]*theta[count,m] + b_l[m]*lambda[count,m] + re[count] + gamma[l]);
        logy[count,m] = bernoulli_logit_lpmf(y[count,m] | b_t[m]*theta[count,m] + b_l[m]*lambda[count,m] + z[count,]*bz[m] + re[count] + gamma[l]);
      }
    }
  }
  

}
