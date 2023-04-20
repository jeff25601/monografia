data {
  int<lower=1> K; //categorias
  int<lower=1> Je; //itens para efetividade
  int<lower=1> Jc; //itens para custo de oportunidade
  int<lower=1> N; //número de indivíduos
  int<lower=1> M; //número de medidas protetivas
  int<lower=0,upper=K> x[N,Je]; //padrão de respostas para efetividade
  int<lower=0,upper=K> w[N,Jc]; //padrão de respostas para custo de opt
  real mu;
  real<lower=0,upper=1> I;
  real<lower=0,upper=1> Um0;
  int<lower=1> L; //número de bairros
  int<lower=1> zl; //number of individuals variables
  matrix[N, zl] z; //indivuals features
  int<lower=0,upper=1> y[N];
  vector[L] muviz;
  matrix<lower=0,upper=L>[L,L] viz;
  matrix<lower=0,upper=1>[L,L] vizmat;
  int<lower=1> nk[L];
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
  real<lower = -1, upper=1> rho3;
  //real tau;
  vector[zl] bz;
  real b_l;
  real b_t;
  vector[N] re;
  real<lower=0> sig;
  vector[L] gamma;
  
}

model {
  int count = 0;
  
  //prioris para discriminação
  //for(m in 1:M) {
    a_e ~ gamma(0.01, 0.01);
    a_c ~ gamma(0.01, 0.01);
  //}
  

  b_l ~ normal(0, 10);
  b_t ~ normal(0, 10);

  //prioris para dificuldade
  //for(m in 1:M) {
    for(je in 1:Je) {
      b_e[je] ~ normal(0,1);
    }
    
    for(jc in 1:Jc) {
      b_c[jc] ~ normal(0,1);
    }

    //tau ~ normal(0, 10);

    bz ~ normal(0, 10);
  //}
  
  // prioris para os parametros de corr
  rho1 ~ uniform(-0.2, 1);
  rho2 ~ uniform(-0.2, 1);
  rho3 ~ uniform(-1, 1);
  
  //priori para traços latentes
  for(i in 1:N) {
    theta[i] ~ normal(mu, I + rho1*Um0);
    lambda[i] ~ normal(mu, I + rho2*Um0); 
  }
  
  gamma ~ multi_normal(muviz, viz + rho3*vizmat);

  sig ~ inv_gamma(10, 10);
  re ~ normal(0, sig);
  
  
  
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
  
    for(l in 1:L){
      for(k in 1:nk[l]){
        count += 1;
        y[count] ~ bernoulli_logit(b_t*theta[count] + b_l*lambda[count] + z[count,]*bz + re[count] + gamma[l]);
      }
    }
  //}
  
}


generated quantities {

  int xpred[N, Je];
  int wpred[N, Jc];
  int ypred[N];
  real logy[N];
  
  //for(m in 1:M){
  //for(i in 1:N) {
    int count = 0;
    for(l in 1:L){
      for(k in 1:nk[l]){
        count += 1;
        for(je in 1:Je) {
      
          xpred[count,je] = bernoulli_rng(Phi(a_e[je]*(theta[count]-b_e[je])));

        }

        for(jc in 1:Jc) {
       
          wpred[count,jc] = bernoulli_rng(Phi(a_c[jc]*(lambda[count]-b_c[jc])));
        }
      
      
        ypred[count] = bernoulli_logit_rng(b_t*theta[count] + b_l*lambda[count] + z[count,]*bz + re[count] + gamma[l]);
        logy[count] = bernoulli_logit_lpmf(y[count] | b_t*theta[count] + b_l*lambda[count] + z[count,]*bz + re[count] + gamma[l]);
      }
    }
      
  //}
  //}

}
