data {
  
  int<lower = 1> N;  // number of observations
  int<lower = 2> ncat;  // number of DAC categories
  array[N] int Y1;  // categorical response variable for hurdle (D = 2, A = 3, or C = 1?)
  int<lower = 1> K;  // number of population-level effects post hurdle
  matrix[N, K] X;  // population-level design matrix post hurdle
  
  // data for group-level effects of ID 1 = person
  int<lower = 1> N_1;  // number of grouping levels
  int<lower = 1> M_1;  // number of coefficients per level (random effects)
  array[N] int<lower = 1> J_1;  // grouping indicator per observation
  vector[N] Z_1_1;  // group 1 (person)-level predictor values
  
  // data for group-level effects of ID 2 = item
  int<lower = 1> N_2;  // number of grouping levels
  int<lower = 1> M_2;  // number of coefficients per level
  array[N] int<lower = 1> J_2;  // grouping indicator per observation
  vector[N] Z_2_1;  // group 2 (item)-level predictor values
  
  // data for group-level effects of ID 3 = study
  int<lower=1> N_3;  // number of grouping levels
  int<lower=1> M_3;  // number of coefficients per level
  array[N] int<lower=1> J_3;  // grouping indicator per observation
  vector[N] Z_3_1;  // group 2 (study)-level predictor values
  
  // data for outcomes
  vector[N] Y2;  // response variable
  vector[N] Y2_complete;  // response variable
  
  // priors
  vector[K - 1] b_prior;
  int prior_only;  // should the likelihood be ignored?
  
  // other stuff
  int<lower = 1> N_test;  // number of observations in test data
  matrix[N_test, K - 1] X_test;  // test data
  
}
transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  
  vector[Kc] b_eta1;  // population-level effects
  real Intercept_eta1;  // temporary intercept for centered predictors
  vector[Kc] b_eta2;  // population-level effects
  real Intercept_eta2;  // temporary intercept for centered predictors
  
  vector[Kc] b_eta3;  // population-level effects for m hurdle
  real Intercept_eta3;  // temporary intercept for centered predictors for m hurdle
  
  vector<lower = 0>[M_1] sd_1;  // person-level standard deviations
  array[M_1] vector[N_1] z_1; //vector[N_1] z_1[M_1];  // standardized person-level effects
  vector<lower = 0>[M_2] sd_2;  // item-level standard deviations
  array[M_2] vector[N_2] z_2; //vector[N_2] z_2[M_2];  // standardized item-level effects
  vector<lower = 0>[M_1] sd_3;  // person-level standard deviations
  array[M_1] vector[N_1] z_3; //vector[N_1] z_3[M_1];  // standardized person-level effects
  vector<lower = 0>[M_2] sd_4;  // item-level standard deviations
  array[M_2] vector[N_2] z_4; //vector[N_2] z_4[M_2];  // standardized item-level effects
  
  vector<lower=0>[M_3] sd_5;  // study-level standard deviations
  array[M_3] vector[N_3] z_5; //vector[N_3] z_5[M_3];  // standardized study-level effects
  vector<lower = 0>[M_3] sd_6;  // study-level standard deviations
  array[M_3] vector[N_3] z_6; //vector[N_3] z_6[M_3];  // standardized study-level effects
  
  vector<lower = 0>[M_1] sd_7;  // person-level standard deviations for m hurdle
  array[M_1] vector[N_1] z_7; //vector[N_1] z_7[M_1];  // standardized person-level effects for m hurdle
  vector<lower = 0>[M_2] sd_8;  // item-level standard deviations for m hurdle
  array[M_2] vector[N_2] z_8; //vector[N_2] z_8[M_2];  // standardized item-level effects for m hurdle
  vector<lower = 0>[M_3] sd_9;  // study-level standard deviations for m hurdle
  array[M_3] vector[N_3] z_9; //vector[N_3] z_9[M_3];  // standardized study-level effects for m hurdle
  
  vector[Kc] b;  // population-level effects
  real Intercept;  // temporary intercept for centered predictors
  vector<lower = 0>[M_1] sd_1a;  // group-level standard deviations
  array[M_1] vector[N_1] z_1a; //vector[N_1] z_1a[M_1];  // standardized group-level effects
  vector<lower = 0>[M_2] sd_2a;  // group-level standard deviations
  array[M_2] vector[N_2] z_2a; //vector[N_2] z_2a[M_2];  // standardized group-level effects
  vector<lower = 0>[M_1] sd_1b;  // group-level standard deviations
  array[M_1] vector[N_1] z_1b; //vector[N_1] z_1b[M_1];  // standardized group-level effects
  vector<lower = 0>[M_2] sd_2b;  // group-level standard deviations
  array[M_2] vector[N_2] z_2b; //vector[N_2] z_2b[M_2];  // standardized group-level effects
  
  vector<lower = 0>[M_3] sd_3a;  // group-level standard deviations
  array[M_3] vector[N_3] z_3a; //vector[N_3] z_3a[M_3];  // standardized group-level effects
  vector<lower = 0>[M_3] sd_3b;  // group-level standard deviations
  array[M_3] vector[N_3] z_3b; //vector[N_3] z_3b[M_3];  // standardized group-level effects

  real Intercept_phi;  // temporary intercept for centered distance var
  vector[Kc] b_dist;  // population-level effects
  
}

transformed parameters {
  vector[N_1] r_1_eta1_1;  // actual person-level effects
  vector[N_2] r_2_eta1_1;  // actual item-level effects
  vector[N_1] r_3_eta2_1;  // actual person-level effects
  vector[N_2] r_4_eta2_1;  // actual item-level effects
  
  vector[N_3] r_5_eta1_1;  // actual study-level effects
  vector[N_3] r_6_eta2_1;  // actual study-level effects
  
  vector[N_1] r_7_eta3_1;  // actual person-level effects for m hurdle
  vector[N_2] r_8_eta3_1;  // actual item-level effects for m hurdle
  vector[N_3] r_9_eta3_1;  // actual study-level effects for m hurdle
  
  vector[N_1] r_1_1;  // actual group-level effects
  vector[N_2] r_2_1;  // actual group-level effects
  vector[N_1] r_1_1b;  // actual group-level effects
  vector[N_2] r_2_1b;  // actual group-level effects
  
  vector[N_3] r_3_1;  // actual group-level effects
  vector[N_3] r_3_1b;  // actual group-level effects
  
  
  r_1_eta1_1 = (sd_1[1] * (z_1[1]));
  r_2_eta1_1 = (sd_2[1] * (z_2[1]));
  r_3_eta2_1 = (sd_3[1] * (z_3[1]));
  r_4_eta2_1 = (sd_4[1] * (z_4[1]));
  
  r_5_eta1_1 = (sd_5[1] * (z_5[1]));
  r_6_eta2_1 = (sd_6[1] * (z_6[1]));
  
  r_7_eta3_1 = (sd_7[1] * (z_7[1]));
  r_8_eta3_1 = (sd_8[1] * (z_8[1]));
  r_9_eta3_1 = (sd_9[1] * (z_9[1]));
  
  r_1_1 = (sd_1a[1] * (z_1a[1]));
  r_2_1 = (sd_2a[1] * (z_2a[1]));
  r_1_1b = (sd_1b[1] * (z_1b[1]));
  r_2_1b = (sd_2b[1] * (z_2b[1]));
  
  r_3_1 = (sd_3a[1] * (z_3a[1]));
  r_3_1b = (sd_3b[1] * (z_3b[1]));

}
model {
  
  // initialize linear predictor term
  vector[N] eta1 = Intercept_eta1 + Xc * b_eta1;
  // initialize linear predictor term
  vector[N] eta2 = Intercept_eta2 + Xc * b_eta2;
  
  // initialize linear predictor term for m hurdle
  vector[N] eta3 = Intercept_eta3 + Xc * b_eta3;
  
  // linear predictor matrix
  array[N] vector[ncat] eta;
  
  // beta post hurdle
  // initialize linear predictor term
  vector[N] mu = Intercept + Xc * b;
  vector[N] phi = Intercept_phi + Xc * b_dist;
  vector[N] A;                         // parameter for beta distn
  vector[N] B;                         // parameter for beta distn
  
  for (n in 1:N) {
    // add more terms to the linear predictor
    eta1[n] += r_1_eta1_1[J_1[n]] * Z_1_1[n] + r_2_eta1_1[J_2[n]] * Z_2_1[n] + r_5_eta1_1[J_3[n]] * Z_3_1[n];
    // add more terms to the linear predictor
    eta2[n] += r_3_eta2_1[J_1[n]] * Z_1_1[n] + r_4_eta2_1[J_2[n]] * Z_2_1[n] + r_6_eta2_1[J_3[n]] * Z_3_1[n];
    
    eta3[n] += r_7_eta3_1[J_1[n]] * Z_1_1[n] + r_8_eta3_1[J_2[n]] * Z_2_1[n] + r_9_eta3_1[J_3[n]] * Z_3_1[n];
    
    eta[n] = [0, eta1[n], eta2[n], eta3[n]]';
    // add more terms to the linear predictor
    mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n] + r_3_1[J_3[n]] * Z_3_1[n];
    phi[n] += r_1_1b[J_1[n]] * Z_1_1[n] + r_2_1b[J_2[n]] * Z_2_1[n] + r_3_1b[J_3[n]] * Z_3_1[n];
    A[n] = inv_logit(mu[n]) * exp(phi[n]);
    B[n] = (1 - inv_logit(mu[n])) * exp(phi[n]);
  }
  
  // priors including all constants
  target += normal_lpdf(b_eta1 | 0, b_prior);
  target += normal_lpdf(b_eta2 | 0, b_prior);
  
  target += normal_lpdf(b_eta3 | 0, b_prior);
  
  target += normal_lpdf(Intercept_eta1 | 0, 5);
  target += normal_lpdf(Intercept_eta2 | 0, 5);
  
  target += normal_lpdf(Intercept_eta3 | 0, 5);
  
  target += student_t_lpdf(sd_1 | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_1[1]);
  target += student_t_lpdf(sd_2 | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_2[1]);
  target += student_t_lpdf(sd_3 | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_3[1]);
  target += student_t_lpdf(sd_4 | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_4[1]);
  
  target += student_t_lpdf(sd_5 | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_5[1]);
  target += student_t_lpdf(sd_6 | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_6[1]);
  
  target += student_t_lpdf(sd_7 | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_7[1]);
  target += student_t_lpdf(sd_8 | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_8[1]);
  target += student_t_lpdf(sd_9 | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_9[1]);
  
  // priors including all constants
  target += normal_lpdf(b | 0, b_prior);
  target += normal_lpdf(Intercept | 0, 5);
  target += normal_lpdf(b_dist | 0, b_prior);
  target += normal_lpdf(Intercept_phi | 0, 5);
  target += student_t_lpdf(sd_1a | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_1a[1]);
  target += student_t_lpdf(sd_2a | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_2a[1]);
  target += student_t_lpdf(sd_1b | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_1b[1]);
  target += student_t_lpdf(sd_2b | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_2b[1]);
  
  target += student_t_lpdf(sd_3a | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_3a[1]);
  target += student_t_lpdf(sd_3b | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  target += std_normal_lpdf(z_3b[1]);
  
  // likelihood including all constants
  if (!prior_only) {  
    for (n in 1:N) {
      if (Y1[n] == 2){
        2 ~ categorical_logit(eta[n]);
      }
      else if (Y1[n] == 3){
        3 ~ categorical_logit(eta[n]);
      }
      else if (Y1[n] == 4){
        4 ~ categorical_logit(eta[n]);
      }
      else {
        1 ~ categorical_logit(eta[n]);
        Y2[n] ~ beta(A[n], B[n]);
      }
    }
  }
}


generated quantities {
  // actual population-level intercept
  real b_eta1_Intercept = Intercept_eta1 - dot_product(means_X, b_eta1);
  // actual population-level intercept
  real b_eta2_Intercept = Intercept_eta2 - dot_product(means_X, b_eta2);
  
  // actual population-level intercept for m hurdle
  real b_eta3_Intercept = Intercept_eta3 - dot_product(means_X, b_eta3);
  
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  // actual population-level intercept
  real b_Intercept_phi = Intercept_phi - dot_product(means_X, b_dist);
  // initialize linear predictor term
  real eta1;
  // initialize linear predictor term
  real eta2;
  
  // initialize linear predictor term for m hurdle
  real eta3;
  
  //real A[N];
  //real B[N];
  vector[N_test] eta1_t = b_eta1_Intercept + X_test * b_eta1;
  vector[N_test] eta2_t = b_eta2_Intercept + X_test * b_eta2;
  
  vector[N_test] eta3_t = b_eta3_Intercept + X_test * b_eta3;
  
  // initialize linear predictor term
  //testing results 
  array[N_test] real pc1_t;
  array[N_test] real pc2_t;
  array[N_test] real pc3_t;
  
  array[N_test] real pc4_t;
  
  vector[ncat] eta;
  vector[N] log_lik;
  array[N] real y_rep;
  real y1_rep;
  
  // initialize linear predictor term
  real mu;
  real phi;
  vector[N_test] mu_t = b_Intercept + X_test * b;
  vector[N_test] phi_t = exp(b_Intercept_phi + X_test * b_dist);
  array[N_test] real A_t;
  array[N_test] real B_t;
  
  for (n in 1:N) {
    // add more terms to the linear predictor
    eta1 = Intercept_eta1 + Xc[n] * b_eta1 + r_1_eta1_1[J_1[n]] * Z_1_1[n] + r_2_eta1_1[J_2[n]] * Z_2_1[n] + r_5_eta1_1[J_3[n]] * Z_3_1[n];
    // add more terms to the linear predictor
    eta2 = Intercept_eta2 + Xc[n] * b_eta2 + r_3_eta2_1[J_1[n]] * Z_1_1[n] + r_4_eta2_1[J_2[n]] * Z_2_1[n] + r_6_eta2_1[J_3[n]] * Z_3_1[n];
    
    eta3 = Intercept_eta3 + Xc[n] * b_eta3 + r_7_eta3_1[J_1[n]] * Z_1_1[n] + r_8_eta3_1[J_2[n]] * Z_2_1[n] + r_9_eta3_1[J_3[n]] * Z_3_1[n];
    
    eta = [0, eta1, eta2, eta3]';  
    // add more terms to the linear predictor
    mu = Intercept + Xc[n] * b + r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n] + r_3_1[J_3[n]] * Z_3_1[n];
    phi = Intercept_phi + Xc[n] * b_dist + r_1_1b[J_1[n]] * Z_1_1[n] + r_2_1b[J_2[n]] * Z_2_1[n] + r_3_1b[J_3[n]] * Z_3_1[n];
    y1_rep = categorical_logit_rng(eta);
    if (y1_rep == 2){
      y_rep[n] = 0;
    } 
    else if (y1_rep == 3){
      y_rep[n] = 1;
    }
    else if (y1_rep == 4){
      y_rep[n] = 0.5;
    }
    else {
      y_rep[n] = beta_rng(inv_logit(mu) * exp(phi), (1 - inv_logit(mu)) * exp(phi));
    }
    if (Y1[n] == 2){
      log_lik[n] = categorical_logit_lpmf(2 | eta);
    }
    else if (Y1[n] == 3){
      log_lik[n] = categorical_logit_lpmf(3 | eta);
    }
    else if (Y1[n] == 4){
      log_lik[n] = categorical_logit_lpmf(4 | eta);
    }
    else {
      log_lik[n] = categorical_logit_lpmf(1 | eta);
      log_lik[n] += beta_lpdf(Y2[n] | inv_logit(mu) * exp(phi), (1 - inv_logit(mu)) * exp(phi));
    }
    
  }
  for (n2 in 1:N_test){
    pc2_t[n2] = exp(eta1_t[n2]) / (1 + exp(eta1_t[n2]) + exp(eta2_t[n2]) + exp(eta3_t[n2]));
    pc3_t[n2] = exp(eta2_t[n2]) / (1 + exp(eta1_t[n2]) + exp(eta2_t[n2]) + exp(eta3_t[n2]));
    
    pc4_t[n2] = exp(eta3_t[n2]) / (1 + exp(eta1_t[n2]) + exp(eta2_t[n2]) + exp(eta3_t[n2]));
    
    pc1_t[n2] = 1 - pc2_t[n2] - pc3_t[n2] - pc4_t[n2];
    A_t[n2] = inv_logit(mu_t[n2]) * phi_t[n2];
    B_t[n2] = (1 - inv_logit(mu_t[n2])) * phi_t[n2];
  }
}

