
library(rstan)

y <- sample(c(1, 2, 3), prob = c(.5, .3, .1), size = 10, replace = T)
y



"
data {
  int K;
  int N;
  int D;
  array[N] int y;
  matrix[N, D] x;
}

parameters {
  matrix[D, K] beta;
}

model {
  matrix[N, K] x_beta = x * beta;

  to_vector(beta) ~ normal(0, 5);

  for (n in 1:N) {
    y[n] ~ categorical_logit(x_beta[n]');

  }
}
" |>
  stan(model_code = _,
       data = list(K = 3,
                   N = 10,
                   D = 1,
                   y = y,
                   x = rep(1, 10))) -> stan0







m0 <- "
data {
  
  int<lower = 1> N;  // number of observations
  int<lower = 2> ncat;  // number of DAC categories
  int Y1[N];  // categorical response variable for hurdle (D = 2, A = 3, or C = 1?) (midpoint 4)
  int<lower = 1> K;  // number of population-level effects post hurdle
  matrix[N, K] X;  // population-level design matrix post hurdle
  
  // data for group-level effects of ID 1 = person
  int<lower = 1> N_1;  // number of grouping levels
  int<lower = 1> M_1;  // number of coefficients per level (random effects)
  int<lower = 1> J_1[N];  // grouping indicator per observation
  vector[N] Z_1_1;  // group 1 (person)-level predictor values
  
  // data for group-level effects of ID 2
  int<lower = 1> N_2;  // number of grouping levels
  int<lower = 1> M_2;  // number of coefficients per level
  int<lower = 1> J_2[N];  // grouping indicator per observation
  vector[N] Z_2_1;  // group 2 (item)-level predictor values
  
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
  
  
  vector[Kc] b_dist;  // population-level effects
  
}


model {
  
  // initialize linear predictor term
  vector[N] eta1 = Intercept_eta1 + Xc * b_eta1;
  // initialize linear predictor term
  vector[N] eta2 = Intercept_eta2 + Xc * b_eta2;
  
  // linear predictor matrix
  vector[ncat] eta[N];
  
  for (n in 1:N) {
    eta[n] = [0, eta1[n], eta2[n]]';
  }
  
  // priors including all constants
  target += normal_lpdf(b_eta1 | 0, b_prior); 
  target += normal_lpdf(b_eta2 | 0, b_prior);
  
  target += normal_lpdf(Intercept_eta1 | 0, 5);
  target += normal_lpdf(Intercept_eta2 | 0, 5);

  // likelihood including all constants
  if (!prior_only) {  
    for (n in 1:N) {
      if (Y1[n] == 2){
        2 ~ categorical_logit(eta[n]);
      }
      else if (Y1[n] == 3){
        3 ~ categorical_logit(eta[n]);
      }
      else {
        1 ~ categorical_logit(eta[n]);
      }
    }
  }
}


"





















m0 <- "
data {
  
  int<lower = 1> N;  // number of observations
  int<lower = 2> ncat;  // number of DAC categories
  int Y1[N];  // categorical response variable for hurdle (D = 2, A = 3, or C = 1?) (midpoint 4)
  int<lower = 1> K;  // number of population-level effects post hurdle
  matrix[N, K] X;  // population-level design matrix post hurdle
  
  // data for group-level effects of ID 1 = person
  int<lower = 1> N_1;  // number of grouping levels
  int<lower = 1> M_1;  // number of coefficients per level (random effects)
  int<lower = 1> J_1[N];  // grouping indicator per observation
  vector[N] Z_1_1;  // group 1 (person)-level predictor values
  
  // data for group-level effects of ID 2
  int<lower = 1> N_2;  // number of grouping levels
  int<lower = 1> M_2;  // number of coefficients per level
  int<lower = 1> J_2[N];  // grouping indicator per observation
  vector[N] Z_2_1;  // group 2 (item)-level predictor values
  
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
  
  vector<lower = 0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // standardized group-level effects
  vector<lower = 0>[M_2] sd_2;  // group-level standard deviations
  vector[N_2] z_2[M_2];  // standardized group-level effects
  vector<lower = 0>[M_1] sd_3;  // group-level standard deviations
  vector[N_1] z_3[M_1];  // standardized group-level effects
  vector<lower = 0>[M_2] sd_4;  // group-level standard deviations
  vector[N_2] z_4[M_2];  // standardized group-level effects
  
  
  vector[Kc] b;  // population-level effects
  real Intercept;  // temporary intercept for centered predictors
  
  vector<lower = 0>[M_1] sd_1a;  // group-level standard deviations
  vector[N_1] z_1a[M_1];  // standardized group-level effects
  vector<lower = 0>[M_2] sd_2a;  // group-level standard deviations
  vector[N_2] z_2a[M_2];  // standardized group-level effects
  vector<lower = 0>[M_1] sd_1b;  // group-level standard deviations
  vector[N_1] z_1b[M_1];  // standardized group-level effects
  vector<lower = 0>[M_2] sd_2b;  // group-level standard deviations
  vector[N_2] z_2b[M_2];  // standardized group-level effects
  real Intercept_phi;  // temporary intercept for centered distance var
  vector[Kc] b_dist;  // population-level effects
  
}

transformed parameters {
  vector[N_1] r_1_eta1_1;  // actual group-level effects
  vector[N_2] r_2_eta1_1;  // actual group-level effects
  vector[N_1] r_3_eta2_1;  // actual group-level effects
  vector[N_2] r_4_eta2_1;  // actual group-level effects
  
  r_1_eta1_1 = (sd_1[1] * (z_1[1]));
  r_2_eta1_1 = (sd_2[1] * (z_2[1]));
  r_3_eta2_1 = (sd_3[1] * (z_3[1]));
  r_4_eta2_1 = (sd_4[1] * (z_4[1]));
}


model {
  
  // initialize linear predictor term
  vector[N] eta1 = Intercept_eta1 + Xc * b_eta1;
  // initialize linear predictor term
  vector[N] eta2 = Intercept_eta2 + Xc * b_eta2;
  
  // linear predictor matrix
  vector[ncat] eta[N];
  
  for (n in 1:N) {
    // add more terms to the linear predictor
    eta1[n] += r_1_eta1_1[J_1[n]] * Z_1_1[n] + r_2_eta1_1[J_2[n]] * Z_2_1[n];
    // add more terms to the linear predictor
    eta2[n] += r_3_eta2_1[J_1[n]] * Z_1_1[n] + r_4_eta2_1[J_2[n]] * Z_2_1[n];
    
    eta[n] = [0, eta1[n], eta2[n]]';
  }
  
  // priors including all constants
  target += normal_lpdf(b_eta1 | 0, b_prior); //b_eta1 ~ normal(0, b_prior);
  target += normal_lpdf(b_eta2 | 0, b_prior);
  
  target += normal_lpdf(Intercept_eta1 | 0, 5);
  target += normal_lpdf(Intercept_eta2 | 0, 5);
  
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
  
  // likelihood including all constants
  if (!prior_only) {  
    for (n in 1:N) {
      if (Y1[n] == 2){
        2 ~ categorical_logit(eta[n]);
      }
      else if (Y1[n] == 3){
        3 ~ categorical_logit(eta[n]);
      }
      else {
        1 ~ categorical_logit(eta[n]);
      }
    }
  }
}


"