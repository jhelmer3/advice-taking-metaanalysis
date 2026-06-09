
## messy simulation

library(tidyverse)
library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# data

n_people <- 1000

# parameters

beta_decline_0 <- 1.5
beta_adopt_0 <- 0.5

mu_beta_0 <- 0.5
phi_beta_0 <- 3

# transformed parameters

mu_decline <- beta_decline_0
mu_adopt <- beta_adopt_0

mu_beta <- 1 / exp(1 - mu_beta_0)
phi_beta <- exp(phi_beta_0)

etas <- tibble(eta_decline = exp(mu_decline) / (1 + exp(mu_decline) + exp(mu_adopt)),
               eta_adopt = exp(mu_adopt) / (1 + exp(mu_decline) + exp(mu_adopt)),
               eta_compromise = 1 - eta_decline - eta_adopt) |>
  pivot_longer(everything(), names_to = "param", values_to = "eta")

# simulating

d <- tibble(id = 1:n_people, 
       dac = sample(c("decline" = 1, "adopt" = 2, "compromise" = 3),
                    size = n_people, prob = pull(etas, eta), replace = T),
       woa = ifelse(dac == 3, rbeta(sum(ifelse(dac == 3, 1, 0)),
                                      mu_beta * phi_beta, (1 - mu_beta) * phi_beta),
                    ifelse(dac == 2, 1, 0)))


# modeling

m <- " 
  data {
    int<lower = 0> N;
    int ncat;
    vector[N] dac;
  }

  parameters {
    real mu_decline;
    real mu_adopt;
  }

  model {
    
    target += normal_lpdf(mu_decline | 0, 5);
    target += normal_lpdf(mu_adopt | 0, 5);
    
    vector[N] eta_decline = ones_vector(N) * mu_decline;
    vector[N] eta_adopt = ones_vector(N) * mu_adopt;
  
    vector[ncat] eta[N];
    
      for (n in 1:N) {
        
        eta[n] = [eta_decline[n], eta_adopt[n], 0]';
      
        if (dac[n] == 1){
          1 ~ categorical_logit(eta[n]);
        }
        else if (dac[n] == 2){
          2 ~ categorical_logit(eta[n]);
        }
        else {
          3 ~ categorical_logit(eta[n]);
        }
      }
  }
"

fit <- stan(model_code = m,
     data = list(N = nrow(d),
                 ncat = 3,
                 dac = pull(d, dac)))
fit

fit_tranf <- fit |>
  as.data.frame() |>
  mutate(eta_decline = exp(mu_decline) / (1 + exp(mu_decline) + exp(mu_adopt)),
         eta_adopt = exp(mu_adopt) / (1 + exp(mu_decline) + exp(mu_adopt)),
         eta_compromise = 1 / (1 + exp(mu_decline) + exp(mu_adopt)))

# visualizing

fit_tranf |>
  pivot_longer(starts_with("eta"), names_to = "param", values_to = "eta") |>
  mutate(param = factor(param, levels = c("eta_decline", "eta_adopt", "eta_compromise"))) |>
  ggplot(aes(x = param, y = eta)) +
  geom_violin() +
  geom_point(data = etas)


fit_tranf|>
  mutate(dac = pmap_int(list(eta_decline, eta_adopt, eta_compromise),
                        \(x, y, z) sample(c(1, 2, 3), size = 1, prob = c(x, y, z))),
         woa = ifelse(dac == 1, 0, ifelse(dac == 2, 1, 0.5))) |>
  ggplot(aes(x = woa)) +
  geom_density(color = "slategray3") +
  geom_density(data = d)

tibble(p1 = c(.1, .4, .7),
       p2 = 1 - p1) 


ggplot(d, aes(x = factor(dac))) +
  geom_bar()

ggplot(d, aes(x = woa)) +
  geom_density()

# modeling

{
m2 <- " 
  data {
    int<lower = 0> N;
    int ncat;
    vector[N] dac;
    vector[N] woa;
  }

  parameters {
    real mu_decline;
    real mu_adopt;
    
    real mu_beta_mu;
    real mu_beta_phi;
  }

  model {
    
    target += normal_lpdf(mu_decline | -3, 0.5);
    target += normal_lpdf(mu_adopt | -3, 0.5);
    target += normal_lpdf(mu_beta_mu | 0, 5);
    target += normal_lpdf(mu_beta_phi | 0, 5);
    
    vector[N] eta_decline = ones_vector(N) * mu_decline;
    vector[N] eta_adopt = ones_vector(N) * mu_adopt;
    
    vector[N] beta_mu = mu_beta_mu * ones_vector(N); 
    vector[N] beta_phi = mu_beta_phi * ones_vector(N);
    vector[N] beta_A;
    vector[N] beta_B; 
  
    vector[ncat] eta[N];
    
      for (n in 1:N) {
        
        eta[n] = [eta_decline[n], 0, eta_adopt[n]]';
        
        beta_A[n] = inv_logit(beta_mu[n]) * exp(beta_phi[n]);
        beta_B[n] = (1 - inv_logit(beta_mu[n])) * exp(beta_phi[n]);
      
        if (dac[n] == 1){
          1 ~ categorical_logit(eta[n]);
        }
        else if (dac[n] == 2){
          2 ~ categorical_logit(eta[n]);
        }
        else {
          3 ~ categorical_logit(eta[n]);
          woa[n] ~ beta(beta_A[n], beta_B[n]);
        }
      }
  }
"

fit2 <- stan(model_code = m2,
            data = list(N = nrow(d),
                        ncat = 3,
                        dac = pull(d, dac),
                        woa = pull(d, woa)))
fit2
}

fit_tranf2 <- fit2 |>
  as.data.frame() |>
  mutate(eta_decline = exp(mu_decline) / (1 + exp(mu_decline) + exp(mu_adopt)),
         eta_adopt = exp(mu_adopt) / (1 + exp(mu_decline) + exp(mu_adopt)),
         eta_compromise = 1 / (1 + exp(mu_decline) + exp(mu_adopt)),
         beta_a = exp(mu_beta_mu) / (1 + exp(mu_beta_mu)) * exp(mu_beta_phi),
         beta_b = (1 - exp(mu_beta_mu) / (1 + exp(mu_beta_mu))) * exp(mu_beta_phi))

# visualizing

fit_tranf2 |>
  pivot_longer(starts_with("eta"), names_to = "param", values_to = "eta") |>
  mutate(param = factor(param, levels = c("eta_decline", "eta_adopt", "eta_compromise"))) |>
  ggplot(aes(x = param, y = eta)) +
  geom_violin() +
  geom_point(data = etas)

fit_tranf2 |>
  mutate(dac = pmap_int(list(eta_decline, eta_adopt, eta_compromise),
                        \(x, y, z) sample(c(1, 2, 3), size = 1, prob = c(x, y, z))),
         woa = pmap_dbl(list(dac, beta_a, beta_b),
                             \(dac, a, b) ifelse(dac == 1, 0,
                                            ifelse(dac == 2, 1, rbeta(1, a, b)))),
         iter = row_number()) |>
  ggplot(aes(x = woa)) +
  geom_density(color = "slategray3") +
  geom_density(data = d)



# archive 

rbeta(10000, 10, 10) |>
  density() |>
  plot()
