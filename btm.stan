data {
  int<lower = 0> K;                      // número de empresas
  int<lower = 0> N;                      // número de comparações
  int<lower = 1, upper = K> player1[N];  // empresa julgada "superior"
  int<lower = 1, upper = K> player0[N];  // empresa julgada "inferior"
  int<lower = 0, upper = 1> y[N];        // 1 se player1 venceu, 0 se player0 venceu
}
parameters {
  vector[K] alpha;                       // posição latente estimada para cada empresa
}
model {
  alpha ~ normal(0, 1);                  // prior
  y ~ bernoulli_logit(alpha[player1] - alpha[player0]);  // likelihood do modelo BT
}
