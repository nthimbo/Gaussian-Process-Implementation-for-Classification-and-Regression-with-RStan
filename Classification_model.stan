functions {
  matrix L_cov_exp_quad_ARD(vector[] x,
                            real alpha,
                            vector rho,
                            real delta) {
    int N = size(x);
    matrix[N, N] K;
    real sq_alpha = square(alpha);
    for (i in 1:(N-1)) {
      K[i, i] = sq_alpha + delta;
      for (j in (i + 1):N) {
        K[i, j] = sq_alpha* exp(-0.5 * dot_self((x[i] - x[j]) ./ rho));
                      
        K[j, i] = K[i, j];
      }
    }
    K[N, N] = sq_alpha + delta;
    return cholesky_decompose(K);
  }
  
  
vector gp_pred_rng(vector[] x_pred,
vector y1, vector[] x,
real magnitude, real length_scale) {
int N = rows(y1);
int N_pred = size(x_pred);
vector[N_pred] f2;

{
matrix[N, N] K = 200 *cov_exp_quad(x, magnitude, length_scale) +
diag_matrix(rep_vector(1e-1, N));
matrix[N, N] L_K = cholesky_decompose(K);
vector[N] L_K_div_y1 = mdivide_left_tri_low(L_K, y1);
vector[N] K_div_y1 = mdivide_right_tri_low(L_K_div_y1', L_K)';
matrix[N, N_pred] k_x_x_pred = cov_exp_quad(x, x_pred, magnitude, length_scale);
f2 = (k_x_x_pred' * K_div_y1);
}
return f2;
}
  
}
data {
  int<lower=1> N;
  int<lower=1> D;                                                                                           
  vector[D] x[N];
  //vector[N] y;
  int<lower=1> N_pred;
  vector[D] x_pred[N_pred];
  int<lower=0,upper=2> y[N];
}


transformed data {
  real delta = 1e-9;
}

parameters {
  real<lower=0> magnitude;
  real<lower=0> length_scale;
  vector<lower=0>[D] rho1;
  real<lower=0> rho;
  real<lower=0> alpha;
  real<lower=0> sigma;
  vector[N] eta;
}

transformed parameters {
vector[N] f1;
vector[N] f;

   {
        matrix[N, N] K;
        matrix[N, N] L_K;
        matrix[N, N] L_K1;
        L_K1 = L_cov_exp_quad_ARD(x, alpha, rho1, delta);
        f1 = L_K1 * eta;
        
        

        K = 69 * cov_exp_quad(x, alpha, rho)+ diag_matrix(rep_vector(1e-1, N));
        L_K = cholesky_decompose(K);
        f = L_K * eta;
    }
}




model {
  
  length_scale ~ gamma(2, 2);
  magnitude ~ cauchy(0,5);
  rho1 ~ inv_gamma(5, 5);
  alpha ~ std_normal();
  sigma ~ std_normal();
  eta ~ normal(0, 5);
  rho ~ inv_gamma(5, 5);
  
  //y1 ~ normal(f1, sigma);
  y ~ multinomial(softmax(f));
}

generated quantities {
vector[N_pred] f_pred = gp_pred_rng(x_pred, f, x, magnitude, length_scale);
int y_pred[N_pred];
int y_pred_in[N];

y_pred_in = multinomial_rng(softmax(f),N); // in sample predictin
y_pred = multinomial_rng(softmax(f_pred),N_pred); // out of sample predictions

}


