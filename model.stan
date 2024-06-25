//Model
data{
  int<lower=1> N;  // total number of observations
  int Skill[N];  // response variable
  int<lower=2> nthres;  // number of thresholds
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  // data for group-level effects of courses
  int<lower=1> N_courses;  // number of grouping levels
  int<lower=1> J_1_1[N];  // grouping indicator per observation
  real W_1_1[N];  // multi-membership weights
  int<lower=1> J_1_2[N];  // grouping indicator per observation
  real W_1_2[N];  // multi-membership weights
  int<lower=1> J_1_3[N];  // grouping indicator per observation
  real W_1_3[N];  // multi-membership weights
  int<lower=1> J_1_4[N];  // grouping indicator per observation
  real W_1_4[N];  // multi-membership weights
  int<lower=1> J_1_5[N];  // grouping indicator per observation
  real W_1_5[N];  // multi-membership weights
  int<lower=1> J_1_6[N];  // grouping indicator per observation
  real W_1_6[N];  // multi-membership weights
  int<lower=1> J_1_7[N];  // grouping indicator per observation
  real W_1_7[N];  // multi-membership weights
  int<lower=1> J_1_8[N];  // grouping indicator per observation
  real W_1_8[N];  // multi-membership weights
  // data for group-level effects of students
  int<lower=1> N_students;  // number of grouping levels
  // group-level predictor values
  int<lower=1> J_2[N];  // grouping indicator per observation
}
parameters {
  vector[K] b;  // population-level effects
  ordered[nthres] Intercept;  // temporarSkill thresholds for centered predictors
  real<lower=0> sd_courses;  // group-level standard deviations
  real<lower=0> sd_students;  // group-level standard deviations
  vector[N_courses] r_courses;  // actual group-level effects
  vector[N_students] r_student;  //actual group-level effects
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior 
  lprior += student_t_lpdf(Intercept | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_courses | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_students | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  // likelihood including constants
  // initialize linear predictor term
  vector[N] mu = rep_vector(0.0, N);
  target += normal_lpdf(r_courses|0,sd_courses);
  target += normal_lpdf(r_student|0,sd_students);
  mu += X * b;
  for (n in 1:N) {
  // add more terms to the linear predictor
	mu[n] += W_1_1[n] * r_courses[J_1_1[n]] + W_1_2[n] * r_courses[J_1_2[n]] + W_1_3[n] * r_courses[J_1_3[n]] + W_1_4[n] * r_courses[J_1_4[n]] + W_1_5[n] * r_courses[J_1_5[n]] + W_1_6[n] * r_courses[J_1_6[n]] + W_1_7[n] * r_courses[J_1_7[n]] + W_1_8[n] * r_courses[J_1_8[n]] + r_student[J_2[n]];
  }
  for (n in 1:N) {
    target += ordered_logistic_lpmf(Skill[n] | mu[n], Intercept);
}
  // priors including constants
  target += lprior;  
}
