
// 24 sept 2024
functions {
  vector do_dt(real t, vector y,  real ks1m1, real ks2m2,  real ds1m1, real ds2m2,
  real ds1s2, real ds2s1){
    real pi600 = 2*pi()/.6; // Eigenfrequency s1, m1
    real pi610 = 2*pi()/.610; //.609375;  // Eigenfrequency s2, m2 (600 * 609.375/9.375) /1000 = 39
    
    real dm1dt = pi600; 
    real ds1dt = dm1dt + 
    ( 
      ks1m1 * sin(y[1] - y[2] + ds1m1) + 
      (1-ks1m1) * sin(y[4] - y[2] + ds1s2)  ); 
    real dm2dt = pi610;
    real ds2dt = dm2dt + 
    ( 
        ks2m2 * sin(y[3] - y[4] + ds2m2) + 
        (1-ks2m2) * sin(y[2] - y[4] + ds2s1)  ); 
        return to_vector([dm1dt,ds1dt,dm2dt,ds2dt]);
  }
}

data {
  int<lower=1> n_times; // Number of time steps minus one
  int<lower=1> n_gen_times;
  real time0;
  array[n_times] real observed; // Observed taps, minus initial state
  array[n_times] real time ;
  array[n_times] int<lower=1> names_indicator; // names of oscillators
  array[n_gen_times] real time_gen; // times to generate simulated data for
}

parameters {
  real<lower = 0,upper=1> ks1m1;
  real<lower = 0,upper=1> ks2m2;
  real <lower = -pi(),upper=pi()> ds1m1; // 5_aug
  real <lower = -pi(),upper=pi()> ds2m2; // 5_aug
  real <lower = -pi(),upper=pi()> ds1s2; // 5_aug
  real <lower = -pi(),upper=pi()> ds2s1; // 5_aug
  // real <lower = 0, upper = 20> kappa[4];
  array[4] real<lower=0> observation_std; // Variance of observation
  real y0_s1;
  real y0_s2;
}

transformed parameters {
  vector[4] y0 = [.0,y0_s1,.0,y0_s2]' ;
  array[n_times] vector[4] omega = ode_rk45(do_dt, y0, time0, time, ks1m1, ks2m2, ds1m1, ds2m2, ds1s2, ds2s1) ;
}

model {
  ks1m1 ~ beta(19,2) ;
  ks2m2 ~ beta(19,2) ;
  ds1m1 ~ normal(.0,.5) ;
  ds2m2 ~ normal(.0,.5) ;
  ds1s2 ~ normal(.0,.5) ;
  ds2s1 ~ normal(.0,.5) ;
  
  observation_std[1] ~ normal(.0,.1) ;
  observation_std[2] ~ normal(.0,1) ;
  observation_std[3] ~ normal(.0,.1) ;
  observation_std[4] ~ normal(.0,1) ;

  // kappa ~ lognormal(0.1,1.0) ;
  y0_s1 ~ normal(.0,.15) ;
  y0_s2 ~ normal(.0,.15) ;
  real pi2 = 2*pi() ;
  real state;
  
  for (n in 1:n_times){
    state = fmod(omega[n,names_indicator[n]] + pi(), pi2 ) - pi(); 
    // single value returned here because we have only one observation per time
    observed[n] ~ normal(state, (observation_std[names_indicator[n]]));

    // names_indicator defines which unit is active at n
  }
}

generated quantities {
  array[n_gen_times] vector[4] observations_generated;
  array[n_gen_times] vector[4] states_generated = ode_rk45(do_dt, y0, time0, time_gen, ks1m1, ks2m2, ds1m1, ds2m2,ds1s2,ds2s1) ;
  for (n in 1:n_gen_times){
    observations_generated[n,1] = normal_rng(states_generated[n,1],observation_std[1]);
    observations_generated[n,2] = normal_rng(states_generated[n,2],observation_std[2]);
    observations_generated[n,3] = normal_rng(states_generated[n,3],observation_std[3]);
    observations_generated[n,4] = normal_rng(states_generated[n,4],observation_std[4]);
  }
  
}

