
// 31 jul 2023
functions {
  vector do_dt(real t, vector y,  real ks1m1, real ks2m2,  real ds1m1, real ds2m2,
  real ds1s2, real ds2s1){
    real pi600 = 2*pi()/.6; // Eigenfrequency s1, m1
    real pi610 = 2*pi()/.61;  // Eigenfrequency s2, m2

    real dm1dt = pi600; 
    real ds1dt = pi600 + 
              .5*( 
              ks1m1 * sin(y[1] - y[2] + ds1m1) + 
              (1-ks1m1) * sin(y[4] - y[2] + ds1s2)  ); 
    real dm2dt = pi610;
    real ds2dt = pi610 + 
              .5*( 
              ks2m2 * sin(y[3] - y[4] + ds2m2) + 
              (1-ks2m2) * sin(y[2] - y[4] + ds2s1)  ); 
    return to_vector([dm1dt,ds1dt,dm2dt,ds2dt]);
  }
}

data {
  int<lower=1> n_times; // Number of time steps minus one
  int<lower=1> n_gen_times;
  real time0;
  real phase[n_times] ; // Observed taps, minus initial state
  real time[n_times] ;
  int<lower=1>  names_indicator[n_times]; // names of oscillators
  real time_gen[n_gen_times]; // times to generate simulated data for
}

parameters {
  real <lower = 0,upper=1> ks1m1;
  real <lower = 0,upper=1> ks2m2;
  // real <lower = -pi()/2,upper=pi()/2> ds1m1; // 5_aug
  // real <lower = -pi()/2,upper=pi()/2> ds2m2; // 5_aug
  real <lower = -pi(),upper=pi()> ds1m1; // 5_aug
  real <lower = -pi(),upper=pi()> ds2m2; // 5_aug
  real <lower = -pi(),upper=pi()> ds1s2; // 5_aug
  real <lower = -pi(),upper=pi()> ds2s1; // 5_aug
  real <lower = 0, upper = 20> kappa;
  real y0_s1;
  real y0_s2;

}

transformed parameters {
  vector[4] y0 = [.0,y0_s1,.0,y0_s2]' ;
  vector[4] omega[n_times] = ode_rk45(do_dt, y0, time0, time, ks1m1, ks2m2, ds1m1, ds2m2, ds1s2, ds2s1) ;
}

model {
  ks1m1 ~ beta(19,2) ;
  ks2m2 ~ beta(19,2) ;
  ds1m1 ~ normal(.0,.5) ;
  ds2m2 ~ normal(.0,.5) ;
  ds1s2 ~ normal(.0,.5) ;
  ds2s1 ~ normal(.0,.5) ;
  kappa ~ lognormal(0.1,1.0) ;
  y0_s1 ~ normal(.0,.15) ;
  y0_s2 ~ normal(.0,.15) ;
  real pi2 = 2*pi() ;
  for (n in 1:n_times){
    phase[n] ~ von_mises(fmod(omega[n,names_indicator[n]] + pi(), pi2 ) - pi(), kappa) ;
  }
}

generated quantities {
  vector[4] phase_gen[n_gen_times] = ode_rk45(do_dt, y0, time0, time_gen, ks1m1, ks2m2, ds1m1, ds2m2,ds1s2,ds2s1) ;
}

