require(distr)

sim <- new("Simulation",
           seed = setRNG(),
           distribution = Norm(mean = 0, sd = 1),
           filename="sim_01",
           runs = 1000,
           samplesize = 30)
#generate an object of class Simulation
#(ideal) situation:  x_i~i.i.d. N(0,1) 

contsim <- new("Contsimulation",
               seed = setRNG(),
               distribution.id = Norm(mean = 0, sd = 1),
               distribution.c = Norm(mean = 0, sd = 9),
               rate = 0.1,
               filename="contsim_01",
               runs = 1000,
               samplesize = 30)

#generate an object of class Contsimulation

#fill the data-slots
simulate(sim)
simulate(contsim)


#procedures: 

#a location M-estimator of Huber-type
psim <- function(theta,y,m0){
  mean(pmin(pmax(-m0, y - theta), m0))
}
mestimator <- function(x, m = 0.7) {
  uniroot(psim,
          low = -20,
          up = 20,
          tol = 1e-10,
          y = x,
          m0 = m,
          maxiter = 20)$root
}

#competing procedures: mean, median, M-estimator

#are evaluated in ideal and contaminated situation
result.id.mean <- evaluate(sim, mean)
result.id.mest <- evaluate(sim, mestimator)
result.id.median <- evaluate(sim, median)


result.cont.mean <- evaluate(contsim, mean)
result.cont.mest <- evaluate(contsim, mestimator)
result.cont.median <- evaluate(contsim, median)


##graphics: boxplots of each procedure in each situation

opar <- par(mfcol=c(1,3))
yrange.id = range(result(result.id.median))
boxplot(result(result.id.mean), ylim  = yrange.id)
title("Results for Mean under ideal conditions")
boxplot(result(result.id.mest), ylim  = yrange.id)
title("Results for Huber-Estimator under ideal conditions")
boxplot(result(result.id.median), ylim  = yrange.id)
title("Results for Median under ideal conditions")

cat("Hit <enter> to continue...")
readline()

yrange.cont = range(result(result.cont.mean))
boxplot(result(result.cont.mean), ylim  = yrange.cont)
title("Results for Mean under worse conditions")
boxplot(result(result.cont.mest), ylim  = yrange.cont)
title("Results for Huber-Estimator under worse conditions")
boxplot(result(result.cont.median), ylim  = yrange.cont)
title("Results for Median under worse conditions")
par(opar)
