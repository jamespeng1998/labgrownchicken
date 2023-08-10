# Load libraries.
library(pwr); library(data.table); library(DT); library(tidyverse);

# Determine the sample sizes.
pwr.t.test(
  d = 0.25/0.5, # Effect size = 0.25, standard deviation = 0.5.
  sig.level = 0.05, # alpha = 5%.
  power = 0.95, # Power = 95%.
  alternative = 'two.sided' # Two sided test.
); # n = 105

# Start: Simulate 1,000 experiments.

# Start: Create function.
analyze.experiment <- function(the.dat) {
  require(data.table)
  setDT(the.dat)
  
  the.test <- t.test(x = the.dat[Group == "Treatment", rating_diff],
                     y = the.dat[Group == "Control", rating_diff],
                     alternative = "two.sided")
  
  the.effect <- the.test$estimate[1] - the.test$estimate[2]
  p <- the.test$p.value
  
  result <- data.table(effect = the.effect, p = p)
  
  return(result)
}
# End: Create function.

# Start: Set simulation parameters.
n <- 105; # Calculated above. Sample size = 105.
B <- 1000; # Number of experiments = 1,000.
alpha <- 0.05; # Significance level = 5%, confidence interval = 95%.
Experiment <- 1:B; # Simulation iterator.
RNGversion(vstr = 3.6); # Random number generator version.
# End: Set simulation parameters.

# Start: Q1, scenario 1: no effect.
set.seed(seed = 541);
Group <- c(rep.int(x = "Treatment", times = n/2),
           rep.int(x = "Control", times = n/2));

sim.dat <- as.data.table(expand.grid(Experiment = Experiment, Group = Group));
setorderv(x = sim.dat, cols = c("Experiment", "Group"), order = c(1,1));
sim.dat[Group == "Control",
        rating_diff :=
          round(x = rnorm(n = .N, mean = 3, sd = 0.5), digits = 4) -
          round(x = rnorm(n = .N, mean = 3, sd = 0.5), digits = 4) ];
sim.dat[Group == "Treatment",
        rating_diff :=
          round(x = rnorm(n = .N, mean = 3, sd = 0.5), digits = 4) -
          round(x = rnorm(n = .N, mean = 3, sd = 0.5), digits = 4) ];
dim(sim.dat);

exp.results <- sim.dat[, analyze.experiment(the.dat = .SD),
                       keyby = "Experiment"];

# Preview results.
DT::datatable(data = round(x = exp.results[1:100, ], digits = 3), rownames = F);

# Calculate simulation metrics.
exp.results[, mean(p < 0.05)]; # Power = 5.6% (which is close to 5%).
exp.results[, summary(effect)]; # Mean effect = -0.001565 (close to 0).
exp.results[, quantile(x = effect, probs = c(alpha/2, 1 - alpha/2))]; # CI (-0.2769813, 0.2743340)
# End: Q1, scenario 1: no effect.

# Start: Q1, scenario 2: effect of -0.5.
set.seed(seed = 541);
Group <- c(rep.int(x = "Treatment", times = n/2),
           rep.int(x = "Control", times = n/2));

sim.dat <- as.data.table(expand.grid(Experiment = Experiment, Group = Group));
setorderv(x = sim.dat, cols = c("Experiment", "Group"), order = c(1,1));
sim.dat[Group == "Control",
        rating_diff :=
          round(x = rnorm(n = .N, mean = 3, sd = 0.5), digits = 4) -
          round(x = rnorm(n = .N, mean = 3, sd = 0.5), digits = 4) ];
sim.dat[Group == "Treatment",
        rating_diff :=
          round(x = rnorm(n = .N, mean = 2.5, sd = 0.5), digits = 4) -
          round(x = rnorm(n = .N, mean = 3, sd = 0.5), digits = 4) ];
dim(sim.dat);

exp.results <- sim.dat[, analyze.experiment(the.dat = .SD),
                       keyby = "Experiment"];

# Preview results.
DT::datatable(data = round(x = exp.results[1:100, ], digits = 3), rownames = F);

# Calculate simulation metrics.
exp.results[, mean(p < 0.05)]; # Power = 93.7%.
exp.results[, summary(effect)]; # Mean effect = -0.5016.
exp.results[, quantile(x = effect, probs = c(alpha/2, 1 - alpha/2))]; # CI (-0.7769813, -0.2256660)
# End: Q1, scenario 2: effect of -0.5.

# Start: Q2, scenario 1: no effect.
set.seed(seed = 541);
Group <- c(rep.int(x = "Treatment", times = n/2),
           rep.int(x = "Control", times = n/2));

sim.dat <- as.data.table(expand.grid(Experiment = Experiment, Group = Group));
setorderv(x = sim.dat, cols = c("Experiment", "Group"), order = c(1,1));
sim.dat[Group == "Control",
        rating_diff :=
          round(x = rnorm(n = .N, mean = 8, sd = 2), digits = 4) -
          round(x = rnorm(n = .N, mean = 8, sd = 2), digits = 4) ];
sim.dat[Group == "Treatment",
        rating_diff :=
          round(x = rnorm(n = .N, mean = 8, sd = 2), digits = 4) -
          round(x = rnorm(n = .N, mean = 8, sd = 2), digits = 4) ];
dim(sim.dat);

exp.results <- sim.dat[, analyze.experiment(the.dat = .SD),
                       keyby = "Experiment"];

# Preview results.
DT::datatable(data = round(x = exp.results[1:100, ], digits = 3), rownames = F);

# Calculate simulation metrics.
exp.results[, mean(p < 0.05)]; # Power = 5.6%.
exp.results[, summary(effect)]; # Mean effect = -0.006261 (close to 0).
exp.results[, quantile(x = effect, probs = c(alpha/2, 1 - alpha/2))]; # CI (-1.107895, 1.097316)
# End: Q2, scenario 1: no effect.

# Start: Q2, scenario 2: effect of -$2.00.
set.seed(seed = 541);
Group <- c(rep.int(x = "Treatment", times = n/2),
           rep.int(x = "Control", times = n/2));

sim.dat <- as.data.table(expand.grid(Experiment = Experiment, Group = Group));
setorderv(x = sim.dat, cols = c("Experiment", "Group"), order = c(1,1));
sim.dat[Group == "Control",
        rating_diff :=
          round(x = rnorm(n = .N, mean = 8, sd = 2), digits = 4) -
          round(x = rnorm(n = .N, mean = 8, sd = 2), digits = 4) ];
sim.dat[Group == "Treatment",
        rating_diff :=
          round(x = rnorm(n = .N, mean = 6, sd = 1.5), digits = 4) -
          round(x = rnorm(n = .N, mean = 8, sd = 2), digits = 4) ];
dim(sim.dat);

exp.results <- sim.dat[, analyze.experiment(the.dat = .SD),
                       keyby = "Experiment"];

# Preview results.
DT::datatable(data = round(x = exp.results[1:100, ], digits = 3), rownames = F);

# Calculate simulation metrics.
exp.results[, mean(p < 0.05)]; # Power = 95.9%.
exp.results[, summary(effect)]; # Mean effect = -2.0051 (close to -2).
exp.results[, quantile(x = effect, probs = c(alpha/2, 1 - alpha/2))]; # CI (-3.0173269, -0.9177123)
# End: Q2, scenario 2: effect of -$2.00.

# End: Simulate 1,000 experiments.