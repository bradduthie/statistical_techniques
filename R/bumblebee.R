

N          <- 256;
radiations <- c("None", "Low", "Medium", "High");
colony_dat <- c("A", "B", "C", "D", "E", "F", "G", "H");
survive    <- c("Yes", "No");
checks     <- FALSE;
distrtest  <- FALSE;
cor_test   <- FALSE;
ctest      <- FALSE;
while(checks == FALSE){
  radiation <- c(rep(x = radiations[1], times = 64),
                 rep(x = radiations[2], times = 64),
                 rep(x = radiations[3], times = 64),
                 rep(x = radiations[4], times = 64))
  colons    <- c(rep(x = colony_dat[1], times = 8),
                 rep(x = colony_dat[2], times = 8),
                 rep(x = colony_dat[3], times = 8),
                 rep(x = colony_dat[4], times = 8),
                 rep(x = colony_dat[5], times = 8),
                 rep(x = colony_dat[6], times = 8),
                 rep(x = colony_dat[7], times = 8),
                 rep(x = colony_dat[8], times = 8));
  colony    <- rep(colons, 4);
  survived  <- sample(x = survive, size = N, replace = TRUE,
                      prob = c(0.55, 0.45));
  mass      <- round(rnorm(n = N, mean = 0.225, sd = 0.0539), digits = 3);
  CO2       <- round(rnorm(n = N, mean = 15.896, sd = 5.5884), digits = 3);
  nect_val  <- rpois(n = N, lambda = 3)/20 + runif(n = N, min = 0, max = 0.3);
  nectar    <- round(nect_val, digits = 3);
  if(shapiro.test(nectar)$p.value < 0.04 & shapiro.test(mass)$p.value > 0.1 &
     shapiro.test(CO2)$p.value > 0.1){
    distrtest <- TRUE;
  }else{
    distrtest <- FALSE;
  }
  c1 <- cor.test(mass, nectar, alternative = "greater", method = "spearman");
  c2 <- cor.test(mass, CO2);
  c3 <- cor.test(CO2, nectar);
  if(c1$p.value < 0.04 & c2$p.value < 0.04 & c3$p.value > 0.1){
    cor_test <- TRUE;
  }else{
    cor_test <- FALSE;
  }
  chi_test <- chisq.test(x = radiation, y = survived);
  props1   <- table(survived, radiation)[1,]/table(survived, radiation)[2,];
  props    <- as.numeric(props1);
  if(chi_test$p.value < 0.04 & props[1] > props[2] & props[4] < props[2]){
    ctest <- TRUE;
  }else{
    ctest <- FALSE;
  }
  if(distrtest == TRUE & cor_test == TRUE & ctest == TRUE){
    checks <- TRUE;
  }else{
    checks <- FALSE;
  }
}
# Still need to do some manual tweaks.

ID        <- 1:N;
bumblebee <- data.frame(ID, radiation, colony, survived, mass, CO2, nectar);
write.csv(bumblebee, "data/bumblebee.csv", row.names = FALSE);




