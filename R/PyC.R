

N         <- 240;
s_lm_chk1 <- FALSE;
s_lm_chk2 <- FALSE;
m_r_chk1  <- FALSE;
m_r_chk2  <- FALSE;
norms     <- FALSE;
results   <- FALSE;
while(norms == FALSE | results == FALSE){
  norms     <- FALSE;
  results   <- FALSE;
  pH        <- round(rnorm(n = N, mean = 6, sd = 0.4), digits = 1);
  fire_freq <- 21;
  while(max(fire_freq) > 20){
    fire_freq <- sample(x = 0:20, size = N, replace = TRUE);
  }
  uuu       <- runif(n = N, min = 0, max = 100);
  depth     <- round((20 - fire_freq) + uuu, digits = 1);
  tempr     <- round(runif(n = N, min = 22, max = 27), digits = 1);
  rain      <- round(rnorm(n = N, mean = 225, sd = 15), digits = 1);
  PyC       <- rnorm(n = N, mean = -3);
  while(min(PyC) <= 0 | shapiro.test(PyC)$p.value < 0.2){
    AA        <- round(rnorm(n = N, mean = 1, sd = 0.4), digits = 2);
    CC        <- (fire_freq * 0.05) + rnorm(n = N, sd = 0.01);
    PyC       <- round(AA + CC, digits = 2);
  }
  SOC       <- rep(0, length = length(PyC));
  while(min(SOC) <= 0 | sum(SOC <= PyC) > 0){
      SOC       <- round((40 - 0.2*depth) * rexp(n = N, rate = 40), digits = 2);
      SOC       <- round(SOC + rnorm(n = N, mean = 1, sd = 0.5), digits = 2);
      SOC       <- SOC + PyC
  }
  log_SOC   <- log(SOC);
  if(shapiro.test(PyC)$p.value > 0.1 & shapiro.test(log_SOC)$p.value > 0.1){
      norms <- TRUE;
  }
  mod1 <- lm(PyC ~ depth);
  mod2 <- lm(PyC ~ fire_freq);
  mod3 <- lm(PyC ~ depth + fire_freq);
  mod4 <- lm(PyC ~ tempr + rain);
  mod5 <- lm(log_SOC ~ depth);
  mod6 <- lm(log_SOC ~ depth + tempr + rain);
  m1B1 <- summary(mod1)$coefficients[2, 1];
  m1p1 <- summary(mod1)$coefficients[2, 4];
  m2B1 <- summary(mod2)$coefficients[2, 1];
  m2p1 <- summary(mod2)$coefficients[2, 4];
  m3B1 <- summary(mod3)$coefficients[2, 1];
  m3p1 <- summary(mod3)$coefficients[2, 4];
  m3B2 <- summary(mod3)$coefficients[3, 1];
  m3p2 <- summary(mod3)$coefficients[3, 4];
  m4B1 <- summary(mod4)$coefficients[2, 1];
  m4p1 <- summary(mod4)$coefficients[2, 4];
  m5B1 <- summary(mod5)$coefficients[2, 1];
  m5p1 <- summary(mod5)$coefficients[2, 4];
  if(m1B1 < 0 & m1p1 < 0.04 & m2B1 > 0 & m2p1 < 0.04 & m3B1 < 0 & m3p1 > 0.06 &
     m3p2 < 0.06 & m5B1 < 0 & m5p1 < 0.04 & m4p1 < 0.04){
      results <- TRUE;
  }
}

fire_carbon <- data.frame(depth, fire_freq, tempr, rain, SOC, PyC, pH);




