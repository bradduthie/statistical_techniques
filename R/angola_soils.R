library("car");
dat <- read.csv("data/Angola_soils.csv");

crit <- FALSE;
mnN  <- mean(dat$Nitrogen, na.rm = TRUE);
mnP  <- mean(dat$Phosphorus, na.rm = TRUE);
mnK  <- mean(dat$Potassium, na.rm = TRUE);
sdN  <- 40;
sdP  <- 50;
N    <- dim(dat)[1];
chks <- rep(x = 0, times = 8);
while(crit == FALSE){
  Site       <- as.factor(dat$site);
  Profile    <- as.factor(dat$profile);
  Nitrogen   <- round(rnorm(n = N, mean = mnN, sd = sdN), digits = 2);
  Phosphorus <- round(rnorm(n = N, mean = mnP, sd = sdP), digits = 2);
  Potassium  <- round(rexp(n = N, rate = 1/mnK), digits = 2);
  check1     <- FALSE;
  check2     <- FALSE;
  check3     <- FALSE;
  check4     <- FALSE;
  check5     <- FALSE;
  check6     <- FALSE;
  check7     <- FALSE;
  check8     <- FALSE;
  Funda      <- which(Site == "Funda");
  Bailundo   <- which(Site == "Bailundo");
  lower_p    <- which(Profile == "lower");
  middle_p   <- which(Profile == "middle");
  upper_p    <- which(Profile == "upper");
  both_vr    <- which(Site == "Funda" & Profile == "upper_p");
  Nitrogen[Funda]     <- Nitrogen[Funda] + 48;
  Nitrogen[upper_p]   <- Nitrogen[upper_p] - 54;
  Nitrogen[both_vr]   <- Nitrogen[both_vr] + 70;
  Phosphorus[Funda]   <- Phosphorus[Funda] - 25;
  Phosphorus[lower_p] <- Phosphorus[lower_p] - 32;
  Phosphorus[14] <- NA;
  Phosphorus[30] <- NA;
  # Nitrogen properties
  eq_sit_VrN <- leveneTest(Nitrogen ~ Site)[["Pr(>F)"]][1];
  eq_pro_VrN <- leveneTest(Nitrogen ~ Profile)[["Pr(>F)"]][1];
  N_norm     <- shapiro.test(Nitrogen)$p.value;
  N_norm_F   <- shapiro.test(Nitrogen[Funda])$p.value;
  N_norm_B   <- shapiro.test(Nitrogen[Bailundo])$p.value;
  N_norm_l   <- shapiro.test(Nitrogen[lower_p])$p.value;
  N_norm_m   <- shapiro.test(Nitrogen[middle_p])$p.value;
  N_norm_u   <- shapiro.test(Nitrogen[upper_p])$p.value;
  if(N_norm   > 0.2 & N_norm_F > 0.2 & N_norm_B > 0.2 &
     N_norm_l > 0.2 & N_norm_m > 0.2 & N_norm_u > 0.2 &
     eq_sit_VrN > 0.2 & eq_pro_VrN > 0.2){
    check1 <- TRUE;
  }else{
    check1 <- FALSE;
  }
  # Phosphorus properties
  eq_sit_VrP <- leveneTest(Phosphorus ~ Site)[["Pr(>F)"]][1];
  eq_pro_VrP <- leveneTest(Phosphorus ~ Profile)[["Pr(>F)"]][1];
  P_norm     <- shapiro.test(Phosphorus)$p.value;
  P_norm_F   <- shapiro.test(Phosphorus[Funda])$p.value;
  P_norm_B   <- shapiro.test(Phosphorus[Bailundo])$p.value;
  P_norm_l   <- shapiro.test(Phosphorus[lower_p])$p.value;
  P_norm_m   <- shapiro.test(Phosphorus[middle_p])$p.value;
  P_norm_u   <- shapiro.test(Phosphorus[upper_p])$p.value;
  if(P_norm   > 0.2 & P_norm_F > 0.2 & P_norm_B > 0.2 &
     P_norm_l > 0.2 & P_norm_m > 0.2 & P_norm_u > 0.2 &
     eq_sit_VrP > 0.2 & eq_pro_VrP > 0.2){
    check2 <- TRUE;
  }else{
    check2 <- FALSE;
  }
  # Exercise 1
  site_test  <- t.test(Nitrogen ~ Site, var.equal = TRUE)$p.value;
  site_aov   <- anova(lm(Nitrogen ~ Site))[["Pr(>F)"]][1];
  if(site_test > 0.005 & site_test < 0.045){
    check3 <- TRUE;
  }else{
    check3 <- FALSE;
  }
  # Exercise 2
  profile_aov <- anova(lm(Nitrogen ~ Profile));
  profile_pvl <- profile_aov[["Pr(>F)"]][1];
  if(profile_pvl > 0.005 & profile_pvl < 0.045){
    check4 <- TRUE;
  }else{
    check4 <- FALSE;
  }
  # Exercise 3
  profile_hsd <- aov(Nitrogen ~ Profile);
  profile_tuk <- as.numeric(TukeyHSD(profile_hsd)$Profile[,4]);
  low_mid     <- t.test(Nitrogen[lower_p], Nitrogen[middle_p],
                        var.equal = TRUE)$p.value;
  low_upp     <- t.test(Nitrogen[lower_p], Nitrogen[upper_p],
                        var.equal = TRUE)$p.value;
  mid_upp     <- t.test(Nitrogen[middle_p], Nitrogen[upper_p],
                        var.equal = TRUE)$p.value;
  HSD_sigs    <- sum(profile_tuk < 0.045);
  Bonf_sigs   <- sum(c(low_mid, low_upp, mid_upp) < (0.05/3));
  if(HSD_sigs > 0 & Bonf_sigs == 0){
    check5 <- TRUE;
  }else{
    check5 <- FALSE;
  }
  # Exercise 4
  profile_aov_P <- anova(lm(Potassium ~ Profile))[["Pr(>F)"]][1];
  if(profile_aov_P > 0.1){
    check6 <- TRUE;
  }else{
    check6 <- FALSE;
  }
  # Exercise 5
  two_way_N <- anova(lm(Nitrogen ~ Site * Profile))[["Pr(>F)"]][1:3];
  two_way_p <- sum(two_way_N < 0.045);
  if(two_way_p == 3){
    check7 <- TRUE;
  }else{
    check7 <- FALSE;
  }
  # Exercise 6
  two_way_P <- anova(lm(Phosphorus ~ Site * Profile))[["Pr(>F)"]][1:3];
  if(two_way_P[1] < 0.045 & two_way_P[1] > 0.001 &
     two_way_P[2] < 0.045 & two_way_P[2] > 0.001 &
     two_way_P[3] > 0.07){
    check8 <- TRUE;
  }else{
    check8 <- FALSE;
  }
  if(check1 == TRUE & check2 == TRUE & check3 == TRUE & check4 == TRUE &
     check5 == TRUE & check6 == TRUE & check7 == TRUE & check8 == TRUE){
    crit <- TRUE;
  }
  chks[1] <- chks[1] + check1;
  chks[2] <- chks[2] + check2;
  chks[3] <- chks[3] + check3;
  chks[4] <- chks[4] + check4;
  chks[5] <- chks[5] + check5;
  chks[6] <- chks[6] + check6;
  chks[7] <- chks[7] + check7;
  chks[8] <- chks[8] + check8;
  print(chks);
}


Angola_soils <- data.frame(Site, Profile, Nitrogen, Phosphorus, Potassium);
write.csv(Angola_soils, "data/Angola_soils.csv", row.names = FALSE);







