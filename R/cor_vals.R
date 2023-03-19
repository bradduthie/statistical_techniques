
the_cor <- -100;
while(the_cor > 0.01 | the_cor < -0.01){
  r1      <- 0;
  x1      <- rnorm(n = 100, mean = 0, sd = 1);
  x2      <- rnorm(n = 100, mean = 0, sd = 1);
  y1      <- r1 * x1 + (sqrt(1 - r1^2)) * x2;
  the_cor <- cor(x1, y1);
}

X1 <- x1;
Y1 <- y1;

the_cor <- -100;
while(the_cor < -0.251 | the_cor > -0.249){
  r1      <- -0.25;
  x1      <- rnorm(n = 100, mean = 0, sd = 1);
  x2      <- rnorm(n = 100, mean = 0, sd = 1);
  y1      <- r1 * x1 + (sqrt(1 - r1^2)) * x2;
  the_cor <- cor(x1, y1);
}

X2 <- x1;
Y2 <- y1;


the_cor <- -100;
while(the_cor > 0.51 | the_cor < 0.49){
  r1      <- 0.5;
  x1      <- rnorm(n = 100, mean = 0, sd = 1);
  x2      <- rnorm(n = 100, mean = 0, sd = 1);
  y1      <- r1 * x1 + (sqrt(1 - r1^2)) * x2;
  the_cor <- cor(x1, y1);
}

X3 <- x1;
Y3 <- y1;


X4 <- rnorm(n = 100, mean = 0, sd = 1);
Y4 <- X4;

cor_vals <- data.frame(X1, Y1, X2, Y2, X3, Y3, X4, Y4);
write.csv(cor_vals, "data/cor_vals.csv", row.names = FALSE);







