#Unošenje podataka
podaci <- c(5,8,2,1,8,2,3,5,1,3,1,7,3,3,4,3,4,4,5,2,10,1,5,1,6,14,3,8,5,6,2,5,1,12,6,2,3,2,1,6,7,2,2,4,2,1,1,2,16,2)

h = hist(podaci, probability = TRUE, main = "Histogram za podatke", xlab = "Intervali", ylab = "Relativne frekvencije")

n <- length(podaci)

summary(podaci)
boxplot(podaci, main = "Dijagram pravokutnika za podatke", ylab = "Intervali")

#Procjene metodom momenata
m_hat = mean(podaci)
lambda_hat = 1/mean(podaci)

#Eksponencijalna s parametrom dobivenog iz MLE
gustoca_exp <- function(x){
  dexp(x, rate = lambda_hat)
}

#Trokutasta s parametrom $m$
gustoca_tri <- function(x,m){
  if(x < 0 | x > 3*m){
    0
  }
  else{
    (2/(3*m)) * (1 - x/(3*m)) 
  }
}
  
#Trokutasta s parametrom dobivenim iz metode momenata
gustoca_tri_mom <- function(x){
  if(x < 0 | x > 3*m_hat){
    0
  }
  else{
    (2/(3*m_hat)) * (1 - x/(3*m_hat)) 
  }
}

#MLE za procjenu parametra
mle <- function(m, data = podaci){
  prod = 1
  l <- length(podaci)
  for(i in 1:l){
    prod = prod * gustoca_tri(data[i],m)
  }
  prod
}

m_hat_MLE <- optimize(mle, data = podaci, c(0,10), maximum = TRUE)$maximum

#Trokutasta s parametrom iz MLE
gustoca_tri_MLE <- function(x){
  if(x < 0 | x > 3*m_hat_MLE){
    0
  }
  else{
    (2/(3*m_hat_MLE)) * (1 - x/(3*m_hat_MLE)) 
  }
}

#Za crtanje gustoæa preko histograma
curve(gustoca_exp, from = 0, to = 15, add = TRUE, col = 'red', lwd = 3)
curve(gustoca_tri_mom, from = 0, to = 15, add = TRUE, col = 'red', lwd = 3)
curve(gustoca_tri_MLE, from = 0, to = 15, add = TRUE, col = 'red', lwd = 3)

#Raspodjela u nove razrede za \chi^2-testove 
granice <- c(0,1,2,3,5,7,16)
h_drugi = hist(podaci, probability = T, breaks = granice)

frekvencije <- h_drugi$counts

#Distribucija trokutaste s parametrom $m$
cdf_triangle <- function(x, m=1){
  if(x < 0){
    0
  }
  else if(x > 3*m){
    1
  }
  else{
    -(x*(x-6*m))/(9*m^2)
  }
}

#\chi^2-testna statistika za trokutastu razdiobu s parametrom $x$ i danim podacima
chi_triangle <- function(x){
  j <- length(frekvencije)
  ocekivane = rep(NA,j)
  for(i in 1:j){
    ocekivane[i] = 50*(cdf_triangle(granice[i+1],x)-cdf_triangle(granice[i],x))
  }
  ocekivane[j] = 50*(1-cdf_triangle(granice[j],x))
  #print(ocekivane) ##Trebalo nam je da provjerimo jesu li svi razredi >= 5
  sum((ocekivane-frekvencije)^2/(ocekivane))
}

#Pronalazak minimuma i provoðenje testa
g_tri <- Vectorize(chi_triangle)
curve(g_tri, from = 2.5, to = 5.5)

m_hat_chi_sq <- optimize(g_tri, lower = 2.5, upper = 4, maximum = FALSE)$minimum
m_hat_chi_sq

h_1 <- chi_triangle(m_hat_chi_sq)
h_1
pchisq(h_1, df = 4, lower.tail=FALSE)

#\chi^2-testna statistika za eksponencijalnu razdiobu s parametrom $x$ i danim podacima
chi_exp <- function(x){
  j <- length(frekvencije)
  ocekivane = rep(NA,j)
  for(i in 1:j){
    ocekivane[i] = 50*(pexp(granice[i+1],rate = x)-pexp(granice[i],rate = x))
  }
  ocekivane[j] = 50*(1-pexp(granice[j],x))
  #print(ocekivane) ##Trebalo nam je da provjerimo jesu li svi razredi >= 5
  sum((ocekivane-frekvencije)^2/(ocekivane))
}

#Pronalazak minimuma i provoðenje testova
g_exp <- Vectorize(chi_exp)
curve(g_exp, from = 0.05, to = 0.4)

lambda_hat_chi_sq <- optimize(g_exp, lower = 0.1, upper = 0.3, maximum = FALSE)$minimum
lambda_hat_chi_sq

h_2 <- chi_exp(lambda_hat_chi_sq)
h_2
pchisq(h_2, df = 4, lower.tail=FALSE)

## 95%-CI u sluèaju da koristimo asimptotsku normalnost
z = -qnorm(0.025)

lower = (mean(podaci)*sqrt(n)) / (sqrt(n) - z)
upper = (mean(podaci)*sqrt(n)) / (sqrt(n) + z)
lower
upper
mean(podaci)

## 95%-CI opisan kao u seminaru
alpha_1 = qgamma(0.025, shape = 50, rate = 1)
alpha_2 = qgamma(1-0.025, shape=50, rate = 1)
t = sum(podaci)
t/alpha_1
t/alpha_2

#t-test za oèekivanje
t.test(podaci, alternative = "two.sided", mu = 4)
