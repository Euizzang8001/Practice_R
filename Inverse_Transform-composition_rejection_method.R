#pdf가 exp((x-exp(x))/2)/sqrt(2*pi)인 분포에서 rejection method와 composition 방법을 사용하여 난수 추출하기 
#accept-rejection 알고리즘을 사용하기 위한 가능한 상수 M의 최댓값을 구하는 함수 선언 
find.m <- function(fx, gx){
  M<-optimize(f = function(x){fx(x)/gx(x)}, maximum=T, interval=c(-100, 100))$objective
  return(M)
}

fx <- function(x){ #fx함수 선언 
  return(exp(1/2*(x-exp(x)))/sqrt(2*pi))
}

g1x <- function(x){ #문제에서 주어진 cauchy(0, 1)의 pdf 선언 
  return(1/(pi*(1+x^2)))
}

M <- find.m(fx, g1x) #g1x를 이용해 fx의 분포를 알기 위한 rejection method를 활용하기 위한 M의 값 찾음
M

#앞에서 구한 M을 활용하여 rejection method를 사용하는 함수 선언 
solution1 <- function(){
  Nsim = 10^4; #생성할 난수의 수를 10000개로 설정 
  v=runif(Nsim)#Nsim의 값만큼 unif(0, 1)에서 난수 추출
  y=tan(pi*(runif(Nsim)-0.5))#inverse function을 이용하여 g1x를 따르는 난수 y생성
  x=y[v < fx(y)/(g1x(y)*M)]; #rejection method 수행 
  return(x) #accpet 된 y의 값만 x에 저장하여 return 
}

#rejection method를 활용하여 구한 x의 분포와 실제 분포 비교하는 히스토그램과 curve 그래프 그리기 
hist(solution1(), nclass=30,freq=F,border="blue4", col="gray", xlim = c(-15, 5), ylim = c(0, 0.3))
curve(fx, add=TRUE, col="blue4",lwd=2)

solution2 <- function(){ #normal 분포의 mixture로 나타난 pdf를 활용하여 method of composition 방식을 사용하는 함수 선언 
  #문제에 제시된 정보를 각각 저장 
  mean.table = c(-11.40, -5.24, 1.51, -0.65, 0.53, -2.36) 
  var.table = c(5.80, 2.61, 0.17, 0.64, 0.34, 1.26)
  cum.prob = c(0.007, 0.112, 0.156, 0.496, 0.742, 1)#j가 1부터 6까지였을 때의 누적 확률 값임 -> method of composition에 활용 
  
  #box-muiller 방식으로 독립인 2개의 normal분포로부터 난수를 생성
  nsim <- 10^4 * 2; z <- matrix(0,nsim/2,2) #z[,1]과 z[,2]를 합하여 총 10000개의 난수를 생성 
  
  for (i in 1:(nsim/2)){
    #box muller trnasform을 사용하여 N(0, 1)의 값을 따르는 난수를 각각 z[i,1]과 z[i,2]에 저장 
    u <- runif(2)
    z[i,1] <- sqrt(-2*log(u[1]))*cos(2*pi*u[2])
    z[i,2] <- sqrt(-2*log(u[1]))*sin(2*pi*u[2])
    
    #method of composition을 활용하는 부분 
    prob1 <- runif(1)
    prob2 <- runif(1)
    
    #prob1과 prob2가 각각 누적 확률에 어떤 값보다 작은지를 비교하며 method of composition 수행
    #prob1 비교 
    if(prob1<cum.prob[1]){
      z[i, 1] <- z[i, 1] * var.table[1]^(1/2) + mean.table[1]
    }else if(prob1 < cum.prob[2]){
      z[i, 1] <- z[i, 1] * var.table[2]^(1/2) + mean.table[2]
    }else if(prob1 < cum.prob[3]){
      z[i, 1] <- z[i, 1] * var.table[3]^(1/2) + mean.table[3]
    }else if(prob1 < cum.prob[4]){
      z[i, 1] <- z[i, 1] * var.table[4]^(1/2) + mean.table[4]
    }else if(prob1 < cum.prob[5]){
      z[i, 1] <- z[i, 1] * var.table[5]^(1/2) + mean.table[5]
    }else{
      z[i, 1] <- z[i, 1] * var.table[6]^(1/2) + mean.table[6]
    }
    #prob2 비교 
    if(prob1<cum.prob[1]){
      z[i, 2] <- z[i, 2] * var.table[1]^(1/2) + mean.table[1]
    }else if(prob1 < cum.prob[2]){
      z[i, 2] <- z[i, 2] * var.table[2]^(1/2) + mean.table[2]
    }else if(prob1 < cum.prob[3]){
      z[i, 2] <- z[i, 2] * var.table[3]^(1/2) + mean.table[3]
    }else if(prob1 < cum.prob[4]){
      z[i, 2] <- z[i, 2] * var.table[4]^(1/2) + mean.table[4]
    }else if(prob1 < cum.prob[5]){
      z[i, 2] <- z[i, 2] * var.table[5]^(1/2) + mean.table[5]
    }else{
      z[i, 2] <- z[i, 2] * var.table[6]^(1/2) + mean.table[6]
    }
  }
  
  result <- c(z[,1], z[,2]) #z[,1]과 z[,2]에 저장된 모든 생성된 난수를 result에 합치고 result를 return 
  
  return(result)
}

#method of composition을 이용한 결과와 실제 fx의 분포를 히스토그램과 curve 그래프를 그려 비교 
hist(solution2(), nclass = 50,freq=F,border="blue4", col="gray", xlim = c(-15, 5), ylim = c(0, 0.3))
curve(fx, add=TRUE, col="blue4",lwd=2)

#efficiency 비교 진행 
library(microbenchmark)
microbenchmark(solution1(), solution2())




#pdf가 x(x^2+1)^(-3/2)인 x와 1/(pi*(1-y^2)^(1/2))인 y를 사용하여 T = XY에서의 T를 따르는 난수 추출하기
n <- 10^4 #생성 난수 개수 설정  
inv1 <- function(n){ #반복 수 n만큼 inverse transform을 이용하여 fx에서 난수 생성하여 return 
  u <- runif(n);
  x <- ((1-u)^(-2) - 1)^(1/2)
  return(x)
}

pdf1 <- function(x){ #fx의 pdf를 선언 
  return(x*(x^2+1)^(-3/2))  
}
x <- inv1(n) #x에 inverse transform으로 생성한 난수를 저장 
clear_x <- x[which(x <= 15)]#x에 저장된 값 중 15와 같거나 작은 값들만 골라 clear_x에 저장
#inverse transeform으로 생성한 난수에 의한 분포와 실제 fx의 분포가 같은지 히스토그램과 curve 그래프로 비교 
hist(clear_x, freq=FALSE, xlim=c(0, 15), ylim=c(0, 0.5), xlab="X", ylab="Probability", breaks=30)
curve(pdf1, from=0, to=15, add=TRUE, col='red')

#Y = W/sqrt(1+W^2)인 점을 사용하여 Y에서의 난수 추출하기 
n <- 1e4 #생성 난수 개수 설정 
w=tan(pi*(runif(n)-0.5)) #cauchy 분포의 inverse cdf로 난수 추출 
y <- w/(1+w^2)^(1/2) #Y|W의 pdf와 위에서 생성된 난수 w를 이용하여 Y를 따르는 난수들을 y에 저장 

pdf2 <- function(y){ #y의 pdf 선언 
  return(1/(pi * (1-y^2)^(1/2)))
}
#w의 값을 이용하여 생성한 Y의 분포와 실제 Y의 분포를 히스토그램과 curve그래프를 그려 비교 
hist(y, freq=FALSE, xlab="y", ylab="Probability")
curve(pdf2, from=-1, to=1, add=TRUE, col='red')

t <- x*y #a)Ti = Xi * Yi이므로 위에서 생성한 각각의 난수가 저장된 x와 y를 곱하여 t에 저장 
ks.test(t, "pcauchy", location = 0, scale = 1) #ks.test를 통해 우리가 생성한 난수들이 cauchy(0,1)을 따르는지 검증 


#(0, 1)에서 sin(2*pi*x)^2/(x+1) * exp(cos(2*pi*x) - x)의 적분 값을 몬테카를로 적분을 활용한 2개의 방식으로 구하기

#1
#몬테카를로 적분을 이용하여 k1의 값을 구하는 함수 선언 
#Unif 분포를 따르는 x에서 integrand의 평균을 구하는 방식 
mc_int1 <- function(n) {
  u <- runif(n) 
  integrand <- function(x){(sin(2*pi*x))^2/(x+1)}
  integral_estimate <- mean(integrand(u))
  return(integral_estimate)
}

#실제 h1의 pdf를 선언 
pdf1 <- function(x){
  return((sin(2*pi*x))^2/(x+1))
}

#몬테 카를로 방식으로 구한 k1과 실제 적분을 이용한 방식으로 구한 k1을 비교 
result1 <- mc_int1(10^4)
result1
result2 <- integrate(pdf1, lower = 0, upper = 1)
result2

k1 <- result1 #앞에서 구한 값을 k1에 저장 
fbx <- function(x){(sin(2*pi*x))^2/((x+1)*k1)} #fbx선언

#rejection method를 이용하여 fx를 따르는 난수 생성하는 함수 선언 
solution1 <- function(fx){
  Nsim = 10^4; #생성할 난수의 수를 10000개로 설정
  v <- runif(Nsim) #Nsim의 값만큼 unif(0, 1)에서 난수 추출하여 v에 저장 
  u <- runif(Nsim) #Nsim의 값만큼 unif(0, 1)에서 난수 추출하여 u에 저장
  x=u[fx(u) > v]; #rejection method 수행 
  return(x) #accpet 된 y의 값만 x에 저장하여 return 
}

#solution1의 rejection method를 활용하여 얻은 난수를 이용해 몬테 카를로 적분 수행 
mc_int2 <- function(x) {
  integrand <- function(x){k1 * (exp(cos(2*pi*x) - x))}
  integral_estimate <- mean(integrand(x))
  return(integral_estimate)
}

#rejection method와 몬테 카를로 방식으로 theta값 계산하기 
result3 <- solution1(fbx)
result4 <- mc_int2(result3)
result4

#실제 적분을 이용하여 theta값 계산하기 
pdf.theta <- function(x){ #theta 계산을 위해 피적분 함수 선언 
  return((sin(2*pi*x))^2/(x+1) * exp(cos(2*pi*x) - x))
}
#적분 실행 
result5 <- integrate(pdf.theta, lower = 0, upper = 1)
result5


#2
#몬테 카를로 적분을 이용하여 k2의 값 구하기
#Exp(1)을 따르는 분포의 h2(x)의 평군을 구하는 방식 
mc_int3 <- function(n) {
  u <- rexp(n)
  integrand <- function(x){exp(cos(2*pi*x))}
  integral_estimate <- mean(integrand(u))
  return(integral_estimate)
}

#실제 h2(x)의 pdf를 적분하기 위해 h2(x)의 pdf 선언 
pdf3 <- function(x){
  return(exp(cos(2*pi*x) - x))
}
#몬테 카를로 적분을 활용하여 구한 k2와 실제 적분을 활용한 k2의 값 비교 
result6 <- mc_int3(10^4)
result6
result7 <- integrate(pdf3, lower = 0, upper = Inf)
result7

k2 <- result6 #앞에서 구한 값을 k2에 저장 
fdx <- function(x){exp(cos(2*pi*x) - x)/k2} #f2x의 pdf를 fdx의 이름으로 선언 
gdx <- function(x){exp(-x)}#candidate pdf를 gdx로 선언

#최적의 M의 값을 구하기 위한 find.m 함수 선언 
find.m <- function(fx, gx){
  M<-optimize(f = function(x){fx(x)/gx(x)}, maximum=T, interval=c(0, 100))$objective
  return(M)
}
M <- find.m(fdx, gdx) #gdx를 이용해 f2x의 분포를 알기 위한 rejection method를 활용하기 위한 M의 값 찾음

solution2 <- function() {
  Nsim = 10^4; #생성할 난수의 수를 10000개로 설정 
  v = runif(Nsim)#Nsim의 값만큼 unif(0, 1)에서 난수 추출
  y = -log(1 - runif(Nsim))#inverse function을 이용하여 gdx를 따르는 난수 y생성
  x = y[v < fdx(y)/(gdx(y)*M)]; #rejection method 수행
  return(x)
}

mc_int4 <- function(x) { #solution2을 실행하여 구한 난수를 이용해 몬테 카를로 적분 수행 
  integrand <- function(x) {k2 * (sin(2*pi*x))^2 / (x + 1) }
  integral_estimate <- mean(integrand(x))
  return(integral_estimate)
}

#rejection method로 난수 생성하기 
result8 <- solution2()
#rejection method로 구한 난수가 fdx의 분포를 잘 따르는지 히스토그램과 curve 그래프로 확인 
hist(result8, nclass = 1000, freq=F,border="blue4", col="gray", xlim = c(0, 1), ylim = c(0, 10))
curve(fdx, col="blue4",lwd=2, add = TRUE)

#theta값을 구하기 위해 적분 범위 밖에 있는 난수를 0의 값으로 처리 
result8[result8>1] <- 0

#rejection method와 몬테 카를로 적분을 활용하여 구한 theta값과 실제 적분을 활용한 theta값 비교 
result9 <- mc_int4(result8)
result9

result10 <- integrate(pdf.theta, lower = 0, upper = 1)
result10

#앞에서 선언한 몬테 카를로 적분 값을 얻는 함수를 실제 sample들을 return하도록 수정 
mc_int5 <- function(x) {
  integrand <- function(x){k1 * (exp(cos(2*pi*x) - x))}
  return(integrand(x))
}

#앞에서 선언한 몬테 카를로 적분 값을 얻는 함수를 실제 sample들을 return하도록 수정 
mc_int6 <- function(x) {
  integrand <- function(x) { k2 * (sin(2*pi*x))^2 / (x + 1) }
  return(integrand(x))
}
#두 sample들의 분산을 구하여 비교하기 
print(c(var(mc_int5(result3)), var(mc_int6(result8))))





#mixture 방식을 사용하여 2가지 방법으로 double exponential distribution을 따르는 분포에서 난수를 추출하기 
solution <- function(){
  Nsim <- 10^4 #생성하려는 난수의 개수 저장 
  y <- -2 * log(1-runif(Nsim)) #method of composition 방식을 수행하기 위해 먼저 Exp(1/2)를 따르는 Y에서 난수를 얻음 
  
  #box-muiller 방식으로 독립인 2개의 normal분포로부터 난수를 생성
  z <- matrix(0,Nsim/2,2) #z[,1]과 z[,2]를 합하여 총 10000개의 난수를 생성 
  
  for (i in 1:(Nsim/2)){
    #box muller trnasform을 사용하여 N(0, 1)의 값을 따르는 난수를 각각 z[i,1]과 z[i,2]에 저장 
    u <- runif(2)
    z[i,1] <- sqrt(-2*log(u[1]))*cos(2*pi*u[2])
    z[i,2] <- sqrt(-2*log(u[1]))*sin(2*pi*u[2])
    #우리가 원하는 T1의 난수는 y값을 sd로 가지므로, 그거에 따라 난수 수정 
    z[i, 1] <- z[i, 1] * y[2*i-1]^(1/2)
    z[i, 2] <- z[i, 2] * y[2*i]^(1/2)
  }
  result <- c(z[,1], z[, 2])
  return(result)
}
#생성된 난수들을 합쳐 최종적인 sample 저장 
result <- solution()
#실제 fx의 pdf를 fx에 저장 
fx <- function(x){
  exp(-abs(x))/2
}
#method of composition을 이용한 분포와 X의 분포가 일치하는지 히스토그램과 curve 그래프를 그려 비교 
hist(result, nclass = 50,freq=F,border="blue4", col="gray",main="hist of double exponential distribution", xlim = c(-10, 10), ylim = c(0, 0.5))
curve(fx, add=TRUE, col="blue4",lwd=2)

Nsim <- 10^4 #난수 생성 개수 선언 

solution2 <- function() { #Gamma(2, 1)을 따르는 난수를 생성하기 위해 rejection method를 사용 
  Nsim <- 10^4
  fdx <- function(x){x*exp(-x)} #Gamma(2, 1)의 pdf 선언 
  gdx <- function(x) (1/2)*exp(-x/2) #candidate function인 Exp(1/2)의 pdf 선언 
  M <- 2 #풀이 사진 참조 
  v = runif(Nsim)#Nsim의 값만큼 unif(0, 1)에서 난수 추출
  y = -2 * log(1-runif(Nsim))#inverse function을 이용하여 gdx를 따르는 난수 y생성
  x = y[v < fdx(y)/(gdx(y)*M)]; #rejection method 수행
  return(x)
}
#rejection method로 생성한 난수들을 활용하여 T분포를 따르는 난수를 생성하여 result4d에 저장 
result2 <- solution2()

#T2|U는 unif(-u, u)를 따른다는 점을 활용 
result3 <- runif(Nsim, min = -result2, max = result2)

fx <- function(x){ #실제 fx의 pdf를 선언 
  exp(-abs(x))/2
}
#문제의 조건에 따라 생성한 난수의 분포와 실제 X의 분포가 동일한지 히스토그램과 curve 그래프를 그려 비교 
hist(result3, nclass = 50,freq=F,border="blue4", col="gray", xlim = c(-10, 10), ylim = c(0, 0.6))
curve(fx, add=TRUE, col="blue4",lwd=2)

#4-b와 4-d의 efficiency 비교 진행 
library(microbenchmark)
microbenchmark(solution(), solution2())






#이항 분포에서 횟수가 커지면 정규 분포와 유사하다는 것을 확인하기 
#inverse transform 활용 시 discreate distribution이므로 생성된 난수와 값을 비교하며 F(x) = u가 성립하는 x의 값을 찾는 함수 선언 
cdf.binom <- function(x,n,p){
  Fx <- 0
  for (i in 0:x) {
    Fx <- Fx + choose(n,i)*p^i*(1-p)^(n-i)}
  return(Fx)}
#inverse transform을 이용하여 binom(n, p)에서 난수를 구하는 sim.binim0함수 선언 
sim.binom0 <- function(n,p){
  X <- 0; U <- runif(1)
  while(cdf.binom(X,n,p) < U) {
    X <- X+1 }
  return(X)}
#문제에서 주어진 m과 p의 값 설정 
m = 1000; p = 0.4
Nsim = 10^4 #생성 난수 횟수 선언 
#생성 난수 횟수만큼 반복하여 난수 생성하기 
result <- replicate(Nsim, sim.binom0(m,p))

#정규 분포 N(mp, mp(1-p))의 pdf를 result로 선언하고, binom의 inverse transform을 이용한 sample들의 분포와 이 normal분포를 히스토그램과 curve그래프를 그려 비교하기 
hist(result, nclass = 50,freq=F,border="blue4", col="gray",main="hist of normal distribution from binomial ", xlim = c(300, 500), ylim = c(0, 0.03))

result5a2 <- function(x){exp(-(x-m*p)^2/(2*m*p*(1-p)))/sqrt(2*pi*m*p*(1-p))}
curve(result5a2, add=TRUE, col="red4",lwd=2)
