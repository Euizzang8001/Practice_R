#Inverse Transform을 활용하여 정규 분포를 따르는 난수 추출하기기
sqrt1plus <- function(x, n){ #sqrt(1+x)의 함수값을 테일러 급수를 활용한 방식으로 계산하는 함수 선언 
  result <- 0 
  for(i in 0:n){
    if(i==0){
      result <- result + 1 * x^i
    }
    else{
      result <- result + choose(1/2, i)*x^i
    }
  }
  return(result)
}

solution1 <- sqrt1plus(-1/2, 100) # 1/sqrt(1/2)값 계산 

fixedpoint <- function(f, x0, tol = 1e-9, max.iter = 100){ #고정점을 찾는 함수 선언 
  xold <- x0; xnew <- f(xold); iter <- 1;
  while((abs(xnew - xold) > tol) & (iter < max.iter)){
    xold <- xnew; xnew <- f(xold)
    iter <- iter + 1
  }
  if(abs(xnew - xold)> tol){
    cat("Algoritm failed to converge\n"); return(NULL) #고정점을 못 찾았을 경우 NULL을 return 
  }
  else{
    cat("Algorithm converged\n")
    return(xnew)#고정점을 찾았으면 해당 고정점을 return 
  }
}

g1 <- function(x){x + cos(x) - solution1} #해당 함수는 고정점을 알맞게 찾음
result1 <- fixedpoint(g1, 1) * 4 #고정점을 알맞게 찾는 함수를 활용하여 파이값을 계산

cdfnorm <- function(x, n){ #최종적으로 정규 분포의 CDF를 반환하는 함수 선언 
  abs_x = abs(x)
  odd <- 1
  result <- 0
  for(i in 0:n){
    if(i==0){
      result <- result + abs_x
    }
    else if(i%%2 == 1){
      odd <- odd * i
    }
    else{
      result <- result + (-1)^(i/2) * odd * abs_x^(i+1) / (factorial(i) * (i+1))
    }
  }
  result <- result*solution1/sqrt(result1)
  if(x<0){return(0.5 - result)} #위에서 구한 범위는 0~양수의 범위이므로 음수가 인수로 설정되었을 때의 값은 정규분포의 대칭성을 활용하여 계산 
  else{return(0.5 + result)} # 위에서 구한 범위는 0~양수의 범위이므로 음의 무한대~양수에서의 값을 계산하기 위해 0.5를 더함 
}

v <- c(-2, -1, 0, 1, 2)

solution2 <- c()

#-2, -1, 0, 1, 2에서의 cdfnorm을 이용한 값 계산
for(i in v){
  solution2 <- c(solution2, cdfnorm(i, 100)) #2-a~d)에서 구한 함수 및 값들로 정규분포의 CDF를 계산 
}
solution2

#r에 내장되어 있는 함수인 pnorm을 이용하여 우리가 정의한 cdfnorm 함수의 결과와 비교 
pnorm(v)

pdfnorm <- function(x){ #문제에서 주어진 pdf를 코드로 표현 
  return(1/(2 * result1)^(1/2) * exp(-1/2 * x^2))  
}

inv.cdf <- function(u) { #뉴턴 랩슨 방법을 이용하여 cdfnorm(x) - u = 0을 성립하게 만드는 x를 찾는 함수인 inv.cdf 정의 
  iter <- 0; temp <- 1; x <- temp + 10^(-3);
  while ( abs(x- temp) > 10^(-4)) {
    iter <- iter + 1
    if (iter > 100)
      stop("Newton-Raphson algorithm does not converge")
    temp <- x; x <- x - (cdfnorm(x, 100) - u) / pdfnorm(x)
  }
  return(x)
}

nreps=10^4; #10000개의 unif(1) 난수 생성하여 u에 저장 
u <- replicate(nreps, runif(1))
vectorized_invcdf <- Vectorize(inv.cdf) #inv.cdf가 백터값을 인수로 받을 수 있도록 Vectorize함 
#[-5,5]범위에서의 inv.cdf의 결과를 히스토그램으로 출력
hist(vectorized_invcdf(u),freq=F,main="Standard Normal from Inverse Transform", breaks = 100, xlim = c(-5, 5), ylim = c(0, 0.5)) 

vectorized_pdfnorm <- Vectorize(pdfnorm) #pdfnorm이 백터값을 인수로 받을 수 있도록 Vectorize함 
#[-5,5]범위에서의 pdfnorm의 결과를 curve로 나타냄냄
curve(vectorized_pdfnorm, from = -5, to = 5, xlab = "x", ylab = "pdfnorm", add = TRUE, col = 'red', xlim = c(-5, 5), ylim = c(0, 0.5))


#exp(-(x-2)^3)을 몬테카를로 적분을 사용하여 [1. Inf)에서 적분하기 
midpoint <- function (f, a, b, n) { #midpoint 방식을 사용하여 적분값 구하는 함수 선언 
  h = (b-a) /n; x = seq(a, b-h, by=h) + h/2
  y = f(x); area = sum(y)*h
  return (area)
}
trapezoid <- function(f, a, b, n) { #trapezoid 방식을 사용하여 적분값 구하는 함수 선언 
  x = seq(a, b, length = n + 1); y = f(x)
  area = sum((y[2:(n+1)] + y[1:n]))
  return(area*(b-a)/(2*n))
}
simpson <- function(f, a, b, n) { #simpson 방식을 사용하여 적분값 구하는 함수 선언 
  x = seq(a, b, length = n+1); y = f(x); midindx = 2*(1:(n/2))
  area = sum(y[1:n]+y[2:(n+1)])+sum(2*y[midindx])
  return(area*(b-a)/(3*n))
}

f <- function(x){ #문제에서 주어진 함수 식을 r코드로 정의
  return(exp(-(x-2)^3))
}
#midpoint, trapezoid, simpson은 Inf의 범위를 가질 수 없으므로 임의로 범위를 [1,100]으로 설정하여 유사 적분값을 구함 
result1 <- midpoint(f, 1, 100, 10^4)
result2 <- trapezoid(f, 1, 100, 10^4)
result3 <- simpson(f, 1, 100, 10^4)
result1
result2
result3

#r에 내장되어 있는 integrate 함수를 이용하여 위 3개의 방식 결과의 값들과 비교 
integrate(f, lower = 1, upper = Inf)

#f의 inverse 함수를 구하여 선언 
inv.fx<- function(u){
  return(1 - log(1-u))
}
#unif(1)을 따르는 10000개의 난수를 생성하여 u에 저장 
u <- replicate(10^4, runif(1))
vectorized_solution <- Vectorize(inv.fx) #inv.fx가 벡터값을 가질 수 있도록 Vectorize진행 
result4 <- vectorized_solution(u) #inverse transform을 이용하여 theta값을 구함 

gx <- function(x){
  return(exp(-(x-2)^3 + x - 1))
}

vectorized_gx <- Vectorize(gx)
result5 <- sum(vectorized_gx(result4)) / 10^4
result5

#r에 내장되어 있는 integrate 함수를 이용하여 위 방법의 결과와 비교 
integrate(f, lower = 1, upper = Inf)


#범위에 따라 pdf 식이 다른 분포에서 Inverse transform을 사용하여 난수 추출하기 
#범위에 따른 fx의 역함수를 r코드로 선언 
inv.fx <- function(u){
  if(u < 1 / (2 + exp(-2))) {
    return(log(2 + exp(-2)) + log(u))
  }
  else if( u <= (2 - exp(-2)) / (2 + exp(-2))){
    return(-log(2 - (2 + exp(-2)) * u))
  }
  else{
    return(-2 * log(2 + exp(-2)) - 2 * log(1-u) + 2*log(2) - 2)
  }
}
#unif(1)을 따르는 난수를 생성하고, inv.fx(u)에 해당하는 x값을 return하는 함수 선언 
solution <- function() {
  u <- runif(1); x <- inv.fx(u)
  return(x)}
nreps=10^4; 
X <- replicate(nreps,solution()) #solution3b를 10000번 반복하여 나온 결과를 X에 저장 
hist(X,freq=F, breaks = 100, xlim = c(-10, 10), ylim = c(0, 0.5)) #inverse transform을 이용한 결과를 히스토그램으로 표현 

fx <- function(x){ #실제 함수를 r코드로 선언 
  if(x<0){
    return(exp(x) / (2 + exp(-2)))
  }  
  else if(x<=2){
    return(exp(-x)/(2+exp(-2)))
  }
  else{
    return(exp(-1) * exp(-x/2) / (2 + exp(-2)))
  }
}
vectorized_fx <- Vectorize(fx) #벡터값을 가질 수 있도록 Vectorize 실행 
#[-5,5] 범위에서 실제 fx함수를 curve로 나타내어 inverse transform 방식의 결과와 비교 
curve(vectorized_fx, from = -10, to = 10, xlab = "x", ylab = "f(x)", add = TRUE, col = 'red', xlim = c(-10, 10), ylim = c(0, 0.5))





#주사위 k개를 던졌을 때의 합을 나타내는 분포에서의 평균을 구하는 문제 
#그래프를 2개를 동시에 출력하기 위한 par함수 실행 
par(mfrow = c(1, 2))

cdf <- function(x){ #4-a)에서 구한 cdf를 r코드로 표현 
  if(x<5){
    return (0)
  }else if(x > (6*5)){
    return(1)
  }else{
    sum = 0;
    for(i in 0:5){
      if(x - 6*i >= 5){
        sum <- sum + (-1)^i * factorial(x-6*i)/(factorial(i) * factorial(5-i) * factorial(x-6*i-5)) 
      }
    }
    return(sum/(6^5))
  }
}

solution <- function(u){ # inverse transform을 이용하여 u값을 구함 Fx(i-1) < u <=Fx(i)라면 i를 F-1(u)값으로 return 
  for(i in (5):(6*5)){
    p0 <- cdf(i-1)
    p1 <- cdf(i)
    if(u >p0 && u<=p1){
      return(i)
    }
  }
}

nreps=10^4;
u <- replicate(nreps, runif(1)) #unif(1)을 따르는 10000개의 난수 생성하여 u에서 저장 
vectorized_solution <- Vectorize(solution) #벡터값을 인수로 받을 수 있도록 Vectorize함수 실행 
result <- vectorized_solution(u) #생성된 10000개의 난수를 inverse transform 방식을 사용한 solution4b 함수에 input하여 그 결과를 result4b를 저장 
freq_result <- table(result)/10^4 #result4b의 결과에서 상대도수를 구함 

barplot(freq_result, main = "Inverse Transform Method", ylim = c(0, 0.12)) #상대도수를 나타낸 freq_result를 barplot으로 나타냄 

pdf <- function(x){ #문제에서 주어진 pdf를 r코드로 표현 
  sum <- 0
  for(i in 0:5){
    if(x - 6*i>= 5){
      sum <- sum + (-1)^i * 5 *  factorial(x-6*i-1)/(factorial(i) * factorial(5-i) * factorial(x-6*i-5)) 
    }
  }
  return(sum/(6^5));
}

#pdf를 Vectorize하고, pdf의 범위에 따라 나온 결과를 barplot을 
x <- c(5:30) 
vectorized_pdf <- Vectorize(pdf) 
barplot(vectorized_pdf(x),  names.arg = x, xlab = "x", ylab = "PDF Value", ylim = c(0, 0.12))

#inverse transform 방식을 활용하여 평균을 구함 
result1 <- sum(result)/10^4
result1

#실제 pdf를 이용한 평균을 구하는 방식으로 평균을 구함 
result2 <- 0
for(i in 5:30){
  result2 <- result2 + i * pdf(i)
}
result2


