#linear regression model에서 error가 Logistic(0, 1)를 따를 때 MLE 구하기
set.seed(1); #고정된 랜덤 변수 값들 설정
n <- 100; x <-rnorm(n); y<- 2*x+rlogis(n) #문제 상황에 맞는 x와 y 랜덤으로 100개씩 생성
b_zero = sum(x*y)/sum(x^2) #초기값 설정

ll_prime <- function(b){ #log-likelihood function의 일계도함수 선언 
  result = sum(x) - 2 * (sum((x * exp(-y+b*x))/(1+exp(-y+b*x))))
  return(result)
}

secant <- function(f, b0, b1, tol = 1e-6, n=10^6){ #secant 방법을 이용하여 MLE를 구하는 함수 선언
  iter<-0;temp1<-b1;temp2<-b0;b<-temp1 + 10*tol;
  while(abs(b-temp1) > tol){
    iter <- iter + 1
    if (iter>n) stop("Secant method does not converge")
    teamp2 <- temp1; temp1 <- b;
    b <- temp1 - f(temp1)*(temp1 - temp2)/(f(temp1) - f(temp2))
  }
  return(c(b, iter)) #secant 방법을 통해 구한 MLE와 구할 때까지의 반복 횟수 return
}

sec_val <- secant(ll_prime, b_zero, b_zero + 1) #문제에서 주어진 초기값들을 가지고 secant방식을 사용하여 MLE 계산
print(sec_val) #sec_val 출력

bisection <- function(f, b0, b1, tol = 1e-6){ #bisection 방식을 이용하여 MLE를 계산하는 함수 선언
  iter <-0; f.l <- f(b0); f.r<-f(b1);
  if(f.l == 0){return(b0)}
  else if(f.r == 0){return(b1)}
  else if (f.l*f.r > 0 ){return("incorrect specification")}
  while((b1-b0)>tol){
    iter <- iter + 1
    b.m <-(b0+b1)/2; f.m <- f(b.m); if(f.m==0){return(b.m)}
    else if (f.l*f.m < 0) {b1 <- b.m; f.r <- f.m}
    else{b0 <- b.m; f.l<-f.m}
  }
  return(c((b0+b1)/2, iter)) #bisection 방법을 통해 구한 MLE와 구할 때까지의 반복 횟수 return
}

bis_val <- bisection(ll_prime, b_zero-1, b_zero + 1) #문제에서 주어진 초기값들을 가지고 bisection방식을 사용하여 MLE 계산 
print(bis_val) #bis_val 출력 

# 정규 분포의 CDF를 norm 함수를 사용하지 않고 계산하기
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

solution1 <- sqrt1plus(-1/2, 100) #2-a)의 함수를 이용하여 sqrt(1/2)계산 및 출력 
print(solution1)

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
g2 <- function(X){x - cos(x) + solution1} #해당 함수는 고정점을 찾지 못함(NULL 반환)
result1 <- fixedpoint(g1, 1) * 4 #고정점을 알맞게 찾는 함수를 활용하여 파이값을 계산 
print(result1) #구한 파이값을 출력 

cdfnorm <- function(x, n){ #2 - a~c)에서 구한 방식을 활용하여 최종적으로 정규 분포의 CDF를 반환하는 함수 선언 
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
for(i in v){
  solution2 <- c(solution2, cdfnorm(i, 100)) #2-a~d)에서 구한 함수 및 값들로 정규분포의 CDF를 계산 
}
solution2 #위에서 계산한 값들 출력
pnorm(v) #R의 내장 함수를 활용하여 계산한 방식과 비교하기 위해 출력 


#Cauchy(theta, 1)을 따르는 분포의 log-likelihood function 구하기 
l.prime <- function(theta){sum(2*(x-theta)/(1+(x-theta)^2))} #일계도함수를 선언 

num.deriv <- function(theta, epsilon = 1e-3, f){ #특정 함수가 들어오면 central difference 방식을 이용하여 이계도함수 값 근사 
  result1 <- f(theta + epsilon)
  result2 <- f(theta - epsilon)
  return((result1 - result2)/(2*epsilon))
}

MLE.est <- function(init.theta, tol = 1e-6){ #MLE를 구하는 함수 선언 
  temp <- init.theta; result_theta <- temp + 10 * tol;
  while(abs(result_theta-temp) > tol){ #오차가 허용 범위일 때까지 
    temp <- result_theta; result_theta<- result_theta-l.prime(result_theta)/num.deriv(result_theta, f = l.prime) #뉴턴 랩슨 방식을 활용하여 temp와 result_temp를 반복하여 저장 
  }
  #반복분이 종료되면 result_theta 변수에 MLE가 저장되어 있음 
  MLE.var <- -1/num.deriv(result_theta, f = l.prime) #오차가 허용 범위라면 MLE.var값을 구함 
  return(c(result_theta, result_theta + qnorm(0.025)*sqrt(MLE.var), result_theta + qnorm(0.975)*sqrt(MLE.var))) #구한 MLE값과 MLE.var값을 이용하여 95%의 신뢰구간을 구함 
  
}

n <- 100; x<- 5+rcauchy(n) #문제에서 요구한 조건을 충족하는 100개의 수를 랜덤으로 생성함 
result <- MLE.est(5) #위 값들을 이용하여 MLE 및 95%의 신뢰 구간을 구함 
print(result)#위에서 구한 MLE값과 신뢰 구간을 출력
check <- 0 #실제 theta의 값인 5가 신뢰구간에 몇 번 포함되었는지 확인하는 변수 check 선언 
for(i in 1:1000){ #랜덤 수를 만들고 그것들의 MLE와 95%의 신뢰구간을 구하는 것을 1000번 반복 
  n <- 100; x<- 5+rcauchy(n)
  result <- MLE.est(5)
  if((5 >= result[2]) & (5 <= result[3])){ #구한 신뢰구간 안에 실제 theta값인 5가 포함된다면 
    check <- check + 1 #check에 1을 더함 
  }
}

print(check/1000) #신뢰구간에 실제 theta값이 포함된 비율을 확인
