#pdf가 2/(pi*(1+x^2))인 분포에서 Inverse Transform 방식을 활용하여 난수 추출하기 
start_time1 <- Sys.time() #1 - d를 위한 시간 계산 함수
inv_fx <- function(x){ #fx의 역함수를 정의
  return(tan(pi/2 * x))
}
u <- replicate(10^4, runif(1)) #unif(0, 1) 분포를 따르는 10000개의 난수 생성

vectorized_invfx <- Vectorize(inv_fx) #inv_fx를 Vectorize
result1 <- vectorized_invfx(u) #unif(0, 1)로 생성된 u 벡터를 fx의 역함수에 대입한 함수값(x) 계산(inverse_transform)

end_time1 <- Sys.time() #1 - d를 위한 시간 계산 함수 실행

hist(result1, freq = F, xlim = c(0, 100), breaks = 10000, ylim=c(0, 1)) # inverse_transform을 통한 x의 pdf를 히스토그램으로 그림림

fx <- function(x){ #fx의 pdf 자체를 정의
  return(2/(pi*(1+x^2)))
}
vectorized_fx <-Vectorize(fx) #vectorize를 하여 벡터 값을 인수로 받을 수 있게 함
#fx의 pdf 자체를 이용하여 fx의 그래프를 구하기 
curve(vectorized_fx, from = 0, to = 100, xlab = "x", add = T, ylab = "f(x)", col = 'red', xlim = c(0, 40), ylim = c(0, 1))

start_time2 <- Sys.time() #1 - d)를 위한 시간 계산 함수 실행 
library(invgamma) #invgamma 패키지를 불러옴 

y.sim <- function(){ #y의 값을 랜덤으로 생성하는 함수 정의(method of composition)
  l <- rinvgamma(1, 1/2, 1) #z|Λ=λ계산을 위해 λ를 랜덤으로 뽑음 
  z.l <- rinvgamma(1, 1/2 ,1/l) #계산된 λ를 이용하여 z|Λ=λ의 z값을 뽑음 
  result <- sqrt(z.l) #Z = Y^2인 점을 이용해 y값을 계산하여 return
  return(result) 
}

y.rvs <- replicate(10^4, y.sim()) #y값을 랜덤으로 10000개 뽑음 

end_time2 <- Sys.time() #1 - d를 위한 시간 계산 함수 실행

#method of composition 방법을 사용한 fx의 pdf를 그래프로 그림 
hist(y.rvs, breaks = 10000, freq = FALSE, xlim = c(0, 40), ylim = c(0, 1)) 

print(end_time1 - start_time1) #1-a)의 방법의 계산 시간 출력
print(end_time2 - start_time2) #1-c)의 방법의 계산 시간 출력




#범위에 따라 CDF 식이 다른 분포에서 Method of composition 방식을 사용해 난수 추출하기
u1 <- replicate(10^4, runif(1)) #unif(0, 1)을 따르는 난수를 10000개 뽑아 u1에 저장  
u2 <- replicate(10^4, runif(1, min = 0, max = 2 + exp(-2))) #unif(0, 2 + exp(-2))를 따르는 난수를 10000개 뽑아 u2에 저장 

inv.fx <- function(u1, u2){#역함수를 나타내는 inv.fx를 선언 
  if(u2 <= 1){ #p1에 해당하면, 2-a)의 F1의 역함수를 이용하여 x 계산 
    return(log(u1))
  }
  else if(u2 <=  2 - exp(-2)){ #p2에 해당하면, 2-a)의 F2의 역함수를 이용하여 x 계산 
    return(-log(1 - (1-exp(-2))*u1))
  }
  else{ #p3에 해당하면, 2-a)의 F3의 역함수를 이용하여 x 계산 
    return(2 *(1 - log(1-u1)))
  }
}

vectorized_inv.fx <- Vectorize(inv.fx) #inv.fx가 벡터 값을 가지도록 vectorize 함
#method of composition을 이용한 방법으로 fx의 pdf를 히스토그램으로 나타냄 
hist(vectorized_inv.fx(u1, u2), freq=F, breaks = 100, xlim = c(-10, 10), ylim = c(0,1))






#pdf가 x*exp(-2x-1/x)/c인 분포 X(x>0)에서 c의 값을 구하고 rejection method를 활용하여 X의 평균을 구하기 
N <- 10000000 #몬테 카를로 적분을 위한 횟수 N설정 
u <- runif(N) #N만큼의 unif(0, 1)로부터 난수 생성 
x <- -log(1-u)/2 # 2 * exp(-2*x)를 pdf로 가지는 분포를 X라 설정하고, X * exp(1/X)/2 분포의 평균을 구하는 방식으로 계산 -> X의 cdf인 1-exp(-2*x) = u의 관계와 u의 값을 이용하여 x를 구함 
mc_estimate <- mean(x * exp(-1/x)/2) #몬테 카를로 적분을 평균 값으로 최종 계산을 진행 
mc_estimate

cx <- function(x){ #c의 값을 구하기 위해 적분 기호 안 함수를 선언 
  return(x * exp(-2*x-1/x))
}
result <- integrate(cx, 0, Inf) #cx를 0부터 inf까지 적분한 실제 값을 계산 
result

u1 <- runif(10^4, max = 10) #x의 범위는 무한대이지만, 10까지의 범위에서 더 정확히 구하기 위해 unif(0, 10)에서 난수 뽑음 

fx <- function(x){ #fx의 pdf를 선언 
  return(x * exp(-2*x-1/x)/mc_estimate)
}
gx <- function(x, a, b) { #gamma(a, b)의 pdf를 선언 
  return((b^a * x^(a - 1) * exp(-b * x)) / gamma(a))
}

u2 <- numeric(10^4) #10000개의 길이를 갖는 vector u2생성 
X<-c() #rejection method시 hit된 값을 저장하는 X 벡터 생성 
for (i in 1:10^4) { #생성된 난수의 수만큼 반복
  u2[i] <- runif(1, min = 0, max =  3 * gx(u1[i], 1, 1/2)) #gamma(1, 1/2)에서 난수를 뽑고 unif(0, 뽑은 난수)에서 또다른 난수 생성하여 u2[i]에 저장 
  if(u2[i] < fx(u1[i])){ #만약 u2의 값이 fx(u1[i])보다 작다면 rejection method에서 hit-> X에 저장  
    X <- c(X, u1[i]) 
  }
}
#rejection method를 이용한 방식으로 구한 fx와 실제 fx를 비교 
hist(X, freq = FALSE, breaks = 100, xlim = c(0, 6), ylim = c(0,1))
curve(fx, 0, 5, add = TRUE)

mean(X) #rejection method를 이용한 방식으로 나온 값들의 평균 값을 계산 

#gamma 분포의 pdf를 이용하여 fx의 평균을 계산하는 방법 
N <- 10000000 
u <- runif(N)
x <- -2 * log(1-u)
mean_mc <- mean(2 * x^2 * exp(-3*x/2-1/x) / mc_estimate)
mean_mc

#실제 fx의 평균을 구하는 함수를 선언하고, 그것을 0부터 inf까지 적분하여 실제 평균 값을 구함 
meanx <- function(x){ 
  return(x^2*exp(-2*x-1/x)/mc_estimate)
}
result2 <- integrate(meanx, 0, Inf) 
result2








#Bart Simpson or Claw 분포 나타내기(rejection method와 method of composition 비교하기 )
start_time <- Sys.time() #4-c)를 위한 시간 계산 함수 실행 
cx <- function(x) { #rejection method를 사용하기 위한 cauchy(0, 1)의 pdf를 cx로 정의 
  return(1 / (pi * (1 + (x)^2)))
}

u1 <- runif(10^4, min = -50, max = 50) #실제 fx의 x 범위는 -inf, inf지만, -50부터 50까지의 범위로 계산 진행 

#rejection sampling을 이용하여 계산 
fx <- function(x){ #fx의 pdf를 선언 
  result <- 1/2 * dnorm(x, 0, 1)
  for(i in 0:9){
    result <- result + dnorm(x, (i-5)/2, 1/15)/20
  }
  return(result)
}

u2 <- numeric(10^4) #unif(0, 1)을 따르는 난수 10000개를 생성 
X<-c() #rejection sampling에서 hit한 값들을 X에 저장 
for (i in 1:10^4) {
  u2[i] <- runif(1, min = 0, max = 2 * cx(u1[i])) #난수로 뽑은 u1의 cauchy(0, 1)에서의 값을 얻고, 0부터 그 값 * 2까지의 unif 분포를 따르는 난수를 뽑아 u2에 저장
  if(u2[i] < fx(u1[i])){ #rejection method에서 hit했다면, X에 저장 
    X <- c(X, u1[i])
  }
}
#rejection method를 이용한 fx를 히스토그램으로 구하기 
hist(X, freq = FALSE, breaks = 100, xlim = c(-5, 5), ylim = c(0,1))
end_time <- Sys.time() #4-c)를 위한 시간 계산 함수 실행  

start_time2 <- Sys.time() #4-c)를 위한 시간 계산 함수 실행 
#method of composition을 이용하기 위해 각각 unif(0, 1)을 따르는 난수 u1과 u2를 10000개 뽑아 저장 
u1 <- replicate(10^4, runif(1)) 
u2 <- replicate(10^4, runif(1))

x<-numeric(10^4) #난수를 뽑은 수만큼의 길이를 가진 x를 생성 

for(i in 1:10000){ 
  if(u2[i]<1/2){ #1/2의 확률로 u1의 확률값을 갖는 N(0, 1)에서의 x값을 fx의 값으로 함 
    x[i] <- qnorm(u1[i], 0, 1)
  }
  else{
    j <- sample(0:9, 1) #1/20의 확률로 u1의 확률값을 갖는 N((j-5)/2, 1/15)에서의 x값을 fx의 값으로 저장 
    x[i] <- qnorm(u1[i], (j-5)/2, 1/15)
  }
}

#method of composition를 이용하여 구한 fx를 히스토그램으로 구함 
hist(x, freq=F, breaks = 100, xlim = c(-5, 5), ylim = c(0,1))
end_time2 <- Sys.time() #4-c)를 위한 시간 계산 함수 실행 

print(end_time - start_time) #rejection sampling을 사용하여 계산한 시간 계산 
print(end_time2 - start_time2) #method of composition을 사용하여 계산한 방식의 시간 계산 
```
