#각자가 정한 동전 H, T의 순서가 나오면 승리하는 게임에서의 승리할 확률 구하기 
sim.game <- function(A, B){
  first <- sample(c("H", "T"), 1); #가장 처음 동전의 면 랜덤 선택
  second <- sample(c("H", "T"), 1); #두 번째 동전의 면 랜덤 선택
  third <- sample(c("H", "T"), 1); #세 번째 동전의 면 랜덤 선택
  
  if(first == A[1] && second==A[2] && third==A[3]){ #처음 세 번의 동전에 A의 승리 조건이 있다면 A가 승리했음을 return
    return(0)
  }
  else if(first == B[1] && second == B[2] && third == B[3]){ #처음 세 번의 동전에 B의 승리 조건이 있다면 B가 승리했음을 return
    return(1)
  }
  else{ #처음 세 번의 동전으로 승패 결정이 안 난다면, 동전을 계속 던짐
    while(TRUE){
      first <- second; second <- third; third <- sample(c("H", "T"), 1);
      #마지막 3번의 결과만 비교하면 되므로 동전을 새로 뽑을 때마다 마지막 세 번의 순서를 앞으로 당겨서 저장 
      if(first == A[1] && second==A[2] && third==A[3]){ #새로 뽑고 난 후 A의 승리 조건이 된다면 A가 승리했음을 return 
        return(0)
      }
      else if(first == B[1] && second == B[2] && third == B[3]){ #새로 뽑고 난 후 B의 승리 조건이 된다면 B가 승리헸음을 return 
        return(1)
      }
    }
  }
}

result1 <- replicate(10^4, sim.game(c('H', 'H', 'T'), c('H', 'H', 'H'))); 
#replicate 함수를 활용하여 A가 HHT, B가 HHH를 선택했을 때의 시뮬레이션을 10^4번 진행 
mean(result1)#진행한 결과의 평균은 B의 승리 확률과 같음 

choice.B <- function(A){
  a <- '' #A의 선택을 2진수 문자열로 나타낸 결과를 저장하는 변수 a선언 
  #아래 조건문은 A의 승리 조건을 H=0, T = 1로 하여 2진수로 나타내는 조건문임
  if(A[1]=='H'){
    if(A[2]=='H'){
      if(A[3]=='H') a <-'000'
      else a<-'001'
    }
    else{
      if(A[3]=='H') a <-'010'
      else a<-'011'
    }
  }
  else{
    if(A[2]=='H'){
      if(A[3]=='H') a <-'100'
      else a<-'101'
    }
    else{
      if(A[3]=='H') a <-'110'
      else a<-'111'
    }
  }
  decimal_a <- strtoi(a, base = 2) #생성된 2진수를 10진수로 바꿈
  quotient_a <- decimal_a%/%2 #위에서 구한 10진수를 2로 나눈 몫을 구함
  quotient_a <- quotient_a * 5 + 4#문제 조건에 따른 계산 
  binary_result <- intToBits(quotient_a) #계산 결과를 2진수로 바꿈
  
  b_result <- c() #b의 선택을 문제 조건에 따라 저장하는 변수 b_result 선언
  #binary_result[i]는 오른쪽에서 2^(i-1)의 위치에 해당하는 bit의 값이 문자열로 나타나 있음 
  if (binary_result[3] == '00'){ #B의 선택 순서 중 가장 처음에 해당하는 부분 
    b_result[1] <- 'H'
  }else{
    b_result[1] <- 'T'
  }
  if (binary_result[2] == '00'){ #B의 선택 순서 중 가운데에 해당하는 부분 
    b_result[2] <- 'H'
  }else{
    b_result[2] <- 'T'
  }
  if (binary_result[1] == '00'){ #C의 선택 순서 중 마지막에 해당하는 부분 
    b_result[3] <- 'H'
  }else{
    b_result[3] <- 'T'
  }
  return(b_result)
}
choice.B(c('T', 'H', 'T')) #문제 예시 A가 THT를 골랐을 때 B의 선택은 TTH가 되어야 함 

B_chosen <- choice.B(c('H','H', 'T'))
result2 <- replicate(10^4, sim.game(c('H', 'H', 'T'), B_chosen)); 
#replicate 함수를 활용하여 A가 HHT, B가 HHH를 선택했을 때의 시뮬레이션을 10^4번 진행 
mean(result2)#진행한 결과의 평균은 B의 승리 확률과 같음 



#Geom(p)의 분포를 따르는 X 및 Xi의 합 계산 문제 
result <- replicate(10^4, min(rgeom(50, 0.01))) #기하 분포를 따르는 X1, ... ,Xn을 랜덤으로 생성하고 그 최솟값을 return하는 것을 10^4번 시뮬레이션 
(table(result)/length(result))[1:11]#y가 0부터 10까지 P(Y=y)의 값을 출력 

y <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10) # 주어진 y 값 
pdfy <- function(y, n, p){ #문제에서 주어진 식에 따른 값을 구하는 함수 생성 
  result <- (1-p)^(n*y) * (1-(1-p)^n) 
  return(result) 
}
pdfy(y, 50, 0.01) #식에 따른 값을 구함 

solution <- function(n, k, p, probmat){ #n은 성공 횟수, k는 실패 횟수, p는 성공 확률, n과 k 값에 따른 확률을 저장한 probmat을 argument로 한 함수 선언 
  #index의 값이 1부터 시작하므로 k가 0의 값이 index 1부터 저장되어 k+1까지의 index값을 갖는다
  if(probmat[n, k+1] != 0){ #이미 계산된 상황이라면, probmat 그대로를 return 
    return(probmat)
  }else{ #아직 계산되지 않은 상황이라면, 계산하여 probmat에 저장하고 return한다
    if(n==1){ #n==1일 때 문제 식에 따라 확률 계산하여 저장하고 return
      probmat[n, k+1] <- (1-p)^k * p
      return(probmat)
    }
    if(k==0){ #k==0일 때 문제 식에 따라 확률을 계산하여 저장하고 return
      probmat[n, k+1] <- p^n
      return(probmat)
    }
    #위 두 경우가 아니라면, 이전 확률들을 활용하여 계산해야 함 
    probmat1 <- solution(n, k-1, p, probmat) #recursive 방법을 활용해 qn(k-1)의 값을 구함 
    probmat2 <- solution(n-1, k, p, probmat1) #recursive 방법을 활용해 qn-1(k)의 값을 구함 
    probmat2[n, k+1] <- (1-p) * probmat2[n, k] + p * probmat2[n-1, k+1] #위 두 값을 활용하여 n, k일 때의 확률 값 구함 
    return(probmat2) #모든 확률이 구해져 저장되어 있는 matrix return
  }
}
probmat <- matrix(0, nrow = 10, ncol = 41) #n=10, k=0:40에 따른 10 X 41의 matrix를 선언하고 0으로 초기화 -> 해당 확률 값이 0이면 아직 계산 안 된 상황임 
result <- solution(10, 40, 0.4, probmat) #n = 1:10, k = 0:40, p = 0.4일 때의 qn(k)의 값을 구하는 함수 실행
result[10,] #n = 10인 경우만 출력
print('\n') #재귀 방식과 음이항 분포 식 결과를 보기 편하게 나누기 위한 print('\n')임
n<-10;k<-0:40;p<-0.4; #문제 조건 설정
dnbinom(x = k, size = 10, prob = 0.4)#음이항 분포를 활용하는 dnbinom 함수를 활용해 n=10일 때의 k = 0:40인 경우의 qn(k)의 값들을 구함 

n <- c(1:100) #문제 조건에 따른 n의 범위 설정 
p <-0.4 #문제 조건에 따른 확률 설정 
result2 <- c() #n에 값에 따른 문제 요구 확률을 저장하기 위해 result2d 선언 
for(i in n){ #i가 1부터 n까지 반복 
  sn <- floor(i * (1-p)/p) #Sn은 정수의 값만을 가지므로, Sn의 범위는 n*(1-p)*p를 내림한 정수보다 작거나 같다고 할 수 있음 
  probmat <- matrix(0, nrow = i, ncol = sn+1) #2-c)의 함수를 사용하기 위한 matrix 선언 
  result_mat <- solution(i, sn, p, probmat) #2-c)의 함수 실행 
  result2 <- c(result2, sum(result_mat[i,]))  #그 결과를 2-d)에서 요구하는 부등식의 범위에 맞게 sum을 하여 result2d에 저장 
}
plot(n, result2, xlab = 'n', ylab = 'an', col='red', lwd=2, type = 'l') #구한 결과를 plot으로 나타냄




#pdf가 f(x) = (1+x^2)^(-3/2)/2인 분포의 cdf를 적분 함수 없이 계산하기 
n <- 100 #테일러 급수를 어디까지 전개하는지 결정 
sqrt1plus <- function(x){ #sqrt(1+x)의 함수값을 테일러 급수를 활용한 방식으로 계산하는 함수 선언
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

dt.taylor <- function(x, j){
  a <- c((1+j)^(-1) * sqrt1plus(-j/(j+1))) #HW2의 (1+x)^(1/2)의 테일러 급수 함수를 활용하여 ak값들을 저장하는 a 선언 (a0를 포함한) 
  result <- 0
  for(k in 0: n){
    if(k != 0){
      a <- c(a, (-1/2-k)*(1+j)^(-1) * a[k]/k) #a[1]은 문제의 a0를 의미 
    }
    result <- result + a[k+1] * (x^2-j)^k #k일 때의 값을 결과 값에 더해줌 
  }
  result <- result/2 #마지막으로 1/2을 곱함 
  return(result) #결과 return 
}

result1 <- c()
real1 <-c()
for(i in -3:3){
  result1 <- c(result1, dt.taylor(i, i^2 + 0.1)) #테일러 급수 구현 함수를 이용하여 계산 
  real1 <- c(real1, (1+i^2)^(-3/2)/2) #실제 함수를 이용하여 계산 
}
result1 #테일러 급수 구현 함수를 이용하여 계산한 결과 
real1 #실제 함수를 이용하여 계산한 결과



#linear regression model에서 error가 Cauchy(0, 1)의 분포를 따를 때의 MLE 구하기
#문제에서 주어진 데이터 
x <- c(-1.18, -0.71, 0.02, 0.07, -0.10, 0.68, -0.29, -0.21, -0.56, 0.18, -0.41, 0.69, 2.03, -1.10, 0.44, -0.28, 1.76, 0.89, -0.34, -0.16)
y <- c(-3.04, -1.32, -0.87, 60.38, -0.90, 0.49, -8.07, 0.78, 0.24, -0.45, -7.58, 3.50, 2.48, -5.64, 1.29, 2.05, 2.29, 1.32, -0.48, -2.61)

#일계도함수와 이계도함수를 코드로 작성 
l_prime <- function(b){sum((2*x*(y-b*x))/(1+(y-b*x)^2))}

ll_prime <- function(b){sum((2*x^2*(-1 + (y-b*x)^2))/(1+(y-b*x)^2)^2)}

#-1부터 2까지 10^3개의 수로 나누고 해당 값에 함수 값을 대응하여 그래프 생성 
b <- seq(-1, 2, length = 10^3)
par(mfrow = c(1, 2)) #그래프 2개를 보여줄 것임 
plot(b, sapply(b, l_prime), type='l', ylab = 'l_prime', xlab = 'b') #일계도함수 그래프 그리기 
plot(b, sapply(b, ll_prime), type='l', ylab = 'll_prime', xlab = 'b')#이계도함수 그래프 그리기 

#fixed point 
g1 <- function(b){l_prime(b)/100 + b} #고정점을 찾을 수 있는 g함수를 찾아 g1에 선언 
fixedpoint <- function(f, b0, tol = 1e-11, max.iter = 1000){ #고정점을 찾는 함수선언  
  bold <- b0; bnew <- f(bold); iter <- 1; 
  while((abs(bnew - bold) > tol) & (iter < max.iter)){#오차 허용 범위가 되거나 최대 반복 횟수를 넘기 전까지 고정점 찾기 반복 
    bold <- bnew; bnew <- f(bold) 
    iter <- iter + 1 
  } 
  if(abs(bnew - bold)> tol){ 
    cat("Algoritm failed to converge\n"); return(NULL) #고정점을 못 찾았을 경우 NULL을 return  
  } 
  else{ #고정점을 찾았으면 해당 고정점(bnew)을 이용하여 신뢰구간 추정하고, 고정값과 신뢰구간을 return 
    cat("Algorithm converged\n")
    MLE.var <- -1/ll_prime(bnew) #오차가 허용 범위라면 MLE.var값을 구함 
    return(c(bnew, bnew + qnorm(0.025)*sqrt(MLE.var), bnew + qnorm(0.975)*sqrt(MLE.var))) 
  } 
}
result2 <- fixedpoint(g1, b0 = 1) #고정점을 찾는 방법을 활용하여 MLE와 CI 계산 
print(result2)

#Newton-Raphson
MLE.est <- function(b, f, tol = 1e-11){ #MLE를 구하는 함수 선언  
  temp <- b; result_b <- temp + 10 * tol; 
  while(abs(result_b-temp) > tol){ #오차가 허용 범위일 때까지  
    temp <- result_b; result_b<- result_b-f(result_b)/ll_prime(result_b) #뉴턴 랩슨 방식을 활용하여 temp와 result_temp를 반복하여 저장  
  } 
  #반복분이 종료되면 result_theta 변수에 MLE가 저장되어 있음  
  MLE.var <- -1/ll_prime(result_b) #오차가 허용 범위라면 MLE.var값을 구함  
  return(c(result_b, result_b + qnorm(0.025)*sqrt(MLE.var), result_b + qnorm(0.975)*sqrt(MLE.var))) #구한 MLE값과 MLE.var값을 이용하여 95%의 신뢰구간을 구함  
}
MLE.est(1, l_prime) #뉴턴 랩슨 방법을 통해 MLE와 CI계산 

#secant method
secant <- function(f, b0, b1, tol = 1e-11, n=10^6){ #secant 방법을 이용하여 MLE를 구하는 함수 선언 
  iter<-0;temp1<-b1;temp2<-b0;b<-temp1 + 10*tol; 
  while(abs(b-temp1) > tol){ 
    iter <- iter + 1 
    if (iter>n) stop("Secant method does not converge") 
    teamp2 <- temp1; temp1 <- b; 
    b <- temp1 - f(temp1)*(temp1 - temp2)/(f(temp1) - f(temp2)) 
  }
  MLE.var <- -1/ll_prime(b) #오차가 허용 범위라면 MLE.var값을 구함 
  return(c(b, b + qnorm(0.025)*sqrt(MLE.var), b + qnorm(0.975)*sqrt(MLE.var))) #secant 방법을 통해 구한 MLE와 구할 때까지의 반복 횟수 return 
} 

secant(l_prime, 1, 2) #secant 방식을 통해 MLE와 CI 계산

num.deriv <- function(b, epsilon = 1e-11, f){ #특정 함수가 들어오면 central difference 방식을 이용하여 이계도함수 값 근사  
  result1 <- f(b + epsilon) 
  result2 <- f(b - epsilon) 
  return((result1 - result2)/(2*epsilon)) 
} 

bisection <- function(f, b0, b1, tol = 1e-11){ #bisection 방식을 이용하여 MLE를 계산하는 함수 선언 
  iter <-0; f.l <- f(b0); f.r<-f(b1); b<0 #bisection방식을 사용하기 위한 변수 선언 
  #만약 b0나 b1의 함수값이 0이라면 bisection방식을 진행하지 않아도 됨 
  if(f.l == 0){b <- b0} 
  else if(f.r == 0){b <- b1} 
  else if (f.l*f.r > 0 ){return("incorrect specification")}
  else{#위 경우가 아니라면, bisection방식을 활용하여 진행 
    while((b1-b0)>tol){ 
      iter <- iter + 1 
      b.m <-(b0+b1)/2; f.m <- f(b.m);
      if(f.m==0){ #함수값이 0인 b.m을 찾으면 b에 b.m을 저장하고 while문 break 
        b <- b.m
        break
      }#그렇지 않다면, 부호에 따라 b0, b1값 재설정 
      else if (f.l*f.m < 0) {b1 <- b.m; f.r <- f.m} 
      else{b0 <- b.m; f.l<-f.m} 
    }
    b <- (b0 + b1)/2 #최종적인 b의 값을 구함 
  }
  MLE.var <- -1/num.deriv(b, f = f) #오차가 허용 범위라면 MLE.var값을 구함 
  return(c(b, b + qnorm(0.025)*sqrt(MLE.var), b + qnorm(0.975)*sqrt(MLE.var))) #bisection 방법을 통해 구한 MLE와 구할 때까지의 반복 횟수 return 
}
bisection(l_prime, 1, 2)

#5 - a)
p <- 0.3 #문제 조건에 따른 p값 설정 
cp <- (1-p)/30 #간편한 계산을 위해 확률 식에 공통된 부분을 cp로 설정 
craps_sim <- function(){ #크랩스 게임 시뮬레이션을 위한 함수 craps_sim 선언 
  roll <- sample(2:12, 1, prob = c(cp, 2*cp, 3*cp, 4*cp, 5*cp, p, 5*cp, 4*cp, 3*cp, 2*cp, cp)) #문제 조건에 따른 확률로 주사위 합을 랜덤하게 뽑음 
  if (roll == 2 || roll == 3 || roll == 12) win <- 0 #첫 번째 뽑은 결과로 바로 질 수 있는 경우라면 0을 저장 
  else{
    if (roll == 7 || roll == 11) win <- 1 #첫 번째 뽑은 결과로 바로 이길 수 있는 경우라면 1을 저장  
    else{ #첫 번째 뽑은 결과로 승패를 따질 수 없다면 승패가 결정될 때까지 계속 반복 
      roll2 <- 1 #두 번째 부터 뽑은 주사위 합을 저장할 변수 roll2 선언 
      while (roll2 != 7 && roll2 != roll ){ #승패가 결정될 때까지 랜덤으로 합을 뽑음 
        roll2 <- sample(2:12, 1, prob = c(cp, 2*cp, 3*cp, 4*cp, 5*cp, p, 5*cp, 4*cp, 3*cp, 2*cp, cp))
        if (roll2 == 7) win <- 0 #패배가 결정되면 0을 저장 
        if (roll2 == roll) win <- 1 #승리가 결정되면 1을 저장 
      }}}
  return(win) #승패 결과를 저장한 변수 win을 return 
}
craps_res <- replicate(10^4,craps_sim()); #시뮬레이션을 10^4번 실행함 
mean(craps_res) #평균은 승리할 확률을 나타냄 

real5a <- function(p){(1+14*p)/15 + (1-p)^2 * (1/(5*(1+9*p)) + 8/(15*(2+13*p)) + 1/(3*(1+5*p)))} #문제에서 제공한 식을 활용한 계산을 하는 함수 선언 
real5a(0.3)#p가 0.3일 때의 식 계산 값 출력





#Craps 문제 
state = c("C","W","L",4,5,6,8,9,10) #문제 조건에 따라 state선언 
p <- 0.3 #문제 조건에 따른 p값 설정 
cp <- (1-p)/30 #간편한 계산을 위해 확률 식에 공통된 부분을 cp로 설정 
P.craps = matrix(0, nrow=length(state), ncol=length(state)) #크랩스 게임을 위한 transition matrix 선언 
colnames(P.craps) = rownames(P.craps) = state #matrix의 행과 열의 이름을 state로 설정 
P.craps[1,2:3] = c(p + 2*cp, 4*cp) #첫번째에 바로 승패가 결정될 경우에 따라 P.craps 값 수정 
P.craps[1,-(1:3)] = c(3,4,5,5,4,3) * cp #첫 번째에 승패가 결정되지 않을 경우에 따라 P.craps값 수정 
diag(P.craps)[2:3] = 1 #승패가 결정되면 자기 자신으로만 반복해야 하므로 각각 1을 설정 
P.craps[-(1:3),2] = c(3,4,5,5,4,3) * cp #여러번 뽑을 때의 승리할 확률을 크랩스 게임의 규칙에 따라 설정 
P.craps[-(1:3),3] = p #여러번 뽑을 떄의 패배할 확률을 크랩스 게임의 규칙에 따라 설정
#승패가 결정되지 않은 경우를 첫번째 뽑은 주사위 합에 따라 matrix값을 수정
P.craps[4, 4] = 1 - P.craps[4, 2] - P.craps[4, 3]
P.craps[5, 5] = 1 - P.craps[5, 2] - P.craps[5, 3]
P.craps[6, 6] = 1 - P.craps[6, 2] - P.craps[6, 3]
P.craps[7, 7] = 1 - P.craps[7, 2] - P.craps[7, 3]
P.craps[8, 8] = 1 - P.craps[8, 2] - P.craps[8, 3]
P.craps[9, 9] = 1 - P.craps[9, 2] - P.craps[9, 3]


library(markovchain) #markovchain library를 불러옴
Pmatcraps <- as(P.craps,"markovchain"); #P.craps를 markovchain 클래스로 변환한 Pmatcraps를 선언하고 
is(Pmatcraps,"markovchain"); #Pmatcraps가 markovchain이 맞는지 확인 
result <- P.craps #P.craps를 10^4제곱한 결과를 저장할 result5b 선언 
for(i in 2:10^4){ #result5b에 10^4제곱한 결과를 저장하기 위한 반복문 실행 
  result5 <- result %*% P.craps
}
print(round(result, 3))[1, ] #10^4제곱했을 때의 승패 확률 계산 결과를 출력 
