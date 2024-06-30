#Linear Regression model의 점화식을 이용하여 빠르게 계산하기
solution1 <- function(x, y){
  result <- NULL #data frame인 result 선언
  for(i in 2:length(x)){ #data의 수가 2개부터 n개일 때까지 ahat과 bhat구하기
    xbar <- sum(x[1:i])/i #ahat과 bhat을 구하기 위한 xbar변수 설정
    ybar <- sum(y[1:i])/i #ahat과 bhat을 구하기 위한 ybar변수 설정
    
    bhatover <- sum((x[1:i] - xbar)*(y[1:i] - ybar))#data의 수가 i개일 때 bhat의 분자 구하기
    bhatunder <- sum((x[1:i] - xbar)^2) #data의 수가 i개일 때 bhat의 분모 구하기
    
    bhat <- bhatover/bhatunder #data의 수가 i개일 때 bhat 값 구하기
    ahat <- ybar - bhat * xbar#data의 수가 i개일때 ahat 값 구하기
    result <- rbind(result, c(bhat, ahat)) #data의 수가 i개일 때의 ahat, bhat을 result에 저장하기
  }
  return(result) #data의 수가 1개일 때부터의 ahat, bhat값 re  turn
}

solution2 <- function(x, y){
  xbar <- 0 #ahat과 bhat을 구하기 위한 xbar변수 선언
  ybar <- 0 #ahat과 bhat을 구하기 위한 ybar변수 선언
  
  ahat <- 0 #ahat 변수 선언
  bhatover <- 0 #bhat 분자 변수 선언
  bhatunder <- 0 #bhat 분모 변수 선언
  bhat <- 0 #bhat 변수 선언
  result <- NULL #ahat과 bhat 값을 저장하기 위한 result 선언
  
  for(i in 2:length(x)){ #data의 수가 2개일 때부터 모든 data를 포함할 때까지
    if(i==2){
      xbar <- sum(x[1:i])/i #ahat과 bhat을 구하기 위한 xbar변수 설정
      ybar <- sum(y[1:i])/i #ahat과 bhat을 구하기 위한 ybar변수 설정
      
      bhatover <- sum((x[1:i] - xbar)*(y[1:i] - ybar))#data의 수가 i개일 때 bhat의 분자 구하기
      bhatunder <- sum((x[1:i] - xbar)^2) #data의 수가 i개일 때 bhat의 분모 구하기
      
      bhat <- bhatover/bhatunder #data의 수가 i개일 때 bhat 값 구하기
      ahat <- ybar - bhat * xbar#data의 수가 i개일때 ahat 값 구하기
      result <- rbind(result, c(bhat, ahat)) #data의 수가 i개일 때의 ahat, bhat을 result에 저장하기
    }
    else{
      beforebhatover <- bhatover #이전 데이터까지의 bhat의 분자 값을 beforebhatover 변수에 저장
      beforebhatunder <- bhatunder #이전 데이터까지의 bhat의 분모값을 beforebhatunder 변수에 저장
      beforeahat <- ahat #이전 데이터까지의 ahat 값을 beforeahat 변수에 저장
      
      bhatover <- beforebhatover + (i-1)*(y[i]-ybar)*(x[i] - xbar)/i #이전 데이터까지의 bhat 분자 값을 이용하여 현재 데이터까지 포함한 bhat 분자 값을 구하기
      bhatunder <- beforebhatunder + (i-1)*(x[i] - xbar)^2/i #이전 데이터까지의 bhat 분모 값을 이용하여 현재 데이터까지 포함한 bhat 분모 값을 구하기
      
      xbar <- (xbar * (i-1) + x[i])/i #현재 데이터까지 포함한 xbar 구하여 저장
      ybar <- (ybar * (i-1) + y[i])/i #현재 데이터까지 포함한 ybar 구하여 저장
      
      bhat <- bhatover / bhatunder #구한 bhat 분자 값과 bhat 분모 값을 이용하여 데이터의 수가 i개 일 때의 bhat 구하기
      ahat <- ybar - xbar * bhat #구한 bhat값을 이용하여데이터의 수가 i개 일 때의 ahat 구하기
      result <- rbind(result, c(bhat, ahat)) #구한 bhat과 ahat을 result data frame 에 저장
    }
  }
  return(result) #bhat과 ahat을 누적 저장한 data frame인 result를 return
}

n <- 10^4; x <- rnorm(n); y <- 2+x+0.25*rnorm(n) # 10000개의 데이터를 랜덤으로 생성
system.time(solution1(x, y)) #ahat과 bhat을 구하는 공식으로 구하는 함수를 실행하여 실행 시간 출력
print(solution1(x, y)[9990:9999,]) #많은 데이터로 데이터의 수가 9991 ~ 10000개일 때의 bhat과 ahat 출력
system.time(solution2(x, y)) #이전 데이터까지의 ahat과 bhat을 활용하여 현재 데이터를 포함한 ahat과 bhat을 구하는 함수를 실행하여 실행 시간 출력
print(solution2(x, y)[9990:9999,]) #많은 데이터로 데이터의 수가 9991 ~ 10000개일 때의 bhat과 ahat 출력


#X ~ DU(m)인 X의 Moments를 계산하고 Y = X/m일 때의 Y의 moments도 계산하기 
solution1 <- function(m, n){ #DU(m)의 분포를 따를 때 n번째까지의의 moment 값을 구하는 과정
  result <- numeric(n)
  for(i in 1:n){ #1~n번째까지의 moment 구하기
    sum <- 0 #t = 1~m까지, j=0~i-1까지의 C(i,j) * t^(j+1)의 합 결과를 저장하는 sum 변수 선언
    for(t in 1:m){ #t = 1~m까지, j=0~i-1까지의 t^(j+1)의 합 구하기
      for(j in 0:(i-1)){
        sum <- sum +exp(lgamma(i+1) - lgamma(j+1) - lgamma(i-j+1) + (j+1)*log(t))
      }
    }
    moment <- exp(i*log(m+1) - i*log(m)) - exp(log(sum) - (i+1)*log(m)) #2-(a)를 이용하여 i번째 moment 구하기
    result[i] <- moment #result에 moment값 저장
  }
  return(result) #n번째까지의 moment 결과들을 return 
}

solution2 <- function(m, n){
  sum <- 0
  if(n == 1){
    return(c((m+1)/(2*m)))
  }
  else{
    result <- solution2(m, n-1) #n-1번째까지의 moment값 구하기
    for(j in 1:(n-1)){
      sum <- sum + exp(lgamma(n+1) - lgamma(j) - lgamma(n-j+2) + j*log(m) + log(result[j])) #result에 저장된 이전 moment값들을 활용하여 n번째 moment를 구하기 위한  j = 1~n-1까지의 C(n, j-1) * j번째 moment의 합 구하기
    }
    an <- exp(log(exp(n*log(m+1)) - sum) - log(n+1) - n*log(m)) #반복문을 통해 구한 sum값과 2-(b)의 공식을 활용하여 n번째 moment 구하기
    return(c(result, an)) #n번째까지 구한 moment들을 return
  }
}


system.time(solution1(10000, 10)) #solution1을 활용하여 Y의 n번째 moment를 구하기 위한 함수 실행 시간 확인
print(solution1(10000, 10))
system.time(solution2(10000, 10)) #solution2를 활용하여 Y의 n번째 moment를 구하기 위한 함수 실행 시간 확인
print(solution2(10000, 10))

#k개의 방에 n명의 사람들이 들어갈 때의 경우의 수 구하기 
solution1 <- function(n, k){ #방의 수 k와 사람의 수 n을 함수의 인수로 설정
  if(k == 1){ #방의 수가 1개일 때, 빈 방이 없을 확률은 1이므로 1을 return
    return(c(1))
  }
  else{ #방의 수 k가 2 이상이면 다음과 같이 코드 실행
    except <- 0 #빈 방이 있을 확률을 except에 저장 
    result <- solution1(n, k-1) #사람의 수가 n명일 때, 방의 수가 1개일 때부터 k-1개일 때까지 확률을  재귀함수를 통해 구함
    for(i in 1:(k-1)){ #방의 수가 1개일 때부터 k-1개일 때까지의 경우의 수를 이용하여 사람의 수가 n명, 방의 수가 k일 때의 확률 구하기
      #k개일 때 빈 방이 없을 경우는 방이 1개일 때부터 k-1개일 때까지의 경우(빈 방이 k-1개부터 1개가 있을 경우)를 빼주면 구할 수 있음
      #이를 위해 방이 1개~k-1개일 때 빈방이 없는 경우의 수를 구하고 이를 방이 k개일 때의 경우의 수를 구하는데 활용 
      except <- except + exp(lgamma(k+1) + n*log(i) + log(result[i]) - lgamma(i+1)-lgamma(k-i+1) - n*log(k)) 
      #except <- except + factorial(k)*(i^n)*result[i]/(factorial(i)*factorial(k-i)*(k^n))
    }
    return(c(result, 1 - except)) #방의 수가 k개일 때까지의 결과 return
  }
}

result1 <- solution1(30, 10)[10] #방의 수가 10개, 사람의 수가 30명일 때 빈 방이 없을 확률 구하기
result2 <- solution1(50, 20)[20] #방의 수가 20개, 사람의 수가 50명일 때 빈 방이 없을 확률 구하기
result3 <- solution1(1000, 120)[120] #방의 수가 120개, 사람의 수가 1000명일 때 빈 방이 없을 확률 구하기
result4 <- solution1(10000, 1200)[1200] #방의 수가 1200개, 사람의 수가 10000명일 때 빈 방이 없을 확률 구하기
print(c(result1, result2, result3, result4)) #문제에서 요구하는 경우의 확률 구하기


#하나의 스티커에서 12간지 중 하나가 나오는 조건 하에서 20번 안에 돼지가 뽑힐 확률 구하기
solution1 <- function(){
  check <- FALSE #돼지(12번)을 뽑았음을 나타내는 변수 check 선언
  count <- 0 #시도한 횟수를 count 변수에 저장
  while(!check && count < 20){ #돼지를 아직 못 뽑았거나 뽑은 횟수가 20번이 안되었을 때까지 반복 실행 
    sticker <- sample(1:12, 1) #스티커는 1번부터 12번 스티커까지 랜덤으로 하나가 뽑힘
    count<-count + 1 #전체 뽑은 횟수 1 증가
    if(sticker == 12){ #뽑기 중 돼지(12번)이 나왔다면, 
      check <- TRUE #돼지를 뽑았음을 나타내는 check에 TRUE 저장 
      return(c(1)) #돼지를 뽑았음을 알리는 1 return 
    }
  }
  return(c(0)) #20번 시도할 때까지 돼지를 못 뽑았으므로 0 return 
}
result1 <- replicate(10^4,solution1()) #20번 중 돼지가 한 번이라도 뽑힐 확률을 구하기 위한 함수를 10000번 반복 실행 
print(sum(result1)/length(result1)) #돼지가 20번의 뽑기 중 한 번이라도 뽑힐 확률 출력 

#위 문제와 조건 동일하게 모든 스티커를 모으는 뽑기 횟수의 평균 구하기 
solution2 <- function(){
  check <- rep(0, 12) #해당 스티커를 이미 소유하였는지 확인하는 check선언
  all <- FALSE #모든 스티커를 아직 구하지 못했다면 FALSE, 다 구했다면 TRUE를 나타내는 all변수 선언
  count <- 0 #시도한 횟수를 count 변수에 저장
  while(all == FALSE){ #모든 스티커를 다 구할 때까지 반복 실행
    sticker <- sample(1:12, 1) #스티커는 1번부터 12번 스티커까지 랜덤으로 하나가 뽑힘
    check[sticker] <- check[sticker] + 1 #뽑힌 번호의 스티커가 뽑힌 횟수를 하나 증가시킴
    count<-count + 1 #전체 뽑은 횟수 1 증가
    if(min(check) != 0){ #모든 스티커를 뽑았다면(가장 적게 뽑힌 스티커의 수가 0개가 아니라면)
      return(count) #그때의 뽑은 횟수를 return
    }
  }
}
result2 <- replicate(10^4,solution2()) #모든 스티커를 뽑을 때까지의 스티커 뽑기 시도 횟수를 구하는 함수를 10000번 시뮬레이션 진행
print(sum(result2)/length(result2)) #모든 스티커를 뽑을 때까지의 평균 스티커 뽑기 시도 횟수를 출력

#100마리 중 20마리에 tag를 하고 풀어주었을 때, tag된 동물을  5마리 잡는 경우의 평균 구하기 
solution <- function(n, k, r){ #모든 동물의 수 n, tag된 동물의 수 k, 다시 모아야 하는 tag된 동물의 수 r을 함수의 인수로 받음
  tag <- sample(1:n, k, replace = FALSE) #각 동물에 번호가 있다고 하면, 그 중 k마리를 랜덤으로 선택하여 tag에 저장
  check <- rep(0, n) #다시 동물을 잡았을 때 동물이 잡혔는지의 여부를 확인하는 check 선언
  count <- 0 #잡은 동물의 수를 저장하는 변수 count 선언
  capture <- 0 #잡은 tag된 동물의 수를 저장하는 변수 capture 선언
  while(capture<r){ #잡은 tag동물의 수가 r마리가 될 때까지 동물을 잡는 과정 시행
    now <- sample(1:n, 1) #1번부터 n번까지의 동물 중 한 마리를 랜덤으로 잡음
    if(check[now]==0){ #잡은 동물이 잡히지 않은 동물이었다면,
      check[now] = 1 #해당 번호의 동물이 잡혔다는 것을 알기 위한 코드 수행
      count <- count + 1 #잡은 동물의 수에 1을 더함
      if(now %in% tag){ #잡은 동물이 tag된 동물이라면
        capture <- capture + 1 #잡힌 tag된 동물의 수에 1을 더함
      }
    }
  }
  return(count) #tag된 동물의 수를 r마리 잡을 때까지의 잡은 동물의 수를 return
}
result <- replicate(10^4,solution(100, 20, 5)) #100마리 중 20마리에 tag를 하고 tag된 동물을 5마리까지 잡을 때까지의 잡은 동물의 수를 구하는 과정을 10000번 시뮬레이션을 진행
print(sum(result)/length(result))#10000번 반복한 결과 평균 잡은 동물의 수를 출력

#몬티홀 문제
solution <- function(){
  answer <- sample(c("A", "B", "C"), 1, prob=c(1/4, 1/2, 1/4)) #A, B, C가 석방될 확률을 이용하여 랜덤으로 석방될 한 명 구하기
  countab <- 0 #b가 석방되지 않는다고 하였을 때, a가 석방된 경우라면 1을 저장
  countb <- 0 #b가 석방되지 않는다고 하였다면 1을 저장
  countac <- 0 #c가 석방되지 않는다고 하였을 때, a가 석방된 경우라면 1을 저장
  countc <- 0 #c가 석방되지 않는다고 하였다면 1을 저장
  if(answer == 'A'){ #A가 석방되는 경우였을 때,
    choice = sample(c("B", "C"), 1) #B와 C 중 먼저 발표될 석방되지 않는 사람을 고름 
    if(choice == 'B'){ #B가 골라졌다면 이 경우를 나타내는 변수에 1을 저장
      countab <- countab + 1 
      countb <- countb + 1
    }
    else{ #C가 선택되었다면 이 경우를 나타내는 변수에 1을 저장
      countac <- countac + 1
      countc <- countc + 1
    }
  }
  else if(answer== 'B'){ #B가 석방되는 경우라면, C가 먼저 발표되므로 이 경우를 나타내는 변수에 1을 저장
    countc <- countc + 1
  }
  else{
    countb <- countb + 1 #C가 석방되는 경우라면, B가 먼저 발표되므로 이 경우를 나타내는 변수에 1을 저장 
  }
  return(c(countab, countb, countac, countc)) #시뮬레이션 1번을 실행했을 때 모든 경우들의 결과를 return
}
result <- replicate(10^6,solution()) #10^6번만큼 시뮬레이션을 실행
print(sum(result[1,]) / sum(result[2,])) #시뮬레이션을 통해 나온, B가 먼저 석방되지 않는다고 발표되었을 때 A가 석방되는 확률을 출력
print(sum(result[3,]) / sum(result[4,])) #시뮬레이션을 통해 나온, C가 먼저 석방되지 않는다고 발표되었을 때 A가 석방되는 확률을 출력
