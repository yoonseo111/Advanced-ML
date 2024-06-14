# Data generation
n <- 100
p <- 2
x1 <- matrix(runif(n/2*p, -1, 1) -2, ncol=p)
x2 <- matrix(runif(n/2*p, -1, 1) +2, ncol=p)
X <- rbind(x1, x2)
y <- c(rep(-1, n/2), rep(1, n/2))

plot(X[,1], X[,2], col=factor(y))

# solve.QP()
library(quadprog)
?solve.QP
n
dvec <- rep(1, n)
bvec <- rep(0, n+1)
Amat <- t(rbind(y, 
                diag(n)))

Dmat[1, 1] <- y[1]*y[1]*sum(X[1,]*X[1,]) # 곱해서 더하면 inner product
Dmat[1, 2] <- y[1]*y[2]*sum(X[1,]*X[2,])
Dmat <- matrix(NA, n, n)
for(i in 1:n){
  for(j in 1:n){
    Dmat[i,j] <- y[i] * y[j] *sum(X[i,] * X[j, ])
  }
}
dim(Dmat)

solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq =1)
res_eigen <- eigen(Dmat)
round(res_eigen$values, 10) # 0이 꽤 많음 -> solve.QP 풀지 못하는게 정상

# Dmat이 positive definite이 되도록 대각원소에 작은 상수 다 더해줌
eps <- 1e-10
res_qp <- solve.QP(Dmat = Dmat + eps * diag(n), dvec = dvec, Amat = Amat, bvec = bvec, meq =1)
res_qp$solution # lambda 값
res_qp$value # 최적값
plot(sort(res_qp$solution)) # 떨어진 3개의 점이 w*

which(res_qp$solution > 0) # 눈으로 보기엔 3개였지만, 실제로 출력하면 더 많이 나옴
tol <- 1e-4
idx_sv <- which(res_qp$solution > tol) # 이렇게 하면 딱 세개만 나옴(난 두개..?)
y[idx_sv]
X[idx_sv,] 
w <- c(0, 0)
for(i in idx_sv){
  w <- res_qp$solution[i] * y[i] * X[i, ]
}
w
plot(X[,1], X[,2], col=factor(y))
points(X[idx_sv, 1], X[idx_sv, 2], pch=3, col="blue")

i <- idx_sv[1]
(1 - y[i] * sum(w * X[i,])) / y[i]
i<- idx_sv[3]
b <- 0
for(i in idx_sv){
  b <- b + (1-y[i]*sum(w*X[i,])) / y[i]
}
b <- b / length(idx_sv)
w * X[i,]

-w[1] / w[2]
-b / w[2]
plot(X[,1], X[,2], col=factor(y))
abline(a = -b/w[2],
       b = -w[1]/w[2],
       col = "blue",
       lty = 2)
abline(a = -(b+1)/w[2],
       b = -w[1]/w[2],
       col = "red", lty = 3)
abline(a = -(b-1)/w[2],
       b = -w[1]/w[2],
       col = "red", lty = 3)

# p.16
Dmat <- matrix(0, p+1, p+1)
Dmat[1:p, 1:p] <- diag(p)
Dmat
dvec <- rep(0, p+1) # 0벡터
Amat <- matrix(NA, n, p+1)
for(i in 1:n){
  Amat[i,1:p] <- y[i] * X[i,]
  Amat[i,p+1] <- y[i]
}
dim(Amat)
Amat <- t(Amat)
bvec <- rep(1, n)
res_qp_primal <- solve.QP(Dmat + eps * diag(p+1),
         dvec,
         Amat,
         bvec)

