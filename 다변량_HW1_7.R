# 공분산 행렬들
cov_xx <- matrix(c(10, 0, 0, 1), nrow = 2, byrow = TRUE)
cov_xy <- matrix(c(0, 0, 0.8, 0), nrow = 2, byrow = TRUE)
cov_yx <- matrix(c(0, 0.8, 0, 0), nrow = 2, byrow = TRUE)
cov_yy <- matrix(c(1, 0, 0, 20), nrow = 2, byrow = TRUE)

# 전체 공분산 행렬
cov_matrix <- rbind(cbind(cov_xx, cov_xy), cbind(cov_yx, cov_yy))

# 공분산 행렬을 상관계수 행렬로 변환
cor_matrix <- cov2cor(cov_matrix)

# 상관계수 행렬의 고유값과 고유벡터 계산
eigen_result <- eigen(cor_matrix)

# 정준변수
canonical_variables <- eigen_result$vectors

# 정준상관계수
canonical_correlations <- sqrt(eigen_result$values)

# 결과 출력
cat("정준변수:\n")
print(canonical_variables)
cat("\n정준상관계수:\n")
print(canonical_correlations)
