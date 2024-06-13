# 데이터 불러오기
uscrime <- read.table("/Users/yoonseo/Desktop/programming/uscrime.txt", header = TRUE)
# (a) 표본상관행렬 R을 구하기
R <- cor(uscrime[, -1]) # 첫 번째 열은 변수 이름이므로 제외
print(R)

# (b) 표본상관행렬을 사용하여 주성분을 구하기
pca_result <- princomp(R)
print(pca_result)

# (c) 한 주성분에 의해 가장 많이 설명되는 분산양 출력
max_variance <- max(pca_result$sdev^2)
print(paste("가장 많이 설명되는 분산양:", max_variance))

# (d) 첫 번째 주성분과 두 번째 주성분 구하기
PC1 <- pca_result$scores[,1]
PC2 <- pca_result$scores[,2]
print(PC1)
print(PC2)

# (e) 첫 번째 주성분과 두 번째 주성분 그래프 그리기
plot(PC1, PC2, main="PC1 vs PC2", xlab="PC1", ylab="PC2")

# (f) 첫 번째 주성분과 세 번째 주성분 구하기
PC3 <- pca_result$scores[,3]
plot(PC1, PC3, main="PC1 vs PC3", xlab="PC1", ylab="PC3")

# (g) 두 번째 주성분과 세 번째 주성분 구하기
plot(PC2, PC3, main="PC2 vs PC3", xlab="PC2", ylab="PC3")

# (h) 첫 번째 두 번째 주성분에 의해 설명되는 분산의 비율 출력
variance_pc1 <- var(PC1)
variance_pc2 <- var(PC2)

total_variance <- variance_pc1 + variance_pc2
proportion_variance_pc1 <- variance_pc1 / total_variance
proportion_variance_pc2 <- variance_pc2 / total_variance

print(paste("PC1에 의해 설명되는 분산의 비율:", proportion_variance_pc1))
print(paste("PC2에 의해 설명되는 분산의 비율:", proportion_variance_pc2))

# (i) 스크리 플랏 그리기
plot(pca_result)
