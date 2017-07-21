#author narumeena
#description applying svd algoritham to corpus data to reduce features.
#date 14/12/2015


library(MASS)

data <- read.csv("dtm.csv")

data.numeric <- data[,sapply(data,is.numeric)]

data.matrix <- as.matrix(data.numeric)

data.matrix <- data.matrix[,colSums(data.matrix != 0) != 0]

# svd() function returns data structure with decomposition
data.svd <- svd(data.matrix)
head(data.svd)
dim(data.svd)
data.svd$u
data.svd$d  # singular values

# Extract matrices U, Σ and V
Sigma <- diag(data.svd$d) # reduced to square matrix
U <- data.svd$u # coordinate transformations U and V
V <- data.svd$v # recall that V contains the latent dimensions


# Now reconstruct M from decomposition
round(U %*% Sigma %*% t(V), 2)


# Coordinates of target nouns in latent DSM space
U %*% Sigma

data.matrix %*% V # this version preserves row names


# Compute rank-m approximations of the original matrix M
svd.approx <- function (m) {
            U[,1:m, drop=FALSE] %*% Sigma[1:m,1:m, drop=FALSE] %*%
            t(V)[1:m,, drop=FALSE]
}


head(round(svd.approx(1), 1))


round(svd.approx(2), 1)
#part 2 
mySVD <- svd(data.matrix)

Mp <- mySVD$u[, c(1,2)] %*% diag(mySVD$d)[c(1, 2), c(1, 2)] %*% t(mySVD$v[, c(1, 2)])


cor(t(Mp))


inspect(removeSparseTerms(data, 0.4))
