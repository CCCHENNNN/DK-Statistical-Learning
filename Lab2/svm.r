require(LowRankQP)

#     This routine implements a primal-dual interior point method
#     solving quadratic programming problems of the form
#
#       min        d^T alpha + 1/2 alpha^T H alpha
#       such that  A alpha = b
#                  0 <= alpha <= u

svm.sigmoid <- function(X, Y, degree=1, gamma=1, coef0=0, C=100) {

	m = nrow(X)

	# sigmoid: tanh(gamma*u'*v + coef0)
	Q = (Y%*%t(Y))*tanh(gamma*X%*%t(X) + coef0)
	d = rep(-1, m)
	A = matrix(Y, nrow=1)
	b = 0
	u = rep(C, m)

	model = LowRankQP(Vmat=Q,
			  dvec=d,
			  Amat=A,
			  bvec=b,
			  uvec=u,
			  method="CHOL")

	return (model)
}

svm.polynomial <- function(X, Y, degree=1, gamma=1, coef0=0, C=100) {

	m = nrow(X)

	# linear: u'*v
	# polynomial: (gamma*u'*v + coef0)^degree
	Q = (Y%*%t(Y))*(gamma*X%*%t(X) + coef0)^degree
	d = rep(-1, m)
	A = matrix(Y, nrow=1)
	b = 0
	u = rep(C, m)

	model = LowRankQP(Vmat=Q,
			  dvec=d,
			  Amat=A,
			  bvec=b,
			  uvec=u,
			  method="CHOL")

	return (model)
}

svm.gaussian <- function(X, Y, gamma=1, C=100) {

	m = nrow(X)

	# radial basis: exp(-gamma*|u-v|^2)

	ids = 1:nrow(X)
	X_Xt = outer(ids,ids, function(id1,id2) { 
		      exp(-gamma*rowSums((X[id1,] - X[id2,])^2)) } )

	Q = (Y%*%t(Y))*X_Xt

	d = rep(-1, m)
	A = matrix(Y, nrow=1)
	b = 0
	u = rep(C, m)

	model = LowRankQP(Vmat=Q,
			  dvec=d,
			  Amat=A,
			  bvec=b,
			  uvec=u,
			  method="CHOL")

	return (model)
}

############## SOME TESTS #################
test.linear <- function(degree=1, coef0=0, C=100) {

	X = cbind(rnorm(mean=0, sd=1, n=1000), rnorm(mean=0, sd=1, n=1000))
	X = rbind(X, cbind(rnorm(mean=10, sd=1, n=1000), rnorm(mean=10, sd=1, n=1000)))

	Y = c(rep(-1, 1000), rep(1, 1000))

	plot(X, col=Y+2)
	locator(1)

	model = svm.polynomial(X, Y, degree, coef0, C)

	ret = list()
	ret$model = model
	ret$X = X
	ret$Y = Y
	ret$alphas = model$alpha
	ret$b = model$beta

	return (ret)
}

classify <- function(ret, mat) {

	labels = matrix(0, nrow(mat))
	for (i in 1:nrow(mat)) {
		x_i = mat[i,]
		labels[i] = sum(ret$alphas*ret$Y*(ret$X%*%x_i)) + ret$b
	}

	return (labels)
}

hyperplane <- function(ret) {

	x = seq(min(ret$X), max(ret$X), len=100)
	resp = outer(x,x, function(x_i, x_j) {
		      		classify(ret, cbind(x_i, x_j))
			  })
	filled.contour(resp)
}



