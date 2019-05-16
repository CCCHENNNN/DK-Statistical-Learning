
estimating.shattering.coefficient <- function(iter=1000, n.start=1, n.end=100, R=2, hyperplane=1) {

	Shattering = NULL

	cat("Sample size\tNumber of different classifications\n")

	for (n in n.start:n.end) {
		# Building up a data sample
		S = NULL
		for (i in 1:R) {
			# rnorm -> Normal distribution
			S = cbind(S, rnorm(mean=0, sd=1, n=n))
		}
		# print(S)

		# classify
		HashTable = list() # record the number of different classifications
		keys = NULL # vector to record the different results after calssification
		max.attempts.classify = n^2*iter # loop times
		for(j in 1:max.attempts.classify){
			key_value = 0
			for (k in 1:hyperplane) { # Because there is more than one hyperplane
									  # We need to combine the result of each hyperplane to get the final result
				hp = runif(min=-1, max=1, n=R+1)
				labels = sign(cbind(S,1) %*% hp)
				labels[labels%in%c(-1)]<-0 # change the -1 to 0 in order to get a binary label

				keyB = paste(labels, sep="#", collapse="")
				# change the binary key to decimal
				keyD = strtoi(keyB, base = 2)
				# 2^n is the upper value of keyD, so using this formula to record all key into a value
				key_value = key_value * (2^n) + keyD
			}

			HashTable[[as.character(key_value)]] = 1

		}

		cat(n, "\t\t", length(HashTable), "\n")
		Shattering = rbind(Shattering, c(n, length(HashTable)))
	}
	# print(Shattering)
	return (Shattering)
}

main <- function(){
	input.dim = 7
	hidden1.hp = 9
	hidden2.hp = 5
	out.hp = 3
	cat("Hidden1 Layer:\n")
	Shattering1 = estimating.shattering.coefficient(R=input.dim, hyperplane=hidden1.hp)
	cat("Hidden2 Layer:\n")
	Shattering2 = estimating.shattering.coefficient(R=hidden1.hp, hyperplane=hidden2.hp)
	cat("Output Layer:\n")
	Shattering3 = estimating.shattering.coefficient(R=hidden2.hp, hyperplane=out.hp)
}



estimating.shattering.coefficient(R=5, hyperplane=1)