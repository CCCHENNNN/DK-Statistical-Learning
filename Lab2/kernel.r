source("/Users/hchen/Desktop/DK/Term2/Statistical\ Learning/Lab2/svm.r")

sample_size = 2000

read_data <- function(){

	data = read.table("/Users/hchen/Desktop/DK/Term2/Statistical\ Learning/Lab2/covtype.data", sep = ",", header = FALSE)
	dataset =  as.matrix(data)
	return(dataset[1:sample_size,])

}

# classify the dataset to one label and other labels
get_single_dataset <- function(class, dataset){

	new_dataset = dataset
	col = ncol(dataset)
	for(i in 1 : nrow(new_dataset)) {
		if(new_dataset[i, col] != class) {
			new_dataset[i, col] = -1
		}
	}
	return(new_dataset)

}

# compute the accuracy
get_accuracy <- function(predicts, test_y) {

	row = nrow(predicts)
	hits = 0

	for(i in 1:row){
		if(predicts[i] > 0) {
			if(test_y[i] > 0){
				hits = hits + 1
			}
		} 
		else {
			if(test_y[i] < 0) {
				hits = hits + 1
			}
		}
	}
	return(hits / row)
}

get_model <- function(class, dataset){

	data = get_single_dataset(class, dataset)
	col = ncol(data)
	row = nrow(data)
	X = data[, 1:col - 1]
	y = as.matrix(data[, col])
	train_X = X[1:sample_size * 0.7, ]
	train_y = as.matrix(y[1:sample_size * 0.7, ])
	test_X = X[(sample_size * 0.7 + 1):nrow(X), ]
	test_y = as.matrix(y[(sample_size * 0.7 + 1):nrow(X), ])
	max_accuracy = 0
	model_final = list()
	degree_final = 0
	gamma_final = 0
	coef0_final = 0

	
	

	# Test for polynomial Kernel

	model_poly = NULL
	max_acc_poly = 0
	degree_poly = 0
	gamma_poly = 0
	coef0_poly = 0

	for(d in 1:4){
		for(g in seq(from = 0, to = 2, length.out = 10)) {
			for(c in seq(from = 0.01, to = 2, length.out = 10)) {
				model = svm.polynomial(train_X, train_y, degree = d, gamma = g, coef0 = c)
				ret = list()
				ret$model = model
				ret$X = train_X
				ret$Y = train_y
				ret$alphas = model$alpha
				ret$b = model$beta
				ret$order = d
				predicts = classify(ret, test_X)
				print(predicts)
				accuracy = get_accuracy(predicts,test_y)
				print(paste("Degree: ", as.character(d), "Gamma: ", as.character(g), "Coef0: ", as.character(c), sep = " "))
				print(paste("Accuracy: ", accuracy, sep = " "))
				# write(predicts, file = paste(as.character(d),as.character(g),as.character(c),sep = " "), sep = "\n")
				if(accuracy > max_acc_poly){
					max_acc_poly = accuracy
					degree_poly = d
					gamma_poly = g
					coef0_poly = c
					model_poly = model
				}
			}
		}
	}

	# Test for Gaussian Kernel

	model_gauss = NULL
	max_acc_gauss = 0
	gamma_gauss = 0
	for(g in seq(from = 0, to = 2, length.out = 10)) {
		model = svm.gaussian(train_X, train_y, gamma = g)
		ret = list()
		ret$model = model
		ret$X = train_X
		ret$Y = train_y
		ret$alphas = model$alpha
		ret$b = model$beta
		predicts = classify(ret, test_X)
		accuracy = get_accuracy(predicts,test_y)
		print(paste("Gamma: ", as.character(g),sep = " "))
		print(paste("Accuracy: ", accuracy, sep = " "))
		if(accuracy > max_acc_gauss) {
			max_acc_gauss = accuracy
			gamma_gauss = g
			model_gauss = model
		}
	}
	
	if(max_acc_poly > max_acc_gauss) {
		model_final = model_poly
		print("The best is Poly")
		print(paste("Degree: ", as.character(degree_poly), "Gamma: ", as.character(gamma_poly), "Coef0: ", as.character(coef0_poly), sep = " "))
		print(paste("The best accuracy: ", as.character(max_acc_poly), sep = " "))
	} else {
		model_final = model_gauss
		print("The best is Gaussian")
		print(paste("Gamma: ", as.character(gamma_gauss), sep = " "))
		print(paste("The best accuracy: ", as.character(max_acc_gauss), sep = " "))
	}
	
	return(best_model)

}

# test_model(1,get_single_dataset(1,read_data()))

main <- function(){
	dataset = read_data()
	for (class in 1:7){
		print(paste("The model for ", as.character(class), sep = " "))
		best_model = get_model(class, dataset)
	}
}

main()


