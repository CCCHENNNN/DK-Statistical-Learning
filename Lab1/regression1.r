x <- c(1:20)
y <- c(2,4,8,16,32,64,120,241,433,741,1393,2154,3384,4487,6455,11287,14034,17555,22830,31559)
result <- data.frame(x, y)
myfit <- lm(y~I(x^7)+I(x^6)+I(x^5)+I(x^4)+I(x^3)+I(x^2)+x,data=result)
print(coefficients(myfit))
plot(result, col=2)
lines(x,fitted(myfit))