
#Q1
answer <- c(4,7,9,4,5,0,4,15,9,9,10,3,7,14,16,7,12,7,11,7,15,2,9,3,1,6,14,11,3,1,2,7,8,6,1,6) 
dpois(x = 15, lambda = mean(answer))


#Q2
dpois(x = 3, lambda = mean(answer)) +
dpois(x = 4, lambda = mean(answer)) +
dpois(x = 5, lambda = mean(answer))


#Q4
dpois(x = answer, lambda = mean(answer))
