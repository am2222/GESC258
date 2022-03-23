
factorial(6)

# Binomial 
p <- 10/200;
n <- 10;
# manual calculation
x <- 1
c_10_1 <- choose(n,x)
p_x <-  c_10_1 * (p^x)*(1-p)^(n-x)

# using R functions
dbinom(x,n,p)


#Bernoulli Trials
#Repeated trials of an experiment are called Bernoulli trials if the following three conditions are satisfied:
  
#  The experiment (each trial) has two possible outcomes, denoted s, for success, and f, for failure.
#The trials are independent.
#The probability of a success, called the success probability and denoted p, remains the same from trial to trial.

#P(X=3)
n <- 25 # number of students
p <- 0.3 # probability of success
k <- 3 # exactly 3 students will pass the exam

choose(n,k)*p^k*(1-p)^(n-k)

# P(X=15)
k <- 3
choose(n,k)*p^k*(1-p)^(n-k)

#For us it is of higher informational interest if we could answer the question what is the probability that k or less students (P(X???k)) will pass the exam, or equally interesting that k or more students (P(X???k)) will pass the exam.


1 - P(x<=9)
#visualize the binomial probability distribution 
size <- 25   # size
p <- 0.3     # probability of success
n <- 100000  # number of random samples

random.binom.numbers <- rbinom(n, size, p)

h <- hist(random.binom.numbers, 
          breaks = length(unique(random.binom.numbers)), 
          plot = FALSE)
plot(h, 
     freq = FALSE, 
     space = NULL,
     xlim = c(0,size),
     ylim= c(0,1),
     xlab = 'Students passing the final exam', # label for x-axis 
     ylab = "Probability", # label for y-axis 
     main = "Binomial Probability Distribution \nfor size=25 and p=0.3", # add title
     col = "#fc9d12", # add color "#fc9d12" 
     xaxt = "n") # do not show ticks of the x-axis

axis(side=2, at=seq(0.5,size, by=1), labels=seq(1,size, by=1)) # set ticks at defined positions for x axis
lines(1:n, dbinom(1:n,n,p), col="red", lwd=2)
#where is p(x=3) , p(x<=0) and p(x>9) and p(x=15)
n <- 25 # number of trials
y <- 1:25 # y <- c(1,2,3,4......) number of successes 
p <- 0.3
cumPx <- pbinom(y,n,p) #pbinom gives cummulative probability values. for y0 or y1 or y2 or
#data.frame(y,cumPx)
plot(y,cumPx, type="l",ylim = c(0,1), col="black", lwd=2, xlab="number of  students", ylab="cumulative probability",
     main="")
polygon(c(1:9, 9,1), c(cumPx[1:9], 0,0), col="red")
abline(h=cumPx[9],v=9)
#lines(1:9,cumPx[9])

##########################

n <- 25
x <- 1:n
p <- .3
px_3 <- dbinom(3,n,p)


Px <- dbinom(x,n,p)


par(mar=c(5,5,5,5), las=1)
plot.new()
plot.window(xlim=range(x), ylim=c(0, max(Px)+0.2))
barplot(Px,space=0, ylim=c(0, max(Px)+0.2),col="#fc9d12")
axis(side=1, at=seq(0.5,n, by=1), labels=seq(1,n, by=1)) # set ticks at defined positions for x axis
box()
lines(x=c(9,9),y=c(0,0.3), col="red",lwd=2,lty="dashed")
arrows(x0=9,y0=0.3,x1=0,y1=0.3, col = "red", lwd=2, lty="dashed")
text(4,0.31,"P(x<=9)")
#plot.window(xlim=range(x), ylim=c(0, max(Px)+0.2))
#barplot(Px[1:9],space=0,ylim=c(0, max(Px)+0.2),col=adjustcolor( "yellow", alpha.f = 0.2))
#add_bars(Px[1:9], seq(1,9,by=1))


cumPx <- pbinom(x,n,p) 
plot.window(xlim=c(0,n), ylim=c(0,1.5))
lines(x, cumPx, col="red", lwd=2)
axis(4,at=seq(0,1.5, by=0.2), labels=seq(0,1.5, by=0.2))

title(paste("Binomial distribution for n=",n,"and p=",p,sep=" "), adj=0)
mtext("Cumulative Probability", side = 4, las=3, line=3 )
mtext("Probability", side = 2, las=3, line=3)

mu <- n*p
abline(v=mu,lty = "dashed")
text(mu+2, 1.1, "mean")

sigma <- sqrt(n*p*(1-p))
sd1.pos <- mu + sigma 
sd1.neg <- mu - sigma 

abline(v=c(sd1.pos,sd1.neg), lty= "dashed", col="gray")
text(sd1.pos+2, 1.2, "+1 sd")
text(sd1.neg-2, 1.2, "-1 sd")

polygon(c(1:9, 9,1), c(cumPx[1:9], Px[1],Px[1]), col=adjustcolor( "green", alpha.f = 0.2))
text(7, 0.20, "P(x<=9)")
lines(x=9:n,y=rep(cumPx[9],length(9:n)), lwd=3, lty="dashed")
text(18, 0.9, "P(x<9)=0.8")


polygon(c(9:n, n,9), c(cumPx[9:n], Px[n],Px[1]), col=adjustcolor( "brown", alpha.f = 0.2))
text(18, 0.20, "P(x>9)")


sum(dbinom(1:9, n, p))
pbinom(9,n,p)
