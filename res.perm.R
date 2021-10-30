
### Function res.perm
res.perm <- function(x,y,N_perm=100)
{
n=length(x)
first_k=10					
last_k=n-10					
Cohen_d=rep(NA,n)

simple.fit = lm(y~x)			
res=simple.fit$residuals		
yf=simple.fit$fitted			

if (N_perm < 100) stop("Too few permutations")
if (n < 50) stop("Too few observations")

### Finding the greatest value of the vector Cohen_d
for (k in first_k:last_k)
{
simple1.fit=lm(y[1:k]~x[1:k])
simple2.fit=lm(y[(k+1):n]~x[(k+1):n])
b1=simple1.fit$coefficients[2]		
b2=simple2.fit$coefficients[2]		
b1s=c(); b2s=c()

for (i in 1:N_perm)
{
	ys=yf+sample(res)
	b1s[i]=lm(ys[1:k]~x[1:k])$coefficients[2]
	b2s[i]=lm(ys[(k+1):n]~x[(k+1):n])$coefficients[2]
}
Cohen_d[k]=(b2-b1)/sqrt((n*var(b1s)+n*var(b2s))/(2*n-2))
}
d = max(Cohen_d[first_k:last_k], na.rm=T)
k_star=order(Cohen_d[first_k:last_k],decreasing=T)[1] + first_k-1
return(list("k_star" = k_star, "chp" = x[k_star], "d" = d))
}
### End of the function


### Example Sample data
xy=read.csv2('data.csv')
x=xy$x
y=xy$y

res.perm(x,y)

### Example The same sample data
xy=read.csv2('http://stat.ue.katowice.pl/data.csv')
x=xy$x
y=xy$y

res.perm(x,y)

