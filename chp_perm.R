
### Function chp.perm

chp.perm <-function(x,y,N_perm=100)
{
n=length(x)
first_k=10					
last_k=90					
alpha=0.05
Cohen_d=rep(NA,n)
simple.fit = lm(y~x)			
res=simple.fit$residuals		
yf=simple.fit$fitted			

### Finding the greatest value of the vecor Cohen_d
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
k_star=order(Cohen_d[first_k:last_k],decreasing = TRUE)[1]+ first_k-1
return(k_star)
}
### End ot the function



### Example 1 Minor normal noise
n=100
x=1:100
chp=50
y<-2+0.05*pmax(x-chp,0)+rnorm(n)/3	

chp.perm(x,y)

### Example 2 Najor normal noise
n=100
x=1:100
chp=50
y<-2+0.05*pmax(x-chp,0)+rnorm(n)	

chp.perm(x,y)

### Example 3 Dominant normal noise
n=100
x=1:100
chp=50
y<-2+0.05*pmax(x-chp,0)+5*rnorm(n)/3	

chp.perm(x,y)

### Example 4
xy=read.csv2('data.csv')
x=xy$x
y=xy$y

chp.perm(x,y)

### Example 5
xy=read.csv2('http://stat.ue.katowice.pl/data.csv')
x=xy$x
y=xy$y

chp.perm(x,y)


