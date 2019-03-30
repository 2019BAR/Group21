head(faithful)
D = faithful$eruptions  # copy to a short name
summary(D)
hist(D)
# Frame
par(cex=0.7)#控制字體大小
plot(0,0,xlim=c(1.5,5.25),ylim=c(0,1.1),xlab="噴發時間(分鐘)", 
     ylab="密度 or (累計)機率", main="分布、機率與密度")
abline(h=1, col='lightgray', lwd=0.25, lty=2)

# Empirical PDF
rug(D) 
# Empirical CDF
plot(ecdf(D), cex=0, verticals=T, lwd=2, col='darkgray', add=T)# add不希望第一張圖直接換第二章，而是加上去

# Histogram PDF  (直方圖算是RUG的超級簡化)
Bins = 20       #指定欄位數                      # no. bins
bx = seq(min(D), max(D), length=Bins+1) # break sequence bin+1因為bin只欄寬會少一根件頭 
hist(D, col="#B3FFFF7F", border="white", ylim=c(0,1.1),
     freq=F, breaks=bx, add=T)#我們想要density不要feq
abline(h=0, col='lightgray', lwd=0.25)
# Histogram CDF
adj = (bx[2] - bx[1])/2
steps = stepfun(bx-adj, c(0, sapply(bx, function(b) mean(D <= b))))
plot(steps, cex=0, col='#33CC337F', lwd=3, lty=1, add=T)

# Smooth PDF  (PDF通常使用這個做為圖表)
Adjust = 1    # bandwidth adjustment
DEN = density(D, adjust = Adjust)          #連續變數:頻寬越小圖形越複雜(bandwidth)
                                           #而類別變數如Hist()則是欄寬越大越複雜(binwidth) 欄數=support/binwidth
lines(DEN, col='gold', lwd=3)
# Smooth CDF
PDF = approxfun(DEN$x, DEN$y, yleft=0, yright=0)
x = seq(1,6,0.1)
y = sapply(x, function(i) integrate(PDF, -Inf, i)$value)
lines(x, y, col='red', lwd=3, lty=2) 

# Mark Range
x1 = 3.8; x2 = 4.8
rect(x1,-0.1,x2,1.2,col= rgb(0,1,0,alpha=0.2),border=NA)
# x = seq(x1, x2, length=100)
# polygon(c(x, x2, x1),  c(PDF(x), 0, 0), col="#FF99003F", border=NA)
# Calculate Probability
(integrate(PDF, x1, x2)$value)

###############################
rbinom(1000000, 1000, 0.2) %>% hist(breaks=100,freq=F)
curve(dnorm(x,mean=200,sd=sqrt(160)), col='red', lwd=2, add=T)


