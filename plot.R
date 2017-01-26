#source('./latin_R/latin_data_read.R',chdir=TRUE,local=TRUE)
#source('./latin_R/latin_data_plot.R',chdir=TRUE,local=TRUE)
#library('ggplot2')

group = 1
gene = 1
probe = 11

fit = fitd(group,gene,probe,pips)
data = data.frame(fit$dat)

p = ggplot(data,aes(x=vlog2t,y=vlog2d))+geom_point(size=5,pch=1)
p = p + labs(title=expression("log2(Intensity) v. log2(Concentration)"))
p = p + labs(x="log2(Concentration)",y="Log2(Intensity)")
p = p +  theme(text = element_text(size=20),
               axis.text.x = element_text(size=20))
p = p + xlim(-15,15) + ylim(5,15)
sig=function(x){sapply(x,function(b){sigmoid(b,fit$theta)})}

min = quantile(data$vlog2d,.05)
max = quantile(data$vlog2d,.95)
slope=diff(quantile(data$vlog2d,c(.05,.95)))/8

pw = function(x){
    return(min+slope*x)
}

vpw = function(x){sapply(x,pw)}
s = seq(-15,10,.01)
s = s + runif(length(s))

df = data.frame(as.matrix(t(rbind(s,vpw(s)))))

p = p+geom_line(data=df,aes(x=s,y=V2),color='blue')
p = p + stat_function(fun=sig,color='red')
pdf("plot3.pdf")
p
dev.off()
