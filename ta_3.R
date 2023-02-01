library(FSA)
library(FSAdata)
library(rfishbase)

# Q1
## Herring
lm1 = lm(log(HerringBWE$ssb)~log(HerringBWE$spawning.year))
ggplot() + geom_point(data=HerringBWE, aes(x=log(spawning.year), y=log(ssb))) + geom_line(data=HerringBWE, aes(x=log(spawning.year), y=lm1$fitted.values))
plot(lm1)

## Bonito
vb = vbFuns(param="Typical")
f.starts <- vbStarts(fl~age,data=Bonito) # vbStarts is an optimization algorithm designed to find the best params
f.fit <- nls(fl~vb(age,Linf,K,t0),data=Bonito,start=f.starts)
coef(f.fit)
ggplot() + geom_point(data=Bonito, aes(x=age, y=fl, color=sex)) + geom_line(data=Bonito, aes(x=age, y=predict(f.fit)))
plot(f.fit)

params = coef(f.fit)
Linf = params[1]
K = params[2]
t0 = params[3]
La = Linf*(1-exp(-K*(1.5-t0)))
La[[1]]


## Black Drum
BlackDrum2001 = na.omit(BlackDrum2001)
lm2 = lm(log(BlackDrum2001$weight)~log(BlackDrum2001$tl))
model_bd = nls(weight~a*tl^b, start=list(a=160,b=3), data=BlackDrum2001)
coef(model_bd)
p1 = ggplot() + geom_point(data=BlackDrum2001, aes(x=tl, y=weight)) + geom_line(data=BlackDrum2001, aes(x=tl, y=predict(model_bd)))
p2 = ggplot() + geom_point(data=BlackDrum2001, aes(x=log(tl), y=log(weight))) + geom_line(data=BlackDrum2001, aes(x=log(tl), y=lm2$fitted.values))
plot(model_bd)
gridExtra::grid.arrange(p1, p2)
