library(FSA)
library(FSAdata)
library(rfishbase)

# Q1
## Herring
lm1 = lm(log(HerringBWE$ssb)~log(HerringBWE$spawning.year))
ggplot() + geom_point(data=HerringBWE, aes(x=log(spawning.year), y=log(ssb))) + geom_line(data=HerringBWE, aes(x=log(spawning.year), y=lm1$fitted.values))
plot(lm1)

## Black Drum
BlackDrum2001 = na.omit(BlackDrum2001)
lm2 = lm(log(BlackDrum2001$weight)~log(BlackDrum2001$tl))
model_bd = nls(weight~a*tl^b, start=list(a=160,b=3), data=BlackDrum2001)
coef(model_bd)
p1 = ggplot() + geom_point(data=BlackDrum2001, aes(x=tl, y=weight)) + geom_line(data=BlackDrum2001, aes(x=tl, y=predict(model_bd)))
p2 = ggplot() + geom_point(data=BlackDrum2001, aes(x=log(tl), y=log(weight))) + geom_line(data=BlackDrum2001, aes(x=log(tl), y=lm2$fitted.values))
plot(model_bd)
gridExtra::grid.arrange(p1, p2)

## Bonito
vb = vbFuns(param="Typical")
Bonito_f = subset(Bonito, sex=="Female")
Bonito_m = subset(Bonito, sex=="Male")
f.starts_f <- vbStarts(fl~age,data=Bonito_f) 
f.fit_f <- nls(fl~vb(age,Linf,K,t0),data=Bonito_f,start=f.starts_f) 
f.starts_m <- vbStarts(fl~age,data=Bonito_m) 
f.fit_m <- nls(fl~vb(age,Linf,K,t0),data=Bonito_m,start=f.starts_m) 
ggplot() + 
  geom_point(data=Bonito, aes(x=age, y=fl, color=sex), pch=20) + 
  geom_line(data=Bonito_f, aes(x=age, y=predict(f.fit_f)), color="red") +
  geom_line(data=Bonito_m, aes(x=age, y=predict(f.fit_m)), color="blue") +
  scale_color_manual(values=c("red", "grey", "blue"))

params = coef(f.fit_f)
Linf = params[1]
K = params[2]
t0 = params[3]
La = Linf*(1-exp(-K*(1.5-t0)))
La[[1]]




