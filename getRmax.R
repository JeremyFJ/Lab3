#' get \eqn{R_max} from rebound potential from method 4 of \emph{Cortes.etal.2016}
#' 
#' the function calculates \eqn{R_max} from method 4 of \emph{Cortes.etal.2016} or from \emph{Pardo.etal.2016}. It uses different inputs from life tables.
#' @param method it can be "Smith.etal.1998" or "Pardo.etal.2016"
#' @param mortality it can be given or calculated from the other arguments according to \emph{Dulvy.etal.2004} or \emph{Then.etal.2014}'s methods.
#' @param fec fecundity
#' @param litter litter size
#' @param cycle number of years between successive litters
#' @param lifespan in number of years
#' @param agemat age at maturity
#' @export
getRmax= function(method = "Smith.etal.1998", mortality = "Then.etal.2014", fec = NULL, litter = NULL, cycle = NULL, lifespan, agemat){


	# source("~/Baselines/R/lifehist.R")
	if (mortality== "Dulvy.etal.2004") M = FraUtils::getMDulvy.etal.2004(lifespan = lifespan, agemat = agemat)
	if (mortality== "Then.etal.2014") M = FraUtils::getMtmax(lifespan = lifespan)
	if (is.numeric(mortality)) M = mortality

	
	if(method=="Smith.etal.1998"){
		tmax = lifespan
		tmat = agemat
		if (is.null(fec)) fec = litter/2/cycle else fec = fec
		Z<-1.5*M
	
		#this is equation 7 in the manuscript
		l_alpha<-(1-exp(-(Z)))/((fec)*(1-exp(-(Z*(tmax-tmat+1)))))

		#this is equation 6 in the manuscript
		eq6 <- function(reb) 1-exp(-(M+reb))-l_alpha*(fec)*1.25*exp(-reb*tmat)*(1-exp(-(M+reb)*(tmax-tmat+1)))
		r_Z<-uniroot(eq6, c(0,5), tol=1e-8)
		return(r_Z$root*2)
	} 
	
	if(method=="Pardo.etal.2016"){
		p = exp(-M) # annual survival rate 
		la = exp(-M)^agemat # proportion of individuals surviving to maturity
		b = litter/2/cycle
		if (is.null(fec)) b = litter/2/cycle else b = fec
		#browser()
		eq <- function(rmax) exp(rmax*agemat) - p*exp(rmax)^(agemat-1) - la*b
		r_max = try(uniroot(eq, c(0,5), tol = 1e-8))
		return(as.numeric(try(r_max$root)))
	}
}


