rfrate = 0.05        						 #expected risk-free rate
t = 1/52           						     #time interval in years
initprice = 53.39     						 #S0
strike_price = 49.5   						 #strike price of the call option
vol = 0.479            						 #volatility
adjusted_SP = strike_price*exp(-rfrate*t)    #discounted strike price

m = 100                  #number of paths
i = 10                   #number of steps in each path
delta_t = t/i            #length of delta t
futureprice <- rep(0,m)  #vector of future prices (to be filled)
payoff <- rep(0,m)       #vector of payoffs (to be filled)


k = 1                    #counting variable
while(k<=m){
	normarray = rnorm(i,0,1)   #vector of standard normal variables
	wiener_seq <- rep(0,i)     #vector of wiener elements (to be filled)
	
	j = 1                      #counting variable
	while(j<=i){
		wiener_seq[j] = sqrt(delta_t) * normarray[j] #sequence of wiener elements
		j = j+1
	}
	wiener_series = sum(wiener_seq)     #sum of wiener elements
	futureprice[k] = initprice * exp(-0.5*(vol^2)*t + vol* wiener_series) #future price
	
	if(futureprice[k]<adjusted_SP){    #if statement of the call option
		payoff[k]=0
	}else{
		payoff[k]= futureprice[k] - adjusted_SP
	}
	k = k+1
}

monte_carlo_price <- sum(payoff)/m #option price as average of prices obtained from different paths
monte_carlo_price
