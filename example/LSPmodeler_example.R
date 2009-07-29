require(LSPmodeler)
require(quantmod)

## two simultanious coin toss example

# build the joint probability table by hand
probs <- rep(0.25,times=4)
plays <- rbind(c(2,2),c(2,-1),c(-1,-1),c(-1,2))

jpt.twocoins <- as.data.frame(cbind(probs,plays))
names(jpt.twocoins) <- c("probability","coin1","coin2")

# maximize geometric mean hpr after 5 tosses
# accept a 20% probability of a 20% drawdown
#lsp.optimize(jpt.twocoins,max.probprofit=FALSE,horizon=5,use.drawdown=TRUE,acceptable.percent=0.2,b=0.8,DEcontrol=list(NP=20))

## real world example

# get data
getSymbols(c("SPY","IWM"),from="2001-01-01",to="2008-12-31")

# generate raw monthly changes for a unit size of 100 shares (excluding transaction cost and slippage)
spy.m <- (Cl(to.monthly(SPY))-Op(to.monthly(SPY)))*100
iwm.m <- (Cl(to.monthly(IWM))-Op(to.monthly(IWM)))*100

# quantize data into 10 buckets each, midpoints of the buckets are used
# as the outcome for the joint probability table
spy.binned <- lsp.component(spy.m,10)
iwm.binned <- lsp.component(iwm.m,10)

p <- lsp.portfolio(list(spy=spy.binned,iwm=iwm.binned))

# get joint probability table
jpt.real <- lsp.jptable(p)

# maximize geometric mean hpr after 4 months
# accept a 20% probability of a 20% drawdown
lsp.optimize(jpt.real,horizon=4,max.probprofit=FALSE,use.drawdown=TRUE,acceptable.percent=0.5,b=0.8,DEcontrol=list(NP=40,CR=0,F=0.6))




