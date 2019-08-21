#' @title implied vol via Black76
#' @description compute implied vol and plot
#' @details this function is used for calculating the implied vol for European options
#' using the balck76 model and plot the figure of implied vol under diverse strike
#' prices for call options vs put options.
#' @param df_raw, is a dataframe with the same formats with the sample df1.
#' @export
#' @examples
#' plotImpliedVol(df1_sample)
#' plotImpliedVol()



# black76, https://en.wikipedia.org/wiki/Black_model
# Sample data.frame
df1_sample = data.frame(strike = c(50,20),                  # the option strike - in $
                type = c("C", "P"),                 # either "c" for call option or "p" for a put option
                optionPrice = c(1.62, 0.01),        # the option price - in $
                futurePrice = c(48.03, 48.03),      # the price of the underlying future - in $
                time_to_expiry = c(0.1423, 0.1423)) # the option time to expiry - in year



plotImpliedVol <- function (df_raw = df1_sample){
  require(ggplot2)
  require(quantmod)
  t10yr <- getSymbols(Symbols = "DGS10", src = "FRED", auto.assign = FALSE)
  r= as.numeric(t10yr['2019-08-19'])/100
  call_fun <- function(type, F, K, r, TT, sig) {
    d1 <- (log(F/K) + (sig^2/2)*TT) / (sig*sqrt(TT))
    d2 <- d1 - sig*sqrt(TT)
    if (type == 'C')
      exp(-r*TT)*(F*pnorm(d1) - K*pnorm(d2))
    else
      exp(-r*TT)*(K*pnorm(-d2) - F*pnorm(-d1))
  }

  sig_impl <- function(df1) {
    type = df1['type']
    F = as.numeric(df1 ['futurePrice'])
    K = as.numeric(df1 ['strike'])
    TT = as.numeric(df1['time_to_expiry'])
    .c = as.numeric(df1['optionPrice'])
    root_fun <- function(sig){
      call_fun(type, F, K, r, TT, sig) - .c
    }
    uniroot(root_fun, c(0, 1), extendInt = 'yes')$root
  }

  df1 = df_raw
  df1$vol <- apply(df1[,,drop=F], 1, sig_impl)
  print(df1)
  ggplot(df1, aes(x=strike, y=vol, colour=type)) +
  geom_point()+ ggtitle('Implied Vol of Call vs Put option under Black76') +
  theme(plot.title = element_text(hjust = 0.5))


}
