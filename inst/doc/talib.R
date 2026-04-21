## ----include = FALSE----------------------------------------------------------
## set chart options
options(talib.chart.dark = FALSE)

knitr::opts_chunk$set(
	collapse = TRUE,
	comment = "#>",
	out.width = "100%",
	out.height = "680",
	fig.align = "center"
)

## -----------------------------------------------------------------------------
str(talib::BTC)

## ----error = TRUE-------------------------------------------------------------
try({
## rename columns to uppercase;
## this will fail
x <- talib::BTC
colnames(x) <- c("Open", "High", "Low", "Close", "Volume")

talib::RSI(x)
})

## -----------------------------------------------------------------------------
tail(
	talib::bollinger_bands(talib::BTC)
)

## -----------------------------------------------------------------------------
## data.frame in -> data.frame out
class(
	talib::RSI(talib::BTC)
)

## -----------------------------------------------------------------------------
## matrix in -> matrix out
class(
	talib::RSI(talib::SPY)
)

## -----------------------------------------------------------------------------
## numeric vector in -> numeric vector out
is.double(
	talib::RSI(talib::BTC$close)
)

## -----------------------------------------------------------------------------
## these are equivalent
identical(
	talib::relative_strength_index(talib::BTC, n = 14),
	talib::RSI(talib::BTC, n = 14)
)

## -----------------------------------------------------------------------------
## SMA with n = 5 has a lookback of 4
head(
	talib::SMA(talib::BTC, n = 5),
	n = 7
)

## -----------------------------------------------------------------------------
x <- talib::SMA(talib::BTC, n = 20)
attr(x, "lookback")

## -----------------------------------------------------------------------------
## RSI on 'high' instead of 'close'
tail(
	talib::RSI(talib::BTC, cols = ~high)
)

## -----------------------------------------------------------------------------
## Stochastic defaults to ~high + low + close;
## here we swap 'close' for 'open'
tail(
	talib::STOCH(
		talib::BTC,
		cols = ~high + low + open
	)
)

## -----------------------------------------------------------------------------
## Bollinger Bands on the first 100 rows only
tail(
	talib::BBANDS(
		talib::BTC,
		subset = 1:nrow(talib::BTC) %in% 1:100
	)
)

## -----------------------------------------------------------------------------
## inject some NAs
x <- talib::BTC
x$close[c(10, 50, 100)] <- NA

## -----------------------------------------------------------------------------
## default: NAs propagate
sum(is.na(
	talib::RSI(x)
))

## -----------------------------------------------------------------------------
## na.bridge = TRUE: NAs are skipped
sum(is.na(
	talib::RSI(x, na.bridge = TRUE)
))

## -----------------------------------------------------------------------------
nrow(talib::RSI(x, na.bridge = TRUE)) == nrow(x)

## -----------------------------------------------------------------------------
## SMA as a specification
str(
	talib::SMA(n = 20)
)

## -----------------------------------------------------------------------------
## Bollinger Bands with an EMA(20) middle band
tail(
	talib::bollinger_bands(
		talib::BTC,
		ma = talib::EMA(n = 20)
	)
)

## -----------------------------------------------------------------------------
## Stochastic with WMA smoothing
tail(
	talib::stochastic(
		talib::BTC,
		slowk = talib::WMA(n = 5),
		slowd = talib::EMA(n = 3)
	)
)

## -----------------------------------------------------------------------------
x <- talib::harami(talib::BTC)
tail(x)

## -----------------------------------------------------------------------------
## find all bullish occurrences
talib::BTC[which(x == 1), ]

## ----chart(basic)-------------------------------------------------------------
{
	## main candlestick chart
	talib::chart(talib::BTC)

	## add Bollinger Bands
	talib::indicator(talib::bollinger_bands)

	## add identified Harami patterns
	talib::indicator(talib::harami)

	## add MACD as a sub-chart
	talib::indicator(talib::MACD)
}

