## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
	collapse = TRUE,
	message = FALSE,
	warning = FALSE,
	comment = "#>",
	out.width = "100%",
	out.height = "520",
	fig.align = "center"
)

## ----echo = FALSE-------------------------------------------------------------
# two example candles
df <- data.frame(
	date = as.Date(c("2021-01-01", "2021-01-02")),
	open = c(30000, 31500),
	high = c(32000, 31800),
	low = c(29500, 30500),
	close = c(31500, 30800)
)

mid <- (df$close + df$open) / 2


annos <- list(
	list(
		x = df$date[1],
		y = mid[1],
		text = "Candle body (Bullish)",
		showarrow = FALSE,
		ax = 0,
		ay = -40,
		arrowhead = 2,
		arrowcolor = "#65a479",
		font = list(size = 12)
	),
	list(
		x = df$date[1],
		y = df$high[1],
		text = "Upper shadow",
		showarrow = TRUE,
		ax = 0,
		ay = -40,
		arrowhead = 2,
		arrowcolor = "gray40",
		font = list(size = 12)
	),
	list(
		x = df$date[1],
		y = df$low[1],
		text = "Lower shadow",
		showarrow = TRUE,
		ax = 0,
		ay = 40,
		arrowhead = 2,
		arrowcolor = "gray40",
		font = list(size = 12)
	),

	list(
		x = df$date[2],
		y = mid[2],
		text = "Candle body (Bearish)",
		showarrow = FALSE,
		ax = 0,
		ay = 40,
		arrowhead = 2,
		arrowcolor = "#d5695d",
		font = list(size = 12)
	),
	list(
		x = df$date[2],
		y = df$high[2],
		text = "Upper shadow",
		showarrow = TRUE,
		ax = 0,
		ay = -40,
		arrowhead = 2,
		arrowcolor = "gray40",
		font = list(size = 12)
	),
	list(
		x = df$date[2],
		y = df$low[2],
		text = "Lower shadow",
		showarrow = TRUE,
		ax = 0,
		ay = 40,
		arrowhead = 2,
		arrowcolor = "gray40",
		font = list(size = 12)
	)
)


df |>
	plotly::plot_ly(
		x = ~date,
		open = ~open,
		high = ~high,
		low = ~low,
		close = ~close,
		type = "candlestick",
		increasing = list(
			line = list(color = "#65a479"),
			fillcolor = "#65a479"
		),
		decreasing = list(
			line = list(color = "#d5695d"),
			fillcolor = "#d5695d"
		)
	) |>
	plotly::layout(
		title = "",
		xaxis = list(
			title = "", rangeslider = list(visible = FALSE), showticklabels = FALSE, showgrid = FALSE),
		yaxis = list(
			title = "", showticklabels = FALSE, showgrid = FALSE),
		annotations = annos
	)

## -----------------------------------------------------------------------------
x <- talib::doji(talib::BTC)
table(x)

## -----------------------------------------------------------------------------
options(talib.normalize = FALSE)
table(talib::engulfing(talib::BTC))

## ----include = FALSE----------------------------------------------------------
## restore default
options(talib.normalize = TRUE)

## -----------------------------------------------------------------------------
## Evening Star with 30% penetration
x <- talib::evening_star(talib::BTC, eps = 0.3)
sum(abs(x), na.rm = TRUE)

## -----------------------------------------------------------------------------
x <- talib::doji(talib::BTC)
attr(x, "lookback")

## -----------------------------------------------------------------------------
## default N = 10
sum(abs(talib::doji(talib::BTC)), na.rm = TRUE)

## -----------------------------------------------------------------------------
## shorter lookback
options(talib.BodyDoji.N = 3)
sum(abs(talib::doji(talib::BTC)), na.rm = TRUE)

## ----include = FALSE----------------------------------------------------------
options(talib.BodyDoji.N = 10)

## -----------------------------------------------------------------------------
## default alpha = 0.1
sum(abs(talib::doji(talib::BTC)), na.rm = TRUE)

## -----------------------------------------------------------------------------
## more permissive: accept bodies up to 20% of the high-low range
options(talib.BodyDoji.alpha = 0.2)
sum(abs(talib::doji(talib::BTC)), na.rm = TRUE)

## ----include = FALSE----------------------------------------------------------
## restore default
options(talib.BodyDoji.alpha = 0.1)

## -----------------------------------------------------------------------------
{
	talib::chart(talib::BTC)
	talib::indicator(talib::doji)
	talib::indicator(talib::engulfing)
}

