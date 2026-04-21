## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse      = TRUE,
  comment       = "#>",
  out.width     = "100%",
  out.height    = "680",
  fig.align     = "center"
)

## -----------------------------------------------------------------------------
talib::chart(
  x = talib::NVDA
)

## -----------------------------------------------------------------------------
talib::chart(
  x = talib::BTC,
  title = "Bitcoin (USDC)"
)

## -----------------------------------------------------------------------------
talib::chart(
  x = talib::BTC,
  type = "ohlc"
)

## -----------------------------------------------------------------------------
{
  talib::chart(talib::BTC)
  talib::indicator(talib::SMA, n = 7)
  talib::indicator(talib::SMA, n = 14)
  talib::indicator(talib::RSI)
}

## -----------------------------------------------------------------------------
{
  talib::chart(talib::BTC)
  talib::indicator(talib::BBANDS)
  talib::indicator(talib::MACD)
  talib::indicator(talib::trading_volume)
}

## -----------------------------------------------------------------------------
{
  talib::chart(talib::BTC)
  talib::indicator(
    talib::RSI(n = 10),
    talib::RSI(n = 14),
    talib::RSI(n = 21)
  )
}

## -----------------------------------------------------------------------------
{
  talib::chart(talib::BTC)
  talib::indicator(
    talib::RSI(n = 14),
    talib::MACD()
  )
}

## -----------------------------------------------------------------------------
{
  talib::chart(talib::BTC)
  talib::indicator(talib::BBANDS)
  talib::indicator(
    talib::RSI(n = 10),
    talib::RSI(n = 14),
    talib::RSI(n = 21)
  )
  talib::indicator(talib::MACD)
}

## -----------------------------------------------------------------------------
{
  ## clear any existing chart
  talib::chart()

  ## plot MACD by itself
  talib::indicator(
    FUN  = talib::MACD,
    data = talib::BTC
  )
}

## -----------------------------------------------------------------------------
{
  ## first chart
  talib::chart(talib::NVDA)
  talib::indicator(talib::RSI)
}

## -----------------------------------------------------------------------------
{
  ## reset and start fresh
  talib::chart()

  talib::chart(talib::BTC)
  talib::indicator(talib::MACD)
}

## -----------------------------------------------------------------------------
{
  talib::chart(
    x   = talib::BTC,
    idx = rownames(talib::BTC)
  )
}

## -----------------------------------------------------------------------------
{
  talib::chart(
    x   = talib::BTC,
    idx = rownames(talib::BTC)
  )

  talib::indicator(
    talib::BBANDS,
    subset = 1:nrow(talib::BTC) %in% 50:100
  )

  talib::indicator(
    talib::ACCBANDS,
    subset = 1:nrow(talib::BTC) %in% 101:151
  )
}

## ----eval = FALSE-------------------------------------------------------------
# ## equivalent ways to set a theme
# talib::set_theme$payout
# talib::set_theme("payout")
# 
# ## list available theme names
# talib::set_theme()

## -----------------------------------------------------------------------------
{
  talib::chart(talib::BTC)
  talib::indicator(talib::SMA, n = 7)
  talib::indicator(talib::SMA, n = 14)
  talib::indicator(talib::SMA, n = 21)
  talib::indicator(talib::SMA, n = 28)
  talib::indicator(talib::MACD)
  talib::indicator(talib::trading_volume)
}

## -----------------------------------------------------------------------------
{
  talib::set_theme$hawks_and_doves
  talib::chart(talib::BTC)
  talib::indicator(talib::SMA, n = 7)
  talib::indicator(talib::SMA, n = 14)
  talib::indicator(talib::SMA, n = 21)
  talib::indicator(talib::SMA, n = 28)
  talib::indicator(talib::MACD)
  talib::indicator(talib::trading_volume)
}

## -----------------------------------------------------------------------------
{
  talib::set_theme$payout
  talib::chart(talib::BTC)
  talib::indicator(talib::SMA, n = 7)
  talib::indicator(talib::SMA, n = 14)
  talib::indicator(talib::SMA, n = 21)
  talib::indicator(talib::SMA, n = 28)
  talib::indicator(talib::MACD)
  talib::indicator(talib::trading_volume)
}

## -----------------------------------------------------------------------------
{
  talib::set_theme$tp_slapped
  talib::chart(talib::BTC)
  talib::indicator(talib::SMA, n = 7)
  talib::indicator(talib::SMA, n = 14)
  talib::indicator(talib::SMA, n = 21)
  talib::indicator(talib::SMA, n = 28)
  talib::indicator(talib::MACD)
  talib::indicator(talib::trading_volume)
}

## -----------------------------------------------------------------------------
{
  talib::set_theme$trust_the_process
  talib::chart(talib::BTC)
  talib::indicator(talib::SMA, n = 7)
  talib::indicator(talib::SMA, n = 14)
  talib::indicator(talib::SMA, n = 21)
  talib::indicator(talib::SMA, n = 28)
  talib::indicator(talib::MACD)
  talib::indicator(talib::trading_volume)
}

## -----------------------------------------------------------------------------
{
  talib::set_theme$bloomberg_terminal
  talib::chart(talib::BTC)
  talib::indicator(talib::SMA, n = 7)
  talib::indicator(talib::SMA, n = 14)
  talib::indicator(talib::SMA, n = 21)
  talib::indicator(talib::SMA, n = 28)
  talib::indicator(talib::MACD)
  talib::indicator(talib::trading_volume)
}

## -----------------------------------------------------------------------------
{
  talib::set_theme$limit_up
  talib::chart(talib::BTC)
  talib::indicator(talib::SMA, n = 7)
  talib::indicator(talib::SMA, n = 14)
  talib::indicator(talib::SMA, n = 21)
  talib::indicator(talib::SMA, n = 28)
  talib::indicator(talib::MACD)
  talib::indicator(talib::trading_volume)
}

## -----------------------------------------------------------------------------
{
  talib::set_theme$bid_n_ask
  talib::chart(talib::BTC)
  talib::indicator(talib::SMA, n = 7)
  talib::indicator(talib::SMA, n = 14)
  talib::indicator(talib::SMA, n = 21)
  talib::indicator(talib::SMA, n = 28)
  talib::indicator(talib::MACD)
  talib::indicator(talib::trading_volume)
}

## -----------------------------------------------------------------------------
{
  talib::set_theme(
    "payout",
    background_color = "#000000",
    bullish_body     = "#00FF00",
    bearish_body     = "#FF0000"
  )
  talib::chart(talib::BTC)
  talib::indicator(talib::RSI)
}

## -----------------------------------------------------------------------------
{
  options(
    talib.chart.slider = TRUE,
    talib.chart.scale  = 0.8,
    talib.chart.main   = 0.6
  )

  talib::chart(talib::SPY)
  talib::indicator(talib::RSI)
  talib::indicator(talib::BBANDS)
}

## ----include = FALSE----------------------------------------------------------
## reset options to defaults
options(
  talib.chart.slider      = FALSE,
  talib.chart.scale       = 1,
  talib.chart.main        = 0.7,
  talib.chart.backend     = "plotly"
)

## ----fig.width = 8, fig.height = 6--------------------------------------------
{
  options(talib.chart.backend = "ggplot2")
  talib::set_theme$hawks_and_doves

  talib::chart(talib::BTC)
  talib::indicator(talib::SMA, n = 14)
  talib::indicator(talib::RSI)
}

## ----include = FALSE----------------------------------------------------------
options(talib.chart.backend = "plotly")

