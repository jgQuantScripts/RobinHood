require("RobinHood")
require("httr") # for place_order fix

# create a connection with Robin Hood: disable Two-Factor Auth on App
RH = RobinHood(username = "username", password = "password")

# get user info
get_user(RH)

# Get account info (see api_accounts for a list of fields)
get_accounts(RH)

# Get summary of your current portfolio
get_portfolios(RH)

# Returns a data frame of stock ownership positions
get_positions(RH)
# ******************************************
#       Get instrument fundamentals
# ******************************************
get_fundamentals(RH, 'TSLA')

# Get quotes
get_quote(RH, symbol = c("AAPL", "AMZN"), limit_output = TRUE)
get_quote(RH, symbol = c("AAPL", "AMZN"), limit_output = FALSE)

# get Fundamentals
get_fundamentals(RH,ticker = "TSLA",include_description = TRUE)
get_fundamentals(RH,ticker = "TSLA",include_description = FALSE)

# Get crypto currency quotes (only one symbol at a time here)
get_quote_crypto(RH, symbol = "BTC")

# Get historical prices
get_historicals(RH, 'OEF', interval = 'hour', span = 'month', tz="America/Los_Angeles") 

# Every 30 minutes for the current day
get_historicals(RH, 'CAT', interval = '5minute', span = 'day')     # Every 5 minutes for the current day
get_historicals(RH, 'CAT', interval = 'day',      span = 'week')   # Every day for the current week
get_historicals(RH, 'CAT', interval = 'week',     span = '3month') # Every week for the last 3 months
get_historicals(RH, 'CAT', interval = 'month',    span = 'year')   # Every month for the current year

# Placing Orders
# Place Order, should generate an email
x = place_order(RH            = RH,
                symbol        = "AAPL",       # Ticker symbol you want to trade
                type          = "limit",      # Type of market order   (market or limit)
                time_in_force = "gfd",        # Time period the order is good for 
                # (gfd:good for day, gtc:good till canceled, ioc:immediate or cancel, opg: opening)
                trigger       = "immediate",  # Trigger or delay order (immediate or stop)
                price         =  100.00,      # The highest price you are willing to pay
                quantity      =  1      ,     # Number of shares you want | Must be a whole number >= 1
                side          = "buy")        # buy or sell 
# *******************************************************************************************************************
# temporary patch for fractional shares
# readMe: https://github.com/JestonBlu/RobinHood/issues/101
# *******************************************************************************************************************
stk_order = function (RH, symbol, type, time_in_force, trigger, price, stop_price = NA, 
                      quantity, side) 
{
  check_rh(RH)
  if (!type %in% c("market", "limit")) 
    stop("type must be 'market' or 'limit'")
  if (!time_in_force %in% c("gfd", "gtc", "ioc", "opg")) 
    stop(" time_in_fore must be one of 'gfd', 'gtc', 'ioc', 'opg'")
  if (!trigger %in% c("immediate", "stop")) 
    stop("trigger must be 'immediate' or 'stop'")
  if (trigger == "stop" & is.na(stop_price) == TRUE) 
    stop("stop price cant be null if trigger == 'stop'")
  if (!side %in% c("buy", "sell")) 
    stop("side must be 'buy' or 'sell'")
  if (is.na(stop_price) == TRUE) 
    stop_price <- ""
  quantity <- as.character(quantity)
  price <- as.character(price)
  instrument_url <- paste(api_endpoints(endpoint = "quotes"), 
                          symbol, sep = "")
  instrument <- api_quote(RH, instrument_url)
  instrument_id <- instrument$instrument
  orders <- api_orders(RH = RH, action = "order", instrument_id = instrument_id, 
                       symbol = symbol, type = type, time_in_force = time_in_force, 
                       trigger = trigger, price = price, stop_price = stop_price, 
                       quantity = quantity, side = side)
  return(orders)
}
api_endpoints <- function(endpoint, source = "equity") {
  
  api.endpoint <- list(
    # RobinHood endpoints
    url                = "https://api.robinhood.com/",
    accounts           = "accounts/",
    ach_transfers      = "ach/transfers/",
    ach_relationships  = "ach/relationships/",
    ach_schedules      = "ach/deposit_schedules/",
    forex              = "marketdata/forex/quotes/",
    fundamentals       = "fundamentals/?symbols=",
    historicals        = "quotes/historicals/",
    markets            = "markets/",
    marketdata_options = "marketdata/options/",
    options            = "options/",
    option_positions   = "options/positions/",
    option_orders      = "options/orders/",
    option_instruments = "options/instruments/",
    orders             = "orders/",
    portfolios         = "portfolios/",
    positions          = "positions/",
    quotes             = "quotes/?symbols=",
    tags               = "midlands/tags/tag/",
    instruments        = "instruments/",
    token              = "oauth2/token/",
    revoke_token       = "oauth2/revoke_token/",
    user               = "user/",
    watchlist          = "watchlists/",
    # Nummus endpoints
    url_nummus         = "https://nummus.robinhood.com/",
    accounts_crypto    = "accounts/",
    currency_pairs     = "currency_pairs/",
    holdings_crypto    = "holdings/",
    orders_crypto      = "orders/",
    portfolios_crypto  = "portfolios/"
  )
  
  x <- which(names(api.endpoint) == endpoint)
  
  if (source == "equity") {
    endpoint <- paste(api.endpoint$url, as.character(api.endpoint[x]), sep = "")
  }
  
  if (source == "crypto") {
    endpoint <- paste(api.endpoint$url_nummus, as.character(api.endpoint[x]), sep = "")
  }
  
  
  return(endpoint)
}
api_quote <- function(RH, symbols_url) {
  
  # URL and token
  url <- symbols_url
  token <- paste("Bearer", RH$tokens.access_token)
  
  # GET call
  dta <- GET(url,
             add_headers("Accept" = "application/json",
                         "Content-Type" = "application/json",
                         "Authorization" = token))
  
  # format return
  dta <- mod_json(dta, "fromJSON")
  dta <- as.data.frame(dta$results)
  
  # Check if api did not return any results
  if (nrow(dta) == 0) stop("Symbol not found")
  
  dta <- dta %>%
    dplyr::mutate_at(c("ask_price", "bid_price", "last_trade_price",
                       "last_extended_hours_trade_price",
                       "previous_close", "adjusted_previous_close"), as.numeric) %>%
    dplyr::mutate_at("previous_close_date", lubridate::ymd) %>%
    dplyr::mutate_at("updated_at", lubridate::ymd_hms)
  
  
  return(dta)
}
api_orders <- function(RH, action, status_url = NULL, cancel_url = NULL, instrument_id = NULL, symbol = NULL, type = NULL,
                       time_in_force = NULL, trigger = NULL, price = NULL, stop_price = NULL, quantity = NULL,
                       side = NULL, page_size = NULL) {
  
  
  if (action == "order") {
    
    url <- api_endpoints("orders")
    token <- paste("Bearer", RH$tokens.access_token)
    
    detail <- data.frame(account = RH$url.account_id,
                         instrument = instrument_id,
                         symbol = symbol,
                         type = type,
                         time_in_force = time_in_force,
                         trigger = trigger,
                         price = price,
                         stop_price = stop_price,
                         quantity = quantity,
                         side = side,
                         client_id = RH$api_client_id)
    
    # If trigger = "stop" then stop_price must be included, otherwise it must be excluded
    if (trigger == "immediate") {
      detail <- detail[, c("account", "instrument", "symbol", "type", "time_in_force",
                           "trigger", "price", "quantity", "side", "client_id")]
    }
    
    dta <- POST(url = url,
                add_headers("Accept" = "application/json",
                            "Content-Type" = "application/json",
                            "Authorization" = token),
                body = mod_json(detail, type = "toJSON"))
    
    dta <- mod_json(dta, "fromJSON")
    dta <- as.list(dta)
    
    # Rename URLs
    names(dta)[names(dta) %in% c("url", "cancel")] <- c("status_url", "cancel_url")
    
    dta$updated_at <-  lubridate::ymd_hms(dta$updated_at)
    dta$last_transaction_at <-  lubridate::ymd_hms(dta$last_transaction_at)
    dta$created_at <-  lubridate::ymd_hms(dta$created_at)
    dta$fees <- as.numeric(dta$fees)
    dta$cumulative_quantity <- as.numeric(dta$cumulative_quantity)
    dta$stop_price <- as.numeric(dta$stop_price)
    dta$reject_reason <- as.numeric(dta$reject_reason)
    dta$price <- as.numeric(dta$price)
    dta$average_price <- as.numeric(dta$average_price)
    dta$quantity <- as.numeric(dta$quantity)
    
    return(dta)
    
  }
  
  
  if (action == "status") {
    
    # Token
    token <- paste("Bearer", RH$tokens.access_token)
    
    # GET call
    dta <- GET(status_url,
               add_headers("Accept" = "application/json",
                           "Content-Type" = "application/json",
                           "Authorization" = token))
    
    # format return
    dta <- mod_json(dta, "fromJSON")
    dta <- as.list(dta)
    
    # Rename urls
    names(dta)[names(dta) %in% c("url", "cancel")] <- c("status_url", "cancel_url")
    
  }
  
  
  if (action == "cancel") {
    
    # Token
    token <- paste("Bearer", RH$tokens.access_token)
    
    # GET call
    dta <- POST(cancel_url,
                add_headers("Accept" = "application/json",
                            "Content-Type" = "application/json",
                            "Authorization" = token))
    
    # Format return
    dta <- mod_json(dta, "fromJSON")
    
  }
  
  
  if (action == "history") {
    
    url <- paste(api_endpoints("orders"), "?page_size=", page_size, sep = "")
    token <- paste("Bearer", RH$tokens.access_token)
    
    # GET call
    dta <- GET(url,
               add_headers("Accept" = "application/json",
                           "Content-Type" = "application/json",
                           "Authorization" = token))
    
    # format return
    dta <- mod_json(dta, "fromJSON")
    dta <- as.data.frame(dta$results)
    
  }
  
  return(dta)
  
  
}
# *******************************************************************************************************************
x = stk_order(RH              = RH,
              symbol        = "AAPL",       # Ticker symbol you want to trade
              type          = "market",      # Type of market order   (market or limit)
              time_in_force = "gfd",        # Time period the order is good for 
              # (gfd:good for day, gtc:good till canceled, ioc:immediate or cancel, opg: opening)
              trigger       = "immediate",  # Trigger or delay order (immediate or stop)
              price         =  100.00,      # The highest price you are willing to pay
              quantity      =  0.02     ,   # Number of shares you want 
              side          = "buy")        # buy or sell 
cancel_order(RH,cancel_url = x$cancel_url)
# *******************************************************************************************************************
# You can identify instruments by popular tags
# 100-most-popular | top-movers | upcoming-events | crypto | etf
get_tag(RH, tag = "100-most-popular")

get_market_hours(RH)

logout(RH)


## Watchlists
# Watchlist commands, currently creating and removing watchlists isn't working
watchlist(RH, action = 'get', watchlist = 'JSN', ticker="TSLA")
# [1] "AAPL" "TWTR" "TSLA" "NFLX" "FB"   "MSFT" "DIS"  "GPRO" ...
watchlist(RH, action = 'add', watchlist = 'Default', ticker = "CAT")
# "Instrument added to watchlist"
watchlist(RH, action = 'delete', watchlist = 'Default', ticker = 'CAT')
# "Instrument removed from watchlist"


