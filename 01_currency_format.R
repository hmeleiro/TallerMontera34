currency_format <- function(symbol_currency = "$", symbol_position = "before", symbol_spacing = "none", separator_thousand = ",", separator_thousand_interval = 3, separator_decimal = ".", separator_decimal_interval = 3, largest_with_cents = 100000, nsmall = 0L, trim = TRUE, scientific = FALSE, digits = 1L, drop0trailing = TRUE, currency_unit = "", negative_parentheses = FALSE) {
  function(x) {
    # format numeric axis labels
    x <- plyr::round_any(x, 0.01)
    if (max(x, na.rm = TRUE) < largest_with_cents & 
        !all(x == floor(x), na.rm = TRUE)) {
      nsmall <- 2L
    } else {
      x <- plyr::round_any(x, 1)
      nsmall <- 0L
    }
    labels_format <- format(x, nsmall = nsmall, trim = trim, scientific = scientific, digits = digits, drop0trailing = drop0trailing, big.mark = separator_thousand, big.interval = separator_thousand_interval, decimal.mark = separator_decimal, small.interval = separator_decimal_interval)
    # add currency symbol to labels and position according to style
    if (symbol_spacing == "none" & symbol_position == "after")
      labels <- paste0(labels_format, symbol_currency)
    if (symbol_spacing == "single" & symbol_position == "before")
      labels <- paste0(symbol_currency, " ", labels_format)
    if (symbol_spacing == "single" & symbol_position == "after")
      labels <- paste0(labels_format, " ", symbol_currency)
    if (symbol_spacing == "none" & symbol_position == "before")
      labels <- paste0(symbol_currency, labels_format)
    # millions
    if (currency_unit == "million_us")  # overrules label/symbol positions
      labels <- paste0(symbol_currency, labels_format, "M")
    if (currency_unit == "million_uk")  # overrules label/symbol positions
      labels <- paste0(symbol_currency, labels_format, "m")
    if (currency_unit == "million_french")  # overrules label/symbol positions
      labels <- paste0(labels_format, " Mio ", symbol_currency)
    # billions
    if (currency_unit == "billion_us")  # overrules label/symbol positions
      labels <- paste0(symbol_currency, labels_format, "B")
    if (currency_unit == "billion_uk")  # overrules label/symbol positions
      labels <- paste0(symbol_currency, labels_format, "bn")
    if (currency_unit == "billion_french")  # overrules label/symbol positions
      labels <- paste0(labels_format, " Mrd ", symbol_currency)
    
    return(labels)
  }
}