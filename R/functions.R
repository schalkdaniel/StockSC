# =============================================================================#
# Auxiliary functions::
# =============================================================================#

nalast <- function(x) 
{
  idx <- which( !is.na(x) )
  return( c(rep(NA, sum(is.na(x))), x[idx]) )
}

decrease <- function(x) 
{
  return( x[length(x):1] )
}

str.in <- function(target, input)
{
  return(
    ifelse( length( grep(target, input ) ) == 1, TRUE, FALSE )
  )
}

any.letter.in <- function(input)
{
  chars <- c(letters, LETTERS)
  
  bool <- c()
  for( i in seq(along = chars) ) {
    bool[i] <- str.in( chars[i], input )
  }
  return( any( bool == TRUE ) )
}

movingAverage <- function(x, order)
{
  y <- try( expr   = {
    filter( x, filter = rep(1 / order, order) )
  },
  silent = TRUE )
  
  return( y )
}

expMA <- function(x, order) 
{
  MA  <- movingAverage(x, order)
  if( class(MA) == 'try-error' ) { stop( 'Could not calculate a moving average' ) }
  EMA <- nalast( MA )
  
  ind <- which( !is.na(EMA) )
  
  EMA[ind] <- .C( 'expMAc', 
                  ema   = as.double( EMA[ind] ), 
                  x     = as.double( x[ind] ), 
                  order = as.double( order ), 
                  n     = as.integer( length(ind) ) )[[1]]
  
  return( EMA )
}

## slow R-function:
# expMA <- function(x, order)
# {
#   MA  <- movingAverage(x, order)
#   if( class(MA) == 'try-error' ) { stop( 'Could not calculate a moving average' ) }
#   EMA <- nalast( MA ) 
#   
#   multiplier <- 2 / ( order + 1 )
#   
#   for( i in which( !is.na(EMA) )[-1] ) {
#     EMA[i] <- ( x[i] - EMA[i - 1] ) * multiplier + EMA[i - 1]
#   }
#   
#   return( nalast(EMA) )
# }

MACD <- function(x, slow = 26, fast = 12) {
  EMAslow <- expMA( x     = x,
                    order = slow )
  EMAfast <- expMA( x     = x, 
                    order = fast )
  
  return( list( EMAslow = EMAslow,
                EMAfast = EMAfast,
                MACD    = EMAfast - EMAslow ) )
}

RSI <- function(x, order) 
{
  change  <- c( NA, x[-1] - x[-length(x)] )
  gain    <- ifelse( change >= 0,  change, 0 )
  loss    <- ifelse( change  < 0, -change, 0 )
  AvgGain <- AvgLoss <- rep( NA, length(x) )
  
  AvgGain[order + 1] <- mean(gain[(1:order) + 1])
  AvgLoss[order + 1] <- mean(loss[(1:order) + 1])
  
  AvgGain[(order + 2):length(AvgGain)] <- 0
  AvgLoss[(order + 2):length(AvgLoss)] <- 0
  
  ind <- which( !is.na(AvgGain) )
  
  L <- .C( 'RSIc',
           gain    = as.double( gain[ind] ),
           AvgGain = as.double( AvgGain[ind]),
           loss    = as.double( loss[ind]),
           AvgLoss = as.double( AvgLoss[ind]),
           x       = as.double( x[ind]),
           order   = as.double( order ),
           n       = as.integer( length(AvgGain) - order ) )
  
  return( c( rep(NA, order), 100 - 100 / (1 + L[[2]] / L[[4]]) ) )
}

## slow R-function:
# RSI <- function(x, order)
# {
#   change  <- c( NA, x[-1] - x[-length(x)] )
#   gain    <- ifelse( change >= 0,  change, 0 )
#   loss    <- ifelse( change  < 0, -change, 0 )
#   AvgGain <- AvgLoss <- rep( NA, length(x) )
#   
#   AvgGain[order + 1] <- mean(gain[(1:order) + 1])
#   AvgLoss[order + 1] <- mean(loss[(1:order) + 1])
#   
#   for( i in (order + 2):length(x) ) {
#     AvgGain[i] <- (AvgGain[i - 1] * (order - 1) + gain[i]) / order
#     AvgLoss[i] <- (AvgLoss[i - 1] * (order - 1) + loss[i]) / order
#   }
#   
#   RS <- AvgGain / AvgLoss
#   
#   return( 100 - 100 / (1 + RS) )
# }
