# =============================================================================#
# Stocklist:
# =============================================================================#

create_stocklist <- function(..., stockRanges = '3d')
{
  stocks <- list(...)
  
  if( length(stocks[[1]]) > 1 ) {
    st <- list()
    for( i in stocks[[1]] ) {
      st[[i]] <- i
    }
    stocks <- st
  }
  if( length(stockRanges) == 1 ) {
    stockRanges <- rep( stockRanges, length(stocks) )
  }
  if( length(stockRanges) != length(stocks) ) {
    stop( 'Length of ranges has to match 1 or length(stocks)' )
  }
  
  L <- list()
  for( i in seq(along = stocks) ){
    L[[i]] <- readStock( stock = stocks[[i]],
                         stockRange = stockRanges[i] )
  }
  class(L) <- 'stocklist'
  return(L)
}

# -----------------------------------------------------------------------------#
# Plot:
# -----------------------------------------------------------------------------#

plot.stocklist <- function( stocklist, 
                            dim, 
                            candlewidth = 1,
                            colup       = c( 83, 199, 86), 
                            coldown     = c(255,  23, 23),
                            font        = 1,
                            RSI         = 14,
                            MA          = TRUE,
                            ma1         = 12,
                            ma2         = 50,
                            macdslow    = 26,
                            macdfast    = 12,
                            macdsignal  = 9,
                            legend      = FALSE )
{
  if( length(dim) == 1 ) { stop( 'Dim must have exact two elements' ) }
  par( mar = c(5.1, 4.1, 4.1, 2.1) )
  par_default <- par()
  
  par( bg       = '#07000d',
       col.axis = 'white',
       col.lab  = 'white',
       col.main = 'white',
       font.axis = font,
       font.lab  = font,
       font.main = font,
       font.sub  = font,
       las      = 1 )
  
  k <- 0
  
  lyout <- matrix( data = numeric(dim[1] * 3 * dim[2]), 
                   ncol = dim[2] )
  for( i in 1:dim[1] ) {
    for( j in 1:dim[2] ) {
      lyout[ 1:3 + 3 * (i - 1), j ] <- 1:3 + 3 * k
      k <- k + 1
    }
  }
  
  e <- try( expr   = { layout(lyout, heights = rep( c(25, 40, 25), dim[1] * dim[2] ) ) },
            silent = TRUE )
  
  if( class(e) == 'try-error' ) {
    dev.off()
    layout(lyout, heights = rep( c(25, 40, 25), dim[1] * dim[2] ) )
  }
  
  for( i in seq( along = stocklist) ) {
    obj <- stocklist[[i]]
    
    limits <- c(min(obj$low), max(obj$high)) + c(-1, 1) * (max(obj$high) - min(obj$low)) / 4
    
    if( MA ) {
      ## Calculate moving averages at the beginning if there is an error:
      MA1 <- movingAverage( x     = obj$close, 
                            order = ma1 )
      if( class(MA1) == 'try-error') { 
        MA <- FALSE
        warning( paste('Set "MA = FALSE" due to failing filter with order', ma1) )
      }
      MA2 <- movingAverage( x     = obj$close, 
                            order = ma2 )
      if( class(MA2) == 'try-error') { 
        MA <- FALSE
        warning( paste('Set "MA = FALSE" due to failing filter with order', ma2) )
      }
    }
    
    oma <- par()$oma
    mar <- par()$mar
    
    if( str.in('d', attr(obj, 'Interval')) ) {
      d <- obj$Date
    }
    if( str.in('m', attr(obj, 'Interval')) ) {
      d <- format(obj$Date, '%Y-%m')
    }
    if( str.in('y', attr(obj, 'Interval')) ) {
      d <- format(obj$Date, '%Y')
    }
    
    
    labs <- unique( d )
    xlabs <- integer( length = length(labs) )
    
    for( i in seq( along = xlabs ) ) {
      xlabs[i] <- min( which( d %in% labs[i] ) )
    }
    
    ## ==============#
    ## Plot 1:
    ## ==============#
    
    par( mar = c(0, mar[-1]) )
    
    ## ----------#
    ## RSI:
    ## ----------#
    
    rsi <- RSI( x    = obj$close,
                order = RSI )
    
    plot( x    = rsi, 
          axes = FALSE,
          main = ifelse( attr(obj, 'Company') == '', 
                         attr(obj, 'Stock'),
                         paste0( attr(obj, 'Company'), ' (', attr(obj, 'Stock'), ')') ),
          ylab = '' )
    
    yaxp  <- seq( par()$yaxp[1], par()$yaxp[2], length.out = par()$yaxp[3] + 1 )
    
    polygon( x      = c(1:length(rsi), length(rsi):1),
             y      = c(ifelse( rsi <= 30, rsi, 30), rep(30, length(rsi))),
             border = rgb(26, 135, 130, 255, maxColorValue = 255),
             col    = rgb( 56, 109, 19, 100, maxColorValue = 255) )
    
    polygon( x      = c(1:length(rsi), length(rsi):1),
             y      = c(ifelse( rsi >= 70, rsi, 70), rep(70, length(rsi))),
             border = rgb(26, 135, 130, 255, maxColorValue = 255),
             col    = rgb(143,  32, 32, 100, maxColorValue = 255) )
    
    lines( x     = rsi,
           col   = rgb(26, 135, 130, 255, maxColorValue = 255) )
    
    abline( h   = c(30, 70),
            col = c( rgb( 56, 109, 19, 255, maxColorValue = 255),
                     rgb(143,  32, 32, 255, maxColorValue = 255)) )
    
    axis( side     = 2,
          at       = c(30, 70),
          labels   = c(30, 70),
          cex.axis = 0.9)
    
    text( x      = 1, 
          y      = yaxp[length(yaxp)] - 0.15 * diff(yaxp[c(1, length(yaxp))]),
          labels = paste0('RSI ', RSI),
          col    = 'white',
          adj    = c(0, 1) )
    
    segments( x0 = xlabs,
              y0 = yaxp[1] - 0.15 * diff(yaxp[c(1, length(yaxp))]),
              x1 = xlabs,
              y1 = yaxp[1] - 0.3 * diff(yaxp[c(1, length(yaxp))]),
              col = 'white' )
    
    box( col = '#5998ff', 
         lwd = 1.5 )
    
    ## ==============#
    ## Plot 2:
    ## ==============#
    
    par( mar = c(0, mar[2], 0, mar[4]) )
    
    plot( x    = obj$high, 
          col  = '#07000d', 
          axes = FALSE,
          ylim = limits,
          xlab = '',
          ylab = 'Stock price' )
    
    xaxp <- seq( par()$xaxp[1], par()$xaxp[2], length.out = par()$xaxp[3] + 1 )
    yaxp <- seq( par()$yaxp[1], par()$yaxp[2], length.out = par()$yaxp[3] + 1 )
    
    if( length(labs) <= length(xaxp) ) {
      segments( x0  = xlabs, 
                y0  = par()$usr[3], 
                x1  = xlabs, 
                y1  = par()$usr[4],
                col = rgb(255, 255, 255, 150, maxColorValue = 255),
                lty = 3 )
      
    } else {
      segments( x0  = xaxp, 
                y0  = par()$usr[3], 
                x1  = xaxp, 
                y1  = par()$usr[4],
                col = rgb(255, 255, 255, 150, maxColorValue = 255),
                lty = 3 )
    }
    
    segments( x0  = par()$usr[1], 
              y0  = yaxp, 
              x1  = par()$usr[2], 
              y1  = yaxp,
              col = rgb(255, 255, 255, 150, maxColorValue = 255),
              lty = 3 )
    
    ## ---------#
    ## Volume:
    ## ---------#
    
    vol <- obj$volume * diff(limits) / 4 / max(obj$volume)
    
    polygon( x      = c(1:length(obj$volume), length(obj$volume):1),
             y      = c(vol, rep(0, length(obj$volume))) - (min(vol) - par()$usr[3]),
             col    = rgb(0, 255, 232, 100, maxColorValue = 255),
             border = rgb(0, 255, 232, 150, maxColorValue = 255) )
    
    # small segments for every new unit:
    segments( x0 = xlabs,
              y0 = limits[2] - 0.02 * diff(limits),
              x1 = xlabs,
              y1 = limits[2] + 0.04 * diff(limits),
              col = 'white' )
    
    text( x      = 1,
          y      = max(vol[1:4]) - (min(vol) - par()$usr[3]),
          labels = 'Volume',
          col    = rgb(0, 255, 232, 255, maxColorValue = 255),
          xpd    = TRUE,
          adj    = c(0, 0) )
    
    ## ---------------#
    ## Candlesticks:
    ## ---------------#
    
    for( i in 1:length(obj$high) ) {
      if( obj$open[i] > obj$close[i] ) {
        rect( xleft   = i - candlewidth * 0.4,
              xright  = i + candlewidth * 0.4,
              ybottom = obj$open[i],
              ytop    = obj$close[i],
              col     = rgb(coldown[1], coldown[2], coldown[3], 125, maxColorValue = 255),
              border  = rgb(coldown[1], coldown[2], coldown[3], 255, maxColorValue = 255) )
        
        segments( x0  = i,
                  x1  = i,
                  y0  = obj$high[i],
                  y1  = obj$open[i],
                  col = rgb(coldown[1], coldown[2], coldown[3], 255, maxColorValue = 255) )
        segments( x0  = i,
                  x1  = i,
                  y0  = obj$low[i],
                  y1  = obj$close[i],
                  col = rgb(coldown[1], coldown[2], coldown[3], 255, maxColorValue = 255) )
      } else {
        rect( xleft   = i - candlewidth * 0.4,
              xright  = i + candlewidth * 0.4,
              ybottom = obj$open[i],
              ytop    = obj$close[i],
              col     = rgb(colup[1], colup[2], colup[3], 125, maxColorValue = 255),
              border  = rgb(colup[1], colup[2], colup[3], 255, maxColorValue = 255) )
        
        segments( x0  = i,
                  x1  = i,
                  y0  = obj$high[i],
                  y1  = obj$close[i],
                  col = rgb(colup[1], colup[2], colup[3], 255, maxColorValue = 255) ) 
        segments( x0  = i,
                  x1  = i,
                  y0  = obj$low[i],
                  y1  = obj$open[i],
                  col = rgb(colup[1], colup[2], colup[3], 255, maxColorValue = 255) ) 
      }
    }
    
    ## ----------------------#
    ## Moving Averages:
    ## ----------------------#
    if( MA ) {
      class(MA1) <- 'numeric'
      class(MA2) <- 'numeric'
      
      idx_ma1 <- which( is.na(MA1) )
      idx_ma2 <- which( is.na(MA2) )
      
      MA1 <- c( MA1[idx_ma1], MA1[-idx_ma1])
      MA2 <- c( MA2[idx_ma2], MA2[-idx_ma2])
      
      points( x    = MA1,
              type = 'l',
              col  = rgb(225, 237, 249, 180, maxColorValue = 255),
              lwd  = 2 )
      points( x    = MA2,
              type = 'l',
              col  = rgb(78, 230, 253, 180, maxColorValue = 255),
              lwd  = 2 )
      
      if( legend ) {
        xlegend <- seq( from       = 1, 
                        to         = length(obj$high), 
                        length.out = 4 )
        xlegend <- floor( xlegend )
        
        if( mean( limits[2] - obj$high[xlegend[1:2]] ) < mean( limits[2] - obj$high[xlegend[2:3]] ) ) {
          if( mean( limits[2] - obj$high[xlegend[2:3]]) < mean( limits[2] - obj$high[xlegend[3:4]] ) ) {
            xlegend <- length(obj$high)
            xadj    <- 1
          } else {
            xlegend <-  mean( seq( along = obj$high ) )
            xadj    <- 0.5
          }
        } else {
          if( mean( limits[2] - obj$high[xlegend[1:2]]) < mean( limits[2] - obj$high[xlegend[3:4]] ) ) {
            xlegend <- length(obj$high)
            xadj    <- 1
          } else {
            xlegend <- xaxp[1]
            xadj    <- 0
          }
        }
        
        legend( x        = xlegend,
                y        = yaxp[length(yaxp)] - 0.00 * diff(yaxp[c(1, length(yaxp))]),
                legend   = paste0(c(ma1, ma2), ' MA'),
                box.col  = NA,
                lty      = 1,
                col      = c( rgb(225, 237, 249, 255, maxColorValue = 255),
                              rgb( 78, 230, 253, 255, maxColorValue = 255) ),
                bg       = rgb(255, 255, 255, 100, maxColorValue = 255),
                text.col =  'white',
                ncol  = 2,
                xjust = xadj
        )
      }
    }
    
    
    axis( side   = 2,
          at     = yaxp,
          labels = yaxp )
    
    box( col = '#5998ff', 
         lwd = 1.5 )
    
    
    
    
    ## ==============#
    ## Plot 3:
    ## ==============#
    
    par( mar = c(rep(mar[2], 2), 0, mar[4]))
    
    ## ---------#
    ## MACD:
    ## ---------#
    
    nslow <- macdslow
    nfast <- macdfast
    nema  <- macdsignal
    
    macd  <- MACD( x    = obj$close,
                   slow = nslow,
                   fast = nfast )
    
    ema9 <- as.numeric( expMA( x     = macd$MACD,
                               order = nema ) )
    
    hist   <- macd$MACD - ema9
    limits <- c( min(c(macd$MACD, ema9, hist), na.rm = TRUE), 
                 max(c(macd$MACD, ema9, hist), na.rm = TRUE) )
    
    if( !legend ) {
      plot( x    = macd$MACD,
            col  = '#07000d', 
            axes = FALSE,
            xlab = '',
            ylab = '',
            ylim = limits + c(-0.1 * diff(limits), 0.3 * diff(limits)) )
    } else {
      plot( x    = macd$MACD,
            col  = '#07000d', 
            axes = FALSE,
            xlab = '',
            ylab = '',
            ylim = limits + c(-0.8 * diff(limits), 0.3 * diff(limits)) )
    }
    
    polygon( x      = c( which(!is.na(hist)), 
                         decrease(which(!is.na(hist))) ),
             y      = c(na.omit(hist), rep(0, length(na.omit(hist)))),
             col    = rgb(0, 255, 232, 100, maxColorValue = 255),
             border = rgb(0, 255, 232, 150, maxColorValue = 255) )
    
    if( length(labs) <= length(xaxp) ) {
      axis( side   = 1,
            at     = xlabs,
            labels = labs )
    } else {
      axis( side   = 1,
            at     = xaxp,
            labels = obj$Date[xaxp + 1] )
    }
    
    yaxp  <- seq( par()$yaxp[1], par()$yaxp[2], length.out = par()$yaxp[3] + 1 )
    yaxp[which(min(abs(yaxp)) == abs(yaxp))] <- 0
    
    yaxp1 <- yaxp
    yaxp  <- yaxp[ yaxp >= limits[1] ]
    
    abline( h   = yaxp,
            col = rgb(0, 255, 232, 50, maxColorValue = 255),
            lty = 2 )
    abline( h   = 0,
            col = rgb(0, 255, 232, 255, maxColorValue = 255) )
    
    axis( side     = 2,
          at       = yaxp,
          labels   = yaxp,
          cex.axis = 0.9 )
    
    if( legend ) {
      legend( x        = xlabs[1],
              y        = limits[1] + 0.1 * diff(limits),
              legend   = c( paste0('MACD ', nfast, ', ', nslow, ', ', nema),
                            paste0('signal line (ema', nema, ' of MACD)'),
                            paste0('historgram line (MACD - signal)') ),
              box.col  = NA,
              lty      = 1,
              col      = c( rgb( 78, 230, 253, 150, maxColorValue = 255),
                            rgb(226, 237, 249, 150, maxColorValue = 255),
                            rgb(  0, 255, 232, 255, maxColorValue = 255) ),
              bg       = rgb(255, 255, 255, 0, maxColorValue = 255),
              text.col =  'white',
              ncol     = 3,
              yjust    = 1,
              xpd      = TRUE
      )
    }
    
    lines( x   = macd$MACD, 
           lwd = 1.5,
           col = rgb(78, 230, 253, 150, maxColorValue = 255) )
    lines( x   = ema9,
           lwd = 1.5,
           col = rgb(226, 237, 249, 150, maxColorValue = 255) )
    lines( x   = hist,
           lwd = 2,
           col = rgb(0, 255, 232, 255, maxColorValue = 255) )
    
    #   segments( x0  = xlabs,
    #             y0  = yaxp[length(yaxp)] + 0.4 * diff(limits),
    #             x1  = xlabs,
    #             y1  = yaxp[length(yaxp)] - 0.2 * diff(yaxp[c(1, yaxp[length(yaxp)])]),
    #             col = 'white' )
    #   
    box( col = '#5998ff', 
         lwd = 1.5 )
    
    par( mar = mar )
  }
  par( par_default[-which(names(par_default) %in% c('cin' ,'cra', 'csi', 'cxy', 'din', 'page'))] )
}
