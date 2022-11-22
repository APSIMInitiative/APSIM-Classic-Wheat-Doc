# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   20/08/2010
# *

#' Visualize xml parameters
#' 
#' @param doc doc
#' @param xnode xnode
#' @param ynode ynode
#' @param width width
#' @param height height
#' @param keylab keylab
#' @param keypos keypos
#' @export
wdVisXY <- function(doc, xnode, ynode,
        xlab = NULL,
        ylab = NULL,
        keylab = NULL,
        keypos = NULL,
        mtext = NULL)
{
    # doc <- xmlInternalTreeParse( xml )
    library(lattice)
    library(grid)
    maxlen <- max( length( xnode ), length( ynode ) )
    xnode <- rep( xnode, len = maxlen )
    ynode <- rep( ynode, len = maxlen )
    
    
    res <- NULL
    dkeylab<- NULL
    for ( i in 1:maxlen )
    {
        temp <- xpathApply( doc, paste( "//", xnode[i], sep = "" ) )
        if ( length( temp ) == 0 )
        {
            stop( paste( "\"", xnode[i], "\" Can not be found \"",     "\".", sep = "" ) )
        }
        xvalue <- xmlValue( temp[[1]] )
        xvalue <- as.numeric( strsplit( xvalue, " " )[[1]] )
        xvalue <- xvalue[ !is.na( xvalue ) ]
        temp <- xpathApply( doc, paste( "//", ynode[i], sep = "" ) )
        if ( length( temp ) == 0 )
        {
            stop( paste( "\"", ynode[i], "\" Can not be found \"","\".", sep = "" ) )
        }
        yvalue <- xmlValue( temp[[1]] )
        yvalue <- as.numeric( strsplit( yvalue, " " )[[1]] )
        yvalue <- yvalue[ !is.na( yvalue ) ]
        
        if ( length( xvalue ) != length( yvalue ) )
        {
            stop( paste( xnode[i], "(", toString( xvalue ),") and ", ynode[i], "(", 
                            toString( yvalue ), ") must be the same length.", 
                            sep = "" ) )
        }
        r <- NULL
        r <- cbind( x = xvalue, y = yvalue, index = i )
        dkeylab[i] <- paste( xnode[i], "vs", ynode[i] )
        r <- as.data.frame( r )
        res <- rbind( res, r )
    }
    res <- as.data.frame( res )
    
    if ( is.null( keylab ) )
    {
        keylab <- dkeylab
    }
    
    if ( is.null( keypos ) )
    {
        keypos <- c(1,1)
    }
    
    p <- NULL
    if ( maxlen == 1 )
    {
        p <- xyplot( y ~ x, data = res,  
            xlab = xlab, ylab = ylab,
            page = function(x)
            {
                if (!is.null(mtext))
                {
                    grid.text(mtext, 
                     x = 0.01, y = 0.99, 
                     default.units = "npc", 
                     just = c("left", "top"))
                }
            },
            panel = function(x, y, ...)
            {
                panel.xyplot(x, y, type = 'p', ...)
                x_r <- diff(range(x)) * 0.3
                x <- c(x[1] - x_r, x, x[length(x)] + x_r)
                y <- c(y[1], y, y[length(y)])
                panel.xyplot(x, y, type = 'l', ...)
            })
    }
    else
    {
        p <- xyplot( y ~ x, data = res, groups = index,  type = "b", 
                xlab = xlab, ylab = ylab,
                pch = 2:(maxlen+1),
                lty = 2:(maxlen+1),
                col = 1:maxlen,
                page = function(x)
                {
                    if (!is.null(mtext))
                    {
                        grid.text(mtext, 
                         x = 0.01, y = 0.99, 
                         default.units = "npc", 
                         just = c("left", "top"))
                    }
                },
                key = list(lines = list( col = 1:maxlen, lty = 2:(maxlen+1) ),
                        points = list( col = 1:maxlen, pch = 2:(maxlen+1) ),
                        text = list( lab = keylab ),
                        corner = keypos ) )
    }
    p
}

