# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   04:04 PM Tuesday, 21 May 2013
# * Copyright: AS IS
# *



# Crown temperature
wdCrownTemperature <- function()
{
    library(lattice)
    

    h_snow <- seq(0, 10, by=2)
    res <- NULL
    for (i in 1:length(h_snow))
    {
        x <- -10:0
        y <- 2+x*(0.4+0.0018*(h_snow[i]-15)^2)
        
        x <- c(x,0:10)
        y <- c(y,0:10)
        
        r <- NULL
        r$x <- x
        r$y <- y
        r$h <- h_snow[i]
        r <- as.data.frame(r)
        res <- rbind(res,r)
    }
    res <- as.data.frame(res)

    key <- list(lines = list(col = length(h_snow):1,lty = length(h_snow):1),
                text = list(lab = as.character(rev(h_snow))),
                corner = c(1,0))
                
    p <- xyplot(y ~ x,  groups = h, data = res, type = "l", key = key, col = 1:length(h_snow),
        lty = 1:length(h_snow),
        xlab = expression(paste("Air Temperature", ~"("*degree*"C)")), 
        ylab = expression(paste("Crown Temperature", ~"("*degree*"C)")))
    p
}

# Photoperiod
wdPhotoPeriod <- function()
{
    library(lattice)
    ps <- c(0,0.5,1,2,3,4,5)
    res <- NULL
    for (i in 1:length(ps))
    {

        x <- 0:20
        y <- 1-0.002*ps[i]*(20-x)*(20-x)
        
        r <- NULL
        r$x <- x
        r$y <- y
        r$ps <- ps[i]
        r <- as.data.frame(r)
        res <- rbind(res,r)
    }
    res <- as.data.frame(res)
    res$y[res$y<0] <- 0
    key <- list(lines = list(col = 1:length(ps),lty = 1:length(ps)),
                text = list(lab = as.character(ps)),
                corner = c(1,0))
                
    p <- xyplot(y ~ x,  groups = ps, data = res, type = "l", key = key, col = 1:length(ps),
        lty = 1:length(ps),
        xlab = "Day length (h)", 
        ylab = expression(Photoperiod~factor~(f[p])))
    p
}

# Vernalisation
wdVernalisation <- function()
{
    library(lattice)

    minT <- seq(-10, 15, 0.2)
    maxT <- seq(0, 40, 0.2)
    gridt <- expand.grid(mint = minT, maxt = maxT)
    gridt <- gridt[gridt$maxt - gridt$mint > 3 & gridt$maxt > 0 & gridt$mint < 15,]

    cmint <- gridt$mint
    cmint[cmint<0] <- 2 + cmint[cmint<0] * (0.4 + 0.0018 * 15 * 15)


    gridt$crownt <- (cmint + gridt$maxt) / 2
    gridt$v = pmin(1.4 - 0.0778 * gridt$crownt,
            0.5 + 13.44 * (13.44 * gridt$crownt) / ((gridt$maxt - gridt$mint + 3)^2 ))
    gridt$v[gridt$v<0] <- 0
    # key <- list(lines = list(col = 1:length(ps),lty = 1:length(ps)),
                # text = list(lab = as.character(ps)),
                # corner = c(1,0))

                
    p <- levelplot(gridt$v ~ gridt$maxt + gridt$mint, 
                colorkey = T, 
                cuts = 10,
                xlim = c(0,40),
                ylim = c(-10,15),
                xlab = expression(paste("Maximum Temperature", ~"("*degree*"C)")), 
                ylab = expression(paste("Minimum Temperature", ~"("*degree*"C)")), 
                region = TRUE,
                contour =TRUE,
                col.regions = rev(heat.colors(20))
                )
    p
}

# devernalisation
wdDevernalisation <- function()
{
    library(lattice)
    x <- 30:50
    y <- 0.5*(x - 30)
    p <- xyplot(y ~ x, type = "l",
        xlab = expression(paste("Maximum Temperature", ~"("*degree*"C)")), 
        ylab = expression(Devernalization~(~Delta~V)))
    p
}

# Vernalisation factor
wdVernalisationFactor <- function()
{
    library(lattice)

    rs <- c(-0.055,0,0.5,1,1.5,2,3,4,5)
    res <- NULL
    for (i in 1:length(rs))
    {

        x <- 0:50
        y <- 1-(0.0054545 * rs[i] + 0.0003) * (50-x)
        r <- NULL
        r$x <- x
        r$y <- y
        r$rs <- rs[i]
        r <- as.data.frame(r)
        res <- rbind(res,r)
    }
    res <- as.data.frame(res)
    res$y[res$y<0] <- 0
    key <- list(lines = list(col = 1:length(rs),lty = 1:length(rs)),
                text = list(lab = as.character(rs)),
                corner = c(1,0))
                
    p <- xyplot(y ~ x,  groups = rs, data = res, type = "l", key = key, col = 1:length(rs),
        lty = 1:length(rs),
        xlab = "Cumulated vernalization (V)", 
        ylab = expression(Vernalization~factor~(f[v])))
    p
}


# Carbon dioxide factor
wdCarbonDioxideFactor <- function()
{
    library(lattice)
    tmean <- seq(from = 0, to = 30, by = 10)
    co2 <- seq(350, 700, 10)
    tmean.len <- length(tmean)
    tmean <- rep(tmean, each = length(co2))
    co2 <- rep(co2, times = tmean.len)
    c1 <- (163 - tmean) / (5 - 0.1 * tmean)
    fc <- (co2 - c1) * (350 + 2 * c1) / ((co2 + 2 * c1) * (350 - c1))

    pd <- cbind(tmean = tmean, co2 = co2, fc = fc)
    pd <- as.data.frame(pd)

    key <- list(lines = list(col = 1:tmean.len,lty = 1:tmean.len),
                text = list(lab = as.character(seq(from = 0, to = 30, by = 10 ))),
                corner = c(0,1))

    p <- xyplot(fc ~ co2, data = pd, groups = tmean, type = "l", lty = 1:tmean.len, 
            xlab = 'Carbon dioxide concentration (ppm)',
            ylab = expression(Factor~of~cardon~dioxide~~(f[c])),
            key = key, col = 1:tmean.len
            )
    p
}



# Function for leaf/stem/pod nitrogen 
wdNitrogenConcentration <- function()
{
    leaf <- wdVisXY(wheat_xml, 
            "x_stage_code", 
            c("y_n_conc_max_leaf", 
                    "y_n_conc_crit_leaf", 
                    "y_n_conc_min_leaf"),
            xlab = "Stage code",
            ylab =  "Nitrogen concentration",
            keylab = c("maximum", "critical","minimum"),
            mtext = 'Leaf')

    stem <- wdVisXY(wheat_xml, 
            "x_stage_code", 
            c("y_n_conc_max_stem", 
                    "y_n_conc_crit_stem", 
                    "y_n_conc_min_stem"),
            xlab = "Stage code",
            ylab =  "Nitrogen concentration",
            keylab = c("maximum", "critical","minimum"),
            mtext = 'Stem')
                
    pod <- wdVisXY(wheat_xml, 
            "x_stage_code", 
            c("y_n_conc_max_pod", 
                    "y_n_conc_crit_pod", 
                    "y_n_conc_min_pod"),
            xlab = "Stage code",
            ylab =  "Nitrogen concentration",
            keylab = c("maximum", "critical","minimum"),
            mtext = 'Pod')
    return (list(leaf=leaf, stem=stem, pod=pod))
}



# stemGrowthStructuralFractionStage
wdStemGrowthStructuralFraction <- function()
{
    library(lattice)
    x <- c(0, 7, 7.01, 11)
    y <- c(.65, .65, 0.0, 0)
    p <- xyplot(y ~ x, type = "b", 
            xlab = "Stage codes",
            ylab = "Fraction of structural biomass of stem" )
    p
}

# Plot kl factoring
wdKLFactoring <- function(doc)
{
    varKl <- function(doc, name, values, label = name)
    {
        getValue <- function(doc, name)
        {
            temp <- xpathApply(doc, paste("//", name, sep = ""))
            xvalue <- as.numeric(xmlValue(temp[[1]]))
            xvalue
        }
        res <- pmin(1, getValue(doc, sprintf('%sA', name)) * 
            exp(getValue(doc, sprintf('%sB', name)) * values))
        res <- as.data.frame(list(var = label,
            x = values,
            y = res))
        return(res)
    }
    df <- rbind(varKl(doc, 'Cl', 0:1500, 'CL'),
        varKl(doc, 'ESP', 0:80),
        varKl(doc, 'EC', seq(0, 4, by = 0.1)))
    library(ggplot2)
    
    p <- ggplot(df) + geom_line(aes(x, y)) +
        facet_wrap(~var, ncol = 1, scales = 'free_x') +
        ylab('KL factor') + xlab('Variable values') +
        theme_bw()
    p    
}
