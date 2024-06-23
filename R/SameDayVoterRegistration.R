# Same-Day Voter Registration
# Andrew Kim

rm(list = ls())
cat("\014")
setwd("/Users/andrew/Desktop/POLI338")

sdr <- data.frame(id = c(1:33, 1:33))

## VARIABLE GENERATION
# time + treatment dummies
sdr$time <- c(rep(0, 33), rep(1, 33))
sdr$policy <- as.numeric(sdr$id %in% 1:5)

# democrat share variable
set.seed(6310)
sdr$dem[sdr$time == 1] <- ifelse(sdr$policy[sdr$time == 1] == 1, rnorm(5, .54, .05), rnorm(28, .49, .09))
set.seed(6310)
sdr$dem[sdr$time == 0] <- rnorm(33, sdr$dem[sdr$time == 1]/1.02, .01)

# median age varaible
set.seed(6310)
sdr$age[sdr$time == 1] <- rnorm(33, 38.1, 3)
set.seed(6310)
sdr$age[sdr$time == 0] <- rnorm(33, sdr$age[sdr$time == 1]/1.03, 1)

# white share variable
set.seed(6310)
sdr$white[sdr$time == 1] <- rnorm(33, .601, .1)
set.seed(6310)
sdr$white[sdr$time == 0] <- rnorm(33, sdr$white[sdr$time == 1]/.95, .02)

# college degree share variable
set.seed(6310)
sdr$educ[sdr$time == 1] <- rnorm(33, .377/1.1, .05)
set.seed(6310)
sdr$educ[sdr$time == 0] <- rnorm(33, sdr$educ[sdr$time == 1]/1.1, .02)

# median income variable
set.seed(6310)
sdr$medinc[sdr$time == 1] <- rnorm(33, 70784, 12000)
set.seed(6310)
sdr$medinc[sdr$time == 0] <- rnorm(33, sdr$medinc[sdr$time == 1]/1.1, 2000)

# voter turnout variable
set.seed(6310)
sdr$turnout <- .41 + .05*sdr$time + .05*sdr$policy + .04*(sdr$policy * sdr$time) - 
  .1*sdr$dem + .001*sdr$age + .1*sdr$white + .1*sdr$educ + .000001*sdr$medinc + 
  rnorm(66, .05, .01)

summary(sdr$turnout[sdr$time == 0])
summary(sdr$turnout[sdr$time == 1])


## REGRESSIONS
# simple
sdrdid <- lm(turnout ~ policy*time, sdr)
summary(sdrdid)

# controls
sdrdidcont <- lm(turnout ~ policy*time + dem + age + white + medinc, sdr)
summary(sdrdidcont)

library(stargazer)
stargazer(sdrdidcont, type = "latex", keep = "policy:time", 
          covariate.labels = "SDR Effect", keep.stat = c("N", "rsq"), 
          dep.var.caption = "", dep.var.labels = "Turnout", 
          title = "DID Results")

## PLOT
# coefs
xrange <- c(.2, .8)
coefficients(sdrdid)
contcoefs <- c(coefficients(sdrdid)[[1]], coefficients(sdrdid)[[1]] + coefficients(sdrdid)[[3]])
treatcoefs <- c(coefficients(sdrdid)[[1]] + coefficients(sdrdid)[[2]], 
                coefficients(sdrdid)[[1]] + coefficients(sdrdid)[[2]]+ coefficients(sdrdid)[[3]] + 
                  coefficients(sdrdid)[[4]])

# plot means
plot(xrange, contcoefs, type = "l", pch = 16, col = "green", 
     ylim = c(.55, .8), ylab = "Voter Turnout",
     xlim = c(0, 1), xlab = "Year", main = "DID SDR Policy Effect", xaxt = "n")
lines(xrange, treatcoefs, col = "blue") # treatment line
points(xrange, contcoefs, pch = 16, col = "green") # control points
points(xrange, treatcoefs, pch = 16, col = "blue") # treatment points
axis(1, c(0.2,0.8), c("2016", "2020")) # x-axis labels

# counterfactual
cfcoefs <- c(coefficients(sdrdid)[[1]] + coefficients(sdrdid)[[2]], 
             coefficients(sdrdid)[[1]] + coefficients(sdrdid)[[2]] + coefficients(sdrdid)[[3]])
lines(xrange, cfcoefs, lty = 2, col = "darkgray")
points(xrange, cfcoefs, pch = 16, col = "darkgray")
text(lab = "Counterfactual\nEstimate", xrange[2] + .1, cfcoefs[2] - .02, cex = .8, col = "darkgray")
points(xrange, treatcoefs, pch = 16, col = "blue")

# legend
legend("topleft", c("Treatment", "Control"), pch = 16, col = c("blue", "green"))

# curly brackets function
Curlybrackets <- function(x0, x1, y0, y1, pos = 1, direction = 1, depth = 1, color = "black") {
  splinepoint <- c(1, 2, 3, 48, 50)
  splinepointdepth <- c(0, .2, .28, .7, .8)
  curve <- spline(splinepoint, splinepointdepth, n = 50, method = "natural")$y * depth
  curve <- c(curve, rev(curve))
  if (pos == 1){
    a_sequence <- seq(x0, x1, length=100)
    b_sequence <- seq(y0, y1, length=100)
  }
  if (pos == 2){
    b_sequence <- seq(x0, x1, length=100)
    a_sequence <- seq(y0, y1, length=100)
  }
  # direction
  if(direction == 1)
    a_sequence <- a_sequence + curve
  if(direction == 2)
    a_sequence <- a_sequence - curve
  # pos
  if(pos==1)
    lines(a_sequence,b_sequence, lwd = 1.5, xpd = NA, col = color)
  if(pos==2)
    lines(b_sequence,a_sequence, lwd = 1.5, xpd = NA, col = color)
}

Curlybrackets(0.82, 0.82, cfcoefs[2], treatcoefs[2], depth = 0.05, color = "blue")
text(0.95, (treatcoefs[2] + cfcoefs[2]) / 2 , "SDR Effect*", cex = .8, col = "darkblue")

# intervention line
abline(v = 0.5, lty = 2, col = "darkgray")
text(lab = "SDR Policies Implemented", x = 0.48, y = .675,  srt = 90, cex = .8, col = "darkgray")

# statistical significance comment
mtext("*The SDR effect reaches statistical significance (p-value = 1.74E-10)",
      side = 1, adj = 1, cex = 0.8, col = "darkgray", line = 3.6)
