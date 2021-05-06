## GOLF CLUB SELECTION ASSISTANT ##

library(ggplot2)
library(Hmisc)

# read in your data (should be in "long" format)
# one column of club number (mine are 1" = D, "2-3" = woods; "4" = H; "5"-"9" = irons; "10"-"11" = PW/SW)
# one column of distances
setwd("~/R/golf 2021")
golf_data <- read.csv("golf.csv", header = T)

# create your prediction models
length_by_club <- lm(length ~ club, data = golf_data) # predict length by club
summary(length_by_club)
confint.lm(length_by_club)

club_by_length <- lm(club ~ length, data = golf_data) # predict club by length
summary(club_by_length)
confint.lm(club_by_length)

# plot your length model
ggplot(golf_data, aes(length, club)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

# create your distance chart
clubs <- data.frame(club = c(1,2,3,4,5,6,7,8,9,10,11)) # give it your clubs
pred_model <- predict(length_by_club, newdata = clubs, se.fit = T, interval = "confidence", level = .95)
print(pred_model$fit)

lengths <- as.data.frame(pred_model) # use your regression model
colnames(lengths) <- c("Distance", "LowerCI", "UpperCI", "SE", "df", "resid")
lengths$Club <- row.names(lengths)
lengths$Club <- factor(lengths$Club, levels = c("1","2","3","4","5","6","7","8","9","10","11")) # convert to factor
print(round(lengths[1:6],0)) # see your distances

# plot your distance chart
# provides average length and 95% confidence intervals 
# Note: CIs are for info - they don't line up with the y-axis
labels <- c("D", "W1", "W3", "H", "5", "6", "7", "8", "9", "PW", "SW")
ggplot(lengths, aes(Club, round(Distance,0), fill = Club)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge", alpha = 0.5, colour = "black") + 
  stat_summary(fun = mean, aes(label = ..y.., group = Club), geom = "text", size = 6) +
  scale_fill_grey(start = .3, end = .7) +
  scale_x_discrete(labels = labels) +
  geom_text(data = lengths, aes(label = round(LowerCI,0)), position = position_stack(vjust = .85)) +
  geom_text(data = lengths, aes(label = round(UpperCI,0)), position = position_stack(vjust = 1.15)) +
  geom_hline(yintercept = 50., linetype="dotted", size = 0.5) +
  geom_hline(yintercept = 100, linetype="dotted", size = 1) +
  geom_hline(yintercept = 150, linetype="dotted", size = 0.5) +
  geom_hline(yintercept = 200, linetype="dotted", size = 1) +
  ylim(0, 250) + # if you can drive over 250, you'll need to adjust this!
  labs(y = "Distance") +
  theme_bw() +
  theme(text = element_text(size = 18), legend.position = "none") +
  coord_flip()

# load the "auto_caddy" functions
auto_caddy_club <- function(model, distance) { # requires a model and a target distance
  to_go <- data.frame(length = c(distance))
  club <- predict(model, newdata = to_go)
  lower <- confint.lm(model)[2]
  upper <- confint.lm(model)[4]
  good_shot <- club - upper
  ave_shot <- club
  bad_shot <- club + lower
  if (distance < 50) {
    cat("For",distance,"you'd probably want a putter!")
    } else if (distance > 250) {
      cat("You can't drive",distance,"yards mate! ;)")
    } else {
      cat("Distance:  ",distance,"\n")
      cat("Bad shot:  ",round(bad_shot,0),"\n")
      cat("Ave shot:  ",round(ave_shot,0),"\n")
      cat("Good shot: ",round(good_shot,0),"\n")
    }
}
 
auto_caddy_distance <- function(model, club) {
  club_choice <- data.frame(club = c(club))
  dist <- predict(model, newdata = club_choice)
  lower <- confint.lm(model)[2]
  upper <- confint.lm(model)[4]
  good_shot <- dist - upper
  ave_shot <- dist
  bad_shot <- dist + lower
  cat("Bad shot:  ", round(bad_shot,0),"  \n")
  cat("Ave shot:  ", round(ave_shot,0),"  \n")
  cat("Good shot: ", round(good_shot,0)," \n")

}

# check the auto_caddy outputs
auto_caddy_club(club_by_length, 170)
auto_caddy_distance(length_by_club, 7)

# END #
