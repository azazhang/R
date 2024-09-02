FAA <- read.csv(file = "data/FAA.csv", header = TRUE)

attach(FAA)
mean(distance)
mean(height)
mean(speed_ground)

mean(pitch)
head(pitch)
pitch[1] <- -4043515
mean(pitch)
length(pitch)
length(FAA)
pitch.sorted <- sort(pitch)
round(pitch.sorted, 2)

pitch.sorted[c(400,401)]
mean(pitch.sorted[c(400,401)])
median(pitch.sorted)

height.sorted <- sort(height)
summary(height)
sort(height)[1:10]
round(pitch.sorted ,2)

mean(pitch, trim = 0.01)
mean(pitch)
median(pitch)
var(height)
sd(height)
sample(pitch)

pitch <- pitch[-1]
lower <- mean(pitch) - 2*sd(pitch)
upper <- mean(pitch) + 2*sd(pitch)

sum((pitch > lower) & pitch < upper)/length(pitch)

lower <- mean(distance)-1*sd(distance)
upper <- mean(distance)+1*sd(distance)
sum(distance > lower & distance < upper) / length(distance)
plot(distance)
quantile(distance, c(0.1, 0.25, 0.5, 0.75, 0.9))
hist(distance, freq = F, ylim = c(0, 0.0008))
lines(density(distance), lwd = 5, col = "blue")

