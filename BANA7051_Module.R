population <- rbinom(10^6, 1, 0.55)
id <- sample(1:10^6, size = 100)
vote.sample <- population[id]
mean(vote.sample)
mean(population)
sd(vote.sample)
sd(population)
population[id]
