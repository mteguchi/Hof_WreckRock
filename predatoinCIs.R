#predationCIs


#total predation
binom.test(x = 11,
           n = 32, alternative = 'two.sided',
           p = 0.5,
           conf.level = 0.95)


# pre cyclone
binom.test(x = 10,
           n = 23, alternative = 'two.sided',
           p = 0.5,
           conf.level = 0.95)

# post cyclone
binom.test(x = 1,
           n = 9, alternative = 'two.sided',
           p = 0.5,
           conf.level = 0.95)

# exclusion device
binom.test(x = 5,
           n = 57, alternative = 'two.sided',
           p = 0.5,
           conf.level = 0.95)

# pre cyclone exclusion device
binom.test(x = 2,
           n = 33, alternative = 'two.sided',
           p = 0.5,
           conf.level = 0.95)

# post cyclone with exclusion devices
binom.test(x = 3,
           n =24, alternative = 'two.sided',
           p = 0.5,
           conf.level = 0.95)
