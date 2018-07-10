#'
#' ^^^^^^^^^^^^^^^^^^^^^^^^^^
#' ^^^^^^^^^^^^^^^^^^^^^^^^^^
#' ^^^^^^^^^^^^^^^^^^^^^^^^^^
#' ||||||||||||||||||||||||||
#' ##########################
#' ##########################
#'
#'  --- SILVIATERRA, LLC ---
#'
#' ##########################
#'
#' ------------------------------------------------------------------------------
#'
# A script to simulate samples of a known populations for the purpose of
# comparison.
# Used in April 2018 article in the Forestry Source

# Load required packages
library(foreign)
library(dplyr)
library(truncnorm)

standSim <- data.frame(BAPA = 175,
                       stdBAPA = 37.7)
                       
# create the populations:
# This creates for each cruised stand, a set of 100000 values
# use truncated normal distribution since no measurements below zero
# simulating a population of potential plots for all stands in the project
population <- rtruncnorm(
  n = 100000,
  sd = standSim$stdBAPA,
  mean = standSim$BAPA,
  a = 0
)

#######
# sampleSize <- t * (std / mean) / allowableE
sampleSize <- (1.78 * (standSim$std / standSim$meanBA) / 0.1 ) ^ 2

# The standard sample size equation tells us that we want 15 plots to
# reach our target of +/- 10% at 90%.
cruise_stand <- function(i) {
  temp <- sample(population, size = 15, replace = F)
  meantemp <- mean(temp)
  sdtemp <- sd(temp)
  
  out <- data.frame(sampleNum = i,
                    meantemp = meantemp,
                    sdtemp = sdtemp)
  return(out)
}

cruise20 <- bind_rows(lapply(1:20, cruise_stand)) %>%
  mutate(CI = (sdtemp / sqrt(15)) * qt(1 - 0.1 / 2, df = 15 - 1))

#### Dataframe of cruise20 used in actual ForestrySource article:
# cruise20 <- {structure(list(sampleNum = 1:20,
#   meantemp = c(174.369177627875,
#     187.166951734099, 188.981527035639, 171.765251990422, 172.551083415091,
#     158.436615197593, 182.465457531922, 173.969441887349, 170.034380319772,
#     181.597445155021, 168.136187383573, 165.203210566948, 170.832251216761,
#     178.940662967202, 166.076592018345, 177.470859789136, 166.220655837215,
#     170.606174766682, 175.75824174835, 165.281346873727),
#   sdtemp = c(31.0971085556902,
#     38.5360493655504, 25.6741838948073, 32.1323127970596, 30.6103230180023,
#     23.129690923854, 42.858488922461, 34.0087281772148, 27.8530352866076,
#     38.9672911860291, 35.4569981038149, 53.0915487804432, 51.8891758784312,
#     40.6539569900961, 32.0678171876942, 31.983614959009, 35.2396721221887,
#     33.7479392750561, 27.9132744620394, 35.4746547132257),
#   CI = c(14.1419798631626,
#     17.5249744894283, 11.6758055172001, 14.6127579584793, 13.9206052212298,
#     10.5186507195848, 19.4906830717582, 15.4660922314572, 12.6666781075723,
#     17.7210896083128, 16.1247195151393, 24.1443545277702, 23.597553421236,
#     18.4881317850568, 14.5834273984604, 14.5451348922498, 16.0258865432444,
#     15.3474937001417, 12.6940729762754, 16.1327491817655),
#   Pass = c(TRUE,
#     TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE,
#     FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
#   overlapPct = c(0.894736842105263,
#     0.684210526315789, 0.263157894736842, 0.894736842105263, 0.842105263157895,
#     0.263157894736842, 0.947368421052632, 0.947368421052632, 0.894736842105263,
#     0.947368421052632, 0.894736842105263, 1, 1, 0.947368421052632,
#     0.789473684210526, 0.947368421052632, 0.842105263157895, 0.894736842105263,
#     0.894736842105263, 0.789473684210526)
#   ),
#   .Names = c("sampleNum", "meantemp", "sdtemp", "CI", "Pass", "overlapPct"),
#   row.names = c(NA, -20L), class = "data.frame")
# }

### How many of our cruises met our target? (+/- 10% at 90%)

cruise20$Pass <- cruise20$CI / cruise20$meantemp < 0.1

#### So if Cruise 1 is true,
# how many of the mean point estimates of the other cruises were within
# Cruise1's CI?

for(i in 1:nrow(cruise20)) {
  origCruise <- cruise20[i, ]
  others <- cruise20[-i, ]
  pointInCI <- nrow(others[others$meantemp < origCruise$meantemp + origCruise$CI &
                others$meantemp > origCruise$meantemp - origCruise$CI ,])
  cruise20$overlapPct[i] <- pointInCI / 19
}

# So if you cruise a stand and then cruise the exact same stand again with the
# same number of plots and same sampling design:
mean(cruise20Pass$overlapPct)

# Some percent of the time, the mean of a second cruise will fall within the CI
# of the first cruise

# Regardless, both cruises are valid representations of the population

compHigh <- cruise20$meantemp[2] + cruise20$CI[2]
compLow <- cruise20$meantemp[2] - cruise20$CI[2]

cruiseExample <- cruise20 %>%
  mutate(inCI = ifelse(meantemp < compHigh & meantemp > compLow,
    'Mean within original cruise CI', 'Mean outside original cruise CI')) %>%
  mutate(inCI = ifelse(sampleNum==2, 'Original cruise', inCI))
       
cruiseExample$inCI <- factor(cruiseExample$inCI,
  levels = c('Original cruise',
    'Mean within original cruise CI',
    'Mean outside original cruise CI'))
