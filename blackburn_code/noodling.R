
# noodling ----------------------------------------------------------------


catch_at_age <- data.frame(
  age = 1:12,
  ct = c(17, 97, 79, 47, 34, 25, 18, 9, 12, 8, 4, 2)
) 

# establish catch curve
plot(log(ct) ~ age, data = catch_at_age)


# chapman-robson method of survival (Ricker p. 31, eq. 2.5)

ChapRob <- function(ages, n, N0 = 2) {
  
  # i <- seq_along(n)
  # 
  # 
  # 
  # nn <- sum(n)
  # 
  # list(
  #   
  #   nn = nn,
  #   i = i,
  #   n,
  #   TT = n[i + 1] * i,
  #   
  #   sumTT = sum(n[i + 1] * i, na.rm = T)
  #   
  # )
  # 
  
  ages
  
  coded_age <- rep(NA, times = length(ages))
  print(coded_age)
  coded_age[ages >= N0] <- seq_along(ages[ages >= N0]) - 1
  TT <- sum(coded_age * n, na.rm = TRUE)
  
  list(
    TT = TT,
    N = sum(n[ages >= N0]),
    S = TT / (sum(n[ages >= N0]) + (TT - 1))
  )
  
}

with(data = catch_at_age[catch_at_age$age < 11, ], expr = {
  # [catch_at_age$age < 11, ]
  
  ChapRob(ages = age, n = ct)
})

sum(catch_at_age$ct)


# 20-Feb-2018 -------------------------------------------------------------

# age frequency for file BCLA.csv

bcla <- read.csv(file = "blackburn_code/BCLA.csv", header = TRUE)

head(bcla)
tail(bcla)

# no NA values
Map(function(x) sum(is.na(x)), bcla)

bcla$ageG <- ifelse(bcla$age > 19, 19, bcla$age)

table(bcla$age)
table(bcla$ageG)
prop.table(table(bcla$age[bcla$age %in% 3:19])) * 48000
prop.table(table(bcla$ageG[bcla$ageG %in% 3:19])) * 48000

# from initial_age_dist matrix
iad <- data.frame(
  age = 0:19,
  freq = c(
    141491743, 3192.023, 2874.605, 743.4951, 919.6688, 1887.2559,
    2643.7050, 3778.5699, 4249.9032, 3177.8537, 1654.6577,
    1068.1057, 597.0728, 1214.5035, 561.4314, 858.8469,
    38.1710, 439.7614, 148.104767, 148.1116858
  )
)


# 24000 is adult slot limit abundance of 48K cut in half for females
plot(
iad[4:20, "freq"] / 24000,
unlist(prop.table(iad[4:20, "freq", drop = FALSE]), use.names = FALSE)
)
abline(a = 0, b = 1, col = 2)
# so iad represents females only

# roughly equal to 24000
sum(iad[4:20, "freq"])











