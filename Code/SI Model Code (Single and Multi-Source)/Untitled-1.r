load("./200_t/Yeast_Hetero_30.200.RData")
load("./200_t/Yeast_Hetero_30_Time.200.RData")

# r <- Yeast_Hetero_30
# t <- Yeast_Hetero_30_Time

r <- c(Yeast_Hetero_30, r)
t <- c(Yeast_Hetero_30_Time, t)

length(r)
length(Yeast_Hetero_30)

Yeast_Hetero_30 <- r
Yeast_Hetero_30_Time <- t

Yeast_Hetero_30[c(1:200)][1]

save(Yeast_Hetero_30, file="./200_t/Yeast_Hetero_30.200.RData")
save(Yeast_Hetero_30_Time, file="./200_t/Yeast_Hetero_30_Time.200.RData")