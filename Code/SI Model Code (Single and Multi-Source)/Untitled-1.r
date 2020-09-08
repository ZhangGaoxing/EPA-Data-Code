library(igraph)

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

load("./Graphs/UKfaculty.RData")
graph <- as.undirected(UKfaculty, mode = "collapse")
graph <- simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)

V(graph)$name
graph <- set.vertex.attribute(graph, "name", value=as.character(V(graph)))

save(graph, file = "UKfaculty.RData")

E(graph)$weights
