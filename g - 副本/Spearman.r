load(file = "USPG.RData")
load(file = "USPG_Hetero_5.RData")
load(file = "USPG_Hetero_5_Time.RData")

library(igraph)

# 计算图中各点的离心率，取10%作为监视
ecc <- eccentricity(graph, vids = V(graph), mode = c("all"))
#ecc <- degree(graph)
#ecc <- betweenness(graph, v = V(graph), directed = FALSE, weights = NULL,nobigint = TRUE, normalized = FALSE)
#ecc <- closeness(graph, vids = V(graph), mode = c("all"), weights = NULL, normalized = TRUE)
#ecc <- eigen_centrality(graph, directed = FALSE, scale = TRUE, weights = NULL, options = arpack_defaults)$vector
count <- round(length(V(graph)) * 0.6)
estimated_ecc <- V(graph)[as.numeric(order(ecc, decreasing = FALSE)[1:count])]$name

res_nodes <- vector(mode = "character", length = 0)
res_distances <- vector(mode = "numeric", length = 0)
spearmans <- vector(mode = "numeric", length = 0)

i <- 1
count <- 1
while (i <= 200) {
    print(count)
    # 感染图的第一个节点为源
    source <- V(graph)[USPG_Hetero_5[[i]][1]]$name
    # 生成感染图
    sg <- induced_subgraph(graph, USPG_Hetero_5[[i]], impl = c("copy_and_delete"))

    # 取感染图中的监视
    monitor <- intersect(estimated_ecc, V(sg)$name)
    monitor_num <- length(monitor)

    if (source %in% monitor || monitor_num <= 1) {
        i <- i + 1
        next
    }
        
    # 从感染图中排除监视
    no_monitor_nodes <- setdiff(V(sg)$name, monitor)
    # 监视在感染图的索引和时间
    node_indexs <- match(monitor, V(graph)$name)
    indexs <- match(node_indexs, USPG_Hetero_5[[i]])
    times <- USPG_Hetero_5_Time[[i]][indexs]

    temp_s <- vector(mode = "numeric", length = 0)
    temp_r <- vector(mode = "character", length = 0)
    #遍历感染图的节点

    for (node in no_monitor_nodes) {
        distances <- vector(mode = "numeric", length = 0)

        for (m in monitor) {
            distances[length(distances) + 1] <- distances(sg, v = m, to = node, mode = c("all"), weights = NULL, algorithm = c("unweighted"))
        }

        s <- 1 - 6 / (monitor_num * (monitor_num ** 2 - 1)) * sum((rank(distances) - rank(times))**2)

        temp_s[length(temp_s) + 1] <- s
        temp_r[length(temp_r) + 1] <- node
    }

    # 存在多个点结果相同
    col_index <- 1
    if (length(which(temp_s == max(temp_s))) != 0) {
        clo <- closeness(sg, vids = temp_r[which(temp_s == max(temp_s))], mode = c("all"), weights = NULL, normalized = TRUE)
	    col_index <- which(max(clo) == clo)
    }

    res_nodes[length(res_nodes) + 1] <- temp_r[which(temp_s == max(temp_s))][col_index]
    res_distances[length(res_distances) + 1] <- distances(sg, v = source, to = res_nodes[length(res_nodes)], mode = c("all"), weights = NULL, algorithm = c("unweighted"))
    spearmans[length(spearmans) + 1] <- max(temp_s)

    i <- i + 1
    count <- count + 1
    if (count > 100) {
       break
    }
}