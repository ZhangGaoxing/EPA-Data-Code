l2 <- list()
temp <<- list()
level <- 1
score <- function (Du1, Di, l, level, temp) {
    if (level > (radius(sg))) {
            return(0)
    }
    for (item in l) {
        l2 <- append(l2, attributes(which(Ai[item,] == 1))$names)
    }
    l2 <- unlist(l2)        #感染节点与其邻居
    l3 <- setdiff(l2, l)        #感染节点的邻居
    l3 <- setdiff(l3, intersect(l3, temp))
    l3 <- as.character(l3)
    scr <- 0
    if (length(l3) == 0) {
        return(0)
    }
    for (item in l) {
        scr <- scr + (Di[item, item] / Du1[item, item]) / (1 / (1 + log(Du1[item, item])))      #一节点的level prominence
    }
    alpha <- 0
    temp <- unique(unlist(append(temp, l)))
    if (length(l3) == 0) {
        return(0)
    }
    else {
        return(scr + score(Du1, Di, l3, level + 1, temp))
    }
}


#Give paths to input underlying graphs and infection graphs
#Set the working directory
load(file = "./Graphs/Facebook.RData")
load(file = "./Infection Graphs (Single Source)/Facebook_Hetero_2.RData")
#Facebook_Hetero_2 contains infected nodes list corresponding to the underlying graph, which in this case is Facebook. 
#Replace Facebook_Hetero_2 in the rest of the code according to the graph and infection size. For example if the 
#underlying graph is Regular and infection size is 40-60%, replace it with Regular_Hetero_40.

library(igraph)

#Code for Exoneration and Prominence based Age (EPA)
Au <- as.matrix(get.adjacency(graph))        #邻接矩阵
Du <- diag(rowSums(Au))        #根据邻接矩阵的行和生成对角阵，图的度矩阵
rownames(Du) <- as.character(V(graph)$name)        #顶点名称
colnames(Du) <- as.character(V(graph)$name)

sys <- 0
est_sum <- 0
EPA_FB_Ht_2 <- list()
i <- 1
while (i <= 100) {
    source <- V(graph)[Facebook_Hetero_2[[i]][1]]$name      #感染图的第一个节点为源
    sg <- induced_subgraph(graph, Facebook_Hetero_2[[i]], impl = c("copy_and_delete"))        #感染图的子图
    radius <- radius(sg)        #感染子图的半径
    nnodes <<- length(V(sg))        #子图的节点数量
    Ai <<- as.matrix(get.adjacency(sg))        #子图的邻接矩阵
    Di <- diag(rowSums(Ai))        #子图的度矩阵
    colnames(Di) <- as.character(V(sg)$name)        #顶点名称
    rownames(Di) <- as.character(V(sg)$name)
    Du1 <- Du[rownames(Di), colnames(Di)]
    scr_list <- list()
    k <- 1
    for (node in as.character(V(sg)$name)) {        #遍历感染图的节点
        scr <- score(Du1, Di, as.character(node), level, temp)        #节点年龄
        penalty <- as.numeric(eccentricity(sg, vids = node, mode = c("all")))        #penalty年龄
        scr_list[k] <- scr / penalty        #最终的评价参数
        k <- k+1
    }
    scr_list <- unlist(scr_list)        #list转向量
    index <- which(max(scr_list) == scr_list)
    d <- distances(sg, v = as.character(V(sg)[index]$name), to = as.character(source), mode = c("all"), weights = NULL, algorithm = c("unweighted"))        #计算得到的源与真正源的距离
    est_sum <- est_sum + as.numeric(d[1])
    EPA_FB_Ht_2[[i]] <- as.numeric(d[1])
    # if (i %% 5 == 0) {
    #     save(EPA_FB_Ht_2, file = "./Result Objects/EPA_FB_Ht_2.RData")
    # }

    cat(sprintf("Current index: %d\n", i))
    cat(sprintf("Source distance: %d\n", d))
    cat("\n")
    # print(est_sum)
    i <- i + 1
}