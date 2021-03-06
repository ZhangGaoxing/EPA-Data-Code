#Give paths to input underlying graphs and infection graphs
#Set the working directory
load(file="./Graphs/Jazz.RData")
load(file="./Infection Graphs (Multiple Source)/2/Jazz_Hetero_10_2S.RData") 
#Jazz_Hetero_10_2S contains infected nodes list corresponding to the underlying graph, which in this case is Jazz. 

library(igraph)
library(gtools)

#Code for K-Means based Distance Centrality heuristic (DC) 

nos=2 # Iniitialize the number of sources

DC_Jazz_Ht_10_2S_DR=list()
DC_Jazz_Ht_10_2S_AED=list()

est_sum=0
out_min_sum=0
f1_sum=0
i=1
while(i<=100){
	sources=V(graph)[Jazz_Hetero_10_2S[[i]][1:nos]]$name
	sg=induced_subgraph(graph, Jazz_Hetero_10_2S[[i]], impl = c("copy_and_delete"))
	radius=radius(sg)
	nnodes<<-length(V(sg))
	old_max=0
	conv=F
	old_centers=as.character(c((sample(V(sg))[1:nos]$name)))
	iterations=1
	convergence=F
	while(!convergence){
	part=list()
	h=1
	while(h<=length(old_centers)){
		part[[h]]=old_centers[h]
		h=h+1
	}
	for(node in as.character(V(sg)$name))
	{
		dist=distances(sg, v = node, to = old_centers, mode = c("all"), weights=NULL, algorithm = c("unweighted"))
		which=which(min(dist)==dist)[1]
		if(length(which)>1){
			for(wh in which){
				part[[wh]]=c(part[[wh]],node)
			}
		}
	else{
		part[[which]]=c(part[[which]],node)}
	}
	new_centers=list()
	part=lapply(part, function(x) x[-1])
	for(pt in part){
		tr=list()
		for(p in pt)
		{
			tr[length(tr)+1]=sum(distances(sg, v = p, to = pt, mode = c("all"), weights=NULL, algorithm = c("bellman-ford")))
		}
		tr=unlist(tr)
		nc=which(min(tr)==tr)
		nc=which(min(tr)==tr)[1]
		new_centers[length(new_centers)+1]=pt[nc]
	}
	new_centers=unlist(new_centers)
	p=(old_centers==new_centers)
	convergence=prod(p)
	if(iterations>=10){
		convergence=T
	}
	old_centers=new_centers
	iterations=iterations+1
}
indices=unlist(new_centers)
seeds=indices
print(paste("number of estimated sources: ",length(indices)))
outer_sum=10000000000
perm=permutations(n=nos,r=nos,v=seeds)
pit=1
while(pit<=nrow(perm))
{
	sit=1
	inner_sum=0
	while(sit<=length(perm[pit,])){
		inner_sum=inner_sum+as.numeric(distances(sg, v = as.character(sources[sit]), to = as.character(perm[pit,sit]), mode = c("all"), weights=NULL, algorithm = c("unweighted")))
		sit=sit+1
	}
	inner_sum=inner_sum/length(sources)
	if(inner_sum<outer_sum){
		outer_sum=inner_sum
	}
	pit=pit+1

}
print(i)
print(outer_sum)
est_sum=est_sum+outer_sum
print(est_sum)

dr=length(intersect(as.character(sources),as.character(seeds)))/length(sources)
print(dr)
print("_______________")
DC_Jazz_Ht_10_2S_DR[[i]]=dr
save(DC_Jazz_Ht_10_2S_DR, file="./Result Objects/DC_Jazz_Ht_10_2S_DR.RData")

DC_Jazz_Ht_10_2S_AED[[i]]=outer_sum
save(DC_Jazz_Ht_10_2S_AED, file="./Result Objects/DC_Jazz_Ht_10_2S_AED.RData")
	i=i+1	
}
