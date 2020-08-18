library(igraph)

#Load the desired graph over which the infection is to be generated
#Set the working directory
load(file="./Graphs/Random.RData")

#Code to simulate infection over a graph using SI model.

Random_Hetero_30=list() #Replace Random_Hetero_2 depending on graph and infection size
Random_Hetero_30_Time=list()

k=1
while(k<=50){ #Generate 100 infection graphs
	print(k)
random=as.numeric(c((sample(V(graph))[1]))) #Randomly pick one source

t=0
neighbors=list()
temp_active=list()
perm_active=list()
temp=list()
adj=list()
perm_active=random
time=list()
tt=0
time=tt

while(1)
{
	t=t+1
	neighbors=list()
	for(node in unlist(perm_active))
	{
		adj=adjacent_vertices(graph,(node))
		i=1
		while(i<=length(unlist(adj)))
		{
			neighbors[length(unlist(neighbors))+1]=unlist(adj)[i]
			i=i+1
		}
	}
	neighbors=unique(unlist(neighbors))
	i=1	
	rel_neighbors=setdiff(unlist(neighbors),unlist(perm_active))
	#print(t)
	#print(neighbors)
	#print(rel_neighbors)
	temp=perm_active
	#temp_active=list()
	tt=tt+1
	ttt=0
	for(inactive in rel_neighbors)
	{	
		count=0
		edge_list=list()
		for(active in temp)
		{	
			
			
			if(are.connected(graph,(inactive),(active)))
			{
				count=count+1
			#	print("c")
				#print(count)
				e=get.edge.ids(graph, c((inactive),(active)))
				edge_list[length(unlist(edge_list))+1]=E(graph)[e]$weight

				#print(unlist(edge_list))
				#print(active)
				#print(inactive)
			}
			#print(edge_list)
		}
		if(count>1)
		{
			i=1
			prob=1
			while(i<=count)
			{
				prob=prob*(1-unlist(edge_list)[i])
			#	print(prob)
				i=i+1
			}
			prob=1-prob
			#print(prob)
		}
		else
		{

			prob=unlist(edge_list)
		}

		ttt=ttt+1
		if(prob>runif(n=1, min=0, max=1))
		{
			time[length(unlist(perm_active))+1]=as.numeric(paste(tt,ttt,sep="."))
			perm_active[length(unlist(perm_active))+1]=(inactive)
			
			#print(unlist(perm_active))				
			#print(prob)
		}
	}
	if(length(unlist(perm_active))>=length(V(graph))*0.25) #Set infection size
			{
				if(length(unlist(perm_active))<=length(V(graph))*0.35){
				Random_Hetero_30[[length(Random_Hetero_30)+1]]=unlist(perm_active)
				Random_Hetero_30_Time[[length(Random_Hetero_30_Time)+1]]=unlist(time)
				print(unlist(perm_active))	
				k=k+1
				break
			}
			else{
				
				print(length(unlist(perm_active)))
				print("discarded")
				break
			}

			
			}		
}
}

# save(Random_Hetero_30, file="./200_t/Random_Hetero_30.1.RData")
# save(Random_Hetero_30_Time, file="./200_t/Random_Hetero_30_Time.1.RData")

# load("./200_t/Random_Hetero_30.4.RData")
# load("./200_t/Random_Hetero_30_Time.4.RData")
# f4 <- Random_Hetero_30
# t4 <- Random_Hetero_30_Time

# Random_Hetero_30 <- c(f1,f2,f3,f4)
# Random_Hetero_30_Time <- c(t1,t2,t3,t4)

# save(Random_Hetero_30, file="./200_t/Random_Hetero_30.RData")
# save(Random_Hetero_30_Time, file="./200_t/Random_Hetero_30_Time.RData")