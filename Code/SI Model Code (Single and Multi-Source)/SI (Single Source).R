library(igraph)

#Load the desired graph over which the infection is to be generated
#Set the working directory
load(file="./Graphs/USPG.RData")

#Code to simulate infection over a graph using SI model.

USPG_Hetero_40=list() #Replace Facebook_Hetero_2 depending on graph and infection size
USPG_Hetero_40_Time=list()

k=1
while(k<=200){ #Generate 100 infection graphs
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
	#ttt=0
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

		#ttt=ttt+1
		if(prob>runif(n=1, min=0, max=1))
		{
			time[length(unlist(perm_active))+1]=tt#as.numeric(paste(tt,ttt,sep="."))
			perm_active[length(unlist(perm_active))+1]=(inactive)
			
			#print(unlist(perm_active))				
			#print(prob)
		}
	}
	if(length(unlist(perm_active))>=length(V(graph))*0.4) #Set infection size
			{
				if(length(unlist(perm_active))<=length(V(graph))*0.42){
				USPG_Hetero_40[[length(USPG_Hetero_40)+1]]=unlist(perm_active)
				USPG_Hetero_40_Time[[length(USPG_Hetero_40_Time)+1]]=unlist(time)
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

save(USPG_Hetero_40, file="./200/USPG_Hetero_40.RData")
save(USPG_Hetero_40_Time, file="./200/USPG_Hetero_40_Time.RData")
