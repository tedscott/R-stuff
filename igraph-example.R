# from Rui: example of using igraph?

library(CosmosToR)
library(dplyr)
library(igraph)
library(tictoc)

#setwd("D:/Analysis/UserCollaborationNetworks/Input/SPO")

# fresno tenant id
FresnoTenantId <- "74c90083-03c6-453a-801c-9251cdd17eb8"

#use iscope to get data for fresno
vc <- vc_connection('https://cosmos14.osdinfra.net/cosmos/ACE.proc/')

# path to stream in cosmos
TenantUserPath <- "/local/Projects/UserCollaborationNetworks/SPO/Graphes/2017/02/28/TenantUsers_Prod_RL28D.ss"
TenantNodePath <- "/local/Projects/UserCollaborationNetworks/SPO/Graphes/2017/02/28/NodeGraphAttributes_Prod_RL28D.ss"
TenantLinkPath <- "/local/Projects/UserCollaborationNetworks/SPO/Graphes/2017/02/28/WeightedGraph_Prod_RL28D.ss"

# get the data - might time out on the Links (weighted_graph is 5gb stream)
# filter users to Fresno
tic()
Users <- ss_all(vc, TenantUserPath)
Users <- Users[Users$OMSTenantId == FresnoTenantId,] #filter it immediately to save space
toc() # takes 323 sec
# save it now that we have it
write.csv(Users, "FresnoUsersFeb2017.csv")


tic()
Nodes <- ss_all(vc, TenantNodePath)
Nodes <- Nodes[Nodes$Puid %in% Users$Puid,] #filter it immediately to save space
toc() # takes 1214 sec
# save it now that we have it
write.csv(Nodes, "FresnoNodesFeb2017.csv")

###
# this iscope query will likely time out so using 
#tic()
#Links <- ss_all(vc, TenantLinkPath)
#Links <- Links[Links$FromPuid %in% Users$Puid & Links$ToPuid %in% Users$Puid,]
#toc() # takes XXX sec
# save it now that we have it
#write.csv(Links, "FresnoLinksFeb2017.csv")
Links <- read.csv("FresnoLinksFeb2017.csv", stringsAsFactors = FALSE)


# filter links to WXPO link info only
LinksWXPO <- Links %>% select(FromPuid, ToPuid, NumOneNoteDocs)#, Weight=DecayedWeightByMedianCollaborationTime)

LinksWXPO <- LinksWXPO %>% filter(NumOneNoteDocs > 1)

#build the network graph
Network <- graph_from_data_frame(d=LinksWXPO, vertices=Nodes, directed=T)

#show vertices and edges
V(Network)
E(Network)

# find those with low degree (less than 2 edges)
lowDeg <- V(Network)[degree(Network)<1]

# remove those from graph
testNetwork <- Network
testNetwork <- delete.vertices(testNetwork, lowDeg)

# plot it?
par(bg='black')
plot.igraph(testNetwork, vertex.label=NA, vertex.size = 1.5, edge.width = 0.5, 
            edge.color="#F3D3DC", edge.arrow.size = .1, asp = .6,vertex.color='#0e29f2')


V(testNetwork)
E(testNetwork)


#par(mfrow=c(2,2), mar=c(0,0,0,0))   # plot four figures - 2 rows, 2 columns
#l <- layout_with_fr(Network)
#plot.igraph(Network, layout=l, vertex.label=NA, vertex.size = 1.3, edge.width = 0.8, edge.color="#C1CDCD",edge.arrow.size = .2, asp = .6,vertex.color='#b01010', vertex.frame.color='#b01010')





# Load in data
# TenantUserMapping <- read.csv(file="TenantUserMapping.csv", header = F, sep=",", stringsAsFactors=FALSE, quote="")
# colnames(TenantUserMapping) <- c("Puid", "IsRead", "IsEditor", "IsCollaborator", "OMSTenantId", "UserType")
# TenantUserMapping <- TenantUserMapping[TenantUserMapping$OMSTenantId != "",]
# nodes <- read.csv("NodeDegrees_RL28D.csv", header = FALSE, stringsAsFactors = FALSE)
# colnames(nodes) <- c("Puid", "OutboundDegree", "InboundDegree")
# links <- read.csv("WeightedGraph_RL28D.csv", header = FALSE, stringsAsFactors = FALSE)
# colnames(links) <- c("FromPuid", "ToPuid", "Weight", "NumWordDocs", "NumExcelDocs", "NumPowerPointDocs");

# TargetTenantId
# TargetTenantId <- "463607d3-1db3-40a0-8a29-970c56230104"
# TenantUsers <- TenantUserMapping[TenantUserMapping$OMSTenantId == TargetTenantId,]
# TenantNodes <- nodes[nodes$Puid %in% TenantUsers$Puid,]
# TenantLinks <- links[links$FromPuid %in% TenantNodes$Puid & links$ToPuid %in% TenantNodes$Puid,]
#TenantNetworks <- graph_from_data_frame(d=TenantLinks, vertices=TenantNodes, directed=T) 

# par(mfrow=c(2,2), mar=c(0,0,0,0))   # plot four figures - 2 rows, 2 columns
# l <- layout_with_fr(TenantNetworks)
# plot.igraph(TenantNetworks, layout=l, vertex.label=NA, vertex.size = 1.3, edge.width = 0.8, edge.color="#C1CDCD",edge.arrow.size = .2, asp = .6,vertex.color='#b01010', vertex.frame.color='#b01010')

