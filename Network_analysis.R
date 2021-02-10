#########################################################
# Practical Introduction Psychological Network Analysis
#########################################################

# Install packages: 
library("bootnet")
library("qgraph")
library("psych")

# Load data: 
data("bfi")

head(bfi)
bfi <- na.omit(bfi[,1:25])

# Some items need to be recoded:
bfi$A1 <- 7 - bfi$A1 
bfi$C4 <- 7 - bfi$C4
bfi$C5 <- 7 - bfi$C5
bfi$E1 <- 7 - bfi$E1
bfi$E2 <- 7 - bfi$E2
bfi$O2 <- 7 - bfi$O2
bfi$O5 <- 7 - bfi$O5

# Sum-scores:
bfi_traits <- data.frame(Agreeableness     = rowSums(bfi[,1:5]),
                         Conscientiousness =  rowSums(bfi[,6:10]),
                         Extraversion      = rowSums(bfi[,11:15]),
                         Neuroticism       = rowSums(bfi[,16:20]),
                         Openness          = rowSums(bfi[,21:25]))

Traits <- c("Agreeableness", "Conscientiousness", "Extraversion", "Neuroticism", "Openness")

#########################################################
# Exercise 1: Estimating a partial correlation network 
#########################################################

??estimateNetwork
Network <- estimateNetwork(bfi_traits, default = "...")

# Show edge weights:
Network$graph 

#########################################################
# Exercise 2: Plotting a network model
#########################################################

par(mfrow=c(1,2))
qgraph(Network$graph, layout = "...", theme = "colorblind")
qgraph(Network$graph, layout = "...", theme = "colorblind")

# Extra plot options: 
par(mfrow=c(1,3))
qgraph(Network$graph, layout = "circle", edge.labels = TRUE, theme = "colorblind", edge.label.cex = 2)
qgraph(Network$graph, layout = "circle", vsize = 15, theme = "colorblind")
qgraph(Network$graph, layout = "circle", vsize = 20, labels = Traits, theme = "colorblind")

#########################################################
# Exercise 3: Estimate and plot a partial correlation 
# network on all 25 items
#########################################################

# Estimate a network with bootnet:
Network_25Items <- estimateNetwork(bfi, default = "...")

# Plotting a network with qgraph: 
qgraph(..., layout = "...", theme = "colorblind")

#########################################################
# EXTRA: Estimate and plot a partial correlation 
# network on all 25 items
#########################################################

# Form item clusters:
Traits <- rep(c('Agreeableness','Conscientiousness',
                'Extraversion','Neuroticism','Opennness'), each = 5)

qgraph(Network_25Items$graph, layout = "spring", theme = "colorblind", 
       groups = Traits, legend.cex = 1, vsize = 5)

#########################################################
# Exercise 4: Estimate (and plot) a network with model selection
#########################################################

Network_regularized <- estimateNetwork(bfi, default = "...")

# Plot network:
qgraph(Network_regularized$graph, layout = "spring", theme = "colorblind",
     groups = Traits)


#########################################################
# Exercise 5: Compare partial correlation network with 
# a network with model selection
#########################################################

plot_network  <- qgraph(Network_25Items$graph, layout = "spring")
plot_network_regularized <- qgraph(Network_regularized$graph, layout = "spring")

Layout <- averageLayout(plot_network, plot_network_regularized)
par(mfrow=c(1,2))
qgraph(Network_25Items$graph, layout = Layout, title = "Partial Correlation", theme = "colorblind", groups = Traits)
qgraph(Network_regularized$graph, layout = Layout, title = "Model selection", theme = "colorblind", groups = Traits)

#########################################################
# Exercise 6: Assess Stability of your network 
#########################################################

# Assess stability:
Stability <- bootnet(bfi, default = "...", nBoots = ...)
plot(..., order = "...")

#########################################################
# Exercise 7: Investigate the node strength, closeness, and betweenness 
#########################################################

# Obtain centrality plot 
??centralityPlot
centralityPlot(Network_regularized, include = "...")

#########################################################
# Exercise 8: Assess stability for the centrality measures
#########################################################

??bootnet
Boots2 <- bootnet()
plot()
