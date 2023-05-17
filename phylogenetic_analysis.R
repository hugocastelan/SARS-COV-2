#Author Simon Dellicur 
#This script was used for introduction of SARS-CoV-2 in Mexico  

library(diagram)
library(lubridate)
library(seraphim)
library(treeio)

writingFiles = FALSE
showingPlots = FALSE

analysis = "TreeTime_17102021"

# 1. Preparation of the discrete phylogeographic analysis

tree = readAnnotatedNexus(paste0(analysis,".tre"))
data = read.csv(paste0(analysis,".csv"), head=T)
seqIDs1 = tree$tip.label; seqIDs2 = c()
locations = rep(NA, length(seqIDs1))
collection_dates = rep(NA, length(seqIDs1))
for (i in 1:length(seqIDs1))
	{
		locations[i] = gsub("'","",unlist(strsplit(seqIDs1[i],"\\|"))[1])
		if (locations[i] == "Mexico") seqIDs2 = c(seqIDs2, seqIDs1[i])
		if (locations[i] != "Mexico") locations[i] = "other"
		collection_dates[i] = gsub("'","",unlist(strsplit(seqIDs1[i],"\\|"))[length(unlist(strsplit(seqIDs1[i],"\\|")))])
	}
tab = cbind(gsub("'","",seqIDs1),locations,collection_dates)
colnames(tab) = c("sequence_ID","location","collection_date"); txt = c()
for (i in 1:length(seqIDs1)) txt = c(txt, paste0(">",gsub("'","",seqIDs1[i])),"NNNN")
write.table(tab, paste0(analysis,".txt"), row.names=F, quote=F, sep="\t")
write(txt, paste0(analysis,".fasta"))

# 2. Analysis of the discrete phylogeographic analysis

burnIn = 51
trees = scan(paste0(analysis,".trees"), what="", sep="\n", quiet=T, blank.lines.skip=F)
indices1 = which(!grepl("tree STATE_",trees)); indices2 = which(grepl("tree STATE_",trees))
mostRecentSamplingDate = max(decimal_date(ymd(collection_dates)))
mexicoBranches_list = rep(NA,length(trees)); mexicoIntroductions_list = rep(NA,length(trees))
mexicoTipBranches_list = rep(NA,length(trees)); mexico_tMRCAs_list = list()
for (i in (burnIn+1):length(indices2))
	{
		tree1 = trees[c(indices1[1:(length(indices1)-1)],indices2[i],indices1[length(indices1)])]
		write(tree1, paste0("TEMP_sampled_tree_",i,".tree"))
		tree2 = readAnnotatedNexus(paste0("TEMP_sampled_tree_",i,".tree"))
		mexicoBranches = 0; mexicoIntroductions = 0; mexicoTipBranches = 0; mexico_tMRCAs = c()
		for (j in 1:dim(tree2$edge)[1])
			{
				if (tree2$annotations[[j]]$location == "Mexico")
					{
						mexicoBranches = mexicoBranches + 1
						index = which(tree2$edge[,2]==tree2$edge[j,1])
						if (tree2$annotations[[index]]$location != "Mexico")
							{
								mexicoIntroductions = mexicoIntroductions + 1
								tMRCA = mostRecentSamplingDate-nodeheight(tree2,tree2$edge[j,1])
								mexico_tMRCAs = c(mexico_tMRCAs, tMRCA)
							}
						if (!tree2$edge[j,2]%in%tree2$edge[,1])
							{
								mexicoTipBranches = mexicoTipBranches + 1
							}
					}
			}
		mexicoBranches_list[i] = mexicoBranches
		mexicoIntroductions_list[i] = mexicoIntroductions
		mexicoTipBranches_list[i] = mexicoTipBranches
		mexico_tMRCAs_list[[i]] = mexico_tMRCAs
		file.remove(paste0("TEMP_sampled_tree_",i,".tree"))
	}
quantiles = quantile(mexicoIntroductions_list[!is.na(mexicoIntroductions_list)],probs=c(0.025,0.975))
cat("A minimum number of ",median(mexicoIntroductions_list[!is.na(mexicoIntroductions_list)])," Delta lineage introduction event (95% HPD interval = [",
	quantiles[1],"-",quantiles[2],"])"," identified from the phylogenetic analysis of ",mexicoTipBranches," samples collected in Mexico",sep="")
	# A minimum number of 33 Delta lineage introduction events (95% HPD interval = [32-34]) identified from the phylogenetic analysis of 1,393 samples collected in Mexico

# 3. Visualisation of the discrete phylogeographic analysis

if (showingPlots)
	{
		tree = readAnnotatedNexus(paste0(analysis,".tree"))
		rootHeight = max(nodeHeights(tree)); root_time = mostRecentSamplingDate-rootHeight
		selectedLabels = c("01-11-2019","01-01-2020","01-03-2020","01-05-2020","01-07-2020","01-09-2020","01-11-2020","01-01-2021","01-03-2021","01-05-2021","01-07-2021")
		selectedDates = decimal_date(dmy(selectedLabels))
		cols = rep("gray30",dim(tree$edge)[1]); lwds = rep(0.1,dim(tree$edge)[1])
		for (i in 1:dim(tree$edge)[1])
			{
				if (tree$edge[i,1]%in%tree$edge[,2])
					{
						index = which(tree$edge[,2]==tree$edge[i,1])
						if ((tree$annotations[[index]]$location=="Mexico")&(tree$annotations[[i]]$location=="Mexico"))
							{
								cols[i] = rgb(0,104,71,255,maxColorValue=255); lwds[i] = 0.4 # green of the Mexico flag
							}
					}
			}
		pdf("Figure_1_NEW.pdf", width=7, height=7); par(oma=c(0,0,0,0), mar=c(0,0,0,0.0), lwd=0.1)
		plot(tree, type="fan", show.tip.label=F, show.node.label=F, edge.width=lwds, cex=0.6, align.tip.label=3, col="gray30", edge.color=cols)
		for (i in 1:dim(tree$edge)[1])
			{
				if (tree$annotations[[i]]$location == "Mexico")
					{
						index = which(tree$edge[,2]==tree$edge[i,1])
						if (tree$annotations[[index]]$location != "Mexico")
							{
								nodelabels(node=tree$edge[i,2], pch=16, cex=0.6, col=rgb(206,17,38,255,maxColorValue=255)) # red of the Mexico flag
								nodelabels(node=tree$edge[i,2], pch=1, cex=0.6, col="gray30", lwd=0.5)
							}
					}
			}
		add.scale.bar(x=0.0, y=0.15, length=NULL, ask=F, lwd=0.5 , lcol ="gray30", cex=0.7)
		dev.off()
		pdf("Figure_2_NEW.pdf", width=7, height=7); par(oma=c(0,0,0,0), mar=c(0,0,0,0.0), lwd=0.1)
		plot(tree, show.tip.label=F, show.node.label=F, edge.width=lwds, cex=0.6, align.tip.label=3, col="gray30", edge.color=cols)
		for (i in 1:dim(tree$edge)[1])
			{
				if (tree$annotations[[i]]$location == "Mexico")
					{
						index = which(tree$edge[,2]==tree$edge[i,1])
						if (tree$annotations[[index]]$location != "Mexico")
							{
								nodelabels(node=tree$edge[i,2], pch=16, cex=0.6, col=rgb(206,17,38,255,maxColorValue=255)) # red of the Mexico flag
								nodelabels(node=tree$edge[i,2], pch=1, cex=0.6, col="gray30", lwd=0.5)
							}
					}
			}
		axis(lwd=0.2, at=selectedDates-root_time, labels=selectedLabels, cex.axis=0.5, mgp=c(0,0.05,-0.9), lwd.tick=0.2, 
			 col.lab="gray30", col="gray30", tck=-0.005, side=1)
		dev.off()
	}

