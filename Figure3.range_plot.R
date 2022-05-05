library(readxl)
library(ggpubr)
library(ggplot2)

viral_family_genomesize<- read_excel("data/Figure3.viralfamily_rangeplot.xlsx")
log_viral_family_genomesize <- read_excel("data/Figure3.viralfamily_rangeplot.xlsx",sheet = "genome size log")

#Turn 'viral family' column into a character vector
log_viral_family_genomesize$Viral_family <- as.character(log_viral_family_genomesize$Viral_family)
#Then turn it back into a factor with the levels in the correct order
log_viral_family_genomesize$Viral_family <- factor(log_viral_family_genomesize$Viral_family, levels=rev(unique(log_viral_family_genomesize$Viral_family)))

### genome size plotting
z1 <- ggplot(log_viral_family_genomesize, aes(x=Viral_family))+
  geom_linerange(aes(ymin=minimum,ymax=maximum),linetype=1,color="gray",size=1.1)+
  geom_point(aes(y=minimum),size=2,color="#18a1cd")+
  geom_point(aes(y=maximum),size=2,color="#00dca6")+
  theme_bw()+
  ylab("genome size (log10 transformed kbp)")+
  theme(axis.title.y= element_blank(), 
        text = element_text(size=15), 
        panel.border = element_rect(colour = "#7e7e7e")) +
  coord_flip()

### virion size plotting
viral_family_virion_size <- read_excel("data/Figure3.viralfamily_rangeplot.xlsx",sheet = "virus size")

#Turn 'viral family' column into a character vector
viral_family_virion_size$Viral_family <- as.character(viral_family_virion_size$Viral_family)
#Then turn it back into a factor with the levels in the correct order
viral_family_virion_size$Viral_family <- factor(viral_family_virion_size$Viral_family, levels=rev(unique(viral_family_virion_size$Viral_family)))

z2 <- ggplot(viral_family_virion_size, aes(x=Viral_family))+
  geom_linerange(aes(ymin=min,ymax=max),linetype=1,color="gray",size=1.1)+
  geom_point(aes(y=min),size=2,color="#FFA07A")+
  geom_point(aes(y=max),size=2,color="#CD5C5C")+
  theme_bw()+
  ylab("genome size (virion size nm)")+
  theme(axis.title.y= element_blank(), 
        text = element_text(size=15), 
        panel.border = element_rect(colour = "#7e7e7e")) +
  coord_flip()

pdf("plots/Figure3.virion.genome.size.pdf",width=10, height=7.5)
comb <- ggarrange(z1, z2,  
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

print(comb)
dev.off()