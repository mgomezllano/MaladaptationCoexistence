
pacman::p_load(plyr, lme4, ggplot2, gridExtra, car, tidyverse, emmeans, grid)

setwd("~/Dropbox/Transplants_Coexistence/DRYAD")

####################
####################
####################
### LARVAE EXPERIMENT

### GROWTH RATE

D <- read.csv(file = "Larvae.csv")
head(D)

D$Lake <- as.factor(D$Lake)
D$Transplant <- as.factor(D$Transplant)
D$Treatment <- as.factor(D$Treatment)
D$Species <- as.factor(D$Species)
D$Cage <- as.factor(D$Cage)

# GROWTH RATE
GR <- lmer(gr_rate ~ Lake * Transplant * Treatment * Species + (1|Rep), data = D)

summary(GR)
Anova(GR, type = 3)

### DEATH RATE
DR <- lmer(dRate ~ Lake * Transplant * Treatment * Species + (1|Rep), data = D)

summary(DR)
Anova(DR, type = 3)

yDeath <- expression('Per capita mortality (day'^'-1'~')')
yGrowth <- expression('Per capita growth (day'^'-1'~')')

D2 <- ddply(D, .(Lake, Source, Species, Transplant), summarise, mDeath = mean(dRate),
            seD = sd(dRate)/sqrt(length(dRate)), mGrowth = mean(gr_rate), 
            seG = sd(gr_rate)/sqrt(length(gr_rate)))

p1 <- ggplot(D2, aes(Transplant, mGrowth, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), col = "black") +
  geom_errorbar(aes(ymin = mGrowth - seG, ymax = mGrowth + seG),
                position = position_dodge(width = 0.9), width = 0) +
  scale_fill_manual(label = c(expression(italic("E. exsulans")),
                              expression(italic("E. traviatum"))), name = "",
                      values = c("#F2BD68", "#6EBECC")) +
  facet_wrap("Lake") + ylab(yGrowth) + xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        text = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 14, color = "black")) 

D3 <- ddply(D, .(Lake, Source, Species, Treatment, Transplant), summarise, mDeath = mean(dRate),
            seD = sd(dRate)/sqrt(length(dRate)), mGrowth = mean(gr_rate), 
            seG = sd(gr_rate)/sqrt(length(gr_rate)))

p2 <- ggplot(D3, aes(Treatment, mGrowth, fill = Species, shape = Transplant)) +
  geom_errorbar(aes(ymin = mGrowth - seG, ymax = mGrowth + seG),
                position = position_dodge(width = 0.5), width = 0) +
  geom_line(aes(Treatment, mGrowth,
                group = interaction(Transplant, Species)), 
            position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  scale_shape_manual(values = c(21, 22), guide = 'none') +
  scale_fill_manual(label = c(expression(italic("E. exsulans")),
                              expression(italic("E. traviatum"))), name = "",
                    values = c("#F2BD68", "#6EBECC")) +
  facet_wrap("Lake") + ylab(yGrowth) + xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        text = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 14, color = "black")) +
  guides(fill=guide_legend(override.aes=list(shape=21))) 

pdf("Fig1.pdf", width = 10, height = 10)
grid.arrange(p1, p2, ncol = 1)
dev.off()

p3 <- ggplot(D2, aes(Transplant, mDeath, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), col = "black") +
  geom_errorbar(aes(ymin = mDeath - seD, ymax = mDeath + seD),
                position = position_dodge(width = 0.9), width = 0) +
  scale_fill_manual(label = c(expression(italic("E. exsulans")),
                              expression(italic("E. traviatum"))), name = "",
                    values = c("#F2BD68", "#6EBECC")) +
  facet_wrap("Lake") + ylab(yDeath) + xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        text = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 14, color = "black"))

p4 <- ggplot(D3, aes(Treatment, mDeath, fill = Species, shape = Transplant)) +
  geom_errorbar(aes(ymin = mDeath - seD, ymax = mDeath + seD),
                position = position_dodge(width = 0.5), width = 0) +
  geom_line(aes(Treatment, mDeath,
                group = interaction(Transplant, Species)), 
            position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  scale_shape_manual(values = c(21, 22), guide = 'none') +
  scale_fill_manual(label = c(expression(italic("E. exsulans")),
                              expression(italic("E. traviatum"))), name = "",
                    values = c("#F2BD68", "#6EBECC")) +
  facet_wrap("Lake") + ylab(yDeath) + xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        text = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 14, color = "black")) +
  guides(fill = guide_legend(override.aes = list(shape = 21))) 

pdf("Fig2.pdf", width = 10, height = 10)
grid.arrange(p3, p4, ncol = 1)
dev.off()

####################
####################
####################
### PREDATOR PREFERENCE

pred_all <- read.csv('predation_final.csv')
head(pred_all)

pred_all$Lake <- as.factor(pred_all$Lake)
pred_all$Treatment <- as.factor(pred_all$Treatment)
pred_all$Species <- as.factor(pred_all$Species)
pred_all$Rep <- as.factor(pred_all$Rep)

PP <- lmer(dRate ~ Fish_length + Lake * Treatment * Species + (1|Rep), data = pred_all)

summary(PP)
Anova(PP, type = 3)


tst1 <- ddply(pred_all, .(Species, Treatment, Lake), summarise,
              mAlive = mean(na.omit(dRate)), 
              se = sd(na.omit(dRate))/sqrt(length(na.omit(dRate))))

yDeath <- expression('Per capita mortality (day'^'-1'~')')

fPre <- ggplot(tst1, aes(Treatment, mAlive, fill = Species, group = Species)) +
  geom_errorbar(aes(Treatment, ymin = mAlive - se, ymax = mAlive + se, width = 0.1),
                position = position_dodge(width = 0.5)) +
  geom_line(aes(Treatment, mAlive), col = "Black",
            position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size=5, shape = 21) +
  facet_wrap("Lake") + xlab("") + ylab(yDeath) +
  scale_fill_manual(label = c(expression(italic("E. exsulans")),
                              expression(italic("E. traviatum"))), name = "",
                    values = c("#F2BD68", "#6EBECC")) +
  theme_bw() + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        text = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 14, color = "black"),
        legend.position = c(0.5, 0.85)) 

pdf("Fig3.pdf", width = 8, height = 6)
fPre
dev.off()

####################
####################
####################


### ENVIRONMENT

lks <- read.csv("Lakes.csv")

bk <- data.frame(t(lks[1,]))
colnames(bk) <- "vals"
fy <- data.frame(t(lks[2,]))
colnames(fy) <- "vals"
lc <- data.frame(t(lks[3,]))
colnames(lc) <- "vals"
Lk_all <- rbind(bk, fy, lc)
Lk_all$env <- rep(rownames(bk), 3)
Lk_all$Lake <- c(rep("Bob Kidd", nrow(bk)), rep("Fayetteville", nrow(fy)), rep("Lincoln", nrow(lc)))
Lk_all <- Lk_all[which(Lk_all$env != "DO_Percent" & Lk_all$env != "DO_ppm"),]

Lk_all <- Lk_all[which(Lk_all$env == "Fish" | 
                         Lk_all$env == "Prey" |
                         Lk_all$env == "Larvae" |
                         Lk_all$env == "Macrophyte"),]
Lk_all$vals <- as.numeric(Lk_all$vals)

pf <- ggplot(Lk_all[which(Lk_all$env == "Fish"),], aes(Lake, vals)) +
  geom_bar(stat = 'identity', col = 'black') +
  xlab("") + ylab("Fish density") +
  theme_bw() +
  geom_text(aes(label = vals), vjust = -0.2, size = 5) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        text = element_text(size = 14, color = "black"),
        legend.position = 'none') +
  annotate('text', x = 0.5, y = 105, label = 'C', size = 7)

pp <- ggplot(Lk_all[which(Lk_all$env == "Prey"),], aes(Lake, vals)) +
  geom_bar(stat = 'identity', col = 'black') +
  xlab("") + ylab("Prey density") +
  theme_bw() + geom_text(aes(label = vals), vjust = -0.2, size = 5) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        text = element_text(size = 14, color = "black"),
        legend.position = 'none') +
  annotate('text', x = 0.5, y = 700, label = 'D', size = 7)

pl <- ggplot(Lk_all[which(Lk_all$env == "Larvae"),], aes(Lake, vals)) +
  geom_bar(stat = 'identity', col = 'black') +
  xlab("") + ylab("Damselfly density") +
  theme_bw() + geom_text(aes(label = vals), vjust = -0.2, size = 5) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        text = element_text(size = 14, color = "black"),
        legend.position = 'none') +
  annotate('text', x = 0.5, y = 1010, label = 'E', size = 7)

pm <- ggplot(Lk_all[which(Lk_all$env == "Macrophyte"),], aes(Lake, vals)) +
  geom_bar(stat = 'identity', col = 'black') +
  xlab("") + ylab("Macrophyte density") +
  theme_bw() + geom_text(aes(label = vals), vjust = -0.2, size = 5) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        text = element_text(size = 14, color = "black"),
        legend.position = 'none') +
  annotate('text', x = 0.5, y = 85, label = 'F', size = 7)

# blank <- grid.rect(gp=gpar(col="white"))

pdf("FigS1.pdf", width = 13, height = 15)
grid.arrange(pf, pp, pl, pm, ncol = 2)
dev.off()


#### Correlations

dat_sum <- ddply(D, .(Lake, Transplant, Species), summarise, gr_rate = mean(gr_rate), d_rate = mean(dRate))

lake <- unique(D$Lake)
transp <- unique(D$Transplant)
spp <- unique(D$Species)

correlation <- rep(10, 12)
p_vals <- rep(10, 12)
z=1 
for(i in 1:length(lake)){
  for(j in 1:length(transp)){
    for(e in 1:length(spp)){
      tst <- D[which(D$Lake==lake[i] & D$Transplant == transp[j] & D$Species == spp[e]), ]
      as <- cor.test(tst$gr_rate, tst$dRate)
      correlation[z] = as$estimate
      p_vals[z] = as$p.value
      z=z+1
    }
  }
}
dat_sum <- data.frame(dat_sum, correlation, p_vals)

dat_sum

pdf("FigS2.pdf", width = 10, height = 8)
ggplot(D, aes(gr_rate, dRate, col = Species)) +
  geom_point(size = 3) + 
  scale_color_manual(label = c(expression(italic("E. exsulans")), 
                                expression(italic("E. traviatum"))), 
                     name = "", values = c("#F2BD68", "#6EBECC")) +
  xlab(yGrowth) + ylab(yDeath) +
  facet_wrap(facets = c('Transplant','Lake')) +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        text = element_text(size = 15, color = "black"))
dev.off()

