#Landcover, ANN, matrix, and habitat ####

# Load necessary libraries
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(tidyverse)

# Set working directory or file path for input files####
frag_file <- "FragmentMetrics.xlsx"
sample_file <- "frag_comm_enviro_trait.xlsx"

#sitekey links IDs from ArcGIS spatial assessments to the UID used for the
  #water quality and biotic community data. Probably don't change the guess_max
  #unless you want to redefine the data type later
sitekey <- read_excel("FragmentMetrics.xlsx", sheet = "SiteKey", guess_max = 10000)


frag_file <- "FragmentMetrics.xlsx"

sample_file <- "frag_comm_enviro_trait.xlsx"



# Read in the fragmentation metrics. These are the median distance, mean distance,
  #and number of habitat patches (stream segments) within the given buffer radius.

fragmetrics <- read.csv("FragFINAL_MultiBuffer_Metrics.csv")



# Join the ANN/Habitat metrics by buffer

scales <- list()

scales.raw <- list()

moops <- list()

for (n in c(1, 5, 10, 20, 50)) {
  
  # Filter for this buffer size and keep only needed columns
  
  buffer_frag <- fragmetrics %>%
    
    filter(Buffer == paste0(n, "km")) %>%
    
    select(IDalt, ANN = MeanDist, Habitat = TotalLength_m, Patches = NumLines ) %>%
    
    right_join(sitekey, by = "IDalt") %>%  # Use right_join to keep all sites
    
    mutate(
      
      ANN = ifelse(is.na(ANN), n * 1000, ANN),  # Assign buffer size in meters
      
      Habitat = ifelse(is.na(Habitat), 0, Habitat) ,
      
      Patches = ifelse(is.na(Patches), 0, Patches),
      
      Patches = ifelse(Patches > 999, 1000, Patches) # Optional: set missing habitat to 0
      
    ) %>%
    
    select(Site, ANN, Habitat, Patches)
  
  
  
  spatial_vars <- buffer_frag
  
  # Load and process landcover data
  
  
  
  # LC2006
  
  lc2006 <- read_excel(frag_file, sheet = paste0("LC2006_",n,"km")) %>%
    
    rename(IDalt = ORIG_FID) %>%
    
    left_join(sitekey, by = "IDalt") %>%
    
    mutate(
      
      Matrix = rowSums(select(., VALUE_21, VALUE_22, VALUE_23, VALUE_24, VALUE_82), na.rm = TRUE),
      
      Develop = rowSums(select(., VALUE_21,VALUE_22, VALUE_23, VALUE_24), na.rm = TRUE),
      ExtraDevelop = rowSums(select(., VALUE_22, VALUE_23, VALUE_24), na.rm = TRUE),
      Agriculture = VALUE_82
      
    ) %>%
    
    select(Site, Matrix, Develop, Agriculture, ExtraDevelop) %>%
    
    #  distinct(Site, .keep_all = TRUE) %>%  # Deduplicate here
    
    rename_with(~ paste0(., "_2006"), -Site)
  
  
  
  # LC2021
  
  lc2021 <- read_excel(frag_file, sheet = paste0("LC2021_",n,"km")) %>%
    
    rename(IDalt = ORIG_FID) %>%
    
    left_join(sitekey, by = "IDalt") %>%
    
    mutate(
      
      Matrix = rowSums(select(., VALUE_21, VALUE_22, VALUE_23, VALUE_24, VALUE_82), na.rm = TRUE),
      
      Develop = rowSums(select(., VALUE_21,VALUE_22, VALUE_23, VALUE_24), na.rm = TRUE),
      ExtraDevelop = rowSums(select(., VALUE_22, VALUE_23, VALUE_24), na.rm = TRUE),
      Agriculture = VALUE_82
      
    ) %>%
    
    select(Site, Matrix, Develop, Agriculture, ExtraDevelop) %>%
    
    # distinct(Site, .keep_all = TRUE) %>%  # Deduplicate here
    
    rename_with(~ paste0(., "_2021"), -Site)
  
  
  
  # Average values between 2006 and 2021 — safe full join
  
  lc_avg <- full_join(lc2006, lc2021, by = "Site") %>%
    
    mutate(
      
      Matrix_avg = rowMeans(select(., Matrix_2006, Matrix_2021), na.rm = TRUE),
      
      Develop_avg = rowMeans(select(., Develop_2006, Develop_2021), na.rm = TRUE),
      ExtraDevelop_avg = rowSums(select(., ExtraDevelop_2006,ExtraDevelop_2021), na.rm = TRUE),
      Agriculture_avg = rowMeans(select(., Agriculture_2006, Agriculture_2021), na.rm = TRUE)
      
    ) %>%
    
    select(Site, Matrix_avg, Develop_avg, Agriculture_avg,ExtraDevelop_avg)
  
  
  
  # Merge landcover and spatial variables
  
  landcover_data <- spatial_vars %>%
    
    left_join(lc2006, by = "Site", na_matches = "never") %>%
    
    left_join(lc2021, by = "Site", na_matches = "never") %>%
    
    left_join(lc_avg, by = "Site", na_matches = "never")
  
  
  
  # Load sample metadata from both community datasets, deduplicate, and combine
  
  sample_info_epa <- read_excel(sample_file, sheet = "EPACommunity") %>%
    
    mutate(Site = as.character(Site)) %>%
    
    select(Site, Year)
  
  
  
  sample_info_occ <- read_excel(sample_file, sheet = "OCCCommunity") %>%
    
    mutate(Site = as.character(Site)) %>%
    
    select(Site, Year)
  
  
  
  sample_info <- bind_rows(sample_info_epa, sample_info_occ) %>%
    
    distinct(Site, .keep_all = TRUE)
  
  
  
  # Join year and assign correct landcover set
  
  landcover_final <- landcover_data %>%
    
    mutate(Site = as.character(Site)) %>%
    
    left_join(sample_info, by = "Site", na_matches = "never") %>%
    
    mutate(
      
      Matrix = case_when(
        
        Year <= 2008 ~ Matrix_2006,
        
        Year >= 2016 ~ Matrix_2021,
        
        TRUE ~ Matrix_avg
        
      ),
      
      Develop = case_when(
        
        Year <= 2008 ~ Develop_2006,
        
        Year >= 2016 ~ Develop_2021,
        
        TRUE ~ Develop_avg
        
      ),
      ExtraDevelop = case_when(
        
        Year <= 2008 ~ ExtraDevelop_2006,
        
        Year >= 2016 ~ ExtraDevelop_2021,
        
        TRUE ~ ExtraDevelop_avg
        
      ),
      
      
      Agriculture = case_when(
        
        Year <= 2008 ~ Agriculture_2006,
        
        Year >= 2016 ~ Agriculture_2021,
        
        TRUE ~ Agriculture_avg
        
      )
      
    ) %>%
    
    select(Site, Year, ANN, Habitat, Matrix, Patches, Develop, ExtraDevelop, Agriculture)
  
  
  
  # View result
  
  print(head(landcover_final))
  
  #View(landcover_final)
  
  landcover <- landcover_final %>%
    
    distinct(Site, .keep_all = TRUE)
  
  
  
  #saveRDS(landcover,"landcover1FINAL.rds")
  
  
  
  #Calculate fragmentation from ANN & Habitat ####
  
  library(glmmTMB)
  
  
  
  #landcover <- readRDS("landcover1FINAL.rds")
  
  landcover2 <- landcover
  
  landcover <- landcover %>%

    mutate(across(c(Matrix, ANN, Habitat, Patches, ExtraDevelop,Develop, Agriculture), ~ as.numeric(scale(.))))

  
  
  
  
  
  
  landcover$ANN <- landcover$ANN - min(landcover$ANN) + 1
  
  #hab <- landcover$Habitat/sqrt(landcover$Patches)
  
  
  
  moop <- glmmTMB(ANN~Habitat*Patches, data=landcover, family=lognormal,
                  
                  ziformula = ~0,
                  
                  control=glmmTMBControl(optimizer=optim,
                                         
                                         optCtrl=list(maxit=1e8),
                                         
                                         optArgs=list(method="BFGS")),
                  
                  na.action = "na.omit")
  
  
  
  
  
  
  
  moops[[n]] <- moop
  
  
  
  #Replace previous "fragment" with new, improved "Fragment"
  
  landcover$Fragment <- scale(residuals(moop, type = "pearson"))
  
  
  
  #landcover <- landcover %>% filter(abs(Fragment) < 5)
  
  
  
  
  
  scales[[n]] <- landcover
  
  scales.raw[[n]] <-landcover2
  
  
  
}


#PCA of abiotic factors, extract some enviro components ####
sample_file <- "frag_comm_enviro_trait.xlsx"
abiotic <- c(
  "DO",
  "Temp",
  "Turbidity",
  "Alkalinity",
  "Conductivity",
  "pH",
  "Sulfates",
  "Ammonia",
  "Nitrate",
  "Nitrite",
  "TKN",
  "TP",
  "TSS",
  "HabitatScore"
)

occ <- read_excel(sample_file, sheet = "OCCEnviro")
epa <- read_excel(sample_file, sheet = "EPAEnviro")

occ.enviro <- data.frame(scale(occ[,abiotic]))
epa.enviro <- data.frame(scale(epa[,abiotic]))

occ.enviro$Site <- occ$Site
epa.enviro$Site <- epa$Site

enviro <- rbind(occ.enviro,epa.enviro)

pca_data <- enviro %>% 
  drop_na()

set.seed(123)
pca <- prcomp(pca_data[,abiotic])

enviro.output <- data.frame(cbind(Site=pca_data$Site,pca$x[,1:2]))

enviro.output$PC1 <- scale(as.numeric(enviro.output$PC1))
enviro.output$PC2 <- scale(as.numeric(enviro.output$PC2))


# Creating trait group relative abundances ####
occ.comm <- read_excel(sample_file, sheet = "OCCCommunity")
epa.comm <- read_excel(sample_file, sheet = "EPACommunity")

epa.comm <- epa.comm %>% 
  filter(Basin==11) %>% 
  filter(Strahler < 4)

epa.comm$Site <- factor(epa.comm$Site)
occ.comm$Site <- factor(occ.comm$Site)

epa.comm$Source <- "EPA"
occ.comm$Source <- "OCC"

comm.data <- data.frame(merge(epa.comm, occ.comm, all = T))
comm.data[, 6:ncol(comm.data)][is.na(comm.data[, 6:ncol(comm.data)])] <- 0

traits <- read_excel(sample_file, sheet = "Traits")

bugs <- intersect(traits$Taxa, colnames(comm.data))


comm.data2 <- pivot_longer(comm.data, cols = bugs, 
                           names_to = "Taxa",
                           values_to = "Abundance")

comm.data3 <- left_join(comm.data2, traits[,c("Taxa","TraitGroup")],
                        by="Taxa", na_matches = "never")

final.comm <- comm.data3 %>% select(Site,Source,Taxa,Abundance,TraitGroup)

#correlations #####
for (buffer in c(5, 10, 20)) {
  
  print(buffer)
  
  semifinal.data <- final.comm %>% 
    left_join(enviro.output, by = "Site", na_matches ="never") %>%
    distinct(.keep_all = TRUE)
  
  final.data <- semifinal.data %>%
    left_join(scales[[buffer]], by="Site",na_matches="never") %>%
    distinct(.keep_all = TRUE)
  
  #correct the variable types
  final.data$Year <- as.factor(final.data$Year)
  final.data$Fragment <- as.numeric(final.data$Fragment)
  final.data$Matrix   <- as.numeric(final.data$Develop)
  final.data$Habitat  <- as.numeric(final.data$Habitat)
  final.data$PC1      <- as.numeric(final.data$PC1)
  final.data$PC2      <- as.numeric(final.data$PC2)
  
  #SCALED IN THE LANDCOVER PART
  # final.data <- final.data %>%
  #   mutate(across(c(Habitat, Matrixt), scale)) 
  # 
  
  final.data <- final.data %>% filter(abs(Fragment) < 5 &
                                        abs(Habitat) < 5 &
                                        abs(Matrix) < 5 &
                                        abs(PC1) < 5 &
                                        abs(PC2) < 5)
  
  
  print(cor(final.data[c("Fragment","Matrix","Habitat","PC1","PC2")]))
  
}  
  
# Combine everything ####
Models <- list()
modelsum <- list()
anovas <- list()
all_predict <- list()
cis <- list()
Frag.Hab.Mat.df <- list()
Frag.Hab.Mat.plot <- list()
trait.lineplot <- list()
matrix.lineplot <- list()
Frag.Hab.TG.df <- list()
Frag.Hab.TG.plot <- list()
Frag.Hab.Mat.plot.nohigh <- list()

for (buffer in c(5, 10, 20)) {
  
  print(buffer)
  
  semifinal.data <- final.comm %>% 
    left_join(enviro.output, by = "Site", na_matches ="never") %>%
    distinct(.keep_all = TRUE)
  
  final.data <- semifinal.data %>%
    left_join(scales[[buffer]], by="Site",na_matches="never") %>%
    distinct(.keep_all = TRUE)
  
  #correct the variable types
  final.data$Year <- as.factor(final.data$Year)
  final.data$Fragment <- as.numeric(final.data$Fragment)
  final.data$Matrix   <- as.numeric(final.data$Develop)
  final.data$Habitat  <- as.numeric(final.data$Habitat)
  final.data$PC1      <- as.numeric(final.data$PC1)
  final.data$PC2      <- as.numeric(final.data$PC2)
  
  #SCALED IN THE LANDCOVER PART
  # final.data <- final.data %>%
  #   mutate(across(c(Habitat, Matrixt), scale)) 
  # 
  
  final.data <- final.data %>% filter(abs(Fragment) < 5 &
                                        abs(Habitat) < 5 &
                                        abs(Matrix) < 5 &
                                        abs(PC1) < 5 &
                                        abs(PC2) < 5)
  
  
  cor(final.data[c("Fragment","Matrix","Habitat","PC1","PC2")])
  
  
  final.data$binary <- ifelse(final.data$Abundance > 0 , 1, 0)
  # 
  # final.data2 <- final.data %>%
  #   group_by(Site, TraitGroup, PC1, PC2, Year, ANN, Habitat, Matrix, Develop, Agriculture, Fragment, ExtraDevelop) %>%
  #   summarise(Abundance = sum(Abundance, na.rm = TRUE),
  #             Richness = sum(binary, na.rm=T),.groups = "drop_last") %>%
  #   group_by(Site) %>%
  #   mutate(TotalAbundance = sum(Abundance, na.rm = TRUE),
  #          RelAbundance = Abundance / TotalAbundance,
  #          TotalRichness = sum(Richness, na.rm=T),
  #          RelRichness = Richness/TotalRichness) %>%
  #   ungroup() %>%
  #   mutate(RelAbundance = case_when(
  #     RelAbundance == 0 ~ RelAbundance + 1e-5,
  #     RelAbundance == 1 ~ RelAbundance - 1e-5,
  #     TRUE ~ RelAbundance),
  #     RelRichness = case_when(
  #       RelRichness == 0 ~ RelRichness + 1e-5,
  #       RelRichness == 1 ~ RelRichness - 1e-5,
  #       TRUE ~ RelRichness
  #   ))%>% filter(Abundance < 3000)
  # 
  # 
  # final.data$logabund <- log(1+final.data$Abundance)
  # 
  # final.data2$logabund <- log(1+final.data2$Abundance)
  # 
  # final.data$binary <- ifelse(final.data$Abundance > 0 , 1, 0)
  # 
  # final.data2$binary <- ifelse(final.data2$Abundance > 0 , 1, 0)
  # 
  # colnames(final.data)
  
  final.data <- final.data %>% filter(Abundance < 1000)
  
  
 
  
  model2 <- glmmTMB(Abundance~Fragment*Habitat*TraitGroup+
                      Fragment*Habitat*Matrix+
                      PC1+PC2+(1|Year),
                    ziformula = ~1,
                    control=glmmTMBControl(optimizer=optim,
                                           optCtrl=list(maxit= 1e9),
                                           optArgs=list(method="L-BFGS-B")),
                    family=nbinom2(),
                    data=final.data)
  
  
  
  hab_q <- quantile(final.data$Habitat, probs = c(0.01, 0.5, 0.99), na.rm = TRUE)
  emm <- emmeans::emmeans(
    model2,
    ~ Fragment | TraitGroup * Habitat,
    at = list(
      Fragment = seq(min(final.data$Fragment, na.rm = TRUE),
                     max(final.data$Fragment, na.rm = TRUE),
                     length.out = 50),
      Habitat = as.numeric(hab_q),
      PC1 = mean(final.data$PC1, na.rm = TRUE),
      PC2 = mean(final.data$PC2, na.rm = TRUE),
      Matrix = mean(final.data$Matrix, na.rm=TRUE)
    ),
    type = "response"
  )
  
  emm_df <- as.data.frame(emm)
  
  emm_df <- emm_df %>% mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
                                                   Habitat == median(Habitat) ~ "Medium",
                                                   Habitat == min(Habitat) ~ "Low"),
                              Habitat = factor(Habitat,levels = c("High","Medium","Low")),
                              TraitGroup = factor(
                                str_wrap(TraitGroup, width = 20),   # 👈 wrap labels here
                                levels = str_wrap(c(
                                  "Big & winged",
                                  "Slower reproducing, stationary flies", 
                                  "Small beetles & bugs",
                                  
                                  "Drift-prone, quicker reproducing flies",
                                  "Long-lived & passive"
                                ), width = 20)))
  
  
  trait.lineplot[[buffer]] <- ggplot() +
    # geom_point(data=final.data, aes(x=Fragment, y=Abundance), color="black")+
    geom_line(data=emm_df, aes(x = Fragment, y = response, color = factor(Habitat)), linewidth=1) +
    geom_ribbon(data=emm_df, aes(x = Fragment,ymin = response-SE, ymax = response+SE, fill = factor(Habitat)),
                alpha = 0.2, color = NA) +
    facet_wrap(~TraitGroup, scales = "free_y", ncol=5) +
    scale_color_manual(values = c("black", "gray30","gray60")) +
    scale_fill_manual(values = c("black", "gray30","gray60"))  +
    labs(x = "Fragmentation", y = "Predicted Abundance",
         color = "Habitat amount", fill = "Habitat amount") +
    theme_classic()
  
  
  
  mat_q <- quantile(final.data$Matrix, probs = c(0.01, 0.5, 0.99), na.rm = TRUE)
  emm2 <- emmeans::emmeans(
    model2,
    ~ Fragment | Matrix* Habitat,   # vary Matrix, condition plots by Matrix
    at = list(
      Fragment = seq(min(final.data$Fragment, na.rm = TRUE),
                     max(final.data$Fragment, na.rm = TRUE),
                     length.out = 50),
      Matrix   = as.numeric(mat_q),
      TraitGroup = "Big & winged",  # or whichever level you want fixed
      PC1 = mean(final.data$PC1, na.rm = TRUE),
      PC2 = mean(final.data$PC2, na.rm = TRUE),
      Habitat = as.numeric(hab_q) # now held constant
    ),
    type = "response"
  )
  
  
  emm_df2 <- as.data.frame(emm2)
  
  emm_df2 <- emm_df2 %>% mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
                                                     Habitat == median(Habitat) ~ "Medium",
                                                     Habitat == min(Habitat) ~ "Low"),
                                Habitat = factor(Habitat,levels = c("High","Medium","Low")),
                                Matrix = case_when( Matrix == min(Matrix) ~ "High",
                                                    Matrix == median(Matrix) ~ "Medium",
                                                    Matrix == max(Matrix) ~ "Low"),
                                Matrix = factor(Matrix, levels = c("High","Medium", "Low")))
  
  
  matrix.lineplot[[buffer]] <- ggplot() +
    # geom_point(data=final.data, aes(x=Fragment, y=Abundance), color="black")+
    geom_line(data=emm_df2, aes(x = Fragment, y = response, color = factor(Habitat)), linewidth=1) +
    geom_ribbon(data=emm_df2, aes(x = Fragment,ymin = response-SE, ymax = response+SE, fill = factor(Habitat)),
                alpha = 0.2, color = NA) +
    facet_wrap(~Matrix, scales = "free_y", ncol=5) +
    scale_color_manual(values = c("black", "gray30","gray60"))  +
    scale_fill_manual(values = c("black", "gray30","gray60"))  +
    labs(x = "Fragmentation", y = "Predicted Abundance",
         color = "Habitat amount", fill = "Habitat amount") +
    theme_classic()
  
  #AIC(model2,model2alt)
  
  
  # Define sequences for continuous predictors
  seq_Fragment <- seq(min(final.data$Fragment, na.rm = TRUE), max(final.data$Fragment, na.rm = TRUE), length.out = 30)
  seq_Habitat <- seq(min(final.data$Habitat, na.rm = TRUE), max(final.data$Habitat, na.rm = TRUE), length.out = 30)
  seq_Matrix <- seq(min(final.data$Matrix, na.rm = TRUE), max(final.data$Matrix, na.rm = TRUE), length.out = 30)
  
  # Get mean values for control predictors
  mean_PC1 <- mean(final.data$PC1, na.rm = TRUE)
  mean_PC2 <- mean(final.data$PC2, na.rm = TRUE)
  mean_Year <- levels(final.data$Year)[1]  # reference year
  
  # Get levels for trait group
  trait_levels <- unique(final.data$TraitGroup)
  
  selected.trait <- trait_levels[1]
  
  # all_grids$Frag_Hab_TG <- expand.grid(
  #   Fragment = seq_Fragment,
  #   Habitat = seq_Habitat,
  #   Matrix = mean(final.data$Matrix, na.rm = TRUE),
  #   PC1 = mean_PC1,
  #   PC2 = mean_PC2,
  #   Year = mean_Year,
  #   TraitGroup = as.vector(na.omit(trait_levels))
  # )
  # 
  # predict2 <- predict(model2, newdata = all_grids$Frag_Hab_TG, type = "response", se.fit = TRUE)
  # predict.df2 <- cbind(all_grids$Frag_Hab_TG, fit = predict2$fit, se = predict2$se.fit)
  # 
  # 
  # predict.df2$TraitGroup <-factor(predict.df2$TraitGroup, levels=c("Big & winged",
  #                                                                   "Drift-prone, quicker reproducing flies",
  #                                                                   "Long-lived & passive",
  #                                                                   "Slower reproducing, stationary flies",
  #                                                                   "Small beetles & bugs"))
  # ggplot(data = predict.df2, aes(x = Fragment, y = Habitat)) +
  #   geom_tile(aes(fill = fit)) +
  #   geom_contour(aes(z = fit), color = "white", binwidth = 0.005) +
  #   labs(x = "Fragmentation", y = "Habitat", fill="") +
  #   scale_fill_viridis_c() +
  #   facet_wrap(~TraitGroup, ncol=5)+
  #   theme_classic() +
  #   theme(text = element_text(size = 14))
  
  
  Frag_Hab_TG <- data.frame(emmeans::emtrends(model2, ~TraitGroup|Habitat, var = "Fragment",
                                              at= list(Matrix = mean(final.data$Matrix, na.rm = TRUE),
                                                       Habitat = quantile(final.data$Habitat, na.rm = TRUE, probs = c(0.01,0.50,0.99)),
                                                       PC1 = mean_PC1,
                                                       PC2 = mean_PC2)))
  
  
  Frag_Hab_Mat <- data.frame(emmeans::emtrends(model2, ~Matrix|Habitat, var = "Fragment",
                                               at= list(Matrix = quantile(final.data$Matrix, na.rm = TRUE,probs = c(0.01,0.50,0.99)),
                                                        Habitat = quantile(final.data$Habitat, na.rm = TRUE, probs = c(0.01,0.50,0.99)),
                                                        PC1 = mean_PC1,
                                                        PC2 = mean_PC2)))
  
  
  
  
  # Frag_Hab_Mat_TG <- data.frame(emmeans::emtrends(model2, ~TraitGroup|Matrix|Habitat, var = "Fragment",
  #                                              at= list(Matrix = quantile(final.data$Matrix, na.rm = TRUE,probs = c(0.01,0.50,0.99)),
  #                                                       Habitat = quantile(final.data$Habitat, na.rm = TRUE, probs = c(0.01,0.50,0.99)),
  #                                                       PC1 = mean_PC1,
  #                                                       PC2 = mean_PC2)))
  # 
  # plotFrag_Hab_Mat_TG <- Frag_Hab_Mat_TG  %>% 
  #   mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
  #                               Habitat == median(Habitat) ~ "Medium",
  #                               Habitat == min(Habitat) ~ "Low"),
  #          Habitat = factor(Habitat, levels = c("High","Medium", "Low")),
  #          Matrix = case_when( Matrix == min(Matrix) ~ "High",
  #                              Matrix == median(Matrix) ~ "Medium",
  #                              Matrix == max(Matrix) ~ "Low"),
  #          Matrix = factor(Matrix, levels = c("High","Medium", "Low")),
  #          TraitGroup = factor(TraitGroup, levels = c("Big & winged",
  #                                                     "Drift-prone, quicker reproducing flies",
  #                                                     "Long-lived & passive",
  #                                                     "Small beetles & bugs",
  #                                                     "Slower reproducing, stationary flies"
  #          )))
  
  
  # Frag.Hab.Mat.TG.plot[[buffer]] <- ggplot(data =  plotFrag_Hab_Mat_TG, aes(x =Matrix, color = Habitat, group = Habitat)) +
  #   geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.4, size=1,
  #                 position = position_dodge(width = 0.5)) +
  #   geom_point(aes(y = Fragment.trend),position = position_dodge(width = 0.5), size=2) +
  #   scale_color_manual(values = c("black","gray30","gray70")) +
  #   labs(x = "Matrix quality", 
  #        y = "Estimated fragmentation coefficient",
  #        color = "Habitat amount") +
  #   geom_hline(aes(yintercept = 0), linetype="dashed")+
  #   theme_classic() +
  #   facet_wrap(~TraitGroup)+
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #         text = element_text(size=18))
  #          
  
  
  Frag.Hab.Mat.df[[buffer]] <- Frag_Hab_Mat
  
  plotFrag_Hab_Mat <- Frag_Hab_Mat  %>% 
    mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
                                Habitat == median(Habitat) ~ "Medium",
                                Habitat == min(Habitat) ~ "Low"),
           Habitat = factor(Habitat, levels = c("High","Medium", "Low")),
           Matrix = case_when( Matrix == min(Matrix) ~ "High",
                               Matrix == median(Matrix) ~ "Medium",
                               Matrix == max(Matrix) ~ "Low"),
           Matrix = factor(Matrix, levels = c("High","Medium", "Low")))
  
  
  Frag.Hab.Mat.plot[[buffer]] <- ggplot(data =  plotFrag_Hab_Mat , aes(x =Matrix, color = Habitat, group = Habitat)) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.4, size=1,
                  position = position_dodge(width = 0.5)) +
    geom_point(aes(y = Fragment.trend),position = position_dodge(width = 0.5), size=3) +
    scale_color_manual(values = c("black", "gray30","gray60"))  +
    labs(x = "Matrix quality", 
         y = "Estimated fragmentation coefficient",
         color = "Habitat amount") +
    geom_hline(aes(yintercept = 0), linetype="dashed")+
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size=18))
  
  Frag.Hab.Mat.nohigh <- plotFrag_Hab_Mat %>% filter(Habitat != "High")
  
  Frag.Hab.Mat.plot.nohigh[[buffer]] <- ggplot(data =  Frag.Hab.Mat.nohigh , aes(x =Matrix, color = Habitat, group = Habitat)) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.4, size=1,
                  position = position_dodge(width = 0.5)) +
    geom_point(aes(y = Fragment.trend),position = position_dodge(width = 0.5), size=3) +
    scale_color_manual(values = c("gray30","gray60"))  +
    labs(x = "Matrix quality", 
         y = "Estimated fragmentation coefficient",
         color = "Habitat amount") +
    geom_hline(aes(yintercept = 0), linetype="dashed")+
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size=18))
  
  plotFrag_Hab_TG <- Frag_Hab_TG  %>% 
    mutate(Habitat = case_when(  Habitat == max(Habitat) ~ "High",
                                 Habitat == median(Habitat) ~ "Medium",
                                 Habitat == min(Habitat) ~ "Low"),
           Habitat = factor(Habitat, levels = c("High","Medium", "Low")),
           TraitGroup = factor(
             str_wrap(TraitGroup, width = 20),   # 👈 wrap labels here
             levels = str_wrap(c(
               "Big & winged",
               "Slower reproducing, stationary flies", 
               "Small beetles & bugs",
               
               "Drift-prone, quicker reproducing flies",
               "Long-lived & passive"
             ), width = 20)))
  
  
  Frag.Hab.TG.df[[buffer]] <- Frag_Hab_TG
  
  
  
  Frag.Hab.TG.plot[[buffer]] <- ggplot(data =  plotFrag_Hab_TG , aes(x =TraitGroup, color = Habitat, group = Habitat)) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.4,size=1,
                  position = position_dodge(width = 0.5)) +
    geom_point(aes(y = Fragment.trend),position = position_dodge(width = 0.5), size=3) +
    scale_color_manual(values = c("black", "gray30","gray60"))  +
    labs(x = "", y = "Estimated fragmentation coefficient", color="Habitat amount") +
    geom_hline(aes(yintercept = 0), linetype="dashed")+
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size=14))
  
  
  # 
  # model3 <- glmmTMB(binary~Fragment*Habitat*TraitGroup+
  #                     Fragment*Habitat*Matrix+
  #                     PC1+PC2+(1|Year),
  #                   ziformula = ~PC1+PC2+(1|Year),
  #                   control=glmmTMBControl(optimizer=optim,
  #                                          optCtrl=list(maxit= 1e9),
  #                                          optArgs=list(method="BFGS")),
  #                   family=binomial(),
  #                   data=final.data)
  
  
  
  Models[[buffer]] <- model2
  modelsum[[buffer]] <- summary(model2) 
  anovas[[buffer]] <- car::Anova(model2)
  cis[[buffer]] <- confint(model2)
  
  
}

#null model comparisons ####
for (buffer in c(5, 10, 20)) {
  
  print(buffer)
  
  semifinal.data <- final.comm %>% 
    left_join(enviro.output, by = "Site", na_matches ="never") %>%
    distinct(.keep_all = TRUE)
  
  final.data <- semifinal.data %>%
    left_join(scales[[buffer]], by="Site",na_matches="never") %>%
    distinct(.keep_all = TRUE)
  
  #correct the variable types
  final.data$Year <- as.factor(final.data$Year)
  final.data$Fragment <- as.numeric(final.data$Fragment)
  final.data$Matrix   <- as.numeric(final.data$Develop)
  final.data$Habitat  <- as.numeric(final.data$Habitat)
  final.data$PC1      <- as.numeric(final.data$PC1)
  final.data$PC2      <- as.numeric(final.data$PC2)
  
  #SCALED IN THE LANDCOVER PART
  # final.data <- final.data %>%
  #   mutate(across(c(Habitat, Matrixt), scale)) 
  # 
  
  final.data <- final.data %>% filter(abs(Fragment) < 5 &
                                        abs(Habitat) < 5 &
                                        abs(Matrix) < 5 &
                                        abs(PC1) < 5 &
                                        abs(PC2) < 5)
  
  
  final.data$binary <- ifelse(final.data$Abundance > 0 , 1, 0)
  # 

  
  final.data <- final.data %>% filter(Abundance < 1000)

  
  model2 <- glmmTMB(Abundance~Fragment+
                      PC1+PC2+(1|Year),
                    ziformula = ~1,
                    control=glmmTMBControl(optimizer=optim,
                                           optCtrl=list(maxit= 1e9),
                                           optArgs=list(method="L-BFGS-B")),
                    family=nbinom2(),
                    data=final.data)
  
  model3 <- glmmTMB(Abundance~Fragment*Habitat+
                      PC1+PC2+(1|Year),
                    ziformula = ~1,
                    control=glmmTMBControl(optimizer=optim,
                                           optCtrl=list(maxit= 1e9),
                                           optArgs=list(method="L-BFGS-B")),
                    family=nbinom2(),
                    data=final.data)
  
message("Actual model ", AIC(Models[[buffer]]))  
message("Null model ",AIC(model2))  
message("Null HAH model ", AIC(model3))
}

for (buffer in c(5, 10, 20)) {
  
  print(buffer)
  
  semifinal.data <- final.comm %>% 
    left_join(enviro.output, by = "Site", na_matches ="never") %>%
    distinct(.keep_all = TRUE)
  
  final.data <- semifinal.data %>%
    left_join(scales[[buffer]], by="Site",na_matches="never") %>%
    distinct(.keep_all = TRUE)
  
  #correct the variable types
  final.data$Year <- as.factor(final.data$Year)
  final.data$Fragment <- as.numeric(final.data$Fragment)
  final.data$Matrix   <- as.numeric(final.data$Develop)
  final.data$Habitat  <- as.numeric(final.data$Habitat)
  final.data$PC1      <- as.numeric(final.data$PC1)
  final.data$PC2      <- as.numeric(final.data$PC2)
  
  #SCALED IN THE LANDCOVER PART
  # final.data <- final.data %>%
  #   mutate(across(c(Habitat, Matrixt), scale)) 
  # 
  
  final.data <- final.data %>% filter(abs(Fragment) < 5 &
                                        abs(Habitat) < 5 &
                                        abs(Matrix) < 5 &
                                        abs(PC1) < 5 &
                                        abs(PC2) < 5)
  
  
  final.data$binary <- ifelse(final.data$Abundance > 0 , 1, 0)
  # 
  
  
  final.data <- final.data %>% filter(Abundance < 1000)
  
  
  model2 <- glmmTMB(Abundance~(1|Year),
                    ziformula = ~1,
                    control=glmmTMBControl(optimizer=optim,
                                           optCtrl=list(maxit= 1e9),
                                           optArgs=list(method="L-BFGS-B")),
                    family=nbinom2(),
                    data=final.data)
  

  
  message("Actual model ", AIC(Models[[buffer]]))  
  message("True Null model ",AIC(model2))  

}
#1 and 50km #####
for (buffer in c(1,50)) {
  
  print(buffer)
  
  semifinal.data <- final.comm %>% 
    left_join(enviro.output, by = "Site", na_matches ="never") %>%
    distinct(.keep_all = TRUE)
  
  final.data <- semifinal.data %>%
    left_join(scales[[buffer]], by="Site",na_matches="never") %>%
    distinct(.keep_all = TRUE)
  
  #correct the variable types
  final.data$Year <- as.factor(final.data$Year)
  final.data$Fragment <- as.numeric(final.data$Fragment)
  final.data$Matrix   <- as.numeric(final.data$Develop)
  final.data$Habitat  <- as.numeric(final.data$Habitat)
  final.data$PC1      <- as.numeric(final.data$PC1)
  final.data$PC2      <- as.numeric(final.data$PC2)
  
  #SCALED IN THE LANDCOVER PART
  # final.data <- final.data %>%
  #   mutate(across(c(Habitat, Matrixt), scale)) 
  # 
  
  final.data <- final.data %>% filter(abs(Fragment) < 5 &
                                        abs(Habitat) < 5 &
                                        abs(Matrix) < 5 &
                                        abs(PC1) < 5 &
                                        abs(PC2) < 5)
  
  
  final.data$binary <- ifelse(final.data$Abundance > 0 , 1, 0)
  # 
  # final.data2 <- final.data %>%
  #   group_by(Site, TraitGroup, PC1, PC2, Year, ANN, Habitat, Matrix, Develop, Agriculture, Fragment, ExtraDevelop) %>%
  #   summarise(Abundance = sum(Abundance, na.rm = TRUE),
  #             Richness = sum(binary, na.rm=T),.groups = "drop_last") %>%
  #   group_by(Site) %>%
  #   mutate(TotalAbundance = sum(Abundance, na.rm = TRUE),
  #          RelAbundance = Abundance / TotalAbundance,
  #          TotalRichness = sum(Richness, na.rm=T),
  #          RelRichness = Richness/TotalRichness) %>%
  #   ungroup() %>%
  #   mutate(RelAbundance = case_when(
  #     RelAbundance == 0 ~ RelAbundance + 1e-5,
  #     RelAbundance == 1 ~ RelAbundance - 1e-5,
  #     TRUE ~ RelAbundance),
  #     RelRichness = case_when(
  #       RelRichness == 0 ~ RelRichness + 1e-5,
  #       RelRichness == 1 ~ RelRichness - 1e-5,
  #       TRUE ~ RelRichness
  #   ))%>% filter(Abundance < 3000)
  # 
  # 
  # final.data$logabund <- log(1+final.data$Abundance)
  # 
  # final.data2$logabund <- log(1+final.data2$Abundance)
  # 
  # final.data$binary <- ifelse(final.data$Abundance > 0 , 1, 0)
  # 
  # final.data2$binary <- ifelse(final.data2$Abundance > 0 , 1, 0)
  # 
  # colnames(final.data)
  
  final.data <- final.data %>% filter(Abundance < 1000)
  
  
  
  # #run model
  # model2 <- glmmTMB(RelAbundance~Fragment*Habitat*TraitGroup+
  #                             Fragment*Habitat*Matrix+
  #                             PC1+PC2+(1|Year),
  #                 ziformula = ~0,
  #                  control=glmmTMBControl(optimizer=optim,
  #                                         optCtrl=list(maxit= 1e9),
  #                                         optArgs=list(method="BFGS")),
  #                  family=beta_family(),data=final.data2)
  # 
  
  
  
  
  # model <- glmmTMB(Richness~Fragment*Habitat*TraitGroup+
  #                   Fragment*Habitat*Matrix+
  #                   PC1+PC2+(1|Year),
  #                   ziformula =~1,
  #                   control=glmmTMBControl(optimizer=optim,
  #                   optCtrl=list(maxit= 1e9),
  #                   optArgs=list(method="BFGS")),
  #                   family=nbinom2(),
  #                   data=final.data2)
  # summary(model)
  # 
  # modelalt <- glmmTMB(Richness~Fragment*Habitat*TraitGroup+
  #                    Fragment*Habitat*Matrix+
  #                    PC1+PC2+(1|Year),
  #                  ziformula =~1,
  #                  control=glmmTMBControl(optimizer=optim,
  #                                         optCtrl=list(maxit= 1e9),
  #                                         optArgs=list(method="BFGS")),
  #                  family=truncated_genpois(),
  #                  data=final.data2)
  # 
  # modelalt2 <- glmmTMB(Richness~Fragment*Habitat*TraitGroup+
  #                       Fragment*Habitat*Matrix+
  #                       PC1+PC2+(1|Year),
  #                     ziformula =~PC1+PC2+(1|Year),
  #                     control=glmmTMBControl(optimizer=optim,
  #                                            optCtrl=list(maxit= 1e9),
  #                                            optArgs=list(method="BFGS")),
  #                     family=genpois(),
  #                     data=final.data2)
  # 
  # summary(modelalt)
  # 
  
  # 
  # 
  # model <- modelalt2
  # 
  # predict <- predict(model, newdata = all_grids$Frag_Hab_TG, type = "response", se.fit = TRUE)
  # predict.df <- cbind(all_grids$Frag_Hab_TG, fit = predict$fit, se = predict$se.fit)
  # 
  # 
  # predict.df$TraitGroup <-factor(predict.df$TraitGroup, levels=c("Big & winged",
  #                                                                  "Drift-prone, quicker reproducing flies",
  #                                                                  "Long-lived & passive",
  #                                                                  "Slower reproducing, stationary flies",
  #                                                                  "Small beetles & bugs"))
  # 
  # ggplot(data = predict.df, aes(x = Fragment, y = Habitat)) +
  #   geom_tile(aes(fill = fit)) +
  #   geom_contour(aes(z = fit), color = "white", binwidth = 0.5) +
  #   labs(x = "Fragmentation", y = "Habitat", fill="") +
  #   scale_fill_viridis_c() +
  #   facet_wrap(~TraitGroup, ncol=5)+
  #   theme_classic() +
  #   theme(text = element_text(size = 14))
  # 
  # coefplots$Frag_Hab_TG <- data.frame(emmeans::emtrends(model, ~TraitGroup|Habitat, var = "Fragment",
  #                                                       at= list(Matrix = mean(final.data$Matrix, na.rm = TRUE),
  #                                                                Habitat = quantile(final.data$Habitat, na.rm = TRUE, probs = c(0.01,0.50,0.99)),
  #                                                                PC1 = mean_PC1,
  #                                                                PC2 = mean_PC2)))
  # 
  # 
  # 
  # plotFrag_Hab_TG <- coefplots$Frag_Hab_TG  %>% 
  #   mutate(Habitat = case_when( Habitat < -1 ~ "Low",
  #                               Habitat > -1 & Habitat < 1 ~ "Medium",
  #                               Habitat > 1 ~ "High"),
  #          Habitat = factor(Habitat, levels = c("High","Medium", "Low")))
  # 
  # ggplot(data =  plotFrag_Hab_TG , aes(x =TraitGroup, color = Habitat, group = Habitat)) +
  #   geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.2,
  #                 position = position_dodge(width = 0.5)) +
  #   geom_point(aes(y = Fragment.trend),position = position_dodge(width = 0.5)) +
  #   scale_color_manual(values = c("black","gray30","gray80")) +
  #   labs(x = "", y = "Estimated fragmentation slope") +
  #   geom_hline(aes(yintercept = 0))+
  #   theme_classic() +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # 
  
  
  
  model2 <- glmmTMB(Abundance~Fragment*Habitat*TraitGroup+
                      Fragment*Habitat*Matrix+
                      PC1+PC2+(1|Year),
                    ziformula = ~1,
                    control=glmmTMBControl(optimizer=optim,
                                           optCtrl=list(maxit= 1e9),
                                           optArgs=list(method="L-BFGS-B")),
                    family=nbinom2(),
                    data=final.data)
  
  
  
  hab_q <- quantile(final.data$Habitat, probs = c(0.01, 0.5, 0.99), na.rm = TRUE)
  emm <- emmeans::emmeans(
    model2,
    ~ Fragment | TraitGroup * Habitat,
    at = list(
      Fragment = seq(min(final.data$Fragment, na.rm = TRUE),
                     max(final.data$Fragment, na.rm = TRUE),
                     length.out = 50),
      Habitat = as.numeric(hab_q),
      PC1 = mean(final.data$PC1, na.rm = TRUE),
      PC2 = mean(final.data$PC2, na.rm = TRUE),
      Matrix = mean(final.data$Matrix, na.rm=TRUE)
    ),
    type = "response"
  )
  
  emm_df <- as.data.frame(emm)
  
  emm_df <- emm_df %>% mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
                                                   Habitat == median(Habitat) ~ "Medium",
                                                   Habitat == min(Habitat) ~ "Low"),
                              Habitat = factor(Habitat,levels = c("High","Medium","Low")),
                              TraitGroup = factor(
                                str_wrap(TraitGroup, width = 20),   # 👈 wrap labels here
                                levels = str_wrap(c(
                                  "Big & winged",
                                  "Slower reproducing, stationary flies", 
                                  "Small beetles & bugs",
                                  
                                  "Drift-prone, quicker reproducing flies",
                                  "Long-lived & passive"
                                ), width = 20)))
  
  
  trait.lineplot[[buffer]] <- ggplot() +
    # geom_point(data=final.data, aes(x=Fragment, y=Abundance), color="black")+
    geom_line(data=emm_df, aes(x = Fragment, y = response, color = factor(Habitat)), linewidth=1) +
    geom_ribbon(data=emm_df, aes(x = Fragment,ymin = response-SE, ymax = response+SE, fill = factor(Habitat)),
                alpha = 0.2, color = NA) +
    facet_wrap(~TraitGroup, scales = "free_y", ncol=5) +
    scale_color_manual(values = c("black", "gray30","gray60")) +
    scale_fill_manual(values = c("black", "gray30","gray60"))  +
    labs(x = "Fragmentation", y = "Predicted Abundance",
         color = "Habitat amount", fill = "Habitat amount") +
    theme_classic()
  
  
  
  mat_q <- quantile(final.data$Matrix, probs = c(0.01, 0.5, 0.99), na.rm = TRUE)
  emm2 <- emmeans::emmeans(
    model2,
    ~ Fragment | Matrix* Habitat,   # vary Matrix, condition plots by Matrix
    at = list(
      Fragment = seq(min(final.data$Fragment, na.rm = TRUE),
                     max(final.data$Fragment, na.rm = TRUE),
                     length.out = 50),
      Matrix   = as.numeric(mat_q),
      TraitGroup = "Big & winged",  # or whichever level you want fixed
      PC1 = mean(final.data$PC1, na.rm = TRUE),
      PC2 = mean(final.data$PC2, na.rm = TRUE),
      Habitat = as.numeric(hab_q) # now held constant
    ),
    type = "response"
  )
  
  
  emm_df2 <- as.data.frame(emm2)
  
  emm_df2 <- emm_df2 %>% mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
                                                     Habitat == median(Habitat) ~ "Medium",
                                                     Habitat == min(Habitat) ~ "Low"),
                                Habitat = factor(Habitat,levels = c("High","Medium","Low")),
                                Matrix = case_when( Matrix == min(Matrix) ~ "High",
                                                    Matrix == median(Matrix) ~ "Medium",
                                                    Matrix == max(Matrix) ~ "Low"),
                                Matrix = factor(Matrix, levels = c("High","Medium", "Low")))
  
  
  matrix.lineplot[[buffer]] <- ggplot() +
    # geom_point(data=final.data, aes(x=Fragment, y=Abundance), color="black")+
    geom_line(data=emm_df2, aes(x = Fragment, y = response, color = factor(Habitat)), linewidth=1) +
    geom_ribbon(data=emm_df2, aes(x = Fragment,ymin = response-SE, ymax = response+SE, fill = factor(Habitat)),
                alpha = 0.2, color = NA) +
    facet_wrap(~Matrix, scales = "free_y", ncol=5) +
    scale_color_manual(values = c("black", "gray30","gray60"))  +
    scale_fill_manual(values = c("black", "gray30","gray60"))  +
    labs(x = "Fragmentation", y = "Predicted Abundance",
         color = "Habitat amount", fill = "Habitat amount") +
    theme_classic()
  
  #AIC(model2,model2alt)
  
  
  # Define sequences for continuous predictors
  seq_Fragment <- seq(min(final.data$Fragment, na.rm = TRUE), max(final.data$Fragment, na.rm = TRUE), length.out = 30)
  seq_Habitat <- seq(min(final.data$Habitat, na.rm = TRUE), max(final.data$Habitat, na.rm = TRUE), length.out = 30)
  seq_Matrix <- seq(min(final.data$Matrix, na.rm = TRUE), max(final.data$Matrix, na.rm = TRUE), length.out = 30)
  
  # Get mean values for control predictors
  mean_PC1 <- mean(final.data$PC1, na.rm = TRUE)
  mean_PC2 <- mean(final.data$PC2, na.rm = TRUE)
  mean_Year <- levels(final.data$Year)[1]  # reference year
  
  # Get levels for trait group
  trait_levels <- unique(final.data$TraitGroup)
  
  selected.trait <- trait_levels[1]
  
  # all_grids$Frag_Hab_TG <- expand.grid(
  #   Fragment = seq_Fragment,
  #   Habitat = seq_Habitat,
  #   Matrix = mean(final.data$Matrix, na.rm = TRUE),
  #   PC1 = mean_PC1,
  #   PC2 = mean_PC2,
  #   Year = mean_Year,
  #   TraitGroup = as.vector(na.omit(trait_levels))
  # )
  # 
  # predict2 <- predict(model2, newdata = all_grids$Frag_Hab_TG, type = "response", se.fit = TRUE)
  # predict.df2 <- cbind(all_grids$Frag_Hab_TG, fit = predict2$fit, se = predict2$se.fit)
  # 
  # 
  # predict.df2$TraitGroup <-factor(predict.df2$TraitGroup, levels=c("Big & winged",
  #                                                                   "Drift-prone, quicker reproducing flies",
  #                                                                   "Long-lived & passive",
  #                                                                   "Slower reproducing, stationary flies",
  #                                                                   "Small beetles & bugs"))
  # ggplot(data = predict.df2, aes(x = Fragment, y = Habitat)) +
  #   geom_tile(aes(fill = fit)) +
  #   geom_contour(aes(z = fit), color = "white", binwidth = 0.005) +
  #   labs(x = "Fragmentation", y = "Habitat", fill="") +
  #   scale_fill_viridis_c() +
  #   facet_wrap(~TraitGroup, ncol=5)+
  #   theme_classic() +
  #   theme(text = element_text(size = 14))
  
  
  Frag_Hab_TG <- data.frame(emmeans::emtrends(model2, ~TraitGroup|Habitat, var = "Fragment",
                                              at= list(Matrix = mean(final.data$Matrix, na.rm = TRUE),
                                                       Habitat = quantile(final.data$Habitat, na.rm = TRUE, probs = c(0.01,0.50,0.99)),
                                                       PC1 = mean_PC1,
                                                       PC2 = mean_PC2)))
  
  
  Frag_Hab_Mat <- data.frame(emmeans::emtrends(model2, ~Matrix|Habitat, var = "Fragment",
                                               at= list(Matrix = quantile(final.data$Matrix, na.rm = TRUE,probs = c(0.01,0.50,0.99)),
                                                        Habitat = quantile(final.data$Habitat, na.rm = TRUE, probs = c(0.01,0.50,0.99)),
                                                        PC1 = mean_PC1,
                                                        PC2 = mean_PC2)))
  
  
  
  
  # Frag_Hab_Mat_TG <- data.frame(emmeans::emtrends(model2, ~TraitGroup|Matrix|Habitat, var = "Fragment",
  #                                              at= list(Matrix = quantile(final.data$Matrix, na.rm = TRUE,probs = c(0.01,0.50,0.99)),
  #                                                       Habitat = quantile(final.data$Habitat, na.rm = TRUE, probs = c(0.01,0.50,0.99)),
  #                                                       PC1 = mean_PC1,
  #                                                       PC2 = mean_PC2)))
  # 
  # plotFrag_Hab_Mat_TG <- Frag_Hab_Mat_TG  %>% 
  #   mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
  #                               Habitat == median(Habitat) ~ "Medium",
  #                               Habitat == min(Habitat) ~ "Low"),
  #          Habitat = factor(Habitat, levels = c("High","Medium", "Low")),
  #          Matrix = case_when( Matrix == min(Matrix) ~ "High",
  #                              Matrix == median(Matrix) ~ "Medium",
  #                              Matrix == max(Matrix) ~ "Low"),
  #          Matrix = factor(Matrix, levels = c("High","Medium", "Low")),
  #          TraitGroup = factor(TraitGroup, levels = c("Big & winged",
  #                                                     "Drift-prone, quicker reproducing flies",
  #                                                     "Long-lived & passive",
  #                                                     "Small beetles & bugs",
  #                                                     "Slower reproducing, stationary flies"
  #          )))
  
  
  # Frag.Hab.Mat.TG.plot[[buffer]] <- ggplot(data =  plotFrag_Hab_Mat_TG, aes(x =Matrix, color = Habitat, group = Habitat)) +
  #   geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.4, size=1,
  #                 position = position_dodge(width = 0.5)) +
  #   geom_point(aes(y = Fragment.trend),position = position_dodge(width = 0.5), size=2) +
  #   scale_color_manual(values = c("black","gray30","gray70")) +
  #   labs(x = "Matrix quality", 
  #        y = "Estimated fragmentation coefficient",
  #        color = "Habitat amount") +
  #   geom_hline(aes(yintercept = 0), linetype="dashed")+
  #   theme_classic() +
  #   facet_wrap(~TraitGroup)+
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #         text = element_text(size=18))
  #          
  
  
  Frag.Hab.Mat.df[[buffer]] <- Frag_Hab_Mat
  
  plotFrag_Hab_Mat <- Frag_Hab_Mat  %>% 
    mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
                                Habitat == median(Habitat) ~ "Medium",
                                Habitat == min(Habitat) ~ "Low"),
           Habitat = factor(Habitat, levels = c("High","Medium", "Low")),
           Matrix = case_when( Matrix == min(Matrix) ~ "High",
                               Matrix == median(Matrix) ~ "Medium",
                               Matrix == max(Matrix) ~ "Low"),
           Matrix = factor(Matrix, levels = c("High","Medium", "Low")))
  
  
  Frag.Hab.Mat.plot[[buffer]] <- ggplot(data =  plotFrag_Hab_Mat , aes(x =Matrix, color = Habitat, group = Habitat)) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.4, size=1,
                  position = position_dodge(width = 0.5)) +
    geom_point(aes(y = Fragment.trend),position = position_dodge(width = 0.5), size=3) +
    scale_color_manual(values = c("black", "gray30","gray60"))  +
    labs(x = "Matrix quality", 
         y = "Estimated fragmentation coefficient",
         color = "Habitat amount") +
    geom_hline(aes(yintercept = 0), linetype="dashed")+
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size=18))
  
  Frag.Hab.Mat.nohigh <- plotFrag_Hab_Mat %>% filter(Habitat != "High")
  
  Frag.Hab.Mat.plot.nohigh[[buffer]] <- ggplot(data =  Frag.Hab.Mat.nohigh , aes(x =Matrix, color = Habitat, group = Habitat)) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.4, size=1,
                  position = position_dodge(width = 0.5)) +
    geom_point(aes(y = Fragment.trend),position = position_dodge(width = 0.5), size=3) +
    scale_color_manual(values = c("gray30","gray60"))  +
    labs(x = "Matrix quality", 
         y = "Estimated fragmentation coefficient",
         color = "Habitat amount") +
    geom_hline(aes(yintercept = 0), linetype="dashed")+
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size=18))
  
  plotFrag_Hab_TG <- Frag_Hab_TG  %>% 
    mutate(Habitat = case_when(  Habitat == max(Habitat) ~ "High",
                                 Habitat == median(Habitat) ~ "Medium",
                                 Habitat == min(Habitat) ~ "Low"),
           Habitat = factor(Habitat, levels = c("High","Medium", "Low")),
           TraitGroup = factor(
             str_wrap(TraitGroup, width = 20),   # 👈 wrap labels here
             levels = str_wrap(c(
               "Big & winged",
               "Slower reproducing, stationary flies", 
               "Small beetles & bugs",
               
               "Drift-prone, quicker reproducing flies",
               "Long-lived & passive"
             ), width = 20)))
  
  
  Frag.Hab.TG.df[[buffer]] <- Frag_Hab_TG
  
  
  
  Frag.Hab.TG.plot[[buffer]] <- ggplot(data =  plotFrag_Hab_TG , aes(x =TraitGroup, color = Habitat, group = Habitat)) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.4,size=1,
                  position = position_dodge(width = 0.5)) +
    geom_point(aes(y = Fragment.trend),position = position_dodge(width = 0.5), size=3) +
    scale_color_manual(values = c("black", "gray30","gray60"))  +
    labs(x = "", y = "Estimated fragmentation coefficient", color="Habitat amount") +
    geom_hline(aes(yintercept = 0), linetype="dashed")+
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size=14))
  
  
  # 
  # model3 <- glmmTMB(binary~Fragment*Habitat*TraitGroup+
  #                     Fragment*Habitat*Matrix+
  #                     PC1+PC2+(1|Year),
  #                   ziformula = ~PC1+PC2+(1|Year),
  #                   control=glmmTMBControl(optimizer=optim,
  #                                          optCtrl=list(maxit= 1e9),
  #                                          optArgs=list(method="BFGS")),
  #                   family=binomial(),
  #                   data=final.data)
  
  
  
  Models[[buffer]] <- model2
  modelsum[[buffer]] <- summary(model2) 
  anovas[[buffer]] <- car::Anova(model2)
  cis[[buffer]] <- confint(model2)
  
  
}

#make those predictions!
trait.lineplot[[5]]+
  labs(x="", title = "5km")+
  theme(
    plot.title = element_text(size = 24,hjust = 0.5),
    text = element_text(size=18),
    legend.position = "none",                     # move it underneath
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(1, "cm"),
    legend.box = "horizontal"                       # arrange items in a row
  ) 

Frag.Hab.TG.plot[[5]]+
  labs(x="", title = "5km")+
  theme(
    legend.position = "bottom",                     # move it underneath
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(1, "cm"),
    legend.box = "horizontal"                       # arrange items in a row
  )

#richness ######
Models.rich <- list()
modelsum.rich <- list()
anovas.rich <- list()
all_predict.rich <- list()
cis.rich <- list()
Frag.Hab.Mat.df.rich <- list()
Frag.Hab.Mat.plot.rich <-list()
trait.lineplot.rich <- list()
matrix.lineplot.rich <- list()
Frag.Hab.TG.df.rich <- list()
Frag.Hab.TG.plot.rich <-list()
for (buffer in c(5, 10, 20)) {
  
  print(buffer)
  
  semifinal.data <- final.comm %>% 
    left_join(enviro.output, by = "Site", na_matches ="never") %>%
    distinct(.keep_all = TRUE)
  
  final.data <- semifinal.data %>%
    left_join(scales[[buffer]], by="Site",na_matches="never") %>%
    distinct(.keep_all = TRUE)
  
  #correct the variable types
  final.data$Year <- as.factor(final.data$Year)
  final.data$Fragment <- as.numeric(final.data$Fragment)
  final.data$Matrix   <- as.numeric(final.data$Develop)
  final.data$Habitat  <- as.numeric(final.data$Habitat)
  final.data$PC1      <- as.numeric(final.data$PC1)
  final.data$PC2      <- as.numeric(final.data$PC2)
  
  
  # final.data <- final.data %>%
  #   mutate(across(c(Fragment, Habitat, Matrix, PC1, PC2), scale)) 
  # 
  
  final.data <- final.data %>% filter(abs(Fragment) < 5 &
                                        abs(Habitat) < 5 &
                                        abs(Matrix) < 5 &
                                        abs(PC1) < 5 &
                                        abs(PC2) < 5)
  
  
  final.data$binary <- ifelse(final.data$Abundance > 0 , 1, 0)
  # 
  final.data2 <- final.data %>%
    group_by(Site, TraitGroup, PC1, PC2, Year, ANN, Habitat, Matrix, Develop, Agriculture, Fragment, ExtraDevelop) %>%
    summarise(Abundance = sum(Abundance, na.rm = TRUE),
              Richness = sum(binary, na.rm=T),.groups = "drop_last") %>%
    group_by(Site) %>%
    mutate(TotalAbundance = sum(Abundance, na.rm = TRUE),
           RelAbundance = Abundance / TotalAbundance,
           TotalRichness = sum(Richness, na.rm=T),
           RelRichness = Richness/TotalRichness) %>%
    ungroup() %>%
    mutate(RelAbundance = case_when(
      RelAbundance == 0 ~ RelAbundance + 1e-5,
      RelAbundance == 1 ~ RelAbundance - 1e-5,
      TRUE ~ RelAbundance),
      RelRichness = case_when(
        RelRichness == 0 ~ RelRichness + 1e-5,
        RelRichness == 1 ~ RelRichness - 1e-5,
        TRUE ~ RelRichness
      ))%>% filter(Abundance < 3000)
  
  
  
  final.data <- final.data %>% filter(Abundance < 1000)
  
  
  
  
  
  model2 <- glmmTMB(binary~Fragment*Habitat*TraitGroup+
                      Fragment*Habitat*Matrix+
                      PC1+PC2+(1|Year),
                    ziformula = ~1,
                    control=glmmTMBControl(optimizer=optim,
                                           optCtrl=list(maxit= 1e9),
                                           optArgs=list(method="L-BFGS-B")),
                    family=binomial(),
                    data=final.data)
  
  
  
  hab_q <- quantile(final.data$Habitat, probs = c(0.01, 0.5, 0.99), na.rm = TRUE)
  emm <- emmeans::emmeans(
    model2,
    ~ Fragment | TraitGroup * Habitat,
    at = list(
      Fragment = seq(min(final.data$Fragment, na.rm = TRUE),
                     max(final.data$Fragment, na.rm = TRUE),
                     length.out = 50),
      Habitat = as.numeric(hab_q),
      PC1 = mean(final.data$PC1, na.rm = TRUE),
      PC2 = mean(final.data$PC2, na.rm = TRUE),
      Matrix = mean(final.data$Matrix, na.rm=TRUE)
    ),
    type = "response"
  )
  
  emm_df <- as.data.frame(emm)
  
  emm_df <- emm_df %>% mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
                                                   Habitat == median(Habitat) ~ "Medium",
                                                   Habitat == min(Habitat) ~ "Low"),
                              Habitat = factor(Habitat,levels = c("High","Medium","Low")),
                              TraitGroup = factor(
                                str_wrap(TraitGroup, width = 20),   # 👈 wrap labels here
                                levels = str_wrap(c(
                                  "Big & winged",
                                  "Small beetles & bugs",
                                  "Slower reproducing, stationary flies", 
                                  "Drift-prone, quicker reproducing flies",
                                  "Long-lived & passive"
                                ), width = 20)))
  
  
  trait.lineplot.rich[[buffer]] <- ggplot() +
    # geom_point(data=final.data, aes(x=Fragment, y=Abundance), color="black")+
    geom_line(data=emm_df, aes(x = Fragment, y = response, color = factor(Habitat)), linewidth=1) +
    geom_ribbon(data=emm_df, aes(x = Fragment,ymin = response-SE, ymax = response+SE, fill = factor(Habitat)),
                alpha = 0.2, color = NA) +
    facet_wrap(~TraitGroup, scales = "free_y", ncol=5) +
    scale_color_manual(values = c("black", "gray35","gray70")) +
    scale_fill_manual(values = c("black", "gray35","gray70"))  +
    labs(x = "Fragmentation", y = "Predicted Abundance",
         color = "Habitat amount", fill = "Habitat amount") +
    theme_classic()
  
  
  
  mat_q <- quantile(final.data$Matrix, probs = c(0.01, 0.5, 0.99), na.rm = TRUE)
  emm2 <- emmeans::emmeans(
    model2,
    ~ Fragment | Matrix* Habitat,   # vary Matrix, condition plots by Matrix
    at = list(
      Fragment = seq(min(final.data$Fragment, na.rm = TRUE),
                     max(final.data$Fragment, na.rm = TRUE),
                     length.out = 50),
      Matrix   = as.numeric(mat_q),
      TraitGroup = "Big & winged",  # or whichever level you want fixed
      PC1 = mean(final.data$PC1, na.rm = TRUE),
      PC2 = mean(final.data$PC2, na.rm = TRUE),
      Habitat = as.numeric(hab_q) # now held constant
    ),
    type = "response"
  )
  
  
  emm_df2 <- as.data.frame(emm2)
  
  emm_df2 <- emm_df2 %>% mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
                                                     Habitat == median(Habitat) ~ "Medium",
                                                     Habitat == min(Habitat) ~ "Low"),
                                Habitat = factor(Habitat,levels = c("High","Medium","Low")),
                                Matrix = case_when( Matrix == min(Matrix) ~ "High",
                                                    Matrix == median(Matrix) ~ "Medium",
                                                    Matrix == max(Matrix) ~ "Low"),
                                Matrix = factor(Matrix, levels = c("High","Medium", "Low")))
  
  
  matrix.lineplot.rich[[buffer]] <- ggplot() +
    # geom_point(data=final.data, aes(x=Fragment, y=Abundance), color="black")+
    geom_line(data=emm_df2, aes(x = Fragment, y = response, color = factor(Habitat)), linewidth=1) +
    geom_ribbon(data=emm_df2, aes(x = Fragment,ymin = response-SE, ymax = response+SE, fill = factor(Habitat)),
                alpha = 0.2, color = NA) +
    facet_wrap(~Matrix, scales = "free_y", ncol=5) +
    scale_color_manual(values = c("black", "gray35","gray70"))  +
    scale_fill_manual(values = c("black", "gray35","gray70"))  +
    labs(x = "Fragmentation", y = "Predicted Abundance",
         color = "Habitat amount", fill = "Habitat amount") +
    theme_classic()
  
  #AIC(model2,model2alt)
  
  
  # Define sequences for continuous predictors
  seq_Fragment <- seq(min(final.data$Fragment, na.rm = TRUE), max(final.data$Fragment, na.rm = TRUE), length.out = 30)
  seq_Habitat <- seq(min(final.data$Habitat, na.rm = TRUE), max(final.data$Habitat, na.rm = TRUE), length.out = 30)
  seq_Matrix <- seq(min(final.data$Matrix, na.rm = TRUE), max(final.data$Matrix, na.rm = TRUE), length.out = 30)
  
  # Get mean values for control predictors
  mean_PC1 <- mean(final.data$PC1, na.rm = TRUE)
  mean_PC2 <- mean(final.data$PC2, na.rm = TRUE)
  mean_Year <- levels(final.data$Year)[1]  # reference year
  
  # Get levels for trait group
  trait_levels <- unique(final.data$TraitGroup)
  
  selected.trait <- trait_levels[1]
  
  # all_grids$Frag_Hab_TG <- expand.grid(
  #   Fragment = seq_Fragment,
  #   Habitat = seq_Habitat,
  #   Matrix = mean(final.data$Matrix, na.rm = TRUE),
  #   PC1 = mean_PC1,
  #   PC2 = mean_PC2,
  #   Year = mean_Year,
  #   TraitGroup = as.vector(na.omit(trait_levels))
  # )
  # 
  # predict2 <- predict(model2, newdata = all_grids$Frag_Hab_TG, type = "response", se.fit = TRUE)
  # predict.df2 <- cbind(all_grids$Frag_Hab_TG, fit = predict2$fit, se = predict2$se.fit)
  # 
  # 
  # predict.df2$TraitGroup <-factor(predict.df2$TraitGroup, levels=c("Big & winged",
  #                                                                   "Drift-prone, quicker reproducing flies",
  #                                                                   "Long-lived & passive",
  #                                                                   "Slower reproducing, stationary flies",
  #                                                                   "Small beetles & bugs"))
  # ggplot(data = predict.df2, aes(x = Fragment, y = Habitat)) +
  #   geom_tile(aes(fill = fit)) +
  #   geom_contour(aes(z = fit), color = "white", binwidth = 0.005) +
  #   labs(x = "Fragmentation", y = "Habitat", fill="") +
  #   scale_fill_viridis_c() +
  #   facet_wrap(~TraitGroup, ncol=5)+
  #   theme_classic() +
  #   theme(text = element_text(size = 14))
  
  
  Frag_Hab_TG <- data.frame(emmeans::emtrends(model2, ~TraitGroup|Habitat, var = "Fragment",
                                              at= list(Matrix = mean(final.data$Matrix, na.rm = TRUE),
                                                       Habitat = quantile(final.data$Habitat, na.rm = TRUE, probs = c(0.01,0.50,0.99)),
                                                       PC1 = mean_PC1,
                                                       PC2 = mean_PC2)))
  
  
  Frag_Hab_Mat <- data.frame(emmeans::emtrends(model2, ~Matrix|Habitat, var = "Fragment",
                                               at= list(Matrix = quantile(final.data$Matrix, na.rm = TRUE,probs = c(0.01,0.50,0.99)),
                                                        Habitat = quantile(final.data$Habitat, na.rm = TRUE, probs = c(0.01,0.50,0.99)),
                                                        PC1 = mean_PC1,
                                                        PC2 = mean_PC2)))
  
  
  
  
  # Frag_Hab_Mat_TG <- data.frame(emmeans::emtrends(model2, ~TraitGroup|Matrix|Habitat, var = "Fragment",
  #                                              at= list(Matrix = quantile(final.data$Matrix, na.rm = TRUE,probs = c(0.01,0.50,0.99)),
  #                                                       Habitat = quantile(final.data$Habitat, na.rm = TRUE, probs = c(0.01,0.50,0.99)),
  #                                                       PC1 = mean_PC1,
  #                                                       PC2 = mean_PC2)))
  # 
  # plotFrag_Hab_Mat_TG <- Frag_Hab_Mat_TG  %>% 
  #   mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
  #                               Habitat == median(Habitat) ~ "Medium",
  #                               Habitat == min(Habitat) ~ "Low"),
  #          Habitat = factor(Habitat, levels = c("High","Medium", "Low")),
  #          Matrix = case_when( Matrix == min(Matrix) ~ "High",
  #                              Matrix == median(Matrix) ~ "Medium",
  #                              Matrix == max(Matrix) ~ "Low"),
  #          Matrix = factor(Matrix, levels = c("High","Medium", "Low")),
  #          TraitGroup = factor(TraitGroup, levels = c("Big & winged",
  #                                                     "Drift-prone, quicker reproducing flies",
  #                                                     "Long-lived & passive",
  #                                                     "Small beetles & bugs",
  #                                                     "Slower reproducing, stationary flies"
  #          )))
  
  
  # Frag.Hab.Mat.TG.plot[[buffer]] <- ggplot(data =  plotFrag_Hab_Mat_TG, aes(x =Matrix, color = Habitat, group = Habitat)) +
  #   geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.4, size=1,
  #                 position = position_dodge(width = 0.5)) +
  #   geom_point(aes(y = Fragment.trend),position = position_dodge(width = 0.5), size=2) +
  #   scale_color_manual(values = c("black","gray30","gray70")) +
  #   labs(x = "Matrix quality", 
  #        y = "Estimated fragmentation coefficient",
  #        color = "Habitat amount") +
  #   geom_hline(aes(yintercept = 0), linetype="dashed")+
  #   theme_classic() +
  #   facet_wrap(~TraitGroup)+
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #         text = element_text(size=18))
  #          
  
  
  Frag.Hab.Mat.df.rich[[buffer]] <- Frag_Hab_Mat
  
  plotFrag_Hab_Mat <- Frag_Hab_Mat  %>% 
    mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
                                Habitat == median(Habitat) ~ "Medium",
                                Habitat == min(Habitat) ~ "Low"),
           Habitat = factor(Habitat, levels = c("High","Medium", "Low")),
           Matrix = case_when( Matrix == min(Matrix) ~ "High",
                               Matrix == median(Matrix) ~ "Medium",
                               Matrix == max(Matrix) ~ "Low"),
           Matrix = factor(Matrix, levels = c("High","Medium", "Low")))
  
  
  Frag.Hab.Mat.plot.rich[[buffer]] <- ggplot(data =  plotFrag_Hab_Mat , aes(x =Matrix, color = Habitat, group = Habitat)) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.4, size=1,
                  position = position_dodge(width = 0.5)) +
    geom_point(aes(y = Fragment.trend),position = position_dodge(width = 0.5), size=2) +
    scale_color_manual(values = c("darkblue", "skyblue4","lightblue")) +
    labs(x = "Matrix quality", 
         y = "Estimated fragmentation coefficient",
         color = "Habitat amount") +
    geom_hline(aes(yintercept = 0), linetype="dashed")+
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size=18))
  
  plotFrag_Hab_TG <- Frag_Hab_TG  %>% 
    mutate(Habitat = case_when(  Habitat == max(Habitat) ~ "High",
                                 Habitat == median(Habitat) ~ "Medium",
                                 Habitat == min(Habitat) ~ "Low"),
           Habitat = factor(Habitat, levels = c("High","Medium", "Low")),
           TraitGroup = factor(
             str_wrap(TraitGroup, width = 20),   # 👈 wrap labels here
             levels = str_wrap(c(
               "Big & winged",
               "Small beetles & bugs",
               "Slower reproducing, stationary flies", 
               "Drift-prone, quicker reproducing flies",
               "Long-lived & passive"
             ), width = 20)))
  
  
  Frag.Hab.TG.df.rich[[buffer]] <- Frag_Hab_TG
  
  
  
  Frag.Hab.TG.plot.rich[[buffer]] <- ggplot(data =  plotFrag_Hab_TG , aes(x =TraitGroup, color = Habitat, group = Habitat)) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.4,size=1,
                  position = position_dodge(width = 0.5)) +
    geom_point(aes(y = Fragment.trend),position = position_dodge(width = 0.5), size=2) +
    scale_color_manual(values = c("darkblue", "skyblue4","lightblue")) +
    labs(x = "", y = "Estimated fragmentation coefficient", color="Habitat amount") +
    geom_hline(aes(yintercept = 0), linetype="dashed")+
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size=14))
  
  
  # 
  # model3 <- glmmTMB(binary~Fragment*Habitat*TraitGroup+
  #                     Fragment*Habitat*Matrix+
  #                     PC1+PC2+(1|Year),
  #                   ziformula = ~PC1+PC2+(1|Year),
  #                   control=glmmTMBControl(optimizer=optim,
  #                                          optCtrl=list(maxit= 1e9),
  #                                          optArgs=list(method="BFGS")),
  #                   family=binomial(),
  #                   data=final.data)
  
  
  
  Models.rich[[buffer]] <- model2
  modelsum.rich[[buffer]] <- summary(model2) 
  anovas.rich[[buffer]] <- car::Anova(model2)
  cis.rich[[buffer]] <- confint(model2)
  
  
}
# Combine everything NEW WITH 4-WAY INTERACTION####
Models <- list()
modelsum <- list()
anovas <- list()
all_predict <- list()
cis <- list()
Frag.Hab.Mat.df <- list()
Frag.Hab.Mat.plot <-list()
trait.lineplot <- list()
matrix.lineplot <- list()
Frag.Hab.TG.df <- list()
Frag.Hab.TG.plot <-list()

for (buffer in c( 5, 10, 20)) {
  
  print(buffer)
  
  semifinal.data <- final.comm %>% 
    left_join(enviro.output, by = "Site", na_matches ="never") %>%
    distinct(.keep_all = TRUE)
  
  final.data <- semifinal.data %>%
    left_join(scales[[buffer]], by="Site",na_matches="never") %>%
    distinct(.keep_all = TRUE)
  
  #correct the variable types
  final.data$Year <- as.factor(final.data$Year)
  final.data$Fragment <- as.numeric(final.data$Fragment)
  final.data$Matrix   <- as.numeric(final.data$Develop)
  final.data$Habitat  <- as.numeric(final.data$Habitat)
  final.data$PC1      <- as.numeric(final.data$PC1)
  final.data$PC2      <- as.numeric(final.data$PC2)
  
  
  # final.data <- final.data %>%
  #   mutate(across(c(Fragment, Habitat, Matrix, PC1, PC2), scale)) 
  # 
  
  final.data <- final.data %>% filter(abs(Fragment) < 5 &
                                        abs(Habitat) < 5 &
                                        abs(Matrix < 5))
  
  
  final.data$binary <- ifelse(final.data$Abundance > 0 , 1, 0)
  
  final.data <- final.data %>% filter(Abundance < 1000)
  
  
  model1 <- glmmTMB(Abundance~Fragment*Habitat*TraitGroup+
                      # Fragment*Habitat*Matrix+
                      PC1+PC2+(1|Year),
                    ziformula = ~1,
                    control=glmmTMBControl(optimizer=optim,
                                           optCtrl=list(maxit= 1e9),
                                           optArgs=list(method="L-BFGS-B")),
                    family=nbinom2(),
                    data=final.data)
  
  model2 <- glmmTMB(Abundance~Fragment*Habitat*Matrix*TraitGroup+
                      # Fragment*Habitat*Matrix+
                      PC1+PC2+(1|Year),
                    ziformula = ~1,
                    control=glmmTMBControl(optimizer=optim,
                                           optCtrl=list(maxit= 1e9),
                                           optArgs=list(method="L-BFGS-B")),
                    family=nbinom2(),
                    data=final.data)
  
  hab_q <- quantile(final.data$Habitat, probs = c(0.01, 0.5, 0.99), na.rm = TRUE)
  emm <- emmeans::emmeans(
    model1,
    ~ Fragment | TraitGroup * Habitat,
    at = list(
      Fragment = seq(min(final.data$Fragment, na.rm = TRUE),
                     max(final.data$Fragment, na.rm = TRUE),
                     length.out = 50),
      Habitat = as.numeric(hab_q),
      PC1 = mean(final.data$PC1, na.rm = TRUE),
      PC2 = mean(final.data$PC2, na.rm = TRUE),
      Matrix = mean(final.data$Matrix, na.rm=TRUE)
    ),
    type = "response"
  )
  
  emm_df <- as.data.frame(emm)
  
  emm_df <- emm_df %>% mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
                                                   Habitat == median(Habitat) ~ "Medium",
                                                   Habitat == min(Habitat) ~ "Low"),
                              Habitat = factor(Habitat,levels = c("High","Medium","Low")),
                              TraitGroup = factor(
                                str_wrap(TraitGroup, width = 20),   # 👈 wrap labels here
                                levels = str_wrap(c(
                                  "Big & winged",
                                  "Small beetles & bugs",
                                  "Slower reproducing, stationary flies", 
                                  "Drift-prone, quicker reproducing flies",
                                  "Long-lived & passive"
                                ), width = 20)))
  
  trait.lineplot[[buffer]] <- ggplot() +
    # geom_point(data=final.data, aes(x=Fragment, y=Abundance), color="black")+
    geom_line(data=emm_df, aes(x = Fragment, y = response, color = factor(Habitat)), linewidth=1) +
    geom_ribbon(data=emm_df, aes(x = Fragment,ymin = response-SE, ymax = response+SE, fill = factor(Habitat)),
                alpha = 0.2, color = NA) +
    facet_wrap(~TraitGroup, scales = "free_y", ncol=5) +
    scale_color_manual(values = c("black", "gray30","gray60")) +
    scale_fill_manual(values = c("black", "gray30","gray60")) +
    labs(x = "Fragmentation", y = "Predicted Abundance",
         color = "Habitat amount", fill = "Habitat amount") +
    theme_classic()
  
  
  
  mat_q <- quantile(final.data$Matrix, probs = c(0.01, 0.5, 0.99), na.rm = TRUE)
  emm2 <- emmeans::emmeans(
    model2,
    ~ Fragment | Matrix* Habitat*TraitGroup,   # vary Matrix, condition plots by Matrix
    at = list(
      Fragment = seq(min(final.data$Fragment, na.rm = TRUE),
                     max(final.data$Fragment, na.rm = TRUE),
                     length.out = 50),
      Matrix   = as.numeric(mat_q),
      TraitGroup = unique(final.data$TraitGroup),  # or whichever level you want fixed
      PC1 = mean(final.data$PC1, na.rm = TRUE),
      PC2 = mean(final.data$PC2, na.rm = TRUE),
      Habitat = as.numeric(hab_q) # now held constant
    ),
    type = "response"
  )
  
  
  emm_df2 <- as.data.frame(emm2)
  
  emm_df2 <- emm_df2 %>% mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
                                                     Habitat == median(Habitat) ~ "Medium",
                                                     Habitat == min(Habitat) ~ "Low"),
                                Habitat = factor(Habitat,levels = c("High","Medium","Low")),
                                Matrix = case_when( Matrix == min(Matrix) ~ "High",
                                                    Matrix == median(Matrix) ~ "Medium",
                                                    Matrix == max(Matrix) ~ "Low"),
                                Matrix = factor(Matrix, levels = c("High","Medium", "Low")))
  
  
  matrix.lineplot[[buffer]] <- ggplot() +
    # geom_point(data=final.data, aes(x=Fragment, y=Abundance), color="black")+
    geom_line(data=emm_df2, aes(x = Fragment, y = response, color = factor(Habitat)), linewidth=1) +
    geom_ribbon(data=emm_df2, aes(x = Fragment,ymin = response-SE, ymax = response+SE, fill = factor(Habitat)),
                alpha = 0.2, color = NA) +
    facet_grid(rows=vars(TraitGroup),cols= vars(Matrix), scales = "free_y") +
    scale_color_manual(values = c("black", "gray30","gray60")) +
    scale_fill_manual(values = c("black", "gray30","gray60")) +
    labs(x = "Fragmentation", y = "Predicted Abundance",
         color = "Habitat amount", fill = "Habitat amount") +
    theme_classic()
  
  #AIC(model2,model2alt)
  
  
  # Define sequences for continuous predictors
  seq_Fragment <- seq(min(final.data$Fragment, na.rm = TRUE), max(final.data$Fragment, na.rm = TRUE), length.out = 30)
  seq_Habitat <- seq(min(final.data$Habitat, na.rm = TRUE), max(final.data$Habitat, na.rm = TRUE), length.out = 30)
  seq_Matrix <- seq(min(final.data$Matrix, na.rm = TRUE), max(final.data$Matrix, na.rm = TRUE), length.out = 30)
  
  # Get mean values for control predictors
  mean_PC1 <- mean(final.data$PC1, na.rm = TRUE)
  mean_PC2 <- mean(final.data$PC2, na.rm = TRUE)
  mean_Year <- levels(final.data$Year)[1]  # reference year
  
  # Get levels for trait group
  trait_levels <- unique(final.data$TraitGroup)
  
  selected.trait <- trait_levels[1]
  
  # all_grids$Frag_Hab_TG <- expand.grid(
  #   Fragment = seq_Fragment,
  #   Habitat = seq_Habitat,
  #   Matrix = mean(final.data$Matrix, na.rm = TRUE),
  #   PC1 = mean_PC1,
  #   PC2 = mean_PC2,
  #   Year = mean_Year,
  #   TraitGroup = as.vector(na.omit(trait_levels))
  # )
  # 
  # predict2 <- predict(model2, newdata = all_grids$Frag_Hab_TG, type = "response", se.fit = TRUE)
  # predict.df2 <- cbind(all_grids$Frag_Hab_TG, fit = predict2$fit, se = predict2$se.fit)
  # 
  # 
  # predict.df2$TraitGroup <-factor(predict.df2$TraitGroup, levels=c("Big & winged",
  #                                                                   "Drift-prone, quicker reproducing flies",
  #                                                                   "Long-lived & passive",
  #                                                                   "Slower reproducing, stationary flies",
  #                                                                   "Small beetles & bugs"))
  # ggplot(data = predict.df2, aes(x = Fragment, y = Habitat)) +
  #   geom_tile(aes(fill = fit)) +
  #   geom_contour(aes(z = fit), color = "white", binwidth = 0.005) +
  #   labs(x = "Fragmentation", y = "Habitat", fill="") +
  #   scale_fill_viridis_c() +
  #   facet_wrap(~TraitGroup, ncol=5)+
  #   theme_classic() +
  #   theme(text = element_text(size = 14))
  
  
  Frag_Hab_TG <- data.frame(emmeans::emtrends(model1, ~TraitGroup|Habitat, var = "Fragment",
                                              at= list(Matrix = mean(final.data$Matrix, na.rm = TRUE),
                                                       Habitat = quantile(final.data$Habitat, na.rm = TRUE, probs = c(0.01,0.50,0.99)),
                                                       PC1 = mean_PC1,
                                                       PC2 = mean_PC2)))
  
  
  Frag_Hab_Mat <- data.frame(emmeans::emtrends(model2, ~TraitGroup|Matrix|Habitat, var = "Fragment",
                                               at= list(Matrix = quantile(final.data$Matrix, na.rm = TRUE,probs = c(0.01,0.50,0.99)),
                                                        Habitat = quantile(final.data$Habitat, na.rm = TRUE, probs = c(0.01,0.50,0.99)),
                                                        PC1 = mean_PC1,
                                                        PC2 = mean_PC2)))
  
  
  
  
  # Frag_Hab_Mat_TG <- data.frame(emmeans::emtrends(model2, ~TraitGroup|Matrix|Habitat, var = "Fragment",
  #                                              at= list(Matrix = quantile(final.data$Matrix, na.rm = TRUE,probs = c(0.01,0.50,0.99)),
  #                                                       Habitat = quantile(final.data$Habitat, na.rm = TRUE, probs = c(0.01,0.50,0.99)),
  #                                                       PC1 = mean_PC1,
  #                                                       PC2 = mean_PC2)))
  # 
  # plotFrag_Hab_Mat_TG <- Frag_Hab_Mat_TG  %>% 
  #   mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
  #                               Habitat == median(Habitat) ~ "Medium",
  #                               Habitat == min(Habitat) ~ "Low"),
  #          Habitat = factor(Habitat, levels = c("High","Medium", "Low")),
  #          Matrix = case_when( Matrix == min(Matrix) ~ "High",
  #                              Matrix == median(Matrix) ~ "Medium",
  #                              Matrix == max(Matrix) ~ "Low"),
  #          Matrix = factor(Matrix, levels = c("High","Medium", "Low")),
  #          TraitGroup = factor(TraitGroup, levels = c("Big & winged",
  #                                                     "Drift-prone, quicker reproducing flies",
  #                                                     "Long-lived & passive",
  #                                                     "Small beetles & bugs",
  #                                                     "Slower reproducing, stationary flies"
  #          )))
  
  
  # Frag.Hab.Mat.TG.plot[[buffer]] <- ggplot(data =  plotFrag_Hab_Mat_TG, aes(x =Matrix, color = Habitat, group = Habitat)) +
  #   geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.4, size=1,
  #                 position = position_dodge(width = 0.5)) +
  #   geom_point(aes(y = Fragment.trend),position = position_dodge(width = 0.5), size=2) +
  #   scale_color_manual(values = c("black","gray30","gray70")) +
  #   labs(x = "Matrix quality", 
  #        y = "Estimated fragmentation coefficient",
  #        color = "Habitat amount") +
  #   geom_hline(aes(yintercept = 0), linetype="dashed")+
  #   theme_classic() +
  #   facet_wrap(~TraitGroup)+
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #         text = element_text(size=18))
  #          
  
  
  Frag.Hab.Mat.df[[buffer]] <- Frag_Hab_Mat
  
  plotFrag_Hab_Mat <- Frag_Hab_Mat  %>% 
    mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
                                Habitat == median(Habitat) ~ "Medium",
                                Habitat == min(Habitat) ~ "Low"),
           Habitat = factor(Habitat, levels = c("High","Medium", "Low")),
           Matrix = case_when( Matrix == min(Matrix) ~ "High",
                               Matrix == median(Matrix) ~ "Medium",
                               Matrix == max(Matrix) ~ "Low"),
           Matrix = factor(Matrix, levels = c("High","Medium", "Low")), 
           TraitGroup = factor(
             str_wrap(TraitGroup, width = 20),   # 👈 wrap labels here
             levels = str_wrap(c(
               "Big & winged",
               "Small beetles & bugs",
               "Slower reproducing, stationary flies", 
               "Drift-prone, quicker reproducing flies",
               "Long-lived & passive"
             ), width = 20)))
  
  
  Frag.Hab.Mat.plot[[buffer]] <- ggplot(data =  plotFrag_Hab_Mat , aes(x =Matrix, color = Habitat, group = Habitat)) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.4, size=1,
                  position = position_dodge(width = 0.5)) +
    geom_point(aes(y = Fragment.trend),position = position_dodge(width = 0.5), size=2) +
    scale_color_manual(values = c("black", "gray30","gray70")) +
    facet_wrap(~TraitGroup, ncol=5)+
    labs(x = "Matrix quality", 
         y = "Estimated fragmentation coefficient",
         color = "Habitat amount") +
    geom_hline(aes(yintercept = 0), linetype="dashed")+
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size=16))
  
  plotFrag_Hab_TG <- Frag_Hab_TG  %>% 
    mutate(Habitat = case_when(  Habitat == max(Habitat) ~ "High",
                                 Habitat == median(Habitat) ~ "Medium",
                                 Habitat == min(Habitat) ~ "Low"),
           Habitat = factor(Habitat, levels = c("High","Medium", "Low")),
           TraitGroup = factor(
             str_wrap(TraitGroup, width = 20),   # 👈 wrap labels here
             levels = str_wrap(c(
               "Big & winged",
               "Small beetles & bugs",
               "Slower reproducing, stationary flies", 
               "Drift-prone, quicker reproducing flies",
               "Long-lived & passive"
             ), width = 20)))
  
  
  Frag.Hab.TG.df[[buffer]] <- Frag_Hab_TG
  
  
  
  Frag.Hab.TG.plot[[buffer]] <- ggplot(data =  plotFrag_Hab_TG , aes(x =TraitGroup, color = Habitat, group = Habitat)) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.4,size=1,
                  position = position_dodge(width = 0.5)) +
    geom_point(aes(y = Fragment.trend),position = position_dodge(width = 0.5), size=2) +
    scale_color_manual(values = c("black", "gray30","gray60")) +
    labs(x = "", y = "Estimated fragmentation coefficient", color="Habitat amount") +
    geom_hline(aes(yintercept = 0), linetype="dashed")+
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size=16))
  
  
  # 
  # model3 <- glmmTMB(binary~Fragment*Habitat*TraitGroup+
  #                     Fragment*Habitat*Matrix+
  #                     PC1+PC2+(1|Year),
  #                   ziformula = ~PC1+PC2+(1|Year),
  #                   control=glmmTMBControl(optimizer=optim,
  #                                          optCtrl=list(maxit= 1e9),
  #                                          optArgs=list(method="BFGS")),
  #                   family=binomial(),
  #                   data=final.data)
  
  
  
  Models[[buffer]] <- model2
  modelsum[[buffer]] <- summary(model2) 
  anovas[[buffer]] <- car::Anova(model2)
  cis[[buffer]] <- confint(model2)
  
  
}
#make those predictions!








Models.rich <- list()
modelsum.rich <- list()
anovas.rich <- list()
all_predict.rich <- list()
cis.rich <- list()
Frag.Hab.Mat.df.rich <- list()
Frag.Hab.Mat.plot.rich <-list()
trait.lineplot.rich <- list()
matrix.lineplot.rich <- list()
Frag.Hab.TG.df.rich <- list()
Frag.Hab.TG.plot.rich <-list()
for (buffer in c( 5, 10, 20)) {
  
  print(buffer)
  
  semifinal.data <- final.comm %>% 
    left_join(enviro.output, by = "Site", na_matches ="never") %>%
    distinct(.keep_all = TRUE)
  
  final.data <- semifinal.data %>%
    left_join(scales[[buffer]], by="Site",na_matches="never") %>%
    distinct(.keep_all = TRUE)
  
  #correct the variable types
  final.data$Year <- as.factor(final.data$Year)
  final.data$Fragment <- as.numeric(final.data$Fragment)
  final.data$Matrix   <- as.numeric(final.data$Develop)
  final.data$Habitat  <- as.numeric(final.data$Habitat)
  final.data$PC1      <- as.numeric(final.data$PC1)
  final.data$PC2      <- as.numeric(final.data$PC2)
  
  
  final.data <- final.data %>%
    mutate(across(c(Fragment, Habitat, Matrix, PC1, PC2), scale)) 
  
  
  final.data <- final.data %>% filter(abs(Fragment) < 5 &
                                        abs(Habitat) < 5 &
                                        abs(Matrix < 5))
  
  
  final.data$binary <- ifelse(final.data$Abundance > 0 , 1, 0)
  # 
  final.data2 <- final.data %>%
    group_by(Site, TraitGroup, PC1, PC2, Year, ANN, Habitat, Matrix, Develop, Agriculture, Fragment, ExtraDevelop) %>%
    summarise(Abundance = sum(Abundance, na.rm = TRUE),
              Richness = sum(binary, na.rm=T),.groups = "drop_last") %>%
    group_by(Site) %>%
    mutate(TotalAbundance = sum(Abundance, na.rm = TRUE),
           RelAbundance = Abundance / TotalAbundance,
           TotalRichness = sum(Richness, na.rm=T),
           RelRichness = Richness/TotalRichness) %>%
    ungroup() %>%
    mutate(RelAbundance = case_when(
      RelAbundance == 0 ~ RelAbundance + 1e-5,
      RelAbundance == 1 ~ RelAbundance - 1e-5,
      TRUE ~ RelAbundance),
      RelRichness = case_when(
        RelRichness == 0 ~ RelRichness + 1e-5,
        RelRichness == 1 ~ RelRichness - 1e-5,
        TRUE ~ RelRichness
      ))%>% filter(Abundance < 3000)
  
  
  
  final.data <- final.data %>% filter(Abundance < 1000)
  
  
  
  
  
  model2 <- glmmTMB(Richness~Fragment*Habitat*TraitGroup+
                      Fragment*Habitat*Matrix+
                      PC1+PC2+(1|Year),
                    ziformula = ~1,
                    control=glmmTMBControl(optimizer=optim,
                                           optCtrl=list(maxit= 1e9),
                                           optArgs=list(method="L-BFGS-B")),
                    family=genpois(),
                    data=final.data2)
  
  
  
  hab_q <- quantile(final.data$Habitat, probs = c(0.01, 0.5, 0.99), na.rm = TRUE)
  emm <- emmeans::emmeans(
    model2,
    ~ Fragment | TraitGroup * Habitat,
    at = list(
      Fragment = seq(min(final.data$Fragment, na.rm = TRUE),
                     max(final.data$Fragment, na.rm = TRUE),
                     length.out = 50),
      Habitat = as.numeric(hab_q),
      PC1 = mean(final.data$PC1, na.rm = TRUE),
      PC2 = mean(final.data$PC2, na.rm = TRUE),
      Matrix = mean(final.data$Matrix, na.rm=TRUE)
    ),
    type = "response"
  )
  
  emm_df <- as.data.frame(emm)
  
  emm_df <- emm_df %>% mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
                                                   Habitat == median(Habitat) ~ "Medium",
                                                   Habitat == min(Habitat) ~ "Low"),
                              Habitat = factor(Habitat,levels = c("High","Medium","Low")),
                              TraitGroup = factor(
                                str_wrap(TraitGroup, width = 20),   # 👈 wrap labels here
                                levels = str_wrap(c(
                                  "Big & winged",
                                  "Small beetles & bugs",
                                  "Slower reproducing, stationary flies", 
                                  "Drift-prone, quicker reproducing flies",
                                  "Long-lived & passive"
                                ), width = 20)))
  
  
  trait.lineplot.rich[[buffer]] <- ggplot() +
    # geom_point(data=final.data, aes(x=Fragment, y=Abundance), color="black")+
    geom_line(data=emm_df, aes(x = Fragment, y = response, color = factor(Habitat)), linewidth=1) +
    geom_ribbon(data=emm_df, aes(x = Fragment,ymin = response-SE, ymax = response+SE, fill = factor(Habitat)),
                alpha = 0.2, color = NA) +
    facet_wrap(~TraitGroup, scales = "free_y", ncol=5) +
    scale_color_manual(values = c("black", "gray35","gray70")) +
    scale_fill_manual(values = c("black", "gray35","gray70"))  +
    labs(x = "Fragmentation", y = "Predicted Abundance",
         color = "Habitat amount", fill = "Habitat amount") +
    theme_classic()
  
  
  
  mat_q <- quantile(final.data$Matrix, probs = c(0.01, 0.5, 0.99), na.rm = TRUE)
  emm2 <- emmeans::emmeans(
    model2,
    ~ Fragment | Matrix* Habitat,   # vary Matrix, condition plots by Matrix
    at = list(
      Fragment = seq(min(final.data$Fragment, na.rm = TRUE),
                     max(final.data$Fragment, na.rm = TRUE),
                     length.out = 50),
      Matrix   = as.numeric(mat_q),
      TraitGroup = "Big & winged",  # or whichever level you want fixed
      PC1 = mean(final.data$PC1, na.rm = TRUE),
      PC2 = mean(final.data$PC2, na.rm = TRUE),
      Habitat = as.numeric(hab_q) # now held constant
    ),
    type = "response"
  )
  
  
  emm_df2 <- as.data.frame(emm2)
  
  emm_df2 <- emm_df2 %>% mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
                                                     Habitat == median(Habitat) ~ "Medium",
                                                     Habitat == min(Habitat) ~ "Low"),
                                Habitat = factor(Habitat,levels = c("High","Medium","Low")),
                                Matrix = case_when( Matrix == min(Matrix) ~ "High",
                                                    Matrix == median(Matrix) ~ "Medium",
                                                    Matrix == max(Matrix) ~ "Low"),
                                Matrix = factor(Matrix, levels = c("High","Medium", "Low")))
  
  
  matrix.lineplot.rich[[buffer]] <- ggplot() +
    # geom_point(data=final.data, aes(x=Fragment, y=Abundance), color="black")+
    geom_line(data=emm_df2, aes(x = Fragment, y = response, color = factor(Habitat)), linewidth=1) +
    geom_ribbon(data=emm_df2, aes(x = Fragment,ymin = response-SE, ymax = response+SE, fill = factor(Habitat)),
                alpha = 0.2, color = NA) +
    facet_wrap(~Matrix, scales = "free_y", ncol=5) +
    scale_color_manual(values = c("black", "gray35","gray70"))  +
    scale_fill_manual(values = c("black", "gray35","gray70"))  +
    labs(x = "Fragmentation", y = "Predicted Abundance",
         color = "Habitat amount", fill = "Habitat amount") +
    theme_classic()
  
  #AIC(model2,model2alt)
  
  
  # Define sequences for continuous predictors
  seq_Fragment <- seq(min(final.data$Fragment, na.rm = TRUE), max(final.data$Fragment, na.rm = TRUE), length.out = 30)
  seq_Habitat <- seq(min(final.data$Habitat, na.rm = TRUE), max(final.data$Habitat, na.rm = TRUE), length.out = 30)
  seq_Matrix <- seq(min(final.data$Matrix, na.rm = TRUE), max(final.data$Matrix, na.rm = TRUE), length.out = 30)
  
  # Get mean values for control predictors
  mean_PC1 <- mean(final.data$PC1, na.rm = TRUE)
  mean_PC2 <- mean(final.data$PC2, na.rm = TRUE)
  mean_Year <- levels(final.data$Year)[1]  # reference year
  
  # Get levels for trait group
  trait_levels <- unique(final.data$TraitGroup)
  
  selected.trait <- trait_levels[1]
  
  # all_grids$Frag_Hab_TG <- expand.grid(
  #   Fragment = seq_Fragment,
  #   Habitat = seq_Habitat,
  #   Matrix = mean(final.data$Matrix, na.rm = TRUE),
  #   PC1 = mean_PC1,
  #   PC2 = mean_PC2,
  #   Year = mean_Year,
  #   TraitGroup = as.vector(na.omit(trait_levels))
  # )
  # 
  # predict2 <- predict(model2, newdata = all_grids$Frag_Hab_TG, type = "response", se.fit = TRUE)
  # predict.df2 <- cbind(all_grids$Frag_Hab_TG, fit = predict2$fit, se = predict2$se.fit)
  # 
  # 
  # predict.df2$TraitGroup <-factor(predict.df2$TraitGroup, levels=c("Big & winged",
  #                                                                   "Drift-prone, quicker reproducing flies",
  #                                                                   "Long-lived & passive",
  #                                                                   "Slower reproducing, stationary flies",
  #                                                                   "Small beetles & bugs"))
  # ggplot(data = predict.df2, aes(x = Fragment, y = Habitat)) +
  #   geom_tile(aes(fill = fit)) +
  #   geom_contour(aes(z = fit), color = "white", binwidth = 0.005) +
  #   labs(x = "Fragmentation", y = "Habitat", fill="") +
  #   scale_fill_viridis_c() +
  #   facet_wrap(~TraitGroup, ncol=5)+
  #   theme_classic() +
  #   theme(text = element_text(size = 14))
  
  
  Frag_Hab_TG <- data.frame(emmeans::emtrends(model2, ~TraitGroup|Habitat, var = "Fragment",
                                              at= list(Matrix = mean(final.data$Matrix, na.rm = TRUE),
                                                       Habitat = quantile(final.data$Habitat, na.rm = TRUE, probs = c(0.01,0.50,0.99)),
                                                       PC1 = mean_PC1,
                                                       PC2 = mean_PC2)))
  
  
  Frag_Hab_Mat <- data.frame(emmeans::emtrends(model2, ~Matrix|Habitat, var = "Fragment",
                                               at= list(Matrix = quantile(final.data$Matrix, na.rm = TRUE,probs = c(0.01,0.50,0.99)),
                                                        Habitat = quantile(final.data$Habitat, na.rm = TRUE, probs = c(0.01,0.50,0.99)),
                                                        PC1 = mean_PC1,
                                                        PC2 = mean_PC2)))
  
  
  
  
  # Frag_Hab_Mat_TG <- data.frame(emmeans::emtrends(model2, ~TraitGroup|Matrix|Habitat, var = "Fragment",
  #                                              at= list(Matrix = quantile(final.data$Matrix, na.rm = TRUE,probs = c(0.01,0.50,0.99)),
  #                                                       Habitat = quantile(final.data$Habitat, na.rm = TRUE, probs = c(0.01,0.50,0.99)),
  #                                                       PC1 = mean_PC1,
  #                                                       PC2 = mean_PC2)))
  # 
  # plotFrag_Hab_Mat_TG <- Frag_Hab_Mat_TG  %>% 
  #   mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
  #                               Habitat == median(Habitat) ~ "Medium",
  #                               Habitat == min(Habitat) ~ "Low"),
  #          Habitat = factor(Habitat, levels = c("High","Medium", "Low")),
  #          Matrix = case_when( Matrix == min(Matrix) ~ "High",
  #                              Matrix == median(Matrix) ~ "Medium",
  #                              Matrix == max(Matrix) ~ "Low"),
  #          Matrix = factor(Matrix, levels = c("High","Medium", "Low")),
  #          TraitGroup = factor(TraitGroup, levels = c("Big & winged",
  #                                                     "Drift-prone, quicker reproducing flies",
  #                                                     "Long-lived & passive",
  #                                                     "Small beetles & bugs",
  #                                                     "Slower reproducing, stationary flies"
  #          )))
  
  
  # Frag.Hab.Mat.TG.plot[[buffer]] <- ggplot(data =  plotFrag_Hab_Mat_TG, aes(x =Matrix, color = Habitat, group = Habitat)) +
  #   geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.4, size=1,
  #                 position = position_dodge(width = 0.5)) +
  #   geom_point(aes(y = Fragment.trend),position = position_dodge(width = 0.5), size=2) +
  #   scale_color_manual(values = c("black","gray30","gray70")) +
  #   labs(x = "Matrix quality", 
  #        y = "Estimated fragmentation coefficient",
  #        color = "Habitat amount") +
  #   geom_hline(aes(yintercept = 0), linetype="dashed")+
  #   theme_classic() +
  #   facet_wrap(~TraitGroup)+
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #         text = element_text(size=18))
  #          
  
  
  Frag.Hab.Mat.df.rich[[buffer]] <- Frag_Hab_Mat
  
  plotFrag_Hab_Mat <- Frag_Hab_Mat  %>% 
    mutate(Habitat = case_when( Habitat == max(Habitat) ~ "High",
                                Habitat == median(Habitat) ~ "Medium",
                                Habitat == min(Habitat) ~ "Low"),
           Habitat = factor(Habitat, levels = c("High","Medium", "Low")),
           Matrix = case_when( Matrix == min(Matrix) ~ "High",
                               Matrix == median(Matrix) ~ "Medium",
                               Matrix == max(Matrix) ~ "Low"),
           Matrix = factor(Matrix, levels = c("High","Medium", "Low")))
  
  
  Frag.Hab.Mat.plot.rich[[buffer]] <- ggplot(data =  plotFrag_Hab_Mat , aes(x =Matrix, color = Habitat, group = Habitat)) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.4, size=1,
                  position = position_dodge(width = 0.5)) +
    geom_point(aes(y = Fragment.trend),position = position_dodge(width = 0.5), size=2) +
    scale_color_manual(values = c("black", "gray30","gray60")) +
    labs(x = "Matrix quality", 
         y = "Estimated fragmentation coefficient",
         color = "Habitat amount") +
    geom_hline(aes(yintercept = 0), linetype="dashed")+
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size=18))
  
  plotFrag_Hab_TG <- Frag_Hab_TG  %>% 
    mutate(Habitat = case_when(  Habitat == max(Habitat) ~ "High",
                                 Habitat == median(Habitat) ~ "Medium",
                                 Habitat == min(Habitat) ~ "Low"),
           Habitat = factor(Habitat, levels = c("High","Medium", "Low")),
           TraitGroup = factor(
             str_wrap(TraitGroup, width = 20),   # 👈 wrap labels here
             levels = str_wrap(c(
               "Big & winged",
               "Small beetles & bugs",
               "Slower reproducing, stationary flies", 
               "Drift-prone, quicker reproducing flies",
               "Long-lived & passive"
             ), width = 20)))
  
  
  Frag.Hab.TG.df.rich[[buffer]] <- Frag_Hab_TG
  
  
  
  Frag.Hab.TG.plot.rich[[buffer]] <- ggplot(data =  plotFrag_Hab_TG , aes(x =TraitGroup, color = Habitat, group = Habitat)) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.4,size=1,
                  position = position_dodge(width = 0.5)) +
    geom_point(aes(y = Fragment.trend),position = position_dodge(width = 0.5), size=2) +
    scale_color_manual(values = c("black", "gray30","gray60")) +
    labs(x = "", y = "Estimated fragmentation coefficient", color="Habitat amount") +
    geom_hline(aes(yintercept = 0), linetype="dashed")+
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          text = element_text(size=14))
  
  
  # 
  # model3 <- glmmTMB(binary~Fragment*Habitat*TraitGroup+
  #                     Fragment*Habitat*Matrix+
  #                     PC1+PC2+(1|Year),
  #                   ziformula = ~PC1+PC2+(1|Year),
  #                   control=glmmTMBControl(optimizer=optim,
  #                                          optCtrl=list(maxit= 1e9),
  #                                          optArgs=list(method="BFGS")),
  #                   family=binomial(),
  #                   data=final.data)
  
  
  
  Models.rich[[buffer]] <- model2
  modelsum.rich[[buffer]] <- summary(model2) 
  anovas.rich[[buffer]] <- car::Anova(model2)
  cis.rich[[buffer]] <- confint(model2)
  
  
}
#make those predictions!


#number of samples removed ####
for (buffer in c(5, 10, 20)) {
  print(buffer)
  
  semifinal.data <- final.comm %>% 
    left_join(enviro.output, by = "Site", na_matches = "never") %>%
    distinct(.keep_all = TRUE)
  
  final.data <- semifinal.data %>%
    left_join(scales[[buffer]], by = "Site", na_matches = "never") %>%
    distinct(.keep_all = TRUE)
  
  print("unfiltered final")
  n_unfiltered <- length(unique(final.data$Site))
  print(n_unfiltered)
  
  # Correct types
  final.data$Year     <- as.factor(final.data$Year)
  final.data$Fragment <- as.numeric(final.data$Fragment)
  final.data$Matrix   <- as.numeric(final.data$Develop)
  final.data$Habitat  <- as.numeric(final.data$Habitat)
  final.data$PC1      <- as.numeric(final.data$PC1)
  final.data$PC2      <- as.numeric(final.data$PC2)
  
  # Helpers
  count_filtered_single <- function(df, var, limit = 5, n_before) {
    n_after <- df %>% filter(abs(.data[[var]]) < limit) %>% distinct(Site) %>% nrow()
    cat(sprintf("%s filtered out: %d\n", var, n_before - n_after))
  }
  
  count_filtered_pc12 <- function(df, limit = 5, n_before) {
    n_after <- df %>% filter(abs(PC1) < limit, abs(PC2) < limit) %>% distinct(Site) %>% nrow()
    cat(sprintf("PC1 & PC2 together filtered out: %d\n", n_before - n_after))
  }
  
  # NEW: combined Fragment + Habitat + Matrix (joint filter)
  count_filtered_fhm3 <- function(df, limit = 5, n_before) {
    n_after <- df %>%
      filter(abs(Fragment) < limit,
             abs(Habitat)  < limit,
             abs(Matrix)   < limit) %>%
      distinct(Site) %>% nrow()
    cat(sprintf("Fragment & Habitat & Matrix together filtered out: %d\n",
                n_before - n_after))
  }
  
  # Per-variable drops (vs unfiltered)
  count_filtered_single(final.data, "Fragment", 5, n_unfiltered)
  count_filtered_single(final.data, "Habitat",  5, n_unfiltered)
  count_filtered_single(final.data, "Matrix",   5, n_unfiltered)
  count_filtered_single(final.data, "PC1",      5, n_unfiltered)
  count_filtered_single(final.data, "PC2",      5, n_unfiltered)
  
  # Joint drops
  count_filtered_pc12(final.data, 5, n_unfiltered)
  count_filtered_fhm3(final.data, 5, n_unfiltered)
  
  # Final combined filtering (all variables together)
  final.data <- final.data %>%
    filter(abs(Fragment) < 5,
           abs(Habitat)  < 5,
           abs(Matrix)   < 5,
           abs(PC1)      < 5,
           abs(PC2)      < 5)
  
  print("filtered final")
  n_filtered <- length(unique(final.data$Site))
  print(n_filtered)
  cat("Total sites removed (all filters):", n_unfiltered - n_filtered, "\n\n")
}


#raw metrics for spatial varaibles ####
# helper to z-score safely
zscore_safe <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(x - m)
  (x - m) / s
}

range_summary <- tibble::tibble(
  buffer = integer(),
  n_sites_before = integer(),
  n_sites_after  = integer(),
  habitat_min = numeric(),
  habitat_max = numeric(),
  matrix_min  = numeric(),
  matrix_max  = numeric()
)

for (buffer in c(5, 10, 20)) {
  message("Buffer: ", buffer)
  
  # --- NEW: scale first, inside scales.raw[[buffer]] ---
  scaled_buf <- scales.raw[[buffer]] %>%
    dplyr::mutate(
      # ensure a Matrix column exists for z-scoring; keep raw values untouched
,
      Z_Habitat  = zscore_safe(Habitat),
      Z_Matrix   = zscore_safe(Develop)
    ) 
  
  # --- join the other datasets (unchanged, except we join scaled_buf) ---
  semifinal.data <- final.comm[c("Site","Source")] %>% 
    dplyr::left_join(enviro.output, by = "Site", na_matches ="never") %>%
    dplyr::distinct(.keep_all = TRUE)
  
  final.data <- semifinal.data %>%
    dplyr::left_join(scaled_buf, by="Site", na_matches="never") %>%
    dplyr::distinct(.keep_all = TRUE)
  
  n_before <- dplyr::n_distinct(final.data$Site)
  
  # rename if Matrix not present (use Develop)  (kept as you had)
  final.data$Matrix <- final.data$Develop
  
  # use the precomputed Z-columns for filtering (no recompute here)
  zcols <- intersect(c("Z_Habitat","Z_Matrix","Z_PC1","Z_PC2"), names(final.data))
  
  # apply filters (abs < 5 for all present Z vars)
  filtered <- final.data %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(zcols), ~ abs(.x) < 5))
  
  n_after <- dplyr::n_distinct(filtered$Site)
  
  # get ranges on unscaled Habitat and Matrix (unchanged)
  h_rng <- if ("Habitat" %in% names(filtered)) {
    range(filtered$Habitat, na.rm = TRUE)
  } else c(NA_real_, NA_real_)
  
  m_rng <-  
    range(filtered$Develop, na.rm = TRUE)
  
  range_summary <- dplyr::bind_rows(
    range_summary,
    tibble::tibble(
      buffer = buffer,
      n_sites_before = n_before,
      n_sites_after  = n_after,
      habitat_min = h_rng[1],
      habitat_max = h_rng[2],
      matrix_min  = m_rng[1],
      matrix_max  = m_rng[2]
    )
  )
}

print(range_summary)




