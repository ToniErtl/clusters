library(foreign)
library(cluster)

for (k in 0:1) {
	data <- read.dta("~/Data Archive/ConstructedData/children_familyAggregate_stat12.dta", convert.factors = F)
	
	#Data preparation including NAs
	
	id <- data.frame(data$slno, data$mid)
	data <- data.frame(data$patient_choicesOffspringMean, data$patient_choices_father, data$patient_choices_mother, data$binswangerOffspringMean, data$binswanger_father, data$binswanger_mother, data$spitefulOffspringMean, data$spiteful_father, data$spiteful_mother, data$altruisticOffspringMean, data$altruistic_father, data$altruistic_mother, data$egalitarianOffspringMean, data$egalitarian_father, data$egalitarian_mother, data$selfishOffspringMean, data$selfish_father, data$selfish_mother)
	
	# Data preparation when removing NAs
	
	if (removeNA <- k) {
		id <- id[complete.cases(data), ]
		data <- data[complete.cases(data), ] # only pamx works with NAs
		}
		
		data <- scale(data)
		mydata <- data
		rm(data)
		
		require(fpc)
		pamx <- pamk(mydata, krange=2:30,criterion="asw", usepam = TRUE,critout = T)
		
		# 2 clusters are optimal according to average silhouette width
		
		require(cluster)
		pamx <- pam(mydata, 2)
		
		if (!removeNA) {
			
			# Save clustering when NAs are not removed
			
			save <- data.frame(id, pamx$clustering)
			save <- setNames(save, c("slno","mid","pam_full"))
			
			# Save results
			
			write.dta(save, "~/Data Archive/ConstructedData/cluster_pam_full.dta")
			} 
			else {
			
			# Save clustering when NAs are not removed
			
			save <- data.frame(id, pamx$clustering)
			save <- setNames(save, c("slno","mid","pam_narm"))
			
			# Save results
			write.dta(save, "~/Data Archive/ConstructedData/cluster_pam_narm.dta")
			}
		}