library(tidyr)
load(file="cache/S1.RData")


# Aggregate on several temporal levels, compute dyadic interdependence
computeInterdependence <- function(scaleName) {
	moment_level <- S1 %>% 
		group_by(couple_moment_uid, sex) %>% 
		summarise(y:=mean(get(scaleName), na.rm=TRUE)) %>% 
		ungroup() %>% 
		select(couple_moment_uid, sex, y) %>%
		spread(sex, y) %>% 
		select(-couple_moment_uid)
	
	moment.dyadicCor <- cor(moment_level, use="p")


	day_level <- S1 %>% 
		group_by(couple_day_uid, sex) %>% 
		summarise(y=mean(get(scaleName), na.rm=TRUE)) %>% 
		ungroup() %>% 
		select(couple_day_uid, sex, y) %>%
		spread(sex, y) %>% 
		select(-couple_day_uid)
	
	day.dyadicCor <- cor(day_level, use="p")


	person_level <- S1 %>% 
		group_by(couple_uid, sex) %>% 
		summarise(y=mean(get(scaleName), na.rm=TRUE)) %>% 
		ungroup() %>% 
		select(couple_uid, sex, y) %>%
		spread(sex, y) %>% 
		select(-couple_uid)
	
	person.dyadicCor <- cor(person_level, use="p")	
	
	return(list(person=person.dyadicCor[1, 2], day=day.dyadicCor[1, 2], moment=moment.dyadicCor[1, 2]))
}

computeInterdependence("C")
computeInterdependence("A")
computeInterdependence("Ind")
computeInterdependence("Pow")
computeInterdependence("RS2")