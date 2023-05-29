# with Intercept
mylm<-function(x,y){
	s = summary(lm(y ~ x))$coefficients
	toPrint=paste(s[2,1],s[2,2],s[2,4],sep='\t')
	s_y = qtrans(y)
	s = summary(lm(s_y ~ x))$coefficients
	toPrint=paste(toPrint,s[2,1],s[2,2],s[2,4],sep='\t')
	return(toPrint)
}

# with Intercept, age and sex
mylm2<-function(y,x,age1,age2,sex1,sex2){
	s = summary(lm(y ~ x + as.numeric(age1) + as.numeric(age2) + as.factor(sex1) + as.factor(sex2)))$coefficients
	toPrint=paste(s[2,1],s[2,2],s[2,4],sep='\t')
	for (i in 3:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,4],sep='\t')
	}
	s_y = qtrans(y)
	s = summary(lm(s_y ~ x + as.numeric(age1) + as.numeric(age2) + as.factor(sex1) + as.factor(sex2) ))$coefficients
	for (i in 2:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,4],sep='\t')
	}
	return(toPrint)
}

# with Intercept. DQ, familyQ, random effect for families
mylmer <- function(phenotype,DQ,familyQ,family,data){
	lmer1 = lmer(phenotype~DQ+familyQ+(1|family),data=data)
	s = summary(lmer1)$coefficients
	toPrint=paste(s[2,1],s[2,2],s[2,5],sep='\t')
	for (i in 3:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,5],sep='\t')
	}
	variance = VarCorr(lmer1)$family[1,1]
	lmer1_0 = lm(phenotype~DQ+familyQ,data=data)
	lmer1_variance_st = exactLRT(lmer1,lmer1_0)
	toPrint = paste(toPrint,variance,lmer1_variance_st$statistic,lmer1_variance_st$p.value,sep='\t')
	
	data[,'phenotype'] = qtrans(data[,'phenotype'])
	lmerS = lmer(phenotype~DQ+familyQ+(1|family),data=data)
	s = summary(lmerS)$coefficients
	for (i in 2:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,5],sep='\t')
	}
	variance = VarCorr(lmerS)$family[1,1]
	lmerS_0 = lm(phenotype~DQ+familyQ,data=data)
	lmerS_variance_st = exactLRT(lmerS,lmerS_0)
	toPrint = paste(toPrint,variance,lmerS_variance_st$statistic,lmerS_variance_st$p.value,sep='\t')
	return(toPrint)
}

# with Intercept. DQ, familyQ, random effect for families, age and sex
mylmer2 <- function(phenotype,DQ,familyQ,family,age,sex,data){
	lmer1 = lmer(phenotype~DQ+familyQ+as.numeric(age)+as.factor(sex)+(1|family),data=data)
	s = summary(lmer1)$coefficients
	toPrint=paste(s[2,1],s[2,2],s[2,5],sep='\t')
	for (i in 3:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,5],sep='\t')
	}
	variance = VarCorr(lmer1)$family[1,1]
	lmer1_0 = lm(phenotype~DQ+familyQ+as.numeric(age)+as.factor(sex),data=data)
	lmer1_variance_st = exactLRT(lmer1,lmer1_0)
	toPrint = paste(toPrint,variance,lmer1_variance_st$statistic,lmer1_variance_st$p.value,sep='\t')

	data[,'phenotype'] = qtrans(data[,'phenotype'])
	lmerS = lmer(phenotype~DQ+familyQ+as.numeric(age)+as.factor(sex)+(1|family),data=data)
	s = summary(lmerS)$coefficients
	for (i in 2:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,5],sep='\t')
	}
	variance = VarCorr(lmerS)$family[1,1]
	lmerS_0 = lm(phenotype~DQ+familyQ+as.numeric(age)+as.factor(sex),data=data)
	lmerS_variance_st = exactLRT(lmerS,lmerS_0)
	toPrint = paste(toPrint,variance,lmerS_variance_st$statistic,lmerS_variance_st$p.value,sep='\t')	
	return(toPrint)
}

# with Intercept. DQ, familyQ
mylmerFake <- function(phenotype,DQ,familyQ,data){
	lmer1 = lm(phenotype~DQ+familyQ,data=data)
	s = summary(lmer1)$coefficients
	toPrint=paste(s[2,1],s[2,2],s[2,4],sep='\t')
	for (i in 3:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,4],sep='\t')
	}
	data[,'phenotype'] = qtrans(data[,'phenotype'])
	lmerS = lm(phenotype~DQ+familyQ,data=data)
	s = summary(lmerS)$coefficients
	for (i in 2:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,4],sep='\t')
	}
	return(toPrint)
}

# with Intercept. DQ, familyQ, age and sex 
mylmerFake2 <- function(phenotype,DQ,familyQ,age,sex,data){
	lmer1 = lm(phenotype~DQ+familyQ+as.numeric(age)+as.factor(sex),data=data)
	s = summary(lmer1)$coefficients
	toPrint=paste(s[2,1],s[2,2],s[2,4],sep='\t')
	for (i in 3:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,4],sep='\t')
	}
	data[,'phenotype'] = qtrans(data[,'phenotype'])
	lmerS = lm(phenotype~DQ+familyQ+as.numeric(age)+as.factor(sex),data=data)
	s = summary(lmerS)$coefficients
	for (i in 2:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,4],sep='\t')
	}	
	return(toPrint)
}

# No intercept
mylmNoIntc<-function(x,y){
	s = summary(lm(y ~ x - 1))$coefficients
	toPrint=paste(s[1,1],s[1,2],s[1,4],sep='\t')
	s_y = qtrans(y)
	s = summary(lm(s_y ~ x - 1))$coefficients
	toPrint=paste(toPrint,s[1,1],s[1,2],s[1,4],sep='\t')
	return(toPrint)
}

#No intercept, with age and sex
mylm2NoIntc<-function(y,x,age1,age2,sex1,sex2){
	s = summary(lm(y ~ x + as.numeric(age1) + as.numeric(age2) + as.factor(sex1) + as.factor(sex2) - 1))$coefficients
	toPrint=paste(s[1,1],s[1,2],s[1,4],sep='\t')
	for (i in 2:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,4],sep='\t')
	}
	s_y = qtrans(y)
	s = summary(lm(s_y ~ x + as.numeric(age1) + as.numeric(age2) + as.factor(sex1) + as.factor(sex2) - 1))$coefficients
	for (i in 1:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,4],sep='\t')
	}
	return(toPrint)
}

#No intercept, with age and sex, random effect for families
mylm2NoIntcF<-function(y,x,age1,age2,sex1,sex2,family){
	s = summary(lmer(y ~ x + as.numeric(age1) + as.numeric(age2) + as.factor(sex1) + as.factor(sex2) + (1|family) - 1))$coefficients
	toPrint=paste(s[1,1],s[1,2],s[1,5],sep='\t')
	for (i in 2:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,5],sep='\t')
	}
	s_y = qtrans(y)
	s = summary(lmer(s_y ~ x + as.numeric(age1) + as.numeric(age2) + as.factor(sex1) + as.factor(sex2) + (1|family) - 1))$coefficients
	for (i in 1:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,5],sep='\t')
	}
	return(toPrint)
}

#No intercept. With DQ, familyQ, random effect for families
mylmerNoIntc <- function(phenotype,DQ,familyQ,family,data){
	lmer1 = lmer(phenotype~DQ+familyQ+(1|family)-1,data=data)
	s = summary(lmer1)$coefficients
	toPrint=paste(s[1,1],s[1,2],s[1,5],sep='\t')
	for (i in 2:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,5],sep='\t')
	}
	variance = VarCorr(lmer1)$family[1,1]
	lmer1_0 = lm(phenotype~DQ+familyQ-1,data=data)
	lmer1_variance_st = exactLRT(lmer1,lmer1_0)
	toPrint = paste(toPrint,variance,lmer1_variance_st$statistic,lmer1_variance_st$p.value,sep='\t')
	data[,'phenotype'] = qtrans(data[,'phenotype'])
	lmerS = lmer(phenotype~DQ+familyQ+(1|family) -1 ,data=data)
	s = summary(lmerS)$coefficients
	for (i in 1:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,5],sep='\t')
	}
	variance = VarCorr(lmerS)$family[1,1]
	lmerS_0 = lm(phenotype~DQ+familyQ-1,data=data)
	lmerS_variance_st = exactLRT(lmerS,lmerS_0)
	toPrint = paste(toPrint,variance,lmerS_variance_st$statistic,lmerS_variance_st$p.value,sep='\t')
	return(toPrint)
}

#No intercept. With DQ, familyQ, random effect for families, age and sex. 
mylmer2NoIntc <- function(phenotype,DQ,familyQ,family,age,sex,data){
	lmer1 = lmer(phenotype~DQ+familyQ+as.numeric(age)+as.factor(sex)+(1|family)-1,data=data)
	s = summary(lmer1)$coefficients
	toPrint=paste(s[1,1],s[1,2],s[1,5],sep='\t')
	for (i in 2:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,5],sep='\t')
	}
	variance = VarCorr(lmer1)$family[1,1]
	if (variance==0){
		statistic = NA
		p.value = NA
	}
	else{
	lmer1_0 = lm(phenotype~DQ+familyQ+as.numeric(age)+as.factor(sex)-1,data=data)
	lmer1_variance_st = exactLRT(lmer1,lmer1_0)
		statistic = lmer1_variance_st$statistic
		p.value = lmer1_variance_st$p.value
	}
	toPrint = paste(toPrint,variance,statistic,p.value,sep='\t')

	data[,'phenotype'] = qtrans(data[,'phenotype'])
	lmerS = lmer(phenotype~DQ+familyQ+as.numeric(age)+as.factor(sex)+(1|family)-1,data=data)
	s = summary(lmerS)$coefficients
	for (i in 1:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,5],sep='\t')
	}
	variance = VarCorr(lmerS)$family[1,1]
	lmerS_0 = lm(phenotype~DQ+familyQ+as.numeric(age)+as.factor(sex)-1,data=data)
	lmerS_variance_st = exactLRT(lmerS,lmerS_0)
	toPrint = paste(toPrint,variance,lmerS_variance_st$statistic,lmerS_variance_st$p.value,sep='\t')	
	return(toPrint)
}

## No Intercept. With DQ, familyQ
mylmerFakeNoIntc <- function(phenotype,DQ,familyQ,data){
	lmer1 = lm(phenotype~DQ+familyQ-1,data=data)
	s = summary(lmer1)$coefficients
	toPrint=paste(s[1,1],s[1,2],s[1,4],sep='\t')
	for (i in 2:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,4],sep='\t')
	}
	data[,'phenotype'] = qtrans(data[,'phenotype'])
	lmerS = lm(phenotype~DQ+familyQ-1,data=data)
	s = summary(lmerS)$coefficients
	for (i in 1:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,4],sep='\t')
	}
	return(toPrint)
}

#No intercept. With DQ, familyQ, age and sex 
mylmerFake2NoIntc <- function(phenotype,DQ,familyQ,age,sex,data){
	lmer1 = lm(phenotype~DQ+familyQ+as.numeric(age)+as.factor(sex)-1,data=data)
	s = summary(lmer1)$coefficients
	toPrint=paste(s[1,1],s[1,2],s[1,4],sep='\t')
	for (i in 2:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,4],sep='\t')
	}
	data[,'phenotype'] = qtrans(data[,'phenotype'])
	lmerS = lm(phenotype~DQ+familyQ+as.numeric(age)+as.factor(sex)-1,data=data)
	s = summary(lmerS)$coefficients
	for (i in 1:dim(s)[1]){
		toPrint = paste(toPrint,s[i,1],s[i,2],s[i,4],sep='\t')
	}	
	return(toPrint)
}

getFromMerged <- function(inds,phenotype){
	indIndex = match(inds,merged$particid)
	if (paste0('B18_',phenotype) %in% colnames(merged)){
		ifelse(!is.na(merged[indIndex,paste0('B18_',phenotype)]),merged[indIndex,paste0('B18_',phenotype)],merged[indIndex,phenotype])
	}
	else{
		merged[indIndex,phenotype]
	}
}
