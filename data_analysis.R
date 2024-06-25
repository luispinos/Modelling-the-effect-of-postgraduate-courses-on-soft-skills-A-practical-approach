
##################### Preliminary checks on the full dataset ###################

library("dplyr")
#dataset with the courses
full_dataset<-readRDS("full_dataset_coursesRE.RDS")


### Mean Skills per stage ###
mean_skills_stage<-full_dataset %>%
  group_by(stage) %>% 
  summarize(m_problem_solving=mean(Skill3, na.rm=TRUE),sd_problem_solving=sd(Skill3, na.rm=TRUE),m_innovation=mean(Skill4, na.rm=TRUE),sd_innovation=sd(Skill4, na.rm=TRUE),
            m_organisation=mean(Skill5, na.rm=TRUE),sd_organisation=sd(Skill5, na.rm=TRUE),m_adaptability=mean(Skill6, na.rm=TRUE),sd_adaptability=sd(Skill6, na.rm=TRUE),
            m_communication=mean(Skill7, na.rm=TRUE),sd_communication=sd(Skill7, na.rm=TRUE),m_project_management=mean(Skill8, na.rm=TRUE),sd_project_management=sd(Skill8, na.rm=TRUE),
            m_conviction=mean(Skill9, na.rm=TRUE),sd_conviction=sd(Skill9, na.rm=TRUE),m_stress_management=mean(Skill10, na.rm=TRUE),sd_stress_management=sd(Skill10, na.rm=TRUE),
            m_leadership=mean(Skill11, na.rm=TRUE),sd_leadership=sd(Skill11, na.rm=TRUE),m_decision_making=mean(Skill12, na.rm=TRUE),sd_decision_making=sd(Skill12, na.rm=TRUE))

mean_skills_stage<-mean_skills_stage[,c(1,2,4,6,8,10,12,14,16,18,20)]
mean_skills_stage<-t(mean_skills_stage)
mean_skills_stage<-data.frame(mean_skills_stage)
mean_skills_stage$change1<-mean_skills_stage[,2]-mean_skills_stage[,1]
mean_skills_stage$change2<-mean_skills_stage[,3]-mean_skills_stage[,2]

write.csv(mean_skills_stage,"mean_skills_stage.CSV")

plot(x=mean_skills_stage$stage,y=mean_skills_stage$m_problem_solving,xlab="Stage",type="b",ylab="Soft Skill proficiency",xlim=c(0,3),col=rainbow(12)[1],lwd=2,ylim=c(1,4))
lines(x=mean_skills_stage$stage,y=mean_skills_stage$m_innovation,col=rainbow(12)[2],type="b",add=TRUE, yaxt="n",lwd=2)
lines(x=mean_skills_stage$stage,y=mean_skills_stage$m_organisation,col=rainbow(12)[3],type="b",add=TRUE, yaxt="n",lwd=2) 
lines(x=mean_skills_stage$stage,y=mean_skills_stage$m_adaptability,col=rainbow(12)[4],type="b",add=TRUE, yaxt="n",lwd=2) 
lines(x=mean_skills_stage$stage,y=mean_skills_stage$m_communication,col=rainbow(12)[5],type="b",add=TRUE, yaxt="n",lwd=2) 
lines(x=mean_skills_stage$stage,y=mean_skills_stage$m_project_management,col=rainbow(12)[6],type="b",add=TRUE, yaxt="n",lwd=2) 
lines(x=mean_skills_stage$stage,y=mean_skills_stage$m_conviction,col=rainbow(12)[7],type="b",add=TRUE, yaxt="n",lwd=2)
lines(x=mean_skills_stage$stage,y=mean_skills_stage$m_stress_management,col=rainbow(12)[8],type="b",add=TRUE, yaxt="n",lwd=2)
lines(x=mean_skills_stage$stage,y=mean_skills_stage$m_leadership,col=rainbow(12)[9],type="b",add=TRUE, yaxt="n",lwd=2)
lines(x=mean_skills_stage$stage,y=mean_skills_stage$m_decision_making,col=rainbow(12)[10],type="b",add=TRUE, yaxt="n",lwd=2)
legend("topright",legend=c(names(mean_skills_stage)[c(2,4,6,8,10,12,14,16,18,20)]),col=rainbow(12),cex=0.8,lty=3:1,lwd=2)


################## Multiple imputation stage 0 FISE2021 ########################

#Multiple imputation with whole data set stage 0 FISE2021
library(mice)

full_dataset<-readRDS("full_dataset_coursesRE.RDS")

#initial imputation to modify the predictor matrix and methods
m=10
imp0<-mice(full_dataset,maxit = 0,m=m,seed=500,print=FALSE)
predM<-imp0$predictorMatrix
predM[c(3:5,18:21,23:39),]=0
predM[,c(3:5,18:21,23:39)]=0
meth<-imp0$method
meth[18]=""
meth[22]="sample"
meth
#imputation with m=10 datasets
imp<-mice(full_dataset,predictorMatrix = predM,method=meth,m=m,seed=500,print=FALSE)
#rows to ignore for the imputation
rows<-rep(FALSE,times=nrow(full_dataset))
#rows to be imputed = stage 0 and evaluator id is NA
rows[full_dataset$stage==0 & is.na(full_dataset$evaluator_id)]=TRUE
data_to_keep_from_imputation<-full_dataset[!rows,]
for(i in 1:m){
  d<-mice::complete(imp,i)
  imp_d<-rbind(data_to_keep_from_imputation,d[rows,])
  imp_d<-imp_d[order(imp_d$student_id,imp_d$stage),]
  saveRDS(imp_d,paste0("imputed_dataset_",as.character(i),".RDS"))
}

################################## MODEL FIT ###################################

library(rstan)

# For execution on a local, multicore CPU with excess RAM we recommend calling
options(mc.cores = parallel::detectCores())

# To avoid recompilation of unchanged Stan programs, we recommend calling
rstan_options(auto_write = TRUE)

#each of the 5 imputed datasets from d_1 to d_5
imp_d<-readRDS("imputed_dataset_1.RDS")

#each of the 10 soft skills from skill 3 to skill 12
#Skill12
imp_d<-imp_d[!is.na(imp_d$Skill12),]

courses_ids<-unique(c(imp_d$c1,imp_d$c2,imp_d$c3,imp_d$c4,imp_d$c5,
         imp_d$c6,imp_d$c7,imp_d$c8))
summary(courses_ids)

x<-unique(c(imp_d$c1,imp_d$c2,imp_d$c3,imp_d$c4,imp_d$c5,
         imp_d$c6,imp_d$c7,imp_d$c8))

new_courses_ids<-as.numeric(as.factor(x))
summary(new_courses_ids)

course_id_change<-data.frame(unlist(courses_ids),unlist(new_courses_ids))
names(course_id_change)<-c("original_course_id","new_course_id")
saveRDS(course_id_change,"course_id_change_skill12.RDS")

weights<-data.frame(cbind(imp_d$c1,imp_d$c2,imp_d$c3,imp_d$c4,
                          imp_d$c5,imp_d$c6,imp_d$c7,imp_d$c8))
for(i in 1:8){
  weights[weights[,i]!=0,i]<-1
}

#columns 26 to 33
for(j in 26:33){
  for(i in 1:nrow(imp_d)){
    orig_course_id<-imp_d[i,j]
    #if(orig_course_id!=0){
    index<-which(course_id_change$original_course_id==orig_course_id)
    imp_d[i,j]<-course_id_change$new_course_id[index]
    #}
  }
}

student_ids<-unique(imp_d$student_id)

new_student_ids<-as.numeric(as.factor(student_ids))
summary(new_student_ids)
student_id_change<-data.frame(unlist(student_ids),unlist(new_student_ids))
names(student_id_change)<-c("original_student_id","new_student_id")
saveRDS(student_id_change,"student_id_change_skill12.RDS")

for(i in 1:nrow(imp_d)){
  orig_student_id<-imp_d$student_id[i]
  index<-which(student_id_change$original_student_id==orig_student_id)
  imp_d$student_id[i]<-student_id_change$new_student_id[index]
}

#Log Data for the model
stan_data_model<-list(N=nrow(imp_d),nthres=3,Skill=imp_d$Skill12,N_courses=nrow(course_id_change),K=1,X=matrix(imp_d$N_courses_followed),
                       N_students=length(unique(imp_d$student_id)),J_2=imp_d$student_id,
                       J_1_1=imp_d$c1,J_1_2=imp_d$c2,J_1_3=imp_d$c3,J_1_4=imp_d$c4,J_1_5=imp_d$c5,
                       J_1_6=imp_d$c6,J_1_7=imp_d$c7,J_1_8=imp_d$c8,W_1_1=weights$X1,W_1_2=weights$X2,
                       W_1_3=weights$X3,W_1_4=weights$X4,W_1_5=weights$X5,W_1_6=weights$X6,W_1_7=weights$X7,W_1_8=weights$X8)


#Fit for the model
#Running fits for each imputed dataset from skills 3 to skills 12 (total of 50 fits)
fit_stan_model<-stan(file="model.stan",data=stan_data_model,iter=5000,chains=4,warmup =1000,seed=1)

#### pooling with standard errors

library(rstan)
rows<-c("Problem Solving","Innovation","Organisation","Adaptability","Communication",
        "Project Management","Conviction","Stress Management","Leadership","Decision Making")
whole_summary<-data.frame(matrix(0,nrow = length(rows), ncol =112))
rownames(whole_summary)<-rows
for(i in 1:10){
  standard_errors<-c(0,0,0,0,0)
  b_values<-c(0,0,0,0,0)
  print(i)
  for(j in 1:5){
    skill<-i+2
    fit_stan_model<-readRDS(paste0("imp_d",j,"_skill",skill,"_5_1.RDS"))
    summary_pars<-summary(fit_stan_model,pars=c("b","Intercept","sd_courses","sd_students","r_courses"))$summary
    if(j==1){
      whole_summary[i,1:length(t(summary_pars[,1]))]<-t(summary_pars[,1])
    }else{
      whole_summary[i,1:length(t(summary_pars[,1]))]<-whole_summary[i,1:length(t(summary_pars[,1]))]+t(summary_pars[,1])
    }
    standard_errors[j]<-summary_pars[1,2]
    b_values[j]<-summary_pars[1,1]
    print(j)
  }
  whole_summary[i,1:length(t(summary_pars[,1]))]<-whole_summary[i,1:length(t(summary_pars[,1]))]/5
  whole_summary[i,(length(t(summary_pars[,1]))+1)]<-(1/5)*sum(standard_errors^2)+(1+1/5)*var(b_values)
  
}

colnames(whole_summary)<-rownames(summary_pars)
colnames(whole_summary)[112]<-"std_error_b"
saveRDS(whole_summary,"pooled_results_all.RDS")



###########arrange all pooled results


whole_summary<-readRDS("pooled_results_all.RDS")

copy_w_s<-whole_summary
copy_w_s[,7:111]<-0#zeros on the un arranged residuals
#do this from j=3 to j=12
for(j in 3:12){
  c_i_c<-readRDS(paste0("course_id_change_skill",j,".RDS"))
  for(k in 1:10){
    for(i in 2:nrow(c_i_c)){
      value_to_move<-whole_summary[k,paste0("r_courses[",c_i_c[i,"new_course_id"],"]")]
      copy_w_s[k,paste0("r_courses[",c_i_c[i,"original_course_id"],"]")]<-value_to_move
    }
  }
}
copy_w_s<-copy_w_s[,-c(109,113)]
saveRDS(copy_w_s,"arranged_pooled_results_all.RDS")
write.csv(whole_summary,"arranged_pooled_results_all.CSV")

whole_summary<-readRDS("arranged_pooled_results_all.RDS")


transpose<-t(whole_summary[,7:110])

correlation_matrix<-cor(transpose)


#### pooling thetas

library(rstan)
rows<-c("Problem Solving","Innovation","Organisation","Adaptability","Communication",
        "Project Management","Conviction","Stress Management","Leadership","Decision Making")
thetas_summary<-data.frame(matrix(0,nrow = length(rows), ncol =880))
rownames(thetas_summary)<-rows
for(i in 1:10){
  print(i)
  for(j in 1:5){
    skill<-i+2
    fit_stan_model<-readRDS(paste0("imp_d",j,"_skill",skill,"_5_1.RDS"))
    summary_pars<-summary(fit_stan_model,pars=c("r_student"))$summary
    if(j==1){
      thetas_summary[i,1:length(t(summary_pars[,1]))]<-t(summary_pars[,1])
    }else{
      thetas_summary[i,1:length(t(summary_pars[,1]))]<-thetas_summary[i,1:length(t(summary_pars[,1]))]+t(summary_pars[,1])
    }
    print(j)
  }
  thetas_summary[i,1:length(t(summary_pars[,1]))]<-thetas_summary[i,1:length(t(summary_pars[,1]))]/5
}

colnames(thetas_summary)<-rownames(summary_pars)
colnames(thetas_summary)[857:880]<-paste0("r_student[",seq(857,880,1),"]")

saveRDS(thetas_summary,"pooled_thetas_all.RDS")

##### arrange all thetas
thetas_summary<-readRDS("pooled_thetas_all.RDS")
copy_w_s<-thetas_summary
copy_w_s[,]<-0#zeros on the un arranged residuals
for(j in 3:12){
  c_i_c<-readRDS(paste0("student_id_change_skill",j,".RDS"))
  #k=1 to k=10 where k is the row
  for(k in 1:10){
    for(i in 1:nrow(c_i_c)){
      value_to_move<-thetas_summary[k,paste0("r_student[",c_i_c[i,"new_student_id"],"]")]
      copy_w_s[k,paste0("r_student[",c_i_c[i,"original_student_id"],"]")]<-value_to_move
    }
  }
}
library(dplyr)
check<-copy_w_s %>% select_if(colSums(.) != 0)
saveRDS(check,"arranged_thetas_results_all.RDS")
write.csv(check,"arranged_thetas_results_all.CSV")
arranged_thetas_summary<-readRDS("arranged_thetas_results_all.RDS")

##################################################################




############## FIGURES FOR THE PAPER ############################

########### FIGURE 2

library("dplyr")
#dataset with the courses
full_dataset<-readRDS("full_dataset_coursesRE.RDS")

#Organisation skills per stage
jpeg(file="org_skills_stage.jpg", width=8.5, height=8.5,units="cm", res=300)
lattice::histogram(~ as.factor(Skill5)| factor(stage,levels=c(0,1,2),labels=c("Stage 0","Stage 1","Stage 2")), data = full_dataset, type="percent",layout=c(3,1),xlab="Proficiency scale",ylab="Percentage")
dev.off()

########### FIGURE 3

whole_summary<-readRDS("arranged_pooled_results_all.RDS")

#library(lattice)
values_communication<-t(whole_summary[5,7:110])+whole_summary[5,1]
values_leadership<-t(whole_summary[9,7:110])+whole_summary[9,1]

jpeg(file="courses_effects_example.jpg", width=18, height=8.5,units="cm", res=300)
par(mfrow=c(1,2))

plot(density(values_communication),xlab="Course effects in log-odds scale",main="Communication",ylab="Density",col="red")
abline(v =mean(values_communication),col = "red")

plot(density(values_leadership),xlab="Course effects in log-odds scale",main="Leadership",ylab="Density",col="blue")
abline(v =mean(values_leadership),col = "blue")
dev.off()

########### FIGURE 4

library(reshape2)
library(ggplot2)
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

whole_summary<-readRDS("arranged_pooled_results_all.RDS")

rownames(whole_summary)[3]<-"Organization"


transpose<-t(whole_summary[,7:110])

correlation_matrix<-cor(transpose)

cormat<-correlation_matrix
cormat <- reorder_cormat(correlation_matrix)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
jpeg(file="heatmap_correlations.jpg", width=18, height=15,units="cm", res=300)
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
ggheatmap <-ggheatmap + 
  geom_text(aes(Var2, Var1, label = round(value,digits=2)), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
print(ggheatmap)
dev.off()



############## END OF FIGURES FOR THE PAPER ############################













