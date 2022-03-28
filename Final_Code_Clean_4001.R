##Initial set up of workspace
setwd("~/Working Directory/SOA-Challenge-main/Data")
require(data.table)
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(rmarkdown)
require(knitr)
require(cowplot)

##Loading in data
#Just loads in the datasets with their names
temp = list.files(pattern="*.csv")
for( i in 1:length(temp)){
  assign(temp[i], fread(temp[i]), envir = .GlobalEnv )
}

results_2021 <- tournament_results.csv[, c(3,4)]
results_2020 <- tournament_results.csv[, c(1,2)]

#Filtering to 2021 -- for most recent league stats
data2021_def <- l_defense_imputed.csv[ Year == '2021',1:32]
data2021_gk <- l_goal_imputed.csv[ Year == '2021',1:27]
data2021_pass <- l_pass_imputed.csv[ Year == '2021',1:31]
data2021_shoot <- l_shoot_imputed.csv[ Year == '2021',1:26]

#Preparing to normalise the data
normalise <- function(dataset,type){
  classes <- lapply(dataset, class)
  for(i in 1:ncol(dataset)){
    if(classes[i] == "numeric"){
      col <- dataset[ , ..i]
      col_norm <- scale(col)
      dataset[, i] <- col_norm
    }
  }
  assign(paste0("norm",type), dataset, envir = .GlobalEnv)
  
}

norm_def <- normalise(data2021_def,"def")
norm_shoot <- normalise(data2021_shoot,"shoot")
norm_pass <- normalise(data2021_pass,"pass")
norm_gk <- normalise(data2021_gk,"gk")

#Compiling normalised master dataset for the three main stats - Defense, Shooting, Passing
norm_master <- merge(norm_def, norm_shoot[, c(1:2,4,8:26)], by=c("Player","Nation","Squad","League","Year"), all.data=TRUE)
norm_master <- merge(norm_master, norm_pass[, c(1:2,4,8:31)], by=c("Player","Nation","Squad","League","Year"), all.data=TRUE)
#norm_master <- merge(norm_master, norm_gk[, c(2,6:24)], by="Player", all.data=TRUE)


##Making positional splits
norm_def_df <- norm_def[grep("DF", norm_def$Pos),c(1:4,8:32)]
norm_def_gk <- norm_def[grep("GK", norm_def$Pos),c(1:4,8:32)]
norm_def_mf <- norm_def[grep("MF", norm_def$Pos),c(1:4,8:32)]
norm_def_fw <- norm_def[grep("FW", norm_def$Pos),c(1:4,8:32)]

norm_gk_df <- norm_gk[grep("DF", norm_gk$Pos),c(1:4,7:27)]
norm_gk_gk <- norm_gk[grep("GK", norm_gk$Pos),c(1:4,7:27)]
norm_gk_mf <- norm_gk[grep("MF", norm_gk$Pos),c(1:4,7:27)]
norm_gk_fw <- norm_gk[grep("FW", norm_gk$Pos),c(1:4,7:27)]

norm_pass_df <- norm_pass[grep("DF", norm_pass$Pos),c(1:4,8:31)]
norm_pass_gk <- norm_pass[grep("GK", norm_pass$Pos),c(1:4,8:31)]
norm_pass_mf <- norm_pass[grep("MF", norm_pass$Pos),c(1:4,8:31)]
norm_pass_fw <- norm_pass[grep("FW", norm_pass$Pos),c(1:4,8:31)]

norm_shoot_df <- norm_shoot[grep("DF", norm_shoot$Pos),c(1:4,8:26)]
norm_shoot_gk <- norm_shoot[grep("GK", norm_shoot$Pos),c(1:4,8:26)]
norm_shoot_mf <- norm_shoot[grep("MF", norm_shoot$Pos),c(1:4,8:26)]
norm_shoot_fw <- norm_shoot[grep("FW", norm_shoot$Pos),c(1:4,8:26)]

#Averaging the data by country
avg_stat <- function(dataset,type){
  classes <- lapply(dataset, class)
  cols <- classes=="numeric"
  vec <-names(dataset[,..cols])
  stat_avg <- dataset[, lapply(.SD, mean), by=Nation, .SDcols=vec] 
  assign(paste0("stat_avg_",type),stat_avg, envir=.GlobalEnv)
}

#Averaging data by country and position
avg_stat(norm_def_df,"def_df")
avg_stat(norm_def_mf,"def_mf")
avg_stat(norm_def_fw,"def_fw")
avg_stat(norm_def_gk,"def_gk")

avg_stat(norm_pass_df,"pass_df")
avg_stat(norm_pass_mf,"pass_mf")
avg_stat(norm_pass_fw,"pass_fw")
avg_stat(norm_pass_gk,"pass_gk")

avg_stat(norm_shoot_df,"shoot_df")
avg_stat(norm_shoot_mf,"shoot_mf")
avg_stat(norm_shoot_fw,"shoot_fw")
avg_stat(norm_shoot_gk,"shoot_gk")

avg_stat(norm_gk_df,"gk_df")
avg_stat(norm_gk_mf,"gk_mf")
avg_stat(norm_gk_fw,"gk_fw")
avg_stat(norm_gk_gk,"gk_gk")

##Adding on skill scores using weighted sum approach
scoring <- function(dataset,factors,type){
  colcount <- ncol(dataset)-6
  score_col <- ncol(dataset) -2
  for(i in 1:colcount){
    j <- i + 4
    dataset[,j] <- dataset[,j,with=FALSE]*factors[i]
    
  }
  dataset$score <- rowSums(dataset[,5:score_col])
  assign(paste0("scored",type), dataset, envir = .GlobalEnv)
  
}

##Determing coefficients for each variable used in modelling
#Defense - Important for defenders and midfielders
fac_def_df <- c(0,5,4,3,1,0#dribblers tackled
                ,4,5,0,0,0,5#Pressures%
                ,4,3,1,0,4,5#interceptions
                ,5,3,0,5,-5#Errors
)
fac_def_gk <- c(0,4,3,0,0,0#num dribblers tackled
                ,3,4,0,0,0,4#Pressures%??????????
                ,3,1,0,0,5,5#Blocks ShSv
                ,3,3,0,5,-5#Errors
)
fac_def_mf <- c(0,5,4,4,4,0#dribblers tackled
                ,3,5,0,3,0,5 #Pressures%
                ,3,4,4,0,2,2#interceptions
                ,4,5,0,3,-5#Errors
)
fac_def_fw <- c(0,5,1,3,4,0#dribblers tackled
                ,3,5,0,3,0,5#Pressures%
                ,1,3,4,1,0,1#interceptions
                ,4,5,0,2,-5#Errors
)
#Passing - Important for all positions, particularly midfield
fac_pass_df <- c(0,0,0,2,2,0
                 ,3,5,0,3,5,0
                 ,3,5,3,1,0,2
                 ,2,1,2,2)
fac_pass_gk <- c(0,0,0,1,3,0
                 ,3,5,0,3,5,0
                 ,3,5,3,1,0,1
                 ,1,0,0,3)
fac_pass_mf <- c(0,0,0,2,2,0
                 ,3,5,0,3,5,0
                 ,3,5,4,3,0,3
                 ,4,3,4,1)
fac_pass_fw <- c(0,0,0,3,1,0
                 ,3,5,0,3,5,0
                 ,3,5,5,4,0,4
                 ,1,4,4,1)

#Shooting - Important for forwards and midfielders
fac_shoot_df <- c(0,0,3,5,0,0
                  ,5,3,4,4,5,4
                  ,0,0,3,0,0)
fac_shoot_gk <- c(0,0,1,5,0,0
                  ,4,3,2,4,5,4
                  ,3,0,1,0,0)
fac_shoot_mf <- c(0,0,3,5,0,0
                  ,5,3,3,2,4,2
                  ,2,0,3,0,0)
fac_shoot_fw <- c(0,0,3,5,0,0
                  ,5,3,4,3,4,2
                  ,2,0,4,0,0)

#Goalkeeping - Important for goalkeepers
fac_gk_gk <- c(2,2,3,0,0,-5
               ,3,0,5,2,1,0
               ,0,3,0,-3,4,1
               ,0)

#Running the scoring function for each skill and position
scoring(norm_def_df,fac_def_df,'def_df')
scoring(norm_def_gk,fac_def_gk,'def_gk')
scoring(norm_def_fw,fac_def_fw,'def_fw')
scoring(norm_def_mf,fac_def_mf,'def_mf')

scoring(norm_shoot_df,fac_shoot_df,'shoot_df')
scoring(norm_shoot_gk,fac_shoot_gk,'shoot_gk')
scoring(norm_shoot_fw,fac_shoot_fw,'shoot_fw')
scoring(norm_shoot_mf,fac_shoot_mf,'shoot_mf')

scoring(norm_pass_df,fac_pass_df,'pass_df')
scoring(norm_pass_gk,fac_pass_gk,'pass_gk')
scoring(norm_pass_fw,fac_pass_fw,'pass_fw')
scoring(norm_pass_mf,fac_pass_mf,'pass_mf')

scoring(norm_gk_gk,fac_gk_gk,'gk_gk')

#Combining the skill scores for each position
add_score <- function(position,position1){
  def_data <- get(paste0("scoreddef_",position))
  pass_data <- get(paste0("scoredpass_",position))
  shoot_data <- get(paste0("scoredshoot_",position))
  scored_data <- norm_master[ grep(position1,norm_master$Pos),]
  scored_data[def_data, on = .(Player,Nation,Squad,League,Year), score_def := i.score]
  scored_data[pass_data, on = .(Player,Nation,Squad,League,Year), score_pass := i.score]
  scored_data[shoot_data, on = .(Player,Nation,Squad,League,Year), score_shoot := i.score]
  
  assign(paste0("scored_",position),scored_data, envir=.GlobalEnv)
}

add_score("df","DF")
add_score("fw","FW")
add_score("mf","MF")

#Goalkeeping is a slightly special case since it considers four skill areas instead of three
add_score("gk","GK")
scored_gk[scoredgk_gk, on = .(Player,Nation,Squad,League,Year), score_gk := i.score]

#Normalising the scores for each position
scale_score <- function(dataset,type){
  dataset[, "score_def"] <- scale(dataset[, "score_def"])
  dataset[, "score_pass"] <- scale(dataset[, "score_pass"])
  dataset[, "score_shoot"] <- scale(dataset[, "score_shoot"])
  assign(paste0("scale_score_",type), dataset, envir = .GlobalEnv)
  
}

scale_score(scored_df,"df")
scale_score(scored_mf,"mf")
scale_score(scored_fw,"fw")

scale_score(scored_gk,"gk")
scale_score_gk[, "score_gk"] <- scale(scale_score_gk[, "score_gk"])
names(scored_gk)
scoredgk_gk[Player ==  "W. Nasiru",]
#Determining weightings of each skill area for each position
fac_df <- c(0.59,0.49,0.02)
fac_mf <- c(0.2,0.6,0.2)
fac_fw <- c(0.05,0.25,0.7)
fac_gk <- c(0.04,0.1,0.01,0.85)

#Applying the weighted sum approach of each skill area to determine a total score of skill
final_data_df <- copy(scale_score_df)
final_data_mf <- copy(scale_score_mf)
final_data_fw <- copy(scale_score_fw)
final_data_gk <- copy(scale_score_gk)

final_data_df$skill_score = final_data_df$score_def*fac_df[1] + final_data_df$score_pass*fac_df[2] + final_data_df$score_shoot * fac_df[3]
final_data_mf$skill_score = final_data_mf$score_def*fac_mf[1] + final_data_mf$score_pass*fac_mf[2] + final_data_mf$score_shoot * fac_mf[3]
final_data_fw$skill_score = final_data_fw$score_def*fac_fw[1] + final_data_fw$score_pass*fac_fw[2] + final_data_fw$score_shoot * fac_fw[3]
final_data_gk$skill_score = final_data_gk$score_def*fac_gk[1] + final_data_gk$score_pass*fac_gk[2] + final_data_gk$score_shoot * fac_gk[3] + final_data_gk$score_gk * fac_gk[4]


#Producing output datasets of scored players
fwrite(final_data_df, file="league2021_skill_score_df_fixed_nocoll.csv")
fwrite(final_data_mf, file="league2021_skill_score_mf_fixed_nocoll.csv")
fwrite(final_data_fw, file="league2021_skill_score_fw_fixed_nocoll.csv")
fwrite(final_data_gk, file="league2021_skill_score_gk_fixed_nocoll.csv")


###Misc. Analysis - Both exploratory and checking
##Chosen Team Analysis
graphcheck <- function(dataset,type){
  data1 <- ggplot(dataset, aes(y=skill_score,x="")) + geom_jitter(position=position_jitter(0.1)) + 
    scale_color_brewer(palette="RdYlGn") + theme_classic() + xlab("Total") + ylab("Score")
  data2 <- ggplot(dataset, aes(y=score_def,x="")) + geom_jitter(position=position_jitter(0.1)) + 
    scale_color_brewer(palette="RdYlGn") + theme_classic() + xlab("Defense") + ylab("Score")
  data3 <- ggplot(dataset, aes(y=score_pass,x="")) + geom_jitter(position=position_jitter(0.1)) + 
    scale_color_brewer(palette="RdYlGn") + theme_classic() + xlab("Passing") + ylab("Score")
  data4 <- ggplot(dataset, aes(y=score_shoot,x="")) + geom_jitter(position=position_jitter(0.1)) + 
    scale_color_brewer(palette="RdYlGn") + theme_classic() + xlab("Shooting") + ylab("Score")
  assign(paste0(type,"1"), data1, envir = .GlobalEnv)
  assign(paste0(type,"2"), data2, envir = .GlobalEnv)
  assign(paste0(type,"3"), data3, envir = .GlobalEnv)
  assign(paste0(type,"4"), data4, envir = .GlobalEnv)
  
}

chosen_df <- final_data_df[ Player %in% c("Q. bin Ismail", "O. Tshuma", "T. Okoro", "X. Takagi", "M. Ludwig", "T. Nouri", "C. Kawooya", "M. Ayebazibwe"),]
chosen_mf <- final_data_mf[ Player %in% c("O. Wanjala", "X. Leroy", "J. Nurhayati", "H. Tourgeman", "F. Chin", "K. Chisi", "M. Nankwanga", "C. Arineitwe"),]
chosen_fw <- final_data_fw[ Player %in% c("K. Kazlo?", "I. Saha", "H. Mubaiwa", "L. Tarigan", "W. Ofori", "V. Golob", "C. Kadosh"),]
chosen_gk <- final_data_gk[ Player %in% c("W. Nasiru", "F. Akumu"),]
graphcheck(chosen_df,"skill_df_")
graphcheck(chosen_mf,"skill_mf_")
graphcheck(chosen_fw,"skill_fw_")
graphcheck(chosen_gk,"skill_gk_")
skill_gk_5 <- ggplot(chosen_gk, aes(y=score_gk,x="")) + geom_jitter(position=position_jitter(0.1)) + 
  scale_color_brewer(palette="RdYlGn") + theme_classic() + xlab("Goalkeeping") + ylab("Score")


plot_grid( skill_df_1, skill_df_2, skill_df_3, skill_df_4,
           labels=c("A","B","C","D"))
plot_grid( skill_mf_1, skill_mf_2, skill_mf_3, skill_mf_4,
           labels=c("A","B","C","D"))
plot_grid( skill_fw_1, skill_fw_2, skill_fw_3, skill_fw_4,
           labels=c("A","B","C","D"))
plot_grid( skill_gk_1, skill_gk_2, skill_gk_3, skill_gk_4, skill_gk_5,
           labels=c("A","B","C","D", "E"))

##Tournament Analysis
data_def <- t_defense_imputed.csv[ ,1:31]
data_gk <- t_goal_imputed.csv[ ,1:26]
data_pass <- t_pass_imputed.csv[ ,1:30]
data_shoot <- t_shoot_imputed.csv[ ,1:25]

attach_results <- function(dataset,type){
  data2021 <- dataset[ Year == '2021', ]
  merged_dataset_2021 <- merge(data2021, results_2021, by.x = "Nation", by.y = "Country", all.x = TRUE)
  merged_dataset_2021a <- merged_dataset_2021[, Results := 25-`2021 Tournament Place`]
  merged_dataset_2021a$group = cut(merged_dataset_2021a$Results,c(0,6,12,18,24))
  levels(merged_dataset_2021a$group) = c("Very Bad","Bad","Good","Very Good")
  merged_dataset_2021a <- merged_dataset_2021a[, `2021 Tournament Place` := NULL]
  
  data2020 <- dataset[ Year == '2020', ]
  merged_dataset_2020 <- merge(data2020, results_2020, by.x = "Nation", by.y = "Country", all.x = TRUE)
  merged_dataset_2020a <- merged_dataset_2020[, Results := 17-`2020 Tournament Place`]
  merged_dataset_2020a$group = cut(merged_dataset_2020a$Results,c(0,4,8,12,16))
  levels(merged_dataset_2020a$group) = c("Very Bad","Bad","Good","Very Good")
  merged_dataset_2020a <- merged_dataset_2020a[, `2020 Tournament Place`:= NULL]
  
  final <- rbind(merged_dataset_2020a, merged_dataset_2021a)
  assign(paste0("data_result",type), final, envir = .GlobalEnv)
}

attach_results(data_pass,"_pass")
attach_results(data_shoot,"_shoot")
attach_results(data_def,"_def")
attach_results(data_gk,"_gk")

##Creating 2021 dataset
data2021_def <- data_result_def[ Year == '2021',]
data2021_gk <- data_result_gk[ Year == '2021',]
data2021_pass <- data_result_pass[ Year == '2021',]
data2021_shoot <- data_result_shoot[ Year == '2021',]


##Creating 90 minute datasets
data2021_pass_90min <- copy(data2021_pass)
cols <- names(data2021_pass_90min[, c(7:8,10:13,15:16,18:19,21,24:28)])
data2021_pass_90min[, (cols) := lapply(.SD, function(d) d/get('X90s')), .SDcols = cols]
data2021_pass_90min <- data2021_pass_90min[ X90s > 0.5, ]
head(data2021_pass_90min$Total.Cmp.)
head(data2021_pass$Total.Cmp.)


data2021_shoot_90min <- copy(data2021_shoot)
cols <- names(data2021_shoot_90min[, c(7:17,20:25)])
data2021_shoot_90min[, (cols) := lapply(.SD, function(d) d/get('X90s')), .SDcols = cols]
data2021_shoot_90min <- data2021_shoot_90min[ X90s > 0.5, ]

data2021_def_90min <- copy(data2021_def)
cols <- names(data2021_def_90min[, 7:29])
data2021_def_90min[, (cols) := lapply(.SD, function(d) d/get('X90s')), .SDcols = cols]
data2021_def_90min <- data2021_def_90min[ X90s > 0.5, ]


data2021_gk_90min <- copy(data2021_gk)
cols <- names(data2021_gk_90min[, 7:28])
data2021_gk_90min[, (cols) := lapply(.SD, function(d) d/get('X90s')), .SDcols = cols]

normalise(data2021_def,"_def")
normalise(data2021_shoot,"_shoot")
normalise(data2021_pass,"_pass")
normalise(data2021_gk,"_gk")

norm_master <- merge(norm_def, norm_shoot[, c(2,6:23)], by="Player", all.data=TRUE)
norm_master <- merge(norm_master, norm_pass[, c(2,6:28)], by="Player", all.data=TRUE)
#norm_master <- merge(norm_master, norm_gk[, c(2,6:24)], by="Player", all.data=TRUE)

#90 minute version
norm_master_90min <- merge(norm_def_90min, norm_shoot_90min[, c(2,6:23)], by="Player", all.data=TRUE)
norm_master_90min <- merge(norm_master_90min, norm_pass_90min[, c(2,6:28)], by="Player", all.data=TRUE)
#norm_master <- merge(norm_master, norm_gk[, c(2,6:24)], by="Player", all.data=TRUE)
names(norm_def)
##Making positional splits
norm_def_df <- norm_def[grep("DF", norm_def$Pos),c(1:3,7:29,33)]
norm_def_gk <- norm_def[grep("GK", norm_def$Pos),c(1:3,7:29,33)]
norm_def_mf <- norm_def[grep("MF", norm_def$Pos),c(1:3,7:29,33)]
norm_def_fw <- norm_def[grep("FW", norm_def$Pos),c(1:3,7:29,33)]

norm_gk_df <- norm_gk[grep("DF", norm_gk$Pos),c(1:3,6:24,28)]
norm_gk_gk <- norm_gk[grep("GK", norm_gk$Pos),c(1:3,6:24,28)]
norm_gk_mf <- norm_gk[grep("MF", norm_gk$Pos),c(1:3,6:24,28)]
norm_gk_fw <- norm_gk[grep("FW", norm_gk$Pos),c(1:3,6:24,28)]

norm_pass_df <- norm_pass[grep("DF", norm_pass$Pos),c(1:3,7:28,32)]
norm_pass_gk <- norm_pass[grep("GK", norm_pass$Pos),c(1:3,7:28,32)]
norm_pass_mf <- norm_pass[grep("MF", norm_pass$Pos),c(1:3,7:28,32)]
norm_pass_fw <- norm_pass[grep("FW", norm_pass$Pos),c(1:3,7:28,32)]

norm_shoot_df <- norm_shoot[grep("DF", norm_shoot$Pos),c(1:3,7:17,20:25,27)]
norm_shoot_gk <- norm_shoot[grep("GK", norm_shoot$Pos),c(1:3,7:17,20:25,27)]
norm_shoot_mf <- norm_shoot[grep("MF", norm_shoot$Pos),c(1:3,7:17,20:25,27)]
norm_shoot_fw <- norm_shoot[grep("FW", norm_shoot$Pos),c(1:3,7:17,20:25,27)]


scoring(norm_def_df,fac_def_df,'def_df')
scoring(norm_def_gk,fac_def_gk,'def_gk')
scoring(norm_def_fw,fac_def_fw,'def_fw')
scoring(norm_def_mf,fac_def_mf,'def_mf')

scoring(norm_shoot_df,fac_shoot_df,'shoot_df')
scoring(norm_shoot_gk,fac_shoot_gk,'shoot_gk')
scoring(norm_shoot_fw,fac_shoot_fw,'shoot_fw')
scoring(norm_shoot_mf,fac_shoot_mf,'shoot_mf')

scoring(norm_pass_df,fac_pass_df,'pass_df')
scoring(norm_pass_gk,fac_pass_gk,'pass_gk')
scoring(norm_pass_fw,fac_pass_fw,'pass_fw')
scoring(norm_pass_mf,fac_pass_mf,'pass_mf')

scoring(norm_gk_gk,fac_gk_gk,'gk_gk')

add_score <- function(position,position1){
  def_data <- get(paste0("scoreddef_",position))
  pass_data <- get(paste0("scoredpass_",position))
  shoot_data <- get(paste0("scoredshoot_",position))
  scored_data <- norm_master[ grep(position1,norm_master$Pos),]
  scored_data[def_data, on = .(Player,Nation), score_def := i.score]
  scored_data[pass_data, on = .(Player,Nation), score_pass := i.score]
  scored_data[shoot_data, on = .(Player,Nation), score_shoot := i.score]
  
  assign(paste0("scored_",position),scored_data, envir=.GlobalEnv)
}


add_score("df","DF")
add_score("fw","FW")
add_score("mf","MF")

#Goalkeeping is a funky one
add_score("gk","GK")
scored_gk[scoredgk_gk, on = .(Player,Nation), score_gk := i.score]


scale_score(scored_df,"df")
scale_score(scored_mf,"mf")
scale_score(scored_fw,"fw")

#Doing gk separately cause funky
scale_score(scored_gk,"gk")
scale_score_gk[, "score_gk"] <- scale(scale_score_gk[, "score_gk"])


final_data_df <- copy(scale_score_df)
final_data_mf <- copy(scale_score_mf)
final_data_fw <- copy(scale_score_fw)
final_data_gk <- copy(scale_score_gk)

final_data_df$skill_score = final_data_df$score_def*fac_df[1] + final_data_df$score_pass*fac_df[2] + final_data_df$score_shoot * fac_df[3]
final_data_mf$skill_score = final_data_mf$score_def*fac_mf[1] + final_data_mf$score_pass*fac_mf[2] + final_data_mf$score_shoot * fac_mf[3]
final_data_fw$skill_score = final_data_fw$score_def*fac_fw[1] + final_data_fw$score_pass*fac_fw[2] + final_data_fw$score_shoot * fac_fw[3]
final_data_gk$skill_score = final_data_gk$score_def*fac_gk[1] + final_data_gk$score_pass*fac_gk[2] + final_data_gk$score_shoot * fac_gk[3] + final_data_gk$score_gk * fac_gk[4]

eda_graph <- function(loop_dataset, name){
  for(i in 1:ncol(loop_dataset)){
    loop_class <- lapply(loop_dataset, class)
    if(loop_class[i] == "numeric"){
      col <- colnames(loop_dataset)[i]
      graph <- ggplot(loop_dataset, aes(y=get(col),x=" ", color=group)) + geom_jitter(position=position_jitter(0.1)) + scale_color_brewer(palette="RdYlGn") + theme_classic() + ylab(col) + xlab("")
      assign(paste0(name,i), graph, envir = .GlobalEnv)
    }
  }
}

avg_stat <- function(dataset,type){
  classes <- lapply(dataset, class)
  cols <- classes=="numeric"
  vec <-names(dataset[,..cols])
  stat_avg <- dataset[, lapply(.SD, mean), by=Nation, .SDcols=vec] 
  stat_avg[dataset, on = .(Nation), group := i.group]
  assign(paste0("stat_avg_",type),stat_avg, envir=.GlobalEnv)
}

avg_stat(norm_def_df,"def_df")
avg_stat(norm_def_mf,"def_mf")
avg_stat(norm_def_fw,"def_fw")
avg_stat(norm_def_gk,"def_gk")

avg_stat(norm_pass_df,"pass_df")
avg_stat(norm_pass_mf,"pass_mf")
avg_stat(norm_pass_fw,"pass_fw")
avg_stat(norm_pass_gk,"pass_gk")

avg_stat(norm_shoot_df,"shoot_df")
avg_stat(norm_shoot_mf,"shoot_mf")
avg_stat(norm_shoot_fw,"shoot_fw")
avg_stat(norm_shoot_gk,"shoot_gk")

avg_stat(norm_gk_df,"gk_df")
avg_stat(norm_gk_mf,"gk_mf")
avg_stat(norm_gk_fw,"gk_fw")
avg_stat(norm_gk_gk,"gk_gk")

eda_graph(stat_avg_def_df, "ddf_")
eda_graph(stat_avg_def_mf, "dmf_")
eda_graph(stat_avg_def_fw, "dfw_")
eda_graph(stat_avg_def_gk, "dgk_")
eda_graph(stat_avg_pass_df, "pdf_")
eda_graph(stat_avg_pass_mf, "pmf_")
eda_graph(stat_avg_pass_fw, "pfw_")
eda_graph(stat_avg_pass_gk, "pgk_")
eda_graph(stat_avg_shoot_df, "sdf_")
eda_graph(stat_avg_shoot_mf, "smf_")
eda_graph(stat_avg_shoot_fw, "sfw_")
eda_graph(stat_avg_shoot_gk, "sgk_")
eda_graph(stat_avg_gk_gk, "ggk_")

norm_def_df[, Pos:=NULL]
norm_def_mf[, Pos:=NULL]
norm_def_fw[, Pos:=NULL]
norm_def_gk[, Pos:=NULL]
norm_pass_df[, Pos:=NULL]
norm_pass_mf[, Pos:=NULL]
norm_pass_fw[, Pos:=NULL]
norm_pass_gk[, Pos:=NULL]
norm_shoot_df[, Pos:=NULL]
norm_shoot_mf[, Pos:=NULL]
norm_shoot_fw[, Pos:=NULL]
norm_shoot_gk[, Pos:=NULL]
norm_gk_gk[, Pos:=NULL]

eda_graph(norm_def_df, "ddf_whole_")
eda_graph(norm_def_mf, "dmf_whole_")
eda_graph(norm_def_fw, "dfw_whole_")
eda_graph(norm_def_gk, "dgk_whole_")
eda_graph(norm_pass_df, "pdf_whole_")
eda_graph(norm_pass_mf, "pmf_whole_")
eda_graph(norm_pass_fw, "pfw_whole_")
eda_graph(norm_pass_gk, "pgk_whole_")
eda_graph(norm_shoot_df, "sdf_whole_")
eda_graph(norm_shoot_mf, "smf_whole_")
eda_graph(norm_shoot_fw, "sfw_whole_")
eda_graph(norm_shoot_gk, "sgk_whole_")
eda_graph(norm_gk_gk, "ggk_whole_")

graphcheck <- function(dataset,type){
  data1 <- ggplot(dataset, aes(y=skill_score,x="", color=group)) + geom_jitter(position=position_jitter(0.1)) + 
    scale_color_brewer(palette="RdYlGn") + theme_classic()
  data2 <- ggplot(dataset, aes(y=score_def,x="arg", color=group)) + geom_jitter(position=position_jitter(0.1)) + 
    scale_color_brewer(palette="RdYlGn") + theme_classic()
  data3 <- ggplot(dataset, aes(y=score_pass,x="arg", color=group)) + geom_jitter(position=position_jitter(0.1)) + 
    scale_color_brewer(palette="RdYlGn") + theme_classic()
  data4 <- ggplot(dataset, aes(y=score_shoot,x="arg", color=group)) + geom_jitter(position=position_jitter(0.1)) + 
    scale_color_brewer(palette="RdYlGn") + theme_classic()
  assign(paste0(type,"1"), data1, envir = .GlobalEnv)
  assign(paste0(type,"2"), data2, envir = .GlobalEnv)
  assign(paste0(type,"3"), data3, envir = .GlobalEnv)
  assign(paste0(type,"4"), data4, envir = .GlobalEnv)
  
}

graphcheck(final_data_df,"skill_df_")
graphcheck(final_data_mf,"skill_mf_")
graphcheck(final_data_fw,"skill_fw_")

#render("Graph_Stuff.Rmd")
