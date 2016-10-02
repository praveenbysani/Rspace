library(lubridate)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggrepel)

sqlite_drv <- dbDriver("SQLite")
sql_db <- dbConnect(sqlite_drv,"data/database.sqlite")
dbListTables(sql_db)

player_df <- dbGetQuery(sql_db,"select * from Player")
pstats_df <- dbGetQuery(sql_db,"select * from Player_Stats")
match_df <- dbGetQuery(sql_db,"select * from Match")
team_df <- dbGetQuery(sql_db,"select * from Team")
country_df <- dbGetQuery(sql_db,"select * from Country")
league_df <- dbGetQuery(sql_db,"select * from League")

player_df$birthday <- ymd_hms(player_df$birthday)
player_df$age <- (now()-player_df$birthday)/365

player_recent_stats <- as.data.frame(pstats_df %>% group_by(player_api_id) %>% top_n(1,wt=date_stat))
player_recent_stats$date_stat <- ymd_hms(player_recent_stats$date_stat)

match_df_imp <- match_df[,1:11]
away_team_matches <- merge(team_df,match_df_imp,by.x="team_api_id",by.y="away_team_api_id")
home_team_matches <- merge(team_df,match_df_imp,by.x="team_api_id",by.y="home_team_api_id")
home_team_matches <- subset(home_team_matches,select=-c(id.x,id.y))
away_team_matches <- subset(away_team_matches,select=-c(id.x,id.y))

colnames(home_team_matches)[10:12] <- c("opponent_team_id","goals_scored","goals_conceded")
colnames(away_team_matches)[10:12] <- c("opponent_team_id","goals_conceded","goals_scored")
home_team_matches <- cbind(home_team_matches,side="home")
away_team_matches <- cbind(away_team_matches,side="away")
all_matches <- rbind(home_team_matches,away_team_matches)

all_matches <- all_matches %>% mutate(result=ifelse(goals_scored > goals_conceded,"win",ifelse(goals_scored < goals_conceded,"loss","draw")))


team_stats <- all_matches %>% group_by(team_long_name,league_id) %>% summarise(matches=n(),h_matches=length(result[side=="home"]),a_matches=length(result[side=="away"]),
                                                                               tot_scored=sum(goals_scored),home_scored=sum(goals_scored[side=="home"]),away_scored=sum(goals_scored[side=="away"]),
                                                                               tot_conceded = sum(goals_conceded),home_conceded=sum(goals_conceded[side=="home"]),away_conceded = sum(goals_conceded[side=="away"]),
                                                                               wins=length(result[result=="win"]),losses=length(result[result=="loss"]),draws=length(result[result=="draw"]),
                                                                               h_wins=length(result[result=="win" & side=="home"]), a_wins=length(result[result=="win" & side=="away"]),
                                                                               h_loss=length(result[result=="loss" & side=="home"]), a_loss=length(result[result=="loss" & side=="away"]),
                                                                               mean_goals=mean(goals_scored),var_goals=var(goals_scored), win_pct=wins/matches,loss_pct=losses/matches,
                                                                               hwin_pct=h_wins/h_matches, awin_pct=a_wins/a_matches)

team_stats <- inner_join(ungroup(team_stats),league_df,by=c("league_id"="id")) %>% select(-one_of("country_id","league_id")) %>% rename(league_name=name)

league_stats <- team_stats %>% group_by(league_name) %>% summarise(num_teams=n_distinct(team_long_name),num_matches=sum(matches)/2,
                                                                   num_draws=sum(draws)/2) %>% arrange(desc(num_matches))

plot_wpct_freq <- 
  ggplot(team_stats,aes(win_pct*100))+geom_area(stat="bin",bins=20,fill="light blue")+xlab("Winning percentage")
#top 15 teams by overall winning percentage
top_win_teams <- team_stats %>% arrange(desc(win_pct)) %>% head(15) 
plot_top_teams <-
  ggplot(top_win_teams,aes(x=reorder(team_long_name,-win_pct),y=win_pct,fill=league_name))+
  geom_bar(stat = 'identity')+theme(axis.text.x=element_text(angle=45))+coord_cartesian(ylim = c(0.5, 0.8))+
  xlab("Team name")+ylab("Winning Percentage")

plot_team_consistency <- 
  ggplot(top_win_teams,aes(x=mean_goals,y=var_goals))+geom_point(aes(color=league_name))+
  geom_text(aes(label=team_long_name,angle=45,vjust=1))+xlab("Average number of goals scored")+ylab("Variance in goals scored")+scale_x_continuous(breaks=seq(1,3.5,by=0.1))+scale_y_continuous(breaks=seq(1,4,by=0.1))


top_hm_away_pct <- melt(top_win_teams %>% 
                          select(one_of("team_long_name","hwin_pct","awin_pct","win_pct")),id=c("team_long_name","win_pct"))  
plot_home_away_wins <- 
  ggplot(top_hm_away_pct,aes(x=reorder(team_long_name,-win_pct),y=value,fill=variable))+
  geom_bar(stat="identity",position="dodge")+scale_fill_discrete(labels=c("home","away"))+
  theme(axis.text.x=element_text(angle = 45))+xlab("Team name")+ylab("Winning Percentage")+coord_cartesian(ylim = c(0.5,0.9))


##### compute the performance over time
team_season_stats <- all_matches %>% group_by(team_long_name,league_id,season) %>% summarise(matches=n(),h_matches=length(result[side=="home"]),a_matches=length(result[side=="away"]),
                                                                                             tot_scored=sum(goals_scored),tot_conceded = sum(goals_conceded),
                                                                                             wins=length(result[result=="win"]),losses=length(result[result=="loss"]),draws=length(result[result=="draw"]),
                                                                                             mean_goals=mean(goals_scored),var_goals=var(goals_scored), win_pct=wins/matches,loss_pct=losses/matches
                                                                                             )

#team_season_stats <- inner_join(ungroup(team_season_stats),league_df,by=c("league_id"="id")) %>% select(-one_of("country_id","league_id")) %>% rename(league_name=name)

#league_season_stats <- team_season_stats %>% group_by(league_name) %>% summarise(num_teams=n_distinct(team_long_name),num_matches=sum(matches)/2,
#                                                                                 num_draws=sum(draws)/2) %>% arrange(desc(num_matches))
#####

top_10_teams <- team_stats %>% arrange(desc(win_pct)) %>% head(10) 
top_team_season_stats <- team_season_stats %>% filter(team_long_name %in% top_10_teams$team_long_name)
plot_top_team_wpct <-ggplot(top_team_season_stats,aes(x=season,y=win_pct*100,colour=team_long_name,group=team_long_name))+geom_line(size=1.5)+
                        geom_label_repel(data=subset(top_team_season_stats,season=="2015/2016" | season == "2008/2009"),aes(label=paste(team_long_name, format(win_pct*100 ,digits=3))),segment.color = NA)+
                        scale_color_discrete(guide="none")
                          
plot_top_team_sgoal <-ggplot(top_team_season_stats,aes(x=season,y=tot_scored/matches,colour=team_long_name,group=team_long_name))+geom_line(size=1.5)+
  geom_label_repel(data=subset(top_team_season_stats,season=="2015/2016" | season == "2008/2009"),
                   aes(label=paste(team_long_name,format(tot_scored/matches,digits = 3)) ))+scale_color_discrete(guide="none")+
                   ylab("Average number of goals scored per match")

plot_top_team_cgoal <-ggplot(top_team_season_stats,aes(x=season,y=tot_conceded/matches,colour=team_long_name,group=team_long_name))+geom_line(size=1.5)+
  geom_label_repel(data=subset(top_team_season_stats,season=="2015/2016" | season == "2008/2009"),
                   aes(label=paste(team_long_name,format(tot_conceded/matches,digits = 2)) ))+scale_color_discrete(guide="none")+
                   ylab("Average number of goals conceded per match")


### analyse the big winners and losers i.e teams that win/lose by big margins###



