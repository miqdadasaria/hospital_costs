# Calculating lifetime hospital costs by age, sex and deprivation quintile
# 
# Author: Miqdad Asaria
# Date: March 2017
###############################################################################

library(ggplot2)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(scales)
library(xlsx)

###############################################################################
# Extract mortality data and cost data from database 
###############################################################################

extract_data_from_db = function(cached=TRUE){
	filename = "data/cost_data.RData"
	if(cached){
		load(filename)
	} else {
		source("../db_connection.R")
		con = get_db_connection()
		
		hes_missing_data_inflation_factor = 1+(18808903-17820106)/18808903
		
		cost_sql = paste("SELECT AGE, SEX, IMD_QUINTILE, POPULATION, N*",hes_missing_data_inflation_factor," AS N, 
					TOTAL_COST*",hes_missing_data_inflation_factor," AS TOTAL_COST, 
					STDDEV_COST, 
				(TOTAL_COST/POPULATION)*",hes_missing_data_inflation_factor," AS AVERAGE_COST, 
				(N/POPULATION)*100000*",hes_missing_data_inflation_factor," AS HOSP_EPI_RATE 
				FROM 
				((SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION p
				INNER JOIN
				IMD_2010 imd
				ON p.LSOA01CD=imd.LSOA01CD
				WHERE AGE<85 AND YEAR=2011
				GROUP BY AGE, SEX, QUINTILE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				COUNT(*) n, 
				age, 
				sex, 
				imd_quintile 
				FROM hes_episode_cost_2011
				WHERE AGE<85
				GROUP BY age, sex, imd_quintile) c
				ON p.age=c.age AND p.sex=c.sex AND p.imd_quintile=c.imd_quintile)
				
				UNION
				
				(SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION pop
				INNER JOIN
				IMD_2010 imd
				ON pop.LSOA01CD=imd.LSOA01CD
				WHERE AGE=85 AND YEAR=2011
				GROUP BY AGE, SEX, QUINTILE
				ORDER BY QUINTILE, SEX, AGE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				COUNT(*) n, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				85 AS age, 
				sex, 
				imd_quintile 
				FROM hes_episode_cost_2011
				WHERE age > 84
				GROUP BY sex, imd_quintile) c
				ON p.sex=c.sex AND p.imd_quintile=c.imd_quintile AND p.age=c.age))
				ORDER BY IMD_QUINTILE, SEX, AGE",sep="")
		
		cost_table = dbGetQuery(con,cost_sql)
		
		cost_sql_imd = paste("SELECT AGE, IMD_QUINTILE, POPULATION, N*",hes_missing_data_inflation_factor," AS N, 
TOTAL_COST*",hes_missing_data_inflation_factor," AS TOTAL_COST, 
STDDEV_COST, 
				(TOTAL_COST/POPULATION)*",hes_missing_data_inflation_factor," AS AVERAGE_COST, 
				(N/POPULATION)*100000*",hes_missing_data_inflation_factor," AS HOSP_EPI_RATE 
				FROM 
				((SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION p
				INNER JOIN
				IMD_2010 imd
				ON p.LSOA01CD=imd.LSOA01CD
				WHERE AGE<85 AND YEAR=2011
				GROUP BY AGE, QUINTILE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				COUNT(*) n, 
				age, 
				imd_quintile 
				FROM hes_episode_cost_2011
				WHERE AGE<85
				GROUP BY age, imd_quintile) c
				ON p.age=c.age AND p.imd_quintile=c.imd_quintile)
				
				UNION
				
				(SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION pop
				INNER JOIN
				IMD_2010 imd
				ON pop.LSOA01CD=imd.LSOA01CD
				WHERE AGE=85 AND YEAR=2011
				GROUP BY AGE, QUINTILE
				ORDER BY QUINTILE, AGE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				COUNT(*) n, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				85 AS age, 
				imd_quintile 
				FROM hes_episode_cost_2011
				WHERE age > 84
				GROUP BY imd_quintile) c
				ON p.imd_quintile=c.imd_quintile AND p.age=c.age))
				ORDER BY IMD_QUINTILE, AGE", sep="")
		
		cost_table_imd = dbGetQuery(con,cost_sql_imd)
		
		cost_sql_sex = paste("SELECT AGE, SEX, POPULATION, N*",hes_missing_data_inflation_factor," AS N, 
TOTAL_COST*",hes_missing_data_inflation_factor," AS TOTAL_COST, 
STDDEV_COST, 
				(TOTAL_COST/POPULATION)*",hes_missing_data_inflation_factor," AS AVERAGE_COST, 
				(N/POPULATION)*100000*",hes_missing_data_inflation_factor," AS HOSP_EPI_RATE 
				FROM 
				((SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX 
				FROM
				ONS_POPULATION p
				WHERE AGE<85 AND YEAR=2011
				GROUP BY AGE, SEX) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				COUNT(*) n, 
				age, 
				sex 
		 		FROM hes_episode_cost_2011
				WHERE AGE<85
				GROUP BY age, sex) c
				ON p.age=c.age AND p.sex=c.sex)
				
				UNION
				
				(SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX 
				FROM
				ONS_POPULATION pop
				WHERE AGE=85 AND YEAR=2011
				GROUP BY AGE, SEX
				ORDER BY SEX, AGE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				COUNT(*) n, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				85 AS age, 
				sex 
				FROM hes_episode_cost_2011
				WHERE age > 84
				GROUP BY sex) c
				ON p.sex=c.sex AND p.age=c.age))
				ORDER BY  SEX, AGE", sep="")
		
		cost_table_sex = dbGetQuery(con,cost_sql_sex)
		
		cost_sql_overall = paste("SELECT AGE, POPULATION, N*",hes_missing_data_inflation_factor," AS N, 
TOTAL_COST*",hes_missing_data_inflation_factor," AS TOTAL_COST, 
STDDEV_COST, 
				(TOTAL_COST/POPULATION)*",hes_missing_data_inflation_factor," AS AVERAGE_COST, 
				(N/POPULATION)*100000*",hes_missing_data_inflation_factor," AS HOSP_EPI_RATE 
				FROM 
				((SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE 
				FROM
				ONS_POPULATION p
				WHERE AGE<85 AND YEAR=2011
				GROUP BY AGE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				COUNT(*) n, 
				age 
				FROM hes_episode_cost_2011
				WHERE AGE<85
				GROUP BY age) c
				ON p.age=c.age)
				
				UNION
				
				(SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE 
				FROM
				ONS_POPULATION pop
				WHERE AGE=85 AND YEAR=2011
				GROUP BY AGE
				ORDER BY AGE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				COUNT(*) n, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				85 AS age
				FROM hes_episode_cost_2011
				WHERE age > 84) c
				ON p.age=c.age))
				ORDER BY AGE", sep="")
		
		cost_table_overall = dbGetQuery(con,cost_sql_overall)
		
		emergency_cost_sql = paste("SELECT AGE, SEX, IMD_QUINTILE, POPULATION, N*",hes_missing_data_inflation_factor," AS N, 
TOTAL_COST*",hes_missing_data_inflation_factor," AS TOTAL_COST, 
STDDEV_COST, 
				(TOTAL_COST/POPULATION)*",hes_missing_data_inflation_factor," AS AVERAGE_COST, 
				(N/POPULATION)*100000*",hes_missing_data_inflation_factor," AS HOSP_EPI_RATE 
				FROM 
				((SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION p
				INNER JOIN
				IMD_2010 imd
				ON p.LSOA01CD=imd.LSOA01CD
				WHERE AGE<85 AND YEAR=2011
				GROUP BY AGE, SEX, QUINTILE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				COUNT(*) n, 
				age, 
				sex, 
				imd_quintile 
				FROM hes_episode_cost_2011
				WHERE AGE<85 AND EMERGENCY=1
				GROUP BY age, sex, imd_quintile) c
				ON p.age=c.age AND p.sex=c.sex AND p.imd_quintile=c.imd_quintile)
				
				UNION
				
				(SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION pop
				INNER JOIN
				IMD_2010 imd
				ON pop.LSOA01CD=imd.LSOA01CD
				WHERE AGE=85 AND YEAR=2011
				GROUP BY AGE, SEX, QUINTILE
				ORDER BY QUINTILE, SEX, AGE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				COUNT(*) n, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				85 AS age, 
				sex, 
				imd_quintile 
				FROM hes_episode_cost_2011
				WHERE age > 84 AND EMERGENCY=1
				GROUP BY sex, imd_quintile) c
				ON p.sex=c.sex AND p.imd_quintile=c.imd_quintile AND p.age=c.age))
				ORDER BY IMD_QUINTILE, SEX, AGE", sep="")
		
		emergency_cost_table = dbGetQuery(con,emergency_cost_sql)
		
		elective_cost_sql = paste("SELECT AGE, SEX, IMD_QUINTILE, POPULATION, N*",hes_missing_data_inflation_factor," AS N, 
TOTAL_COST*",hes_missing_data_inflation_factor," AS TOTAL_COST, 
STDDEV_COST, 
				(TOTAL_COST/POPULATION)*",hes_missing_data_inflation_factor," AS AVERAGE_COST, 
				(N/POPULATION)*100000*",hes_missing_data_inflation_factor," AS HOSP_EPI_RATE 
				FROM 
				((SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION p
				INNER JOIN
				IMD_2010 imd
				ON p.LSOA01CD=imd.LSOA01CD
				WHERE AGE<85 AND YEAR=2011
				GROUP BY AGE, SEX, QUINTILE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				COUNT(*) n, 
				age, 
				sex, 
				imd_quintile 
				FROM hes_episode_cost_2011
				WHERE AGE<85 AND EMERGENCY=0
				GROUP BY age, sex, imd_quintile) c
				ON p.age=c.age AND p.sex=c.sex AND p.imd_quintile=c.imd_quintile)
				
				UNION
				
				(SELECT p.*, c.total_cost, c.stddev_cost, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION pop
				INNER JOIN
				IMD_2010 imd
				ON pop.LSOA01CD=imd.LSOA01CD
				WHERE AGE=85 AND YEAR=2011
				GROUP BY AGE, SEX, QUINTILE
				ORDER BY QUINTILE, SEX, AGE) p
				LEFT JOIN
				(SELECT SUM(fce_cost+unbundled_cost) total_cost, 
				COUNT(*) n, 
				STDDEV(fce_cost+unbundled_cost) stddev_cost,
				85 AS age, 
				sex, 
				imd_quintile 
				FROM hes_episode_cost_2011
				WHERE age > 84 AND EMERGENCY=0
				GROUP BY sex, imd_quintile) c
				ON p.sex=c.sex AND p.imd_quintile=c.imd_quintile AND p.age=c.age))
				ORDER BY IMD_QUINTILE, SEX, AGE", sep="")
		
		elective_cost_table = dbGetQuery(con,elective_cost_sql)
		
		outpatient_sql = "SELECT AGE, SEX, IMD_QUINTILE, POPULATION, N, (N/POPULATION)*100000 AS HOSP_APPT_RATE 
				FROM 
				((SELECT p.*, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION p
				INNER JOIN
				IMD_2010 imd
				ON p.LSOA01CD=imd.LSOA01CD
				WHERE AGE<85 AND YEAR=2011
				GROUP BY AGE, SEX, QUINTILE) p
				LEFT JOIN
				(SELECT COUNT(*) n, 
				AGE, 
				SEX, 
				IMD_QUINTILE 
				FROM hes_appointments_2011
				WHERE AGE<85
				GROUP BY AGE, SEX, IMD_QUINTILE) c
				ON p.age=c.age AND p.sex=c.sex AND p.imd_quintile=c.imd_quintile)
				
				UNION
				
				(SELECT p.*, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION p
				INNER JOIN
				IMD_2010 imd
				ON p.LSOA01CD=imd.LSOA01CD
				WHERE AGE=85 AND YEAR=2011
				GROUP BY AGE, SEX, QUINTILE) p
				LEFT JOIN
				(SELECT COUNT(*) n, 
				85 AS AGE, 
				SEX, 
				IMD_QUINTILE 
				FROM hes_appointments_2011
				WHERE AGE>84
				GROUP BY SEX, IMD_QUINTILE) c
				ON p.age=c.age AND p.sex=c.sex AND p.imd_quintile=c.imd_quintile))
				ORDER BY IMD_QUINTILE, SEX, AGE"
		
		outpatient_table = dbGetQuery(con,outpatient_sql)

		outpatient_sql_imd = "SELECT AGE, IMD_QUINTILE, POPULATION, N, (N/POPULATION)*100000 AS HOSP_APPT_RATE 
				FROM 
				((SELECT p.*, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION p
				INNER JOIN
				IMD_2010 imd
				ON p.LSOA01CD=imd.LSOA01CD
				WHERE AGE<85 AND YEAR=2011
				GROUP BY AGE, QUINTILE) p
				LEFT JOIN
				(SELECT COUNT(*) n, 
				AGE, 
				IMD_QUINTILE 
				FROM hes_appointments_2011
				WHERE AGE<85
				GROUP BY AGE, IMD_QUINTILE) c
				ON p.age=c.age AND p.imd_quintile=c.imd_quintile)
				
				UNION
				
				(SELECT p.*, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, QUINTILE AS IMD_QUINTILE 
				FROM
				ONS_POPULATION p
				INNER JOIN
				IMD_2010 imd
				ON p.LSOA01CD=imd.LSOA01CD
				WHERE AGE=85 AND YEAR=2011
				GROUP BY AGE, QUINTILE) p
				LEFT JOIN
				(SELECT COUNT(*) n, 
				85 AS AGE, 
				IMD_QUINTILE 
				FROM hes_appointments_2011
				WHERE AGE>84
				GROUP BY IMD_QUINTILE) c
				ON p.age=c.age AND p.imd_quintile=c.imd_quintile))
				ORDER BY IMD_QUINTILE, AGE"
		
		outpatient_table_imd = dbGetQuery(con,outpatient_sql_imd)		
		
		outpatient_sql_sex = "SELECT AGE, SEX, POPULATION, N, (N/POPULATION)*100000 AS HOSP_APPT_RATE 
				FROM 
				((SELECT p.*, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX 
				FROM
				ONS_POPULATION p
				WHERE AGE<85 AND YEAR=2011
				GROUP BY AGE, SEX) p
				LEFT JOIN
				(SELECT COUNT(*) n, 
				AGE, 
				SEX 
				FROM hes_appointments_2011
				WHERE AGE<85
				GROUP BY AGE, SEX) c
				ON p.age=c.age AND p.sex=c.sex)
				
				UNION
				
				(SELECT p.*, c.n
				FROM
				(SELECT SUM(population) AS population, AGE, SEX 
				FROM
				ONS_POPULATION p
				WHERE AGE=85 AND YEAR=2011
				GROUP BY AGE, SEX) p
				LEFT JOIN
				(SELECT COUNT(*) n, 
				85 AS AGE, 
				SEX 
				FROM hes_appointments_2011
				WHERE AGE>84
				GROUP BY SEX) c
				ON p.age=c.age AND p.sex=c.sex ))
				ORDER BY SEX, AGE"
		
		outpatient_table_sex = dbGetQuery(con,outpatient_sql_sex)

		outpatient_sql_overall = "SELECT AGE,POPULATION, N, (N/POPULATION)*100000 AS HOSP_APPT_RATE 
				FROM 
				((SELECT p.*, c.n
				FROM
				(SELECT SUM(population) AS population, AGE 
				FROM
				ONS_POPULATION p
				WHERE AGE<85 AND YEAR=2011
				GROUP BY AGE) p
				LEFT JOIN
				(SELECT COUNT(*) n, 
				AGE 
				FROM hes_appointments_2011
				WHERE AGE<85
				GROUP BY AGE) c
				ON p.age=c.age)
				
				UNION
				
				(SELECT p.*, c.n
				FROM
				(SELECT SUM(population) AS population, AGE 
				FROM
				ONS_POPULATION p
				WHERE AGE=85 AND YEAR=2011
				GROUP BY AGE) p
				LEFT JOIN
				(SELECT COUNT(*) n, 
				85 AS AGE 
				FROM hes_appointments_2011
				WHERE AGE>84) c
				ON p.age=c.age ))
				ORDER BY AGE"
		
		outpatient_table_overall = dbGetQuery(con,outpatient_sql_overall)
		
		dbDisconnect(con)
		
		results = list()
		results[["cost_table"]]=cost_table
		results[["cost_table_imd"]]=cost_table_imd
		results[["cost_table_sex"]]=cost_table_sex
		results[["cost_table_overall"]]=cost_table_overall
		results[["emergency_cost_table"]]=emergency_cost_table
		results[["elective_cost_table"]]=elective_cost_table
		results[["outpatient_table"]]=outpatient_table
		results[["outpatient_table_imd"]]=outpatient_table_imd
		results[["outpatient_table_sex"]]=outpatient_table_sex
		results[["outpatient_table_overall"]]=outpatient_table_overall
		
		save(results,file=filename)
	}
	return(results)	
}

# calculate the population standard error from the sub sample standard deviation
cost_standard_error = function(cost_table){
	return(sqrt(((cost_table$N/cost_table$POPULATION) * (cost_table$STDDEV_COST^2+(cost_table$TOTAL_COST/cost_table$N)^2) - (cost_table$TOTAL_COST/cost_table$POPULATION)^2))/sqrt(cost_table$POPULATION))
}

db_data = extract_data_from_db(cached=TRUE)

cost_table = db_data[["cost_table"]]
cost_table$SE_COST = cost_standard_error(cost_table)
cost_table_imd = db_data[["cost_table_imd"]]
cost_table_imd$SE_COST = cost_standard_error(cost_table_imd)
cost_table_sex = db_data[["cost_table_sex"]]
cost_table_sex$SE_COST = cost_standard_error(cost_table_sex)
cost_table_overall = db_data[["cost_table_overall"]]
cost_table_overall$SE_COST = cost_standard_error(cost_table_overall)
emergency_cost_table = db_data[["emergency_cost_table"]]
emergency_cost_table$SE_COST = cost_standard_error(emergency_cost_table)
elective_cost_table = db_data[["elective_cost_table"]]
elective_cost_table$SE_COST = cost_standard_error(elective_cost_table)
outpatient_table = db_data[["outpatient_table"]]
outpatient_table_imd = db_data[["outpatient_table_imd"]]
outpatient_table_sex = db_data[["outpatient_table_sex"]]
outpatient_table_overall = db_data[["outpatient_table_overall"]]

###############################################################################
# Functions to make generic deprivation and sex plots 
###############################################################################

imd_labels = c("Q1 (most deprived)","Q2","Q3","Q4","Q5 (least deprived)")

deprivation_plot = function(graph_data,xvar,yvar,xlab,ylab,filename,breaks=NULL,caption=NULL){
	plot = ggplot(graph_data, aes_string(x=xvar,y=yvar,group="IMD_QUINTILE",colour="IMD_QUINTILE",linetype="IMD_QUINTILE")) + 
			geom_line() + 
			facet_grid(. ~ SEX) + 
			xlab(xlab) +
			ylab(ylab) +
			scale_y_continuous(labels = comma) +
			scale_colour_manual(name="IMD Group", values=c("black","lightblue","lightgreen","lightgrey","darkgrey")) +
			scale_linetype_manual(name="IMD Group", values=c(1,2,2,2,1)) +
			theme_minimal(base_size = 14)
	if(!is.null(breaks)){
		plot = plot + scale_x_continuous(breaks=breaks)
	}
	if(!is.null(caption)){
		plot = 
				arrangeGrob(plot, 
						sub = textGrob(caption, x = 0, hjust = -0.1, vjust=0.1,
								gp = gpar(fontface = "italic", fontsize = 14)))
	}
	ggsave(filename=paste("output/",filename,".png",sep=""),plot=plot,width=27,height=10,units="cm",dpi=300)
}		

deprivation_panel = function(graph_data,xvar,yvar,xlab,ylab,filename,caption,breaks=NULL){
	plot = ggplot(graph_data, aes_string(x=xvar,y=yvar,group="IMD_QUINTILE",colour="IMD_QUINTILE",linetype="IMD_QUINTILE")) + 
			geom_line() + 
			facet_grid(FACET ~ SEX, scales="free_y") + 
			xlab(xlab) +
			ylab(ylab) +
			scale_y_continuous(labels = comma) +
			scale_colour_manual(name="IMD Group", values=c("black","lightblue","lightgreen","lightgrey","darkgrey")) +
			scale_linetype_manual(name="IMD Group", values=c(1,2,2,2,1)) +
			theme_minimal(base_size = 14)
	if(!is.null(breaks)){
		plot = plot + scale_x_continuous(breaks=breaks)
	}
	final_plot = 
			arrangeGrob(plot, 
					sub = textGrob(caption, x = 0, hjust = -0.1, vjust=0.1,
							gp = gpar(fontface = "italic", fontsize = 14)))
	
	ggsave(filename=paste("output/",filename,".png",sep=""),plot=final_plot,width=27,height=20,units="cm",dpi=300)
}		

sex_plot = function(graph_data,xvar,yvar,xlab,ylab,filename,breaks=NULL){
	plot = ggplot(graph_data, aes_string(x=xvar,y=yvar,group="SEX",colour="SEX",linetype="SEX")) + 
			geom_line() + 
			facet_wrap(~IMD_QUINTILE, nrow=2) + 
			xlab(xlab) +
			ylab(ylab) +
			scale_y_continuous(labels = comma) +
			scale_colour_manual(name="SEX", values=c("black","darkgrey")) +
			scale_linetype_manual(name="SEX", values=c(1,2)) +
			theme_minimal(base_size = 14)
	if(!is.null(breaks)){
		 plot = plot + scale_x_continuous(breaks=breaks)
	}
	
	ggsave(filename=paste("output/",filename,"2.png",sep=""),plot=plot,width=39,height=20,units="cm",dpi=300)
}		

###############################################################################
# Inpatient utilisation and cost plots 
###############################################################################

make_cost_plots = function(cost_table, prefix, caption){
	cost_graph_data = cost_table %>%
			mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male")))
	
	# make total hospital episodes deprivation and sex plots
	deprivation_plot(subset(cost_graph_data,AGE<85),"AGE","N","Age","Inpatient Episodes",paste(prefix,"hospitalisations",sep=""))
	sex_plot(subset(cost_graph_data,AGE<85),"AGE","N","Age","Inpatient Episodes",paste(prefix,"hospitalisations",sep=""))
	
	# make hospitalisation rate deprivation and sex plots
	deprivation_plot(cost_graph_data,"AGE","HOSP_EPI_RATE","Age","Inpatient Episodes per 100,000",paste(prefix,"hosp_curves",sep=""))
	sex_plot(cost_graph_data,"AGE","HOSP_EPI_RATE","Age","Inpatient Episodes per 100,000",paste(prefix,"hosp_curves",sep=""))
	
	panel_graph_data = cost_graph_data %>% 
			gather(FACET,VALUE,N,HOSP_EPI_RATE)
	panel_graph_data$FACET = factor(panel_graph_data$FACET, levels=c("N","HOSP_EPI_RATE"), labels=c("Total Hospital Episodes","Hospital Episodes per 100,000 Population"))
	deprivation_panel(subset(panel_graph_data,AGE<85),"AGE","VALUE","Age","",paste(prefix,"resource_use_panel",sep=""),caption)		
	
	# make cost deprivation and sex plots
	deprivation_plot(cost_graph_data,"AGE","AVERAGE_COST","Age","Average Annual Cost (\u00A3)",paste(prefix,"cost_curves",sep=""))
	sex_plot(cost_graph_data,"AGE","AVERAGE_COST","Age","Average Annual Cost (\u00A3)",paste(prefix,"cost_curves",sep=""))
}
make_cost_plots(cost_table,"","Figure 1. Hospital inpatient admissions by age, sex and deprivation (2011/12)")
make_cost_plots(emergency_cost_table,"emergency_","Figure A1. Emergency hospital inpatient admissions by age, sex and deprivation (2011/12)")
make_cost_plots(elective_cost_table,"elective_","Figure A2. Elective hospital inpatient admissions by age, sex and deprivation (2011/12)")

###############################################################################
# Outpatient utilisation plots 
###############################################################################

outpatient_graph_data = outpatient_table %>%
		mutate(IMD_QUINTILE=factor(IMD_QUINTILE, levels=1:5, labels=imd_labels), SEX=factor(SEX,levels=c("F","M"),labels=c("Female","Male")))

# make total outpatient appointment deprivation and sex plots
deprivation_plot(subset(outpatient_graph_data,AGE<85),"AGE","N","Age","Outpatient Appointments",paste("appointments",sep=""))
sex_plot(subset(outpatient_graph_data,AGE<85),"AGE","N","Age","Outpatient Appointments",paste("appointments",sep=""))

# make appointment rate deprivation and sex plots
deprivation_plot(outpatient_graph_data,"AGE","HOSP_APPT_RATE","Age","Outpatient Appointments per 100,000",paste("appt_curves",sep=""))
sex_plot(outpatient_graph_data,"AGE","HOSP_APPT_RATE","Age","Outpatient Appointments per 100,000",paste("appt_curves",sep=""))

###############################################################################
# Resource use and cost refence tables  
###############################################################################
summary_population = cost_table %>% 
		select(AGE,SEX,IMD_QUINTILE,POPULATION) %>%
		reshape( 
				timevar = "IMD_QUINTILE",
				idvar = c("AGE","SEX"),
				direction = "wide")

summary_imd_sex = cost_table %>% 
		select(AGE,SEX,IMD_QUINTILE,AVERAGE_COST,SE_COST) %>%
		reshape( 
				timevar = "IMD_QUINTILE",
				idvar = c("AGE","SEX"),
				direction = "wide")

summary_imd = cost_table_imd  %>% 
		select(AGE,IMD_QUINTILE,AVERAGE_COST,SE_COST) %>%
		reshape( 
				timevar = "IMD_QUINTILE",
				idvar = c("AGE"),
				direction = "wide")

summary_sex = cost_table_sex %>% 
		select(AGE,SEX,AVERAGE_COST,SE_COST) 

summary_overall = cost_table_overall %>%
		select(AGE,AVERAGE_COST,SE_COST) 

summary_outpatient = outpatient_table %>% 
		select(AGE,SEX,IMD_QUINTILE,N) %>%
		reshape( 
				timevar = "IMD_QUINTILE",
				idvar = c("AGE","SEX"),
				direction = "wide")

outpatient_imd_sex = outpatient_table %>% 
		select(AGE,SEX,IMD_QUINTILE,HOSP_APPT_RATE) %>%
		reshape( 
				timevar = "IMD_QUINTILE",
				idvar = c("AGE","SEX"),
				direction = "wide")

outpatient_imd = outpatient_table_imd  %>% 
		select(AGE,IMD_QUINTILE,HOSP_APPT_RATE) %>%
		reshape( 
				timevar = "IMD_QUINTILE",
				idvar = c("AGE"),
				direction = "wide")

outpatient_sex = outpatient_table_sex %>% 
		select(AGE,SEX,HOSP_APPT_RATE) 

outpatient_overall = outpatient_table_overall %>%
		select(AGE,HOSP_APPT_RATE) 

save_xlsx = function (file, ...){
	objects = list(...)
	fargs = as.list(match.call(expand.dots = TRUE))
	objnames = as.character(fargs)[-c(1, 2)]
	nobjects = length(objects)
	for(i in 1:nobjects) {
		if(i == 1){
			write.xlsx(objects[[i]], file, sheetName = objnames[i])
		} else {
			write.xlsx(objects[[i]], file, sheetName = objnames[i], append = TRUE)
		}
	}
}

save_xlsx("output/cost_summary_unformatted.xlsx", summary_population, summary_imd_sex, summary_imd, summary_sex, summary_overall, summary_outpatient, outpatient_imd_sex, outpatient_imd, outpatient_sex, outpatient_overall)
