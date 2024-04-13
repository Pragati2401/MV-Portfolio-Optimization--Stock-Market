##########################################################################
###############################  APPENDIX  ###############################
##########################################################################
# Select 'SP500TR' and 15 tickers; three for each of the five group members.
#LIFELINE

#random12 <- c('IHG','ISUZY','IBOC','AENZ','ASR',
#              'ABM','MHO','MTLHY','MVBF','KMPR',
#              'KYOCY','KTCC','SHEN','SMCI','SMBC')

#Q1. Cumulative return chart for Project Range #1 (2016-2020) for stock tickers selected by your team
chart.CumReturns(Ra_training,legend.loc = 'topleft') #Generate Chart
acc_Ra_training<-Return.cumulative(Ra_training);acc_Ra_training #Cumulative returns for 2016-01-01 thru 2020-12-31
write.csv(acc_Ra_training,'C:/Temp/acc_Ra_training.csv') #Write to CSV

#Q2 Weights of your optimized portfolio (four digits precision) and the sum of these weights based on Project Range #1
opt_w;sum(opt_w) #Check sum opt_w equals 1
csv_opt_w <- rbind(random12,sprintf(opt_w, fmt = '%#.4f')) #Merge ticker names and formatted opt_w
write.csv(csv_opt_w,'C:/Temp/csv_opt_w.csv') #Write to CSV.

#Q3. Cumulative return chart for your optimized portfolio and SP500TR index for all available 2021 data (Project Range #2)
chart.CumReturns(Rp,legend.loc = 'bottomright')

#Q4. Annualized returns for your portfolio and SP500TR index for Project Range #2
tbl_ARPort<-table.AnnualizedReturns(Rp)
write.csv(tbl_ARPort,'C:/Temp/tbl_ARPort.csv')

##########################################################################
#################################  END  ##################################
##########################################################################