options(digits=3)
#prep data
temp=list_iButton_corr_tidy_Aegidii[[1]][,3]
temp_two=list_iButton_corr_tidy_Aegidii[[2]][,3]
temp_three=list_iButton_corr_tidy_Aegidii[[3]][,3]
temp_four=list_iButton_corr_tidy_Aegidii[[4]][,3]
temp_five=list_iButton_corr_tidy_Aegidii[[5]][,3]
date=list_iButton_corr_tidy_Aegidii[[1]][,2]
factor=list_iButton_factor_Aegidii[[1]][,6]
str(wind)
#use wind
temp_wind_all=data.frame(wind$MESS_DATUM, 
                     wind$wind_speed,
                     temp,
                     temp_two,
                     temp_three,
                     temp_four,
                     temp_five,
                     date,
                     factor)
temp_wind_all$temp_two
#compare Ehrenpark (87) and Haus Kump (64)
Ehrenpark=list_iButton_corr_tidy_date_factor[["87"]]
Haus_Kump=list_iButton_corr_tidy_date_factor[["64"]]
Stiftherrenstrasse=list_iButton_corr_tidy_date_factor[["72"]]

mean(Ehrenpark$Temperature_C_w_off[Ehrenpark$Time_factor=="day"],na.rm=T)
mean(Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="day"],na.rm=T)

mean(Ehrenpark$Temperature_C_w_off[Ehrenpark$Time_factor=="night"],na.rm=T)
mean(Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="night"],na.rm=T)

#test significance
#normality:
shapiro.test(Ehrenpark$Temperature_C_w_off[Ehrenpark$Time_factor=="day"])
wilcox.test(Ehrenpark$Temperature_C_w_off[Ehrenpark$Time_factor=="day"], 
            Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="day"])

shapiro.test(Ehrenpark$Temperature_C_w_off[Ehrenpark$Time_factor=="night"])
wilcox.test(Ehrenpark$Temperature_C_w_off[Ehrenpark$Time_factor=="night"],
            Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="night"])

#compare Haus Kump and first Aegidiilogger
mean(Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="day"],na.rm=T)
mean(temp_wind_all$temp[temp_wind_all$factor=="day"], na.rm=T)

mean(Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="night"],na.rm=T)
mean(temp_wind_all$temp[temp_wind_all$factor=="night"], na.rm=T)

shapiro.test(Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="day"])
wilcox.test(temp_wind_all$temp[temp_wind_all$factor=="day"],
            Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="day"])

shapiro.test(temp_wind_all$temp[temp_wind_all$factor=="night"])
wilcox.test(temp_wind_all$temp[temp_wind_all$factor=="night"],
            Haus_Kump$Temperature_C_w_off[Haus_Kump$Time_factor=="night"])

#compare STiftherrenstraße and second to fifth aegiidi logger
#second
mean(Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="day"],na.rm=T)
mean(temp_wind_all$temp_two[temp_wind_all$factor=="day"], na.rm=T)

mean(Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="night"],na.rm=T)
mean(temp_wind_all$temp_two[temp_wind_all$factor=="night"], na.rm=T)

shapiro.test(Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="day"])
wilcox.test(temp_wind_all$temp_two[temp_wind_all$factor=="day"],
            Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="day"])

shapiro.test(temp_wind_all$temp_two[temp_wind_all$factor=="night"])
wilcox.test(temp_wind_all$temp_two[temp_wind_all$factor=="night"],
            Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="night"])

#third
mean(Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="day"],na.rm=T)
mean(temp_wind_all$temp_three[temp_wind_all$factor=="day"], na.rm=T)

mean(Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="night"],na.rm=T)
mean(temp_wind_all$temp_three[temp_wind_all$factor=="night"], na.rm=T)

shapiro.test(Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="day"])
wilcox.test(temp_wind_all$temp_three[temp_wind_all$factor=="day"],
            Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="day"])

shapiro.test(temp_wind_all$temp_three[temp_wind_all$factor=="night"])
wilcox.test(temp_wind_all$temp_three[temp_wind_all$factor=="night"],
            Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="night"])

#forth
mean(Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="day"],na.rm=T)
mean(temp_wind_all$temp_four[temp_wind_all$factor=="day"], na.rm=T)

mean(Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="night"],na.rm=T)
mean(temp_wind_all$temp_four[temp_wind_all$factor=="night"], na.rm=T)

shapiro.test(Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="day"])
wilcox.test(temp_wind_all$temp_four[temp_wind_all$factor=="day"],
            Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="day"])

shapiro.test(temp_wind_all$temp_four[temp_wind_all$factor=="night"])
wilcox.test(temp_wind_all$temp_four[temp_wind_all$factor=="night"],
            Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="night"])

#fifth
mean(Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="day"],na.rm=T)
mean(temp_wind_all$temp_five[temp_wind_all$factor=="day"], na.rm=T)

mean(Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="night"],na.rm=T)
mean(temp_wind_all$temp_five[temp_wind_all$factor=="night"], na.rm=T)

shapiro.test(Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="day"])
wilcox.test(temp_wind_all$temp_five[temp_wind_all$factor=="day"],
            Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="day"])

shapiro.test(temp_wind_all$temp_five[temp_wind_all$factor=="night"])
wilcox.test(temp_wind_all$temp_five[temp_wind_all$factor=="night"],
            Stiftherrenstrasse$Temperature_C_w_off[Stiftherrenstrasse$Time_factor=="night"])

#
temp_wind_all$diff=Ehrenpark$Temperature_C_w_off-Haus_Kump$Temperature_C_w_off
temp_wind_all$diff_Ae=temp-Haus_Kump$Temperature_C_w_off
#plot difference between Ehrenpark and Haus Kump with wind
#pdf(file=paste(substr(list_iButton_corr_tidy_Aegidii[[1]][1,2],1,10),
              # "diff_Ehrenpark_Haus_K.pdf"), 
   # paper = "a4r", height=7, width=14)
#plot(Ehrenpark$Datetime.1, diff, type="l",ylim=c(-5,10),
   #  sub=">0: warmer in Ehrenpark, <0 warmer in Haus K")
#points(wind_sw$MESS_DATUM, wind_sw$wind_speed, col="red")

#abline(v=sun2$sunrise, col="green")
#abline(v=sun2$sunset, col="blue")
#dev.off()


cor.test(diff, wind_sw$wind_speed, method="spearman")

#for Ehrenpark for day/night
#correlated -0.407 p value <2e-16 
temp_wind_all$wind_sw=wind_sw$wind_speed
cor.test(temp_wind_all$diff[temp_wind_all$factor=="day"], 
         temp_wind_all$wind_sw[temp_wind_all$factor=="day"],
         method="spearman")
#pvalue -0.108 pvalue 0.009

cor.test(temp_wind_all$diff[temp_wind_all$factor=="night"], 
         temp_wind_all$wind_sw[temp_wind_all$factor=="night"],
         method="spearman")
#pvalue -0.357 pvalue 1e-06

#for first Aegidiilogger for day/night
cor.test(temp_wind_all$diff_Ae[temp_wind_all$factor=="day"], 
         temp_wind_all$wind_sw[temp_wind_all$factor=="day"],
         method="spearman")
#pvalue -0.153 pvalue 2e-04

cor.test(temp_wind_all$diff_Ae[temp_wind_all$factor=="night"], 
         temp_wind_all$wind_sw[temp_wind_all$factor=="night"],
         method="spearman")
#pvalue -0.352 pvalue 2e-06