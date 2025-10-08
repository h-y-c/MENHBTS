
library(dplyr)
library(plot3D)
library(scatterplot3d)
library(lubridate)
library(mapplots)
library(sp)
library(rgdal)
library(raster)
library(RColorBrewer)
library(maptools)
library(png)
library(jpeg)
library(shapefiles)
library(gridExtra)
library(reshape)
library(latticeExtra)
library(viridis)
library(maps)
library(automap)
library(mapproj)
library(grid)
library(colorRamps)
library(mapdata)
library(ggplot2)
library(StereoMorph)
library(MeanRarity)
library(iNEXT)
library(ggpubr)
library(readxl)



spe_names_pp_use<-read.csv("species_names.csv",header=TRUE,sep=",")%>%
  dplyr::filter(COMMON_NAME%in%unique(tolower(fil_amount_dat_spe_df$Common_Name)))%>%
  mutate(Common_Name=stringr::str_to_title(COMMON_NAME))%>%
  arrange(comm_name)
spe_bw_cg_n_b_regime_en_names_p_df<-spe_bw_cg_n_b_regime_en_df%>%
  full_join(spe_names_pp_use)%>%
  mutate(p_val=ifelse(line_type=="sig","p<0.05",
                      ifelse(line_type=="nonsig","p>0.05",NA)))
cg_text_year_loc<-1987
cg_arrow_year_loc<-1987
NE_text_lab_loc<-270;SW_text_lab_loc<-80
offshore_text_lab_loc<-15;inshore_text_lab_loc<--15
shallow_text_lab_loc<--50;deep_text_lab_loc<--130
alongshore_arrow_NE<-250;alongshore_arrow_SW<-100
crossshore_arrow_offshore<-13;crossshore_arrow_inshore<--13
depth_arrow_shallow<--60;depth_arrow_deep<--125
cg_text_df1<-rbind(
  expand.grid(variable="Depth (m)",
              comm_name=spe_names_pp_use$comm_name[1],
              Season=c("Fall","Spring"),
              Year=cg_text_year_loc,
              CG=deep_text_lab_loc,
              label="Deep"),
  expand.grid(variable="Depth (m)",
              comm_name=spe_names_pp_use$comm_name[1],
              Season=c("Fall","Spring"),
              Year=cg_text_year_loc,
              CG=shallow_text_lab_loc,
              label="Shallow"),
  expand.grid(variable="Crossshore (km)",
              comm_name=spe_names_pp_use$comm_name[1],
              Season=c("Fall","Spring"),
              Year=cg_text_year_loc,
              CG=inshore_text_lab_loc,
              label="Inshore"),
  expand.grid(variable="Crossshore (km)",
              comm_name=spe_names_pp_use$comm_name[1],
              Season=c("Fall","Spring"),
              Year=cg_text_year_loc,
              CG=offshore_text_lab_loc,
              label="Offshore"),
  expand.grid(variable="Alongshore (km)",
              comm_name=spe_names_pp_use$comm_name[1],
              Season=c("Fall","Spring"),
              Year=cg_text_year_loc,
              CG=SW_text_lab_loc,
              label="SW"),
  expand.grid(variable="Alongshore (km)",
              comm_name=spe_names_pp_use$comm_name[1],
              Season=c("Fall","Spring"),
              Year=cg_text_year_loc,
              CG=NE_text_lab_loc,
              label="NE")
)
ggplot(dplyr::filter(spe_bw_cg_n_b_regime_en_names_p_df,comm_name%in%spe_names_pp_use$comm_name[1:11]), 
       aes(x=Year,y=CG,group=Season)) +
  geom_point(aes(color=Season),size=0.5)+ 
  geom_line(
    aes(x=Year,y=value,color=Season,linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=0.7)+
  geom_ribbon(
    aes(ymax=up_value,ymin=lo_value,fill=Season,color=NULL),alpha=0.3) +
  facet_grid(variable~comm_name,scales="free_y") +
  theme_bw()+
  theme(plot.margin=unit(c(0,0.5,0,2),"lines"), 
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=12))+   
  scale_color_manual(values=c("blue","orange"))+
  scale_fill_manual(values=c("blue","orange"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    linetype="p-value"
  )+
  ylab("")  + 
  geom_segment(
    x=cg_arrow_year_loc,xend=cg_arrow_year_loc,y=depth_arrow_deep,yend=depth_arrow_shallow,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Depth (m)",
                       comm_name=spe_names_pp_use$comm_name[1],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=cg_arrow_year_loc,xend=cg_arrow_year_loc,y=crossshore_arrow_inshore,yend=crossshore_arrow_offshore,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Crossshore (km)",
                       comm_name=spe_names_pp_use$comm_name[1],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=cg_arrow_year_loc,xend=cg_arrow_year_loc,y=alongshore_arrow_SW,yend=alongshore_arrow_NE,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Alongshore (km)",
                       comm_name=spe_names_pp_use$comm_name[1],
                       Season=c("Fall","Spring"))
  ) +
  geom_text(
    data=cg_text_df1,
    aes(label=label),size=4
  )+  
  coord_cartesian(clip="off",xlim=c(2002,2024),expand=FALSE)
cg_text_df2<-rbind(
  expand.grid(variable="Depth (m)",
              comm_name=spe_names_pp_use$comm_name[12],
              Season=c("Fall","Spring"),
              Year=cg_text_year_loc,
              CG=deep_text_lab_loc,
              label="Deep"),
  expand.grid(variable="Depth (m)",
              comm_name=spe_names_pp_use$comm_name[12],
              Season=c("Fall","Spring"),
              Year=cg_text_year_loc,
              CG=shallow_text_lab_loc,
              label="Shallow"),
  expand.grid(variable="Crossshore (km)",
              comm_name=spe_names_pp_use$comm_name[12],
              Season=c("Fall","Spring"),
              Year=cg_text_year_loc,
              CG=inshore_text_lab_loc,
              label="Inshore"),
  expand.grid(variable="Crossshore (km)",
              comm_name=spe_names_pp_use$comm_name[12],
              Season=c("Fall","Spring"),
              Year=cg_text_year_loc,
              CG=offshore_text_lab_loc,
              label="Offshore"),
  expand.grid(variable="Alongshore (km)",
              comm_name=spe_names_pp_use$comm_name[12],
              Season=c("Fall","Spring"),
              Year=cg_text_year_loc,
              CG=SW_text_lab_loc,
              label="SW"),
  expand.grid(variable="Alongshore (km)",
              comm_name=spe_names_pp_use$comm_name[12],
              Season=c("Fall","Spring"),
              Year=cg_text_year_loc,
              CG=NE_text_lab_loc,
              label="NE")
)
ggplot(dplyr::filter(spe_bw_cg_n_b_regime_en_names_p_df,comm_name%in%spe_names_pp_use$comm_name[12:22]), 
       aes(x=Year,y=CG,group=Season)) +
  geom_point(aes(color=Season),size=0.5)+ 
  geom_line(
    aes(x=Year,y=value,color=Season,linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=0.7)+
  geom_ribbon(
    aes(ymax=up_value, ymin=lo_value,fill=Season,color=NULL),alpha=0.3) +
  facet_grid(variable~comm_name,scales="free_y") +
  theme_bw()+
  theme(plot.margin=unit(c(0,0.5,0,2),"lines"), 
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text( 
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=11),
        strip.text.y=element_text(size=12))+   
  scale_color_manual(values=c("blue","orange"))+
  scale_fill_manual(values=c("blue","orange"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    linetype="p-value"
  )+
  ylab("")  + 
  geom_segment(
    x=cg_arrow_year_loc,xend=cg_arrow_year_loc,y=depth_arrow_deep,yend=depth_arrow_shallow,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Depth (m)",
                       comm_name=spe_names_pp_use$comm_name[12],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=cg_arrow_year_loc,xend=cg_arrow_year_loc,y=crossshore_arrow_inshore,yend=crossshore_arrow_offshore,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Crossshore (km)",
                       comm_name=spe_names_pp_use$comm_name[12],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=cg_arrow_year_loc,xend=cg_arrow_year_loc,y=alongshore_arrow_SW,yend=alongshore_arrow_NE,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Alongshore (km)",
                       comm_name=spe_names_pp_use$comm_name[12],
                       Season=c("Fall","Spring"))
  ) +
  geom_text(
    data=cg_text_df2,
    aes(label=label),size=4
  )+  
  coord_cartesian(clip="off",xlim=c(2002,2024),expand=FALSE)
fa_cg_spe<-c("Aca redfish","Alewife","Am plaice","Atl herring","Butterfish","Lh sculpin","Monkfish",
             "Red hake","White hake","Silver hake",
             "Am lobster","Jonah crab","Dichelo shr","Montagui shr",
             "Longfin sq","Shortfin sq",
             "Wdp fl","Winter fl","Witch fl"
)
fa_spe_bw_cg_n_b_regime_en_names_p_df<-spe_bw_cg_n_b_regime_en_names_p_df%>%
  dplyr::filter(Season=="Fall")%>%
  mutate(comm_name=factor(comm_name,levels=fa_cg_spe))
fa_cg_text_year_loc<-1987
fa_cg_arrow_year_loc<-1987
fa_NE_text_lab_loc<-270;fa_SW_text_lab_loc<-80
fa_offshore_text_lab_loc<-15;fa_inshore_text_lab_loc<--15
fa_shallow_text_lab_loc<--50;fa_deep_text_lab_loc<--130
fa_alongshore_arrow_NE<-250;fa_alongshore_arrow_SW<-100
fa_crossshore_arrow_offshore<-13;fa_crossshore_arrow_inshore<--13
fa_depth_arrow_shallow<--60;fa_depth_arrow_deep<--125
fa_cg_text_df1<-rbind(
  expand.grid(variable="Depth (m)",
              comm_name=fa_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=fa_cg_text_year_loc,
              CG=fa_deep_text_lab_loc,
              label="Deep"),
  expand.grid(variable="Depth (m)",
              comm_name=fa_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=fa_cg_text_year_loc,
              CG=fa_shallow_text_lab_loc,
              label="Shallow"),
  expand.grid(variable="Crossshore (km)",
              comm_name=fa_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=fa_cg_text_year_loc,
              CG=fa_inshore_text_lab_loc,
              label="Inshore"),
  expand.grid(variable="Crossshore (km)",
              comm_name=fa_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=fa_cg_text_year_loc,
              CG=fa_offshore_text_lab_loc,
              label="Offshore"),
  expand.grid(variable="Alongshore (km)",
              comm_name=fa_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=fa_cg_text_year_loc,
              CG=fa_SW_text_lab_loc,
              label="SW"),
  expand.grid(variable="Alongshore (km)",
              comm_name=fa_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=fa_cg_text_year_loc,
              CG=fa_NE_text_lab_loc,
              label="NE")
)
ggplot(dplyr::filter(fa_spe_bw_cg_n_b_regime_en_names_p_df,comm_name%in%fa_cg_spe[1:10]), 
       aes(x=Year,y=CG)) +
  geom_point(size=0.5)+ 
  geom_line(
    aes(x=Year,y=value,
        color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1)+
  geom_ribbon(
    aes(ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3) +
  facet_grid(variable~comm_name,scales="free_y") +
  theme_bw()+
  theme(plot.margin=unit(c(0,0.5,0,2),"lines"),
        plot.title=element_text(size=20,vjust=-1,face="plain"),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(, 
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=12))+   
  scale_color_manual(values=c("red","black"))+
  scale_fill_manual(values=c("red","black"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value"
  )+
  ylab("")+ 
  ggtitle("Fall")+
  geom_segment(
    x=fa_cg_arrow_year_loc,xend=fa_cg_arrow_year_loc,y=fa_depth_arrow_deep,yend=fa_depth_arrow_shallow,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Depth (m)",
                       comm_name=fa_cg_spe[1],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=fa_cg_arrow_year_loc,xend=fa_cg_arrow_year_loc,y=fa_crossshore_arrow_inshore,yend=fa_crossshore_arrow_offshore,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Crossshore (km)",
                       comm_name=fa_cg_spe[1],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=fa_cg_arrow_year_loc,xend=fa_cg_arrow_year_loc,y=fa_alongshore_arrow_SW,yend=fa_alongshore_arrow_NE,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Alongshore (km)",
                       comm_name=fa_cg_spe[1],
                       Season=c("Fall","Spring"))
  ) +
  geom_text(
    data=fa_cg_text_df1,
    aes(label=label),size=4
  )+  
  coord_cartesian(clip="off",xlim=c(2002,2024),expand=FALSE)
fa_cg_text_df2<-rbind(
  expand.grid(variable="Depth (m)",
              comm_name=fa_cg_spe[11],
              Season=c("Fall","Spring"),
              Year=fa_cg_text_year_loc,
              CG=fa_deep_text_lab_loc,
              label="Deep"),
  expand.grid(variable="Depth (m)",
              comm_name=fa_cg_spe[11],
              Season=c("Fall","Spring"),
              Year=fa_cg_text_year_loc,
              CG=fa_shallow_text_lab_loc,
              label="Shallow"),
  expand.grid(variable="Crossshore (km)",
              comm_name=fa_cg_spe[11],
              Season=c("Fall","Spring"),
              Year=fa_cg_text_year_loc,
              CG=fa_inshore_text_lab_loc,
              label="Inshore"),
  expand.grid(variable="Crossshore (km)",
              comm_name=fa_cg_spe[11],
              Season=c("Fall","Spring"),
              Year=fa_cg_text_year_loc,
              CG=fa_offshore_text_lab_loc,
              label="Offshore"),
  expand.grid(variable="Alongshore (km)",
              comm_name=fa_cg_spe[11],
              Season=c("Fall","Spring"),
              Year=fa_cg_text_year_loc,
              CG=fa_SW_text_lab_loc,
              label="SW"),
  expand.grid(variable="Alongshore (km)",
              comm_name=fa_cg_spe[11],
              Season=c("Fall","Spring"),
              Year=fa_cg_text_year_loc,
              CG=fa_NE_text_lab_loc,
              label="NE")
)

ggplot(dplyr::filter(fa_spe_bw_cg_n_b_regime_en_names_p_df,comm_name%in%fa_cg_spe[11:19]), 
       aes(x=Year,y=CG)) +
  geom_point(size=0.5)+ 
  geom_line(
    aes(x=Year,y=value,
        color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1)+
  geom_ribbon(
    aes(ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3) +
  facet_grid(variable~comm_name,scales="free_y") +
  theme_bw()+
  theme(plot.margin=unit(c(0,0.5,0,2),"lines"),
        plot.title=element_text(size=20,vjust=-1,face="plain"),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text( 
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=12))+   
  scale_color_manual(values=c("red","black"))+
  scale_fill_manual(values=c("red","black"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value"
  )+
  ylab("")+ 
  ggtitle("Fall")+
  geom_segment(
    x=fa_cg_arrow_year_loc,xend=fa_cg_arrow_year_loc,y=fa_depth_arrow_deep,yend=fa_depth_arrow_shallow,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Depth (m)",
                       comm_name=fa_cg_spe[11],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=fa_cg_arrow_year_loc,xend=fa_cg_arrow_year_loc,y=fa_crossshore_arrow_inshore,yend=fa_crossshore_arrow_offshore,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Crossshore (km)",
                       comm_name=fa_cg_spe[11],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=fa_cg_arrow_year_loc,xend=fa_cg_arrow_year_loc,y=fa_alongshore_arrow_SW,yend=fa_alongshore_arrow_NE,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Alongshore (km)",
                       comm_name=fa_cg_spe[11],
                       Season=c("Fall","Spring"))
  ) +
  geom_text(
    data=fa_cg_text_df2,
    aes(label=label),size=4
  )+  
  coord_cartesian(clip="off",xlim=c(2002,2024),expand=FALSE)
sp_cg_spe<-c("Alewife","Am plaice","Am shad",
             "Atl herring","Blu herring","Lh sculpin",
             "Wdp fl","Winter fl",
             "Am lobster","Jonah crab","Dichelo shr","Montagui shr","N shr",
             "Red hake","White hake","Silver hake"
)
sp_spe_bw_cg_n_b_regime_en_names_p_df<-spe_bw_cg_n_b_regime_en_names_p_df%>%
  dplyr::filter(Season=="Spring")%>%
  mutate(comm_name=factor(comm_name,levels=sp_cg_spe))
sp_cg_text_year_loc<-1990
sp_cg_arrow_year_loc<-1990
sp_NE_text_lab_loc<-270;sp_SW_text_lab_loc<-80
sp_offshore_text_lab_loc<-15;sp_inshore_text_lab_loc<--15
sp_shallow_text_lab_loc<--50;sp_deep_text_lab_loc<--130
sp_alongshore_arrow_NE<-250;sp_alongshore_arrow_SW<-100
sp_crossshore_arrow_offshore<-13;sp_crossshore_arrow_inshore<--13
sp_depth_arrow_shallow<--60;sp_depth_arrow_deep<--125
sp_cg_text_df1<-rbind(
  expand.grid(variable="Depth (m)",
              comm_name=sp_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=sp_cg_text_year_loc,
              CG=sp_deep_text_lab_loc,
              label="Deep"),
  expand.grid(variable="Depth (m)",
              comm_name=sp_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=sp_cg_text_year_loc,
              CG=sp_shallow_text_lab_loc,
              label="Shallow"),
  expand.grid(variable="Crossshore (km)",
              comm_name=sp_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=sp_cg_text_year_loc,
              CG=sp_inshore_text_lab_loc,
              label="Inshore"),
  expand.grid(variable="Crossshore (km)",
              comm_name=sp_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=sp_cg_text_year_loc,
              CG=sp_offshore_text_lab_loc,
              label="Offshore"),
  expand.grid(variable="Alongshore (km)",
              comm_name=sp_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=sp_cg_text_year_loc,
              CG=sp_SW_text_lab_loc,
              label="SW"),
  expand.grid(variable="Alongshore (km)",
              comm_name=sp_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=sp_cg_text_year_loc,
              CG=sp_NE_text_lab_loc,
              label="NE")
)

ggplot(dplyr::filter(sp_spe_bw_cg_n_b_regime_en_names_p_df,comm_name%in%sp_cg_spe[1:8]), 
       aes(x=Year,y=CG)) +
  geom_point(size=0.5)+ 
  geom_line(
    aes(x=Year,y=value,
        color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1)+
  geom_ribbon(
    aes(ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3) +
  facet_grid(variable~comm_name,scales="free_y") +
  theme_bw()+
  theme(plot.margin=unit(c(0,0.5,0,2),"lines"), 
        plot.title=element_text(size=20,vjust=-1,face="plain"),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text( 
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=12))+   
  scale_color_manual(values=c("red","black"))+
  scale_fill_manual(values=c("red","black"))+
  scale_linetype_manual(values=c("solid", "twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value"
  )+
  ylab("")+ 
  ggtitle("Spring")+
  geom_segment(
    x=sp_cg_arrow_year_loc,xend=sp_cg_arrow_year_loc,y=sp_depth_arrow_deep,yend=sp_depth_arrow_shallow,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Depth (m)",
                       comm_name=sp_cg_spe[1],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=sp_cg_arrow_year_loc,xend=sp_cg_arrow_year_loc,y=sp_crossshore_arrow_inshore,yend=sp_crossshore_arrow_offshore,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Crossshore (km)",
                       comm_name=sp_cg_spe[1],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=sp_cg_arrow_year_loc,xend=sp_cg_arrow_year_loc,y=sp_alongshore_arrow_SW,yend=sp_alongshore_arrow_NE,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Alongshore (km)",
                       comm_name=sp_cg_spe[1],
                       Season=c("Fall","Spring"))
  ) +
  geom_text(
    data=sp_cg_text_df1,
    aes(label=label),size=4
  )+  
  coord_cartesian(clip="off",xlim=c(2002,2024),expand=FALSE)
sp_cg_text_df2<-rbind(
  expand.grid(variable="Depth (m)",
              comm_name=sp_cg_spe[9],
              Season=c("Fall","Spring"),
              Year=sp_cg_text_year_loc,
              CG=sp_deep_text_lab_loc,
              label="Deep"),
  expand.grid(variable="Depth (m)",
              comm_name=sp_cg_spe[9],
              Season=c("Fall","Spring"),
              Year=sp_cg_text_year_loc,
              CG=sp_shallow_text_lab_loc,
              label="Shallow"),
  expand.grid(variable="Crossshore (km)",
              comm_name=sp_cg_spe[9],
              Season=c("Fall","Spring"),
              Year=sp_cg_text_year_loc,
              CG=sp_inshore_text_lab_loc,
              label="Inshore"),
  expand.grid(variable="Crossshore (km)",
              comm_name=sp_cg_spe[9],
              Season=c("Fall","Spring"),
              Year=sp_cg_text_year_loc,
              CG=sp_offshore_text_lab_loc,
              label="Offshore"),
  expand.grid(variable="Alongshore (km)",
              comm_name=sp_cg_spe[9],
              Season=c("Fall","Spring"),
              Year=sp_cg_text_year_loc,
              CG=sp_SW_text_lab_loc,
              label="SW"),
  expand.grid(variable="Alongshore (km)",
              comm_name=sp_cg_spe[9],
              Season=c("Fall","Spring"),
              Year=sp_cg_text_year_loc,
              CG=sp_NE_text_lab_loc,
              label="NE")
)

ggplot(dplyr::filter(sp_spe_bw_cg_n_b_regime_en_names_p_df,comm_name%in%sp_cg_spe[9:16]), 
       aes(x=Year,y=CG)) +
  geom_point(size=0.5)+ 
  geom_line(
    aes(x=Year,y=value,
        color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1)+
  geom_ribbon(
    aes(ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3) +
  facet_grid(variable~comm_name,scales="free_y") +
  theme_bw()+
  theme(plot.margin=unit(c(0,0.5,0,2),"lines"),
        plot.title=element_text(size=20,vjust=-1,face="plain"),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=12))+   
  scale_color_manual(values=c("red","black"))+
  scale_fill_manual(values=c("red","black"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value"
  )+
  ylab("")+ 
  ggtitle("Spring")+
  geom_segment(
    x=sp_cg_arrow_year_loc,xend=sp_cg_arrow_year_loc,y=sp_depth_arrow_deep,yend=sp_depth_arrow_shallow,
    arrow = arrow(type="open", ends="both",length = unit(10, "pt")),
    data = expand.grid(variable="Depth (m)",
                       comm_name=sp_cg_spe[9],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=sp_cg_arrow_year_loc,xend=sp_cg_arrow_year_loc,y=sp_crossshore_arrow_inshore,yend=sp_crossshore_arrow_offshore,
    arrow = arrow(type="open", ends="both",length = unit(10, "pt")),
    data = expand.grid(variable="Crossshore (km)",
                       comm_name=sp_cg_spe[9],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=sp_cg_arrow_year_loc,xend=sp_cg_arrow_year_loc,y=sp_alongshore_arrow_SW,yend=sp_alongshore_arrow_NE,
    arrow = arrow(type="open", ends="both",length = unit(10, "pt")),
    data = expand.grid(variable="Alongshore (km)",
                       comm_name=sp_cg_spe[9],
                       Season=c("Fall","Spring"))
  ) +
  geom_text(
    data=sp_cg_text_df2,
    aes(label=label),size=4
  )+  
  coord_cartesian(clip="off",xlim=c(2002,2024),expand=FALSE)
ggplot(dplyr::filter(fa_spe_bw_cg_n_b_regime_en_names_p_df,comm_name%in%fa_cg_spe[1:10]), 
       aes(x=Year,y=CG)) +
  geom_point(size=0.5)+ 
  geom_line(
    aes(x=Year,y=value,linetype=factor(p_val,levels=c("p<0.05","p>0.05")),
        color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1)+
  geom_ribbon(
    aes(ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3) +
  facet_grid(variable~comm_name,scales="free_y") +
  theme_bw()+
  theme(plot.margin=unit(c(0,0.5,0,2),"lines"),
        plot.title=element_text(size=20,vjust=-1,face="plain"),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=12))+   
  scale_color_manual(values=c("red","black"))+
  scale_fill_manual(values=c("red","black"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value",
    linetype="p-value"
  )+
  ylab("")+ 
  ggtitle("Fall")+
  geom_segment(
    x=fa_cg_arrow_year_loc,xend=fa_cg_arrow_year_loc,y=fa_depth_arrow_deep,yend=fa_depth_arrow_shallow,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Depth (m)",
                       comm_name=fa_cg_spe[1],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=fa_cg_arrow_year_loc,xend=fa_cg_arrow_year_loc,y=fa_crossshore_arrow_inshore,yend=fa_crossshore_arrow_offshore,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Crossshore (km)",
                       comm_name=fa_cg_spe[1],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=fa_cg_arrow_year_loc,xend=fa_cg_arrow_year_loc,y=fa_alongshore_arrow_SW,yend=fa_alongshore_arrow_NE,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Alongshore (km)",
                       comm_name=fa_cg_spe[1],
                       Season=c("Fall","Spring"))
  ) +
  geom_text(
    data=fa_cg_text_df1,
    aes(label=label),size=4
  )+  
  coord_cartesian(clip="off",xlim=c(2002,2024),expand=FALSE)
ggplot(dplyr::filter(fa_spe_bw_cg_n_b_regime_en_names_p_df,comm_name%in%fa_cg_spe[11:19]), 
       aes(x=Year,y=CG)) +
  geom_point(size=0.5)+ 
  geom_line(
    aes(x=Year,y=value,linetype=factor(p_val,levels=c("p<0.05","p>0.05")),
        color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1)+
  geom_ribbon(
    aes(ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3) +
  facet_grid(variable~comm_name,scales="free_y") +
  theme_bw()+
  theme(plot.margin=unit(c(0,0.5,0,2),"lines"),
        plot.title=element_text(size=20,vjust=-1,face="plain"),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text( 
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=12))+   
  scale_color_manual(values=c("red","black"))+
  scale_fill_manual(values=c("red","black"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value",
    linetype="p-value"
  )+
  ylab("")+ 
  ggtitle("Fall")+
  geom_segment(
    x=fa_cg_arrow_year_loc,xend=fa_cg_arrow_year_loc,y=fa_depth_arrow_deep,yend=fa_depth_arrow_shallow,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Depth (m)",
                       comm_name=fa_cg_spe[11],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=fa_cg_arrow_year_loc,xend=fa_cg_arrow_year_loc,y=fa_crossshore_arrow_inshore,yend=fa_crossshore_arrow_offshore,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Crossshore (km)",
                       comm_name=fa_cg_spe[11],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=fa_cg_arrow_year_loc,xend=fa_cg_arrow_year_loc,y=fa_alongshore_arrow_SW,yend=fa_alongshore_arrow_NE,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Alongshore (km)",
                       comm_name=fa_cg_spe[11],
                       Season=c("Fall","Spring"))
  ) +
  geom_text(
    data=fa_cg_text_df2,
    aes(label=label),size=4
  )+  
  coord_cartesian(clip="off",xlim=c(2002,2024),expand = FALSE)
ggplot(dplyr::filter(sp_spe_bw_cg_n_b_regime_en_names_p_df,comm_name%in%sp_cg_spe[1:8]), 
       aes(x=Year,y=CG)) +
  geom_point(size=0.5)+ 
  geom_line(
    aes(x=Year,y=value,linetype=factor(p_val,levels=c("p<0.05","p>0.05")),
        color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1)+
  geom_ribbon(
    aes(ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3) +
  facet_grid(variable~comm_name,scales="free_y") +
  theme_bw()+
  theme(plot.margin=unit(c(0,0.5,0,2),"lines"),
        plot.title=element_text(size=20,vjust=-1,face="plain"),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=12))+   
  scale_color_manual(values=c("red","black"))+
  scale_fill_manual(values=c("red","black"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value",
    linetype="p-value"
  )+
  ylab("")+ 
  ggtitle("Spring")+
  geom_segment(
    x=sp_cg_arrow_year_loc,xend=sp_cg_arrow_year_loc,y=sp_depth_arrow_deep,yend=sp_depth_arrow_shallow,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Depth (m)",
                       comm_name=sp_cg_spe[1],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=sp_cg_arrow_year_loc,xend=sp_cg_arrow_year_loc,y=sp_crossshore_arrow_inshore,yend=sp_crossshore_arrow_offshore,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Crossshore (km)",
                       comm_name=sp_cg_spe[1],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=sp_cg_arrow_year_loc,xend=sp_cg_arrow_year_loc,y=sp_alongshore_arrow_SW,yend=sp_alongshore_arrow_NE,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Alongshore (km)",
                       comm_name=sp_cg_spe[1],
                       Season=c("Fall","Spring"))
  ) +
  geom_text(
    data=sp_cg_text_df1,
    aes(label=label),size=4
  )+  
  coord_cartesian(clip="off",xlim=c(2002,2024),expand=FALSE)
ggplot(dplyr::filter(sp_spe_bw_cg_n_b_regime_en_names_p_df,comm_name%in%sp_cg_spe[9:16]), 
       aes(x=Year,y=CG)) +
  geom_point(size=0.5)+ 
  geom_line(
    aes(x=Year,y=value,linetype=factor(p_val,levels=c("p<0.05","p>0.05")),
        color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1)+
  geom_ribbon(
    aes(ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3) +
  facet_grid(variable~comm_name,scales="free_y") +
  theme_bw()+
  theme(plot.margin=unit(c(0,0.5,0,2),"lines"),
        plot.title=element_text(size=20,vjust=-1,face="plain"),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=12))+   
  scale_color_manual(values=c("red","black"))+
  scale_fill_manual(values=c("red","black"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value",
    linetype="p-value"
  )+
  ylab("")+ 
  ggtitle("Spring")+
  geom_segment(
    x=sp_cg_arrow_year_loc,xend=sp_cg_arrow_year_loc,y=sp_depth_arrow_deep,yend=sp_depth_arrow_shallow,
    arrow=arrow(type="open",ends="both",length=unit(10, "pt")),
    data=expand.grid(variable="Depth (m)",
                       comm_name=sp_cg_spe[9],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=sp_cg_arrow_year_loc,xend=sp_cg_arrow_year_loc,y=sp_crossshore_arrow_inshore,yend=sp_crossshore_arrow_offshore,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Crossshore (km)",
                       comm_name=sp_cg_spe[9],
                       Season=c("Fall","Spring"))
  ) +
  geom_segment(
    x=sp_cg_arrow_year_loc,xend=sp_cg_arrow_year_loc,y=sp_alongshore_arrow_SW,yend=sp_alongshore_arrow_NE,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Alongshore (km)",
                       comm_name=sp_cg_spe[9],
                       Season=c("Fall","Spring"))
  ) +
  geom_text(
    data=sp_cg_text_df2,
    aes(label=label),size=4
  )+  
  coord_cartesian(clip = "off",xlim=c(2002,2024),expand=FALSE)

spe_bw_b_regime_en_names_p_df<-spe_aw_ind_conf_dist_en_pp_df2%>%
  full_join(spe_names_pp_use)
spe_bound_regime_mean_yr_pp_df2<-spe_bound_regime_mean_yr_p_df2%>%
  full_join(spe_names_pp_use)%>%
  mutate(lower_line_type=ifelse(lower_line_type=="sig","p<0.05",
                                ifelse(lower_line_type=="nonsig","p>0.05",NA)),
         upper_line_type=ifelse(upper_line_type=="sig","p<0.05",
                                ifelse(upper_line_type=="nonsig","p>0.05",NA)))
survey_bound_en_pp_df3<-survey_bound_en_pp_df3%>%
  full_join(spe_names_pp_use)%>%
  dplyr::filter(!is.na(variable))%>%
  mutate(boundary=ifelse(boundary=="offshore","Offshore",
                         ifelse(boundary=="inshore","Inshore",
                                ifelse(boundary=="depth","Depth",boundary))))
b_text_year_loc<-1987
b_arrow_year_loc<-1987
b_NE_text_lab_loc<-450;b_SW_text_lab_loc<--10
b_offshore_text_lab_loc<-55;b_inshore_text_lab_loc<--30
b_shallow_text_lab_loc<--50;b_deep_text_lab_loc<--200
b_alongshore_arrow_NE<-400;b_alongshore_arrow_SW<-0
b_crossshore_arrow_offshore<-50;b_crossshore_arrow_inshore<--25
b_depth_arrow_shallow<--60;b_depth_arrow_deep<--180
b_text_df1<-rbind(
  expand.grid(variable="Depth (m)",
              comm_name=spe_names_pp_use$comm_name[1],
              Season=c("Fall","Spring"),
              Year=b_text_year_loc,
              b=b_deep_text_lab_loc,
              label="Deep"),
  expand.grid(variable="Depth (m)",
              comm_name=spe_names_pp_use$comm_name[1],
              Season=c("Fall","Spring"),
              Year=b_text_year_loc,
              b=b_shallow_text_lab_loc,
              label="Shallow"),
  expand.grid(variable="Crossshore (km)",
              comm_name=spe_names_pp_use$comm_name[1],
              Season=c("Fall","Spring"),
              Year=b_text_year_loc,
              b=b_inshore_text_lab_loc,
              label="Inshore"),
  expand.grid(variable="Crossshore (km)",
              comm_name=spe_names_pp_use$comm_name[1],
              Season=c("Fall","Spring"),
              Year=b_text_year_loc,
              b=b_offshore_text_lab_loc,
              label="Offshore"),
  expand.grid(variable="Alongshore (km)",
              comm_name=spe_names_pp_use$comm_name[1],
              Season=c("Fall","Spring"),
              Year=b_text_year_loc,
              b=b_SW_text_lab_loc,
              label="SW"),
  expand.grid(variable="Alongshore (km)",
              comm_name=spe_names_pp_use$comm_name[1],
              Season=c("Fall","Spring"),
              Year=b_text_year_loc,
              b=b_NE_text_lab_loc,
              label="NE")
)
ggplot(dplyr::filter(spe_bw_b_regime_en_names_p_df,comm_name%in%spe_names_pp_use$comm_name[1:11]), 
       aes(x=Year,y=lower)) +
  geom_point(aes(color=Season),size=1)+
  geom_point(aes(x=Year, y=upper,color=Season),size=1)+
  geom_line(data=dplyr::filter(spe_bound_regime_mean_yr_pp_df2,comm_name%in%spe_names_pp_use$comm_name[1:11]),
            aes(x=Year,y=value_lower,color=Season,
                linetype=factor(lower_line_type,levels=c("p<0.05","p>0.05"))),
            linewidth=1.2)+
  geom_line(data=dplyr::filter(spe_bound_regime_mean_yr_pp_df2,comm_name%in%spe_names_pp_use$comm_name[1:11]),
            aes(x=Year,y=value_upper,color=Season,
                linetype=factor(upper_line_type,levels=c("p<0.05","p>0.05"))),
            linewidth=1.2)+
  facet_grid(variable~comm_name,scales="free_y")+
  geom_ribbon(aes(ymax=upper,ymin=lower,fill=Season,color=NULL),alpha=0.3) +
  theme_bw()+
  theme(plot.margin=unit(c(0,1,0,2.5),"lines"),,
        plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=16),
        legend.title=element_text(size=14,face="bold"),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=14, 
          face="plain"),
        axis.title.y=element_text(
          size=14, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=14))+  
  scale_color_manual(name="Season",values=c("blue","orange","forestgreen"),guide=guide_legend(order=1))+
  scale_fill_manual(name="Season",values=c("blue","orange","forestgreen"),guide=guide_legend(order=1))+
  scale_linetype_manual("p-value",values=c("p<0.05"=1,"p>0.05"=6),guide=guide_legend(order=2))+
  ggnewscale::new_scale_color()+
  geom_hline(data=dplyr::filter(survey_bound_en_pp_df3,comm_name%in%spe_names_pp_use$comm_name[1:11]),
             mapping=aes(yintercept=value,color=boundary,group=boundary),
             alpha=0.5,
             linewidth=1.5)+
  scale_colour_manual(name='Boundary',
                      values=c('NE'='green','SW'='red','Offshore'='black','Inshore'='cyan','Depth'='forestgreen'),
                      labels=c('NE','SW','Offshore','Inshore','Depth'),
                      guide=guide_legend(order=3)
  )+
  guides(color=guide_legend(
    order=3,
    override.aes=list(
      color=c('green','red','black','cyan','forestgreen'),
      fill=NA,
      labels=c('NE','SW','Offshore','Inshore','Depth'),
      linetype=c("solid","solid","solid","solid","solid"),
      shape=c(NA, NA,NA,NA,NA)))
  ) +
  ylab("")+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  geom_segment(
    x=b_arrow_year_loc,xend=b_arrow_year_loc,y=b_alongshore_arrow_SW,yend=b_alongshore_arrow_NE,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Alongshore (km)",
                       comm_name=spe_names_pp_use$comm_name[1],
                       Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=b_arrow_year_loc,xend=b_arrow_year_loc,y=b_crossshore_arrow_inshore,yend=b_crossshore_arrow_offshore,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Crossshore (km)",
                       comm_name=spe_names_pp_use$comm_name[1],
                       Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=b_arrow_year_loc,xend=b_arrow_year_loc,y=b_depth_arrow_shallow,yend=b_depth_arrow_deep,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Depth (m)",
                       comm_name=spe_names_pp_use$comm_name[1],
                       Season=c("Fall","Spring"))
  )+
  geom_text(
    data=b_text_df1,
    aes(x=Year,y=b,label=label),size=4
  )+
  coord_cartesian(clip="off",xlim=c(2002,2024),expand = FALSE)
b_text_df2<-rbind(
  expand.grid(variable="Depth (m)",
              comm_name=spe_names_pp_use$comm_name[12],
              Season=c("Fall","Spring"),
              Year=b_text_year_loc,
              b=b_deep_text_lab_loc,
              label="Deep"),
  expand.grid(variable="Depth (m)",
              comm_name=spe_names_pp_use$comm_name[12],
              Season=c("Fall","Spring"),
              Year=b_text_year_loc,
              b=b_shallow_text_lab_loc,
              label="Shallow"),
  expand.grid(variable="Crossshore (km)",
              comm_name=spe_names_pp_use$comm_name[12],
              Season=c("Fall","Spring"),
              Year=b_text_year_loc,
              b=b_inshore_text_lab_loc,
              label="Inshore"),
  expand.grid(variable="Crossshore (km)",
              comm_name=spe_names_pp_use$comm_name[12],
              Season=c("Fall","Spring"),
              Year=b_text_year_loc,
              b=b_offshore_text_lab_loc,
              label="Offshore"),
  expand.grid(variable="Alongshore (km)",
              comm_name=spe_names_pp_use$comm_name[12],
              Season=c("Fall","Spring"),
              Year=b_text_year_loc,
              b=b_SW_text_lab_loc,
              label="SW"),
  expand.grid(variable="Alongshore (km)",
              comm_name=spe_names_pp_use$comm_name[12],
              Season=c("Fall","Spring"),
              Year=b_text_year_loc,
              b=b_NE_text_lab_loc,
              label="NE")
)
ggplot(dplyr::filter(spe_bw_b_regime_en_names_p_df,comm_name%in%spe_names_pp_use$comm_name[12:22]), 
       aes(x=Year,y=lower)) +
  geom_point(aes(color=Season),size=1)+
  geom_point(aes(x=Year, y=upper,color=Season),size=1)+
  geom_line(data=dplyr::filter(spe_bound_regime_mean_yr_pp_df2,comm_name%in%spe_names_pp_use$comm_name[12:22]),
            aes(x=Year,y=value_lower,color=Season,
                linetype=factor(lower_line_type,levels=c("p<0.05","p>0.05"))),
            linewidth=1.2)+
  geom_line(data=dplyr::filter(spe_bound_regime_mean_yr_pp_df2,comm_name%in%spe_names_pp_use$comm_name[12:22]),
            aes(x=Year,y=value_upper,color=Season,
                linetype=factor(upper_line_type,levels=c("p<0.05","p>0.05"))),
            linewidth=1.2)+
  facet_grid(variable~comm_name,scales="free_y")+
  geom_ribbon(aes(ymax=upper,ymin=lower,fill=Season,color=NULL),alpha=0.3) +
  theme_bw()+
  theme(plot.margin=unit(c(0,1,0,2.5),"lines"),,
        plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=16),
        legend.title=element_text(size=14,face="bold"),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=14, 
          face="plain"),
        axis.title.y=element_text(
          size=14, 
          face="plain"),
        strip.text.x=element_text(size=11),
        strip.text.y=element_text(size=14))+  
  scale_color_manual(name="Season",values=c("blue","orange","forestgreen"),guide=guide_legend(order=1))+
  scale_fill_manual(name="Season",values=c("blue","orange","forestgreen"),guide=guide_legend(order=1))+
  scale_linetype_manual("p-value",values=c("p<0.05"=1,"p>0.05"=6),guide=guide_legend(order=2))+
  ggnewscale::new_scale_color()+
  geom_hline(data=dplyr::filter(survey_bound_en_pp_df3,comm_name%in%spe_names_pp_use$comm_name[12:22]),
             mapping=aes(yintercept=value,color=boundary,group=boundary),
             alpha=0.5,
             linewidth=1.5)+
  scale_colour_manual(name='Boundary', 
                      values=c('NE'='green','SW'='red','Offshore'='black','Inshore'='cyan','Depth'='forestgreen'),
                      labels=c('NE','SW','Offshore','Inshore','Depth'),
                      guide=guide_legend(order=3)
  )+
  guides(color=guide_legend(
    order=3,
    override.aes=list(
      color=c('green','red','black','cyan','forestgreen'),
      fill=NA,
      labels=c('NE','SW','Offshore','Inshore','Depth'),
      linetype=c("solid","solid","solid","solid","solid"),
      shape=c(NA,NA,NA,NA,NA)))
  ) +
  ylab("")+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  geom_segment(
    x=b_arrow_year_loc,xend=b_arrow_year_loc,y=b_alongshore_arrow_SW,yend=b_alongshore_arrow_NE,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Alongshore (km)",
                       comm_name=spe_names_pp_use$comm_name[12],
                       Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=b_arrow_year_loc,xend=b_arrow_year_loc,y=b_crossshore_arrow_inshore,yend=b_crossshore_arrow_offshore,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Crossshore (km)",
                       comm_name=spe_names_pp_use$comm_name[12],
                       Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=b_arrow_year_loc,xend=b_arrow_year_loc,y=b_depth_arrow_shallow,yend=b_depth_arrow_deep,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Depth (m)",
                       comm_name=spe_names_pp_use$comm_name[12],
                       Season=c("Fall","Spring"))
  )+
  geom_text(
    data=b_text_df2,
    aes(x=Year,y=b,label=label),size=4
  )+
  coord_cartesian(clip="off",xlim=c(2002,2024),expand = FALSE)

fa_spe_bw_b_regime_en_names_p_df<-spe_bw_b_regime_en_names_p_df%>%
  dplyr::filter(Season=="Fall")%>%
  mutate(comm_name=factor(comm_name,levels=fa_cg_spe))
fa_spe_bound_regime_mean_yr_pp_df2<-spe_bound_regime_mean_yr_pp_df2%>%
  dplyr::filter(Season=="Fall")%>%
  mutate(comm_name=factor(comm_name,levels=fa_cg_spe))
fa_survey_bound_en_pp_df3<-survey_bound_en_pp_df3%>%
  dplyr::filter(Season=="Fall")%>%
  mutate(comm_name=factor(comm_name,levels=fa_cg_spe))

fa_b_text_year_loc<-1987
fa_b_arrow_year_loc<-1987
fa_b_NE_text_lab_loc<-450;fa_b_SW_text_lab_loc<--10
fa_b_offshore_text_lab_loc<-55;fa_b_inshore_text_lab_loc<--30
fa_b_shallow_text_lab_loc<--50;fa_b_deep_text_lab_loc<--200
fa_b_alongshore_arrow_NE<-400;fa_b_alongshore_arrow_SW<-0
fa_b_crossshore_arrow_offshore<-50;fa_b_crossshore_arrow_inshore<--25
fa_b_depth_arrow_shallow<--60;fa_b_depth_arrow_deep<--180
fa_b_text_df1<-rbind(
  expand.grid(variable="Depth (m)",
              comm_name=fa_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=fa_b_text_year_loc,
              b=fa_b_deep_text_lab_loc,
              label="Deep"),
  expand.grid(variable="Depth (m)",
              comm_name=fa_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=fa_b_text_year_loc,
              b=fa_b_shallow_text_lab_loc,
              label="Shallow"),
  expand.grid(variable="Crossshore (km)",
              comm_name=fa_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=fa_b_text_year_loc,
              b=fa_b_inshore_text_lab_loc,
              label="Inshore"),
  expand.grid(variable="Crossshore (km)",
              comm_name=fa_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=fa_b_text_year_loc,
              b=fa_b_offshore_text_lab_loc,
              label="Offshore"),
  expand.grid(variable="Alongshore (km)",
              comm_name=fa_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=fa_b_text_year_loc,
              b=fa_b_SW_text_lab_loc,
              label="SW"),
  expand.grid(variable="Alongshore (km)",
              comm_name=fa_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=fa_b_text_year_loc,
              b=fa_b_NE_text_lab_loc,
              label="NE")
)

ggplot(dplyr::filter(fa_spe_bw_b_regime_en_names_p_df,comm_name%in%fa_cg_spe[1:10]), 
       aes(x=Year,y=lower)) +
  geom_point(size=1)+
  geom_point(aes(x=Year,y=upper),size=1)+
  geom_line(data=dplyr::filter(fa_spe_bound_regime_mean_yr_pp_df2,comm_name%in%fa_cg_spe[1:10]),
            aes(x=Year,y=value_lower,color=factor(lower_line_type,levels=c("p<0.05","p>0.05"))#,
            ),
            linewidth=1.2)+
  geom_line(data=dplyr::filter(fa_spe_bound_regime_mean_yr_pp_df2,comm_name%in%fa_cg_spe[1:10]),
            aes(x=Year,y=value_upper,color=factor(upper_line_type,levels=c("p<0.05","p>0.05"))#,
            ),
            linewidth=1.2)+
  facet_grid(variable~comm_name,scales="free_y")+
  geom_ribbon(aes(ymax=upper,ymin=lower,color=NULL),alpha=0.3) +
  theme_bw()+
  theme(plot.margin=unit(c(0,1,0,2.5),"lines"),,
        plot.title=element_text(size=20,vjust=0),
        legend.position="top",
        legend.text=element_text(size=16),
        legend.title=element_text(size=14,face="bold"),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=14, 
          face="plain"),
        axis.title.y=element_text(
          size=14, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=14))+  
  scale_color_manual(name="p-value",values=c("red","black"),guide=guide_legend(order=1))+
  ggnewscale::new_scale_color()+
  geom_hline(data=dplyr::filter(fa_survey_bound_en_pp_df3,comm_name%in%fa_cg_spe[1:10]),
             mapping=aes(yintercept=value,color=boundary,group=boundary),
             alpha=0.5,
             linewidth=1.5)+
  scale_colour_manual(name='Boundary',
                      values=c('NE'='green','SW'='orange','Offshore'='blue','Inshore'='cyan','Depth'='forestgreen'),
                      labels=c('NE','SW','Offshore','Inshore','Depth'),
                      guide=guide_legend(order=3)
  )+
  guides(color=guide_legend(
    order=3,
    override.aes=list(
      color=c('green','orange','blue','cyan','forestgreen'),
      fill=NA,
      labels=c('NE','SW','Offshore','Inshore','Depth'),
      linetype=c("solid","solid","solid","solid","solid"),
      shape=c(NA,NA,NA,NA,NA)))
  ) +
  ylab("")+
  ggtitle("Fall")+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  geom_segment(
    x=fa_b_arrow_year_loc,xend=fa_b_arrow_year_loc,y=fa_b_alongshore_arrow_SW,yend=fa_b_alongshore_arrow_NE,
    arrow = arrow(type="open",ends="both",length=unit(10,"pt")),
    data = expand.grid(variable="Alongshore (km)",
                       comm_name=fa_cg_spe[1],
                       Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=fa_b_arrow_year_loc,xend=fa_b_arrow_year_loc,y=fa_b_crossshore_arrow_inshore,yend=fa_b_crossshore_arrow_offshore,
    arrow = arrow(type="open",ends="both",length=unit(10,"pt")),
    data = expand.grid(variable="Crossshore (km)",
                       comm_name=fa_cg_spe[1],
                       Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=fa_b_arrow_year_loc,xend=fa_b_arrow_year_loc,y=fa_b_depth_arrow_shallow,yend=fa_b_depth_arrow_deep,
    arrow = arrow(type="open",ends="both",length=unit(10,"pt")),
    data = expand.grid(variable="Depth (m)",
                       comm_name=fa_cg_spe[1],
                       Season=c("Fall","Spring"))
  )+
  geom_text(
    data=fa_b_text_df1,
    aes(x=Year,y=b,label=label),size=4
  )+
  coord_cartesian(clip="off",xlim=c(2002,2024),expand = FALSE)

fa_b_text_df2<-rbind(
  expand.grid(variable="Depth (m)",
              comm_name=fa_cg_spe[11],
              Season=c("Fall","Spring"),
              Year=fa_b_text_year_loc,
              b=fa_b_deep_text_lab_loc,
              label="Deep"),
  expand.grid(variable="Depth (m)",
              comm_name=fa_cg_spe[11],
              Season=c("Fall","Spring"),
              Year=fa_b_text_year_loc,
              b=fa_b_shallow_text_lab_loc,
              label="Shallow"),
  expand.grid(variable="Crossshore (km)",
              comm_name=fa_cg_spe[11],
              Season=c("Fall","Spring"),
              Year=fa_b_text_year_loc,
              b=fa_b_inshore_text_lab_loc,
              label="Inshore"),
  expand.grid(variable="Crossshore (km)",
              comm_name=fa_cg_spe[11],
              Season=c("Fall","Spring"),
              Year=fa_b_text_year_loc,
              b=fa_b_offshore_text_lab_loc,
              label="Offshore"),
  expand.grid(variable="Alongshore (km)",
              comm_name=fa_cg_spe[11],
              Season=c("Fall","Spring"),
              Year=fa_b_text_year_loc,
              b=fa_b_SW_text_lab_loc,
              label="SW"),
  expand.grid(variable="Alongshore (km)",
              comm_name=fa_cg_spe[11],
              Season=c("Fall","Spring"),
              Year=fa_b_text_year_loc,
              b=fa_b_NE_text_lab_loc,
              label="NE")
)

ggplot(dplyr::filter(fa_spe_bw_b_regime_en_names_p_df,comm_name%in%fa_cg_spe[11:19]), 
       aes(x=Year,y=lower)) +
  geom_point(size=1)+
  geom_point(aes(x=Year,y=upper),size=1)+
  geom_line(data=dplyr::filter(fa_spe_bound_regime_mean_yr_pp_df2,comm_name%in%fa_cg_spe[11:19]),
            aes(x=Year,y=value_lower,color=factor(lower_line_type,levels=c("p<0.05","p>0.05"))#,
            ),
            linewidth=1.2)+
  geom_line(data=dplyr::filter(fa_spe_bound_regime_mean_yr_pp_df2,comm_name%in%fa_cg_spe[11:19]),
            aes(x=Year,y=value_upper,color=factor(upper_line_type,levels=c("p<0.05","p>0.05"))#,
            ),
            linewidth=1.2)+
  facet_grid(variable~comm_name,scales="free_y")+
  geom_ribbon(aes(ymax=upper,ymin=lower,color=NULL),alpha=0.3) +
  theme_bw()+
  theme(plot.margin=unit(c(0,1,0,2.5),"lines"),,
        plot.title=element_text(size=20,vjust=0),
        legend.position="top",
        legend.text=element_text(size=16),
        legend.title=element_text(size=14,face="bold"),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=14, 
          face="plain"),
        axis.title.y=element_text(
          size=14, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=14))+  
  scale_color_manual(name="p-value",values=c("red","black"),guide=guide_legend(order=1))+
  ggnewscale::new_scale_color()+
  geom_hline(data=dplyr::filter(fa_survey_bound_en_pp_df3,comm_name%in%fa_cg_spe[11:19]),
             mapping=aes(yintercept=value,color=boundary,group=boundary),
             alpha=0.5,
             linewidth=1.5)+
  scale_colour_manual(name='Boundary', 
                      values=c('NE'='green','SW'='orange','Offshore'='blue','Inshore'='cyan','Depth'='forestgreen'),
                      labels=c('NE','SW','Offshore','Inshore','Depth'),
                      guide=guide_legend(order=3)
  )+
  guides(color=guide_legend(
    order=3,
    override.aes=list(
      color=c('green','orange','blue','cyan','forestgreen'),
      fill=NA,
      labels=c('NE','SW','Offshore','Inshore','Depth'),
      linetype=c("solid","solid","solid","solid","solid"),
      shape=c(NA,NA,NA,NA,NA)))
  ) +
  ylab("")+
  ggtitle("Fall")+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  geom_segment(
    x=fa_b_arrow_year_loc,xend=fa_b_arrow_year_loc,y=fa_b_alongshore_arrow_SW,yend=fa_b_alongshore_arrow_NE,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Alongshore (km)",
                       comm_name=fa_cg_spe[11],
                       Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=fa_b_arrow_year_loc,xend=fa_b_arrow_year_loc,y=fa_b_crossshore_arrow_inshore,yend=fa_b_crossshore_arrow_offshore,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Crossshore (km)",
                       comm_name=fa_cg_spe[11],
                       Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=fa_b_arrow_year_loc,xend=fa_b_arrow_year_loc,y=fa_b_depth_arrow_shallow,yend=fa_b_depth_arrow_deep,
    arrow = arrow(type="open",ends="both",length=unit(10,"pt")),
    data = expand.grid(variable="Depth (m)",
                       comm_name=fa_cg_spe[11],
                       Season=c("Fall","Spring"))
  )+
  geom_text(
    data=fa_b_text_df2,
    aes(x=Year,y=b,label=label),size=4
  )+
  coord_cartesian(clip="off",xlim=c(2002,2024),expand = FALSE)
sp_spe_bw_b_regime_en_names_p_df<-spe_bw_b_regime_en_names_p_df%>%
  dplyr::filter(Season=="Spring")%>%
  mutate(comm_name=factor(comm_name,levels=sp_cg_spe))
sp_spe_bound_regime_mean_yr_pp_df2<-spe_bound_regime_mean_yr_pp_df2%>%
  dplyr::filter(Season=="Spring")%>%
  mutate(comm_name=factor(comm_name,levels=sp_cg_spe))
sp_survey_bound_en_pp_df3<-survey_bound_en_pp_df3%>%
  dplyr::filter(Season=="Spring")%>%
  mutate(comm_name=factor(comm_name,levels=sp_cg_spe))

sp_b_text_year_loc<-1988
sp_b_arrow_year_loc<-1988
sp_b_NE_text_lab_loc<-450;sp_b_SW_text_lab_loc<--10
sp_b_offshore_text_lab_loc<-55;sp_b_inshore_text_lab_loc<--30
sp_b_shallow_text_lab_loc<--50;sp_b_deep_text_lab_loc<--200
sp_b_alongshore_arrow_NE<-400;sp_b_alongshore_arrow_SW<-0
sp_b_crossshore_arrow_offshore<-50;sp_b_crossshore_arrow_inshore<--25
sp_b_depth_arrow_shallow<--60;sp_b_depth_arrow_deep<--180
sp_b_text_df1<-rbind(
  expand.grid(variable="Depth (m)",
              comm_name=sp_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=sp_b_text_year_loc,
              b=sp_b_deep_text_lab_loc,
              label="Deep"),
  expand.grid(variable="Depth (m)",
              comm_name=sp_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=sp_b_text_year_loc,
              b=sp_b_shallow_text_lab_loc,
              label="Shallow"),
  expand.grid(variable="Crossshore (km)",
              comm_name=sp_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=sp_b_text_year_loc,
              b=sp_b_inshore_text_lab_loc,
              label="Inshore"),
  expand.grid(variable="Crossshore (km)",
              comm_name=sp_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=sp_b_text_year_loc,
              b=sp_b_offshore_text_lab_loc,
              label="Offshore"),
  expand.grid(variable="Alongshore (km)",
              comm_name=sp_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=sp_b_text_year_loc,
              b=sp_b_SW_text_lab_loc,
              label="SW"),
  expand.grid(variable="Alongshore (km)",
              comm_name=sp_cg_spe[1],
              Season=c("Fall","Spring"),
              Year=sp_b_text_year_loc,
              b=sp_b_NE_text_lab_loc,
              label="NE")
)

ggplot(dplyr::filter(sp_spe_bw_b_regime_en_names_p_df,comm_name%in%sp_cg_spe[1:8]), 
       aes(x=Year,y=lower)) +
  geom_point(size=1)+
  geom_point(aes(x=Year,y=upper),size=1)+
  geom_line(data=dplyr::filter(sp_spe_bound_regime_mean_yr_pp_df2,comm_name%in%sp_cg_spe[1:8]),
            aes(x=Year,y=value_lower,color=factor(lower_line_type,levels=c("p<0.05","p>0.05"))#,
            ),
            linewidth=1.2)+
  geom_line(data=dplyr::filter(sp_spe_bound_regime_mean_yr_pp_df2,comm_name%in%sp_cg_spe[1:8]),
            aes(x=Year,y=value_upper,color=factor(upper_line_type,levels=c("p<0.05","p>0.05"))#,
            ),
            linewidth=1.2)+
  facet_grid(variable~comm_name,scales="free_y")+
  geom_ribbon(aes(ymax=upper,ymin=lower,color=NULL),alpha=0.3) +
  theme_bw()+
  theme(plot.margin=unit(c(0,1,0,2.5),"lines"),,
        plot.title=element_text(size=20,vjust=0),
        legend.position="top",
        legend.text=element_text(size=16),
        legend.title=element_text(size=14,face="bold"),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=14, 
          face="plain"),
        axis.title.y=element_text(
          size=14, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=14))+  
  scale_color_manual(name="p-value",values=c("red","black"),guide=guide_legend(order=1))+
   ggnewscale::new_scale_color()+
  geom_hline(data=dplyr::filter(sp_survey_bound_en_pp_df3,comm_name%in%sp_cg_spe[1:8]),
             mapping=aes(yintercept=value,color=boundary,group=boundary),
             alpha=0.5,
             linewidth=1.5)+
  scale_colour_manual(name='Boundary', 
                      values=c('NE'='green','SW'='orange','Offshore'='blue','Inshore'='cyan','Depth'='forestgreen'),
                      labels=c('NE','SW','Offshore','Inshore','Depth'),
                      guide=guide_legend(order=3)
  )+
  guides(color=guide_legend(
    order=3,
    override.aes=list(
      color=c('green','orange','blue','cyan','forestgreen'),
      fill=NA,
      labels=c('NE','SW','Offshore','Inshore','Depth'),
      linetype=c("solid","solid","solid","solid","solid"),
      shape=c(NA,NA,NA,NA,NA)))
  ) +
  ylab("")+
  ggtitle("Spring")+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  geom_segment(
    x=sp_b_arrow_year_loc,xend=sp_b_arrow_year_loc,y=sp_b_alongshore_arrow_SW,yend=sp_b_alongshore_arrow_NE,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Alongshore (km)",
                     comm_name=sp_cg_spe[1],
                     Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=sp_b_arrow_year_loc,xend=sp_b_arrow_year_loc,y=sp_b_crossshore_arrow_inshore,yend=sp_b_crossshore_arrow_offshore,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Crossshore (km)",
                     comm_name=sp_cg_spe[1],
                     Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=sp_b_arrow_year_loc,xend=sp_b_arrow_year_loc,y=sp_b_depth_arrow_shallow,yend=sp_b_depth_arrow_deep,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Depth (m)",
                     comm_name=sp_cg_spe[1],
                     Season=c("Fall","Spring"))
  )+
  geom_text(
    data=sp_b_text_df1,
    aes(x=Year,y=b,label=label),size=4
  )+
  coord_cartesian(clip="off",xlim=c(2002,2024),expand = FALSE)
sp_b_text_df2<-rbind(
  expand.grid(variable="Depth (m)",
              comm_name=sp_cg_spe[9],
              Season=c("Fall","Spring"),
              Year=sp_b_text_year_loc,
              b=sp_b_deep_text_lab_loc,
              label="Deep"),
  expand.grid(variable="Depth (m)",
              comm_name=sp_cg_spe[9],
              Season=c("Fall","Spring"),
              Year=sp_b_text_year_loc,
              b=sp_b_shallow_text_lab_loc,
              label="Shallow"),
  expand.grid(variable="Crossshore (km)",
              comm_name=sp_cg_spe[9],
              Season=c("Fall","Spring"),
              Year=sp_b_text_year_loc,
              b=sp_b_inshore_text_lab_loc,
              label="Inshore"),
  expand.grid(variable="Crossshore (km)",
              comm_name=sp_cg_spe[9],
              Season=c("Fall","Spring"),
              Year=sp_b_text_year_loc,
              b=sp_b_offshore_text_lab_loc,
              label="Offshore"),
  expand.grid(variable="Alongshore (km)",
              comm_name=sp_cg_spe[9],
              Season=c("Fall","Spring"),
              Year=sp_b_text_year_loc,
              b=sp_b_SW_text_lab_loc,
              label="SW"),
  expand.grid(variable="Alongshore (km)",
              comm_name=sp_cg_spe[9],
              Season=c("Fall","Spring"),
              Year=sp_b_text_year_loc,
              b=sp_b_NE_text_lab_loc,
              label="NE")
)

ggplot(dplyr::filter(sp_spe_bw_b_regime_en_names_p_df,comm_name%in%sp_cg_spe[9:16]), 
       aes(x=Year,y=lower)) +
  geom_point(size=1)+
  geom_point(aes(x=Year,y=upper),size=1)+
  geom_line(data=dplyr::filter(sp_spe_bound_regime_mean_yr_pp_df2,comm_name%in%sp_cg_spe[9:16]),
            aes(x=Year,y=value_lower,color=factor(lower_line_type,levels=c("p<0.05","p>0.05"))#,
            ),
            linewidth=1.2)+
  geom_line(data=dplyr::filter(sp_spe_bound_regime_mean_yr_pp_df2,comm_name%in%sp_cg_spe[9:16]),
            aes(x=Year,y=value_upper,color=factor(upper_line_type,levels=c("p<0.05","p>0.05"))#,
            ),
            linewidth=1.2)+
  facet_grid(variable~comm_name,scales="free_y")+
  geom_ribbon(aes(ymax=upper,ymin=lower,color=NULL),alpha=0.3) +
  theme_bw()+
  theme(plot.margin=unit(c(0,1,0,2.5),"lines"),,
        plot.title=element_text(size=20,vjust=0),
        legend.position="top",
        legend.text=element_text(size=16),
        legend.title=element_text(size=14,face="bold"),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=14, 
          face="plain"),
        axis.title.y=element_text(
          size=14, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=14))+  
  scale_color_manual(name="p-value",values=c("red","black"),guide=guide_legend(order=1))+
  ggnewscale::new_scale_color()+
  geom_hline(data=dplyr::filter(sp_survey_bound_en_pp_df3,comm_name%in%sp_cg_spe[9:16]),
             mapping=aes(yintercept=value,color=boundary,group=boundary),
             alpha=0.5,
             linewidth=1.5)+
  scale_colour_manual(name='Boundary',
                      values=c('NE'='green','SW'='orange','Offshore'='blue','Inshore'='cyan','Depth'='forestgreen'),
                      labels=c('NE','SW','Offshore','Inshore','Depth'),
                      guide=guide_legend(order=3)
  )+
  guides(color=guide_legend(
    order=3,
    override.aes=list(
      color=c('green','orange','blue','cyan','forestgreen'),
      fill=NA,
      labels=c('NE','SW','Offshore','Inshore','Depth'),
      linetype=c("solid","solid","solid","solid","solid"),
      shape=c(NA,NA,NA,NA,NA)))
  ) +
  ylab("")+
  ggtitle("Spring")+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  geom_segment(
    x=sp_b_arrow_year_loc,xend=sp_b_arrow_year_loc,y=sp_b_alongshore_arrow_SW,yend=sp_b_alongshore_arrow_NE,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Alongshore (km)",
                     comm_name=sp_cg_spe[9],
                     Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=sp_b_arrow_year_loc,xend=sp_b_arrow_year_loc,y=sp_b_crossshore_arrow_inshore,yend=sp_b_crossshore_arrow_offshore,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Crossshore (km)",
                     comm_name=sp_cg_spe[9],
                     Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=sp_b_arrow_year_loc,xend=sp_b_arrow_year_loc,y=sp_b_depth_arrow_shallow,yend=sp_b_depth_arrow_deep,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Depth (m)",
                     comm_name=sp_cg_spe[9],
                     Season=c("Fall","Spring"))
  )+
  geom_text(
    data=sp_b_text_df2,
    aes(x=Year,y=b,label=label),size=4
  )+
  coord_cartesian(clip="off",xlim=c(2002,2024),expand = FALSE)

ggplot(dplyr::filter(fa_spe_bw_b_regime_en_names_p_df,comm_name%in%fa_cg_spe[1:10]), 
       aes(x=Year,y=lower)) +
  geom_point(size=1)+
  geom_point(aes(x=Year,y=upper),size=1)+
  geom_line(data=dplyr::filter(fa_spe_bound_regime_mean_yr_pp_df2,comm_name%in%fa_cg_spe[1:10]),
            aes(x=Year,y=value_lower,color=factor(lower_line_type,levels=c("p<0.05","p>0.05")),
                linetype=factor(lower_line_type,levels=c("p<0.05","p>0.05"))
            ),
            linewidth=1.2)+
  geom_line(data=dplyr::filter(fa_spe_bound_regime_mean_yr_pp_df2,comm_name%in%fa_cg_spe[1:10]),
            aes(x=Year,y=value_upper,color=factor(upper_line_type,levels=c("p<0.05","p>0.05")),
                linetype=factor(upper_line_type,levels=c("p<0.05","p>0.05"))
            ),
            linewidth=1.2)+
  facet_grid(variable~comm_name,scales="free_y")+
  geom_ribbon(aes(ymax=upper,ymin=lower,color=NULL),alpha=0.3) +
  theme_bw()+
  theme(plot.margin=unit(c(0,1,0,2.5),"lines"),,
        plot.title=element_text(size=20,vjust=0),
        legend.position="top",
        legend.text=element_text(size=16),
        legend.title=element_text(size=14,face="bold"),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=14, 
          face="plain"),
        axis.title.y=element_text( 
          size=14, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=14))+  
  scale_color_manual(name="p-value",values=c("red","black"),guide=guide_legend(order=1))+
  scale_linetype_manual("p-value",values=c("p<0.05"=1,"p>0.05"=6),guide=guide_legend(order=1))+
  ggnewscale::new_scale_color()+
  geom_hline(data=dplyr::filter(fa_survey_bound_en_pp_df3,comm_name%in%fa_cg_spe[1:10]),
             mapping=aes(yintercept=value,color=boundary,group=boundary),
             alpha=0.5,
             linewidth=1.5)+
  scale_colour_manual(name='Boundary',
                      value=c('NE'='green','SW'='orange','Offshore'='blue','Inshore'='cyan','Depth'='forestgreen'),
                      labels=c('NE','SW','Offshore','Inshore','Depth'),
                      guide=guide_legend(order=3)
  )+
  guides(color=guide_legend(
    order=3,
    override.aes=list(
      color=c('green','orange','blue','cyan','forestgreen'),
      fill=NA,
      labels=c('NE','SW','Offshore','Inshore','Depth'),
      linetype=c("solid", "solid","solid","solid","solid"),
      shape=c(NA,NA,NA,NA,NA)))
  ) +
  ylab("")+
  ggtitle("Fall")+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  geom_segment(
    x=fa_b_arrow_year_loc,xend=fa_b_arrow_year_loc,y=fa_b_alongshore_arrow_SW,yend=fa_b_alongshore_arrow_NE,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Alongshore (km)",
                       comm_name=fa_cg_spe[1],
                       Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=fa_b_arrow_year_loc,xend=fa_b_arrow_year_loc,y=fa_b_crossshore_arrow_inshore,yend=fa_b_crossshore_arrow_offshore,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Crossshore (km)",
                       comm_name=fa_cg_spe[1],
                       Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=fa_b_arrow_year_loc,xend=fa_b_arrow_year_loc,y=fa_b_depth_arrow_shallow,yend=fa_b_depth_arrow_deep,
    arrow = arrow(type="open",ends="both",length=unit(10,"pt")),
    data = expand.grid(variable="Depth (m)",
                       comm_name=fa_cg_spe[1],
                       Season=c("Fall","Spring"))
  )+
  geom_text(
    data=fa_b_text_df1,
    aes(x=Year,y=b,label=label),size=4
  )+
  coord_cartesian(clip="off",xlim=c(2002,2024),expand = FALSE)
ggplot(dplyr::filter(fa_spe_bw_b_regime_en_names_p_df,comm_name%in%fa_cg_spe[11:19]), 
       aes(x=Year,y=lower)) +
  geom_point(size=1)+
  geom_point(aes(x=Year,y=upper),size=1)+
  geom_line(data=dplyr::filter(fa_spe_bound_regime_mean_yr_pp_df2,comm_name%in%fa_cg_spe[11:19]),
            aes(x=Year,y=value_lower,color=factor(lower_line_type,levels=c("p<0.05","p>0.05")),
                linetype=factor(lower_line_type,levels=c("p<0.05","p>0.05"))
            ),
            linewidth=1.2)+
  geom_line(data=dplyr::filter(fa_spe_bound_regime_mean_yr_pp_df2,comm_name%in%fa_cg_spe[11:19]),
            aes(x=Year,y=value_upper,color=factor(upper_line_type,levels=c("p<0.05","p>0.05")),
                linetype=factor(upper_line_type,levels=c("p<0.05","p>0.05"))
            ),
            linewidth=1.2)+
  facet_grid(variable~comm_name,scales="free_y")+
  geom_ribbon(aes(ymax=upper,ymin=lower,color=NULL),alpha=0.3) +
  theme_bw()+
  theme(plot.margin=unit(c(0,1,0,2.5),"lines"),,
        plot.title=element_text(size=20,vjust=0),
        legend.position="top",
        legend.text=element_text(size=16),
        legend.title=element_text(size=14,face="bold"),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=14, 
          face="plain"),
        axis.title.y=element_text( 
          size=14, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=14))+  
  scale_color_manual(name="p-value",values=c("red","black"), guide=guide_legend(order=1))+
  scale_linetype_manual("p-value",values=c("p<0.05"=1,"p>0.05"=6), guide=guide_legend(order=1))+
  ggnewscale::new_scale_color()+
  geom_hline(data=dplyr::filter(fa_survey_bound_en_pp_df3,comm_name%in%fa_cg_spe[11:19]),
             mapping=aes(yintercept=value,color=boundary,group=boundary),
             alpha=0.5,
             linewidth=1.5)+
  scale_colour_manual(name='Boundary',
                      values=c('NE'='green','SW'='orange','Offshore'='blue','Inshore'='cyan','Depth'='forestgreen'),
                      labels=c('NE','SW','Offshore','Inshore','Depth'),
                      guide=guide_legend(order=3)
  )+
  guides(color=guide_legend(
    order=3,
    override.aes=list(
      color=c('green','orange','blue','cyan','forestgreen'),
      fill=NA,
      labels=c('NE','SW','Offshore','Inshore','Depth'),
      linetype=c("solid","solid","solid","solid","solid"),
      shape=c(NA, NA,NA,NA,NA)))
  ) +
  ylab("")+
  ggtitle("Fall")+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  geom_segment(
    x=fa_b_arrow_year_loc,xend=fa_b_arrow_year_loc,y=fa_b_alongshore_arrow_SW,yend=fa_b_alongshore_arrow_NE,
    arrow = arrow(type="open", ends="both",length = unit(10, "pt")),
    data = expand.grid(variable="Alongshore (km)",
                       comm_name=fa_cg_spe[11],
                       Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=fa_b_arrow_year_loc,xend=fa_b_arrow_year_loc,y=fa_b_crossshore_arrow_inshore,yend=fa_b_crossshore_arrow_offshore,
    arrow = arrow(type="open", ends="both",length = unit(10, "pt")),
    data = expand.grid(variable="Crossshore (km)",
                       comm_name=fa_cg_spe[11],
                       Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=fa_b_arrow_year_loc,xend=fa_b_arrow_year_loc,y=fa_b_depth_arrow_shallow,yend=fa_b_depth_arrow_deep,
    arrow=arrow(type="open",ends="both",length=unit(10,"pt")),
    data=expand.grid(variable="Depth (m)",
                       comm_name=fa_cg_spe[11],
                       Season=c("Fall","Spring"))
  )+
  geom_text(
    data=fa_b_text_df2,
    aes(x=Year,y=b,label=label),size=4
  )+
  coord_cartesian(clip="off",xlim=c(2002,2024),expand=FALSE)
ggplot(dplyr::filter(sp_spe_bw_b_regime_en_names_p_df,comm_name%in%sp_cg_spe[1:8]), 
       aes(x=Year,y=lower)) +
  geom_point(size=1)+
  geom_point(aes(x=Year,y=upper),size=1)+
  geom_line(data=dplyr::filter(sp_spe_bound_regime_mean_yr_pp_df2,comm_name%in%sp_cg_spe[1:8]),
            aes(x=Year,y=value_lower,color=factor(lower_line_type,levels=c("p<0.05","p>0.05")),
                linetype=factor(lower_line_type,levels=c("p<0.05","p>0.05"))
            ),
            linewidth=1.2)+
  geom_line(data=dplyr::filter(sp_spe_bound_regime_mean_yr_pp_df2,comm_name%in%sp_cg_spe[1:8]),
            aes(x=Year,y=value_upper,color=factor(upper_line_type,levels=c("p<0.05","p>0.05")),
                linetype=factor(upper_line_type,levels=c("p<0.05","p>0.05"))
            ),
            linewidth=1.2)+
  facet_grid(variable~comm_name,scales="free_y")+
  geom_ribbon(aes(ymax=upper,ymin=lower,color=NULL),alpha=0.3) +
  theme_bw()+
  theme(plot.margin=unit(c(0,1,0,2.5),"lines"),,
        plot.title=element_text(size=20,vjust=0),
        legend.position="top",
        legend.text=element_text(size=16),
        legend.title=element_text(size=14,face="bold"),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=14, 
          face="plain"),
        axis.title.y=element_text(
          size=14, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=14))+  
  scale_color_manual(name="p-value",values=c("red","black"),guide=guide_legend(order=1))+
  scale_linetype_manual("p-value",values=c("p<0.05"=1,"p>0.05"=6),guide=guide_legend(order=1))+
  ggnewscale::new_scale_color()+
  geom_hline(data=dplyr::filter(sp_survey_bound_en_pp_df3,comm_name%in%sp_cg_spe[1:8]),
             mapping=aes(yintercept=value,color=boundary,group=boundary),
             alpha=0.5,
             linewidth=1.5)+
  scale_colour_manual(name='Boundary', 
                      values=c('NE'='green','SW'='orange','Offshore'='blue','Inshore'='cyan','Depth'='forestgreen'),
                      labels=c('NE','SW','Offshore','Inshore','Depth'),
                      guide=guide_legend(order=3)
  )+
  guides(color=guide_legend(
    order=3,
    override.aes=list(
      color=c('green','orange','blue','cyan','forestgreen'),
      fill=NA,
      labels=c('NE','SW','Offshore','Inshore','Depth'),
      linetype=c("solid", "solid","solid","solid","solid"),
      shape=c(NA, NA,NA,NA,NA)))#,
  ) +
  ylab("")+
  ggtitle("Spring")+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  geom_segment(
    x=sp_b_arrow_year_loc,xend=sp_b_arrow_year_loc,y=sp_b_alongshore_arrow_SW,yend=sp_b_alongshore_arrow_NE,
    arrow=arrow(type="open", ends="both",length = unit(10, "pt")),
    data=expand.grid(variable="Alongshore (km)",
                     comm_name=sp_cg_spe[1],
                     Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=sp_b_arrow_year_loc,xend=sp_b_arrow_year_loc,y=sp_b_crossshore_arrow_inshore,yend=sp_b_crossshore_arrow_offshore,
    arrow=arrow(type="open", ends="both",length = unit(10, "pt")),
    data=expand.grid(variable="Crossshore (km)",
                     comm_name=sp_cg_spe[1],
                     Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=sp_b_arrow_year_loc,xend=sp_b_arrow_year_loc,y=sp_b_depth_arrow_shallow,yend=sp_b_depth_arrow_deep,
    arrow=arrow(type="open", ends="both",length = unit(10, "pt")),
    data=expand.grid(variable="Depth (m)",
                     comm_name=sp_cg_spe[1],
                     Season=c("Fall","Spring"))
  )+
  geom_text(
    data=sp_b_text_df1,
    aes(x=Year,y=b,label=label),size=4
  )+
  coord_cartesian(clip="off",xlim=c(2002,2024),expand = FALSE)
ggplot(dplyr::filter(sp_spe_bw_b_regime_en_names_p_df,comm_name%in%sp_cg_spe[9:16]), 
       aes(x=Year,y=lower)) +
  geom_point(size=1)+
  geom_point(aes(x=Year,y=upper),size=1)+
  geom_line(data=dplyr::filter(sp_spe_bound_regime_mean_yr_pp_df2,comm_name%in%sp_cg_spe[9:16]),
            aes(x=Year,y=value_lower,color=factor(lower_line_type,levels=c("p<0.05","p>0.05")),
                linetype=factor(lower_line_type,levels=c("p<0.05","p>0.05"))
            ),
            linewidth=1.2)+
  geom_line(data=dplyr::filter(sp_spe_bound_regime_mean_yr_pp_df2,comm_name%in%sp_cg_spe[9:16]),
            aes(x=Year,y=value_upper,color=factor(upper_line_type,levels=c("p<0.05","p>0.05")),
                linetype=factor(upper_line_type,levels=c("p<0.05","p>0.05"))
            ),
            linewidth=1.2)+
  facet_grid(variable~comm_name,scales="free_y")+
  geom_ribbon(aes(ymax=upper,ymin=lower,color=NULL),alpha=0.3) +
  theme_bw()+
  theme(plot.margin=unit(c(0,1,0,2.5),"lines"),,
        plot.title=element_text(size=20,vjust=0),
        legend.position="top",
        legend.text=element_text(size=16),
        legend.title=element_text(size=14,face="bold"),
        axis.text.x=element_text(
          size=12, 
          face="plain"),
        axis.text.y=element_text(
          size=12, 
          face="plain"),  
        axis.title.x=element_text(
          size=14, 
          face="plain"),
        axis.title.y=element_text(
          size=14, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=14))+  
  scale_color_manual(name="p-value",values=c("red","black"),guide=guide_legend(order=1))+
  scale_linetype_manual("p-value",values=c("p<0.05"=1,"p>0.05"=6),guide=guide_legend(order=1))+
  ggnewscale::new_scale_color()+
  geom_hline(data=dplyr::filter(sp_survey_bound_en_pp_df3,comm_name%in%sp_cg_spe[9:16]),
             mapping=aes(yintercept=value,color=boundary,group=boundary),
             alpha=0.5,
             linewidth=1.5)+
  scale_colour_manual(name='Boundary', 
                      values=c('NE'='green','SW'='orange','Offshore'='blue','Inshore'='cyan','Depth'='forestgreen'),
                      labels=c('NE','SW','Offshore','Inshore','Depth'),
                      guide=guide_legend(order=3)
  )+
  guides(color=guide_legend(
    order=3,
    override.aes=list(
      color=c('green','orange','blue','cyan','forestgreen'),
      fill=NA,
      labels=c('NE','SW','Offshore','Inshore','Depth'),
      linetype=c("solid", "solid","solid","solid","solid"),
      shape=c(NA,NA,NA,NA,NA)))
  ) +
  ylab("")+
  ggtitle("Spring")+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  geom_segment(
    x=sp_b_arrow_year_loc,xend=sp_b_arrow_year_loc,y=sp_b_alongshore_arrow_SW,yend=sp_b_alongshore_arrow_NE,
    arrow=arrow(type="open", ends="both",length = unit(10, "pt")),
    data=expand.grid(variable="Alongshore (km)",
                     comm_name=sp_cg_spe[9],
                     Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=sp_b_arrow_year_loc,xend=sp_b_arrow_year_loc,y=sp_b_crossshore_arrow_inshore,yend=sp_b_crossshore_arrow_offshore,
    arrow=arrow(type="open", ends="both",length = unit(10, "pt")),
    data=expand.grid(variable="Crossshore (km)",
                     comm_name=sp_cg_spe[9],
                     Season=c("Fall","Spring"))
  )+
  geom_segment(
    x=sp_b_arrow_year_loc,xend=sp_b_arrow_year_loc,y=sp_b_depth_arrow_shallow,yend=sp_b_depth_arrow_deep,
    arrow=arrow(type="open", ends="both",length = unit(10, "pt")),
    data=expand.grid(variable="Depth (m)",
                     comm_name=sp_cg_spe[9],
                     Season=c("Fall","Spring"))
  )+
  geom_text(
    data=sp_b_text_df2,
    aes(x=Year,y=b,label=label),size=4
  )+
  coord_cartesian(clip="off",xlim=c(2002,2024),expand = FALSE)

survey_eff_regime_mean_yr_alongshore_pp_df<-survey_eff_regime_mean_yr_alongshore_df%>%
  left_join(spe_names_pp_use)%>%
  mutate(p_val=ifelse(line_type=="sig","p<0.05",
                      ifelse(line_type=="nonsig","p>0.05",NA)))

ggplot(survey_eff_regime_mean_yr_alongshore_pp_df) +
  geom_point(aes(x=Year, y=Pin,color=Season),size=1)+ 
  geom_line(aes(x=Year,y=value,color=Season,linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
            linewidth=1.5)+
  geom_ribbon(aes(x=Year,ymax=up_value,ymin=lo_value,fill=Season,color=NULL),alpha=0.3)+
  facet_wrap(~comm_name, scales = "free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        plot.title=element_text(size=20,vjust=0),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+   
  scale_color_manual(values=c("blue","orange"))+
  scale_fill_manual(values=c("blue","orange"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ggtitle("Alongshore Pin")+
  ylab("Proportion")+
  labs(
    linetype="p-value"
  )

survey_eff_regime_mean_yr_crossshore_pp_df<-survey_eff_regime_mean_yr_crossshore_df%>%
  left_join(spe_names_pp_use)%>%
  mutate(p_val=ifelse(line_type=="sig","p<0.05",
                      ifelse(line_type=="nonsig","p>0.05",NA)))
ggplot(survey_eff_regime_mean_yr_crossshore_pp_df) +
  geom_point(aes(x=Year,y=Pin,color=Season),size=1)+ 
  geom_line(aes(x=Year,y=value,color=Season,linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
            linewidth=1.5)+
  geom_ribbon(aes(x=Year,ymax=up_value,ymin=lo_value,fill=Season,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        plot.title=element_text(size=20,vjust=0),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+   
  scale_color_manual(values=c("blue","orange"))+
  scale_fill_manual(values=c("blue","orange"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ggtitle("Cross-shore Pin")+
  ylab("Proportion")+
  labs(
    linetype="p-value"
  )

survey_eff_regime_mean_yr_depth_pp_df<-survey_eff_regime_mean_yr_depth_df%>%
  left_join(spe_names_pp_use)%>%
  mutate(p_val=ifelse(line_type=="sig","p<0.05",
                      ifelse(line_type=="nonsig","p>0.05",NA)))

ggplot(survey_eff_regime_mean_yr_depth_pp_df) +
  geom_point(aes(x=Year,y=Pin,color=Season),size=1)+ 
  geom_line(aes(x=Year,y=value,color=Season,linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
            linewidth=1.5)+
  geom_ribbon(aes(x=Year,ymax=up_value,ymin=lo_value,fill=Season,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        plot.title=element_text(size=20,vjust=0),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text( 
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+   
  scale_color_manual(values=c("blue","orange"))+
  scale_fill_manual(values=c("blue","orange"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ggtitle("Depth Pin")+
  ylab("Proportion")+
  labs(
    linetype="p_value"
  )

fa_survey_eff_regime_mean_yr_alongshore_pp_df<-survey_eff_regime_mean_yr_alongshore_pp_df%>%
  dplyr::filter(Season=="Fall")%>%
  mutate(comm_name=factor(comm_name,levels=c("Am lobster","Jonah crab","Longfin sq","Shortfin sq",
                                             "Red hake","White hake","Silver hake","Atl herring",
                                             "Witch fl","Winter fl","Monkfish","Lh sculpin",
                                             "Am plaice","Aca redfish","Butterfish")))

ggplot(fa_survey_eff_regime_mean_yr_alongshore_pp_df) +
  geom_point(aes(x=Year,y=Pin),size=1)+ 
  geom_line(aes(x=Year,y=value,
                color=factor(p_val,levels=c("p<0.05","p>0.05"))),
            linewidth=1.5)+
  geom_ribbon(aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3)+
  facet_wrap(~comm_name,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        plot.title=element_text(size=20,vjust=0),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+   
  scale_color_manual(values=c("red","black"))+
  scale_linetype_manual(values=c("solid", "twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ggtitle("Fall Alongshore Pin")+
  ylab("Proportion")+
  labs(
    colour="p-value"
  )

ggplot(fa_survey_eff_regime_mean_yr_alongshore_pp_df) +
  geom_point(aes(x=Year,y=Pin),size=1)+ 
  geom_line(aes(x=Year,y=value,
                color=factor(p_val,levels=c("p<0.05","p>0.05")),
                linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
            linewidth=1.5)+
  geom_ribbon(aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3)+
  facet_wrap(~comm_name,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        plot.title=element_text(size=20,vjust=0),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+   
  scale_color_manual(values=c("red","black"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ggtitle("Fall Alongshore Pin")+
  ylab("Proportion")+
  labs(
    colour="p-value",
    linetype="p-value"
  )

sp_survey_eff_regime_mean_yr_alongshore_pp_df<-survey_eff_regime_mean_yr_alongshore_pp_df%>%
  dplyr::filter(Season=="Spring")%>%
  mutate(comm_name=factor(comm_name,levels=c("Am lobster","Jonah crab","Dichelo shr","N shr",
                                             "Red hake","White hake","Silver hake","Winter fl",
                                             "Alewife","Am plaice","Am shad" ,"Blu herring",
                                             "Lh sculpin")))

ggplot(sp_survey_eff_regime_mean_yr_alongshore_pp_df) +
  geom_point(aes(x=Year,y=Pin),size=1)+ 
  geom_line(aes(x=Year,y=value,
                color=factor(p_val,levels=c("p<0.05","p>0.05"))),
            linewidth=1.5)+
  geom_ribbon(aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3)+
  facet_wrap(~comm_name,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        plot.title=element_text(size=20,vjust=0),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+   
  scale_color_manual(values=c("red","black"))+
  scale_linetype_manual(values=c("solid", "twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ggtitle("Spring Alongshore Pin")+
  ylab("Proportion")+
  labs(
    colour="p-value"
  )

ggplot(sp_survey_eff_regime_mean_yr_alongshore_pp_df) +
  geom_point(aes(x=Year,y=Pin),size=1)+ 
  geom_line(aes(x=Year,y=value,
                color=factor(p_val,levels=c("p<0.05","p>0.05")),
                linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
            linewidth=1.5)+
  geom_ribbon(aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3)+
  facet_wrap(~comm_name,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        plot.title=element_text(size=20,vjust=0),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+   
  scale_color_manual(values=c("red","black"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ggtitle("Spring Alongshore Pin")+
  ylab("Proportion")+
  labs(
    colour="p-value",
    linetype="p-value"
  )

fa_survey_eff_regime_mean_yr_crossshore_pp_df<-survey_eff_regime_mean_yr_crossshore_pp_df%>%
  dplyr::filter(Season=="Fall")%>%
  mutate(comm_name=factor(comm_name,levels=c("Am lobster","Jonah crab","Dichelo shr","Montagui shr","Monkfish",
                                             "Wdp fl","Witch fl","Winter fl","Longfin sq","Shortfin sq",
                                             "Red hake","White hake","Silver hake","Atl herring","Am plaice",
                                             "Alewife","Aca redfish","Butterfish","Lh sculpin")))

ggplot(fa_survey_eff_regime_mean_yr_crossshore_pp_df) +
  geom_point(aes(x=Year,y=Pin),size=1)+ 
  geom_line(aes(x=Year,y=value,color=factor(p_val,levels=c("p<0.05","p>0.05"))),
            linewidth=1.5)+
  geom_ribbon(aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        plot.title=element_text(size=20,vjust=0),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+   
  scale_color_manual(values=c("red","black"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ggtitle("Fall Cross-shore Pin")+
  ylab("Proportion")+
  labs(
    colour="p-value"
  )

ggplot(fa_survey_eff_regime_mean_yr_crossshore_pp_df) +
  geom_point(aes(x=Year,y=Pin),size=1)+ 
  geom_line(aes(x=Year,y=value,color=factor(p_val,levels=c("p<0.05","p>0.05")),
                linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
            linewidth=1.5)+
  geom_ribbon(aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        plot.title=element_text(size=20,vjust=0),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size = 12),
        strip.text.y=element_text(size=16))+   
  scale_color_manual(values=c("red","black"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ggtitle("Fall Cross-shore Pin")+
  ylab("Proportion")+
  labs(
    colour="p-value",
    linetype="p-value"
  )

sp_survey_eff_regime_mean_yr_crossshore_pp_df<-survey_eff_regime_mean_yr_crossshore_pp_df%>%
  dplyr::filter(Season=="Spring")%>%
  mutate(comm_name=factor(comm_name,levels=c("Jonah crab","Dichelo shr","Montagui shr","N shr",
                                             "Red hake","Silver hake","White hake","Wdp fl",
                                             "Alewife","Am shad","Atl herring","Am plaice",
                                             "Lh sculpin")))

ggplot(sp_survey_eff_regime_mean_yr_crossshore_pp_df) +
  geom_point(aes(x=Year,y=Pin),size=1)+ 
  geom_line(aes(x=Year,y=value,color=factor(p_val,levels=c("p<0.05","p>0.05"))),
            linewidth=1.5)+
  geom_ribbon(aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        plot.title=element_text(size=20,vjust=0),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+   
  scale_color_manual(values=c("red","black"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ggtitle("Spring Cross-shore Pin")+
  ylab("Proportion")+
  labs(
    colour="p-value"
  )

ggplot(sp_survey_eff_regime_mean_yr_crossshore_pp_df) +
  geom_point(aes(x=Year,y=Pin),size=1)+ 
  geom_line(aes(x=Year,y=value,color=factor(p_val,levels=c("p<0.05","p>0.05")),
                linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
            linewidth=1.5)+
  geom_ribbon(aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        plot.title=element_text(size=20,vjust=0),
        axis.text.x=element_text(
          size = 16, 
          face = "plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+   
  scale_color_manual(values=c("red","black"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ggtitle("Spring Cross-shore Pin")+
  ylab("Proportion")+
  labs(
    colour="p-value",
    linetype="p-value"
  )

fa_survey_eff_regime_mean_yr_depth_pp_df<-survey_eff_regime_mean_yr_depth_pp_df%>%
  dplyr::filter(Season=="Fall")%>%
  mutate(comm_name=factor(comm_name,levels=c("Jonah crab","Shortfin sq","Witch fl",
                                             "Red hake","White hake","Silver hake",
                                             "Am plaice","Aca redfish","Monkfish")))

ggplot(fa_survey_eff_regime_mean_yr_depth_pp_df) +
  geom_point(aes(x=Year,y=Pin),size=1)+ 
  geom_line(aes(x=Year,y=value,color=factor(p_val,levels=c("p<0.05","p>0.05"))),
            linewidth=1.5)+
  geom_ribbon(aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        plot.title=element_text(size=20,vjust=0),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+   
  scale_color_manual(values=c("red","black"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ggtitle("Fall Depth Pin")+
  ylab("Proportion")+
  labs(
    colour ="p_value"
  )

ggplot(fa_survey_eff_regime_mean_yr_depth_pp_df) +
  geom_point(aes(x=Year,y=Pin),size=1)+ 
  geom_line(aes(x=Year,y=value,color=factor(p_val,levels=c("p<0.05","p>0.05")),
                linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
            linewidth=1.5)+
  geom_ribbon(aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        plot.title=element_text(size=20,vjust=0),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+   
  scale_color_manual(values=c("red","black"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ggtitle("Fall Depth Pin")+
  ylab("Proportion")+
  labs(
    colour="p_value",
    linetype="p_value"
  )

sp_survey_eff_regime_mean_yr_depth_pp_df<-survey_eff_regime_mean_yr_depth_pp_df%>%
  dplyr::filter(Season=="Spring")%>%
  mutate(comm_name=factor(comm_name,levels=c("Jonah crab","N shr","Am plaice",
                                             "Red hake","White hake","Silver hake")))
ggplot(sp_survey_eff_regime_mean_yr_depth_pp_df) +
  geom_point(aes(x=Year,y=Pin),size=1)+ 
  geom_line(aes(x=Year,y=value,color=factor(p_val,levels=c("p<0.05","p>0.05"))),
            linewidth=1.5)+
  geom_ribbon(aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        plot.title=element_text(size=20,vjust=0),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text( 
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+   
  scale_color_manual(values=c("red","black"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ggtitle("Spring Depth Pin")+
  ylab("Proportion")+
  labs(
    colour ="p_value"
  )

ggplot(sp_survey_eff_regime_mean_yr_depth_pp_df) +
  geom_point(aes(x=Year,y=Pin),size=1)+ 
  geom_line(aes(x=Year,y=value,color=factor(p_val,levels=c("p<0.05","p>0.05")),
                linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
            linewidth=1.5)+
  geom_ribbon(aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        plot.title=element_text(size=20,vjust=0),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+   
  scale_color_manual(values=c("red","black"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ggtitle("Spring Depth Pin")+
  ylab("Proportion")+
  labs(
    colour="p_value",
    linetype="p_value"
  )

ggplot(yr_biod_df_abund95SC_biom90_regime_mean_95CI_yr_df) +
  geom_point(aes(x=Year,y=yr_value),size=0.5,color="black")+ 
  geom_line(
    aes(x=Year,y=regime_mean,linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
    color="black",
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL), 
    fill="black",alpha=0.3) +
  facet_grid(biod_type~Season,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=14, 
          face="plain"),
        axis.text.y=element_text(
          size=14, 
          face="plain"),  
        axis.title.x=element_text(
          size=14, 
          face="plain"),
        axis.title.y=element_text(
          size=14, 
          face="plain"),
        strip.text.x=element_text(size=14),
        strip.text.y=element_text(size=14))+   
  scale_color_manual(name="",values=c("blue","orange","forestgreen"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ylab("")+
  labs(linetype="p-value")

ggplot(yr_biod_df_abund95SC_biom90_regime_mean_95CI_yr_df) +
  geom_point(aes(x=Year,y=yr_value),size=0.5,color="black")+ 
  geom_line(
    aes(x=Year,y=regime_mean,
        color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL), 
    fill="black",alpha=0.3) +
  facet_grid(biod_type~Season,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=14, 
          face="plain"),
        axis.text.y=element_text(
          size=14, 
          face="plain"),  
        axis.title.x=element_text(
          size=14, 
          face="plain"),
        axis.title.y=element_text(
          size=14, 
          face="plain"),
        strip.text.x=element_text(size=14),
        strip.text.y=element_text(size=14))+   
  scale_color_manual(name="p-value",values=c("red","black","forestgreen"))+
  scale_linetype_manual(values=c("solid", "twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ylab("")
biod_95SC_with_protocol_change_spe_p<-
  ggplot(yr_biod_df_abund95SC_biom90_regime_mean_95CI_yr_df) +
  geom_point(aes(x=Year,y=yr_value),size=0.5,color="black")+ 
  geom_line(
    aes(x=Year,y=regime_mean,
        color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL), 
    fill="black",alpha=0.3) +
  facet_grid(biod_type~Season,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=14, 
          face="plain"),
        axis.text.y=element_text(
          size=14, 
          face="plain"),  
        axis.title.x=element_text(
          size=14, 
          face="plain"),
        axis.title.y=element_text(
          size=14, 
          face="plain"),
        strip.text.x=element_text(size=14),
        strip.text.y=element_text(size=14))+   
  scale_color_manual(name="p-value",values=c("red","black","forestgreen"))+
  scale_linetype_manual(values=c("solid", "twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ggtitle("With protocol change species")+
  ylab("")

biod_95SC_without_protocol_change_spe_p<-
  ggplot(yr_biod_df_abund95SC_biom90_regime_mean_95CI_yr_df_speadj) +
  geom_point(aes(x=Year,y=yr_value),size=0.5,color="black")+ 
  geom_line(
    aes(x=Year,y=regime_mean,
        color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL), 
    fill="black",alpha=0.3) +
  facet_grid(biod_type~Season,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=14, 
          face="plain"),
        axis.text.y=element_text(
          size=14, 
          face="plain"),  
        axis.title.x=element_text(
          size=14, 
          face="plain"),
        axis.title.y=element_text(
          size=14, 
          face="plain"),
        strip.text.x=element_text(size=14),
        strip.text.y=element_text(size=14))+   
  scale_color_manual(name="p-value",values=c("red","black","forestgreen"))+
  scale_linetype_manual(values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ylab("")
biod_95SC_with_without_protocol_change_spe_p<-ggarrange(biod_95SC_with_protocol_change_spe_p,
                                                        biod_95SC_without_protocol_change_spe_p, 
                                                        labels=c("(a)","(b)"),
                                                        ncol=2,nrow=1)
biod_95SC_without_protocol_change_spe_p_lty<-
  ggplot(yr_biod_df_abund95SC_biom90_regime_mean_95CI_yr_df_speadj) +
  geom_point(aes(x=Year, y=yr_value),size=0.5,color="black")+ 
  geom_line(
    aes(x=Year,y=regime_mean,linetype=factor(p_val,levels=c("p<0.05","p>0.05")),
        color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL), 
    fill="black",alpha=0.3) +
  facet_grid(biod_type~Season,scales="free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=14, 
          face="plain"),
        axis.text.y=element_text(
          size=14, 
          face="plain"),  
        axis.title.x=element_text(
          size=14, 
          face="plain"),
        axis.title.y=element_text(
          size=14, 
          face="plain"),
        strip.text.x=element_text(size=14),
        strip.text.y=element_text(size=14))+   
  scale_color_manual(name="p-value",values=c("red","black","forestgreen"))+
  scale_linetype_manual(name="p-value",values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ylab("Diversity")
yr_biod_95CI_df_speadj<-yr_biod_df_abund_95_speadj%>%
  dplyr::rename(Est=qD,
                Lo=qD.LCL,
                Up=qD.UCL,
                Diversity=biod_type)%>%
  dplyr::select(Season,Diversity,Year,Est,Lo,Up)%>%
  arrange(Season,Diversity,Year)%>%
  right_join(expand.grid(Season=c("Fall","Spring"),
                         Year=seq(2003,2023,1),
                         Diversity=c("Species Richness","Hill-Shannon","Hill-Simpson","Catch Diversity")))
yr_biod_df_abund95SC_biom90_regime_mean_95CI_yr_df_speadj_add_ci<-yr_biod_df_abund95SC_biom90_regime_mean_95CI_yr_df_speadj%>%
  dplyr::rename(Diversity=biod_type)%>%
  full_join(yr_biod_95CI_df_speadj)
biod_95SC_without_protocol_change_spe_p_lty_add_ci<-
  ggplot(yr_biod_df_abund95SC_biom90_regime_mean_95CI_yr_df_speadj_add_ci) +
  geom_point(aes(x=Year,y=yr_value),size=0.5,color="black")+ 
  geom_line(
    aes(x=Year,y=regime_mean,linetype=factor(p_val,levels=c("p<0.05","p>0.05")),
        color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL), 
    fill="black",alpha=0.3) +
  geom_errorbar(aes(x=Year,ymin=Lo,ymax=Up),width=0,
                position=position_dodge(0.05))+
  facet_grid(Diversity~Season, scales = "free_y") +
  theme_bw()+
  theme(legend.position="top",
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=14, 
          face="plain"),
        axis.text.y=element_text(
          size=14, 
          face="plain"),  
        axis.title.x=element_text(
          size=14, 
          face="plain"),
        axis.title.y=element_text(
          size=14, 
          face="plain"),
        strip.text.x=element_text(size=14),
        strip.text.y=element_text(size=14))+   
  scale_color_manual(name="p-value",values=c("red","black","forestgreen"))+
  scale_linetype_manual(name="p-value",values=c("solid","twodash"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ylab("Diversity")

all_spe_sexComb_lobster_mean_length_cm_pp_df<-all_spe_sexComb_lobster_mean_sd_length_cm_df%>%
  dplyr::filter(variable=="Median")%>%
  left_join(spe_names_pp_use)%>%
  mutate(p_val=ifelse(line_type=="sig","p<0.05",
                      ifelse(line_type=="nonsig","p>0.05",NA)))

ggplot(all_spe_sexComb_lobster_mean_length_cm_pp_df) +
  geom_point(aes(color=Season,x=Year,y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=Season,linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value, ymin=regime_lo_value,fill=Season,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual(values=c("blue","orange"))+
  scale_fill_manual(values=c("blue","orange"))+
  scale_linetype_manual("p-value",values=c("p<0.05"=1,"p>0.05"=4))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    linetype="p-value"
  )+
  ggtitle("Median")+
  ylab("Length (cm)")

all_spe_sexComb_lobster_q025_length_cm_pp_df<-all_spe_sexComb_lobster_mean_sd_length_cm_df%>%
  dplyr::filter(variable=="Q2.5%")%>%
  left_join(spe_names_pp_use)%>%
  mutate(p_val=ifelse(line_type=="sig","p<0.05",
                      ifelse(line_type=="nonsig","p>0.05",NA)))


ggplot(all_spe_sexComb_lobster_q025_length_cm_pp_df) +
  geom_point(aes(color=Season,x=Year,y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=Season,linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value,ymin=regime_lo_value,fill=Season,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual(values=c("blue","orange"))+
  scale_fill_manual(values=c("blue","orange"))+
  scale_linetype_manual("p-value",values=c("p<0.05"=1,"p>0.05"=4))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    linetype="p-value"
  )+
  ggtitle("0.025 quantile")+
  ylab("Length (cm)")

all_spe_sexComb_lobster_q975_length_cm_pp_df<-all_spe_sexComb_lobster_mean_sd_length_cm_df%>%
  dplyr::filter(variable=="Q97.5%")%>%
  left_join(spe_names_pp_use)%>%
  mutate(p_val=ifelse(line_type=="sig","p<0.05",
                      ifelse(line_type=="nonsig","p>0.05",NA)))


ggplot(all_spe_sexComb_lobster_q975_length_cm_pp_df) +
  geom_point(aes(color=Season,x=Year,y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=Season,linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value, ymin=regime_lo_value,fill=Season,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual(values=c("blue","orange"))+
  scale_fill_manual(values=c("blue","orange"))+
  scale_linetype_manual("p-value",values=c("p<0.05"=1,"p>0.05"=4))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    linetype="p-value"
  )+
  ggtitle("0.975 quantile")+
  ylab("Length (cm)")

all_spe_sexComb_lobster_sd_length_cm_pp_df<-all_spe_sexComb_lobster_mean_sd_length_cm_df%>%
  dplyr::filter(variable=="SD")%>%
  left_join(spe_names_pp_use)%>%
  mutate(p_val=ifelse(line_type=="sig","p<0.05",
                      ifelse(line_type=="nonsig","p>0.05",NA)))


ggplot(all_spe_sexComb_lobster_sd_length_cm_pp_df) +
  geom_point(aes(color=Season,x=Year,y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=Season,linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value,ymin=regime_lo_value,fill=Season,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual(values=c("blue","orange"))+
  scale_fill_manual(values=c("blue","orange"))+
  scale_linetype_manual("p-value",values=c("p<0.05"=1,"p>0.05"=4))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    linetype = "p-value"
  )+
  ggtitle("Standard deviation")+
  ylab("Length (cm)")
fa_length_spe<-c("Am lobster","Jonah crab","Longfin sq","Shortfin sq","Monkfish",
                 "Wdp fl","Witch fl","Winter fl","Butterfish","Lh sculpin",
                 "Red hake","White hake","Silver hake","Aca redfish","Atl herring",
                 "Alewife","Am plaice")
fa_all_spe_sexComb_lobster_mean_length_cm_pp_df<-all_spe_sexComb_lobster_mean_length_cm_pp_df%>%
  dplyr::filter(Season=="Fall")%>%
  mutate(comm_name=factor(comm_name,levels=fa_length_spe))
ggplot(fa_all_spe_sexComb_lobster_mean_length_cm_pp_df) +
  geom_point(aes(x=Year,y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value,ymin=regime_lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual("p-value",values=c("red","black"))+
  scale_fill_manual(values=c("blue","orange"))+
  scale_linetype_manual(values=c("p<0.05"=1,"p>0.05"=4))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour = "p-value"
  )+
  ggtitle("Fall Median")+
  ylab("Length (cm)")

ggplot(fa_all_spe_sexComb_lobster_mean_length_cm_pp_df) +
  geom_point(aes(x=Year,y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=factor(p_val,levels=c("p<0.05","p>0.05")),
        linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value,ymin=regime_lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual("p-value",values=c("red", "black"))+
  scale_fill_manual(values=c("blue","orange"))+
  scale_linetype_manual(values=c("p<0.05"=1,"p>0.05"=4))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value",
    linetype="p-value"
  )+
  ggtitle("Fall Median")+
  ylab("Length (cm)")
sp_length_spe<-c("Am lobster","Jonah crab","Wdp fl","Winter fl",
                 "Red hake","White hake","Silver hake","Lh sculpin",
                 "Alewife","Am shad","Atl herring","Blu herring",
                 "Am plaice")
sp_all_spe_sexComb_lobster_mean_length_cm_pp_df<-all_spe_sexComb_lobster_mean_length_cm_pp_df%>%
  dplyr::filter(Season=="Spring")%>%
  mutate(comm_name=factor(comm_name,levels=sp_length_spe))
ggplot(sp_all_spe_sexComb_lobster_mean_length_cm_pp_df) +
  geom_point(aes(x=Year,y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value,ymin=regime_lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual("p-value",values=c("red","black"))+
  scale_fill_manual(values=c("blue","orange"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour = "p-value"
  )+
  ggtitle("Spring Median")+
  ylab("Length (cm)")
ggplot(sp_all_spe_sexComb_lobster_mean_length_cm_pp_df) +
  geom_point(aes(x=Year,y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=factor(p_val,levels=c("p<0.05","p>0.05")),
        linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value, ymin=regime_lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual("p-value",values=c("red", "black"))+
  scale_fill_manual(values=c("blue", "orange"))+
  scale_linetype_manual(values=c("p<0.05"=1,"p>0.05"=4))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value",
    linetype="p-value"
  )+
  ggtitle("Spring Median")+
  ylab("Length (cm)")
fa_all_spe_sexComb_lobster_q025_length_cm_pp_df<-all_spe_sexComb_lobster_q025_length_cm_pp_df%>%
  dplyr::filter(Season=="Fall")%>%
  mutate(comm_name=factor(comm_name,levels=fa_length_spe))
ggplot(fa_all_spe_sexComb_lobster_q025_length_cm_pp_df) +
  geom_point(aes(x=Year, y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value,ymin=regime_lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual(values=c("red","black"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value"
  )+
  ggtitle("Fall 0.025 quantile")+
  ylab("Length (cm)")
ggplot(fa_all_spe_sexComb_lobster_q025_length_cm_pp_df) +
  geom_point(aes(x=Year, y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=factor(p_val,levels=c("p<0.05","p>0.05")),
        linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value,ymin=regime_lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual(values=c("red","black"))+
  scale_linetype_manual("p-value",values=c("p<0.05"=1,"p>0.05"=4))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value",
    linetype="p-value"
  )+
  ggtitle("Fall 0.025 quantile")+
  ylab("Length (cm)")

sp_all_spe_sexComb_lobster_q025_length_cm_pp_df<-all_spe_sexComb_lobster_q025_length_cm_pp_df%>%
  dplyr::filter(Season=="Spring")%>%
  mutate(comm_name=factor(comm_name,levels=sp_length_spe))

ggplot(sp_all_spe_sexComb_lobster_q025_length_cm_pp_df) +
  geom_point(aes(x=Year, y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value,ymin=regime_lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size = 12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual(values=c("red","black"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value"
  )+
  ggtitle("Spring 0.025 quantile")+
  ylab("Length (cm)")
ggplot(sp_all_spe_sexComb_lobster_q025_length_cm_pp_df) +
  geom_point(aes(x=Year, y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=factor(p_val,levels=c("p<0.05","p>0.05")),
        linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value,ymin=regime_lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual(values=c("red","black"))+
  scale_linetype_manual("p-value",values=c("p<0.05"=1,"p>0.05"=4))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value",
    linetype="p-value"
  )+
  ggtitle("Spring 0.025 quantile")+
  ylab("Length (cm)")

fa_all_spe_sexComb_lobster_q975_length_cm_pp_df<-all_spe_sexComb_lobster_q975_length_cm_pp_df%>%
  dplyr::filter(Season=="Fall")%>%
  mutate(comm_name=factor(comm_name,levels=fa_length_spe))

ggplot(fa_all_spe_sexComb_lobster_q975_length_cm_pp_df) +
  geom_point(aes(x=Year,y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value,ymin=regime_lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual(values=c("red","black"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value"
  )+
  ggtitle("Fall 0.975 quantile")+
  ylab("Length (cm)")
ggplot(fa_all_spe_sexComb_lobster_q975_length_cm_pp_df) +
  geom_point(aes(x=Year,y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=factor(p_val,levels=c("p<0.05","p>0.05")),
        linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value,ymin=regime_lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual(values=c("red","black"))+
  scale_linetype_manual("p-value",values=c("p<0.05"=1,"p>0.05"=4))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value",
    linetype="p-value"
  )+
  ggtitle("Fall 0.975 quantile")+
  ylab("Length (cm)")

sp_all_spe_sexComb_lobster_q975_length_cm_pp_df<-all_spe_sexComb_lobster_q975_length_cm_pp_df%>%
  dplyr::filter(Season=="Spring")%>%
  mutate(comm_name=factor(comm_name,levels=sp_length_spe))

ggplot(sp_all_spe_sexComb_lobster_q975_length_cm_pp_df) +
  geom_point(aes(x=Year,y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value,ymin=regime_lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual(values=c("red", "black"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value"
  )+
  ggtitle("Spring 0.975 quantile")+
  ylab("Length (cm)")

ggplot(sp_all_spe_sexComb_lobster_q975_length_cm_pp_df) +
  geom_point(aes(x=Year,y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=factor(p_val,levels=c("p<0.05","p>0.05")),
        linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value,ymin=regime_lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual(values=c("red","black"))+
  scale_linetype_manual("p-value",values=c("p<0.05"=1,"p>0.05"=4))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value",
    linetype="p-value"
  )+
  ggtitle("Spring 0.975 quantile")+
  ylab("Length (cm)")

fa_all_spe_sexComb_lobster_sd_length_cm_pp_df<-all_spe_sexComb_lobster_sd_length_cm_pp_df%>%
  dplyr::filter(Season=="Fall")%>%
  mutate(comm_name=factor(comm_name,levels=fa_length_spe))

ggplot(fa_all_spe_sexComb_lobster_sd_length_cm_pp_df) +
  geom_point(aes(x=Year,y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value,ymin=regime_lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual(values=c("red","black"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value"
  )+
  ggtitle("Fall standard deviation")+
  ylab("Length (cm)")
ggplot(fa_all_spe_sexComb_lobster_sd_length_cm_pp_df) +
  geom_point(aes(x=Year, y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=factor(p_val,levels=c("p<0.05","p>0.05")),
        linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value,ymin=regime_lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual(values=c("red","black"))+
  scale_linetype_manual("p-value",values=c("p<0.05"=1,"p>0.05"=4))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value",
    linetype="p-value"
  )+
  ggtitle("Fall standard deviation")+
  ylab("Length (cm)")

sp_all_spe_sexComb_lobster_sd_length_cm_pp_df<-all_spe_sexComb_lobster_sd_length_cm_pp_df%>%
  dplyr::filter(Season=="Spring")%>%
  mutate(comm_name=factor(comm_name,levels=sp_length_spe))

ggplot(sp_all_spe_sexComb_lobster_sd_length_cm_pp_df) +
  geom_point(aes(x=Year,y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value,ymin=regime_lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual(values=c("red","black"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value"
  )+
  ggtitle("Spring standard deviation")+
  ylab("Length (cm)")

ggplot(sp_all_spe_sexComb_lobster_sd_length_cm_pp_df) +
  geom_point(aes(x=Year,y=yr_mean),size=1)+ 
  geom_line(
    aes(x=Year,y=regime_mean,color=factor(p_val,levels=c("p<0.05","p>0.05")),
        linetype=factor(p_val,levels=c("p<0.05","p>0.05"))),
    linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=regime_up_value,ymin=regime_lo_value,color=NULL),alpha=0.3) +
  facet_wrap(~comm_name,scales="free_y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=16))+  
  scale_color_manual(values=c("red","black"))+
  scale_linetype_manual("p-value",values=c("p<0.05"=1,"p>0.05"=4))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p-value",
    linetype="p-value"
  )+
  ggtitle("Spring standard deviation")+
  ylab("Length (cm)")

Fem_mat_pp_df<-Fem_mat_p_df%>%
  left_join(spe_names_pp_use)

ggplot(Fem_mat_pp_df,aes(x=Year,y=perc,group=Maturity_stage)) +
  geom_line()+
  geom_point()+ 
  ggh4x::facet_nested(Maturity_stage~Season+comm_name,scales="free_y",independent="y") +
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="none",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=14, 
          face="plain"),
        axis.text.y=element_text(
          size=14, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=12))+  
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  ylab("Proportion of maturity stage")

est_F_L50_regime_mean_yr_pp_df<-est_F_L50_regime_mean_yr_p_df%>%
  left_join(spe_names_pp_use)

ggplot(est_F_L50_regime_mean_yr_pp_df) +
  geom_point(aes(x=Year,y=est_glm_L50),size=1)+ 
  geom_line(
    aes(x=Year,y=value,color=line_type
    ),linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3) +
  geom_errorbar(aes(x=Year,ymin=est_glm_beta1_95ci_lo,ymax=est_glm_beta1_95ci_up),width=0,
                position=position_dodge(0.05))+
  ggh4x::facet_wrap2(~Season+comm_name,ncol=2,strip=ggh4x::strip_nested(bleed=TRUE),scales="free_y") +
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=18),
        strip.text.y=element_text(size=16))+  
  scale_color_manual("p-value",values=c("red","black","blue", "orange"))+
  scale_fill_manual(values=c("gray","blue", "orange"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    linetype="p_value"
  )+
  ggtitle("Length-at-maturity")+
  ylab("Length (cm)")

est_F_L50_regime_mean_yr_fcn_df<-est_F_L50_regime_mean_yr_pp_df%>%
  mutate(fcn=ifelse(comm_name=="Am plaice","American plaice",
                    ifelse(comm_name=="White hake","White hake",
                           ifelse(comm_name=="Winter fl","Winter flounder",
                                  ifelse(comm_name=="Witch fl","Witch flounder",NA)))))

ggplot(est_F_L50_regime_mean_yr_fcn_df) +
  geom_line(
    aes(x=Year,y=value,color=factor(line_type,levels=c("p<0.05","p>0.05")),
        linetype=factor(line_type,levels=c("p<0.05","p>0.05"))
    ),linewidth=1.2)+
  geom_ribbon(
    aes(x=Year,ymax=up_value,ymin=lo_value,color=NULL),alpha=0.3) +
  geom_point(aes(x=Year,y=est_glm_L50),size=1)+ 
  geom_errorbar(aes(x=Year,ymin=est_glm_beta1_95ci_lo,ymax=est_glm_beta1_95ci_up),width=0,
                position=position_dodge(0.05))+
  ggh4x::facet_wrap2(~Season+fcn,ncol=2,strip=ggh4x::strip_nested(bleed=TRUE),scales="free_y") +
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=18),
        strip.text.y=element_text(size=16))+  
  scale_color_manual("p-value",values=c("red","black","blue", "orange"))+
  scale_fill_manual(values=c("gray","blue", "orange"))+
  scale_linetype_manual("p-value",values=c("p<0.05"=1,"p>0.05"=4))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  labs(
    colour="p_value",
    linetype="p_value"
  )+
  ggtitle("Length-at-maturity")+
  ylab("Length (cm)")

est_F_L50_regime_mean_yr_fcn_95ci_df<-est_F_L50_regime_mean_yr_fcn_df%>%
  dplyr::select(Season,fcn,Year,est_glm_L50,est_glm_beta1_95ci_lo,est_glm_beta1_95ci_up)%>%
  dplyr::rename(Spe_name=fcn,
                Est=est_glm_L50,
                Lo=est_glm_beta1_95ci_lo,
                Up=est_glm_beta1_95ci_up)%>%
  arrange(Season,Spe_name,Year)%>%
  dplyr::filter(!is.na(Est))

length_mat_comb_pp_df<-length_mat_comb_df%>%
  left_join(spe_names_pp_use)

ggplot(length_mat_comb_pp_df,aes(x=factor(Year),y=length_cm,fill=data,color=data))+
  geom_boxplot(aes(fill=data,color=data),position=position_dodge(.9),outlier.size=0.8)+
  ggh4x::facet_wrap2(~Season+comm_name,ncol=2,strip=ggh4x::strip_nested(bleed=TRUE),scales="free_y") +
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=18),
        strip.text.y=element_text(size=16))+  
  scale_color_manual("",values=c("blue","darkorange"))+
  scale_fill_manual("",values=c("lightblue1","bisque1"))+
  scale_x_discrete(
    labels=c(rep("",7),2010,rep("",9),2020,rep("",3)))+
  xlab("Year")+
  ylab("Length (cm)")

length_mat_comb_fcn_df<-length_mat_comb_pp_df%>%
  mutate(fcn=ifelse(comm_name=="Am plaice","American plaice",
                    ifelse(comm_name=="White hake","White hake",
                           ifelse(comm_name=="Winter fl","Winter flounder",
                                  ifelse(comm_name=="Witch fl","Witch flounder",NA)))))%>%
  mutate(data_rn=ifelse(data=="Length (F+M)","Length of combined sexes (F+M)",data))

ggplot(length_mat_comb_fcn_df,aes(x=factor(Year),y=length_cm))+
  geom_boxplot(position=position_dodge(.9),outlier.size=0.8)+
  ggh4x::facet_nested(Season+fcn~data_rn,scales="free_y",independent="y")+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size=18),
        strip.text.y=element_text(size=16))+  
  scale_color_manual("",values=c("blue","darkorange"))+
  scale_fill_manual("",values=c("lightblue1","bisque1"))+
  scale_x_discrete(
    labels=c(rep("",7),2010,rep("",9),2020,rep("",3)))+
  xlab("Year")+
  ylab("Length (cm)")

levelplot(Expanded_Weight_kg~mid_long*mid_lat|Year,data=redfish_map_df,
          panel=panel.levelplot.points,cex=.4,
          col.regions=col.l ,
          col="transparent",
          as.table=TRUE ,
          colorkey=list(labels=list(cex=.5)),
          par.strip.text=list(cex=0.6),
          scales=list(x=list(cex=.5,tck=0.3),y=list(cex=.5,tck=0.3)), 
          xlab=list(label="Longitude", cex=.6), 
          ylab=list(label="Latitude", cex=.6), 
          main=list(label=paste("Fall Acadian redfish (positive CPUE)"),cex=.7)
)  + 
  latticeExtra::layer(sp.polygons(bPols,fill="lightgray",first=FALSE))
par(mgp=c(1.2,0.5,0),oma=c(0,0,0,0),mar=c(1,2,1,1))
boxplot(fa_redfish_2005_df$Expanded_Weight_kg,
        cex=0.5,
        cex.axis=0.6,
        cex.lab=0.7,
        ylab="CPUE")
samp_int_p_df<-samp_int%>%
  ungroup()%>%
  as.data.frame()%>%
  mutate(Region=paste("Region",Region,sep=" "),
         Depth_Stratum=paste("Stratum",Depth_Stratum,sep=" "))
ggplot(samp_int_p_df,aes(x=Year,y=n_samp,colour=Season)) + 
  geom_line() + 
  geom_point() +
  facet_grid(Depth_Stratum~Region)+
  theme_bw()+
  theme(plot.title=element_text(size=20),
        legend.position="top",
        legend.text=element_text(size=18),
        legend.title=element_text(size=16),
        axis.text.x=element_text(
          size=16, 
          face="plain"),
        axis.text.y=element_text(
          size=16, 
          face="plain"),  
        axis.title.x=element_text(
          size=16, 
          face="plain"),
        axis.title.y=element_text(
          size=16, 
          face="plain"),
        strip.text.x=element_text(size = 18),
        strip.text.y=element_text(size = 16))+  
  scale_color_manual("",values=c("blue","darkorange"))+
  scale_fill_manual("",values=c("lightblue1","bisque1"))+
  scale_x_continuous(breaks=c(2010,2020),
                     labels=c(2010,2020))+
  xlab("Year")+
  ylab("Number of tows")+
  ggtitle("Maine-New Hampshire Bottom Trawl Survey")


