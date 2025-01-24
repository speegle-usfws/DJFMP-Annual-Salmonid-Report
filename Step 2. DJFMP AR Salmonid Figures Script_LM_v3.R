# DJFMP Annual Report- Salmonid CPUE figures

### Author- Ryan Mckenzie
### Contact- ryan_mckenzie@fws.gov
### Date- 12/1/21
### R version- 4.0.5

#Enter water year for Current Report
fieldyear <- 2024

#Enter number of start and stop month of Sac Seine
#Aug=1, Sept=2, Oct=3, Nov=4, Dec=5, Jan=6, Feb=7, Mar=8, Apr=9, May=10, Jun=11, Jul=12
#Example: Sac seine started on Oct 1 and ended on Jan 31. So SacSeineStart = 3 and SacSeineStop =6
SacSeineStart <- 3
SacSeineStop <- 6

this_out_root <- file.path("Data",fieldyear)


# Working Environment-------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(extrafont)
library(gridExtra)
library(grid)
library(patternplot)
library(ggpattern)
library(patchwork)

#Import window fonts. Only needed one time for R. Takes a few minutes
# library("extrafont")
# font_import()
# loadfonts(device="win")
# fonts()


## Read in Data ##################################################

CHNcustom_Trawl_Monthly_Means_CurrentYear <- read.csv(
  file.path(this_out_root,"CHNcustom_Trawl_Monthly_Means_CurrentYear.csv"), na.strings="")
CHNcustom_SEIN_Monthly_Means_CurrentYear <- read.csv(
  file.path(this_out_root,"CHNcustom_SEIN_Monthly_Means_CurrentYear.csv"), na.strings="")

CHNcustom_Trawl_Annual_Means <- read.csv(
  file.path(this_out_root,"CHNcustom_Trawl_Annual_Means.csv"), na.strings="")
CHNcustom_SEIN_Annual_Means <- read.csv(
  file.path(this_out_root,"CHNcustom_SEIN_Annual_Means.csv"), na.strings="")

RBT_Trawl_Monthly_Means_CurrentYear <- read.csv(
  file.path(this_out_root,"RBT_Trawl_Monthly_Means_CurrentYear.csv"), na.strings="")
RBT_SEIN_Monthly_Means_CurrentYear <- read.csv(
  file.path(this_out_root,"RBT_SEIN_Monthly_Means_CurrentYear.csv"), na.strings="")

RBT_Trawl_Annual_Means <- read.csv(
  file.path(this_out_root,"RBT_Trawl_Annual_Means.csv"), na.strings="")
RBT_SEIN_Annual_Means <- read.csv(
  file.path(this_out_root,"RBT_SEIN_Annual_Means.csv"), na.strings="")

CHN_Chipps_FL <- read.csv(file.path(this_out_root,"CHN_Chipps_FL.csv"), na.strings="")
RBT_Chipps_FL <- read.csv(file.path(this_out_root,"RBT_Chipps_FL.csv"), na.strings="")


## Double check that monthly means are for the selected field year:
fieldyear_check <- c(CHNcustom_Trawl_Monthly_Means_CurrentYear$FieldYear,
                     CHNcustom_SEIN_Monthly_Means_CurrentYear$FieldYear,
                     RBT_Trawl_Monthly_Means_CurrentYear$FieldYear,
                     RBT_SEIN_Monthly_Means_CurrentYear$FieldYear)

if(any(is.na(fieldyear_check)) || any(fieldyear_check != fieldyear)) {
  stop("Problem with monthly mean data.\n")
  ## Force user to stop:
  rm(ls())
}



## Combine all monthly data across species (CHN, RBT) and method (seine, trawl):

Trawl_Monthly_Means_CurrentYear <- dplyr::bind_rows(
    CHNcustom_Trawl_Monthly_Means_CurrentYear, 
    RBT_Trawl_Monthly_Means_CurrentYear) %>%
  dplyr:::mutate(Location=description) %>%
  dplyr::select(FieldYear, MONTH, Location, OrganismCode, CustomRaceGroup, 
                Origin, AvgCPUE)

SEIN_Monthly_Means_CurrentYear <- dplyr::bind_rows(
    CHNcustom_SEIN_Monthly_Means_CurrentYear,
    RBT_SEIN_Monthly_Means_CurrentYear) %>%
  dplyr::mutate(Location=paste0("region_",RegionCode)) %>%
  dplyr::select(FieldYear, MONTH, Location, OrganismCode, CustomRaceGroup, 
                Origin, AvgCPUE)

all_Monthly_Means_CurrentYear <- dplyr::bind_rows(Trawl_Monthly_Means_CurrentYear,
                                                  SEIN_Monthly_Means_CurrentYear)


## And similarly for annual data:

Trawl_Annual_Means <- dplyr::bind_rows(CHNcustom_Trawl_Annual_Means, 
                                       RBT_Trawl_Annual_Means) %>%
  dplyr:::mutate(Location=description) %>%
  dplyr::select(FieldYear, Location, OrganismCode, CustomRaceGroup, 
                Origin, AvgCPUE)

SEIN_Annual_Means <- dplyr::bind_rows(CHNcustom_SEIN_Annual_Means,
                                      RBT_SEIN_Annual_Means) %>%
  dplyr::mutate(Location=paste0("region_",RegionCode)) %>%
  dplyr::select(FieldYear, Location, OrganismCode, CustomRaceGroup, 
                Origin, AvgCPUE)

all_Annual_Means <- dplyr::bind_rows(Trawl_Annual_Means,
                                     SEIN_Annual_Means)



## Clean up labels for plotting:

formatted_origin_map <- c("Marked_Total"="Marked Hatchery",
                          "Marked_Count"="Marked Hatchery", 
                          "Unmarked_Total"="Unmarked Hatchery",
                          "Wild"="Wild",
                          "Unknown"="Unknown")

formatted_location_method_map <- c(
  "region_1"="Lower Sacramento Seine\n(Region 1)",
  "region_7"="Delta Entrance Seine\n(Region 7)", 
  "Sherwood Harbor"="Delta Entrance Trawl\n(Sherwood Harbor)",
  
  "region_5"="Delta Entrance Seine\n(Region 5)", 
  "Mossdale"="Delta Entrance Trawl\n(Mossdale)", 
  
  "region_2"="North Delta Seine\n(Region 2)", 
  "region_3"="Central Delta Seine\n(Region 3)", 
  "region_4"="South Delta Seine\n(Region 4)", 
  
  "Chipps Island"="Delta Exit Trawl\n(Chipps Island)",
  "region_6"="Bay Seine\n(Region 6)")

area_map <- c("region_1"="Sac",
              "region_7"="Sac",
              "Sherwood Harbor"="Sac",
              
              "region_5"="SJ",
              "Mossdale"="SJ",
              
              "region_2"="Delta", 
              "region_3"="Delta", 
              "region_4"="Delta", 
              
              "Chipps Island"="Chipps_Bay",
              "region_6"="Chipps_Bay")


all_Monthly_Means_CurrentYear <- all_Monthly_Means_CurrentYear %>%
  dplyr::mutate(Species=paste0(OrganismCode, 
                               ifelse(is.na(CustomRaceGroup), "", 
                                      paste0("_",CustomRaceGroup))),
                Location_and_method=formatted_location_method_map[Location], 
                Location_and_method_f=factor(Location_and_method,
                                             levels=formatted_location_method_map,
                                             ordered=TRUE),
                Area=area_map[Location], 
                Origin_f=factor(formatted_origin_map[Origin],
                                levels=unique(formatted_origin_map),
                                ordered=TRUE)) %>%
  dplyr::mutate(FieldMonth_f=factor(MONTH, 
                                    levels=c("Aug","Sep","Oct","Nov","Dec","Jan",
                                             "Feb","Mar","Apr","May","Jun","Jul"),
                                    ordered=TRUE))


all_Annual_Means <- all_Annual_Means %>%
  dplyr::mutate(Species=paste0(OrganismCode, 
                               ifelse(is.na(CustomRaceGroup), "", 
                                      paste0("_",CustomRaceGroup))),
                Location_and_method=formatted_location_method_map[Location], 
                Location_and_method_f=factor(Location_and_method,
                                             levels=formatted_location_method_map,
                                             ordered=TRUE),
                Area=area_map[Location], 
                Origin_f=factor(formatted_origin_map[Origin],
                                levels=unique(formatted_origin_map),
                                ordered=TRUE))


#### Set up:

custom_report_theme <- theme_bw() + 
  theme(
    plot.background=element_rect(fill="white", color=NA), # bg of the plot
    panel.background=element_rect(fill="transparent"), # bg of the panel
    text=element_text(size=13, colour="#000000"),
    axis.title.x=element_blank(), 
    axis.line=element_line(colour="black"),
    axis.text=element_text(colour="black"),
    legend.position="bottom",
    legend.title=element_blank(),
    legend.key.size=unit(0.3, 'in'), 
    legend.margin=margin(0, 0, 0, 0), 
    plot.title=element_text(size=12, hjust=0.5, face="bold")
  )

use_colors <- c("Marked Hatchery"="black", "Unmarked Hatchery"="#E69F00",
                "Wild"="#56B4E9", "Unknown"="#F0E442") 
use_shapes <- c("Marked Hatchery"=21, "Unmarked Hatchery"=22,
                "Wild"=24, "Unknown"=25)


# Figure 2. Timing of Salmonids Upstream of Delta Cross Channel -------------------------------------------------------------

create_monthly_plot <- function(dat, area) {
  ## Used globally: use_colors, use_shapes
  
  stopifnot(area %in% c("Sac","SJ","Delta","Chipps_Bay"))
  
  ## Double check this because I had a typo in the names before:
  stopifnot(all(unique(dat$Origin_f) %in% names(use_colors)))
  stopifnot(all(unique(dat$Origin_f) %in% names(use_shapes)))
  
  area_dat <- dat %>%
    dplyr::filter(Area == area) %>%
    dplyr::mutate(AvgCPUE_per_10000=AvgCPUE*10000)

  p_winter_run <- ggplot(subset(area_dat, Species == "CHN_Winter"),
                         aes(x=FieldMonth_f, y=AvgCPUE_per_10000, col=Origin_f, 
                             fill=Origin_f, pch=Origin_f, group=Origin_f)) +
    geom_point(show.legend=TRUE) +
    geom_line(show.legend=TRUE) + 
    facet_wrap(~ Location_and_method_f, scales="free_y", dir="v", ncol=1) +
    custom_report_theme + 
    theme(axis.title.y=element_blank()) +
    ggtitle("Chinook\nWinter Run")

  p_spring_run <- ggplot(subset(area_dat, Species == "CHN_Spring"),
                         aes(x=FieldMonth_f, y=AvgCPUE_per_10000, col=Origin_f, 
                             fill=Origin_f, pch=Origin_f, group=Origin_f)) +
    geom_point(show.legend=TRUE) +
    geom_line(show.legend=TRUE) +
    facet_wrap(~ Location_and_method_f, scales="free_y", dir="v", ncol=1) +
    custom_report_theme +
    theme(axis.title.y=element_blank()) +
    ggtitle("Chinook\nSpring Run")

  p_not_springwinter_run <- ggplot(subset(area_dat, Species == "CHN_Not_SpringWinter"),
                                   aes(x=FieldMonth_f, y=AvgCPUE_per_10000, col=Origin_f,
                                       fill=Origin_f, pch=Origin_f, group=Origin_f)) +
    geom_point(show.legend=TRUE) +
    geom_line(show.legend=TRUE) +
    facet_wrap(~ Location_and_method_f, scales="free_y", dir="v", ncol=1) +
    custom_report_theme +
    theme(axis.title.y=element_blank()) +
    ggtitle("Chinook\nFall - Late Fall Run")

  p_rbt <- ggplot(subset(area_dat, Species == "RBT"),
                  aes(x=FieldMonth_f, y=AvgCPUE_per_10000, col=Origin_f, 
                      fill=Origin_f, pch=Origin_f, group=Origin_f)) +
    geom_point(show.legend=TRUE) +
    geom_line(show.legend=TRUE) +
    facet_wrap(~ Location_and_method_f, scales="free_y", dir="v", ncol=1) +
    custom_report_theme +
    theme(axis.title.y=element_blank()) +
    ggtitle("Steelhead")

  ## Combine plots into one figure:
  if(area %in% c("Sac","Delta","Chipps_Bay")) {
    
    p_final <- p_winter_run + 
      theme(axis.title.y=element_text(angle=90, hjust=0.5)) +
      labs(y=expression(paste("CPUE (Fish"/"10,000 m"^"3",")"))) +
      p_spring_run + p_not_springwinter_run + p_rbt

  } else if(area == "SJ") {

    p_final <- p_spring_run + 
      theme(axis.title.y=element_text(angle=90, hjust=0.5)) +
      labs(y=expression(paste("CPUE (Fish"/"10,000 m"^"3",")"))) +
      p_not_springwinter_run + p_rbt
    
  }

  p_final <- p_final +
    patchwork::plot_layout(nrow=1, byrow=TRUE, guides="collect",
                           axis_titles="collect") & 
    theme(legend.position="bottom") &
    scale_color_manual(values=use_colors, drop=FALSE) &
    scale_fill_manual(values=use_colors, drop=FALSE) &
    scale_shape_manual(values=use_shapes, drop=FALSE) &
    #xlab("Month in field year") &
    scale_x_discrete(limits=levels(area_dat$FieldMonth_f),
                     labels=c("Aug"="Aug", "Sep"="", "Oct"="Oct", "Nov"="",
                              "Dec"="Dec", "Jan"="", "Feb"="Feb", "Mar"="",
                              "Apr"="Apr", "May"="", "Jun"="Jun", "Jul"="")) & 
    scale_y_continuous(labels=function(x) {
      if(min(x, na.rm=TRUE) == -0.05 && max(x, na.rm=TRUE) == 0.05) {
        x[x != 0] <- ""
      }
      return(x)
    })
  
  print(p_final)

  return(p_final)
}

p_monthly_Sac <- create_monthly_plot(dat=all_Monthly_Means_CurrentYear, area="Sac")
p_monthly_SJ <- create_monthly_plot(dat=all_Monthly_Means_CurrentYear, area="SJ")
p_monthly_Delta <- create_monthly_plot(dat=all_Monthly_Means_CurrentYear, area="Delta")
p_monthly_ChippsBay <- create_monthly_plot(dat=all_Monthly_Means_CurrentYear, 
                                           area="Chipps_Bay")



create_annual_plot <- function(dat, area) {
  ## Used globally: use_colors, use_shapes
  
  stopifnot(area %in% c("Delta","Chipps_Bay"))
  
  stopifnot(all(unique(dat$Origin_f) %in% names(use_colors)))
  stopifnot(all(unique(dat$Origin_f) %in% names(use_shapes)))
  
  area_dat <- dat %>%
    dplyr::filter(Area == area) %>%
    dplyr::mutate(AvgCPUE_per_10000=AvgCPUE*10000)
  
  p_winter_run <- ggplot(subset(area_dat, Species == "CHN_Winter"),
                         aes(x=FieldYear, y=AvgCPUE_per_10000, col=Origin_f, 
                             fill=Origin_f, pch=Origin_f, group=Origin_f)) +
    geom_point(show.legend=TRUE) +
    geom_line(show.legend=TRUE) + 
    facet_wrap(~ Location_and_method_f, scales="free_y", dir="v", ncol=1) +
    custom_report_theme + 
    labs(y=expression(paste("CPUE (Fish"/"10,000 m"^"3",")"))) +
    ggtitle("Chinook\nWinter Run")
  
  p_spring_run <- ggplot(subset(area_dat, Species == "CHN_Spring"),
                         aes(x=FieldYear, y=AvgCPUE_per_10000, col=Origin_f, 
                             fill=Origin_f, pch=Origin_f, group=Origin_f)) +
    geom_point(show.legend=TRUE) +
    geom_line(show.legend=TRUE) +
    facet_wrap(~ Location_and_method_f, scales="free_y", dir="v", ncol=1) +
    custom_report_theme +
    theme(axis.title.y=element_blank()) +
    ggtitle("Chinook\nSpring Run")
  
  p_not_springwinter_run <- ggplot(subset(area_dat, Species == "CHN_Not_SpringWinter"),
                                   aes(x=FieldYear, y=AvgCPUE_per_10000, col=Origin_f,
                                       fill=Origin_f, pch=Origin_f, group=Origin_f)) +
    geom_point(show.legend=TRUE) +
    geom_line(show.legend=TRUE) +
    facet_wrap(~ Location_and_method_f, scales="free_y", dir="v", ncol=1) +
    custom_report_theme +
    theme(axis.title.y=element_blank()) +
    ggtitle("Chinook\nFall - Late Fall Run")
  
  p_rbt <- ggplot(subset(area_dat, Species == "RBT"),
                  aes(x=FieldYear, y=AvgCPUE_per_10000, col=Origin_f, fill=Origin_f,
                      pch=Origin_f, group=Origin_f)) +
    geom_point(show.legend=TRUE) +
    geom_line(show.legend=TRUE) +
    facet_wrap(~ Location_and_method_f, scales="free_y", dir="v", ncol=1) +
    custom_report_theme +
    theme(axis.title.y=element_blank()) +
    ggtitle("Steelhead")
  
  ## Combine plots into one figure:
  p_final <- p_winter_run + p_spring_run + p_not_springwinter_run + p_rbt +
    patchwork::plot_layout(nrow=1, byrow=TRUE, guides="collect",
                           axis_titles="collect") &
    theme(legend.position="bottom") &
    scale_color_manual(values=use_colors, drop=FALSE) &
    scale_fill_manual(values=use_colors, drop=FALSE) &
    scale_shape_manual(values=use_shapes, drop=FALSE) &
    scale_y_continuous(labels=function(x) {
      if(min(x, na.rm=TRUE) == -0.05 && max(x, na.rm=TRUE) == 0.05) {
        x[x != 0] <- ""
      }
      return(x)
    }) & 
    scale_x_continuous(breaks=c(2000, 2010, 2020))
  
  print(p_final)
  
  return(p_final)
}

p_annual_Delta <- create_annual_plot(dat=all_Annual_Means, area="Delta")
p_annual_ChippsBay <- create_annual_plot(dat=all_Annual_Means, area="Chipps_Bay")




# Figure 3. Delta Cross Channel Operations------------------------
# We manually create the Delta Cross Channel .CSV file using the Historical Cross 
# Channel log provided on the USBR website
# https://www.usbr.gov/mp/cvo/vungvari/Ccgates.pdf

# File with daily log:
DCC <- read.csv(paste0(this_out_root,"/","DCC",fieldyear,".csv"), header=TRUE)

DCC$Date <- as.Date(DCC$Date, format="%m/%d/%Y")
DCC$Status <- as.factor(DCC$Status)

# Manipulate log data:
DCC <- DCC %>%
  separate(Date, c("YEAR","MONTH","DAY")) %>%
  mutate(MONTH=as.factor(case_when(MONTH =="08"~"Aug", MONTH =="09"~"Sep", 
                                   MONTH =="10"~"Oct", MONTH =="11"~"Nov", 
                                   MONTH =="12"~"Dec", MONTH =="01"~"Jan", 
                                   MONTH =="02"~"Feb", MONTH =="03"~"Mar", 
                                   MONTH =="04"~"Apr",MONTH =="05"~"May", 
                                   MONTH =="06"~"Jun", MONTH =="07"~"Jul"))) %>%
  mutate(Count=1) %>%
  group_by(MONTH, Status) %>%
  # transmute(Count=sum(Count)) %>%
  summarize(Count=sum(Count), 
            .groups="drop") %>%
  complete(nesting(MONTH), Status, fill=list(Count=0)) %>%
  unique() %>%
  spread(Status, Count, fill=NA) %>%
  as.data.frame() %>%
  mutate(Percent_Close=100*(closed/(open+closed))) %>%
  dplyr::mutate(Month_f=factor(MONTH, levels=c("Aug","Sep","Oct","Nov","Dec","Jan", 
                                               "Feb","Mar","Apr","May","Jun","Jul")))

p_DCC <- ggplot(DCC, aes(x=Month_f, y=Percent_Close)) + 
  geom_bar(stat="identity", colour="black", linewidth=0.5, na.rm = TRUE) + # Thin Black Outlines
  scale_fill_manual(values=c("#000000"), labels=c("Closed")) + #bar colors
  scale_y_continuous(expand=c(0, 0)) + #y axis scale
  scale_x_discrete(labels=c("Aug","","Oct","","Dec","","Feb","","Apr","","Jun","")) + 
  ylab("Percentage of Days Closed") + 
  custom_report_theme + 
  ## Show the y-axis label:
  theme(axis.title.y=element_text(hjust=0.5, vjust=1.5, size=8, family="Arial", 
                                  colour="#000000"))



## Save figures:

png(paste0("Data\\",fieldyear,"\\","Fig 2.Sac Delta Entry.png"), units="in", 
    width=9.2, height=6, res=500)
p_monthly_Sac
dev.off()


png(paste0("Data\\",fieldyear,"\\","Fig 3.Delta Cross Channel.png"), units="in", 
    width=3.5, height=2.5, res=500)
p_DCC
dev.off()


png(paste0("Data\\",fieldyear,"\\","Fig 4.SJ Delta Entry.png"), units="in", 
    width=7, height=4.4, res=500)
p_monthly_SJ
dev.off()


png(paste0("Data\\",fieldyear,"\\","Fig 5.Delta Seines.png"), units="in", 
    width=9, height=6, res=500)
p_monthly_Delta
dev.off()


png(paste0("Data\\",fieldyear,"\\","Fig 6.Annual Delta.png"), units="in", 
    width=9.5, height=6, res=500)
p_annual_Delta
dev.off()


png(paste0("Data\\",fieldyear,"\\","Fig 7.Delta Exit.png"), units="in", 
    width=9, height=5, res=500)
p_monthly_ChippsBay
dev.off()


png(paste0("Data\\",fieldyear,"\\","Fig 8.Annual Delta Exit.png"), units="in", 
    width=9.5, height=5, res=500)
p_annual_ChippsBay
dev.off()







# # Not Used : Figure 9. Annual FL Distribution of Juveniles leaving the Delta--------------
# # I used these kind of plots in the 2018 report, but they are not very informative, so I don't think we should present them.
# 
# unmarked_CHN <- subset(CHN_Chipps_FL, MarkCode=="None") %>%
# 	ggplot(aes(x=ForkLength, y=reorder(as.factor(FieldYear), desc(as.factor(FieldYear))), 
# 						 fill=MarkCode)) + 
#   geom_density_ridges(alpha=0.6, scale=0.7, quantile_lines=TRUE, quantiles=2, 
# 											vline_size=1.1) + 
#   scale_fill_manual(values=c("dark grey"), labels=c("All Races")) + #colors
#   scale_y_discrete(expand=c(0, 0)) + #y axis scale
#   ggtitle("Unmarked Chinook Delta Emigration") + 
#   xlab("Fork Length (mm)") + 
#   custom_report_theme + 
# 	theme(text=element_text(size=18),
# 				plot.title=element_text(hjust=0.5, size=16, family="Arial", colour="#000000"),
# 				axis.title.x=element_text(size=16, family="Arial", colour="#000000"))
# #unmarked_CHN
# 
# unmarked_RBT <- subset(RBT_Chipps_FL, MarkCode=="None") %>%
# 	ggplot(aes(x=ForkLength, y=reorder(as.factor(FieldYear),desc(as.factor(FieldYear))), 
# 						 fill=MarkCode)) + 
#   geom_density_ridges(alpha=0.6, scale=0.7, quantile_lines=TRUE, quantiles=2, 
# 											vline_size=1.1) + 
#   scale_fill_manual(values=c("dark grey"), labels=c("Unmarked")) + #colors
#   scale_y_discrete(expand=c(0, 0)) + #y axis scale
#   ggtitle("Unmarked Steelhead Delta Emigration") + 
#   xlab("Fork Length (mm)") + 
#   custom_report_theme + 
# 	theme(text=element_text(size=18),
# 				plot.title=element_text(hjust=0.5, size=16, family="Arial", colour="#000000"),
# 				axis.title.x=element_text(size=16, family="Arial", colour="#000000"))
# #unmarked_RBT
# 
# 
# ## Common Legend
# legendFL<-get_legend(unmarked_CHN)
# legendFL2<-get_legend(unmarked_RBT)
# 
# png(paste0("Data\\",fieldyear,"\\","Fig 9.Delta Exit Forklength.png"), units="in", 
# 		width=12, height=7.5, res=300)
# CHN_FL_deltexit<-grid.arrange(arrangeGrob(unmarked_CHN+theme(legend.position='hidden'), 
#                               unmarked_RBT+theme(legend.position='hidden'), 
#                               layout_matrix=matrix(c(1,2), ncol=2)),
#                               nrow=1)
# dev.off()

