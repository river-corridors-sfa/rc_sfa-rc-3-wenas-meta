# SI Figure 3: predictor competition and held-out contribution.
# Uses saved omission fits only; does not fit or tune models.
library(tidyverse)
library(here)
library(patchwork)
root <- here("agent_workflows","vibe_coding")
inputs <- file.path(root,"output/paper/predictor_omission")
out <- file.path(root,"output/paper/figures/si_figure_3_predictor_omission")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
summary <- read_csv(file.path(inputs,"paired_omission_summary.csv"),show_col_types=FALSE)
stability <- read_csv(file.path(inputs,"selection_stability.csv"),show_col_types=FALSE)
specs <- read_csv(file.path(inputs,"specifications.csv"),show_col_types=FALSE)
boot <- read_csv(file.path(inputs,"bootstrap_summary.csv"),show_col_types=FALSE)
config <- read_csv(file.path(root,"config/issue6_revised_primary_predictors.csv"),show_col_types=FALSE)
spec_order <- c("full","omit_burned_percent","omit_high_severity","omit_both_burn",
                "omit_runoff","omit_baseflow","omit_both_hydrology","omit_watershed_area",
                "omit_forest_cover","omit_soil","omit_time","omit_all_watershed")
spec_labels <- specs %>% filter(response_var=="DOC") %>% select(specification,label)
ink <- "#222222"
palette <- c(Nitrate="#4386B5",DOC="#D49B47")
theme_si <- theme_classic(base_size=11) + theme(
  text=element_text(colour=ink),axis.text=element_text(colour=ink,size=9),
  axis.title=element_text(colour=ink,size=10),axis.line=element_blank(),
  axis.ticks=element_line(colour=ink,linewidth=.4),
  panel.border=element_rect(colour=ink,fill=NA,linewidth=.45),
  strip.background=element_rect(fill="white",colour=ink,linewidth=.45),
  strip.text=element_text(colour=ink,size=11),plot.tag=element_text(size=12,face="bold"),
  legend.position="bottom")
summary <- summary %>% left_join(spec_labels,by="specification") %>% mutate(
  analyte=factor(response_var,levels=c("NO3","DOC"),labels=c("Nitrate","DOC")),
  specification=factor(specification,levels=rev(spec_order[-1])))
delta_plot <- ggplot(summary,aes(y=specification,colour=analyte)) +
  geom_vline(xintercept=0,linetype="dashed",colour="#707070",linewidth=.4) +
  geom_segment(aes(x=lower_delta_RMSE,xend=upper_delta_RMSE,yend=specification),linewidth=.8,
               position=position_nudge(y=-.10)) +
  geom_point(aes(x=median_delta_RMSE),size=2.4,position=position_nudge(y=-.10)) +
  geom_point(aes(x=delta_RMSE),shape=18,size=2.6,colour=ink,position=position_nudge(y=.13)) +
  facet_wrap(~analyte,nrow=1) + scale_colour_manual(values=palette,guide="none") +
  scale_y_discrete(labels=setNames(spec_labels$label,spec_labels$specification)) +
  labs(x="Change in held-out RMSE after omission (lnRR units)",y=NULL,tag="A") + theme_si

stability <- stability %>% left_join(config,by=c("response_var","predictor")) %>%
  mutate(analyte=factor(response_var,levels=c("NO3","DOC"),labels=c("Nitrate","DOC")),
    specification=factor(specification,levels=rev(spec_order)),
    display=if_else(available,sprintf("%.0f",100*selection_frequency),"×"))
stopifnot(!anyNA(stability$position),all(stability$selection_frequency[stability$available]>=0),
          all(stability$selection_frequency[stability$available]<=1),
          all(is.na(stability$selection_frequency[!stability$available])))
predictor_labels <- c("Post-fire\nyear","Burned\narea (%)","High-severity\nburn","Watershed\narea",
                      "Runoff","Baseflow\nindex","Forest\ncover","Soil\nproperty")
heatmap <- ggplot(stability,aes(x=position,y=specification,fill=selection_frequency)) +
  geom_tile(colour="white",linewidth=.7) +
  geom_text(aes(label=display,colour=available & selection_frequency>.55),size=2.8) +
  scale_colour_manual(values=c("FALSE"=ink,"TRUE"="white"),guide="none") +
  scale_fill_gradient(low="#F3F5F6",high="#264D65",limits=c(0,1),na.value="#D9D9D9",
    breaks=c(0,.25,.5,.75,1),labels=scales::label_percent(),name="Selection frequency",
    guide=guide_colourbar(barwidth=unit(65,"mm"),barheight=unit(3,"mm"))) +
  scale_x_continuous(breaks=1:8,labels=predictor_labels,expand=expansion(mult=0,add=.5)) +
  scale_y_discrete(labels=setNames(spec_labels$label,spec_labels$specification),expand=expansion(mult=0,add=.5)) +
  facet_wrap(~analyte,nrow=1) + labs(x=NULL,y=NULL,tag="B") + theme_si +
  theme(axis.ticks=element_blank(),axis.text.x=element_text(size=8),legend.title=element_text(size=9))
figure <- (delta_plot / heatmap) + plot_layout(heights=c(1,1.25))
ggsave(file.path(out,"si_figure_3_predictor_omission.png"),figure,width=14,height=13,dpi=300,bg="white")
ggsave(file.path(out,"si_figure_3_predictor_omission.pdf"),figure,width=14,height=13,device=pdf,useDingbats=FALSE,bg="white")
write_csv(summary,file.path(out,"panel_a_source.csv"))
write_csv(stability,file.path(out,"panel_b_source.csv"))
counts <- boot %>% mutate(description=paste0(response_var,": ",successful,"/",attempted,
  " successful refits; ",paired_oob_refits," paired out-of-bag comparisons")) %>% pull(description)
caption <- paste0("SI Figure 3. Sensitivity of LASSO selection and predictive performance to individual and grouped predictor omissions. ",
  "Separate DOC and nitrate models evaluate 12 specifications: the full primary set, each of eight individual omissions, ",
  "joint hydrology omission, and complete wildfire- and watershed-group omissions. ",
  "The watershed group contains watershed area, runoff, baseflow index, forest cover and soil property; the wildfire group contains burned area and high-severity burn. ",
  "Time since fire is separate and retained in both complete-group comparisons. All models use equal publication–pair–available-year fitting weights and repeat ",
  "training-fold preprocessing, publication-grouped inner tuning and the one-standard-error penalty rule. ",
  "A: Black diamonds show the change in equal-publication RMSE from nested leave-one-publication-out predictions ",
  "(omitted-set RMSE minus full-set RMSE). Colored circles and bars show the median and central 95% empirical range ",
  "of paired differences from publication-bootstrap fits evaluated on publications absent from each training draw. ",
  "The same training draws, tuning groups and held-out publications are used across specifications. ",
  "Positive differences indicate poorer prediction after omission; negative differences indicate improvement. ",
  "Bars describe variation across resampled training sets and varying held-out publication sets, not formal confidence intervals or significance tests. ",
  "B: Cells give the percentage of successful paired bootstrap refits selecting each available predictor (rounded to whole percentages). ",
  "Gray cells marked × indicate intentionally omitted predictors, rather than zero selection. Soil property is organic matter for DOC and clay content for nitrate. ",
  "Selection summaries include successful draws with no out-of-bag publications, which cannot contribute prediction errors. ",
  paste(counts,collapse="; "),". Fewer-than-three-unique-publication draws are excluded and logged. ",
  "Validation and resampling groups are publications, not connected publication–wildfire components; shared wildfires can cross validation boundaries. ",
  "Selection and omission results are conditional on the measured candidate set and do not establish causal or unique ecological importance. ",
  "The full-set models reproduce the primary leave-one-publication-out predictions; at 1,000 attempts, their selection frequencies also reproduce Figure 4. ")
writeLines(caption,file.path(out,"si_figure_3_caption.txt"))
message("Saved SI Figure 3 PNG/PDF, caption and source tables.")
