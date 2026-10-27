# Figure 3: nested-CV improvement over the same intercept-only baseline.
# All fitting is performed by R/analysis/03_fit_predictor_group_cv_agent_v1.R.
library(tidyverse)
library(here)
root <- here("agent_workflows","vibe_coding")
input <- file.path(root,"output/paper/predictor_group_cv/group_improvement.csv")
out <- file.path(root,"output/paper/figures/figure_3_predictor_group_importance")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
summary <- read_csv(input,show_col_types=FALSE)
stopifnot(nrow(summary)==10,all(is.finite(summary$relative_RMSE_improvement)),all(summary$baseline_RMSE>0))
points <- summary %>% mutate(
  analyte=factor(response_var,levels=c("NO3","DOC"),labels=c("Nitrate","DOC")),
  group=factor(model,levels=c("time_full","time_watershed","time_wildfire","time_only","intercept_only"),
    labels=c("Time + watershed + wildfire","Time + watershed","Time + wildfire","Time","Intercept only")),
  improvement_percent=if_else(abs(relative_RMSE_improvement)<1e-10,0,100*relative_RMSE_improvement),
  label=sprintf("%.1f%%",improvement_percent))
ink <- "#222222"
palette <- c(Nitrate="#4386B5",DOC="#D49B47")
display_range <- range(c(0,points$improvement_percent))
display_span <- max(diff(display_range),10)
points <- points %>% mutate(
  label_position=improvement_percent+if_else(improvement_percent<0,-.04,.04)*display_span,
  label_hjust=if_else(improvement_percent<0,1,0))
plot <- ggplot(points,aes(x=improvement_percent,y=group,colour=analyte)) +
  geom_vline(xintercept=0,linetype="dashed",colour="#858585",linewidth=.45) +
  geom_segment(aes(x=0,xend=improvement_percent,yend=group),linewidth=.9) +
  geom_point(size=3) +
  geom_text(aes(label=label,x=label_position,hjust=label_hjust),size=3.4,colour=ink) +
  facet_wrap(~analyte,nrow=1) +
  scale_colour_manual(values=palette,guide="none") +
  scale_x_continuous(breaks=pretty(display_range,n=5),
    limits=c(display_range[1]-.22*display_span,display_range[2]+.25*display_span)) +
  scale_y_discrete(expand=expansion(add=.65)) +
  labs(x="Change in cross-validated RMSE compared to Intercept only model (%)",y=NULL) +
  theme_classic(base_size=11) + theme(
    text=element_text(colour=ink),axis.text=element_text(colour=ink,size=10),
    axis.title=element_text(colour=ink,size=11),axis.line=element_blank(),
    axis.ticks=element_line(colour=ink,linewidth=.4),
    panel.border=element_rect(colour=ink,fill=NA,linewidth=.45),
    strip.background=element_rect(fill="white",colour=ink,linewidth=.45),
    strip.text=element_text(colour=ink,size=11),legend.position="none")
ggsave(file.path(out,"figure_3_predictor_group_importance.png"),plot,width=11,height=6.2,dpi=300,bg="white")
ggsave(file.path(out,"figure_3_predictor_group_importance.pdf"),plot,width=11,height=6.2,
       device=pdf,useDingbats=FALSE,bg="white")
write_csv(points,file.path(root,"output/paper/tables/figure_3_group_importance.csv"))
numbers <- summary %>% arrange(response_var,model) %>% group_by(response_var) %>%
  summarise(description=paste0(first(response_var),": intercept-only RMSE ",sprintf("%.4f",first(baseline_RMSE)),
    "; time-only RMSE ",sprintf("%.4f",RMSE[model=="time_only"]),
    " (reduction ",sprintf("%.4f",RMSE_improvement[model=="time_only"]),
    "; ",sprintf("%.1f%%",100*relative_RMSE_improvement[model=="time_only"]),")",
    "; time + watershed RMSE ",sprintf("%.4f",RMSE[model=="time_watershed"]),
    " (reduction ",sprintf("%.4f",RMSE_improvement[model=="time_watershed"]),
    "; ",sprintf("%.1f%%",100*relative_RMSE_improvement[model=="time_watershed"]),")",
    "; time + wildfire RMSE ",sprintf("%.4f",RMSE[model=="time_wildfire"]),
    " (reduction ",sprintf("%.4f",if_else(abs(RMSE_improvement[model=="time_wildfire"])<1e-10,0,
      RMSE_improvement[model=="time_wildfire"])),
    "; ",sprintf("%.1f%%",if_else(abs(relative_RMSE_improvement[model=="time_wildfire"])<1e-10,0,
      100*relative_RMSE_improvement[model=="time_wildfire"])),")",
    "; time + watershed + wildfire RMSE ",sprintf("%.4f",RMSE[model=="time_full"]),
    " (reduction ",sprintf("%.4f",RMSE_improvement[model=="time_full"]),
    "; ",sprintf("%.1f%%",100*relative_RMSE_improvement[model=="time_full"]),")",
    "; intercept-only RMSE ",sprintf("%.4f",RMSE[model=="intercept_only"]),
    " (reduction ",sprintf("%.4f",RMSE_improvement[model=="intercept_only"]),
    "; ",sprintf("%.1f%%",100*relative_RMSE_improvement[model=="intercept_only"]),")"),.groups="drop") %>% pull(description)
caption <- paste0("Figure 3. Predictor-group importance. Cross-validated performance of intercept-only, time-only, and time-plus-predictor models, evaluated separately for DOC and nitrate relative to the intercept-only baseline within each analyte. ",
  "Points show 100 × (RMSE_intercept-only − RMSE_model) / RMSE_intercept-only; positive values indicate lower held-out error and negative values indicate higher error than intercept-only. ",
  "The watershed group includes watershed area, runoff, baseflow index, forest cover and soil organic matter (DOC) or clay content (nitrate). ",
  "The wildfire group includes watershed burned percentage and high-severity burn percentage. Time since fire is unpenalized in all three time-plus-predictor models; added group predictors receive the LASSO penalty. ",
  "Rows are ordered intercept only, time, time + wildfire, time + watershed, and time + watershed + wildfire. The intercept-only model is a hierarchically weighted training mean and is shown at zero. ",
  "Outer leave-one-publication-out folds are identical across all five fitted models; ",
  "group-model penalties use up to five publication-grouped inner folds, training-fold preprocessing and the one-standard-error rule. ",
  "Fitting weights balance publications, pairs and available years. RMSE is the square root of the mean held-out-publication MSE, with annual records equally weighted within each held-out publication. ",
  "DOC includes 35 annual responses from seven publications; nitrate includes 68 responses from 12 publications. RMSE and absolute reductions are in lnRR units. ",
  paste(numbers,collapse=". "),". Values within numerical tolerance of zero are displayed as zero. ",
  "Points summarize aggregate held-out predictive performance. Comparing each time-plus-group model with time alone identifies the additional predictive information from its group. For nitrate the four time-containing models have the same predictions within numerical tolerance; their improvement over intercept-only is attributable to time. Cross-analyte comparisons are descriptive because publications, response variation and soil predictors differ. ",
  "Groups contain five watershed and two wildfire candidates. Results quantify conditional predictive contribution, not causal importance. Publications sharing a wildfire can occur on opposite sides of a validation split. ",
  "These time-adjusted group models are distinct from the all-candidate-penalized selection models in Figure 4 and the omission diagnostics in SI Figure 3.")
writeLines(caption,file.path(out,"figure_3_caption.txt"))
message("Saved Figure 3 group importance PNG/PDF, caption and exact source table.")
