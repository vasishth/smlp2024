

#################################################################
##                            Setup                            ##
#################################################################

library("tidyverse")
library("afex")
library("emmeans")
library("ggbeeswarm")
theme_set(theme_bw(base_size = 15) + 
            theme(legend.position="bottom", 
                  panel.grid.major.x = element_blank()))

data("fhch2010") # load 
fhch <- droplevels(fhch2010[ fhch2010$correct,]) # remove errors
str(fhch2010) # structure of the data

##################################################################
##                           Analysis                           ##
##################################################################

##-------------------------------
##  Find model w/o singular fit  
##-------------------------------

mfull <- mixed(rt ~ task*stimulus*length + 
              (stimulus*length|id) + (task|item), fhch)
## singular fit
summary(mfull)$varcor
 # Groups   Name              Std.Dev.  Corr                              
 # item     (Intercept)       0.0673366                                   
 #          task1             0.0951533 -0.875                            
 # id       (Intercept)       0.2189684                                   
 #          stimulus1         0.0596298 -0.216                            
 #          length1           0.0230968  0.092  0.553                     
 #          length2           0.0129958  0.028  0.158 -0.692              
 #          stimulus1:length1 0.0174516 -0.305 -0.666 -0.941  0.445       
 #          stimulus1:length2 0.0071616  0.421  0.376 -0.301  0.852 -0.033
 # Residual                   0.3983609 

mfull_nc <- mixed(rt ~ task*stimulus*length + 
              (stimulus*length||id) + (task||item), fhch, 
               expand_re = TRUE)
## singular fit
summary(mfull_nc)$varcor
 # Groups   Name                     Std.Dev.
 # item     re2.task1                0.096796
 # item.1   (Intercept)              0.068659
 # id       re1.stimulus1_by_length2 0.000000
 # id.1     re1.stimulus1_by_length1 0.012481
 # id.2     re1.length2              0.000000
 # id.3     re1.length1              0.015972
 # id.4     re1.stimulus1            0.059152
 # id.5     (Intercept)              0.219145
 # Residual                          0.398759

m2_nc <- mixed(rt ~ task*stimulus*length + 
              (stimulus+length||id) + (task||item), fhch, 
               expand_re = TRUE)
## singular fit
summary(m2_nc)$varcor
 # Groups   Name          Std.Dev.
 # item     re2.task1     0.096805
 # item.1   (Intercept)   0.068647
 # id       re1.length2   0.000000
 # id.1     re1.length1   0.015897
 # id.2     re1.stimulus1 0.059122
 # id.3     (Intercept)   0.219139
 # Residual               0.398889

m3_nc <- mixed(rt ~ task*stimulus*length + 
              (stimulus||id) + (task||item), fhch, 
               expand_re = TRUE)
summary(m3_nc)$varcor
 # Groups   Name          Std.Dev.
 # item     re2.task1     0.096787
 # item.1   (Intercept)   0.068609
 # id       re1.stimulus1 0.059140
 # id.1     (Intercept)   0.219131
 # Residual               0.399101


##-----------------------------------
##  Compare maximal and final model  
##-----------------------------------

mfull
# Mixed Model Anova Table (Type 3 tests, S-method)
# 
# Model: rt ~ task * stimulus * length + (stimulus * length | id) + (task | 
# Model:     item)
# Data: fhch
#                 Effect        df         F p.value
# 1                 task  1, 44.27 15.26 ***   <.001
# 2             stimulus  1, 49.90 81.01 ***   <.001
# 3               length 2, 132.42 10.13 ***   <.001
# 4        task:stimulus  1, 57.45 28.52 ***   <.001
# 5          task:length 2, 195.31      0.47    .627
# 6      stimulus:length 2, 174.82      1.79    .169
# 7 task:stimulus:length 2, 253.36      0.13    .877
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

m3_nc
# Mixed Model Anova Table (Type 3 tests, S-method)
# 
# Model: rt ~ task * stimulus * length + (stimulus || id) + (task || item)
# Data: fhch
#                 Effect        df         F p.value
# 1                 task  1, 44.23 15.17 ***   <.001
# 2             stimulus  1, 50.36 80.37 ***   <.001
# 3               length 2, 590.04 11.88 ***   <.001
# 4        task:stimulus  1, 58.29 29.01 ***   <.001
# 5          task:length 2, 578.59      0.52    .596
# 6      stimulus:length 2, 590.07      2.07    .127
# 7 task:stimulus:length 2, 578.60      0.14    .871
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1


##--------------------------
##  Inspect marginal means  
##--------------------------

afex_plot(m3_nc, "length", "stimulus", id = "id" ,error = "within")

afex_plot(m3_nc, "length", "stimulus", "task", id = "id", error = "within", 
          data_geom = list(geom_quasirandom, geom_violin), 
          data_arg = list(list(width = 0.05, dodge.width = 0.5), 
                          list(width = 0.5)))
## always export plots via code and not via GUI
ggsave("new-3wayplot.png", width = 16, height = 12, units = "cm", dpi = 500)

afex_plot(m3_nc, "task", "stimulus", id = "id", error = "within", 
          data_geom = list(geom_quasirandom, geom_violin), 
          data_arg = list(list(width = 0.05, dodge.width = 0.5), 
                          list(width = 0.8)))
ggsave("new-2wayplot.png", width = 14, height = 12, units = "cm", dpi = 500)


##----------------------------------
##  emmeans for follow-up analysis  
##----------------------------------

emmeans(m3_nc, "length") %>% 
  contrast("poly")
#  contrast  estimate     SE  df z.ratio p.value
#  linear     0.05405 0.0111 Inf   4.856  <.0001
#  quadratic  0.00827 0.0192 Inf   0.430  0.6670
# 
# Results are averaged over the levels of: task, stimulus 
# Degrees-of-freedom method: asymptotic 

## step 1: specify marginal means object

em1 <- emmeans(m3_nc, c("stimulus", "task"))
em1
#  stimulus task   emmean     SE  df asymp.LCL asymp.UCL
#  word     naming  0.725 0.0517 Inf     0.623     0.826
#  nonword  naming  1.015 0.0518 Inf     0.913     1.116
#  word     lexdec  1.095 0.0464 Inf     1.004     1.186
#  nonword  lexdec  1.163 0.0464 Inf     1.072     1.254
# 
# Results are averaged over the levels of: length 
# Degrees-of-freedom method: asymptotic 
# Confidence level used: 0.95 

## let's test: is there effect of stimulus per task?
## step 2: specify contrasts
con1 <- list(
  stim_naming = c(-1, 1, 0, 0),
  stim_lexdec = c(0, 0, -1, 1)
)
contrast(em1, con1)
#  contrast    estimate     SE  df z.ratio p.value
#  stim_naming   0.2901 0.0301 Inf   9.646  <.0001
#  stim_lexdec   0.0672 0.0273 Inf   2.460  0.0139
# 
# Results are averaged over the levels of: length 
# Degrees-of-freedom method: asymptotic 

## shouldn't we control the family-wise error rate for follow-up tests? 
contrast(em1, con1, adjust = "holm") 
## I recommend always using "holm"
## what do we learn?

## which additional tests could we run?
con2 <- list(
  stim_naming = c(-1, 1, 0, 0),
  stim_lexdec = c(0, 0, -1, 1),
  task_word = c(-1, 0, 1, 0),
  task_nonword = c(0, -1, 0, 1)
)
contrast(em1, con2, adjust = "holm") 

### alternative approach to run the same tests, using "by" argument
emmeans(m3_nc, "stimulus", by = "task")

emmeans(m3_nc, "stimulus", by = "task") %>% 
  pairs() %>% 
  update(by = NULL) %>% 
  summary(adjust = "holm")

emmeans(m3_nc, "stimulus", by = "task") %>% 
  pairs() %>% 
  update(by = NULL) %>% 
  pairs()


#################################################################
##                Bayesian-frequentist p-values                ##
#################################################################

library("brms")
options(mc.cores = parallel::detectCores())
library("bayestestR")
library("emmeans")

options(contrasts = c('contr.equalprior_deviations', 'contr.poly'))

mbayes <- brm(rt ~ task*stimulus*length + (stimulus*length|id) + (task|item), fhch)

joint_tests(mbayes)
 # model term           df1 df2 F.ratio  Chisq p.value
 # task                   1 Inf  13.806 13.806  0.0002
 # stimulus               1 Inf  73.654 73.654  <.0001
 # length                 2 Inf  10.220 20.440  <.0001
 # task:stimulus          1 Inf  26.615 26.615  <.0001
 # task:length            2 Inf   0.437  0.874  0.6463
 # stimulus:length        2 Inf   1.908  3.816  0.1484
 # task:stimulus:length   2 Inf   0.128  0.256  0.8796


