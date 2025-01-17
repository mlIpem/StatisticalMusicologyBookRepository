# Violin player visual feedback {#chapViolinist}

While in the previous chapter we looked at the effect of musical structure on the micro-timing of dancers during a solo-dance, here we look at the effect of visual feedback on instrumental learning.^[Thanks to input and feedback from A. Campo and A. Michalko]

Imagine an orchestra and its string section. Typically, the members of the string section, the violinists, have their bowing movements going up and down in synchronized manner, so that they appear as one single organism moving together. Less experienced violinists have to learn how to play in sync with the principal violinist of the section. They need to learn the bowing gestures as well as the particular expression associated with it. 

Now, imagine an audiovisual play-along system with a visual representation of the principal violinist of the orchestra as teacher-avatar. Wouldn't that be a cool system to train with, for a violinist? And what happens if the teacher-avatar would be visible in 3D, with a Hololens? Would that be of any benefit to learning?

The ultimate goal of the present chapter is to check the effectiveness of a 3D play-back system for learning to play in an orchestra, compared to a 2D play-back system. 
To figure that out is not an easy matter, due the entire setup and resources available. Typically, we deal with a small number of participants and different levels of musical experience.

Owing to these and other reasons, not at least the novelty and highly technical character of the study, this chapter is highly exploratory. 
It shows the flexibility of Bayesian statistical modelling is studies were time is important, even when focusing on individual musicians. 

Learning to perform as a violinist in an orchestra is a multifaceted process that involves structured practice, anticipatory behavior, collaborative engagement, and continuous feedback. These elements work together to create a self-augmented interaction, where the violinist's skills and abilities are continually enhanced through their engagement with the orchestra. 

The modelling techniques introduced in the previous chapter are partly repeated in the present chapter. Data for this chapter can be found in Campo et al. (2023a, 2023b), with software available at Campo et al. (2024). See also Michalko et al. (submitted).
The code can be found in the following scripts for data preparation and plotting, modelling and plotting, and contrast analysis and plotting:


``` r
source("Code/chapAll_00_Initialization.R")
source("Code/chapAll_01_Functions.R")
source("Code/chapViolinist/chapViolinist_02_DataPreparation.R")
source("Code/chapViolinist/chapViolinist_03_DataPlotting.R")
source("Code/chapViolinist/chapViolinist_04_Modelling.R")
source("Code/chapViolinist/chapViolinist_05_ModelPlotting.R")
source("Code/chapViolinist/chapViolinist_06_Contrasts.R")
```


## Theory

Why would we believe that a music playback system with 3D teacher-avatar would be more efficient for learning than its 2D version?
The reason is probably that 3D offers a richer feedback for motor-control.

This hypothesis can be tested in an *augmented reality* setting with violinists wearing a Hololens.
Its unique capabilities allow *parallax*, or visual changes of the visual scene by small head movements, similar to what we do in everyday life when we see an object from different angles and estimate its size  by considering close versus nearby features changing due to head movements.  The bowing gestures of a violin teacher observed under these circumstances enables a more nuanced interpretation than when parallax is not possible, thus providing a better feedback for the control of the own bowing gestures. Parallax is not possible when the avatar is seen in 2D because in that condition, the teacher-avatar is just projected on a screen. 
Using the Hololens the same bowing gestures can seen with or without parallax, depending on whether the scene is presented in 2D or 3D augmented reality. 

Accordingly, the augmented reality setting offers a perfect environment for testing whether parallax is a necessary component in the visual modality for its feedback on motor control.
Figure \@ref(fig:chapViolinistTeacherAvatar1) shows a typical measurement setup of a student (a, b), with teacher-avatar seen from the viewpoint of the student through a Hololens in 2D (c), and in 3D (d).

<div class="figure" style="text-align: center">
<img src="Figures/chapViolinist_TeacherAvatar.jpg" alt="View on the teacher-avatar in 2D and in 3D: (a) student violinist equipped with Hololens and motion tracking suit, (b) stick figure extracted, teacher-avatar in 2D, and (c, d) teacher-avatar in 2D and 3D, as seen through the Hololens" width="100%" />
<p class="caption">(\#fig:chapViolinistTeacherAvatar1)View on the teacher-avatar in 2D and in 3D: (a) student violinist equipped with Hololens and motion tracking suit, (b) stick figure extracted, teacher-avatar in 2D, and (c, d) teacher-avatar in 2D and 3D, as seen through the Hololens</p>
</div>



## Experiment

In the experiment, eleven participants were recruited, all members from the Ghent Universitary Student Orchestra GUSO.
They were engaged in four practice trials spaced evenly over the course of a month, with each student experiencing both conditions (2D, 3D). Like in real orchestral playing, where members of the string section follow the principal violinist, students were instructed to closely imitate the teacher-avatar's bowing gestures, encompassing bowings, articulations, and dynamics. 

Due to the limited amount of students, the design is somewhat peculiar in the sense that
each participant takes part in two conditions (2D and 3D), with four trials per condition (T1, T2, T3, T4).
However, conditions apply to different pieces (F1, F2, F3, F4), with F1 and F2 for first violin players, and F3 and F4 for second violin players. There are two teacher-avatars, one for the principal violinist of the first violin players,
and one for the principal violinist of the second violin players. 
The structure is shown in the table below.

<table class="table table-striped" style="font-size: 10px; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">(\#tab:chapViolinistDesign1)Experimental design</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> subject </th>
   <th style="text-align:left;"> condition </th>
   <th style="text-align:left;"> piece </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> P004 </td>
   <td style="text-align:left;"> 2D </td>
   <td style="text-align:left;"> F1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P006 </td>
   <td style="text-align:left;"> 2D </td>
   <td style="text-align:left;"> F1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P009 </td>
   <td style="text-align:left;"> 2D </td>
   <td style="text-align:left;"> F1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P001 </td>
   <td style="text-align:left;"> 3D </td>
   <td style="text-align:left;"> F1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P002 </td>
   <td style="text-align:left;"> 3D </td>
   <td style="text-align:left;"> F1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P010 </td>
   <td style="text-align:left;"> 3D </td>
   <td style="text-align:left;"> F1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P001 </td>
   <td style="text-align:left;"> 2D </td>
   <td style="text-align:left;"> F2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P002 </td>
   <td style="text-align:left;"> 2D </td>
   <td style="text-align:left;"> F2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P010 </td>
   <td style="text-align:left;"> 2D </td>
   <td style="text-align:left;"> F2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P004 </td>
   <td style="text-align:left;"> 3D </td>
   <td style="text-align:left;"> F2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P006 </td>
   <td style="text-align:left;"> 3D </td>
   <td style="text-align:left;"> F2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P009 </td>
   <td style="text-align:left;"> 3D </td>
   <td style="text-align:left;"> F2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P003 </td>
   <td style="text-align:left;"> 2D </td>
   <td style="text-align:left;"> F3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P005 </td>
   <td style="text-align:left;"> 2D </td>
   <td style="text-align:left;"> F3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P007 </td>
   <td style="text-align:left;"> 3D </td>
   <td style="text-align:left;"> F3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P008 </td>
   <td style="text-align:left;"> 3D </td>
   <td style="text-align:left;"> F3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P011 </td>
   <td style="text-align:left;"> 3D </td>
   <td style="text-align:left;"> F3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P007 </td>
   <td style="text-align:left;"> 2D </td>
   <td style="text-align:left;"> F4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P008 </td>
   <td style="text-align:left;"> 2D </td>
   <td style="text-align:left;"> F4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P011 </td>
   <td style="text-align:left;"> 2D </td>
   <td style="text-align:left;"> F4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P003 </td>
   <td style="text-align:left;"> 3D </td>
   <td style="text-align:left;"> F4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P005 </td>
   <td style="text-align:left;"> 3D </td>
   <td style="text-align:left;"> F4 </td>
  </tr>
</tbody>
</table>

Unfortunately, this design does not allow us to check conditions per student and per piece. as students played one piece in 2D and another piece in 3D.
However, for each piece, we can compare students playing with 2D with students playing with 3D.
For example, for piece F1, we have student P004, P006 and P009 playing in the 2D condition and we have student P001, P002, and P0010 playing in the 3D condition.
Moreover, each subject played four trials, spread over four weeks.
The two groups, formed by condition, can be compared per piece. 

Eleven subjects is not much. The reason is that experiments are very time-consuming and that subjects, such as those who play in an orchestra, are few and sometimes hard to find, especially for participation in longitudinal studies. Nevertheless, thanks to the four trials, which we consider here as repeated measures, we are just on the border of having enough statistical power.


## Data

A study has already been published in Campo et al. (2023a) and the bowing gesture events can be downloaded from Campo et al. (2023b) with software available at Campo et al. (2024).
Here we focus on aspects of statistical modelling that are not explicitly contained in the above publications.

Our data comes from the publicly available dataset (Campo et al., 2023a). It describes the similarity of student and teacher in terms of *events*, that is, the identification of a bowing gesture and the subsequent calculation of a bowing similarity feature between student and teacher. The similarity feature is based on the *Procustus distance (PD)*, which quantifies the extent of deformation needed to transform one gesture into the other gesture. It is based on the scaling, rotating, and translating of the student's bowing gesture. Smaller PD values indicate greater similarity between bowing gestures. 

To facilitate the statistical fitting, we use the scaled log of PD and call it log_PD. Note that in some cases it was is hard to define bowing events. Accordingly, several passages could not be taken into account in the analysis.
We use the dataset specified in the following table.
The original dataset also contains motion capture, audio, and questionnaire data from violinists. 


<table class="table table-striped" style="font-size: 10px; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">(\#tab:chapViolinistDesign2)Data with procustus distance (PD)</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> PD </th>
   <th style="text-align:right;"> logPD </th>
   <th style="text-align:right;"> startIndex </th>
   <th style="text-align:right;"> endIndex </th>
   <th style="text-align:right;"> time </th>
   <th style="text-align:left;"> subject </th>
   <th style="text-align:left;"> trial </th>
   <th style="text-align:left;"> condition </th>
   <th style="text-align:left;"> piece </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0.09 </td>
   <td style="text-align:right;"> -0.84 </td>
   <td style="text-align:right;"> 3.59 </td>
   <td style="text-align:right;"> 4.30 </td>
   <td style="text-align:right;"> 3.59 </td>
   <td style="text-align:left;"> P001 </td>
   <td style="text-align:left;"> T1 </td>
   <td style="text-align:left;"> 3D </td>
   <td style="text-align:left;"> F1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.09 </td>
   <td style="text-align:right;"> -0.84 </td>
   <td style="text-align:right;"> 3.59 </td>
   <td style="text-align:right;"> 4.30 </td>
   <td style="text-align:right;"> 3.89 </td>
   <td style="text-align:left;"> P001 </td>
   <td style="text-align:left;"> T1 </td>
   <td style="text-align:left;"> 3D </td>
   <td style="text-align:left;"> F1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.09 </td>
   <td style="text-align:right;"> -0.84 </td>
   <td style="text-align:right;"> 3.59 </td>
   <td style="text-align:right;"> 4.30 </td>
   <td style="text-align:right;"> 4.19 </td>
   <td style="text-align:left;"> P001 </td>
   <td style="text-align:left;"> T1 </td>
   <td style="text-align:left;"> 3D </td>
   <td style="text-align:left;"> F1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.21 </td>
   <td style="text-align:right;"> 0.26 </td>
   <td style="text-align:right;"> 4.30 </td>
   <td style="text-align:right;"> 5.33 </td>
   <td style="text-align:right;"> 4.30 </td>
   <td style="text-align:left;"> P001 </td>
   <td style="text-align:left;"> T1 </td>
   <td style="text-align:left;"> 3D </td>
   <td style="text-align:left;"> F1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.21 </td>
   <td style="text-align:right;"> 0.26 </td>
   <td style="text-align:right;"> 4.30 </td>
   <td style="text-align:right;"> 5.33 </td>
   <td style="text-align:right;"> 4.60 </td>
   <td style="text-align:left;"> P001 </td>
   <td style="text-align:left;"> T1 </td>
   <td style="text-align:left;"> 3D </td>
   <td style="text-align:left;"> F1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.21 </td>
   <td style="text-align:right;"> 0.26 </td>
   <td style="text-align:right;"> 4.30 </td>
   <td style="text-align:right;"> 5.33 </td>
   <td style="text-align:right;"> 4.90 </td>
   <td style="text-align:left;"> P001 </td>
   <td style="text-align:left;"> T1 </td>
   <td style="text-align:left;"> 3D </td>
   <td style="text-align:left;"> F1 </td>
  </tr>
</tbody>
</table>

The first three columns contain the metrics of the Procustus Distance, `PD`, its (scaled) logarithm `logPD`. 
The columns `startIndex` and `endIndex` contain the start and end times of a bowing gesture in seconds.
The sample times are indicated in `time` and bowing gestures are sampled every .3 seconds.
The other columns `subject`, `trial`, `condition`, `piece` are factors having levels (11 subjects, 4 trials, 2 conditions, 4 pieces).

## Data plotting

Basically, a good performance is here defined as a performance where the student's bowing gestures are in sync with the teacher's bowing gestures. The lower the PD, the more similar the bowing gesture resembles the teacher's bowing gesture. 

A histogram of all data of the PD metric reveals a Poisson-like distribution of the PD values, as shown in the left panel of figure \@ref(fig:chapViolinistPDhist).
The right panel shows the logarithmic transformation of the PD values. Low values get a bit more stretched, and high values get squeezed but the distribution is still slightly skewed due to data with high PD values, especially in the 2D condition. 
Interestingly, difference between 2D and 3D are mainly seen at higher values, where 3D has fewer such values, indicating better alignment.
The dotted lines show the medians for each condition. It looks as if 3D is lower than 2D.

<div class="figure" style="text-align: center">
<img src="Figures/chapViolinist_PD_NewData_p1_hist.png" alt="Density of PD for 2D and 3D" width="49%" /><img src="Figures/chapViolinist_PD_NewData_p2_hist.png" alt="Density of PD for 2D and 3D" width="49%" />
<p class="caption">(\#fig:chapViolinistPDhist)Density of PD for 2D and 3D</p>
</div>

However, there may be differences between pieces and sections.
Figure \@ref(fig:chapViolinistPDdata) shows the data over time, with four different pieces (F1,F2,F3,F4) and two conditions (2D, 3D).
The vertical axis shows the logPD.
The horizontal axis shows the time.
Each dot represents a sample logPD value.

<!-- As mentioned, a bowing gestures have samples corresponding to the duration of the played note(s). Accordingly, longer bowing gestures are represented as a sequence of dots having equal logPD. -->
<!-- But that's difficult to see on this picture. -->
<!-- The white gaps reveal silence (no bowing gesture). -->


<div class="figure" style="text-align: center">
<img src="Figures/chapViolinist_PD_NewData_p3_data.png" alt="Data of four pieces in two conditions" width="100%" />
<p class="caption">(\#fig:chapViolinistPDdata)Data of four pieces in two conditions</p>
</div>


Differences between 2D and 3D are subtle but some things can already be observed by visual inspection. For example, in the first column (piece F1), first row (condition 2D), there are quite some dots above 2 in the 2D condition, whereas only a few dots above 2 in the 3D condition. This suggests a better alignment in 3D. However, in the second column (piece F2), at about 75 seconds, it looks as if the higher logPD values occur in 3D, compared to 2D.

<div class="figure" style="text-align: center">
<img src="Figures/chapViolinist_Score_F1.jpg" alt="Score excerpt of the first piece (F1), with time indication in seconds." width="100%" />
<p class="caption">(\#fig:chapViolinistF1score)Score excerpt of the first piece (F1), with time indication in seconds.</p>
</div>

## Smooth regression

Having our focus on the performance over time, we develop an analysis based on smooth regression. In that approach, we estimate the similarity/synchronization metric in terms of smooths over time. These smooths represent a floating mean and variance over time.
The smooth regression can be specified as follows:


``` r
form = bf(logPD ~ 1 + condition + s(time,k=30) + 
            s(time,by=condition,k=30) +  (1 | subject + trial))
fam = gaussian()
```

This expression says that `logPD` is modeled by a general intercept (represented by “1”) for the two levels of `condition` (2D, 3D), and a smooth over `time` for the levels of `condition`, using a basis of 30 splines. Each value of `logPD` at a certain point in time is a weighted sum of the spline basis functions. Furthermore, `trial` and `subject` are defined as group-level variables, whose variance is extracted such that the estimates for the other variables don’t contain this variance. Given the fact that both PD and 1-R show a Poisson-like distribution, we take the log and apply scaling such that the mean of all data is zero. This allows us to apply a gaussian link function of calculated mean and variance to the data.


This smooth regression is implemented in the R-package `brms` but it comes with a high computational cost, due to the algorithms for optimization. Alternatively, the R-package `mgcv` can be used. However, `brms` is more flexible than `mgcv` as it offers a better grip on the fitted model.  

Based on the fitted model using `brms`, we extract samples from the posterior. In particular, we generate a *posterior predictive distribution* and a *posterior difference distribution*, similar to the way we did it in chapter \@ref(chapDancer). The posterior predictive distribution can be generated using a newly defined data grid in which `time` is sampled, say at .5 seconds, and all other variables in the model are accordingly sampled. This entire posterior prediction contains one posterior predictive distribution per specified time point, and we can then easily extract the distributions for the 2D and 3D condition and subtract them to get the posterior difference distribution. 

The probability mass above or below zero can be calculated and used in support of the difference in posterior predictive distribution (2D - 3D). A probability mass of 90% above zero would give strong support for 2D being distinct from 3D. Note that we will have this posterior at each time instance, so that the performance in 2D versus 3D can be contrasted over time. Obviously, we can also summarize them over time intervals. 

To summarize the steps: (i) run the smooth regression model, (ii) generate a model-based prediction using a dataset with a counterfactual time (e.g. sampling at every .5 seconds), (iii) contrast 2D and 3D predictions at each sampled time.

## Contrasts

As we compare performances with a visual scene in 2D with those in 3D, we develop a contrast analysis that shows us all contrasts over time.

### Posterior over time {-}

<div class="figure" style="text-align: center">
<img src="Figures/chapViolinist_Diff_model_F1.png" alt="Piece F1 with audio (top panel), logPD and posterior predictions for 2D and 3D conditions (middle panel), and posterior difference of 2D and 3D (bottom panel). Vertical lines show the regions of interest." width="100%" />
<p class="caption">(\#fig:chapViolinistPDdiff1)Piece F1 with audio (top panel), logPD and posterior predictions for 2D and 3D conditions (middle panel), and posterior difference of 2D and 3D (bottom panel). Vertical lines show the regions of interest.</p>
</div>
Figure \@ref(fig:chapViolinistPDdiff1) shows audio and posterior distributions of the first piece used in this experiment.
The top panel shows the audio waveform of the teacher's performance. We use it here mainly as a reference, to check rests and fragments that could not be analysed because bow gestures could not be identified. The blocks, marked by vertical lines, show the relevant sections where bow gesture events could be identified. The gray zones show the zones that were bowing gesture events could not be easily defined. Therefore, these events have been deleted in the analysis.

The middle panel shows the logPD of the bowing gestures of the 2D and 3D performances for all subjects and trials in this piece, see the above table.
The smooths (2D in gray, 3D in ocre) show the *posterior predictive distribution* calculated at each .5 seconds, indicating the mean and its uncertainty for each condition over subjects.
Note that the smooths show only 25% of the distribution's probability mass (the critical interval at 25%, or CI-25%). Taking CI-90% would have resulted in too much overlap and a less clear picture.

The bottom panel shows the *posterior difference distribution*. 
At each point in time, 1000 samples are randomly drawn from the (2D, 3D) posterior predictive distributions. Then, for each draw, the number from $2D$ and the corresponding number from $3D$ are subtracted (as in: $2D-3D$), giving 1000 new values used to build a new distribution, called the *posterior difference distribution*. 
Note that in this calculation the entire distribution is used, rather than the CI-25% shown in the middle panel.

This calculation is then applied to all points in time, leading to the *posterior difference distribution* with its its CI-90%. 
The red dots on the horizontal line (at 2D-3D = 0), just indicate whether data samples are available at that point in time.

Note that the *posterior difference distribution* is possible because it is based on the smooths from the *posterior predictive distribution*. A distribution based on data points (rather than smooths) would face the problem that bow strokes have no common onset and offset. And therefore, they cannot be compared. The smooth allows for the contrast! 

As in the previous chapter, the key trick is to define a new data grid with counterfactual time, and then use the model that generalizes over time to extract the samples. The R-code is based on the following expressions (see: `Code/chapViolinist/chapViolinist_05_ModelPlotting.R```):


``` r
newdat <- data %>% data_grid(time = seq(time_begin, time_end,by=.5))
post_pred_distr <- epred_draws(fit, newdat, scale = "response", re_formula = NA) 
```

Given this approach, we can check the probability direction at each sampled time for any defined interval over time.
If, at a certain point in time, this gray band is completely above zero, then, there is a strong support for 3D having a lower logPD than 2D, which means: 3D offers better alignment with the teacher.

If this gray band is completely below zero, then there is a strong support for 2D having a lower logPD value than 3D, which means: 2D offers a better alignment with the teacher.
If it crosses zero, then it depends on how much of the probability mass is different from zero. If it is still 70%, one could argue for a very weak trend, but 50% would certainly indicate no difference.

In short,  when the gray band is above zero it is possible to conclude that the student's bowing gestures are better lined up with the 3D teacher's bowing gestures, confirming the theory that parallax might offer a distinctive feedback on the sensorimotor control needed for bowing. 

The bottom panel shows a contrast analysis per time point.
The graph seems to suggest that the alignment is in favor of 3D, meaning that 3D works better than 2D.
Not at all time instances, but at many.

### Considering blocks {-}

It is useful to look at the data in blocks. 
In figure \@ref(fig:chapViolinistPDdiff1) these blocks have marked time intervals of
$[3.6, 38.7]$,
$[53, 88]$,
$[97.3, 99]$,
$[127,145]$,
and $[165, 180.5]$ seconds, as indicated by the vertical red lines.

First consider block one and two.
The reference is still the bottom panel of figure \@ref(fig:chapViolinistPDdiff1). 
See also the score in figure \@ref(fig:chapViolinistF1score), with a rudimentary indication of time.
In both blocks, a sudden short peak appears at about 26 and at 77 seconds, corresponding to the ending of a phrase where longer notes are played. 
The first block is almost everywhere significantly different, while the second block, more gray parts can be observed, for instance at about 63 seconds.

A distribution of these two blocks can be plotted with density plots, as shown in figures \@ref(fig:chapViolinistContrastblock1a) and \@ref(fig:chapViolinistContrastblock1b).
As mentioned, it can be concluded that there is a contrast in condition for block 1 but not for block 2.

<div class="figure" style="text-align: center">
<img src="Figures/chapViolinist_ContrastBlock_1.png" alt="Contrast of block1 [3.6,26]s" width="49%" />
<p class="caption">(\#fig:chapViolinistContrastblock1a)Contrast of block1 [3.6,26]s</p>
</div>

<div class="figure" style="text-align: center">
<img src="Figures/chapViolinist_ContrastBlock_2.png" alt="Contrast of block 2 [53,77]s" width="49%" />
<p class="caption">(\#fig:chapViolinistContrastblock1b)Contrast of block 2 [53,77]s</p>
</div>

Based on the posterior with smooths, it is possible to summarize the blocks over time.
Table \@ref(tab:chapViolinistBlockContrasts1) shows the result.
The label on the left indicates the piece and the selected interval in seconds, followed by the mean of the posterior difference distribution for PD, and its CI-95% as indicated by the min and max, followed by the probability of direction (pd).

It can be observed that almost all contrasts are positive, suggesting that 2D has higher logPD values than 3D and thus 2D is overall less well aligned compared to 3D. However, the pd shows that only few sections have strong evidence for a contrast. A logPD difference of 0.5 is at least needed for strong evidence of a contrast. The calculation is found in `Code/chapViolinist/chapViolinist_06_Contrasts.R`.


<table class="table table-striped" style="font-size: 10px; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">(\#tab:chapViolinistBlockContrasts1)Contrasts of in defined segments of different Pieces (F1,..,F4)</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Piece[Interval] </th>
   <th style="text-align:right;"> PDdiff </th>
   <th style="text-align:right;"> PDdiff_min </th>
   <th style="text-align:right;"> PDdiff_max </th>
   <th style="text-align:right;"> pd </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> F1[3.6,38.7] </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> -0.19 </td>
   <td style="text-align:right;"> 2.52 </td>
   <td style="text-align:right;"> 96 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F1[53,88] </td>
   <td style="text-align:right;"> 0.56 </td>
   <td style="text-align:right;"> -0.56 </td>
   <td style="text-align:right;"> 2.86 </td>
   <td style="text-align:right;"> 83 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F1[97.3,99] </td>
   <td style="text-align:right;"> 0.72 </td>
   <td style="text-align:right;"> -0.27 </td>
   <td style="text-align:right;"> 1.93 </td>
   <td style="text-align:right;"> 95 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F1[127,145] </td>
   <td style="text-align:right;"> 0.55 </td>
   <td style="text-align:right;"> -0.55 </td>
   <td style="text-align:right;"> 1.85 </td>
   <td style="text-align:right;"> 88 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F1[165,180.5] </td>
   <td style="text-align:right;"> 1.12 </td>
   <td style="text-align:right;"> 0.11 </td>
   <td style="text-align:right;"> 2.40 </td>
   <td style="text-align:right;"> 98 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F2[23,33.5] </td>
   <td style="text-align:right;"> -0.23 </td>
   <td style="text-align:right;"> -1.73 </td>
   <td style="text-align:right;"> 0.53 </td>
   <td style="text-align:right;"> 27 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F2[45,93] </td>
   <td style="text-align:right;"> -0.19 </td>
   <td style="text-align:right;"> -1.94 </td>
   <td style="text-align:right;"> 1.02 </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F2[110,131] </td>
   <td style="text-align:right;"> -0.24 </td>
   <td style="text-align:right;"> -1.98 </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 26 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F2[145.7,180] </td>
   <td style="text-align:right;"> -0.93 </td>
   <td style="text-align:right;"> -2.61 </td>
   <td style="text-align:right;"> 0.03 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F3[14.5,32.5] </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> -0.87 </td>
   <td style="text-align:right;"> 0.87 </td>
   <td style="text-align:right;"> 66 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F3[39,68.5] </td>
   <td style="text-align:right;"> 0.07 </td>
   <td style="text-align:right;"> -0.89 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 55 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F3[110,133] </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;"> -0.90 </td>
   <td style="text-align:right;"> 0.70 </td>
   <td style="text-align:right;"> 54 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F3[145,156] </td>
   <td style="text-align:right;"> -0.16 </td>
   <td style="text-align:right;"> -1.05 </td>
   <td style="text-align:right;"> 0.53 </td>
   <td style="text-align:right;"> 38 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F3[183,188.5] </td>
   <td style="text-align:right;"> 0.03 </td>
   <td style="text-align:right;"> -0.80 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 53 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F4[20,28] </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 0.12 </td>
   <td style="text-align:right;"> 0.94 </td>
   <td style="text-align:right;"> 99 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F4[73,96.6] </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> -0.46 </td>
   <td style="text-align:right;"> 0.45 </td>
   <td style="text-align:right;"> 51 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F4[140,179] </td>
   <td style="text-align:right;"> 0.14 </td>
   <td style="text-align:right;"> -0.57 </td>
   <td style="text-align:right;"> 0.55 </td>
   <td style="text-align:right;"> 68 </td>
  </tr>
</tbody>
</table>


Considering the block defined by $[3.6,38.7]s$, there is evidence in favor of a contrast. The value pd, of direction of probability, says that 96% of the distribution is above zero. And being above zero (with a logPD value of 1.00) means that in the 3D condition, the student's performance is much better aligned with the teacher.

Considering the block defined by $[53,88]s$. Here there is no evidence for a contrast because the value of pd is 83%. One could argue that there is a trend but it is certainly not distinctive enough. In fact, a careful visual inspection of these blocks in \@ref(fig:chapViolinistPDdiff1) already suggested this result.
Now we have it in numbers!

The overall conclusion is that the student's alignment with the teacher, in terms of bowing gestures, works better in 3D than in 2D but the sections showing strong support for this contrast are rare: only 4 out of 17 sections. Moreover, it seems that most of the evidence is found in piece F1.

At this point, a qualitative analysis of score and bow strokes would be needed.
For example, at some time instances, especially when long notes are played at the ending of a block, the difference is suddenly more prominent. Score and associated performance analysis would be needed to further investigate the origin of sudden (dis)alignments between student and teacher.

The major point made in this chapter is methodological. The above analysis illustrates the flexibility of the statistical modelling approach and this flexibility is of great value for future work in this domain.

### Individual violinists {-}

Here we illustrate the flexibility of this type of modelling in an analysis of individual violinists, where we can look at the performance of each violinist.
Figure \@ref(fig:chapViolinistIndividual1) shows how each individual violinist can be visualized.
Each line represents the smooth over the data per trial, for subject P009 only.

<div class="figure" style="text-align: center">
<img src="Figures/chapViolinist_smooths_piece_F1_condition_2D_subject_P009.png" alt="Piece F1 with audio (top panel), logPD and posterior predictions " width="100%" />
<p class="caption">(\#fig:chapViolinistIndividual1)Piece F1 with audio (top panel), logPD and posterior predictions </p>
</div>



## Continuous metric

In what follows, we show some results with a different metric.
The metric itself will be discussed in more detail in the next chapter (chapter \@ref(chapExoskeletons)).

It suffices here to say that it is based on a continuous comparison of the bowing movements, using using time differences between student and teacher captured as relative phase. The metric is ignorant of bowing gesture events because it is a continuous metric. That's an advantage. The disadvantage is that passages in which both the bowing of the teacher and student are at rest shows the highest synchronization because they don't differ in timing. Obviously, these passages of rest can be easily extracted from the audio and should be taken into account to mark the performance. 


<div class="figure" style="text-align: center">
<img src="Figures/chapViolinist_GUSO2_A2model_2conditions_piece_F1.rds.png" alt="Piece F1 with audio (top panel), logPD and posterior predictions (middle panel), and posterior difference of 2D and 3D. Vertical lines show the regions of interest" width="100%" />
<p class="caption">(\#fig:GUSO2A2model2conditionspieceF1)Piece F1 with audio (top panel), logPD and posterior predictions (middle panel), and posterior difference of 2D and 3D. Vertical lines show the regions of interest</p>
</div>
In figure \@ref(fig:GUSO2A2model2conditionspieceF1) we clearly see that the contrast (lowest panel) is in favour of 3D, except for passages that indicate rest. They indicate that at those points, no difference can be made between 2D and 3D. Ideally, student and teacher don't move the bow and therefore, they are most in sync at those points, so the contrast is zero. See the second panel where the log(1-R) measure is lowest in regions where no audio occurs.

Figures \@ref(fig:GUSO2A2model2conditionspieceF2a) and \@ref(fig:GUSO2A2model2conditionspieceF2b) show the two metrics (Procustus and relative phase) next to each other, now applied to the second piece (F2).

At the beginning of the piece, 3D offers a better synchronization than 2D. However, towards the end of the piece, the roles are reversed and 2D offers a better synchronization than 3D. We see this trend reflected in both figures. The reason may be due to the fact that the score is different towards the end. Accordingly, this type of analysis calls for a deeper musicological analysis based on additional score analysis. This book is not the place to provide such analysis. We merely show that some statistical representation of the music performance may be of interest for future work to be done in this domain.

<div class="figure" style="text-align: center">
<img src="Figures/chapViolinist_Diff_model_F2.png" alt="Comparison of Procustus metric for piece F2" width="100%" />
<p class="caption">(\#fig:GUSO2A2model2conditionspieceF2a)Comparison of Procustus metric for piece F2</p>
</div>


<div class="figure" style="text-align: center">
<img src="Figures/chapViolinist_GUSO2_A2model_2conditions_piece_F2.rds.png" alt="Comparison of relative phase metric" width="100%" />
<p class="caption">(\#fig:GUSO2A2model2conditionspieceF2b)Comparison of relative phase metric</p>
</div>

## Discussion

Several noteworthy aspects merit attention when discussing teachers and their bow gestures. These gestures vary in expressiveness, ranging from those that effectively convey a clear intention to those that are less expressive and fail to effectively communicate the intended message. At times, a gesture may be readily anticipated and comprehended by the student, facilitating synchronization between teacher and student. However, there are instances when the gesture is unprepared, plausibly resulting in diminished anticipation and understanding on the part of the student.

Treating all gestures equally, irrespective of their level of anticipation,  -- as we do in the modelling -- could potentially impact the analysis and interpretation of specific effects. Difference in the teachers' expressiveness can consequently result in disparate communicative values, influencing how students perceive and respond to gestures.

Moreover, the student's attention resources likely fluctuate over time due to multitasking, encompassing activities such as playing the instrument, consulting the musical score, and observing the teacher. Given this dynamic, it remains uncertain where the student directs their attention and how attention is distributed among these tasks. Although technologies such as eye-trackers and EEG measurements could provide insights, integrating them would significantly augment the volume of data and introduce numerous challenges for subsequent data analysis and statistical modeling.

Furthermore, additional sources of variability arise from variations in educational backgrounds and the complexity levels of musical compositions. Students exhibit diverse playing skills and varying levels of experience within the orchestra. Moreover, musical pieces themselves present differing degrees of difficulty, further contributing to the complexity of the situation.

The multitude of factors contributing to uncertainty in our data inevitably impacts the outcomes of statistical modeling. Given these limitations, it would be unrealistic to anticipate statistical modeling as a panacea for resolving these issues. Instead, the objective of this chapter is to apply statistical modeling to our available data and assess the extent of our findings within the context of these limitations.

Primarily, this chapter demonstrates the efficacy of smooth regression and the adaptability of Bayesian statistical modeling. While the results may not yield unequivocal conclusions due to the inherent uncertainties, the methodology presented herein is profoundly intriguing and holds substantial potential for future research in music performance analysis.

## Conclusion

Overall, we found a trend in favor of 3D, at least for piece F1. 
The ability to use parallax seems to help in particular circumstances related to the difficulty of the playing. However, 2D may offer already a decent good visual perception opportunity that is sufficient to control the bowing gestures.

With this result, one may wonder whether the cost of a Hololens and addition equipment needed to play along with the teacher-avatar is really worth the effort. We merely see at least a trend in favour of 3D, which is in line with the suggestion that parallax is an important feature of the visual modality in order to work as feedback for the control of bowing gestures.

Nevertheless, the analysis illustrates the flexibility of smooth regression in Bayesian statistics.
It is possible to zoom in on sections, and get specific answers about whether on or the other condition works better in those sections. 
The main idea is that statistical modelling allows for predictions over equally sampled times. This is useful when events from one condition are not necessarily happing at the same time of events of another condition, and you want to compare both conditions assuming that there is correlation over time at short time intervals.

<!-- ## References -->
<!-- Campo et al. (2023a, 2023b, 2024) -->
