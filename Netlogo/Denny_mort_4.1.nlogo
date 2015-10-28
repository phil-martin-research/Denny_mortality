;to do for model
;1 - simplify and tidy code
;2 - get empirical estimates for: (1) - Seedling density in forests unaffected by grazing, (2) estimates of the BA/SD needed to allow seedlings to germinate 
;& (3) to allow seedlings to progress to sapling stage


globals [juvenile-count mature-count mature-BA mast-year dead-total]
breed [trees tree]
breed [juveniles juvenile]
trees-own [age BA tree-size-t1 tree-size-t2 disperse-distance growth-rate tree-density local-BA dist-dead]
juveniles-own [age tree_size disperse-distance tree-density local-BA]
patches-own[dead-count time-dead no-juveniles]

to setup
  clear-all
  setup-patches
  setup-turtles
  reset-ticks
  set dead-total 0
  kill-trees
end

to setup-patches
  ask patches [
    set pcolor white
    set dead-count 0
    set no-juveniles 0]
end

;to setup model

to setup-turtles ;create initial trees
  set-default-shape trees "circle"
  set-default-shape juveniles "circle"
  create-trees n-trees ;creates number of trees set in slider
  [set color green
    set age random-exponential mean-tree-age ;creates trees with ages set in slider
    set tree-size-t1 -1.6 + (35.2 * ln (1 + (0.00061 * (age ^ 1.8)))) ;set tree size as derived from Holzwarth et al. 2013
    set tree-size-t2 0 ;set tree size at time step 2 as zero (this is used for calculation of growth rates)
    set mature-count count trees
    set BA (((tree-size-t1 / 200) ^ 2)  * (3.142)) ;sets intital BA calculated from DBH
    set tree-density count trees in-radius 11.28 ;count number of trees in a radius of 6.35 cells, approximately 40 m^2
    set local-BA ((sum [BA] of trees in-radius 11.28) * 25) ;get per ha BA of trees in a radius of 6.35 cells, approximately 40 m^2
    set size 0.1 * tree-size-t1
    setxy random-xcor random-ycor] ;this locates trees in centre of a random patch
  create-juveniles 82 * n-trees ;creates 20 juveniles for each mature tree
  [set color brown
    set age random-exponential 5 
    set tree_size age * 0.095 ;this determines the size of the seedling based on equations of Collet et al 2001
    set size 0.5
    set tree-density count trees in-radius 11.28;count number of trees in a radius of 6.35 cells, approximately 40 m^2
    set local-BA (sum [BA] of trees in-radius 11.28) * 25 ;get BA of trees in a radius of 6.35 cells, approximately 40 m^2
    set juvenile-count count juveniles
    setxy random-xcor random-ycor]
end

to go
   age-trees
   grow-trees
   age-juveniles
   grow-juveniles
   ifelse random-float 1 <= 0.3
  [set mast-year 1]
  [set mast-year 0]
   reproduce-trees
   kill-trees
   kill-juveniles
   age-patches 
   set dead-total sum[dead-count] of patches 
   tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;1 - TREE REPRODUCTION: Trees over 50 years of age can reproduce in all years but in masting years reproduciton is greatly enhanceed. 
;Number of seedlings produced derived from information in Olesen and Madsen (2008), for more inormation on the method used see Martin et al. (2015)
;Dispersal distance of seedlings derived from information in Hasenkamp (2012)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reproduce-trees
   ask trees [if (mast-year = 1) AND age > 50 [hatch-juveniles 82  ; this simulates masting, number of seeds produced age of masting based on info from Packham et al. 2012. Number of seeds produced and survival rates also based on Packham et al. 2012
    [set age 0
     set tree_size 0
     set color brown
     set size 0.5
    set heading random-float 360
    set disperse-distance random-exponential 12 ;this sets the dispersal distance at a mean of 12 metres, based on info from Packham et al. 2012
    ifelse not any? trees-on patch-ahead disperse-distance [fd disperse-distance][die];to prevent more than one tree occupying each patch; this equates to intraspecific competition     
  ]]]
   ask trees [if (mast-year = 0) AND age > 50 [hatch-juveniles random-normal 2 5  ; this simulates masting, number of seeds produced age of masting based on info from Packham et al. 2012. Number of seeds produced and survival rates also based on Packham et al. 2012
    [set age 0
     set tree_size 0
     set color brown
     set size 0.5
    set heading random-float 360
    set disperse-distance random-exponential 12 ;this sets the dispersal distance at a mean of 12 metres, based on info from Packham et al. 2012
    ifelse not any? trees-on patch-ahead disperse-distance [fd disperse-distance][die];to prevent more than one tree occupying each patch; this equates to intraspecific competition     
  ]]]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;2 - TREE AGING - trees age by 1 year per model tick
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to age-trees ;this ages mature trees by 1 year per tick
  ask trees [set age age + 1] ;increases tree age
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;3 - TREE GROWTH: Mature tree growth varies dependant on its age. The growth of trees was parameterised using the statistical models of Holzwarth et al. (2013)
;This sub-model also calculates the growth rate between years, the basal area of individual trees, the density of trees within an area of 40m squared around tree (equivalent to the plot sizes used in our study), 
;and the basal area of trees within an area of 40m squared around tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to grow-trees ;this increases tree size using equations approximately equivalent to those of Holzwarth et al. 2013
  ;young trees increase in DBH rapidly and this slows in older trees
  ask trees [
    if age < 100 [set tree-size-t2 tree-size-t1 + random-normal 0.4 0.5] ;simulates tree growth for trees <100 years of age
    if (age > 100) AND (age < 200) [set tree-size-t2 tree-size-t1 + random-normal 0.35 0.5] ;simulates tree growth for trees >100 years and <200 years of age
    if (age > 200) [set tree-size-t2 tree-size-t1 + random-normal 0.25 0.5] ;simulates tree growth for trees >200 years of age
     set growth-rate tree-size-t2 - tree-size-t1 ;this sets DBH growth rate over 1 year
     set tree-size-t1 tree-size-t2 ;set tree size at t2 as tree size at t1 for next tick
     set BA ((((tree-size-t1 / 200) ^ 2)  * (3.142))) ;calculates BA from DBH
     set tree-density count trees in-radius 11.28 ;count number of trees in an area equivalent to 20m squared
     set local-BA ((sum [BA] of trees in-radius 11.28) * 25) ;works out the basal area per hectare for each 20m squared area
     set mature-count count trees 
     set mature-BA sum [BA] of trees
     set size 0.1 * tree-size-t1
  ] 
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;4 - MATURE TREE DEATH: The probability of mature tree death is dependant upon tree size and growth rate. 
;A swich allows the user to turn off a component which also causes the probability of mature tree death to increase when close to areas where other trees have died.
;All parameter values were calculated using empirical data from Martin et al. (2015)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to kill-trees
    if dead-total > 1[
  ask trees [set dist-dead distance (min-one-of patches with [dead-count = 1] [distance myself])] ; set the distance to nearest patch wwhere a tree has died
  ]
    ;this determines the survival probability of mature trees based on their size, growth rate and distance to nearest dead tree - derived from the mortality model in the ms
    ifelse spatial-feedback?
    [ask trees
      [
    if (random-float 1) > 1 - ((1 - exp(- exp(-5.029532 + 
            (((tree-size-t1 - 33) / 24 )* (0.2 * DBH-mortality)) + 
            (((growth-rate - 1.9) / 2.5) * (-0.59 * growth-rate-mortality)) +
            (((dist-dead - 30.20579) / 19.34126) * (-0.24 * dist-dead-mortality)) +
            (((1.119015e-17)) * -0.27)
                  )))) [set dead-count 1 set time-dead 0 set pcolor red die]
      ] 
      ]
     [ask trees
      [
    if (random-float 1) > 1 - ((1 - exp(- exp(-5.029532 + 
            (((tree-size-t1 - 33) / 24 )* 0.2) + 
            (((growth-rate - 1.9) / 2.5) * -0.59) +
            (((1.392485e-17)) * -0.24) +
            (((1.119015e-17)) * -0.27)
                  )))) [set dead-count 1 set time-dead 0 set pcolor red die]
      ] 
      ]
  ask trees [if local-BA > 75 [ask trees in-radius 11.28 with-min [tree-size-t1]  [set dead-count 1 set time-dead 0 set pcolor red die]]] ;change this so that instead it kills of all those smaller trees that cause the BA to be >75
  
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;5 - JUVENILE TREE GROWTH: Juveniles grow at different rates dependant on if they germinate in gaps or under canopy. For the purposes of this study we considered closed canopy to represent a basal area equivalent to 20 m sq per ha.
;The growth rates used were derived from Collet et al. (2001).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to grow-juveniles ;simulates seedling growth using equations of Collet et al 2001
  ask juveniles [
    ifelse tree_size < 1.3
    [ifelse local-BA < 20
    [set tree_size tree_size + 0.095] ;seedling growth rates derived from Collet et al 2001
    [set tree_size tree_size + 0.012]
    ]
    [ let tree_size_1 5
      set breed trees
      set color green
      set tree-size-t1 tree_size_1
      set size 0.1 * tree-size-t1
      set age 1
      ]
  ] 
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;6 - JUVENILE DEATH: Juveniles die each year with the probability defined by the user in the "juvenile-mortality" slider
;From our field observations we saw that there was limited recruitment of juveniles in gaps, therefore a switch allows the user to define whether mortality of juveniles differs in gaps differs  from those under a closed canopy
;When switched on this increases annual probability of death for juveniles which do not have a mature tree basal area of >=20 within the surrounding 40 metres squared to die
;In addtion the sub-model limits the number of juveniles that can be found on any single patch to 3, killing the excess. This parameter is based on values from Olesen and Madsen (2008)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to kill-juveniles
  set juvenile-count count juveniles
  ask juveniles [
    set tree-density count trees in-radius 11.28
    set local-BA (sum [BA] of trees in-radius 11.28) * 25
    ]
  ask patches [
    set no-juveniles count juveniles-here
    if no-juveniles >= 3 [ask n-of (no-juveniles - 3) juveniles-here [die]]
    ]
 ifelse differential-juvenile-mortality?
    [ask juveniles [
        if local-BA < 20 [die]
       if random-float 1 > (1 - juvenile-mortality) [die]]]
    [ask juveniles [
        if random-float 1 > (1 - juvenile-mortality) [die]
        ]
    ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;7 - JUVENILE AGING: juveniles age by 1 year per model tick
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to age-juveniles ;age seedlings by 1 year per tick
  ask juveniles [set age age + 1] 
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;8 - PATCH AGING: This submodel calculates the amount of time passed since a mature tree died on a given patch, once this is >=20 the patch sets t

to age-patches
  ask patches[
  if dead-count = 1 AND time-dead < 20 
  [set time-dead time-dead + 1]
  if dead-count = 1 AND time-dead >= 20
  [
    set time-dead 0 
    set dead-count 0 set 
    pcolor white]
  ]
end


; USEFUL REFERENCES
; Olesen, C.R., Madsen, P., 2008. The impact of roe deer (Capreolus capreolus), seedbed, light and seed fall on natural beech (Fagus sylvatica) regeneration. For. Ecol. Manage. 255, 3962–3972. doi:10.1016/j.foreco.2008.03.050
; Hasenkamp, N., Ziegenhagen, B., Mengel, C., Schulze, L., Schmitt, H.P., Liepelt, S., 2011. Towards a DNA marker assisted seed source identification: A pilot study in European beech (Fagus sylvatica L.). Eur. J. For. Res. 130, 513–519. doi:10.1007/s10342-010-0439-3
; Holzwarth, F., Kahl, A., Bauhus, J., Wirth, C., 2013. Many ways to die - partitioning tree mortality dynamics in a near-natural mixed deciduous forest. J. Ecol. 101, 220–230. doi:10.1111/1365-2745.12015
@#$#@#$#@
GRAPHICS-WINDOW
1145
10
1853
739
-1
-1
6.98
1
10
1
1
1
0
1
1
1
0
99
0
99
1
1
1
ticks
30.0

BUTTON
5
20
69
53
NIL
Setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
110
20
173
53
NIL
Go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

PLOT
475
25
675
175
Age structure
Age
Number
0.0
500.0
0.0
20.0
true
false
"" ""
PENS
"default" 10.0 1 -16777216 true "" "histogram [age] of trees"

PLOT
475
205
675
355
Size structure
DBH of trees
Count
0.0
150.0
0.0
10.0
true
false
"" ""
PENS
"default" 5.0 1 -16777216 true "" "histogram [tree-size-t1] of trees"

SLIDER
15
170
187
203
juvenile-mortality
juvenile-mortality
0
1
1
0.1
1
NIL
HORIZONTAL

PLOT
685
25
885
175
Total Basal area
Time
BA per ha
0.0
10.0
0.0
5.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (sum [BA] of trees)"

PLOT
475
470
675
620
Sapling count
Time
Count
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Juveniles" 1.0 0 -5509967 true "" "plot juvenile-count"

SLIDER
15
75
187
108
n-trees
n-trees
0
400
280
1
1
NIL
HORIZONTAL

SLIDER
15
120
187
153
mean-tree-age
mean-tree-age
0
200
96
1
1
NIL
HORIZONTAL

SWITCH
210
75
427
108
differential-juvenile-mortality?
differential-juvenile-mortality?
1
1
-1000

MONITOR
535
400
642
445
NIL
sum [BA] of trees
17
1
11

SWITCH
210
120
362
153
spatial-feedback?
spatial-feedback?
1
1
-1000

MONITOR
695
400
787
445
NIL
juvenile-count
17
1
11

MONITOR
810
400
897
445
NIL
mature-count
17
1
11

PLOT
690
470
890
620
Juveniles height size structure
NIL
NIL
0.0
1.3
0.0
10.0
true
false
"" ""
PENS
"default" 0.1 1 -16777216 true "" "histogram [tree_size] of juveniles"

PLOT
900
25
1100
175
Mean mature tree size
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [tree-size-t1] of trees"

SLIDER
15
215
187
248
DBH-mortality
DBH-mortality
0
10
1
1
1
NIL
HORIZONTAL

SLIDER
15
255
187
288
growth-rate-mortality
growth-rate-mortality
0
10
1
1
1
NIL
HORIZONTAL

SLIDER
15
300
187
333
dist-dead-mortality
dist-dead-mortality
0
10
1
1
1
NIL
HORIZONTAL

@#$#@#$#@
##Purpose
The purpose of this model is to investigate under what conditions loss of tree cover and basal area (BA) might occur in a simplified representation of a New Forest beech woodland. The model concentrates particularly on the pressures caused by grazing (limiting recruitment) and pathogen attack (causing death of mature trees).

##Entities, state variables and scales
The model comprises of two types of entities: grid cells and individuals. Individuals represent beech trees. Each individual is characterised by its location, development stage (seedling, sapling, mature), age (in years), DBH (cm), basal area (m2) and, mean seed dispersal distance (mean distance from the source, in number of cells). 

DBH of mature trees and saplings is derived from the age of trees using an equation for beech growth defined in Holzwarth et al. (2013) and BA defined as (DBH/200)^2×π. All grid cells in the model are considered suitable for individuals. Dispersal distance is a random number drawn from an exponential distribution with a mean of 5. Each grid cell can only contain one mature individual, but may contain multiple seedlings and saplings. The model landscape consists of 100 x 100 grid cells, with each cell representing 1 m2, thus the entire area represents 1 ha. Each model time step represents one year.

##Process overview and scheduling
Initially the distribution of individuals is determined by randomly distributing a number of mature individuals defined by the user. Each individual has an age associated with it that is randomly from an exponential distribution with the mean also defined by the user. At the same time 100 seedlings are randomly distributed across the space. Then in each time step the following events are processed in the given order: increase age of individuals by one year, seed dispersal from mature trees > 20 years old and death.

##Design concepts
The total number of mature trees and basal area emerge from changes in the probability of mature tree mortality that occur as they age, increase in size, differ in growth rates as well as from changes in the mortality of both seedlings and saplings. Interactions between individuals are the result of density dependant mortality processes, which show size asymmetry. This is modelled by defining a maximum number of saplings that can coexist within the area as 274, following Putman et al’s (1989) estimation of sapling density in ungrazed plots in the New Forest.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

horse1
true
0

horse2
false
0
Polygon -7500403 true true 74 228 88 253 131 288 150 289 131 270 102 244 102 233 93 183 63 116 45 122 30 153 31 167 38 218 50 244 47 273 28 293 54 293 63 276 65 264 69 245
Polygon -7500403 true true 216 73 219 56 229 42 237 66 226 71
Polygon -7500403 true true 181 108 213 71 226 64 257 72 260 91 285 112 272 126 234 118 218 136 209 152 204 165 192 180 169 187 150 197 135 197 89 182 69 168 63 115 124 112 160 113 170 106
Polygon -6459832 true false 252 143 242 141
Polygon -6459832 true false 254 136 232 137
Line -16777216 false 75 225 89 180
Polygon -6459832 true false 262 138 234 149
Polygon -7500403 true true 50 121 36 119 24 123 14 128 6 143 8 165 8 181 7 197 4 233 23 201 28 184 30 169 28 153 48 145
Polygon -7500403 true true 223 75 226 58 245 44 244 68 233 73
Line -16777216 false 89 181 112 185
Line -16777216 false 31 150 47 118
Polygon -16777216 true false 235 90 250 91 255 99 248 98 244 92
Line -16777216 false 236 112 246 119
Polygon -16777216 true false 278 119 282 116 274 113
Line -16777216 false 189 201 203 161
Line -16777216 false 218 251 219 257
Polygon -16777216 true false 230 67 228 54 222 62 224 72
Line -16777216 false 246 67 234 64
Line -16777216 false 229 45 235 68
Line -16777216 false 30 150 30 165
Polygon -7500403 false true 168 164 161 165 163 165 179 216 159 251 147 251 150 258 165 265 193 221 177 159 213 209 215 263 229 263 227 205 194 154 179 162 159 166
Rectangle -7500403 true true 181 201 181 202

horse3
false
0
Polygon -7500403 true true 36 136 52 104 60 94 79 90 98 93 114 98 137 99 165 95 199 86 209 75 218 65 240 26 234 45 239 46 253 28 249 46 257 51 268 61 280 76 271 90 245 82 238 87 235 94 224 113 220 122 223 128 223 143 222 158 234 181 242 216 249 244 243 251 234 245 233 221 225 186 199 163 208 205 186 234 181 245 169 247 168 233 176 233 197 201 183 150 167 160 144 163 92 149 80 167 77 194 73 211 103 234 110 246 99 247 91 244 91 237 64 219 62 169 41 194 27 249 15 251 15 246 24 237 31 184 43 168 42 121 58 96 38 101 21 129 12 169 22 192 25 162
Polygon -7500403 true true 269 87 279 85 279 76 271 85
Polygon -7500403 true true 221 52 205 54 213 60 195 63 206 69 183 73 196 78 165 86 184 90 206 85 226 56
Line -7500403 true 219 72 228 79
Line -7500403 true 214 66 212 70
Line -7500403 true 228 82 228 83
Line -7500403 true 214 64 229 82
Polygon -7500403 true true 216 71 230 82 226 82

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.1.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Experiment_1" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="150"/>
    <metric>sum [BA] of trees</metric>
    <enumeratedValueSet variable="spatial-feedback?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-trees">
      <value value="280"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-tree-age">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="differential-juvenile-mortality?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="juvenile-mortality">
      <value value="0"/>
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
1
@#$#@#$#@
