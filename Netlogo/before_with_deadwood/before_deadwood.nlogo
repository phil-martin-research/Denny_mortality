;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEFORE - Beech model
;; based on: Neuert, C. 1999; Rademacher, C. et al. 2004
;; author: Jan C. Thiele
;; contact: jthiele@gwdg.de
;; date: 04 july 2015
;;
;; file: before_with_deadwood.nlogo
;; description: main file, model engine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


breed [beechesIII beechIII]  ;; beeches of the lower crown layer
breed [beechesIV beechIV]    ;; beeches of the upper crown layer
breed [sdeads sdead]         ;; standing deadwood 
breed [ldeads ldead]         ;; lying deadwood on the ground
breed [ddlines ddline]       ;; helper; lines for visualization of dominance degrees of different horizontal layers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load extensions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

extensions [ ]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; include files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

__includes [
            "wind_interaction.nls" "parameter.nls" "initialization.nls" "light_interaction.nls" "gap_closing.nls" "beechesIV_change.nls"
            "beechesIII_change.nls" "layerII_change.nls" "layerI_change.nls" "reproduction.nls" "patch_state.nls" "plots.nls"
            "wind_from_file.nls" "deadwood.nls"
           ]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup procedure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; setup everything
to setup
  ;; i. reset everything
     clear-all
 
  set-default-shape beechesIII "beechIII"
  set-default-shape beechesIV "beechIV"
  set-default-shape sdeads "sdead"
  set-default-shape ldeads "ldead"
    
  ;; ii. parameterize the model
     parameterize      
  ;; iii. initialize the model
    ;; a. initialization of wind direction probabilities
       ifelse (wind_from_file?)
       [ show "wind from file...."
         load_wind_list_from_file 
       ]
       [ initialize_winddirectionprob ]
    ;; b. initialization of all cells
       initialize_forest   
 
  ;; iv. neighborhood intercation: light
     ask patches [ light_interaction ] 
    
  ;; set development states
    set_state_single_patch   ;; patch state without neighbourhood filter 
    set_state_patch_with_neighbours  ;; patch state with neighbourhood filter

  ;; create agentset of inner forest
     set boundarylist patches with [((pxcor <= (max-pxcor - boundary)) and
                                    (pxcor >= (min-pxcor + boundary)) and
                                    (pycor <= (max-pycor - boundary)) and
                                    (pycor >= (min-pycor + boundary))) 
                                   ]  
  
  if (switch-off-wind) [
    set pWindOccurence 0.0
  ]
  
  reset-ticks
  if (plot_results?)
  [
    plots 
    colorize_patch_stateneighbour
    ;colorize_patch_state ;; patch colorization without neighbourhood filter
  ]  
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go procedure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; go procedure - one step ahead
to go
  ;; reset timer
     if ticks = 0 [reset-timer]
  ;; stop simulation after ... steps
     if ticks >= max_ticks [ show timer stop ]

  ;; initialize the wind
  ;; reset wind variables
     set wd []
     set windstrength 0.0
     set windhere ""  
  ;; get winddirection
     ifelse (wind_from_file?)
     [ get_wind_from_list ]
     [ get_winddirection ]    
  
  ;; update deadwood
     update_deadwood
     
  ;; i. loop for one simulation period over all patches
        ask patches 
        [           
          ;; b. gap closing within main canopy layer (layer IV)
             gap_closing
          ;; c. growth of trees of main canopy layer (layer IV)
             beechesIV_change
          ;; d. growth of trees of lower canopy layer (layer III)
             beechesIII_change
          ;; e. gwoth of young trees (layer II)
             layerII_change
          ;; f. growth of seedlings (layer I)
             layerI_change
          ;; reset wind variable
             set ptilt 0.0
        ]


  ;; g. neighborhood interaction: wind
  if ((random-float 1.0) < pWindOccurence)
  [    
     ;; calculate tipover probability with neighbour interaction
     ask patches 
     [
       windbreakprobability
     ]

     ; create agentset of sorted patches
     let patchlist [] 
     ifelse (member? windhere ["E" "SE" "S" "SW"])
     [
       ; for "E" "SE" "S" SW"
       ; sort x 0 to max, y max to 0
       set patchlist patchlist_wind1
       ;set patchlist sort-by [[pxcor] of ?1 > [pxcor] of ?2] patches
       ;set patchlist sort-by [[pycor] of ?1 < [pycor] of ?2] patchlist
     ]
     [
       ; for "W" "NW" "N" "NE"
       ; sort y max to 0, x 0 to max
       set patchlist patchlist_wind2
       ;set patchlist sort-by [[pxcor] of ?1 < [pxcor] of ?2] patches
       ;set patchlist sort-by [[pycor] of ?1 > [pycor] of ?2] patchlist
     ]
       
     ;; execute wind-interaction / wind break in given order of cells   
     foreach patchlist
     [
       ask ? 
       [
         wind_interaction
       ]
     ]  
  ]
  
  
  ;; just when there are beechIII or beechIV trees somewhere in the forest regeneration takes place
  let mother-trees false
  if ((count beechesIII + count beechesIV) > 0)
  [ set mother-trees true ]
  
  ask patches 
  [ 
    ;; h. neighborhood interaction: light
       light_interaction
    ;; i. reproduction
       if (mother-trees)
       [ reproduction ]
  ]

  ;; increment the age of the deadwood
     ageincrement_deadwood
     
  ;; set development states
     set_state_single_patch
     set_state_patch_with_neighbours  
  
  ;; ii. increase tick counter and year
     tick
     set year ticks * simstep_width

  if (plot_results?)
  [
    plots 
    colorize_patch_stateneighbour
    ;colorize_patch_state
  ]


end
@#$#@#$#@
GRAPHICS-WINDOW
782
12
1240
491
-1
-1
8.3
1
10
1
1
1
0
0
0
1
0
53
0
53
1
1
1
ticks
30.0

BUTTON
5
61
123
94
NIL
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
5
100
123
133
NIL
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
1

CHOOSER
5
10
160
55
initial_state
initial_state
"virgin" "harvested" "selection cutting" "random"
0

BUTTON
10
671
231
705
colorize patches (depending on neighbour)
colorize_patch_stateneighbour
NIL
1
T
OBSERVER
NIL
C
NIL
NIL
1

BUTTON
10
631
230
664
colorize patches
colorize_patch_state
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
247
13
776
231
percentage of states (w/wo neighbourhood filter)
time
%
0.0
200.0
0.0
100.0
true
true
"" ""
PENS
"state1" 1.0 2 -1184463 true "" ""
"state2" 1.0 2 -10899396 true "" ""
"state3" 1.0 2 -2674135 true "" ""
"state1neighbour" 1.0 0 -1184463 true "" ""
"state2neighbour" 1.0 0 -10899396 true "" ""
"state3neighbour" 1.0 0 -2674135 true "" ""

PLOT
247
235
775
421
percent optimal state
time
%
0.0
200.0
0.0
100.0
true
false
"" ""
PENS
"state2neighborperc" 1.0 0 -10899396 true "" ""
"state2perc" 1.0 2 -10899396 true "" ""

PLOT
247
427
775
646
percent dominance degree
time
%
0.0
200.0
0.0
100.0
true
true
"" ""
PENS
"ddIperc" 1.0 0 -2064490 true "" ""
"ddIIperc" 1.0 0 -612749 true "" ""
"ddIIIperc" 1.0 0 -4528153 true "" ""
"ddIVperc" 1.0 0 -8330359 true "" ""
"strongwind" 1.0 1 -2674135 true "" ""
"middlewind" 1.0 1 -1604481 true "" ""

PLOT
518
651
775
802
beechesIV age histogram (without boundary)
age
NIL
0.0
400.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

BUTTON
9
443
126
476
NIL
show_layerI
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
9
482
126
515
NIL
show_layerII
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
8
520
126
553
NIL
show_layerIII
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
8
559
127
592
NIL
show_layerIV
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
783
493
995
643
state histogram
NIL
NIL
1.0
4.0
0.0
10.0
true
false
"" ""
PENS
"statescount" 1.0 1 -16777216 true "" ""

TEXTBOX
9
375
180
408
Display dominance degree\n-----------------------
12
0.0
1

BUTTON
9
404
125
437
NIL
reset_display
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
12
598
251
630
Display development state of patch\n------------------------------
12
0.0
1

PLOT
784
652
1029
802
beechesIV crownarea histogram (without boundary)
crownarea
NIL
1.0
9.0
0.0
10.0
true
false
"" ""
PENS
"crownarea" 1.0 1 -16777216 true "" ""

PLOT
247
651
509
801
beechesIII age histogram (without boundary)
age
NIL
0.0
330.0
0.0
10.0
true
false
"" ""
PENS
"age" 1.0 1 -16777216 true "" ""

SWITCH
6
139
183
172
Plot_results?
Plot_results?
0
1
-1000

OUTPUT
250
893
625
1301
12

SLIDER
250
851
382
884
subpatch-xcor
subpatch-xcor
min-pxcor
max-pxcor
28
1
1
NIL
HORIZONTAL

BUTTON
570
851
625
884
show
show-subplot
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
251
812
401
842
Subplot analysis\n---------------
12
0.0
1

SLIDER
407
852
541
885
subpatch-ycor
subpatch-ycor
min-pycor
max-pycor
42
1
1
NIL
HORIZONTAL

TEXTBOX
12
714
162
744
Further Settings\n------------------------
12
0.0
1

MONITOR
1000
594
1167
643
time: winddir.: windstrength
wind_info
0
1
12

SWITCH
7
224
184
257
distribute_trees
distribute_trees
1
1
-1000

SWITCH
7
263
184
296
visual_tree_size
visual_tree_size
1
1
-1000

SWITCH
13
749
200
782
wind_from_file?
wind_from_file?
1
1
-1000

INPUTBOX
13
789
200
849
wind_file_name
test.wind
1
0
String

PLOT
630
807
1030
1046
number of giants
time
count giants
0.0
200.0
0.0
10.0
true
true
"" ""
PENS
"size" 1.0 0 -10899396 true "" ""
"age" 1.0 0 -13345367 true "" ""
"age or size" 1.0 0 -2674135 true "" ""
"age and size" 1.0 0 -16777216 true "" ""

INPUTBOX
7
301
76
361
boundary
8
1
0
Number

MONITOR
1038
1103
1177
1148
count ldead per ha
meanldeadcount / ((((max-pxcor + 1) * 14) * ((max-pycor + 1) * 14)) / 10000)
2
1
11

MONITOR
1037
1052
1178
1097
count sdead per ha
meansdeadcount / ((((max-pxcor + 1) * 14) * ((max-pycor + 1) * 14)) / 10000)
2
1
11

PLOT
631
1052
1032
1300
Volume of deadwood
time
m^3
0.0
200.0
0.0
10.0
true
true
"" ""
PENS
"lying_IIIbeech" 1.0 0 -955883 true "" ""
"standing_IIIbeech" 1.0 0 -6459832 true "" ""
"lying_IVbeech" 1.0 0 -11221820 true "" ""
"standing_IVbeech" 1.0 0 -13345367 true "" ""

MONITOR
1038
1153
1177
1198
vol sdead per ha
meansdeadvol / ((((max-pxcor + 1) * 14) * ((max-pycor + 1) * 14)) / 10000)
2
1
11

MONITOR
1038
1203
1177
1248
vol ldead per ha
meanldeadvol / ((((max-pxcor + 1) * 14) * ((max-pycor + 1) * 14)) / 10000)
2
1
11

MONITOR
1038
652
1165
697
NIL
meangrowuppercent
2
1
11

MONITOR
1038
701
1165
746
NIL
meanoptimalpercent
2
1
11

MONITOR
1038
750
1165
795
NIL
meandecaypercent
2
1
11

PLOT
1035
806
1464
1045
mean min. distance of giants
time
distance
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"size" 1.0 0 -10899396 true "" ""
"age" 1.0 0 -13345367 true "" ""
"age or size" 1.0 0 -2674135 true "" ""
"age and size" 1.0 0 -16777216 true "" ""

PLOT
1170
558
1666
794
g0.8 of giants
time
g0.8
0.0
200.0
0.0
30.0
true
true
"" ""
PENS
"age" 1.0 0 -13345367 true "" ""
"age_interpol" 1.0 2 -13345367 true "" ""
"size" 1.0 0 -10899396 true "" ""
"size_interpol" 1.0 2 -10899396 true "" ""

INPUTBOX
80
302
185
362
max_ticks
300
1
0
Number

CHOOSER
6
175
184
220
tree_visualization
tree_visualization
"trees" "pies" "lines" "none"
3

@#$#@#$#@
## WHAT IS IT?

This section could give a general understanding of what the model is trying to show or explain.

## HOW IT WORKS

This section could explain what rules the agents use to create the overall behavior of the model.

## HOW TO USE IT

This section could explain how to use the model, including a description of each of the items in the interface tab.

## THINGS TO NOTICE

This section could give some ideas of things for the user to notice while running the model.

## THINGS TO TRY

This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.

## EXTENDING THE MODEL

This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.

## NETLOGO FEATURES

This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.

## RELATED MODELS

This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.

## CREDITS AND REFERENCES

This section could contain a reference to the model's URL on the web if it has one, as well as any other necessary credits or references.
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

beechiii
false
15
Circle -955883 true false 130 75 70
Rectangle -6459832 true false 150 195 165 300
Circle -955883 true false 80 81 108
Circle -955883 true false 118 118 92
Circle -955883 true false 90 135 90
Circle -955883 true false 147 87 66

beechiv
false
5
Circle -13840069 true false 133 18 94
Rectangle -6459832 true false 135 195 165 300
Circle -13840069 true false 65 36 108
Circle -13840069 true false 116 41 127
Circle -13840069 true false 60 90 120
Circle -13840069 true false 89 59 152

biii_1
true
0
Polygon -955883 true false 151 150 254 45 243 34 223 20 209 12 198 6 176 2 162 1 151 0
Polygon -16777216 false false 150 0 150 150 255 45 240 30 224 20 207 10 194 5 176 2

biii_2
true
0
Polygon -955883 true false 150 150 150 0 171 2 194 5 213 13 232 24 249 38 264 52 278 72 290 94 294 109 297 121 300 150
Polygon -16777216 false false 150 150 150 0 173 0 192 3 210 10 223 18 231 24 244 32 258 46 268 56 275 66 285 83 292 96 295 108 298 125 299 149

biii_3
true
0
Polygon -955883 true false 150 150 257 257 271 239 277 229 284 218 293 198 299 176 299 131 297 112 290 90 275 65 252 39 232 24 211 13 187 6 165 1 150 0
Polygon -16777216 false false 150 150 150 0 174 3 193 7 211 13 230 23 242 30 254 41 265 51 273 63 282 74 291 91 295 104 298 116 299 134 301 154 300 172 297 189 292 201 285 216 277 230 267 244 256 256

biii_4
true
0
Polygon -955883 true false 150 300 150 0 168 1 188 4 206 10 220 17 237 27 256 41 266 53 275 66 283 78 290 91 295 109 299 123 299 141 299 166 298 181 294 195 288 215 277 232 264 248 248 264 229 279 213 286 190 295 178 298
Polygon -16777216 false false 150 300 150 0 171 1 193 5 210 11 230 21 247 34 257 41 268 54 276 64 283 77 289 91 293 101 297 114 298 124 299 151 299 175 298 188 294 201 288 217 277 233 266 246 254 260 238 272 221 283 203 291 183 297 167 298

biii_5
true
0
Polygon -955883 true false 150 150 45 255 59 270 68 276 81 284 95 291 119 299 138 301 157 300 173 298 196 294 216 287 232 277 248 265 269 246 284 222 293 200 298 183 299 160 299 132 297 107 288 87 276 67 260 45 243 29 220 17 203 9 183 4 165 1 151 1
Polygon -16777216 false false 43 255 149 149 150 -1 170 1 190 4 201 7 218 14 229 20 239 27 249 34 260 43 267 53 274 63 283 76 290 91 298 107 299 122 300 146 299 165 299 177 296 190 291 206 284 222 276 236 265 247 251 263 237 274 223 282 206 290 187 295 160 301 125 299 101 293 79 283 65 275 52 262

biii_6
true
0
Polygon -955883 true false 0 150 150 150 150 0 171 2 192 5 209 11 229 19 243 30 261 48 277 69 291 97 297 117 299 133 299 145 299 162 299 180 292 204 284 223 274 235 265 247 253 262 240 271 222 283 207 291 188 297 174 299 155 299 127 299 105 295 77 284 56 268 38 250 21 228 9 202 3 179 0 160
Polygon -16777216 false false -2 150 148 150 148 0 172 2 186 3 203 8 214 12 229 20 244 31 253 41 260 47 269 60 275 66 281 77 287 88 293 103 296 115 299 130 300 144 300 163 300 175 298 184 295 198 290 209 284 223 276 233 268 244 257 257 250 264 240 272 229 279 218 286 210 290 199 295 188 297 167 300 148 300 122 299 106 297 82 287 68 280 49 263 35 247 18 226 11 208 4 190 0 170

biii_7
true
0
Polygon -955883 true false 150 150 45 45 34 57 25 70 18 82 11 97 6 111 2 128 1 142 0 153 2 179 6 197 12 214 22 229 31 242 43 253 55 266 68 275 78 282 100 292 119 298 139 299 155 299 175 298 196 293 212 287 234 276 252 262 268 245 281 224 292 202 299 176 299 118 291 89 279 70 267 53 250 36 229 22 210 11 187 4 168 0 150 0
Polygon -16777216 false false 150 0 150 150 45 45 40 49 34 56 25 69 17 80 11 94 6 107 3 119 0 133 -2 147 -1 161 0 169 2 178 4 190 6 199 9 208 13 216 22 232 29 241 40 252 49 261 58 268 70 277 81 282 91 288 110 295 120 298 136 299 154 299 172 298 192 295 212 287 226 281 245 269 256 259 268 246 280 226 291 204 297 188 299 170 300 150 299 130 298 113 294 98 291 88 281 72 272 58 261 47 249 36 235 25 222 17 208 9 191 4 176 1 161 0

biii_8
true
0
Circle -955883 true false 0 0 300
Circle -16777216 false false 0 0 300

biv_1
true
0
Polygon -13840069 true false 151 150 254 45 243 34 223 20 209 12 198 6 176 2 162 1 151 0
Polygon -16777216 false false 150 0 150 150 255 45 240 30 224 20 207 10 194 5 176 2

biv_2
true
0
Polygon -13840069 true false 150 150 150 0 171 2 194 5 213 13 232 24 249 38 264 52 278 72 290 94 294 109 297 121 300 150
Polygon -16777216 false false 150 150 150 0 173 0 192 3 210 10 223 18 231 24 244 32 258 46 268 56 275 66 285 83 292 96 295 108 298 125 299 149

biv_3
true
0
Polygon -13840069 true false 150 150 257 257 271 239 277 229 284 218 293 198 299 176 299 131 297 112 290 90 275 65 252 39 232 24 211 13 187 6 165 1 150 0
Polygon -16777216 false false 150 150 150 0 174 3 193 7 211 13 230 23 242 30 254 41 265 51 273 63 282 74 291 91 295 104 298 116 299 134 301 154 300 172 297 189 292 201 285 216 277 230 267 244 256 256

biv_4
true
0
Polygon -13840069 true false 150 300 150 0 168 1 188 4 206 10 220 17 237 27 256 41 266 53 275 66 283 78 290 91 295 109 299 123 299 141 299 166 298 181 294 195 288 215 277 232 264 248 248 264 229 279 213 286 190 295 178 298
Polygon -16777216 false false 150 300 150 0 171 1 193 5 210 11 230 21 247 34 257 41 268 54 276 64 283 77 289 91 293 101 297 114 298 124 299 151 299 175 298 188 294 201 288 217 277 233 266 246 254 260 238 272 221 283 203 291 183 297 167 298

biv_5
true
0
Polygon -13840069 true false 150 150 45 255 59 270 68 276 81 284 95 291 119 299 138 301 157 300 173 298 196 294 216 287 232 277 248 265 269 246 284 222 293 200 298 183 299 160 299 132 297 107 288 87 276 67 260 45 243 29 220 17 203 9 183 4 165 1 151 1
Polygon -16777216 false false 43 255 149 149 150 -1 170 1 190 4 201 7 218 14 229 20 239 27 249 34 260 43 267 53 274 63 283 76 290 91 298 107 299 122 300 146 299 165 299 177 296 190 291 206 284 222 276 236 265 247 251 263 237 274 223 282 206 290 187 295 160 301 125 299 101 293 79 283 65 275 52 262

biv_6
true
0
Polygon -13840069 true false 0 150 150 150 150 0 171 2 192 5 209 11 229 19 243 30 261 48 277 69 291 97 297 117 299 133 299 145 299 162 299 180 292 204 284 223 274 235 265 247 253 262 240 271 222 283 207 291 188 297 174 299 155 299 127 299 105 295 77 284 56 268 38 250 21 228 9 202 3 179 0 160
Polygon -16777216 false false -2 150 148 150 148 0 172 2 186 3 203 8 214 12 229 20 244 31 253 41 260 47 269 60 275 66 281 77 287 88 293 103 296 115 299 130 300 144 300 163 300 175 298 184 295 198 290 209 284 223 276 233 268 244 257 257 250 264 240 272 229 279 218 286 210 290 199 295 188 297 167 300 148 300 122 299 106 297 82 287 68 280 49 263 35 247 18 226 11 208 4 190 0 170

biv_7
true
0
Polygon -13840069 true false 150 150 45 45 34 57 25 70 18 82 11 97 6 111 2 128 1 142 0 153 2 179 6 197 12 214 22 229 31 242 43 253 55 266 68 275 78 282 100 292 119 298 139 299 155 299 175 298 196 293 212 287 234 276 252 262 268 245 281 224 292 202 299 176 299 118 291 89 279 70 267 53 250 36 229 22 210 11 187 4 168 0 150 0
Polygon -16777216 false false 150 0 150 150 45 45 40 49 34 56 25 69 17 80 11 94 6 107 3 119 0 133 -2 147 -1 161 0 169 2 178 4 190 6 199 9 208 13 216 22 232 29 241 40 252 49 261 58 268 70 277 81 282 91 288 110 295 120 298 136 299 154 299 172 298 192 295 212 287 226 281 245 269 256 259 268 246 280 226 291 204 297 188 299 170 300 150 299 130 298 113 294 98 291 88 281 72 272 58 261 47 249 36 235 25 222 17 208 9 191 4 176 1 161 0

biv_8
true
0
Circle -13840069 true false 0 0 300
Circle -16777216 false false 0 0 300

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

dd100
true
0
Rectangle -7500403 true true 0 143 300 158

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

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

ldead
false
0
Rectangle -6459832 true false 90 255 195 285
Polygon -6459832 true false 195 285 210 270 195 255
Polygon -6459832 true false 90 255 75 270 90 285

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

sdead
false
0
Rectangle -6459832 true false 135 180 165 285
Polygon -6459832 true false 135 180 135 165 150 150 165 165 165 180

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
NetLogo 5.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
0
@#$#@#$#@
