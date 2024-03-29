globals []
breed [trees tree]
breed [seedlings seedling]
trees-own [age BA tree_size disperse-distance nhbs n-nhbs]
seedlings-own [age tree_size disperse-distance]

to setup
  clear-all
  setup-patches
  setup-turtles
  reset-ticks
  
end

to setup-patches
  ask patches [set pcolor white]
end

to setup-turtles
  set-default-shape trees "circle"
  set-default-shape seedlings "circle"
  create-trees 70
  [set color green
    set age random-normal 160 40
    set tree_size age * 0.2
    set size 1.5
    setxy random-xcor random-ycor] ;this locates trees in centre of a random patch
  create-seedlings 6500
  [set color brown
    set age random-exponential 5
    set tree_size age * (random-normal 0.05 0.02)
    set size 0.5
    setxy random-xcor random-ycor]
end

to go
   age-trees
   grow-trees
   age-seedlings
   grow-seedlings
   reproduce-trees
   kill-trees
   kill-seedlings
   tick
end

to reproduce-trees
  ask trees [if (random-float 1 > 0.5) and (age > 20)
  [hatch-seedlings random 50
    [set age 0
     set tree_size 0.02
     set color brown
     set size 0.5
    set heading random-float 360
    set disperse-distance random-exponential 10 ;this value sets the mean of a negative exponential distribution
    ifelse not any? trees-on patch-ahead disperse-distance [fd disperse-distance][die];to prevent more than one tree occupying each patch; this equates to intraspecific competition  
    ;move-to patch-here ;this makes sure that juveniles are centred on a patch 
      
  ]]]

end

to age-trees
  ask trees [set age age + 1] ;increases tree age
    ;if age > 300 [die]] ;kills trees over 500 years age
end

to grow-trees
  ask trees [
    ifelse age < 100
    [set tree_size tree_size + 0.5 + ((random-float 0.2) - 0.1)]
    [set tree_size tree_size + 0.3 + ((random-float 0.2) - 0.1)] ;simulates tree growth so that young trees grow more quickly than old trees
  ] 
end

to kill-trees
  ask trees[
  ifelse tree_size > 5
  [if (random-float 1) > (1 - exp(- exp((tree_size - 33) / 24 )* 0.2)) [die]];this is derived from the mortality model in the ms
  [if (random-float 1) > (1 - sapling-mortality) [die]]
  set BA (((tree_size / 200) ^ 2)  * (3.142))
  ]
end

to age-seedlings
  ask seedlings [set age age + 1] 
end

to grow-seedlings
  ask seedlings [
    ifelse tree_size < 5
    [set tree_size tree_size + 0.5 + ((random-float 0.4) - 0.02)]
    [set breed trees
      set color green
      set size 1.5] ;simulates seedling growth using equations of Collet et al 2001
  ] 
end

to kill-seedlings
  ask seedlings [if random-float 1 > (1 - seedling-mortality) [die]]
  ;ask seedlings [trees in-radius 3]
end
@#$#@#$#@
GRAPHICS-WINDOW
485
30
859
425
-1
-1
3.604
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
100
0
100
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
240
15
440
165
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
240
195
440
345
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
"default" 10.0 1 -16777216 true "" "histogram [tree_size] of trees"

SLIDER
15
95
187
128
seedling-mortality
seedling-mortality
0
1
0.65
0.01
1
NIL
HORIZONTAL

PLOT
905
10
1105
160
Total Basal area
Time
Count
0.0
10.0
0.0
5.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (sum [BA] of trees)"

SLIDER
15
150
187
183
sapling-mortality
sapling-mortality
0
1
0.48
0.01
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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
