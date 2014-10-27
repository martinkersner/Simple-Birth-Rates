globals
[
  women-count
  men-count
  binary-length
]

turtles-own
[
  age
  appearance
  intelligence
]

breed [women woman]
breed [men man]

to setup
  clear-output
  setup-experiment
end

to setup-experiment
  cp ct
  clear-all-plots
  reset-ticks
  
  ; initialize women
  create-women women-capacity [
    setxy random-xcor random-ycor
    set color red
    set size 2
    set age 0
    set appearance crg
    set intelligence crg
  ]
  
  ; initialize men
  create-men men-capacity [
    setxy random-xcor random-ycor
    set color blue    
    set size 2
    set age 0
    set appearance crg
    set intelligence crg    
  ]
  
  ; length of genom
  set binary-length 10
end

; repeating procedure of life
to go
  move
  reproduce
  get-older
  pass-away
  selection
  tick
end

; perform move in each life cycle
to move
  ask turtles 
  [
    rt random-float 30 - random-float 30
    fd 1
  ]
end

to reproduce
  ask patches 
  [ ; men and women require private, they cannot share a patch with anybody else   
    if (count men-here = 1) and (count women-here = 1)
    [
      let age-man 0
      let age-woman 0
      ask men [set age-man age]
      ask women [set age-woman age]
      
      ; people older than 15 years old can reproduce
      if (age-man >= 15) and (age-woman >= 15)
      [
        ; get appearance of man and woman
        let ma 0
        let wa 0
        ask men [set ma appearance]
        ask women [set wa appearance]
        
        ; get intelligence of man and woman
        let mi 0
        let wi 0
        ask men [set mi intelligence]
        ask women [set wi intelligence]
      
        ; the difference of intelligence and appearance between man and woman cannot exceed set maximum differences
        if ((genom-diff ma  wa) < appearance-diff) and ((genom-diff mi  wi) < intelligence-diff)
        [
          ask turtles-here 
          [; generate random number of kids and their gender
            let kc kids-count
            let gr gender-random
          
            ; create new women
            ifelse (gr = "women") 
            [ hatch-women kc 
              [
                set color red
                set age 0

                ; cross and mutate appearance/intelligence of mother and father
                set appearance cross&mutate wa ma
                set intelligence cross&mutate wi mi
              ] 
            ]
            ; create new man
            [ hatch-men kc 
              [
                set color blue
                set age 0
                
                ; cross and mutate appearance/intelligence of mother and father
                set appearance cross&mutate ma wa
                set intelligence cross&mutate mi wi
              ] 
            ]
          ]
        ]
      ]
    ]
  ]
end

; people are getting older at each tick of clock
to get-older
  ask turtles 
  [ 
    set age age + 1
    
    ; people are getting prettier until 25 years old, then their appearance is getting lower
    ifelse (age <= 25)
    [set appearance fill-binary integer-to-binary ((binary-to-integer appearance) + appearance-plus-minus)]
    [set appearance fill-binary integer-to-binary ((binary-to-integer appearance) - appearance-plus-minus)]
    
    ; people are getting wiser until 25 years old, then their intelligence is getting lower
    ifelse (age <= 55)
    [set intelligence fill-binary integer-to-binary ((binary-to-integer intelligence) + intelligence-plus-minus)]
    [set intelligence fill-binary integer-to-binary ((binary-to-integer intelligence) - intelligence-plus-minus)]    
  ]
end

; test if person should die according to generated life span
to pass-away
  ask turtles 
  [
    if (age > die-random) [ die ]
    
    if (women-count <= 0) [ stop ]
  ]
end

; generate number of kids
to-report kids-count
  let _mean 1
  let _std  2
  report abs floor random-normal _mean _std
end

; http://en.wikipedia.org/wiki/Sex_ratio
; As of 2014, the global sex ratio at birth is estimated at 107 boys to 100 girls.
to-report gender-random
  let gr random-float 1
  ifelse (gr < 0.507)
  [ report "men" ]
  [ report "women" ]
end

; http://en.wikipedia.org/wiki/Life_expectancy
; the 2010 world average was 67.2
to-report die-random
  report random-normal 67.2 13
end

; convert binary number to integer
to-report binary-to-integer [bits] 
  report reduce [?1 * 2 + ?2] bits 
end

; convert integer to binary number
to-report integer-to-binary [integer]
  let bits remainder integer 2
  set integer floor (integer / 2)

  while [integer > 0]
  [
    set bits sentence (remainder integer 2) bits
    set integer floor (integer / 2)
  ]
  
  report bits
end

; create genom with 10 binary numbers 
; used for appearance
to-report crg
    report (list (random 2) (random 2) (random 2) (random 2) (random 2) (random 2) (random 2) (random 2) (random 2) (random 2))
end

; binary comparison of two lists
to-report hamming-distance [g1 g2]
  let d 0
  
  (foreach g1 g2 [ set d d + (abs (?1 - ?2)) ])
  
  report d
end

; cross and mutate two given genoms
; assumes that both g1 and g2 have same length
to-report cross&mutate [g1 g2]
  let l length g1
  let p random l ; point of crossing
  
  let i 1
  let g_new item i g1
  
  foreach g1 
  [
    if (i <= p) 
    [ set g_new sentence g_new item i g1 ]
    
    set i i + 1
   ]
  
  let j 0
  foreach g2 
  [
    if (j > p) 
    [ set g_new sentence g_new item j g2 ]
    
    set j j + 1
   ]
  
  set g_new mutate g_new
  
  report g_new
end

to-report mutate [l]
  let i random binary-length
  let v random 2
  
  report replace-item i l v
end

; selection of new population according to appearance
to selection
  let count-all count turtles

  if count-all > max-capacity
  [
    let count-kill abs (count-all - max-capacity)
    ask min-n-of count-kill turtles [binary-to-integer appearance]
    [ die ] 
  ]
end

; counts the difference between two binary number
; the difference is counted with decimal numbers
to-report genom-diff [g1 g2]
  let i1 binary-to-integer g1
  let i2 binary-to-integer g2
  
  report abs (i1 - i2)
end

; fills binary number with zeros to full binary number
to-report fill-binary [g]
  let l 0
  
  ifelse (is-list? g)
  [set l length g]
  [
    if (g = 0) or (g = 1) [set l 1]
  ]
    
  while [l < binary-length]
  [
   set g sentence 0 g
   set l (l + 1) 
  ]
  
  report g
end
@#$#@#$#@
GRAPHICS-WINDOW
290
10
704
445
50
50
4.0
1
10
1
1
1
0
1
1
1
-50
50
-50
50
1
1
1
ticks
30.0

BUTTON
66
79
126
112
setup
setup
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
130
79
190
112
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
4
226
284
445
Populations
Generations
Population
0.0
50.0
0.0
1200.0
true
true
"set-plot-y-range 0 floor (1 + men-count * 1.2)" ""
PENS
"Women" 1.0 0 -2674135 true "" "set women-count count women\nplot women-count"
"Men" 1.0 0 -13345367 true "" "set men-count count men\nplot men-count"
"Total" 1.0 0 -10899396 true "" "plot count turtles"

MONITOR
718
95
794
140
# women
women-count
3
1
11

MONITOR
814
96
891
141
# men
men-count
3
1
11

SLIDER
716
13
888
46
men-capacity
men-capacity
0
1000
250
1
1
NIL
HORIZONTAL

SLIDER
717
53
889
86
women-capacity
women-capacity
0
1000
250
1
1
NIL
HORIZONTAL

SLIDER
720
149
892
182
appearance-diff
appearance-diff
0
1024
500
1
1
NIL
HORIZONTAL

SLIDER
723
237
895
270
max-capacity
max-capacity
0
2000
500
1
1
NIL
HORIZONTAL

SLIDER
721
193
893
226
intelligence-diff
intelligence-diff
0
1024
500
1
1
NIL
HORIZONTAL

SLIDER
909
149
1135
182
appearance-plus-minus
appearance-plus-minus
0
100
0
1
1
NIL
HORIZONTAL

SLIDER
912
194
1133
227
intelligence-plus-minus
intelligence-plus-minus
0
100
0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This is a simple model of population genetics. There are two populations, the WOMEN and the MEN. Each has settable capacity, maximum difference of appearance and intelligence
between couples which allows a reproduction. The appearance and intelligence is changing over time. The changes can be also set. The women and men move around and if one man and women are sharing one patch at the time they can reproduce if they are similar enough. The similarity is settable. When the carrying capacity of the terrain is exceeded, some agents die (according to the lowest appearance score) to maintain a relatively constant population.  The model allows you to explore how general personality of population affect the ratio of reds to blues.

## HOW TO USE IT

Each pass through the GO function represents a generation in the time scale of this model.

The MAX-CAPACITY slider sets the carrying capacity of the terrain.

The MEN-CAPACITY and WOMEN-CAPACITY sliders set the initial number of men and women have after first intitialization.

The APPEARANCE-DIFF (INTELLIGENCE-DIFF) sliders set maximum allowed difference between appearance (intelligence) of man and women if they want to reproduce.

The APPEARANCE-PLUS-MINUS (INTELLIGENCE-PLUS-MINUS) denotes change of appearance (intelligence) at each tick. Appearance increases until age of 25, then decreases. Intelligence increases until age of 55, then decreases.

The # WOMEN and # MEN monitors display the number of reds and blues respectively.

The GO button runs the model.  A running plot is also displayed of the number of women (red), men (blue) and total population (in green).

## COPYRIGHT AND LICENSE

Copyright 2014 Martin Kersner, m.kersner@gmail.com
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
0
@#$#@#$#@
