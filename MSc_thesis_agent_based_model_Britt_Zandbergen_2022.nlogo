globals [
  day
  year
  informal-reused-textiles-NL
  formal-reused-textiles-NL
  recycled-textiles
  reused-textiles-outside-NL
  incinerated-textiles
  avg-price-budget-minimalist
  avg-price-casual-minimalist
  avg-price-budget-shopper
  avg-price-premium-shopper
]

breed [consumers consumer]
breed [textiles textile]
breed [resale-retailers resale-retail]
undirected-link-breed [textile-links textile-link]

consumers-own [
  consumer-segment
  V-of-price
  V-of-environment
  V-of-uniqueness
  V-of-ownership
  V-of-convenience
  new-textile-consumption
  reuse-purchasing
  reuse-percentage
  yearly-consumed-textiles
  daily-textile-need
  current-textile-need
  closet-size
  initial-yearly-consumed-textiles
  initial-daily-textile-need
  friends
  avg-reuse-percentage-friends
  replacement-rate
]

textiles-own [
  their-owner
  reused?
  current-possession-span
  current-service-span
  current-wears
  current-possession-wears
  user
  consumer?
  possession-span-textile
  possession-wears-textile
  consumer-friends                      ;; The friends of their current owner.
]

resale-retailers-own [
  current-collection
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETUP PROCEDURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  setup-consumers
  setup-textiles
  setup-resale-retailers
  determine-closet-size
  setup-replacement-rate
  determine-rebound
  setup-experiment
  if random-seed? = true
  [random-seed seed]
  set day 0
  set year 0
  set incinerated-textiles 0
  reset-ticks
end

;;;;;;;;SETUP-CONSUMERS;;;;;;;;

to setup-consumers
  create-consumers number-consumers [
    set shape "person"
    setxy random-xcor random-ycor
    set color white
    set yearly-consumed-textiles 0
  ]
  setup-consumer-segment
  determine-reuse-purchasing
  setup-yearly-consumed-textiles
  setup-friends
end

to setup-consumer-segment
  ask consumers
  [
    ifelse scenario? = false AND segment-distribution = "Based on Gwozdz et al. (2017)"
    [
      let segment-option random 99
      ifelse segment-option < 43                                          ;; Relative segment size of budget minimalist is 43%
      [set consumer-segment "budget-minimalist" ]
      [ifelse segment-option < (43 + 27)                                ;; Relative segment size of casual minimalist is 27%
        [set consumer-segment "casual-minimalist"]
        [ifelse segment-option < (43 + 27 + 21)                           ;; Relative segment size of budget-shopper is 21%
          [set consumer-segment "budget-shopper" ]
          [set consumer-segment "premium-shopper" ]                         ;; Relative segment size of premium-shopper is 9%
        ]
      ]
    ]
    [
      let segment-option random 99
      ifelse segment-option < 25                                          ;; Relative segment size of budget minimalist is 43%
      [set consumer-segment "budget-minimalist" ]
      [ifelse segment-option < 50                              ;; Relative segment size of casual minimalist is 27%
        [set consumer-segment "casual-minimalist"]
        [ifelse segment-option < 75                         ;; Relative segment size of budget-shopper is 21%
          [set consumer-segment "budget-shopper" ]
          [set consumer-segment "premium-shopper" ]                         ;; Relative segment size of premium-shopper is 9%
        ]
      ]
    ]
    if scenario? = true AND segment-distribution = "Budget-minimalist"
    [set consumer-segment "budget-minimalist"]
    if scenario? = true AND segment-distribution = "Budget-shopper"
    [set consumer-segment "budget-shopper"]
    if scenario? = true AND segment-distribution = "Casual-minimalist"
    [set consumer-segment "casual-minimalist"]
    if scenario? = true AND segment-distribution = "Premium-shopper"
    [set consumer-segment "premium-shopper"]
    setup-consumer-values
  ]
end

to setup-consumer-values
  ifelse consumer-segment = "budget-minimalist"
        [set V-of-price random-normal 0.85 0.1                             ;; VoP 0.9 -budget-minimalist
          set V-of-environment random-normal 0.3 0.1                       ;; VoE 0.3 -budget-minimalist
          set V-of-uniqueness random-normal 0.4 0.1                        ;; VoU 0.4 -budget-minimalist
          set V-of-convenience random-normal 0.4 0.1                       ;; VoC 0.4 -budget-minimalist
          set V-of-ownership random-normal 0.4 0.2 ]
  [ ifelse consumer-segment = "casual-minimalist"
    [ set V-of-price random-normal 0.75 0.1                             ;; VoP 0.8  -casual-minimalist
      set V-of-environment random-normal 0.45 0.1                      ;; VoE 0.45 -casual-minimalist
      set V-of-uniqueness random-normal 0.55 0.1                        ;; VoU 0.6  -casual-minimalist
      set V-of-convenience random-normal 0.65 0.1                       ;; VoC 0.55 -casual-minimalist
      set V-of-ownership random-normal 0.4 0.2]
    [ ifelse consumer-segment = "budget-shopper"
      [ set V-of-price random-normal 0.65 0.1                             ;; VoP 0.7  -budget-shopper
        set V-of-environment random-normal 0.45 0.1                      ;; VoE 0.45 -budget-shopper
        set V-of-uniqueness random-normal 0.55 0.1                       ;; VoU 0.5  -budget-shopper
        set V-of-convenience random-normal 0.7 0.1                       ;; VoC 0.9  -budget-shopper
        set V-of-ownership random-normal 0.4 0.2]
      [     set V-of-price random-normal 0.45 0.1                             ;; VoP 0.5  -premium-shopper
        set V-of-environment random-normal 0.6 0.1                       ;; VoE 0.6  -premium-shopper
        set V-of-uniqueness random-normal 0.6 0.1                        ;; VoU 0.6  -premium-shopper
        set V-of-convenience random-normal 0.65 0.1                      ;; VoC 0.65 -premium-shopper
        set V-of-ownership random-normal 0.4 0.2]
    ]
  ]

end


to determine-reuse-purchasing
  ask consumers [
    set reuse-purchasing (V-of-price + V-of-environment + V-of-uniqueness - V-of-ownership - V-of-convenience)
    ifelse reuse-purchasing < 0.5
    [set reuse-percentage 0]
    [ifelse reuse-purchasing < 0.75
      [set reuse-percentage -2.9 + (random-float 11.1)]
      [ ifelse reuse-purchasing < 1
        [set reuse-percentage 8.2 + (random-float 11.1)]
        [ ifelse reuse-purchasing < 1.25
          [ set reuse-percentage 19.3 + (random-float 11.1)]
          [ ifelse reuse-purchasing < 1.5
            [set reuse-percentage 30.3 + (random-float 11.1) ]
            [ ifelse reuse-purchasing < 1.75
              [set reuse-percentage 41.4 + (random-float 11.1)]
              [ifelse reuse-purchasing < 2.0
                [set reuse-percentage 52.5 + (random-float 11.1)]
                [ifelse reuse-purchasing < 2.25
                  [set reuse-percentage 63.6 + (random-float 11.1)]
                  [ifelse reuse-purchasing < 2.5
                    [set reuse-percentage 74.7 + (random-float 11.1)]
                    [ifelse reuse-purchasing < 2.75
                      [set reuse-percentage 85.8 + (random-float 11.1)]
                      [ if reuse-purchasing > 2.75
                        [set reuse-percentage 96.9 + (random-float 3.1)]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
    if reuse-percentage < 0 [set reuse-percentage 0]
    if reuse-percentage > 100 [set reuse-percentage 100]
    if scenario? = true
    [if reuse-percentage-fixed = 100 OR reuse-percentage-fixed = 0
      [set reuse-percentage reuse-percentage-fixed]
     if reuse-percentage-fixed = "Current + 10%"
      [set reuse-percentage reuse-percentage + 10]
      if reuse-percentage-fixed = "Current + 20%"
      [set reuse-percentage reuse-percentage + 20]
      if reuse-percentage-fixed = "Current + 30%"
      [set reuse-percentage reuse-percentage + 30]
      if reuse-percentage-fixed = "Current + 60%"
      [set reuse-percentage reuse-percentage + 60]
      ]
  ]

end


to setup-yearly-consumed-textiles
  ask consumers [
    set yearly-consumed-textiles random-normal yearly-textile-consumption-avg 3     ;; Based on quantified system analsys and average textile weight
    if yearly-textile-consumption-avg > 10                                           ;; Under normal parameter conditions it is assumed that every consumer on average buys at least some clothing items (at least 10)
    [ if yearly-consumed-textiles < 10
      [set yearly-consumed-textiles 10]]
    if consumer-segment = "budget-minimalist"
    [set yearly-consumed-textiles (yearly-consumed-textiles * 0.75)]            ;; Budget minimalist buys 25% less clothing than average
    if consumer-segment = "casual-minimalist"
    [set yearly-consumed-textiles (yearly-consumed-textiles * 0.77)]            ;; Casual minimalist buys 27% clothing less than average
    if consumer-segment = "budget-shopper"
    [set yearly-consumed-textiles (yearly-consumed-textiles * 1.43)]            ;; Casual shopper buys 43% clothing more than average
    if consumer-segment = "budget-spender"
    [set yearly-consumed-textiles (yearly-consumed-textiles * 2)]               ;; Premium shopper buys 100% clothing more than average
    set daily-textile-need (yearly-consumed-textiles / 365) * model-scale    ;; The need a consumer has to buy textiles. Determined based on the consumed textiles yearly divided by the days per year. And adapted based on textiles in model relatively to reality.
    set initial-yearly-consumed-textiles yearly-consumed-textiles
    set initial-daily-textile-need daily-textile-need
  ]
end

to setup-friends
  ask consumers [
  set friends other consumers in-radius social-influence-radius
  if not any? friends
    [set friends min-one-of other consumers [distance myself]]                        ;; Every consumer needs at least one friend, for informal reuse disposal
  ]
end

to setup-replacement-rate
  set avg-price-budget-minimalist 0.85                                               ;; Price averages are necessary to calculate the in- or decrease of the replacement rate by an in- or decrease of the price values
  set avg-price-casual-minimalist 0.75
  set avg-price-budget-shopper 0.65
  set avg-price-premium-shopper 0.45
  ask consumers [
    if consumer-segment = "budget-minimalist"
    [set replacement-rate (initial-replacement-rate + segment-influence)
      ;let segment-average mean [V-of-price] of consumers with [consumer-segment = "budget-minimalist"]
      let price-difference (V-of-price - avg-price-budget-minimalist) * 10
      set replacement-rate replacement-rate - (price-difference * price-influence)
    ]
    if consumer-segment = "casual-minimalist"
    [set replacement-rate (initial-replacement-rate + segment-influence)
    ; let segment-average mean [V-of-price] of consumers with [consumer-segment = "casual-minimalist"]
      let price-difference (V-of-price - avg-price-casual-minimalist) * 10
      set replacement-rate replacement-rate - (price-difference * price-influence)
    ]
    if consumer-segment = "budget-shopper"
    [set replacement-rate (initial-replacement-rate - segment-influence)
    ; let segment-average mean [V-of-price] of consumers with [consumer-segment = "budget-shopper"]
      let price-difference (V-of-price - avg-price-budget-shopper) * 10                                  ;; On average difference is 0.1, multiply by 10 for the right increase
      set replacement-rate replacement-rate - (price-difference * price-influence)
    ]
    if consumer-segment = "premium-shopper"
    [set replacement-rate (initial-replacement-rate - segment-influence)
     ; let segment-average mean [V-of-price] of consumers with [consumer-segment = "premium-shopper"]
      let price-difference (V-of-price - avg-price-premium-shopper) * 10
      set replacement-rate replacement-rate - (price-difference * price-influence)
    ]
    ifelse V-of-environment < 0.5
    [set replacement-rate replacement-rate - (0.5 - V-of-environment) * 2 * environmental-influence]
    [set replacement-rate replacement-rate + (0.5 + V-of-environment) * 2 * environmental-influence]
    if replacement-rate > 100
    [set replacement-rate 100]
    if replacement-rate < 0
    [set replacement-rate 0]
  ]
end

;;;;;;;;SETUP-TEXTILES;;;;;;;;

to setup-textiles
  ask consumers [
    hatch-textiles ((initial-number-textiles * model-scale) * (reuse-percentage / 100)) [      ;; Create reused textiles based on reuse percentage of consumer. Number of textiles in reality is 130. But the model is abstracte
      set color yellow
      set shape "shirt"
      set size 1.5
      set reused? true
      set consumer? true
      ifelse reuse-percentage-fixed = 100                              ;; In the 100% scenario all textile items are reused, but can still be reused once, therefore in this scenario the textile items in the system did not have a previous user.
      [set user 1]
      [set user 2]                                                      ;; Second user, because already worn before.
    ]
    hatch-textiles (initial-number-textiles * model-scale) - ( (initial-number-textiles * model-scale) * (reuse-percentage / 100)) [  ;; Create new textiles
      set color red
      set shape "shirt"
      set size 1.5
      set reused? false
      set consumer? true
      set user 1
    ]
  ]
  ask textiles with [consumer? = true ] [
    set their-owner one-of consumers-here
    create-textile-link-with their-owner
    set possession-span-textile (random-normal possession-span-average possession-span-stdev) * 365        ;; Because Netlogo works with ticks, this is measured per day (= 1 tick)
    if possession-span-textile < 0 [set possession-span-textile 0]
    let textile-quality (possession-span-textile / (possession-span-average * 365) )              ;; What is the possession span of the specific textile compared to the average possession span --> also determines the possession-wears of the textile item
    set possession-wears-textile (possession-wears-average * textile-quality)
    if possession-wears-textile < 0 [set possession-wears-textile 0]
    set current-possession-span (random-float 1 * possession-span-textile)       ;; At the start of the run textiles are already used. Therefore, the current-possession-span should decrease.
    set current-service-span current-possession-span
    set current-possession-wears (random-float 1 * possession-wears-textile)
    set current-wears current-possession-wears
    set consumer-friends [friends] of their-owner
    set xcor random-float 3 + xcor
    set ycor random-float 3 + ycor
     ]
  ask textiles with [reused? = true]
  [ set current-service-span (possession-span-textile + (possession-span-textile * lifetime-decrease-reuse / 100))
    set current-wears (possession-wears-textile) + possession-wears-textile * lifetime-decrease-reuse / 100
      ]
end


;;;;;;;;SETUP-RETAILERS;;;;;;;;

to setup-resale-retailers
  create-resale-retailers 3 [
    set shape "house"
    setxy random-xcor random-ycor
    set size 1.5
    set color green
  ]
  ask resale-retailers [
    hatch-textiles (count consumers * mean [reuse-percentage] of consumers / 30 ) [  ;; At start of run there are already reused textile items, so consumers can buy those. The amount is determined based uopon the average reuse-percentage of consumers and the amount of consumers.
      set color green
      set shape "shirt"
      set size 1.5
      set consumer? false
      set reused? true
      set current-possession-span 0
      set current-possession-wears 0
      set user 1
    ]
  ]
  ask textiles with [consumer? = false] [
    set their-owner one-of resale-retailers-here
    create-textile-link-with their-owner
    set possession-span-textile (random-normal possession-span-average possession-span-stdev) * 365
    let textile-quality (possession-span-textile / (possession-span-average * 365) )              ;; What is the possession span of the specific textile compared to the average possession span --> also determines the possession-wears of the textile item
    set possession-wears-textile (possession-wears-average * textile-quality)
    set possession-span-textile ((100 - lifetime-decrease-reuse) / 100 ) * possession-span-textile  ;; Reuse lifetime decrease
    if possession-span-textile < 0 [set possession-span-textile 0]
    set possession-wears-textile (( 100 - lifetime-decrease-reuse) / 100) * possession-wears-textile ;; Reuse lifetime decrease
    if possession-wears-textile < 0 [set possession-wears-textile 0]
    set xcor random-float 3 + xcor
    set ycor random-float 3 + ycor
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GO PROCEDURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  ifelse day = 365 [
    set day 0
    set year year + 1 ]
  [ set day day + 1 ]
  obtain-textiles
  dispose-textiles
  lifetime-decrease
  social-interaction
  determine-closet-size
  resale-retail-supply
  value-change-start
  friends-update
  if year = 100 [stop]
  if (count textiles with [consumer? = true]) < (daily-textile-wears * count consumers)
  [stop]
  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OBTAINING TEXTILES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to obtain-textiles
  ask consumers [
    set current-textile-need current-textile-need + daily-textile-need
    if current-textile-need > 1
    [ let reuse-chance random-float 100
      ifelse reuse-chance < reuse-percentage
      [obtain-reused-textile]
      [obtain-new-textile]
    ]
  ]
end

to obtain-new-textile
  set current-textile-need (current-textile-need - 1)
  set new-textile-consumption (new-textile-consumption + 1 / model-scale)           ;; At this point a new textile item is consumed, which should be added to the total consumption.
  hatch-textiles 1
  [set color red
    set shape "shirt"
    set size 1.5
    set user 1
    set reused? false
    set consumer? true
    set current-possession-span 0
    set current-service-span 0
    set current-wears 0
    set current-possession-wears 0
    set possession-span-textile (random-normal possession-span-average possession-span-stdev) * 365        ;; Because Netlogo works with ticks, this is measured per day (= 1 tick)
    if possession-span-textile < 0 [set possession-span-textile 0]
    let textile-quality (possession-span-textile / (possession-span-average * 365) )              ;; What is the possession span of the specific textile compared to the average possession span --> also determines the possession-wears of the textile item
    set possession-wears-textile (possession-wears-average * textile-quality)
    if possession-wears-textile < 0 [set possession-wears-textile 0]
  ]
  ask textiles-here
  [ set their-owner one-of consumers-here
    create-textile-link-with their-owner
    set consumer-friends [friends] of their-owner
    set xcor random-float 3 + xcor
    set ycor random-float 3 + ycor]
end

to obtain-reused-textile
  set current-textile-need (current-textile-need - 1)
  let obtained-reused-item min-one-of textiles with [consumer? = false][ distance myself ]      ;; Nearest reused textile item from second-hand retailer is obtained.
  if obtained-reused-item = nobody [stop]
  ask obtained-reused-item
    [move-to myself
      ask my-links [die]                                                                        ;; Current textile link with resale retailer needs to be deleted
      create-textile-link-with myself
      set their-owner myself
      set user user + 1
      set reused? true
      set consumer? true
      set color yellow
      set current-possession-span 0
      set current-possession-wears 0
      set formal-reused-textiles-NL (formal-reused-textiles-NL + 1)
      set xcor random-float 3 + xcor
      set ycor random-float 3 + ycor
      set consumer-friends [friends] of their-owner
  ]
  ifelse reuse-percentage-fixed != 100                                                             ;; In the 100% reuse scenario there is no virgin input, therefore the replacement rate leads then to
  [let replacement-chance random-float 100
  if replacement-chance > replacement-rate
    [obtain-new-textile]]
  [obtain-textiles]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DISPOSAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to dispose-textiles
  if (scenario? = false ) OR (disposal-feedback? = false )
  [
    ask textiles with [consumer? = true] [
      if (current-possession-span >= possession-span-textile) OR      ;; If the posession span of possesion wears are reached, the textile items is disposed
      (current-possession-wears >= possession-wears-textile)
      [ ask my-links [die]
        let disposal-option random 99                                         ;; textiles are disposed based on percentages from MFA (see thesis)
        if disposal-option < 5                                                ;; if random number is within 0 until 4  = 5% than informal reuse is chosen as disposal method
        [ifelse user > max-times-reuse
          [set incinerated-textiles incinerated-textiles + 1
            die]
          [dispose-informal]
        ]
        if disposal-option >= 5 AND disposal-option < 12                     ;; if random number is within 5 until 11  = 9% than resale retail is chosen as disposal method
        [ ifelse user > max-times-reuse
          [set incinerated-textiles incinerated-textiles + 1
            die]
          [dispose-resale-retail ]
        ]
        if disposal-option >= 12 AND disposal-option < 51                    ;; if random number is within 12 until 50 = 39% than collection-bin is chosen as disposal method
        [ dispose-collection-bin]
        if disposal-option >= 51                                             ;; if random number is within 51 until 99 = 49% than regular-waste is chosen as disposal method
        [dispose-regular-waste]
      ]
    ]
  ]
  if (scenario? = true) AND (disposal-feedback? = true )[
    ask textiles with [consumer? = true] [
      if (current-possession-span >= possession-span-textile) OR      ;; If the posession span of possesion wears are reached, the textile items is disposed
      (current-possession-wears >= possession-wears-textile)
      [ ask my-links [die]
        let disposal-option random 99
        let reuse-perc mean ([reuse-percentage] of consumers) * 1.5
        let formal-reuse-perc reuse-perc - (reuse-perc * (informal-reuse/reuse-total / 100))
        let informal-reuse-perc reuse-perc * (informal-reuse/reuse-total / 100)
        let waste-collection-perc 100 - formal-reuse-perc - informal-reuse-perc
        let waste-perc 39 / 88 * waste-collection-perc
        let collection-perc 49 / 88 * waste-collection-perc
        ifelse disposal-option < formal-reuse-perc
        [ifelse user > max-times-reuse
          [set incinerated-textiles incinerated-textiles + 1
            die]
          [dispose-resale-retail]
        ]
        [ ifelse disposal-option < (formal-reuse-perc + informal-reuse-perc)
          [ifelse user > max-times-reuse
            [set incinerated-textiles incinerated-textiles + 1
              die]
            [dispose-informal]
          ]
          [ ifelse disposal-option < (formal-reuse-perc + informal-reuse-perc + collection-perc)
            [dispose-collection-bin]
            [if disposal-option < 100
              [dispose-regular-waste]
            ]
          ]
        ]
      ]
    ]
  ]

end

to dispose-informal
  if count consumer-friends with [reuse-percentage > 0 ] = nobody
    [dispose-resale-retail]                                                    ;; If there are not enough consumers, informal reuse does not work. Therefore all informal reused textiles are then formally reused
  let new-potential-owners consumer-friends with [reuse-percentage > 0]      ;; Informally disposed of textiles are only transfered to consumers that purchase reused textiles
  let new-owner max-one-of new-potential-owners [current-textile-need]
  if new-owner = nobody                                                           ;; If there is no friend with a reuse-percentage higher than zero then a random friend is chosen.
  [set new-owner one-of consumer-friends]
      move-to new-owner
      create-textile-link-with new-owner
      set new-owner their-owner
      set xcor random-float 3 + xcor
      set ycor random-float 3 + ycor
      set user user + 1
      set consumer? true
      set reused? true
      set current-possession-span 0
      set current-possession-wears 0
      set color yellow
      reuse-lifespan-decrease
      set informal-reused-textiles-NL (informal-reused-textiles-NL + 1)
 ask their-owner
   [set current-textile-need current-textile-need - 1
    let replacement-chance random-float 100
    if replacement-chance > replacement-rate
    [obtain-new-textile]]
end

to dispose-collection-bin
  let collection-bin-option random 99                                   ;; final end destination of textiles in collection bin is based on percentages from MFA (see thesis)
  if collection-bin-option < 5                                          ;;if random number is within 0 until 4 = 5% than textiles are reused within the Netherlands and end up within resale-retail
  [ifelse reused? = true
    [set incinerated-textiles incinerated-textiles + 1
      die]
    [dispose-resale-retail ]
  ]
  if collection-bin-option >= 5 AND collection-bin-option < 53         ;;if random number is within 5 until 52 = 48% than textiles are reused outside the Netherlands
  [set reused-textiles-outside-NL reused-textiles-outside-NL + 1
    die]
  if collection-bin-option >= 53 AND collection-bin-option < 86        ;;if random number is within 53 until 85 = 33% than textiles are recycled
  [set recycled-textiles recycled-textiles + 1
    die]
  if collection-bin-option >= 86                                       ;;if random number is within 86 until 99 = 14% than textiles are incinerated
  [set incinerated-textiles incinerated-textiles + 1
    die ]
end

to dispose-resale-retail
  set their-owner min-one-of resale-retailers [distance myself]          ;; Formal reused textiles are brought to the nearest resale retailer
  move-to their-owner
  create-textile-link-with their-owner
  set xcor random-float 3 + xcor
  set ycor random-float 3 + ycor
  set color green
  set consumer? false
  set reused? true
  set current-possession-span 0
  set current-possession-wears 0
  reuse-lifespan-decrease
end


to dispose-regular-waste
  set incinerated-textiles incinerated-textiles + 1
  die
end

to reuse-lifespan-decrease                                                 ;; lifetime is already partly completed, when reused. It is assumed that 30% of the lifetime and amount of wears is completed when reused for every change of owner
  set current-possession-span 0
  set current-possession-wears 0
  set possession-span-textile (possession-span-textile * (1 - (lifetime-decrease-reuse / 100)))
  set possession-wears-textile (possession-wears-textile * (1 - (lifetime-decrease-reuse / 100)))
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EVERYDAY UPDATE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to social-interaction
  ask consumers [
    ifelse count consumers > 2
    [set avg-reuse-percentage-friends (mean [reuse-percentage] of friends )]
    [set avg-reuse-percentage-friends reuse-percentage]                       ;; With just a small amount of consumers, social interaction does not work. Therefore, avg reuse percentage is then equal to own reuse percentage. Resulting in no in or decrease due to social interaction
    ifelse reuse-percentage = 0
    [ if avg-reuse-percentage-friends > social-influence-threshold
      [ set reuse-percentage (avg-reuse-percentage-friends - social-influence-threshold) * social-influence / 1000]]
    [ifelse avg-reuse-percentage-friends < reuse-percentage
      [set reuse-percentage reuse-percentage - (reuse-percentage - avg-reuse-percentage-friends) * social-influence / 10000]
      [set reuse-percentage reuse-percentage + (avg-reuse-percentage-friends - reuse-percentage) * social-influence / 10000]]
    if reuse-percentage < 0
    [set reuse-percentage 0]
    if reuse-percentage > 100
    [set reuse-percentage 100]
  ]
end

to lifetime-decrease
  ask consumers [
    ifelse count my-links > daily-textile-wears                                        ;; To prevent an error from netlogo, if a consumer has no textile items any more
    [ask n-of daily-textile-wears link-neighbors
      [set current-wears (current-wears + model-scale )
        set current-possession-wears (current-possession-wears + model-scale) ]       ;; There are less textiles in the model than in reality. Therefore current wears decrease relatively.
    ]
    [stop]
  ]
  ask textiles with [consumer? = true] [
    set current-service-span current-service-span + 1
    set current-possession-span current-possession-span + 1]
end

to determine-closet-size
  ask consumers [
    set closet-size ( count my-links / model-scale)                                                          ;; Every day the closet size is determined
  ]
end

to resale-retail-supply
  if retailer-supply? = true [
    ask resale-retailers [
      set current-collection count my-out-links
      if current-collection < number-consumers
      [ hatch-textiles 5
        [ set color green
          set shape "shirt"
          set size 1.5
          set current-possession-span 0
          set current-possession-wears 0
          set user 1 + random 1
        ]
        ask textiles-here [
          set their-owner one-of resale-retailers-here
          create-textile-link-with their-owner
          set xcor random-float 3 + xcor
          set ycor random-float 3 + ycor
          set consumer? false
          set reused? true
          set possession-span-textile (random-normal possession-span-average possession-span-stdev) * 365
          let textile-quality (possession-span-textile / (possession-span-average * 365) )              ;; What is the possession span of the specific textile compared to the average possession span --> also determines the possession-wears of the textile item
          set possession-wears-textile (possession-wears-average * textile-quality)
          set possession-span-textile ((100 - lifetime-decrease-reuse) / 100 ) * possession-span-textile  ;; Reuse lifetime decrease
          if possession-span-textile < 0 [set possession-span-textile 0]
          set possession-wears-textile (( 100 - lifetime-decrease-reuse) / 100) * possession-wears-textile ;; Reuse lifetime decrease
          if possession-wears-textile < 0 [set possession-wears-textile 0]
        ]
      ]
    ]
  ]
end

to friends-update
  ask textiles with [consumer? = true]
  [set consumer-friends [friends] of their-owner]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SCENARIO'S ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-experiment
  if scenario? = true
  [set value-of-price-modification 0
    set value-of-environment-modification 0
    set value-of-convenience-modification 0
    if value-change = "Value-of-price-increase"
    [set value-of-price-modification value-change-modification]
    if value-change = "Value-of-environment-increase"
    [set value-of-environment-modification value-change-modification]
    if value-change = "Value-of-convenience-increase"
    [set value-of-convenience-modification value-change-modification]
  ]
end

to value-change-start
  if value-change != "Basecase" [
    if year = 15 AND day = 1
    [ask consumers [
      if value-of-price-modification != 0 OR value-of-environment-modification != 0 OR value-of-convenience-modification != 0
      [ifelse increase-only-for-high-values? = false
        [set v-of-price v-of-price + (v-of-price  * (value-of-price-modification / 100))
          set v-of-environment v-of-environment + (v-of-environment * (value-of-environment-modification / 100))
          set v-of-convenience v-of-convenience + (v-of-convenience * (-1 * value-of-convenience-modification / 100)) ]
        [if v-of-price > 0.5 [set v-of-price v-of-price + (v-of-price  * (value-of-price-modification / 100))]
          if v-of-environment > 0.5 [set v-of-environment v-of-environment + (v-of-environment * (value-of-environment-modification / 100))]
          if v-of-convenience > 0.5 [set v-of-convenience v-of-convenience + (v-of-convenience * (-1 * value-of-convenience-modification / 100))]
        ]
      ]
      ]
      determine-new-reuse-percentage
    ]
  ]
end

to determine-new-reuse-percentage
  ask consumers [
    set reuse-purchasing (V-of-price + V-of-environment + V-of-uniqueness - V-of-ownership - V-of-convenience)
    ifelse reuse-purchasing < 0.5
    [set reuse-percentage 0]
    [ifelse reuse-purchasing < 0.75
      [set reuse-percentage -2.9 + (random-float 11.1)]
      [ ifelse reuse-purchasing < 1
        [set reuse-percentage 8.2 + (random-float 11.1)]
        [ ifelse reuse-purchasing < 1.25
          [ set reuse-percentage 19.3 + (random-float 11.1)]
          [ ifelse reuse-purchasing < 1.5
            [set reuse-percentage 30.3 + (random-float 11.1) ]
            [ ifelse reuse-purchasing < 1.75
              [set reuse-percentage 41.4 + (random-float 11.1)]
              [ifelse reuse-purchasing < 2.0
                [set reuse-percentage 52.5 + (random-float 11.1)]
                [ifelse reuse-purchasing < 2.25
                  [set reuse-percentage 63.6 + (random-float 11.1)]
                  [ifelse reuse-purchasing < 2.5
                    [set reuse-percentage 74.7 + (random-float 11.1)]
                    [ifelse reuse-purchasing < 2.75
                      [set reuse-percentage 85.8 + (random-float 11.1)]
                      [ if reuse-purchasing > 2.75
                        [set reuse-percentage 96.9 + (random-float 3.1)]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    if reuse-percentage < 0 [set reuse-percentage 0]
    if reuse-percentage > 100 [set reuse-percentage 100]
    setup-replacement-rate
  ]
end


to determine-rebound
  if scenario? = true AND rebound? = false
    [
      set Lifetime-decrease-reuse 0
      set initial-replacement-rate 100
      ask consumers [
        set replacement-rate 100
      ]
  ]

    if scenario? = true AND rebound? = true
    [
      set Lifetime-decrease-reuse 30
      set initial-replacement-rate 70
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXPERIMENTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report service-lifespan
  report (mean [current-service-span] of textiles with [consumer? = true]) / 365
end

to-report new-consumption
  ifelse (day = 0) AND (year = 0)
 [report 0 ]
 [report ((mean [new-textile-consumption] of consumers) / (year * 365 + day) * 365) ]
end

to-report wears
  report (mean [current-wears] of textiles with [consumer? = true])
end

to-report owners
 report mean [user] of textiles with [consumer? = true ]
end

to-report reused-textiles-percentage
  report (count textiles with [consumer? = true AND reused? = true]) / (count textiles with [consumer? = true]) * 100
end

to-report textiles-in-closet
  report mean [closet-size] of consumers
end

to-report reused-textiles-average
  report ((count textiles with [consumer? = true AND reused? = true]) / number-consumers) / model-scale
end

to-report new-textiles-average
  report ((count textiles with [consumer? = true AND reused? = false]) / number-consumers) / model-scale
end

to-report value-of-environment
  report mean [v-of-environment] of consumers
end

to-report replacement-rate-budget-minimalist
  report mean [replacement-rate] of consumers with [consumer-segment = "budget-minimalist"]
end

to-report replacement-rate-casual-minimalist
    report mean [replacement-rate] of consumers with [consumer-segment = "casual-minimalist"]
end

to-report replacement-rate-budget-shopper
    report mean [replacement-rate] of consumers with [consumer-segment = "casual-minimalist"]
end

to-report replacement-rate-premium-shopper
    report mean [replacement-rate] of consumers with [consumer-segment = "premium-shopper"]
end

to-report replacement-rate-avg
  report mean [replacement-rate] of consumers
end
@#$#@#$#@
GRAPHICS-WINDOW
395
21
880
507
-1
-1
14.455
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
20
24
110
57
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
113
24
201
57
Go 
Go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
209
21
390
54
Number-consumers
Number-consumers
1
150
50.0
1
1
NIL
HORIZONTAL

SLIDER
209
56
390
89
Initial-number-textiles
Initial-number-textiles
0
300
130.0
1
1
NIL
HORIZONTAL

SLIDER
19
169
201
202
Possession-wears-average
Possession-wears-average
0
150
77.0
1
1
NIL
HORIZONTAL

MONITOR
889
21
951
66
Days
day
17
1
11

MONITOR
956
21
1018
66
Years
year
17
1
11

SLIDER
19
95
201
128
Possession-span-average
Possession-span-average
0
8
5.3
0.1
1
NIL
HORIZONTAL

MONITOR
1188
268
1298
313
Adoption score
mean [reuse-purchasing] of consumers
2
1
11

MONITOR
1301
268
1422
313
Reuse [%]
mean [reuse-percentage] of consumers
2
1
11

MONITOR
1336
39
1421
84
Reuse [%]
mean [reuse-percentage] of consumers with [consumer-segment = \"budget-minimalist\"]
2
1
11

MONITOR
1336
92
1422
137
Reuse [%]
mean [reuse-percentage] of consumers with [consumer-segment = \"casual-minimalist\"]
2
1
11

MONITOR
1335
142
1423
187
Reuse [%]
mean [reuse-percentage] of consumers with [consumer-segment = \"budget-shopper\"]
2
1
11

MONITOR
1336
189
1422
234
Reuse [%]
mean [reuse-percentage] of consumers with [consumer-segment = \"premium-shopper\"]
2
1
11

PLOT
889
71
1175
314
Reuse percentage of consumers
Textile reuse [%]
Consumers 
0.0
60.0
0.0
150.0
true
false
"set-plot-x-range 0 (round max [reuse-percentage] of consumers)\nset-plot-y-range 0 (count consumers / 2)\nset-histogram-num-bars 10" ""
PENS
"consumers" 1.0 1 -16777216 true "" "histogram [round reuse-percentage] of consumers "

SLIDER
209
91
392
124
Yearly-textile-consumption-avg
Yearly-textile-consumption-avg
0
120
36.0
1
1
NIL
HORIZONTAL

CHOOSER
209
126
391
171
Model-scale
Model-scale
0.08 0.09 0.1 0.11 0.12
2

BUTTON
20
59
201
92
Go once
Go
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
441
532
591
550
Experiments
12
0.0
1

SWITCH
24
554
139
587
Scenario?
Scenario?
0
1
-1000

PLOT
891
342
1179
568
Reused v.s. non-reused textiles
Time [days]
Textiles
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Non-reused" 1.0 0 -16777216 true "" "plot ((count textiles with [consumer? = true AND reused? = false]) / number-consumers) / model-scale"
"Reused" 1.0 0 -7500403 true "" "plot ((count textiles with [consumer? = true AND reused? = true]) / number-consumers) / model-scale"
"Total" 1.0 0 -2674135 true "" "plot ((count textiles with [consumer? = true]) / number-consumers) / model-scale\n\n"

MONITOR
893
602
1179
647
Reused textiles [%]
(count textiles with [consumer? = true AND reused? = true]) / (count textiles with [consumer? = true]) * 100
2
1
11

SLIDER
24
320
206
353
Lifetime-decrease-reuse
Lifetime-decrease-reuse
0
100
30.0
1
1
%
HORIZONTAL

PLOT
1182
342
1561
570
End-of-life textiles 
time
%
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"Incinerated" 1.0 0 -12087248 true "" "if (incinerated-textiles + recycled-textiles + informal-reused-textiles-NL + formal-reused-textiles-NL + reused-textiles-outside-NL) > 0 \n[plot (100 * incinerated-textiles / (incinerated-textiles + recycled-textiles + informal-reused-textiles-NL + formal-reused-textiles-NL + reused-textiles-outside-NL))]\n  \n  \n  \n  "
"Recycled  " 1.0 0 -13791810 true "" "if (incinerated-textiles + recycled-textiles + informal-reused-textiles-NL + formal-reused-textiles-NL + reused-textiles-outside-NL) > 0 \n[plot (100 * recycled-textiles / (incinerated-textiles + recycled-textiles + informal-reused-textiles-NL + formal-reused-textiles-NL + reused-textiles-outside-NL))]\n  "
"Reused outside NL" 1.0 0 -2674135 true "" "if (incinerated-textiles + recycled-textiles + informal-reused-textiles-NL + formal-reused-textiles-NL + reused-textiles-outside-NL) > 0 \n[plot (100 * reused-textiles-outside-NL / (incinerated-textiles + recycled-textiles + informal-reused-textiles-NL + formal-reused-textiles-NL + reused-textiles-outside-NL))]\n  "
"Informal reused" 1.0 0 -955883 true "" "if (incinerated-textiles + recycled-textiles + informal-reused-textiles-NL + formal-reused-textiles-NL + reused-textiles-outside-NL) > 0 \n[plot (100 * informal-reused-textiles-NL / (incinerated-textiles + recycled-textiles + informal-reused-textiles-NL + formal-reused-textiles-NL + reused-textiles-outside-NL))]\n  "
"Formal reused" 1.0 0 -6459832 true "" "if (incinerated-textiles + recycled-textiles + informal-reused-textiles-NL + formal-reused-textiles-NL + reused-textiles-outside-NL) > 0 \n[plot (100 * formal-reused-textiles-NL / (incinerated-textiles + recycled-textiles + informal-reused-textiles-NL + formal-reused-textiles-NL + reused-textiles-outside-NL))]\n  "

MONITOR
1183
600
1368
645
Service lifespan [years]
(mean [current-service-span] of textiles) / 365
3
1
11

MONITOR
1372
599
1564
644
Wears [days]
(mean [current-wears] of textiles)
3
1
11

MONITOR
1183
650
1368
695
New consumption [#/year]
((mean [new-textile-consumption] of consumers) / (year * 365 + day)) * 365
2
1
11

SLIDER
23
356
205
389
Initial-replacement-rate
Initial-replacement-rate
0
100
70.0
1
1
%
HORIZONTAL

SWITCH
287
610
497
643
Retailer-supply?
Retailer-supply?
0
1
-1000

TEXTBOX
215
300
430
326
Social interaction
11
0.0
1

SLIDER
209
395
394
428
Social-influence
Social-influence
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
210
356
395
389
Social-influence-threshold
Social-influence-threshold
0
20
5.0
1
1
%
HORIZONTAL

INPUTBOX
336
174
390
234
Seed
50.0
1
0
Number

SWITCH
211
175
336
208
Random-seed?
Random-seed?
1
1
-1000

MONITOR
1185
142
1330
187
Budget shopper [#]
count consumers with [consumer-segment = \"budget-shopper\"]
17
1
11

MONITOR
1186
92
1331
137
Casual minimalist [#]
count consumers with [consumer-segment = \"casual-minimalist\"]
17
1
11

MONITOR
1186
41
1329
86
Budget minimalist [#]
count consumers with [consumer-segment = \"budget-minimalist\"]
17
1
11

MONITOR
1186
191
1331
236
Premium shopper [#]
count consumers with [consumer-segment = \"premium-shopper\"]
17
1
11

TEXTBOX
1299
18
1514
44
Consumer segments
11
0.0
1

TEXTBOX
1286
245
1501
271
Averages
11
0.0
1

TEXTBOX
1319
322
1534
348
System behaviour
11
0.0
1

TEXTBOX
32
300
247
326
Rebound effects
11
0.0
1

TEXTBOX
1248
575
1517
604
Key Performance Indicators (study point in time)
11
0.0
1

MONITOR
1373
649
1563
694
Owners [#]
mean [user] of textiles with [consumer? = true ]
2
1
11

SLIDER
20
132
201
165
Possession-span-stdev
Possession-span-stdev
0
5
2.0
1
1
NIL
HORIZONTAL

SLIDER
23
392
205
425
Segment-influence
Segment-influence
0
10
5.0
1
1
%
HORIZONTAL

MONITOR
1425
267
1554
312
Replacement rate [%]
mean [replacement-rate] of consumers
2
1
11

SLIDER
23
427
204
460
Price-influence
Price-influence
0
10
5.0
1
1
%
HORIZONTAL

SLIDER
23
463
204
496
Environmental-influence
Environmental-influence
0
10
5.0
1
1
%
HORIZONTAL

MONITOR
1426
40
1562
85
Replacement rate [%]
mean [replacement-rate] of consumers with [consumer-segment = \"budget-minimalist\"]
1
1
11

MONITOR
1428
93
1562
138
Replacement rate [%]
mean [replacement-rate] of consumers with [consumer-segment = \"casual-minimalist\"]
2
1
11

MONITOR
1426
142
1562
187
Replacement rate [%]
mean [replacement-rate] of consumers with [consumer-segment = \"budget-shopper\"]
2
1
11

MONITOR
1425
190
1563
235
Replacement rate [%]
mean [replacement-rate] of consumers with [consumer-segment = \"premium-shopper\"]
2
1
11

PLOT
1567
39
1809
310
Replacement rate of consumers
Replacement rate [%]
Consumers
0.0
100.0
0.0
10.0
true
false
"set-plot-x-range 50 100\nset-plot-y-range 0 (count consumers / 2)\nset-histogram-num-bars 10" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [round replacement-rate] of consumers "

SLIDER
20
208
203
241
Daily-textile-wears
Daily-textile-wears
0
10
3.0
1
1
NIL
HORIZONTAL

SLIDER
212
322
394
355
Social-influence-radius
Social-influence-radius
0
20
13.0
1
1
NIL
HORIZONTAL

SLIDER
24
609
283
642
Value-of-price-modification
Value-of-price-modification
-300
300
0.0
1
1
%
HORIZONTAL

SLIDER
24
649
283
682
Value-of-environment-modification
Value-of-environment-modification
-300
300
0.0
1
1
%
HORIZONTAL

SLIDER
25
687
283
720
Value-of-convenience-modification
Value-of-convenience-modification
-300
300
0.0
1
1
%
HORIZONTAL

SWITCH
287
650
498
683
Disposal-feedback?
Disposal-feedback?
0
1
-1000

PLOT
1567
344
1809
570
Service lifespan
Time  [days]
Current lifespan [years]
0.0
0.0
4.5
7.5
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot service-lifespan * 2"

TEXTBOX
971
578
1186
604
System characteristics
11
0.0
1

CHOOSER
501
610
688
655
Reuse-percentage-fixed
Reuse-percentage-fixed
0 "Current" "Current + 10%" "Current + 20%" "Current + 30%" "Current + 60%" 100
1

SLIDER
285
688
498
721
Informal-reuse/reuse-total
Informal-reuse/reuse-total
0
100
25.0
1
1
%
HORIZONTAL

TEXTBOX
26
592
176
610
Value differences
11
0.0
1

TEXTBOX
290
589
440
607
Disposal behaviour
11
0.0
1

TEXTBOX
26
516
873
534
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
11
0.0
1

TEXTBOX
504
590
654
608
Reuse and rebound
11
0.0
1

SWITCH
502
658
688
691
Rebound?
Rebound?
0
1
-1000

TEXTBOX
709
588
859
606
Validation\n
11
0.0
1

CHOOSER
694
609
884
654
Segment-distribution
Segment-distribution
"Based on Gwozdz et al. (2017)" "Equal" "Budget-minimalist" "Budget-shopper" "Casual-minimalist" "Premium-shopper"
0

CHOOSER
695
659
884
704
Value-change
Value-change
"Basecase" "Value-of-price-increase" "Value-of-environment-increase" "Value-of-convenience-increase"
0

SWITCH
24
723
283
756
Increase-only-for-high-values?
Increase-only-for-high-values?
1
1
-1000

SLIDER
695
709
883
742
Value-change-modification
Value-change-modification
-300
300
50.0
1
1
%
HORIZONTAL

SLIDER
21
243
203
276
Max-times-reuse
Max-times-reuse
0
10
1.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This model is established to study rebound effects and value changes related to textile reuse.

## HOW IT WORKS

The model consists of consumers (white), textiles (red/yellow) and second-hand retailers (green). 

At the beginning of the model, each consumer is added to a consumer segment. Depending
on the consumer segment, consumers own values that determine their reused purchasing score. This score represents a percentage of textile reuse. In addition, consumers have a social network which influences this score. The amount of textiles a consumer owns is determined based on its consumer segment and reflects the size of the consumer’s closet. The amount of textile purchases per year is also defined based on the consumer segment. Textile items are linked to a specific owner. Their possession span and possession wears determine how long the textile item remains in possession of the current owner. 

Each day (tick) a consumer wears several of their textile items; for these items, the current possession wears increases. For all items, the current possession span of the item increases. When the current possession span or wears is greater than the predefined possession span and wears, the textile item is disposed of by the current owner. Unless formal or informal reuse is chosen as disposal option, the textile item dies and leaves the modelled system. When textile items are reused, these items acquire a new owner. 

Each day (tick) the textile need of consumers increases with their daily textile need. If the current textile need reaches the threshold to buy one item, the consumer purchases a non-reused or reused textile item. This choice depends upon the reuse percentage
of the consumer. When a non-reused textile item is purchased, a new textile item is created and added to the closet of the consumer. When a reused textile item is purchased, one of the textile items in possession of the resale retailers is added to the closet of the consumer. 

## HOW TO USE IT
By pressing the Setup-button, the modelis calibrated and a model run can start. 

When pressing the Go-button, the model runs in time steps of one day. Every day textile items can be disposed of by their consumer. In addition, every day textile items can be consumed. By pressing the Go-button again the model run stops. 

## THINGS TO NOTICE

Rebound effects evolve in two ways: 1) imperfect substitution 2) price and income effects. 

The first rebound effect is incorporated with a lifetime decrease due to reuse, leading to the following assumption: The possession span and wears of the textile item decreases every time the item changes owner.

The second rebound effect is incorporated by means of a replacement rate, leading to the following assumption: The replacement rate determines whether the purchase of a reused textile item leads to the purchase of an additional item. The replacement rate depends upon the consumer segment, value of price and value of environment of the consumer. 

## THINGS TO TRY

Several experiments can be performed by varying the sliders and switches underneath the model interface.  

## CREDITS AND REFERENCES

Bakker, A. (2021). Monitoring beleidsprogramma circulair textiel - nulmeting peiljaar 2018. https://open.overheid.nl/repository/ronl-9a6b4b22-eefa-4875-a83f-e03404de4e63/1/pdf/bijlage-2-nulmeting-monitoring-beleidsprogramma-circulair-textiel-2020-2025.pdf
Gwozdz, W., Nielsen, K. S., & Müller, T. (2017). An Environmental Perspective on Clothing Consumption: Consumer Segments and Their Behavioral Patterns. Sustainability 2017, Vol. 9, Page 762, 9(5), 762. https://doi.org/10.3390/SU9050762
Hertwich, E. G. (2005). Consumption and the rebound effect: An industrial ecology perspective. Journal of Industrial Ecology, 9(1–2), 85–98. https://doi.org/10.1162/1088198054084635
Klepp, I. G., Laitala, K., & Haugrønning, V. (2019). Wardrobe sizes and clothing lifespans.
Siderius, T., & Poldner, K. (2021). Reconsidering the Circular Economy Rebound effect: Propositions from a case study of the Dutch Circular Textile Valley. Journal of Cleaner Production, 293. https://doi.org/10.1016/j.jclepro.2021.125996
Zink, T., & Geyer, R. (2017). Circular Economy Rebound. Journal of Industrial Ecology, 21(3), 593–602. https://doi.org/10.1111/jiec.12545
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

shirt
false
0
Rectangle -7500403 true true 105 105 180 210
Polygon -7500403 true true 105 120 75 150 60 135 120 75 105 120
Rectangle -7500403 true true 120 75 165 135
Polygon -7500403 true true 180 120 210 150 225 135 165 75 180 120
Polygon -7500403 true true 165 75 165 75 165 75 180 105 165 105 165 75 180 105 165 105
Polygon -7500403 true true 120 75 105 117 181 119 167 75

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="76"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;None&quot;"/>
      <value value="0"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="173"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="46"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Behaviourinsight1" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="173"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="46"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ReboundA1" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="173"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="0"/>
      <value value="&quot;Current&quot;"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="46"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ReboundA4" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="173"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="0"/>
      <value value="&quot;Current&quot;"/>
      <value value="&quot;Current + 25%&quot;"/>
      <value value="&quot;Current + 50%&quot;"/>
      <value value="&quot;Current + 75%&quot;"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="46"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ReboundA4zonder100" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="173"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="0"/>
      <value value="&quot;Current&quot;"/>
      <value value="&quot;Current + 10%&quot;"/>
      <value value="&quot;Current + 20%&quot;"/>
      <value value="&quot;Current + 30%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="46"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="EffectsAmet30reuse" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="173"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="46"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="EffectsAmet30reuse" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="173"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
      <value value="&quot;Current + 30%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="46"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Reboundtruefalse" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="76"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
      <value value="&quot;Current + 30%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="173"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="46"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Reboundtruefalse+60" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="76"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
      <value value="&quot;Current + 30%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="173"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="46"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="!Reusevariation" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="173"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="0"/>
      <value value="&quot;Current&quot;"/>
      <value value="&quot;Current + 10%&quot;"/>
      <value value="&quot;Current + 20%&quot;"/>
      <value value="&quot;Current + 30%&quot;"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="46"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Validationsocialinfluence" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="46"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="173"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sensitivity_distribution" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Equal&quot;"/>
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sensitivity_lifetime" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sensitivity_replacement" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="environmentchange" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Basecase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Price-change-after-15years" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Basecase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Environment-change-after-15years" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Basecase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Convenience-change-after-15years" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Basecase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Combination-after-15-years-with-seed" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
      <value value="100"/>
      <value value="110"/>
      <value value="120"/>
      <value value="130"/>
      <value value="140"/>
      <value value="150"/>
      <value value="160"/>
      <value value="170"/>
      <value value="180"/>
      <value value="190"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Increase-only-for-high-values?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Basecase&quot;"/>
      <value value="&quot;Value-of-price-increase&quot;"/>
      <value value="&quot;Value-of-convenience-increase&quot;"/>
      <value value="&quot;Value-of-environment-increase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change-modification">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Combination-after-15-years-only-high" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Increase-only-for-high-values?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Basecase&quot;"/>
      <value value="&quot;Value-of-price-increase&quot;"/>
      <value value="&quot;Value-of-convenience-increase&quot;"/>
      <value value="&quot;Value-of-environment-increase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change-modification">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Reboundtruefalsefinal" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="590"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Increase-only-for-high-values?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Basecase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change-modification">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Combination-after-15-years-normal-and-high-incl-basecase" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
      <value value="100"/>
      <value value="110"/>
      <value value="120"/>
      <value value="130"/>
      <value value="140"/>
      <value value="150"/>
      <value value="160"/>
      <value value="170"/>
      <value value="180"/>
      <value value="190"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Increase-only-for-high-values?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Basecase&quot;"/>
      <value value="&quot;Value-of-price-increase&quot;"/>
      <value value="&quot;Value-of-convenience-increase&quot;"/>
      <value value="&quot;Value-of-environment-increase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change-modification">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Test" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="10"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Increase-only-for-high-values?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Basecase&quot;"/>
      <value value="&quot;Value-of-price-increase&quot;"/>
      <value value="&quot;Value-of-environment-increase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change-modification">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Test" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <metric>value-of-environment</metric>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="10"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Increase-only-for-high-values?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Value-of-environment-increase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change-modification">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Combination-after-15-years-test-replacement" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <metric>replacement-rate-avg</metric>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="10"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Increase-only-for-high-values?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Basecase&quot;"/>
      <value value="&quot;Value-of-price-increase&quot;"/>
      <value value="&quot;Value-of-convenience-increase&quot;"/>
      <value value="&quot;Value-of-environment-increase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change-modification">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Reboundtruefalse+30" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <metric>replacement-rate-avg</metric>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
      <value value="100"/>
      <value value="110"/>
      <value value="120"/>
      <value value="130"/>
      <value value="140"/>
      <value value="150"/>
      <value value="160"/>
      <value value="170"/>
      <value value="180"/>
      <value value="190"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Increase-only-for-high-values?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Basecase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change-modification">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
      <value value="&quot;Current + 30%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Reusevariationfinal" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Increase-only-for-high-values?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Basecase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change-modification">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="0"/>
      <value value="&quot;Current&quot;"/>
      <value value="&quot;Current + 10%&quot;"/>
      <value value="&quot;Current + 20%&quot;"/>
      <value value="&quot;Current + 30%&quot;"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Masreuse_sensitivity_100" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Increase-only-for-high-values?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Basecase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change-modification">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current + 30%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Max-times-reuse">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Rebound true false" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
      <value value="100"/>
      <value value="110"/>
      <value value="120"/>
      <value value="130"/>
      <value value="140"/>
      <value value="150"/>
      <value value="160"/>
      <value value="170"/>
      <value value="180"/>
      <value value="190"/>
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Increase-only-for-high-values?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Basecase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change-modification">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
      <value value="&quot;Current + 30%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Max-times-reuse">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Greenlight test" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Increase-only-for-high-values?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Basecase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change-modification">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Max-times-reuse">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Segment difference" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="10"/>
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Increase-only-for-high-values?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Basecase&quot;"/>
      <value value="&quot;Value-of-price-increase&quot;"/>
      <value value="&quot;Value-of-convenience-increase&quot;"/>
      <value value="&quot;Value-of-environment-increase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change-modification">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Max-times-reuse">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Segment difference_1" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Increase-only-for-high-values?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Basecase&quot;"/>
      <value value="&quot;Value-of-price-increase&quot;"/>
      <value value="&quot;Value-of-environment-increase&quot;"/>
      <value value="&quot;Value-of-convenience-increase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change-modification">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Budget-minimalist&quot;"/>
      <value value="&quot;Budget-shopper&quot;"/>
      <value value="&quot;Casual-minimalist&quot;"/>
      <value value="&quot;Premium-shopper&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Max-times-reuse">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Basecase" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>service-lifespan</metric>
    <metric>new-consumption</metric>
    <metric>wears</metric>
    <metric>owners</metric>
    <metric>reused-textiles-percentage</metric>
    <metric>textiles-in-closet</metric>
    <metric>reused-textiles-average</metric>
    <metric>new-textiles-average</metric>
    <enumeratedValueSet variable="Value-of-price-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-radius">
      <value value="13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Random-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Rebound?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Seed">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence-threshold">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Price-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Increase-only-for-high-values?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Yearly-textile-consumption-avg">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-stdev">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Social-influence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Disposal-feedback?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Number-consumers">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lifetime-decrease-reuse">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change">
      <value value="&quot;Basecase&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Scenario?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-change-modification">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Model-scale">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-span-average">
      <value value="5.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Segment-distribution">
      <value value="&quot;Based on Gwozdz et al. (2017)&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-convenience-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retailer-supply?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Reuse-percentage-fixed">
      <value value="&quot;Current&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-number-textiles">
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Environmental-influence">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Initial-replacement-rate">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Max-times-reuse">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Informal-reuse/reuse-total">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Value-of-environment-modification">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Daily-textile-wears">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Possession-wears-average">
      <value value="77"/>
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
0
@#$#@#$#@
