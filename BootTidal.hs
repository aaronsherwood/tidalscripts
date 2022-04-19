:set -XOverloadedStrings
:set prompt ""

import Sound.Tidal.Context
import System.IO (hSetEncoding, stdout, utf8)
hSetEncoding stdout utf8

-- total latency = oLatency + cFrameTimespan
tidal <- startTidal (superdirtTarget {oLatency = 0.1, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cFrameTimespan = 1/20, cCtrlPort = 6010})

:{
let p = streamReplace tidal
    hush = streamHush tidal
    list = streamList tidal
    mute = streamMute tidal
    unmute = streamUnmute tidal
    solo = streamSolo tidal
    unsolo = streamUnsolo tidal
    once = streamOnce tidal
    first = streamFirst tidal
    asap = once
    nudgeAll = streamNudgeAll tidal
    all = streamAll tidal
    resetCycles = streamResetCycles tidal
    setcps = asap . cps
    xfade i = transition tidal True (Sound.Tidal.Transition.xfadeIn 4) i
    xfadeIn i t = transition tidal True (Sound.Tidal.Transition.xfadeIn t) i
    histpan i t = transition tidal True (Sound.Tidal.Transition.histpan t) i
    wait i t = transition tidal True (Sound.Tidal.Transition.wait t) i
    waitT i f t = transition tidal True (Sound.Tidal.Transition.waitT f t) i
    jump i = transition tidal True (Sound.Tidal.Transition.jump) i
    jumpIn i t = transition tidal True (Sound.Tidal.Transition.jumpIn t) i
    jumpIn' i t = transition tidal True (Sound.Tidal.Transition.jumpIn' t) i
    jumpMod i t = transition tidal True (Sound.Tidal.Transition.jumpMod t) i
    mortal i lifespan release = transition tidal True (Sound.Tidal.Transition.mortal lifespan release) i
    interpolate i = transition tidal True (Sound.Tidal.Transition.interpolate) i
    interpolateIn i t = transition tidal True (Sound.Tidal.Transition.interpolateIn t) i
    clutch i = transition tidal True (Sound.Tidal.Transition.clutch) i
    clutchIn i t = transition tidal True (Sound.Tidal.Transition.clutchIn t) i
    anticipate i = transition tidal True (Sound.Tidal.Transition.anticipate) i
    anticipateIn i t = transition tidal True (Sound.Tidal.Transition.anticipateIn t) i
    forId i t = transition tidal False (Sound.Tidal.Transition.mortalOverlay t) i
    d1 = p 1 . (|< orbit 0)
    d2 = p 2 . (|< orbit 1)
    d3 = p 3 . (|< orbit 2)
    d4 = p 4 . (|< orbit 3)
    d5 = p 5 . (|< orbit 4)
    d6 = p 6 . (|< orbit 5)
    d7 = p 7 . (|< orbit 6)
    d8 = p 8 . (|< orbit 7)
    d9 = p 9 . (|< orbit 8)
    d10 = p 10 . (|< orbit 9)
    d11 = p 11 . (|< orbit 10)
    d12 = p 12 . (|< orbit 11)
    d13 = p 13
    d14 = p 14
    d15 = p 15
    d16 = p 16

    aSectionChordProg = "<[5,9,13] [6,10,14] [4,8,12] [5,9,13] [5,9,13] [6,10,14] [4,8,12] [3,7,11]>"
    bSectionChordProg = "<[0,5,9,13,17] [[][-1,4,8,12,16]] [-2,3,7,11,15] [] [0,5,9,13,17] [[][-1,4,8,12,16]] [-3,2,6,10,14] []>"
    cSectionSmallChordProg = "<[3,7,11] [4,8,12] [5,9,13] [4,10,14]>"
    cSectionLargeChordProg = "<[1,6,10,14,18,22] [2,7,11,15,19,23] [3,8,12,16,20,24] [4,9,13,17,21,25]>"
    cSectionMelody = "<[[4 7] [8 [~ . . . 7]] [[9~~8][]] [[9~~8][7 6]]] [[4 7] [8 [~ . . . 7]] [[9~~8][~ . . . 7]] [[10~~9][8 7]]] [[4 [11 [[~][~ 11 10 9]]]] [~ [~ . . . 8]] [[10~~9][~ . . . 8]] [[10~~9][8 7]]] [[4 7] [11 [~ . . . 9]] [[10 ~ ~ 9] [~ ~ 8 ~]] [[~ 7 ~ ~] [6 4]]] >"
    -- sloppy chords
    a = "<[e4,gs4,b4,cs5,e3] [g4,b4,cs4,g3] [fs4,b4,cs5,ds5,b3] [fs4,as4,b4,ds5,as3] [ds4,gs4,cs5,f3] [ds4,gs4,b4,cs5,gs3] [fs4,b4,cs5,ds3] [gs4'sus2,c4]>"
    b = "<[e4'min6,e3] [g4,b4,cs5,ds5,g3] [fs4,b4,cs5,ds5,b3] [fs4'sus2,as3] [ds4,gs4,cs5,f3] [e4, gs4,b4,cs5,gs3] [fs4,gs4,b4,cs5,ds3] [ds4,gs4,cs5,c4] >"
    c = "<[e4, gs4,b4,cs5,e3] [f4,as4,ds5,g3] [e4,gs4,b4,ds5,b3] [fs4,as4,b4,ds5,as3] [ds4,gs4,cs5,f3] [ds4,gs4,b4,cs5,gs3] [fs4,b4,cs5,ds3] [gs4'sus2,c4] >"
    originallUnused = "<[g4,c5,d5,e3] [f4,as4,ds5,g3] [g4'sus2,b3] [fs4'sus2,as3] [ds4,gs4,cs5,f3] [e4'sus2,gs3] [fs4,b4,cs5,ds3] [gs4'sus2,c4]>"
    harm = pF "harm"
    timbre = pF "timbre"
    morph = pF "morph"
    engine = pI "engine"
    vwet = pF "verbwet"
    vtime = pF "verbtime"
    vdamp = pF "verbdamp"
    vhp = pF "verbhp"
    vfreeze = pI "verbfreeze"
    vdiff = pF "verbdiff"
    vgain = pF "verbgain"
    cloudspitch = pF "cloudspitch"
    cloudspos = pF "cloudspos"
    cloudssize = pF "cloudssize"
    cloudsdens = pF "cloudsdens"
    cloudstex = pF "cloudstex"
    cloudswet = pF "cloudswet"
    cloudsgain = pF "cloudsgain"
    cloudsspread = pF "cloudsspread"
    cloudsrvb = pF "cloudsrvb"
    cloudsfb = pF "cloudsfb"
    cloudsfreeze = pF "cloudsfreeze"
    cloudsmode = pF "cloudsmode"
    cloudslofi = pF "cloudslofi"
    ringsfreq = pF "ringsfreq"
    ringsstruct = pF "ringsstruct"
    ringsbright = pF "ringsbright"
    ringsdamp = pF "ringsdamp"
    ringspos = pF "ringspos"
    ringsmodel = pI "ringsmodel"
    ringsintern_exciter = pI "ringsinternal"
    ringstrig = pI "ringsinternal"
    ringseasteregg = pI "ringseasteregg"
    ringsbypass= pI "ringsbypass"
    ringspoly= pI "ringspoly"
    linput = pI "linput"
    lname = pS "lname"
    delayIn = room "0.75" # size "0.75" # delayfb 0.5 # delaytime 0.5 # delay 0.75 # lock 1
    cloudsDelayIn = cloudsmode "2" # cloudspos perlin # cloudssize perlin # cloudsdens perlin # cloudstex 0.5 # cloudsfb "0.66" # cloudsrvb "0.4" # cloudsgain "2" # cloudswet "1" # cloudspitch "0" # cloudsspread "1"  # cloudsfreeze "0" # cloudslofi "0" # orbit 10
    cloudsGrain = delayfb 0.5 # delaytime 0.5 # delay "0.8" # lock 1 # cloudspitch 0 # cloudspos "0.01" # cloudswet "0.4" # cloudssize perlin # cloudsdens 0.72 # cloudstex perlin # cloudsgain "5" # cloudsspread "0.75"# cloudsrvb "0.75" # cloudsfb "0.66" # cloudsfreeze "0" # cloudsmode "0" # cloudslofi "0" # orbit 10
    cloudsGrainLess = delayfb 0.5 # delaytime 0.5 # delay "0.18" # lock 1 # cloudspitch 0 # cloudspos "0.01" # cloudswet "0.4" # cloudssize perlin # cloudsdens 0.72 # cloudstex perlin # cloudsgain "5" # cloudsspread "0.75"# cloudsrvb "0.175" # cloudsfb "0.66" # cloudsfreeze "0" # cloudsmode "0" # cloudslofi "0" # orbit 10
    cloudsPitchDown = cloudspos 0.9 # cloudsdens "0.3" # cloudsfb 0.35 # cloudssize 0.75 # cloudswet "0.4" # cloudsgain "5" # cloudspitch "<-24>" # cloudstex rand # cloudsspread "0.75" # cloudsrvb "0.5" # cloudslofi "0" # squiz 0 # orbit 10
    michi = ringsfreq 38 # ringsstruct 1 # ringsbright 0.9 # ringsdamp 0.75 # ringspos 1 # ringspoly 1 # ringsmodel 2  # cloudspitch "0" # cloudspos "0.01" # cloudswet "1" # cloudssize "0.22" # cloudsdens "0.72" # cloudstex "0.75" # cloudsgain "8" # cloudsspread "0.75" # cloudsrvb "0.6" # cloudsfb "0.66" # cloudsfreeze "0" # cloudsmode "0" # cloudslofi "0"
    splatBeat = do
      d1 $ rotL 1 $ every 16 (linger 0.25) $ s "[feel:6 ~ ~ ~ feel?] [~] [~ ~ <feel:6 feel> ~ ~] [~ ~ ~ ~ ~ ~ feel?]" # room 0.95 # speed (slow 4 (range 4 0.5 (saw))) # nudge "-0.027" # gain 0.9 # squiz 1.01 # up "2"
      d2 $ s "[glitch:4 ~ ~ ~ glitch:4? ~ ~]*<8 4 16>" # gain (range 1 1.2 (rand)) # room 0.2
      d3 $ s "~ click:2 ~ click:2" # room 0.2 # gain "1.5"  # nudge 0.027-- # speed "2"
      d4 $ s "~ [~ [click:3] ~ ~ ~ ~ ~] ~ [~ ~ ~ click:3  ~ ~ ~]" # room 0.2 # gain "1.3" # nudge 0.027
      d5 $ s "~ glitch:2 ~ glitch:2?" # room 0.9 # gain 1.3 # speed 2
    micDrop = do
      p "micin" $ jux (rev) $ chop 16 $ s "in1" # gain 1.1 # room "0.95" # cloudsPitchDown
    mosaic = do
      p "record" $ slow 8 $ s "rlooper" # n "<0 1 2 3 4 5 6 7>"
      p "playback" $ slow 8 $ s "loop" # n "[0?,1?,2?,3?,4?,5?,6?,7?]" # room 0.95 # gain 1.2 # djf 0.77 # pan rand
    scrapBook = do
      xfade "halfspeed" $ slow 16 $ s "loop" # n "0" # speed 0.5 # room 0.7
      xfade "grains" $ slow 8 $ s "loop" # n "5" # cloudsGrainLess
      xfade "delay" $ slow 8 $ s "loop" # n "3" # cloudsDelayIn # speed "-1"
    blackInk = do
      p "michi" $ s "in" # michi # delay "0.75" # delayfb "0.5" # delaytime (range 0.6 1 perlin)
    bookBurning = do
      p "record" silence
      p "playback" silence
      p "halfspeed" silence
      p "grains" silence
      p "michi" silence
    -- cow bells
    cow1 = loopAt 4 $ chop 32 $ s "44comp:31" # room 0.7 # cloudsDelayIn # pan perlin -- # gain perlin
    cow2 = loopAt 8 $ chop 32 $ s "44comp:31" # room 0.7 # pan perlin -- # gain perlin
    cow3 = loopAt 16 $ chop 32 $ s "44comp:31" # room 0.7 # gain 1.1 -- # gain (perlin2With (cosine*2) (sine*2))
    cow4 = chop 32 $ loopAt 4 $ sound "44comp:31" # gain 1.2 # room 0.9
    gormaduli = do
      p "cow1" $ cow1
      p "cow2" $ cow2
      p "cow3" $ cow3
    gormaduliQuarantine = do
      xfade "cow1" silence
      xfade "cow2" silence
      xfade "cow3" silence
      xfade "cow4" silence
    sloppy = do
      p "slop1" $ s "feel ~ feel [~ ~ ~ ~ ~ ~ feel?]" # gain 1.1 # room 0.75 # nudge "-0.01" -- # speed (slow 4 (range 8 0.75 (saw))) --(range "-0.05" "-0.03" (rand))
      p "slop2" $ press $ s "hh*4" # room 0.93 # gain (range 1.1 1.3 (rand)) # nudge 0.01 -- # speed (slow 4 (range 8 6 (saw)))
      p "slop3" $ press $ s "feel:4*2" # gain 1.1 # room 0.6 # nudge 0.02 -- (range "0.03" "0.05" (rand))
    cleanSlop how = do
      if how == "clutch"
        then do
          clutch "slop1" silence
          clutch "slop2" silence
          clutch "slop3" silence
      else if how == "xfade"
        then do
          xfade "slop1" silence
          xfade "slop2" silence
          xfade "slop3" silence
      else do
        p "slop1" silence
        p "slop2" silence
        p "slop3" silence
    thisLove = do
      xfade "thisLove0" $ jux (rev) $ slow 8 $ striate' 64 (1/4) $ sound "44comp:36" # gain 0.75 # room "0.9" # size "0.9" # speed "1" # slow 4 ( gain (range 0.35 0.8 perlin) ) # up "-4" -- # up "5"-- # slow 4 ( crush (range 2 32 rand) )
      xfade "thisLove1" $ jux (rev) $ slow 8 $ striate' 64 (1/4) $ sound "44comp:36" # gain 0.75 # room "0.9" # size "0.9" # speed "-2" # slow 4 ( gain (range 0.35 0.8 perlin) ) # up "-4" -- # up "5"
      xfade "thisLove2" $ jux (rev) $ slow 8 $ striate' 64 (1/4) $ sound "44comp:36" # gain 0.75 # room "0.9" # size "0.9" # speed "0.5" # slow 4 ( gain (range 0.35 0.8 perlin) ) # up "-4" -- # up "5"
    brokenHeart = do
      xfade "thisLove0" silence
      xfade "thisLove1" silence
      xfade "thisLove2" silence
    marching = xfade "troops" $ loopAt 8 $ chop 32 $ s "44comp:27" # gain 1.4 # djf 0.65 # gain 1 # room 0.5
    paulineOliveros = xfade "pauline oliveros" $ loopAt 16 $ chop 512 $ s "44comp:26" # room 0.7 # speed 0.95
    blackRain = do
      xfade "blackRainHigh" $ loopAt 8 $ chop 32 $ sound "44comp:25" # cloudsGrainLess # slow 4 ( gain (range 0.5 0.7 perlin) )
      xfade "blackRain" $ loopAt 32 $ chop 32 $ sound "44comp:25" # gain 1
    sunshine how = do
      if how == "xfade"
        then do
          xfade "blackRainHigh" silence
          xfade "blackRain" silence
      else do
        p "blackRainHigh" silence
        p "blackRain" silence
    at = putStrLn "syntactical connections are made";
    merge [] ys = ys
    merge (x:xs) ys = x:merge ys xs
    -- worked example:
    -- ["a","b"] ["x","v"]
    -- a+(merge ["x","v"] ["b"])
    -- a+(x+(merge ["b"] ["v"]))
    -- a+x+b+(merge ["v"] [])
    -- a+x+b+v+(merge([] []))
    improviseChords progression org which rhythms=
      do
        if org == "org"
          then do
          let chords = do {
            if length (rhythms) < length (words progression)
              then do
                take (length rhythms) (words progression)
            else do
                (words progression)
            }
          let combined = merge chords rhythms
          let final = unwords combined
          if which == "midi"
            then
              p "piano" $ density 0.25 $ s (parseBP_E which) <| n (parseBP_E final) # amp 0.7 # nudge "-0.01"
            else
              p "piano" $ density 0.25 $ s (parseBP_E which) <| up (parseBP_E final) # room 0.7 # orbit 3 # gain 0.7 # nudge "-0.01"
        else do
          let chords = "{"++progression++"}%<"++org++">"
          let ts = replicate (length rhythms) "t"
          let pat = unwords (merge ts rhythms)
          if which == "midi"
            then do
              p "piano" $ density 0.25 $ struct (parseBP_E pat) $ s (parseBP_E which) <| n (parseBP_E chords) # amp 0.7
            else
              p "piano" $ density 0.25 $ struct (parseBP_E pat) $ s (parseBP_E which) <| up (parseBP_E chords) # room 0.7 # orbit 3 # gain 0.7 # nudge "-0.01"
    sloppy10 = do
      p "slop1" $ slow 2 $ degradeBy 0.62 $ s "dr_few ~ [~ dr_few] ~ dr_few dr_few ~ [~ dr_few] ~ dr_few dr_few ~ [~ dr_few] ~ ~ dr_few ~ [~ dr_few] ~ dr_few" # room 0.9 # krush 20 # nudge "-0.01" # gain 1 # nudge 0.01  # speed (("{0.375 0.75}")) # up 9 # orbit 3-- # up ( ("<9 12 16 15 10 13 8 17>"+1))
      xfade "slop2" $ slow 2 $ jux rev $ s "{808:3?}%40" # speed "2" # gain (range 0.5 1.1 (rand)) # nudge "-0.01" # room 0.07 # orbit 2-- # speed (slow 4 (range 8 6 (saw)))
      p "slop3" $ degradeBy 0.0 $ s "{808bd}%5" # gain 2 # orbit 2 # krush 20
      p "lovano2" $ slow 2 $ slice 18 "<9 9 9 9 9 9 9 9 9 9>*10" $ s "33:28" # gain 0.85 # speed "2" # room 0.5 # up 1 # nudge "0.02"
      p "lovano1" $ slow 2 $ jux rev $ slice 16 "<0 1 1 0 1 0 0 1 1 1>*40" $ s "33:28*40" # room 0.5 # gain 1.2 # speed "8" # up 0 # cut 1
    our = "[g4,c5,d5,e3] [f4,as4,ds5,g3] [g4'sus2,b3] [fs4'sus2,as3] [ds4,gs4,cs5,f3] [e4'sus2,gs3] [fs4,b4,cs5,ds3] [gs4'sus2,c4]"
    a2 = "[e4,gs4,b4,cs5,e3] [g4,b4,cs4,g3] [fs4,b4,cs5,ds5,b3] [fs4,as4,b4,ds5,as3] [ds4,gs4,cs5,f3] [ds4,gs4,b4,cs5,gs3] [fs4,b4,cs5,ds3] [gs4'sus2,c4]"
    b2 = "[e4'min6,e3] [g4,b4,cs5,ds5,g3] [fs4,b4,cs5,ds5,b3] [fs4'sus2,as3] [ds4,gs4,cs5,f3] [e4,gs4,b4,cs5,gs3] [fs4,gs4,b4,cs5,ds3] [ds4,gs4,cs5,c4]"
    c2 = "[e4,gs4,b4,cs5,e3] [f4,as4,ds5,g3] [e4,gs4,b4,ds5,b3] [fs4,as4,b4,ds5,as3] [ds4,gs4,cs5,f3] [ds4,gs4,b4,cs5,gs3] [fs4,b4,cs5,ds3] [gs4'sus2,c4]"
    rhythms = ["@5","@4","@3","@6","@7","@6","@5","@4"]
    anaosc  = 0
    wshape  = 1
    fm      = 2
    grain = 3
    additive   = 4
    wtable  = 5
    chords  = 6
    vowel   = 7
    swarm    = 8
    noisef  = 9
    noisep  = 10
    string   = 11
    modal = 12
    anabd   = 13
    anasn   = 14
    anahh   = 15
    cow1_medusa = loopAt 4 $ chop 32 $ s "44comp:31" # room 0.7 # cloudsDelayIn # pan perlin # gain (1*(cF 0 "gormaduli"))-- 1 -- # gain perlin
    cow2_medusa = loopAt 8 $ chop 32 $ s "44comp:31" # room 0.7 # pan perlin # gain (1*(cF 0 "gormaduli"))--1 -- # gain perlin
    cow3_medusa = loopAt 16 $ chop 32 $ s "44comp:31" # room 0.7 # gain (1.1*(cF 0 "gormaduli")) --1.1 -- # gain (perlin2With (cosine*2) (sine*2))
    -- cow4 = chop 32 $ loopAt 4 $ sound "44comp:31" # gain (1.2*(cF 0 "gormaduli")) # room 0.9
    gormaduli_medusa = do
      p "cow1" $ cow1_medusa # orbit 11
      p "cow2" $ cow2_medusa # orbit 11
      p "cow3" $ cow3_medusa # orbit 11
      -- p "cow4" $ cow4 # orbit 11
    thisLove_medusa zero = do
      xfadeIn "thisLove0" 8 $ jux (rev) $ slow 8 $ striate' 64 (1/4) $ sound "44comp:36" # room "0.9" # size "0.9" # speed "1" # slow 4 ( gain (zero) ) # up "-4" # orbit 10 -- # up "5"-- # slow 4 ( crush (range 2 32 rand) )
      xfadeIn "thisLove1" 8 $ jux (rev) $ slow 8 $ striate' 64 (1/4) $ sound "44comp:36"  # room "0.9" # size "0.9" # speed "-2" # slow 4 ( gain ( zero) ) # up "-4" # orbit 10  -- # up "5"
      xfadeIn "thisLove2" 8 $ jux (rev) $ slow 8 $ striate' 64 (1/4) $ sound "44comp:36"  # room "0.9" # size "0.9" # speed "0.5" # slow 4 ( gain ( zero)) # up "-4"  # orbit 10 -- # up "5"
    chordProg gain0 gain1 gain2 =
      do
        p "chords0" $ loopAt 32 $ chop 128 $ s "44comp:32" # gain (1*gain0) # room 0.5  # orbit 6 -- # nudge "-0.027"
        p "chords1" $ loopAt 16 $ chop 64 $ s "44comp:33" # gain (1*gain1) # room 0.5  # orbit 6 -- # krush 1
        p "chords2" $ loopAt 8 $ chop 32 $ s "44comp:34" # gain (1*gain2) # room 0.5  # orbit 6
    vocals stopAll mainVocal glitchVocal glitchVocal2 =
      do
        if stopAll == 1
          then do
            hush
        else return ()
        p "vocals0" $ loopAt 2 $ chop 32 $ s "IGLOOGHOST_vocals:18" # delayIn # gain mainVocal # orbit 0
        p "vocals1" $ loopAt 4 $ chop 32 $ s "IGLOOGHOST_vocals:17" # delayIn # gain (0.8*glitchVocal) # orbit 0 --17 is 5 16 is 1
        p "vocals2" $ loopAt 8 $ chop 32 $ s "IGLOOGHOST_vocals:16" # delayIn # gain (0.8*glitchVocal2) # orbit 0
    iglooBeat stopAll vol0 vol1 vol2 =
      do
        if stopAll == 1
          then do
            hush
        else return ()
        p "igloo0" $ loopAt 4 $ chop 32 $ s "IGLOOGHOST_drum_loops:0" # gain (1.2*vol0) # orbit 1-- 5
        p "igloo1" $ loopAt 8 $ chop 64 $ s "IGLOOGHOST_drum_loops:1" # gain (1.2*vol1) # orbit 1-- 8
        p "igloo2" $ loopAt 4 $ chop 32 $ s "IGLOOGHOST_drum_loops:7" # gain (1.2*vol2) # orbit 1--10
    drums stopAll zero one =
      do
        if stopAll == 1
          then do
            hush
        else return ()
        p "drums0" $ loopAt 4 $ chop 32 $ s "33:28" # gain zero # delayIn # pan 0 # orbit 2
        p "drums1" $ loopAt 2 $ chop 32 $ s "33:28" # gain (one) # delayIn # pan 1 # orbit 2 -- 10
    bigBeat stopAll gainAmount slowAmount degradeAmount =
      do
        if stopAll == 1
          then do
            hush
        else return ()
        d6 $ rotL 1 $ slow slowAmount $ every 4 (# speed 0.5) $ someCyclesBy degradeAmount (degrade)$ s "[[monomachine:0!2] [~ monomachine:0@3] [monomachine:0] ~]" # gain gainAmount # speed (range 1 0.125 (saw)) # krush 10 # room 0.7 # orbit 3
    glitch stopAll zero one two =
      do
        if stopAll == 1
          then do
            hush
        else return ()
        p "glitch0" $ s "[glitch:4 ~ ~ ~ glitch:4? ~ ~]*<8 4 16>" # gain ((range 1 1.2 (rand))*zero) # room 0.2 # orbit 4-- 8
        p "glitch1" $ s "~ [~ [click:3] ~ ~ ~ ~ ~] ~ [~ ~ ~ click:3  ~ ~ ~]" # room 0.2 # gain (1.3*one) # nudge 0.027 # orbit 4-- 7
        p "glitch2" $ density 0.5 $ s "~ glitch:2 ~ glitch:2?" # room 0.9 # gain (1.3*two) # speed 2 # orbit 4--10
    sophie stopAll beat0 beat1 beat2 =
      do
        if stopAll == 1
          then do
            hush
        else return ()
        p "sophie0" $ slow 2 $ s "SOPHIE_fx_kicks(3,8)" # n "2" # gain (1.2*beat0) # room 0.4 # orbit 5
        p "sophie1" $ s "[~ SOPHIE_percussion]" # n 5 # gain (beat1) # nudge "-0.02" # orbit 5
        p "sophie2" $ every 4 (slow 2) $ s "[SOPHIE_hihats!4] [[SOPHIE_hihats*4] SOPHIE_hihats!2] [SOPHIE_hihats!2 [SOPHIE_hihats*4?]] [SOPHIE_hihats!4]" # n "<1 1 1 0>" # gain ((range 0.8 1 rand)*beat2) # orbit 5
    climax stopAll zero one two restart =
      do
        if stopAll == 1
          then do
            hush
        else return ()
        if (restart==1)
          then do
            p "windarp" $ qtrigger "windarp" $ loopAt 8 $ chop 32 $ s "44comp:21" # gain (1.2*zero) # room 0.5 # pan 0 # orbit 7
        else do
          p "windarp" $ loopAt 8 $ chop 32 $ s "44comp:21" # gain (1.2*zero) # room 0.5 # pan 0 # orbit 7
        if (restart==1)
          then do
            p "dpoarp" $ qtrigger "dpoarp" $ loopAt 8 $ chop 32 $ s "44comp:6" # gain (0.8*one) # room 0.5 # pan 1 # orbit 7
        else do
          p "dpoarp" $ loopAt 8 $ chop 32 $ s "44comp:6" # gain (0.8*one) # room 0.5 # pan 1 # orbit 7
        if (restart==1)
          then do
            p "melody" $ qtrigger "melody" $ loopAt 32 $ chop 32 $ s "44comp:24" # gain (1.3*two) # room 0.5 # orbit 7
        else do
          p "melody" $ loopAt 32 $ chop 32 $ s "44comp:24" # gain (1.3*two) # room 0.5 # orbit 7
    moreDrums zero one two three =
      do
        p "moreDrums0" $ density 2 $ brak $ sound ("glitch2:0 yeah:3? glitch:4 glitch:1") # room "0.15" # size "0.1" # speed "0.8" # (gain (1.1*zero)) # orbit 8
        p "moreDrums1" $ density 0.5 $ every 4 (# speed "0.95") $ "<0 1 0.5 0.125>" <~ sound "[808bd:3][808bd:7*2?][808bd:0*2?][808bd:5*2?]" # gain ((range 0.7 1.2 (rand))*one) # shape "0.9" # room "0.2" # speed "1.95" # up "-5" # orbit 8
        p "moreDrums2" $ every 4 (density 2) $ sound "dr2*8" # up "-5" # n (irand 6) # speed "12" # cut "1" # pan (rand) # room "0.1" # gain ((range 0.75 1.2 $ rand)*two) # crush "1" # orbit 8
        p "moreDrums3" $ degradeBy 0.1 $ s "lighter*16" # n (irand 33) # gain ((range 0.75 1.3 $ rand)*three) # orbit 8
    morePerc zero one two three four five=
      do
        p "morePerc0" $ degradeBy 0.3 $ s "{808bd:0 808sd:7 808bd:2}%16" # room "0.25" # crush (slow 4 (range 4 8 sine)) # gain zero # orbit 9
        p "morePerc1" $ fast 2 $ s "hh*2 hh*2 hh*2 <hh*6 [hh*2]!3>" # room 0.7 # gain ((range 1 1.2 rand)*one) # orbit 9
        p "morePerc2" $ rotL 1 $ degradeBy 0.35 $ every 4 (# n "30 20 10 6 7 25 11 13") $ s "tabla2*8" # n (run 8) # room "1" # gain ((range 0.85 1 rand)*two) # up "2" # orbit 9
        p "morePerc3" $ rotL 1 $ degradeBy 0.35 $ (fast 2) $ every 4 (# n "30 20 10 6 7 25 11 13") $ s "tabla2*8" # n (run 8) # room "1" # gain ((range 0.85 1 rand)*three) # up "2" # orbit 9
        p "morePerc4" $ rotL 1 $ degradeBy 0.0 $ s "{tabla2:0 tabla2:10 tabla2:20 tabla2:30 tabla2:34}%8" # room "1" # gain ((range 0.85 1 rand)*four) # up "2" # orbit 9
        p "morePerc5" $ rotL 1 $ degradeBy 0.0 $ s "{tabla2:0 tabla2:10 tabla2:20 tabla2:30 tabla2:34}%16" # room "1" # gain ((range 0.85 1 rand)*five) # up "2" # orbit 9
:}

:{
let setI = streamSetI tidal
    setF = streamSetF tidal
    setS = streamSetS tidal
    setR = streamSetR tidal
    setB = streamSetB tidal
:}

:set prompt "tidal> "
:set prompt-cont ""
