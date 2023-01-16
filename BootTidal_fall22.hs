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

    ---------------------------------------
    i_hope_so_too = do {
      all id;
      hush
    }
    reset_it_guys = do {
      once $ ccv "0*128" # ccn 0 # s "midi";
      once $ ccv "0*128" # ccn 1 # s "midi";
      once $ ccv "0*128" # ccn 2 # s "midi";
      once $ ccv "0*128" # ccn 3 # s "midi";
      once $ ccv "0*128" # ccn 4 # s "midi";
      once $ ccv "0*128" # ccn 5 # s "midi";
      once $ ccv "0*128" # ccn 6 # s "midi";
      once $ ccv "0*128" # ccn 7 # s "midi";
      once $ ccv "0*128" # ccn 8 # s "midi";
      once $ ccv "0*128" # ccn 9 # s "midi";
      once $ ccv "0*128" # ccn 11 # s "midi";
      once $ ccv "0*128" # ccn 12 # s "midi";
      once $ ccv "0*128" # ccn 13 # s "midi";
      once $ ccv "0*128" # ccn 14 # s "midi";
      once $ ccv "0*128" # ccn 15 # s "midi";
      once $ ccv "0*128" # ccn 16 # s "midi";
      once $ ccv "0*128" # ccn 17 # s "midi";
      once $ ccv "0*128" # ccn 18 # s "midi";
      once $ ccv "0*128" # ccn 19 # s "midi";
      once $ ccv "0*128" # ccn 20 # s "midi";
      once $ ccv "0*128" # ccn 21 # s "midi";
      once $ ccv "0*128" # ccn 22 # s "midi";
      once $ ccv "0*128" # ccn 23 # s "midi";
      once $ ccv "0*128" # ccn 24 # s "midi";
      once $ ccv "0*128" # ccn 25 # s "midi";
    }
    wait_wait_no = do {
    	hush
    }
    ------------ FIRST BUILD UP AND DROP ----------------------
    yes_im = do {
      d10 $ ccv "6" # ccn 10 # s "midi";
      once $ s "ade" # gain 1.2 # room 0.2
    }
    oh_we_started = do { once $ s "hehe:2" # gain 1.5 # room 0.3 } -- hi everybody + so
    lessgoooo = do {
    	d1 $ stack [s "em2" # note "<c d e>" # room 0.5, slow 4 $ struct "t*4" $ ccv ((segment 128 (range 127 0 saw))) # ccn "0" # s "midi"];
        d10 $ ccv "0" # ccn 10 # s "midi"
    }
    yessirrrr = do {
    	d2 $ stack [s "tink(3,8)" # room 0.2 # krush 2 # speed 0.5 # gain 1.2, struct "t(3,8)" $ ccv ((segment 128 (range 127 0 saw))) # ccn "1" # s "midi"];
        d10 $ ccv "1" # ccn 10 # s "midi"
    }
    wiggle_wiggle = do {
    	d3 $ stack [s "soskick(5,8)" # note "<c e f>" # gain 1, struct "t(5,8)" $ ccv ((segment 128 (range 127 0 saw))) # ccn "2" # s "midi" ]
    }
    no_kick_it = do {
      d2 $ stack [s "tink(3,8)?" # room 0.2 # krush 2 # speed 0.5 # gain 1.2, struct "t*4" $ ccv ((segment 128 (range 127 0 saw))) # ccn "1" # s "midi"];
      d5 $ stack [s "clubkick*2" # speed 2 # gain 0.9, struct "t*2" $ ccv (segment 128(range 120 0 isaw)) #ccn "3" # s "midi"]
    }
    can_you_bring_blobby = do { d4 $ stack [s "supervibe" #note "<c d e>", ccv (segment 128 (range 127 0 saw)) #ccn "4" # s "midi"] }
    nope = do { d5 $ stack [s "clubkick [clubkick*2] clubkick clubkick" # speed 2 # gain 0.9, struct "t [t t] t*2 t" $ ccv ((segment 128 (range 127 0 saw))) # ccn "3" # s "midi"] }
    make_it_more = do { d1 $ stack [s "em2*4" # note "<c d e>", struct "t*4" $ ccv ((segment 128 (range 127 0 saw))) # ccn "0" # s "midi"] }
    ill_try = do { d3 $ someCyclesBy 0.8 (fast 2) $ stack [ s "soskick(5,8)" # note "<c e f>" # gain 1,  struct "t(5,8)" $ ccv ((segment 128 (range 127 0 isaw))) # ccn "2" # s "midi", struct "t(5,8)" $ ccv ((segment 128 (range 127 0 isaw))) # ccn "3" # s "midi" ] }
    from_the_top = do {d5 $ fast 2 $ stack [s "clubkick [clubkick*2] clubkick clubkick" # speed 2 # gain 0.9, struct "t [t t] t*2 t" $ ccv ((segment 128 (range 127 0 saw))) # ccn "5" # s "midi"]}
    like_my_gpa = do {
      d6 $ qtrigger 6 $ seqP [
        (0, 1, s "hehe:1" # gain 1.5 # room 0.3),
        (0, 1, ccv "1" # ccn 6 # s "midi"),
        --(1, 2, ccv (segment 127 (range 1 127 saw)) # ccn 6 # s "midi"),
        (1, 2, ccv "0" # ccn 6 # s "midi"),
        (0, 1, s "soskick(5,8)" # note "<c e f>" # gain 1.5),
        --(0, 1, struct "t(5,8)" $ ccv "127" # ccn "7" # s "midi"),
        --(0, 1, ccv "0" # ccn 10 # s "midi"), -- to change visual - get white so
        (1, 2, ccv "2" # ccn 10 # s "midi"), -- to change visual - get the mult in
        (1, 22, s "[soskick, supervibe]" >| note "<d e f> c c c " #gain 1 # room 0.2),
        (1, 8, ccv (segment 64 (range 0 63 saw)) # ccn 0 # s "midi" ),
        (1, 18, fast 2 $ s "pluck*2 ~" #speed 5 # room 0.3),
        (1, 18, struct "t*2" $ ccv ((segment 128 (range 127 0 saw))) # ccn "7" # s "midi"),
        (1, 10, s "super808*2 super808" #n "c" #gain 2 # room 0.2),
        (1, 10, struct "t*2 t" $ ccv ((segment 128 (range 127 0 saw))) # ccn "2" # s "midi"),
        (1, 14, s "clubkick*8" # speed 2 # gain 1),
        (1, 14, struct "t*16" $ ccv ((segment 128 (range 127 0 saw))) # ccn "3" # s "midi"),
        --(2, 21, slow 1 $ s "hehe:1" # gain 1.3 # room 0.5),
        (1, 14, ccv "[32 0] [32 0] 0 0" # ccn "4" # s "midi"), -- for the invert / blend
        (1, 16, s "supervibe" #note "<c d e>" # room 0.5),
        --(1, 8, s "~ em2*2" # legato 2)
        (8, 18, struct "t [t t] t*2 t" $ ccv ((segment 128 (range 127 0 saw))) # ccn "5" # s "midi"),
        (8, 14, s "cp " # gain 1),
        (8, 18, ccv "127*64" # ccn "0" # s "midi"),
        (18, 19, ccv "0*128" # ccn "5" # s "midi"),
        (8, 9, ccv "1" # ccn 10 # s "midi"),
        (8, 9, ccv "127" # ccn 8 # s "midi")
      ]
    }
    make_it_drOOOOP = do {
    	hush;
        once $ ccv "0*128" # ccn 5 # s "midi";
        once $ ccv "127*128" # ccn 1 # s "midi"
    }
    -- TRANSITION ---------------------------------------
    make_it_a_line = do {
    	d5 $ s "clubkick*2";
        d8 $ struct "t*2" $ ccv (segment 128 (range 120 64 rand)) # ccn "8" # s "midi"
    }
    -------- here is where it starts shrinking
    bro_dat_a_square = do {
      d1 $ s "cp ~ cp ~";
      d5 $ s "clubkick*4";
      d8 $ struct "t*4" $ ccv (segment 128 (range 64 15 rand)) # ccn "8" # s "midi"
      --d5 $ stack [s "clubkick*4", struct "t*4" $ ccv ((segment 128 (range 64 12 rand))) # ccn "8" # s "midi"]
    }
    lol = do {
    	d2 $ s "<tink(3,8) tink(5,8)>" # room 1.5 # gain 1;
        d8 $ ccv ((segment 128 (range 15 6.4 saw))) # ccn "8" # s "midi"
    }
    guys_be_serious = do {
    	--resetMidi
    	d4 $ stack [s "blip(3,8) ~ ~ blip" # room 0.5, struct "t(3,8) ~ ~ t" $ ccv (segment 128 (range 0 127 saw)) # ccn 9 # s "midi"]
        --once $ ccv "4" # ccn 10 # s "midi"
    }
    dania_where_r_u = do {
    	d3 $ stack [s "casio <casio ~> casio <casio ~>" # room 0.5, struct "t <t ~> t <t ~>" $ ccv (segment 128 (range 0 127 saw)) # ccn "11" # s "midi"];
        --ccv (segment 64 (range 0 63 saw)) # ccn 1 # s "midi"
        --resetMidi
        once $ ccv "0*128" # ccn 6 # s "midi";
        once $ ccv "0*128" # ccn 0 # s "midi"
        --once $ ccv "4" # ccn 10 # s "midi"
    }
    i_cant_type = do {
      d6 $ stack [s "drumtraks" # room 0.5]
      --d7 $ jux rev $  stack [s "super808:6(3,8)" # note "<a b4 f>" # gain 1, struct "t(3,8)" $ ccv ((segment 128 (range 127 0 saw))) # ccn "2" # s "midi" ]
    }
    oops_i_forgot = do {
    	d1 $ bite 4 "1 0 1 <2 3>" $ stack [s "sine*4" # note ((scale "pelog" "0 .. 7")+4) # room 0.4 #cut 1, struct "t*4" $ ccv (segment 127 (range 127 0 isaw)) # ccn "12" # s "midi"];
        d2 silence;
        once $ ccv "127" # ccn 13 # s "midi"
    }
    its_a_feature_not_a_bug = do {
    	d5 silence;
    	--d3 silence
    	d4 $ stack [ s "clubkick ~ ~ clubkick" # gain 1.2, struct "t ~ ~ t" $ ccv (segment 128 (range 127 0 saw)) # ccn "13" # s "midi"]
    }
    i_moss_u_too = do {
    	d7 $ jux rev $  stack [s "super808:6(3,8)" # note "<a b4 f>" # gain 1, struct "t(3,8)" $ ccv ((segment 128 (range 127 0 saw))) # ccn "14" # s "midi" ];
        d3 silence;
        once $ ccv "0*128" # ccn 11 # s "midi"
    }
    soooo = do {
    	once $ stack [s "hehe:4" # gain 1.5, ccv "127*128" # ccn "6" # s "midi"]; -- so
    	d6 silence;
        --d3 silence
        d2 $ bite 4 "1 0 1 <2 3>" $ stack [s "sine*8" # note ((scale "pelog" "0 .. 7")+4) # room 0.4 #cut 2, struct "t*8" $ ccv (segment 128 (range 127 0 saw)) # ccn "12" # s "midi"];
        d1 $ fast 2 $ scramble 8 $ s "tink" >| note ((scale "major" "<[2 2 3 4] [4 3 2 1] [0 0 1 2] [[2@3 1] 1]>")) # room 0.4
    }
    ----- SOOOOO Mashup 1
    see_saw = do {
      d14 $ ccv "127*128" # ccn "6" # s "midi";
      d6 $ stack[ slice 8 "[0 5 1 2 0 3 2 4]" $ sound "hehe:4" # room 0.25 # krush 2 # gain 1.5 # pan sine, struct "[t t t t t t t t]" $ ccv ((segment 128 (range 120 0 isaw))) #ccn 14 # s "midi"];
      d5 $ stack [slice 8 "[<0*8 0*2> 3*4 2 4] [4 .. 7]" $ sound "hehe:4" # room 0.25 # squiz 0.5 # gain 1.3 # pan sine, struct "[<t*8 t*2> t*4 t t][t*5]" $ ccv ((segment 128 (range 120 0 isaw))) #ccn 15 # s "midi"];
      d3 $ slow 2 $ stack[ s "hehe:4*2" # gain 1.5 # room 0.3 # krush 2 # pan sine, struct "t*2" $ ccv "0 127" #ccn 4 # s "midi" ];
    }
    u_reap_what_u_so = do {
    	--d14 $ ccv "0*128" # ccn "6" # s "midi"
    	d7 silence;
        d2 $ fast 2 $  bite 4 "1 0 1 <2 3>" $ stack [s "sine*8" # note ((scale "pelog" "0 .. 7")+4) # room 0.4 #cut 2, struct "t*8" $ ccv ((segment 128 (range 120 0 isaw))) # ccn "12" # s "midi"];
        d4 $ stack [s "clubkick(3,8) clubkick(5,8)" # gain 1, struct "t(3,8) t(5,8)" $ ccv ((segment 128 (range 120 10 isaw))) # ccn "13" # s "midi"];
    -- d4 $ stack [ s "clubkick(3,8)" # gain 1, struct "t(3,8)" $ ccv ((segment 128 (range 127 0 saw))) # ccn "1" # s "midi"]
    }
    ----- SOOOOO Mashup 2
    perhaps = do {
      d6 $ fast 1 $ stack [slice 8 "[0 5? 1 2 0 3 2 4]" $ sound "hehe:4" # room 0.25 # krush 2 # gain 1.5 # pan sine, struct "[t t? t t t t t t]" $ ccv ((segment 128 (range 120 0 isaw))) #ccn 14 # s "midi" ];
      d5 $ fast 1 $stack [slice 8 "[<0*8 0*2> 3*4 2 4] [4 .. 7]" $ sound "hehe:4" # room 0.25 # squiz 0.5 # gain 1.5 # pan sine, struct "[<t*8 t*2> t*4 t t][t*5]" $ ccv ((segment 128 (range 120 0 isaw))) #ccn 15 # s "midi"];
      d3 $ s "hehe:4*2" # gain 1.5 # room 0.3 # krush 2 # cut 3 # pan sine
    }
    how_did_he_get_here = do {
      d5 $ qtrigger 5 $ seqP [
      	(0, 4, ccv "0*128" # ccn "17" # s "midi"),
      	(0, 2, slice 8 "[0 5 1 2 0 3 2 4]" $ sound "hehe:4" # room 0.25 # krush 2),
        (0, 2, ccv "1" # ccn "6" # s "midi"),
        (2, 3, ccv "0" # ccn "6" # s "midi"),
        (0, 2, slice 8 "[<0*8 0*2> 3*4 2 4] [4 .. 7]" $ sound "hehe:4" # room 0.25 # squiz 0.5),
        (0, 2, s "hehe:4" # gain 1.3 # room 0.2 # krush 2),
        (0, 1, s "hh*4"),(0, 1, ccv "0 1 0 1" # ccn "3" # s "midi"),
        (0, 1, ccv "0 1 0 1" # ccn "3" # s "midi"),
        (1, 2, s "hh*8"),
        (1, 2, ccv "0 1 0 1 0 1 0 1" # ccn "3" # s "midi"),
        (2, 4, s "hh*16"),
        (2, 4, ccv "0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1" # ccn "3" # s "midi"),
        (4, 5, s "~ ~ clubkick clubkick"),
        (4, 5, s "hehe:3" # gain 1.3 # room 0.3 #speed 0.9),
        (4, 6, ccv "127*128" # ccn "17" # s "midi"),
        (5, 18, s "reverbkick*4" # room 0.3),
        (5, 18, struct "t*4" $ ccv (segment 128 (range 127 0 isaw)) # ccn 20 # s "midi"),
        (5, 6, bite 4 "1 0 1 <2 3>" $ s "sine*4" # note ((scale "pelog" "0 .. 7")+4)),
        (5, 6, struct "t*4" $ ccv (segment 128 (range 127 0 isaw)) # ccn 21 # s "midi"),
        --(5,6, )
        (6, 18, bite 4 "1 0 1 <2 3>" $ s "sine*8" # note ((scale "pelog" "0 .. 7")+4)),
        (6, 18, struct "t*8" $ ccv (segment 128 (range 127 0 isaw)) # ccn 21 # s "midi"),
        (5, 18, s "bass3" <| n (run 8) # room 0.5 # gain 1.5 # krush 2),
        (6, 7, struct "t*4 ~" $ ccv (segment 128 (range 127 0 isaw)) # ccn 22 # s "midi"),
        (7, 18, struct "~ t*4" $ ccv (segment 128 (range 127 0 isaw)) # ccn 22 # s "midi")
      ] # gain 2;
      d14 silence;
      --d3 silence;
      --d6 silence
     }
    bruh = do { -- silences the sooos
    	d3 silence;
      d6 silence
    }
    give_us_an_A_pls = do {
      d1 silence;
      d2 silence;
      d3 silence;
      d6 silence;
      d10 $ struct "t*8" $ ccv ((segment 128 (range 127 0 isaw))) # ccn "0" # s "midi"
    }
    --d1 $ s "battles" # note "a4" # gain 1.1 # room 0.2
    which_party = do {
      all degrade;
      d6 $ stack [s "battles" # room 0.5, ccv (segment 127 (range 127 0 isaw)) # ccn "23" # s "midi"];
      once $ ccv "0*128" # ccn 2 # s "midi";
      once $ ccv "0*128" # ccn 17 # s "midi"
    }
    the_one_u_r_not_invited_to = do {
      hush;
      all id;
      d4 $ qtrigger 4 $ seqP [
        (0, 3, s "battles" # room 0.5 # gain 1.1),
        (0, 4, ccv (segment 127 (range 127 0 isaw)) # ccn "23" # s "midi"),
        (3, 4, s "hehe:5" # gain 2.5 # room 1)
        ]
     };
     lets_begin = do {
       d1 $ fast 2 $ seqPLoop [
       (0,2,s "superfork" <| n "e5 ~ e5 ~ e5 ~ e5 ~"),
       (2,4,s "superfork" <| n "f5*8"),
       (4,6,s "superfork" <| n "g5 ~ g5 ~ g5 ~ g5 ~"),
       (6,8,s "superfork" <| n "a5*3 ~ a5*4")
       ]# lpf (range 400 700 sine) #shape 0.9 # gain (range 1 1.8 sine);
       d16 $ ccn "0*32" # ccv "127 0" # s "midi"
     };
     layer_the_sound = do{
       xfade 2 $ fast 2 $ seqPLoop [
         (0,2,s "superhammond" <| n "a3 ~ b3 ~ c4 ~ d4 ~"),
         (2,4,s "superhammond" <| n "a3 ~ b3 ~ c4 ~ d4 ~"),
         (4,6,s "superhammond" <| n "e4 ~ f4 ~ g4 ~ a4 ~"),
         (6,8,s "superhammond" <| n "a3 ~ b3 ~ c4 ~ d4 ~")
       ] # room 1.2;
       d16 $ ccn "1*64" # ccv "<[10 ~ 30 ~ 50 ~ 70 ~]*2 [10 ~ 30 ~ 50 ~ 70 ~]*2 [50 ~ 90 ~ 110 ~ 127]*2 [10 ~ 30 ~ 50 ~ 70 ~]*2>" # s "midi"
     };
     clap_your_hands = do{
       d3 $ s "realclaps:2" # gain 1.5;
       d16 $ ccn "1*32" # ccv "100 0" # s "midi"
     };
     bleep_bleep = do{
       d4 $ s "808bd:1*4" # speed 4 # gain 1.2;
       d5 $ slice 3 "< [1 3] [~ 3] [~ 3] [~ 3]>" $ s "bleep bleep" # gain (range 1.4 1.8 sine) # room 1.2 # legato 1.5;
       d16 $ ccn "1*32" # ccv "[100 0]*4" # s "midi"
     };
     asmr = do{
       d6 $ qtrigger 6 $ fast 2 $ seqPLoop [
         (0,2,s "808:2*4"),
         (2,4,s "808:2*2")
       ]# legato 0.5 # gain (range 1.6 2 sine);
       d15 $ ccn "2*32" # ccv "80 15" # s "midi"
     };
     feast_your_ears = do{
       d6 $ s "808:2*8" # room (range 0 0.4 sine) # gain 2 # legato 0.5;
       d15 $ ccn "3*32" # ccv "100 0 100 0 100 0 100 0 100" # s "midi"
     };
     get_ready = do{
       d6 $ qtrigger 6 $ fast 2 $ seqPLoop [
         (4,8,s "808:2*8" # room (range 0 0.4 sine)),
         (6,8,s "realclaps")
       ]# legato 0.5 # gain (range 1.6 2 sine);
       d16 $ ccn "1*32" # ccv "[100 0]*8" # s "midi"
     };
     its_gonna_get_wild = do{
       d3 silence;
       d6 $ qtrigger 6 $ fast 2 $ seqP [
         (0,2, s "realclaps" # gain 5),
         (2,4, s "realclaps*2" # gain 5),
         (4,6, s "realclaps*4" # gain 5),
         (6,8, s "<realclaps*8 [realclaps*4 odx:6]>" # gain 5)
       ]# legato 0.5 # gain (range 1.6 2 sine);
       d16 $ ccn "0*32" # ccv "[10 20 15 30]" # s "midi";
       d8 $ qtrigger 8 $ fast 2 $ seqP [
         (8,12, s "superfork" <| n "[d5 g4 d5 g4 d5 g4 c5 g4]/2" # lpf (range 400 700 sine) # gain 1.5 # room 1.1),
         (8,9, s "superhammond" <| n "[e5 f5 g5 a5, e4 f4 g4 a4, e3 f3 g3 a3]" # gain 1.5 # room 1.1),
         (9,10, s "superhammond" <| n "[c5 a5 c5 d5, c4 a4 c4 d4, c3 a3 c3 d3]" # gain 1.5 # room 1.1),
         (10,11, s "superhammond" <| n "[d5 b5 c5 d5*3, d4 b4 c4 d4*3, d3 b3 c3 d3*3]" # gain 1.5 # room 1.1),
         (11,12, s "superhammond" <| n "<[b5 d5 g5 c5] [f5 c5 f5 c5] ~>" # gain 1.5 # room 1.1)
       ];
       d15 $ qtrigger 15 $ ccn "1*32" # ccv "<[100 0][100 0 100 0] [200 0 200 0]*2 [255 0 255 0 255 0 255 0]*2 >" # s "midi"
     };
     eargasm = do {
       d1 $ qtrigger 1 $ fast 2 $ seqPLoop [
         (0,2,s "superfork" <| n "e5 ~ e5 ~ e5 ~ e5 ~"),
         (2,4,s "superfork" <| n "f5 f5 f5? f5? f5? f5 f5 f5"),
         (4,6,s "superfork" <| n "g5 ~ g5 ~ g5 ~ g5 ~"),
         (6,8,s "superfork" <| n "a5*3 ~ a5*4")
       ] # room 2 # shape 0.9 # speed 1.2 # legato 0.5;
       d3 $ qtrigger 3 $ s "realclaps:2" # gain 1.5;
       d4 $ qtrigger 4 $ degradeBy 0.15 $ sound "hh*8" # gain "2.1 1.77 1.785 2.4 1.8 1.77 1.95";
       d5 $ qtrigger 5 $ slice 3 "< [1 3] [~ 3] [~ 3] [~ 3]>" $ s "bleep bleep" # gain (range 1 1.4 sine) # room 1.2 # legato 1.5;
       d6 $ qtrigger 6 $ s "808:2*8" # room (range 0 0.4 sine) # gain 2 # legato 0.5;
       d2 silence;
       d8 $ qtrigger 8 $ fast 2 $ seqPLoop [
         (0,4, s "superfork" <| n "[d5 g4 d5 g4 d5 g4 c5 g4]/2" # lpf (range 400 700 sine) # gain 1.5 # room 1.1),
         (0,1, s "superhammond" <| n "[e5 f5 g5 a5, e4 f4 g4 a4, e3 f3 g3 a3]" # gain 1.5 # room 1.1),
         (1,2, s "superhammond" <| n "[c5 a5 c5 d5, c4 a4 c4 d4, c3 a3 c3 d3]" # gain 1.5 # room 1.1),
         (2,3, s "superhammond" <| n "[d5 b5 c5 d5*3, d4 b4 c4 d4*3, d3 b3 c3 d3*3]" # gain 1.5 # room 1.1),
         (3,4, s "superhammond" <| n "<[b5 d5 g5 c5] [f5 c5 f5 c5] ~>" # gain 1.5 # room 1.1)
       ];
       d15 $ ccn "3*32" # ccv "0 ~ 150 ~" # s "midi"
    }
    now_do_what_i_say = do
     d1 silence
     d5 silence
     d4 silence
     d6 silence
     d15 $ ccn "3*32" # ccv "10 ~ 50 ~ 150 ~ 255 ~" # s "midi"
     d1 $ s "hardcore:0*4" #gain 2
    triangles = do{
      d2 $ s "hh*8" # gain 2;
      d15 $ ccn "4*32" # ccv "20 ~ 50 ~" # s "midi"
    }
    dance = do{
      d3 $ fast 2 $ s "feel [~ feel] feel [feel ~]" # gain 5;
      d13 $ ccn "5*32" # ccv "[10 -10]*2" # s "midi"
    };
    shake = do{
      d4 $ s "reverbkick" # gain 2;
      d15 $ ccn "3*32" # ccv "40 ~ 70 ~ 100 ~ 127" # s "midi"
    };
    party = do{
     d6 $ every 2 (struct "t(5,8)")  $ s "superhoover" <| n "-3 -1 0 2" # gain 2.2 # legato 0.8;
     d15 $ ccn "6*32" # ccv "[0 255]*2" # s "midi";
     d11 $ ccn "9*32" # ccv "[0 5]" # s "midi"
    };
    go_crazyyyy = do
     d1 silence
     d2 silence
     d3 silence
     d4 silence
     d5 silence
     d6 silence
     d7 silence
     d15 $ ccn "8*64" # ccv "<10 127>" # s "midi"
    mosh_pit = do {
      d8 $ qtrigger 8 $ fast 2 $ seqP [
        (0,1,s "superhammond" <| n "[e5 f5 g5 a5, e4 f4 g4 a4, e3 f3 g3 a3]" # legato 0.8 # gain 2),
       (1,2,s "superhoover" <| n "-3 -1 0 2" # gain 2),
       (2,3,s "superhammond" <| n "[e5 f5 g5 a5, e4 f4 g4 a4, e3 f3 g3 a3]" # legato 0.8 # gain 2),
       (3,4,s "superhoover" <| n "-3 -1 0 2" # gain 2),
        (4,5,s "superhammond" <| n "[e5 f5 g5 a5, e4 f4 g4 a4, e3 f3 g3 a3]" # legato 0.8 # gain 2),
       (5,6,s "superhoover" <| n "-3 -1 0 2" # gain 2),
       (6,7,s "superhammond" <| n "[e5 f5 g5 a5, e4 f4 g4 a4, e3 f3 g3 a3]" # legato 0.8 # gain 2),
       (7,8,s "superhoover" <| n "-3 -1 0 2" # gain 2),
        (8,9,s "superhoover" <| n "f5*4"),
        (9,10,s "superhoover" <| n "f5*4"),
        (10,11,s "superhoover" <| n "f5*8"),
       (11,12,s "superhoover" <| n "f5*16"),
        (0,8,s "feel [~ feel] feel [feel ~]" # gain 2),
        (0,8,s "reverbkick*2" # gain 2),
        (0,8,s "realclaps*4" # gain 2),
        (8,9,s "hardcore:0*4" # gain 2),
        (9,10,s "hardcore:0*4" # gain 2),
        (10,11,s "hardcore:0*8" # gain 2),
        (11,12,s "hardcore:0*16" # gain 2)
      ];
      d15 $ ccn "7*64" # ccv "<[100 127 100 127]*4 [30 0 30 0]*4>" # s "midi";
    };
    let_us_end = do{
      d2 $ qtrigger 2 $ seqP [
        (0,2, s "superpiano" <| n "[c5 f5 f5 g5 f5 e5 d5 d5]/2"),
        (2,4, s "superpiano" <| n "[d5 g5 g5 a5 g5 f5 e5 c5]/2"),
        (4,6, s "superpiano" <| n "[c5 a5 a5 10 a5 g5 f5 d5]/2"),
        (6,8, s "superpiano" <| n "[d5 c5 d5 g5 e5 f5]/2"),
        (0,8, s "realclaps" # gain 5),
        (8,9, s "xmas")
      ]
    }
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
