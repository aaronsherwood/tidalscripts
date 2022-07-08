Quarks.checkForUpdates({Quarks.install("SuperDirt", "v1.1.3"); thisProcess.recompile()})
//Install sc3 plugins
//Unzip the release and move it to your SuperCollider extensions folder. You can find it by evaluating:
Platform.userExtensionDir

SuperDirt.start

SuperDirt.stop

"/Users/ags419/Documents/Code/tidalScripts/customDirtStart.scd".load;

(
var startUpSuperDirt;
startUpSuperDirt = {
SuperDirt.start;
SystemClock.sched(2,{
"/Users/ags419/Documents/Code/tidalScripts/mutable.scd".load;
"/Users/ags419/Documents/Code/tidal-looper/Looper.scd".load;

~dirt.orbits.do { |x|
    var clouds = GlobalDirtEffect(\global_mi_clouds, [\cloudspitch, \cloudspos, \cloudssize, \cloudsdens, \cloudstex, \cloudswet, \cloudsgain, \cloudsspread, \cloudsrvb, \cloudsfb, \cloudsfreeze, \cloudsmode, \cloudslofi]);
    var verb = GlobalDirtEffect(\global_mi_verb, [\verbwet, \verbtime, \verbdamp, \verbhp, \verbfreeze, \verbdiff, \verbgain]);
    x.globalEffects = x.globalEffects
      .addFirst(clouds)
      .addFirst(verb);

    x.initNodeTree;
};
}
);

MIDIClient.init;
~midiOut = MIDIOut.newByName("IAC Driver", "Bus 1");
~dirt.soundLibrary.addMIDI(\midi, ~midiOut);
~midiOut.latency = 0.0;
};
startUpSuperDirt.value;
)




ServerOptions.devices;

Server.default.options.inDevice_("External Microphone");
Server.default.options.outDevice_("Soundflower (2ch)");
Server.default.options.outDevice_("Headphones & Soundflower")

Server.default.options.outDevice_("Duet & Soundflower")
Server.default.options.outDevice_("External Headphones");
Server.default.options.outDevice_("USB Audio CODEC ");
Server.default.options.outDevice_("MacBook Pro Speakers");
Server.default.options.outDevice_("Duet USB");
Server.default.options.inDevice_("Duet USB");
Server.default.options.outDevice_(1);

Server.default.reboot
Server.default.quit

(
SynthDef(\plaits, {|out, sustain=1, pan, accelerate, freq, harm=0.5, timbre=0.5, morph=0.5, engine=0|
    var env = EnvGen.ar(Env.linen(0.01, 0.98, 0.01, 1,-3), timeScale:sustain, doneAction:2);
	var sound = MiPlaits.ar(pitch: freq.cpsmidi, engine: engine, harm: harm, timbre: timbre, morph: morph);
	Out.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, env));
}).add
);

(
~dirt.addModule('verb', { |dirtEvent|
	dirtEvent.sendSynth('verb' ++ ~dirt.numChannels,
		[
			vwet: ~vwet,
			vtime: ~vtime,
			vdamp: ~vdamp,
			vhp: ~vhp,
			vfreeze: ~vfreeze,
			vdiff: ~vdiff,
			out: ~out
		]
	)
}, {~vwet.notNil});
)

~dirt.modules;

(
SynthDef("verb" ++ ~dirt.numChannels, { |out, vwet=0.5, vtime=0.7, vdamp=0.5, vhp=0.05, vfreeze=0, vdiff=0.625|
	var signal = In.ar(out, ~dirt.numChannels);
	signal = MiVerb.ar(signal, vwet, vtime, vdamp, vhp, vfreeze, vdiff);
  ReplaceOut.ar(out, signal)
}).add;

);

arg inputArray, pit=0, pos=0.5, size=0.25, dens=0.4, tex=0.5, drywet=0.5, in_gain=1,
		spread=0.5, rvb=0, fb=0, freeze=0, mode=0, lofi=0, trig=0, mul=1.0, add=0.0;
		^this.multiNewList(['audio', pit, pos, size, dens, tex, drywet, in_gain, spread, rvb, fb,
			freeze, mode, lofi, trig] ++ inputArray.asArray).madd(mul);

SynthDef("global_mi_clouds" ++ ~dirt.numChannels, { |dryBus, effectBus, cloudspitch=0, cloudspos=0, cloudssize=0, cloudsdens=0, cloudstex=0, cloudswet=0, cloudsgain=0, cloudsspread=0, cloudsrvb=0, cloudsfb=0, cloudsfreeze=0, cloudsmode=0, cloudslofi=0|
    var signal = In.ar(dryBus, ~dirt.numChannels);
    Out.ar(effectBus, MiClouds.ar(
      inputArray: signal,
      pit: cloudspitch,
      pos: cloudspos,
      size: cloudssize,
      dens: cloudsdens,
      tex: cloudstex,
      drywet: cloudswet,
      in_gain: cloudsgain,
      spread: cloudsspread,
      rvb: cloudsrvb,
      fb: cloudsfb,
      freeze: cloudsfreeze,
      mode: cloudsmode,
      lofi: cloudslofi
    ));
  }, [\ir, \ir]).add;

(
~dirt.orbits.do { |x|
	x.globalEffects = ~dirt.orbits[0].globalEffects.addFirst(
		GlobalDirtEffect(\global_mi_clouds, [\ir])
	)
};
~dirt.orbits[0].initNodeTree;
)

~dirt.orbits.do { |x| x.globalEffects.postln; " ----------".postln; }

(
~dirt.addModule('rings', { |dirtEvent|
	dirtEvent.sendSynth('rings' ++ ~dirt.numChannels,
		[
			rtrig: ~rtrig,
			rfreq: ~rfreq,
			rstruct: ~rstruct,
			rbright: ~rbright,
			rdamp: ~rdamp,
			rpos: ~rpos,
			rmodel: ~rmodel,
			rpoly: ~rpoly,
			rintern_exciter: ~rintern_exciter,
			reasteregg: ~reasteregg,
			rbypass: ~rbypass,
			out: ~out
		]
	)
}, {~rbright.notNil});
)


(
SynthDef("rings" ++ ~dirt.numChannels, { |out, rtrig=0, rfreq=60.0, rstruct=0.75, rbright=0.75, rdamp=0.4, rpos=0.9, rmodel=0, rpoly=1,rintern_exciter=0, reasteregg=0, rbypass=0|
	var signal = In.ar(out, ~dirt.numChannels);
	signal = MiRings.ar(signal, rtrig, rfreq, rstruct, rbright, rdamp, rpos, rmodel, rpoly,rintern_exciter, reasteregg, rbypass);
  ReplaceOut.ar(out, signal)
}).add;

);

~dirt.orbits.do { |x| x.globalEffects.postln }

(
var numChannels = ~dirt.numChannels;
(1..SuperDirt.maxSampleNumChannels).do { |numChannels|
	SynthDef("dirt_global_verb" ++ numChannels, { |dryBus, effectBus, vwet=0.5, vtime=0.7, vdamp=0.5, vhp=0.05, vfreeze=0, vdiff=0.625|
		var signal = In.ar(dryBus, ~dirt.numChannels);
	signal = MiVerb.ar(signal, vwet, vtime, vdamp, vhp, vfreeze, vdiff);
		DirtPause.ar(signal.sum, graceTime:4);
		XOut.ar(effectBus, 1 - vwet, signal)
	}).add;
};
)

(
~dirt.orbits.do { |x|
	x.globalEffects = ~dirt.orbits[0].globalEffects.addFirst(
		GlobalDirtEffect(\dirt_global_verb, [\vwet])
	)
};
~dirt.orbits[0].initNodeTree;
)

~dirt.orbits.do { |x| x.globalEffects.postln; " ----------".postln; }


// show where sample folder is
Quarks.gui


s.meter;
{ SoundIn.ar([0, 0]) }.play;

~dirt.orbits.do { |x|
    var clouds = GlobalDirtEffect(\global_mi_clouds, [\cloudspitch, \cloudspos, \cloudssize, \cloudsdens, \cloudstex, \cloudswet, \cloudsgain, \cloudsspread, \cloudsrvb, \cloudsfb, \cloudsfreeze, \cloudsmode, \cloudslofi]);
    var verb = GlobalDirtEffect(\global_mi_verb, [\verbwet, \verbtime, \verbdamp, \verbhp, \verbfreeze, \verbdiff, \verbgain]);
    x.globalEffects = x.globalEffects
      .addFirst(clouds)
      .addFirst(verb);

    x.initNodeTree;
  };


// send RMS

b = NetAddr.new("127.0.0.1", 7771);    // create the NetAddr

b.postln
(
var w, levels;
w = Window("levels", Rect(100, 100, 500, 400)).front;
~dirt.startSendRMS;
levels = ~dirt.orbits.collect { |orbit, i|
	orbit.defaultParentEvent.put(\rmsReplyRate, 8).put(\rmsPeakLag, 3);
	LevelIndicator(w, Rect(21 * i + 10, 10, 20, 180) ).warning_(0.9).critical_(1.0)

};


OSCFunc({ |msg|
	defer {
		var a = levels[msg[2]];
		a.value = msg[3].ampdb.linlin(-40, 0, 0, 1);
		a.peakLevel = msg[4].ampdb.linlin(-40, 0, 0, 1);
		//msg[3].ampdb.linlin(-40, 0, 0, 1).postln;
		b.sendMsg("/rms", msg[3].ampdb.linlin(-40, 0, 0, 1));
	}

}, "/rms");
CmdPeriod.doOnce { w.close };
)

// to stop it
~dirt.stopSendRMS;