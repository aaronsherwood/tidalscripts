(
// two VST plugins in series:
SynthDef(\synths, { arg bus;
    var sig = VSTPlugin.ar(nil, 2, id: \repro1);
    var sig2 = VSTPlugin.ar(nil, 2, id: \repro5);
    Out.ar(bus, sig);
	Out.ar(bus, sig2);
}).add;
)

// create the synth:
~synth = Synth(\synths, [\bus, 0]);
~repro5 = VSTPluginController(~synth, \repro5).open("Repro-5", editor: true, verbose: true);

~repro5.editor;

// just for channel 2 receiving
(
MIDIClient.init;
MIDIIn.connectAll;
MIDIFunc.noteOn({|vel, pitch, chan|
	if (chan == 2,{
	~repro5.midi.noteOn(0, pitch, vel);
	})
});
MIDIFunc.noteOff({ |vel, pitch, chan|
	if (chan == 2,{
	~repro5.midi.noteOff(0, pitch, vel);
	})
	// ~repro5.midi.noteOff(0, pitch, vel);
});
MIDIFunc.cc({arg ...args; args.postln;~repro5.midi.control(0,args[1],args[0]) }, (1..95));
MIDIFunc.bend({arg ...args; args.postln; ~repro5.midi.bend(0,args[0])})
)


// for any channel
(
MIDIClient.init;
MIDIIn.connectAll;
MIDIFunc.noteOn({|vel, pitch, chan|
	~repro5.midi.noteOn(0, pitch, vel);
});
MIDIFunc.noteOff({ |vel, pitch, chan|
	~repro5.midi.noteOff(0, pitch, vel);
});
MIDIFunc.cc({arg ...args; args.postln;~repro5.midi.control(0,args[1],args[0]) }, (1..95));
MIDIFunc.bend({arg ...args; args.postln; ~repro5.midi.bend(0,args[0])})
)