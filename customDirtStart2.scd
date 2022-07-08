(
var startUpSuperDirt;
startUpSuperDirt = {

(
s.waitForBoot {
    ~dirt = SuperDirt(2, s); // two output channels


// load samples from multiple folders:
   // ~dirt.loadSoundFiles("/Users/aaronsherwood/Live\ Sets/sounds/aaronDirtSounds/*");
    ~dirt.loadSoundFiles("/Users/aaronsherwood/Library/Application\ Support/SuperCollider/downloaded-quarks/Dirt-Samples/*");
s.sync; // wait for supercollider to finish booting up
    ~dirt.start(57120, 0 ! 12); // start superdirt, listening on port 57120,
};
);

SystemClock.sched(5,{
"/Users/aaronsherwood/Documents/Code/tidalscripts/mutable.scd".load;
"/Users/aaronsherwood/Documents/Code/tidal-looper/Looper.scd".load;

MIDIClient.init;
~midiOut = MIDIOut.newByName("IAC Driver", "Bus 1");
~dirt.soundLibrary.addMIDI(\midi, ~midiOut);
~midiOut.latency = 0.0;

		// ~midiOut2 = MIDIOut.newByName("FH2", "FH2");
		// ~dirt.soundLibrary.addMIDI(\midi2, ~midiOut2);
		// ~midiOut2.latency = 0.0;


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


};
startUpSuperDirt.value;
)











