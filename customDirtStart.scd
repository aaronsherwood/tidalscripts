(
var startUpSuperDirt;
startUpSuperDirt = {
SuperDirt.start;
SystemClock.sched(5,{
"/Users/ags419/Documents/Code/tidalscripts/mutable.scd".load;
"/Users/ags419/Documents/Code/tidal-looper/Looper.scd".load;

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

~drumMachinesDir = PathName.new("/Users/ags419/Documents/Code/tidal-drum-machines/machines");
	~machines = ~drumMachinesDir.folders; //All drum machines
	/*~machines = ['Linn9000','RolandTR909']; //Selected drum machines
	~machines.do { arg m,i; ~machines[i]=(~drumMachinesDir+/+m) }*/

	(
		~machines.do({
			arg machine;
			var folders = machine.folders;
			postln("LOADING DRUM MACHINE: "++machine.folderName);
			folders.do({
				arg folder;
				~dirt.loadSoundFiles(folder.fullPath,namingFunction: { |x| x.basename.replace("-","")});
			});
		});
	);

}
);


};
startUpSuperDirt.value;
)