// start everything up
"/Users/ags419/Documents/Code/tidalscripts/customDirtStart.scd".load;

SuperDirt.start
//////////////////////////////////////////

ServerOptions.devices;
Server.default.options.memSize
Server.default.options.memSize = 65536;

Server.default.options.inDevice_("External Microphone");
Server.default.options.outDevice_("Soundflower (2ch)");
Server.default.options.outDevice_("Headphones & Soundflower")

Server.default.options.outDevice_("Duet & Soundflower")
Server.default.options.outDevice_("External Headphones");
Server.default.options.outDevice_("USB Audio CODEC ");
Server.default.options.outDevice_("MacBook Pro Speakers");
Server.default.options.outDevice_("ZEDi10");
Server.default.options.outDevice_("Crestron");
Server.default.options.inDevice_("ZEDi10");
Server.default.options.outDevice_(1);
Server.default.options.outDevice_("JBL Xtreme 2")
Server.default.options.outDevice_("JBL Charge 4")

Server.default.reboot
Server.default.quit

// show where sample folder is
Quarks.gui

// mic input thru
{ SoundIn.ar([0, 0],2) }.play;

//////////////////////////////////////////

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