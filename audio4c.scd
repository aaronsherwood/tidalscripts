// show where sample folder is
Quarks.gui

//list input and output devices
ServerOptions.devices;

//set output for zoom (on Mac, might be a little different on windows)
Server.default.options.inDevice_("AUDIO4c")
Server.default.options.outDevice_("AUDIO4c")

Server.default.options.inDevice_("MacBook Pro Microphone")
Server.default.options.outDevice_("SYNIC Wireless Audio")

"/Users/ags419/Documents/Code/tidalscripts/mutable.scd".load;

SuperDirt.start

//MIDI
MIDIClient.init;

//Note: you need to put the correct name here, IAC (etc.) is for a Mac
~midiOut = MIDIOut.newByName("AUDIO4c", "USB2"); // FOR MAC
~midiOut = MIDIOut.newByName("IAC Driver", "Bus 1");

~dirt.soundLibrary.addMIDI(\midi, ~midiOut);
~midiOut.latency = 0;