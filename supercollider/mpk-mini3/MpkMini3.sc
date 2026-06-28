MpkMini3 {
	var <>knobCC, <>bankCC, <>knobScale, <>relativeKnobs, <>volumeKnob, <>debugLogs, <>currentBank, <>buses;
	var <>server;

	// TODO:
	//  - min and max for relative knobs


	*new {
		arg knobCC = #[70, 71, 72, 73, 74, 75, 76, 77], bankCC = #[16, 17, 18, 19, 20, 21, 22, 23],
		knobScale = 1, relativeKnobs = true, volumeKnob = true, debugLogs = false;

		^super.new.init(knobCC, bankCC, knobScale, relativeKnobs, volumeKnob, debugLogs);
	}


	init {
		arg knobCC, bankCC, knobScale, relativeKnobs, volumeKnob, debugLogs;

		this.knobCC = knobCC;
		this.bankCC = bankCC;
		this.knobScale = knobScale;
		this.relativeKnobs = relativeKnobs;
		this.volumeKnob = volumeKnob;
		this.debugLogs = debugLogs;
		this.currentBank = bankCC[0];
		this.buses = Dictionary.new();
		this.server = Server.default;

		this.server.waitForBoot({
			this.prConnectMidi("MPK mini 3", "MPK mini 3 MIDI 1");
		});

		this.prInitBuses();
		this.prInitMidiDefs();

		postln("MPK Mini3 initialised");
	}


	setKnobScale { | bank, knob, scale |
		this.buses[bank][knob][\scale] = scale;
	}


	getBus { | bank, knob |
		var finalKnob = this.knobCC[this.knobCC.size - 1];

		if (this.volumeKnob and: { knob == finalKnob }) {
			warn("Final knob is reserved for master volume control");
		} {
			^this.buses[bank][knob][\bus];
		}
	}


	prKnobIncrementer { |val, cc|
		var bus = this.buses[currentBank][cc][\bus];
		var toAdd = this.buses[currentBank][cc][\scale];

		var busAdd = { |n|
			var curr = bus.getSynchronous();
			var sum = curr + n;

			this.prBusDebugPrint(cc, sum);

			bus.setSynchronous(sum);
		};

		if(val == 1,
			{ busAdd.(toAdd) },
			{ busAdd.(toAdd.neg) }
		);
	}


	prAbsoluteKnobSet { |val, cc|
		var bus = this.buses[currentBank][cc][\bus];
		this.prBusDebugPrint(cc, val);
		bus.setSynchronous(val);
	}


	prBusDebugPrint { | cc, val |
		// Don't print if final bus reserved for volume
		var finalKnob = this.knobCC[this.knobCC.size - 1];
		var finalBusVolume = this.volumeKnob and: { cc == finalKnob };

		if (this.debugLogs and: { finalBusVolume == false }) {
			postf("bank: %, bus: %, val: %\n", this.currentBank, cc, val);
		};
	}


	prInitBuses {
		// Create the buses dictionary.
		// Each bank has its own dict of knobs. Knobs are event objects.
		this.bankCC.do({ |bank, i|
			var bankDict = Dictionary.new();
			this.knobCC.do({ |kCc, j|
				var knob = ();
				knob[\scale] = knobScale;
				// Should server be configurable?
				knob[\bus] = Bus.control(this.server, 1).set(0); // default value = 0
				bankDict.put(kCc, knob);
			});

			this.buses.put(bank, bankDict);
		});
	}


	prInitMidiDefs {
		if (this.relativeKnobs) {
			MIDIdef.cc(\MpkMini3_Knobs_Relative, { |val, num, chan, src|
				this.prKnobIncrementer(val, num);
			}, ccNum: this.knobCC, chan: 0);
		} {
			MIDIdef.cc(\MpkMini3_Knobs_Absolute, { |val, num, chan, src|
				this.prAbsoluteKnobSet(val, num);
			}, ccNum: this.knobCC, chan: 0);
		};


		MIDIdef.cc(\MpkMini3_Pads, { |val, num, chan, src|
			// Filter MIDI off values.
			if (val != 0) {
				if (this.debugLogs, {
					postf("Pads: %\n", [val, num, chan, src]);
				});
				this.currentBank = num;
			};
		}, ccNum: this.bankCC, chan: 9);


		// Set final knob to control global volume.
		// Applies for all banks.
		if (this.volumeKnob) {
			MIDIdef.cc(\MpkMini3_Volume, { |val, num, chan, src|
				var currentVolume = this.server.volume.volume;
				var toAdd = 0.1;

				var volumeAdd = { |n|
					var newVolume = currentVolume + n;

					if (this.debugLogs == true, {
						postf("Volume: %d\n", newVolume);
					});

					{ this.server.volume.volume = newVolume }.defer;
				};

				if(val == 1,
					{ volumeAdd.(toAdd) },
					{ volumeAdd.(toAdd.neg) }
				);
			}, ccNum: this.knobCC[this.knobCC.size - 1]);

		};
	}


	prConnectMidi { | device, name |
		var connected = false;

		if(MIDIClient.initialized == false) {
			MIDIClient.init;
		};

		MIDIClient.sources.do({ arg endPoint;
			if(device.notNil
				and: { name.notNil }
				and: { endPoint.device == device }
				and: { endPoint.name == name }) {
				// catch exception thrown when already connected
				try {
					// connect MIDI device to SuperCollider in port 0
					MIDIIn.connect(0, endPoint.uid);
					connected = true;
				}
			};
		});

		if (connected == false) {
			postf("Could not find device: %\n", device);
		};
	}
}