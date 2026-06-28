s.quit;


MIDIClient.destinations;

(
MIDIClient.init;
m = MIDIOut(0);
)

/*m.noteOn(0, 60, 100);
m.noteOn(2, 61, 100);
m.allNotesOff(0);*/


(
/*p = Pbind(
	\type, \midi,
	\midiout, m,
	\midicmd, \noteOn,
	\midinote, Pwhite(30, 50, inf),
	\amp, Pexprand(0.1, 0.5, inf),
	\sustain, 0.09,
	\dur, 0.5
);*/

a = Scale.dorian;

~quarter = (60/102 * 4);

Pdef(\scale_seq,
	Pbind(
		\type, \midi,
		\midiout, m,
		\midicmd, \noteOn,
		// \degree, Prand((0..9), inf),
		\degree, Pseq([
			0, 0, \rest,
			Prand((1..8) ++ \rest ++ [9, 13], 14)
		], inf),
		\scale, Pfunc({ a }, inf),
		\root, -10,
		// \amp, Pexprand(0.1, 0.5, inf),
		\sustain, Prand((0.04..0.08), inf),
		\dur, Prand([1/8, 1/16, 1/4], inf), //Prand([0.25, 0.5], inf)
		\stretch, ~quarter
)).play(quant: ~quarter);

)

Pdef(\scale_seq).stop;
Pdef(\scale_seq).play;

Pdef(\scale_seq).set(\detune, 0);


















